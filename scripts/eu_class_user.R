library(magrittr)
library(mlr3)
library(mlr3learners)
library(doParallel, quietly = TRUE)

options(warn = 1)
registerDoParallel()

args = commandArgs(TRUE)
param = jsonlite::read_json(args[1])
options(cores = param$nCores)

sink(param$logFile, split = TRUE)

bandsMonthly = c('NDVI2', 'LAI2', 'FAPAR2', 'FCOVER2', 'B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B11', 'B12')
bandsYearly = c(
  'NDVI2q05',  'NDVI2q50',  'NDVI2q98',
  'NDTI2q05',  'NDTI2q50',  'NDTI2q98',
  'MNDWI2q05', 'MNDWI2q50', 'MNDWI2q98',
  'NDBI2q05',  'NDBI2q50',  'NDBI2q98',
  'BSI2q05',   'BSI2q50',   'BSI2q98',
  'BLFEI2q05', 'BLFEI2q50', 'BLFEI2q98'
)
#bandsMonthly = c('NDVI2', 'B02', 'B03', 'B04', 'B08')
#bandsYearly = c('NDVI2q05',  'NDVI2q50',  'NDVI2q98')
bands = dplyr::bind_rows(
  dplyr::tibble(band = bandsYearly, month = NA_integer_),
  expand.grid(band = bandsMonthly, month = param$monthMin:param$monthMax, stringsAsFactors = FALSE)
) %>%
  dplyr::mutate(
    coverage = paste0(param$coveragePrefix, '_', band),
    var = paste0(band, dplyr::if_else(is.na(month), '_y', sprintf('_m%02d', month)))
  ) %>%
  dplyr::mutate(
    date = dplyr::if_else(is.na(month), paste0(param$year, '-01-01'), sprintf('%04d-%02d', as.integer(param$year), month))
  )

cat('Extracting feature values for training data\n')
trainData = dplyr::bind_rows(lapply(param$referencePoints, dplyr::as_tibble)) %>%
  dplyr::filter(!is.na(label)) %>%
  dplyr::mutate(
    tilex = as.integer(x / param$blockSize) * param$blockSize,
    tiley = as.integer(y / param$blockSize) * param$blockSize,
    tile = paste0(as.integer(x / param$blockSize), '_', as.integer(y / param$blockSize)),
  )

fetchFeaturesByPoint = function(trainData, bands) {
  trainDataValues = foreach (band = split(bands, seq_along(bands$band)), .combine = dplyr::bind_cols) %dopar% {
    cat('\t', band$var, '\n')
    dv = dplyr::tibble(.rows = nrow(trainData))
    dv[, band$var] = purrr::map2_dbl(trainData$x, trainData$y, function(x, y) {
      url = sprintf(
        '%s?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=%s&SUBSET=X(%d)&SUBSET=Y(%d)&SUBSET=%s&FORMAT=%s',
        param$rasdamanUrl,
        URLencode(paste0(param$coveragePrefix, '_', band$band)),
        as.integer(x), as.integer(y),
        URLencode(paste0('ansi("', band$date, 'T00:00:00.000Z")')),
        URLencode('application/json')
      )
      resp = httr::GET(url)
      if (resp$status_code == 200) {
        value = as.integer(httr::content(resp, 'text', encoding = 'UTF-8'))
        return(ifelse(value %in% c(-32768L, 32767L, 65535L), NA_integer_, value))
      } else {
        #cat(url, '\n', resp$status_code, ':', httr::content(resp, 'text', encoding = 'UTF-8'))
        return(NA_integer_)
      }
    })
    dv
  }
  return(dplyr::bind_cols(trainData, trainDataValues))
}
trainData = fetchFeaturesByPoint(trainData, bands)

cat('Training the model\n\n')
featuresCoverage = colSums(!is.na(trainData)) / nrow(trainData)
features = setdiff(names(featuresCoverage)[featuresCoverage >= param$minDataCoverage], c('x', 'y', 'tilex', 'tiley', 'tile'))
trainData$label = factor(trainData$label)
task = mlr3::TaskClassif$new('task', target = 'label', backend = trainData[, features])
if (length(features) > param$maxFeatures) {
  filter = mlr3filters::flt('information_gain')
  importance = filter$calculate(task) %>% mlr3::as.data.table() %>% dplyr::as_tibble()
  cat('\tReducing features count to', param$maxFeatures, 'based on information gain\n')
  features = c('label', importance$feature[1:param$maxFeatures])
  task = mlr3::TaskClassif$new('task', target = 'label', backend = trainData[, features])
  rm(filter, importance)
}
features
learner = mlr3::lrn('classif.ranger')
learner$param_set$values = list(num.threads = param$nCores)
learner$predict_type = 'prob'

if (param$validationFile != '') {
  sampler = mlr3::rsmp("cv", folds = 3L)
  sampler$instantiate(task)
  results = mlr3::resample(task, learner, sampler)
  validationResults = summary(results$score(msr('classif.acc'))$classif.acc)
  print(validationResults)
  writeLines(jsonlite::toJSON(as.list(validationResults, auto_unbox = TRUE)), param$validationFile)
}

learner$train(task)

drawLegend = function(labels, fontSize, pngPath, jsonPath) {
  nl = length(labels)
  colors = scales::hue_pal()(nl)
  writeLines(jsonlite::toJSON(colors), jsonPath)
  grDevices::png(pngPath, 100, 100, 'mm', res = 72)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(grid::unit(0, 'npc'), grid::unit(0, 'npc'), grid::unit(1, 'npc'), grid::unit(1, 'npc'), gp = grid::gpar(fontsize = fontSize)))
  uy = grid::convertY(grid::unit(fontSize, 'bigpts'), 'mm', TRUE)
  width = uy * 5 + max(grid::convertWidth(grid::stringWidth(labels), 'mm', TRUE))
  dev.off()
  grDevices::png(pngPath, width, (nl * 1.5 + 0.5) * uy, 'mm', bg = 'white', res = 72)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(grid::unit(0, 'npc'), grid::unit(0, 'npc'), grid::unit(1, 'npc'), grid::unit(1, 'npc'), just = c('left', 'bottom'), gp = grid::gpar(fontsize = fontSize)))
  for (i in seq_along(labels)) {
    grid::grid.rect(grid::unit(uy * 0.5, 'mm'), grid::unit(i * uy * 1.5 - uy * 1.2, 'mm'), grid::unit(3 * uy, 'mm'), grid::unit(uy * 1.25, 'mm'), gp = grid::gpar(fill = colors[i]), hjust = 0, vjust = 0)
    grid::grid.text(labels[i], grid::unit(4 * uy, 'mm'), grid::unit(i * uy * 1.5 - uy * 0.5, 'mm'), hjust = 0, vjust = 0.5)
  }
  dev.off()
}
drawLegend(levels(trainData$label), param$fontSize, param$legendFilePng, param$legendFileJson)

bands = bands %>%
  dplyr::filter(var %in% features)
labels = levels(trainData$label)
rm(trainData, task)

cat('\nProcessing region of interest\n')
tmpFile = paste0(param$tmpDir, '/', param$runId, '.tif')
suppressWarnings(unlink(tmpFile))
roi = suppressWarnings(
  sf::read_sf(param$roiFile) %>%
    sf::st_set_crs(sf::st_crs(3035))
)
bbox = sf::st_bbox(roi)
px = bbox['xmin']
while (px <= bbox['xmax']) {
  py = bbox['ymin']
  while(py <= bbox['ymax']) {
    ppx = min(px + param$blockSize, bbox['xmax'])
    ppy = min(py + param$blockSize, bbox['ymax'])
    cat('\tblock    ', px, '-', ppx, '    ', py, '-', ppy, '\n')
    pxCount = NULL
   
    dataBlock = foreach (band = split(bands, seq_along(bands$band)), .combine = dplyr::bind_cols) %dopar% {
      cat('\t\tband', band$var, '\n')
      resp = httr::GET(
        sprintf(
          '%s?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=%s&SUBSET=X(%d,%d)&SUBSET=Y(%d,%d)&SUBSET=%s&FORMAT=%s',
          param$rasdamanUrl,
          URLencode(band$coverage),
          as.integer(px), as.integer(ppx), as.integer(py), as.integer(ppy),
          URLencode(paste0('ansi("', band$date, 'T00:00:00.000Z")')),
          URLencode('image/tiff')
        )
      )
      if (resp$status_code == 200) {
        tmpFile = paste0(param$tmpDir, '/', param$runId, '_', band$var, '.tif')
        writeBin(httr::content(resp, 'raw'), tmpFile)
        tmpRast = suppressWarnings(raster::raster(tmpFile))
        if (is.null(pxCount)) {
          pxCount = nrow(tmpRast) * ncol(tmpRast)
        }
        dataBlock = dplyr::tibble(.rows = pxCount)
        dataBlock[, band$var] = as.vector(raster::getValues(tmpRast))
      } else {
        dataBlock = dplyr::tibble(.rows = pxCount)
        dataBlock[, band$var] = NA_integer_
      }
      dataBlock
    }
    tmpFiles = paste0(param$tmpDir, '/', param$runId, '_', bands$var, '.tif')
    tmpRast = suppressWarnings(raster::raster(tmpFiles[1]))
    unlink(tmpFiles)

    cat('\tclassifying\n')
    dataBlock[is.na(dataBlock)] = -100000L
    dataBlock$label = factor(1, levels = labels)
    task = mlr3::TaskClassif$new('task', target = 'label', backend = dataBlock)
    prediction = learner$predict(task)
    values = prediction$response
    probs = apply(prediction$prob, 1, max)
    values[probs < param$classProbMin] = NA_integer_
    targetFileClass = paste0(param$rasterDir, '/class_', px, '_', py, '.tif')
    targetFileProb = paste0(param$rasterDir, '/prob_', px, '_', py, '.tif')
    dir.create(dirname(targetFileClass), FALSE, TRUE)
    suppressWarnings(unlink(c(targetFileClass, targetFileProb)))

    tmpRast = raster::setValues(tmpRast, values)
    suppressWarnings(raster::writeRaster(tmpRast, tmpFile, datatype = 'INT1U', overwrite = TRUE, NAflag = 255))
    # set projection in a way rasdaman can recognize and cut to the roi
    cat('\tpostprocessing\n')
    tmpFile2 = paste0(dirname(tmpFile), '/_', basename(tmpFile))
    cmd = sprintf("gdal_translate -a_srs %s %s %s", shQuote(param$projection), shQuote(tmpFile), shQuote(tmpFile2))
    cat('\t\t', cmd, '\n')
    system(cmd)
    cmd = sprintf("gdalwarp -cutline %s -cl roi -crop_to_cutline %s %s", shQuote(param$roiFile), shQuote(tmpFile2), shQuote(targetFileClass))
    cat('\t\t', cmd, '\n')
    system(cmd)
    suppressWarnings(unlink(c(tmpFile, tmpFile2)))

    tmpRast = raster::setValues(tmpRast, as.integer(254 * probs))
    suppressWarnings(raster::writeRaster(tmpRast, tmpFile, datatype = 'INT1U', overwrite = TRUE, NAflag = 255))
    # set projection in a way rasdaman can recognize and cut to the roi
    cat('\tpostprocessing\n')
    tmpFile2 = paste0(dirname(tmpFile), '/_', basename(tmpFile))
    cmd = sprintf("gdal_translate -a_srs %s %s %s", shQuote(param$projection), shQuote(tmpFile), shQuote(tmpFile2))
    cat('\t\t', cmd, '\n')
    system(cmd)
    cmd = sprintf("gdalwarp -cutline %s -cl roi -crop_to_cutline %s %s", shQuote(param$roiFile), shQuote(tmpFile2), shQuote(targetFileProb))
    cat('\t\t', cmd, '\n')
    system(cmd)
    suppressWarnings(unlink(c(tmpFile, tmpFile2)))

    rm(dataBlock, tmpRast, prediction, values, probs)
    py = py + param$blockSize
  }
  px = px + param$blockSize
}
cat('\nEnded\n')

