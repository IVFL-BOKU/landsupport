library(magrittr)
library(mlr3)
library(mlr3learners)

options(warn = 1)

args = commandArgs(TRUE)
param = jsonlite::read_json(args[1])

bandsMonthly = c('NDVI2', 'LAI2', 'FAPAR2', 'FCOVER2', 'B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B11', 'B12')
bandsYearly = c(
  'NDVI2q05',  'NDVI2q50',  'NDVI2q98',
  'NDTI2q05',  'NDTI2q50',  'NDTI2q98',
  'MNDWI2q05', 'MNDWI2q50', 'MNDWI2q98',
  'NDBI2q05',  'NDBI2q50',  'NDBI2q98',
  'BSI2q05',   'BSI2q50',   'BSI2q98',
  'BLFEI2q05', 'BLFEI2q50', 'BLFEI2q98'
)
bandsMonthly = bandsMonthly[1]
bandsYearly = bandsYearly[1]
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

fetchFeaturesByTile = function(trainData, bands, blockSize) {
  data = trainData %>%
    dplyr::group_by(tile) %>%
    dplyr::do({
      d = .data
      for (i in seq_along(bands$band)) {
        cat('\t', d$tile[1], bands$var[i], '\n')
        resp = httr::GET(
          sprintf(
            '%s?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=%s&SUBSET=X(%d,%d)&SUBSET=Y(%d,%d)&SUBSET=%s&FORMAT=%s',
            param$rasdamanUrl, 
            URLencode(bands$coverage[i]), 
            as.integer(d$tilex[1]), as.integer(d$tilex[1] + blockSize), as.integer(d$tiley[1]), as.integer(d$tiley[1] + blockSize),
            URLencode(paste0('ansi("', bands$date[i], 'T00:00:00.000Z")')), 
            URLencode('image/tiff')
          )
        )
        if (resp$status_code == 200) {
          tmpFile = paste0(param$tmpDir, '/', param$runId, '.tif')
          writeBin(httr::content(resp, 'raw'), tmpFile)
          tmpRast = suppressWarnings(raster::raster(tmpFile))
          d[, bands$var[i]] = raster::extract(tmpRast, d[, c('x', 'y')])
        } else {
          d[, bands$var[i]] = rep(NA_real, nrow(d))
        }
      }
      d[, c('x', 'y', 'label', bands$var)]
    })
  return(data)
}

fetchFeaturesByPoint = function(trainData, bands) {
  for (i in seq_along(bands$band)) {
    cat('\t', bands$var[i], '\n')
    trainData[, bands$var[i]] = purrr::map2_dbl(trainData$x, trainData$y, function(x, y) {
      resp = httr::GET(
        sprintf(
          '%s?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=%s&SUBSET=X(%d)&SUBSET=Y(%d)&SUBSET=%s&FORMAT=%s',
          param$rasdamanUrl, 
          URLencode(paste0(param$coveragePrefix, '_', bands$band[i])), 
          as.integer(x), as.integer(y), 
          URLencode(paste0('ansi("', bands$date[i], 'T00:00:00.000Z")')), 
          URLencode('application/json')
        )
      )
      if (resp$status_code == 200) {
        return(as.numeric(httr::content(resp, 'text', encoding = 'UTF-8')))
      } else {
        return(NA_real_)
      }
    })
  }
  return(trainData)
}

if (nrow(trainData) < 1000) {
  trainData = fetchFeaturesByPoint(trainData, bands)
} else {
  fetchFeaturesByTile(trainData, bands, 25000) 
}

cat('Training the model\n')
featuresCoverage = colSums(!is.na(trainData[, -1:-3])) / nrow(trainData)
features = names(featuresCoverage)[featuresCoverage >= param$minDataCoverage]
trainData$label = factor(trainData$label)
task = mlr3::TaskClassif$new('task', target = 'label', backend = trainData[, -which(names(trainData) %in% c('x', 'y', 'tilex', 'tiley', 'tile'))])
learner = mlr3::lrn('classif.ranger')

if (param$validationFile != '') {
  sampler = mlr3::rsmp("cv", folds = 3L)
  sampler$instantiate(task)
  results = mlr3::resample(task, learner, sampler)
  validationResults = summary(results$score(msr('classif.acc'))$classif.acc)
  print(validationResults)
  writeLines(jsonlite::toJSON(as.list(validationResults, auto_unbox = TRUE)), param$validationFile)
}

learner$train(task)

bands = bands %>%
  dplyr::filter(var %in% features)

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

cat('\nProcessing region of interest\n')
tmpFile = paste0(param$tmpDir, '/', param$runId, '.tif')
suppressWarnings(unlink(tmpFile))
roi = suppressWarnings(
  sf::read_sf(param$roiFile) %>%
    sf::st_set_crs(sf::st_crs(3035))
)
bbox = sf::st_bbox(roi)
px = bbox['xmin']
py = bbox['ymin']
while (px <= bbox['xmax']) {
  while(py <= bbox['ymax']) {
    ppx = min(px + param$blockSize, bbox['xmax'])
    ppy = min(py + param$blockSize, bbox['ymax'])
    cat('\tprocessing block', px, py, ppx, ppy, '\n')
   
    dx = ceiling((ppx - px) / abs(param$resx))
    dy = ceiling((ppy - py) / abs(param$resy))
    dataBlock = dplyr::tibble(.rows = dx * dy)
    for (i in seq_along(bands$band)) {
      cat('\t\tband', bands$var[i], '\n')
      resp = httr::GET(
        sprintf(
          '%s?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=%s&SUBSET=X(%d,%d)&SUBSET=Y(%d,%d)&SUBSET=%s&FORMAT=%s',
          param$rasdamanUrl,
          URLencode(bands$coverage[i]),
          as.integer(px), as.integer(ppx), as.integer(py), as.integer(ppy),
          URLencode(paste0('ansi("', bands$date[i], 'T00:00:00.000Z")')),
          URLencode('image/tiff')
        )
      )
      if (resp$status_code == 200) {
        writeBin(httr::content(resp, 'raw'), tmpFile)
        tmpRast = suppressWarnings(raster::raster(tmpFile))
        dataBlock[, bands$var[i]] = raster::getValues(tmpRast)
        unlink(tmpFile)
      } else {
        dataBlock[, bands$var[i]] = NA_integer
      }
    }

    cat('\tclassifying\n')
    dataBlock[is.na(dataBlock)] = -100000L
    dataBlock$label = factor(1, levels = levels(trainData$label))
    task = mlr3::TaskClassif$new('task', target = 'label', backend = dataBlock)
    prediction = learner$predict(task) %>%
      mlr3::as.data.table() %>%
      use_series('response')
    tmpRast = raster::setValues(tmpRast, prediction)
    targetFile = paste0(param$rasterDir, '/eu_class_user_', param$runId, '/', px, '_', py, '.tif')
    dir.create(dirname(targetFile), FALSE, TRUE)
    suppressWarnings(unlink(targetFile))

    suppressWarnings(raster::writeRaster(tmpRast, tmpFile, datatype = 'INT1U', overwrite = TRUE, NAflag = 255))
    # set projection in a way rasdaman can recognize and cut to the roi
    cat('\tpostprocessing\n')
    tmpFile2 = paste0(dirname(tmpFile), '/_', basename(tmpFile))
    cmd = sprintf("gdal_translate -a_srs %s %s %s", shQuote(param$projection), shQuote(tmpFile), shQuote(tmpFile2))
    cat('\t\t', cmd, '\n')
    system(cmd)
    cmd = sprintf("gdalwarp -cutline %s -cl roi -crop_to_cutline %s %s", shQuote(param$roiFile), shQuote(tmpFile2), shQuote(targetFile))
    cat('\t\t', cmd, '\n')
    system(cmd)
    suppressWarnings(unlink(c(tmpFile, tmpFile2)))

    px = px + param$blockSize
    py = py + param$blockSize
  }
}
cat('\nEnded\n')

