# a quick & dirty script for computing classifications (winter/summer, multiple classes) based on tilesDir content
# - it is assumed that the modelFile contains three models sorted from most to less precise one
# - for performance reasons (1000 times speedup) it's better to read whole rasters at once but it makes the script very memory hungry
#   so choose the number of threads carefully
modelFile = 'vignettes/lucas_features_selection_models.RData'
tilesDir = '/eodc/private/boku/ACube2/tiles'
tmpDir = '/eodc/private/boku/ACube2/tmp'
year = 2018
outBand = 'CLASS'
blockSize = 1000000
learnerNumThreads = 10
nCores = 3
cubeRpath = '/eodc/private/boku/software/cubeR'
skipExisting = TRUE

args = commandArgs(TRUE)
t0 = Sys.time()
cat(paste0(c('Running classify.R', args, as.character(Sys.time()), '\n'), collapse = '\t'))

devtools::load_all(cubeRpath, quiet = TRUE)
library(dplyr, quietly = TRUE)
library(mlr3, quietly = TRUE)
library(mlr3learners, quietly = TRUE)
library(doParallel, quietly = TRUE)

e = new.env()
load(modelFile, envir = e)
models = get(ls(envir = e)[1], envir = e)
models = lapply(models, function(x) {
  x$learner$predict_type = 'prob'
  x$learner$param_set$values$num.threads = learnerNumThreads
  x
})

cols = lapply(models, function(x) {dplyr::tibble(var = x$cols)}) %>%
  dplyr::bind_rows() %>%
  dplyr::distinct() %>%
  dplyr::mutate(var  = gsub('-', '.', .data$var)) %>%
  dplyr::mutate(var2 = gsub('[.]', '-', .data$var)) %>%
  tidyr::separate(.data$var2, c('band', 'date'), sep = '_') %>%
  dplyr::mutate(band = sub('Q([0-9]{2})', 'q\\1', toupper(.data$band)))

registerDoParallel()
options(cores = nCores)
tiles = list.dirs(tilesDir, full.names = FALSE, recursive = FALSE)
tiles = tiles[nchar(tiles) == 6]
tiles = c(c('E47N27', 'E47N28', 'E48N27', 'E48N28', 'E46N19', 'E46N20', 'E47N19', 'E47N20'), tiles) # Marchfeld and Naples first
output = foreach(tls = tiles, .combine = bind_rows) %dopar% {
  cat(tls, '\n', sep = '')

  outFiles = getTilePath(tilesDir, tls, paste0(year, 'y1'), paste0(outBand, c('', 'PROB')), 'tif')
  processed = FALSE
  if (!skipExisting | any(!file.exists(outFiles))) {
    try({
      tmpFiles = sub('^.*/', paste0(tmpDir, '/'), outFiles)
      unlink(c(tmpFiles, outFiles))

      cols = cols %>%
        dplyr::mutate(tileFile = getTilePath(tilesDir, tls, .data$date, .data$band, 'tif'))
      outputClass = raster::raster(raster::raster(cols$tileFile[1]))
      outputProb = raster::raster(outputClass)

      input = vector('list', nrow(cols))
      names(input) = c(cols$var)
      for (i in seq_along(cols$var)) {
        #cat(cols$var[i], '\n')
        input[[i]] = raster::getValues(raster::raster(cols$tileFile[i]))
      }
      input = dplyr::as_tibble(input) %>%
        dplyr::mutate(
          .dummy = rep_len(factor(models[[1]]$levels), n()),
          block = as.integer((row_number() - 1L) / blockSize)
        )
      cat(tls, ' input data read', sep = '')

      output = input %>%
        dplyr::group_by(block) %>%
        dplyr::do({
          x = .data
          cat(tls, ' block ', x$block[1], '\n', sep = '')
          result = dplyr::tibble(
            class = rep(NA_integer_, nrow(x)),
            prob = rep(NA_integer_, nrow(x))
          )
          mask1 = rowSums(is.na(x[, models[[1]]$cols])) == 0
          mask2 = rowSums(is.na(x[, models[[2]]$cols])) == 0 & !mask1
          mask3 = rowSums(is.na(x[, models[[3]]$cols])) == 0 & !mask1 & !mask2
          if (sum(mask1) > 0) {
            tmpVal1 = models[[1]]$learner$predict(mlr3::TaskClassif$new('tmp', x[mask1, ], '.dummy'))
            result$class[mask1] = as.integer(tmpVal1$response)
            result$prob[mask1] = as.integer(100 * apply(tmpVal1$prob, 1, max))
          }
          if (sum(mask2) > 0) {
            tmpVal2 = models[[2]]$learner$predict(mlr3::TaskClassif$new('tmp', x[mask2, ], '.dummy'))
            result$class[mask2] = as.integer(tmpVal2$response)
            result$prob[mask2] = as.integer(100 * apply(tmpVal2$prob, 1, max))
          }
          if (sum(mask3) > 0) {
            tmpVal3 = models[[3]]$learner$predict(mlr3::TaskClassif$new('tmp', x[mask3, ], '.dummy'))
            result$class[mask3] = as.integer(tmpVal3$response)
            result$prob[mask3] = as.integer(100 * apply(tmpVal3$prob, 1, max))
          }
          result
        })
      raster::values(outputClass) = output$class
      raster::values(outputProb) = output$prob

      raster::writeRaster(outputClass, tmpFiles[1], datatype = 'INT1U', overwrite = TRUE, options = c('COMPRESS=DEFLATE', 'TILED=YES', 'BLOCKXSIZE=512', 'BLOCKYSIZE=512'))
      raster::writeRaster(outputProb, tmpFiles[2], datatype = 'INT1U', overwrite = TRUE, options = c('COMPRESS=DEFLATE', 'TILED=YES', 'BLOCKXSIZE=512', 'BLOCKYSIZE=512'))
      file.rename(tmpFiles, outFiles)
      processed = TRUE
      cat(tls, ' finished\n', sep = '')
    })
  }
  dplyr::tibble(tileFile = outFiles, processed = processed)
}
logProcessingResults(output, t0)
