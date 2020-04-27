#' Computes land cover classification
#' @param tiles a dataframe describing tiles to be processed (with \code{tile}
#'   and \code{period} columns, where \code{period} will be preserved in the output
#'   image filenames)
#' @param targetDir a directory where computed images should be saved (a
#'   separate subdirectory for each tile will be created)
#' @param tmpDir a directory for temporary files
#' @param modelFile file storing classification models produced by
#'   \code{\link{prepareLandcoverModel}}
#' @param bandName output band name (the classification probability band name
#'   will be created with a PROB suffix)
#' @param nCores number of cores used for classification
#' @param skipExisting should already existing tiles be skipped?
#' @param gdalOpts \code{raster::writeRaster()} parameters used for output
#'   raster creation
#' @return dataframe describing created images
#' @export
#' @import dplyr
prepareLandcover = function(input, targetDir, tmpDir, modelFile, bandName, nCores, skipExisting, gdalOpts) {
  input = input %>%
    dplyr::mutate(
      band = bandName,
      outFile = getTilePath(tilesDir, .data$tile, .data$period, landcoverModelName),
      probFile  = getTilePath(tilesDir, .data$tile, .data$period, paste0(landcoverModelName, 'PROB'))
    )

  skipped = processed = dplyr::tibble(period = character(), tile = character(), band = character(), tileFile = character())
  if (skipExisting) {
    tmp = file.exists(input$outFile) & file.exists(input$probFile)
    skipped = input %>%
      dplyr::filter(tmp) %>%
      dplyr::select(.data$period, .data$tile, .data$band, .data$outFile) %>%
      dplyr::rename(tileFile = .data$outFile)
    input = input %>%
      dplyr::filter(!tmp)
  }

  if (nrow(input) > 0) {
    createDirs(input$outFile)
    unlink(c(input$outFile, input$probFile))
    input = input %>%
      dplyr::mutate(
        tmpFile = paste0(tmpDir, basename(.data$outFile)),
        tmpProbFile = paste0(tmpDir, basename(.data$probFile)),
      )
    tmpFiles = c(input$tmpFile, input$tmpProbFile)
    on.exit({
      unlink(tmpFiles)
    })

    e = new.env()
    load(modelFile, envir = e)
    models = get(ls(envir = e)[1], envir = e)
    rm(e)
    models = lapply(models, function(x) {
      x$learner$predict_type = 'prob'
      x$learner$param_set$values$num.threads = nCores
      x
    })
    cols = lapply(models, function(x) {dplyr::tibble(var = x$cols)}) %>%
      dplyr::bind_rows() %>%
      dplyr::distinct() %>%
      dplyr::mutate(var  = gsub('-', '.', .data$var)) %>%
      dplyr::mutate(var2 = gsub('[.]', '-', .data$var)) %>%
      tidyr::separate(.data$var2, c('band', 'date'), sep = '_') %>%
      dplyr::mutate(band = sub('Q([0-9]{2})', 'q\\1', toupper(.data$band)))

    processed = input %>%
      dplyr::group_by(.data$period, .data$tile, .data$band) %>%
      dplyr::do({
        cat(.data$tile, '\n')
        processed = FALSE
        try({
          cols = cols %>%
            dplyr::mutate(tileFile = getTilePath(tilesDir, tls, .data$date, .data$band, 'tif'))
          outputClass = raster::raster(raster::raster(cols$tileFile[1]))
          outputProb = raster::raster(outputClass)

          input = vector('list', nrow(cols))
          names(input) = c(cols$var)
          for (i in seq_along(cols$var)) {
            input[[i]] = raster::getValues(raster::raster(cols$tileFile[i]))
          }
          input = dplyr::as_tibble(input) %>%
            dplyr::mutate(
              .dummy = rep_len(factor(models[[1]]$levels), n()),
              block = as.integer((row_number() - 1L) / blockSize)
            )
          cat(tls, '\tinput data read', sep = '')

          output = input %>%
            dplyr::group_by(block) %>%
            dplyr::do({
              x = .data
              cat('\t block ', x$block[1], '\n')
              result = dplyr::tibble(
                class = rep(NA_integer_, nrow(x)),
                prob = rep(NA_integer_, nrow(x))
              )
              mask = rep(FALSE, nrow(x))
              for (m in models) {
                mask = rowSums(is.na(x[, m$cols])) == 0 & !mask
                if (sum(mask) > 0) {
                  tmpVal = m$learner$predict(mlr3::TaskClassif$new('tmp', x[mask, ], '.dummy'))
                  result$class[mask] = as.integer(tmpVal$response)
                  result$prob[mask] = as.integer(100 * apply(tmpVal$prob, 1, max))
                }
              }
              result
            })
          raster::values(outputClass) = output$class
          raster::values(outputProb) = output$prob

          raster::writeRaster(outputClass, .data$tmpFile, datatype = 'INT1U', overwrite = TRUE, options = gdalOpts)
          raster::writeRaster(outputProb, .data$tmpProbFile, datatype = 'INT1U', overwrite = TRUE, options = gdalOpts)
          file.rename(c(.data$tmpFile, .data$tmpProbFile), c(.data$outFile, .data$probFile))
          processed = TRUE

        })
        dplyr::tibble(tileFile = c(.data$outFile, .data$probFile), processed = processed)
      })
  }

  return(dplyr::bind_rows(processed, skipped))
}