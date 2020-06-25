#' Computes urban mask.
#' @param input data frame describing input - must provide \code{period},
#'   \code{tile}, \code{modelFile}, \code{band} and \code{tileFile} columns (in
#'   the long, unnested format).
#' @param targetDir directory storing computed masks
#' @param tmpDir directory for temporary files
#' @param bandName output band name to be used for computed masks
#' @param blockSize size (in pixels) of a processing block (the bigger the block
#'   size, the faster computations but also higher memory usage, 1024 to 2048
#'   seems to be resonable values)
#' @param gdalOpts gdal options to be used while saving the output file (e.g.
#'   setting up compression or internal tiling)
#' @param skipExisting should already existing masks be skipped
#' @import dplyr
#' @export
prepareUrban = function(input, targetDir, tmpDir, bandName, blockSize, gdalOpts, skipExisting = TRUE) {
  input = input %>%
    dplyr::group_by(.data$period, .data$tile, .data$modelFile) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      band = bandName,
      tileFile = getTilePath(targetDir, .data$tile, .data$period, bandName)
    ) %>%
    dplyr::ungroup()

  skipped = processed = dplyr::tibble(period = character(), tile = character(), band = character(), tileFile = character())
  if (skipExisting) {
    tmp = file.exists(input$tileFile)
    skipped = input %>%
      dplyr::filter(tmp) %>%
      dplyr::select(.data$period, .data$tile, .data$band, .data$tileFile) %>%
      dplyr::mutate(processed = FALSE)
    input = input %>%
      dplyr::filter(!tmp)
  }

  if (nrow(input) > 0) {
    createDirs(input$tileFile)
    unlink(input$tileFile)

    processed = input %>%
      dplyr::mutate(
        tmpFile = paste0(tmpDir, '/', basename(.data$tileFile))
      )
    on.exit({
      unlink(processed$tmpFile)
    })

    processed = processed %>%
      dplyr::group_by(.data$period, .data$tile, .data$band) %>%
      dplyr::do({
        e = new.env()
        load(.data$modelFile, envir = e)
        rfmod = get('rfmod', envir = e)
        rasters = raster::stack(.data$data[[1]]$tileFile)
        names(rasters) = .data$data[[1]]$band
        ext = raster::extent(rasters)
        tmpFiles = character()
        for (i in 0:floor(dim(rasters)[1] / blockSize)) {
          for (j in 0:floor(dim(rasters)[2] / blockSize)) {
            tmpExt = ext
            tmpExt@xmin = ext@xmin + i * blockSize * raster::res(rasters)[1]
            tmpExt@xmax = min(ext@xmin + (i + 1) * blockSize * raster::res(rasters)[1], ext@xmax)
            tmpExt@ymin = ext@ymin + j * blockSize * raster::res(rasters)[2]
            tmpExt@ymax = min(ext@ymin + (j + 1) * blockSize * raster::res(rasters)[2], ext@ymax)
            tmpVal = 100 * raster::predict(rasters, rfmod, type = 'prob', ext = tmpExt)
            tmpFile = paste0(.data$tmpFile, '_', i, '_', j, '.tif')
            on.exit({unlink(tmpFile)}, add = TRUE)
            tmpFiles = append(tmpFiles, tmpFile)
            raster::writeRaster(tmpVal, tmpFile, datatype = 'INT1U', overwrite = TRUE)
          }
        }
        rm(tmpVal, rasters)
        command = sprintf(
          'gdalwarp %s %s %s && mv %s %s',
          gdalOpts, paste0(shQuote(tmpFiles), collapse = ' '), shQuote(.data$tmpFile), shQuote(.data$tmpFile), shQuote(.data$tileFile)
        )
        system(command, ignore.stdout = TRUE)
        dplyr::as_tibble(data.frame(tileFile = .data$tileFile, processed = TRUE, stringsAsFactors = FALSE))
      }) %>%
      dplyr::ungroup()
  }

  return(dplyr::bind_rows(processed, skipped))
}
