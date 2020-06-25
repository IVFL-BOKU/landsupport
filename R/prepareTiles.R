#' Generates EQUI7 tiles
#' @param input a data frame describing rasters to be tiled (must contain
#'   \code{period, band, tile, bbox, tileFiles}
#' @param targetDir a directory where tiles should be saved (a separate
#'   subdirectory for each tile will be created)
#' @param gridFile a file providing a tiling grid (in any file format supported
#'   by the \code{\link[sf]{read_sf}}). All features have to have a \code{TILE}
#'   attribute denoting the tile name. The tiles projection will follow the grid
#'   file projection.
#' @param tmpDir a directory for temporary files
#' @param method resampling method (near, bilinear, cubic, cubicspline, lanczos,
#'   average, mode, max, min, med, q1, q3 - see gdalwarp doc)
#' @param skipExisting should already existing tiles be skipped?
#' @param gdalOpts additional gdalwarp options, e.g. enabling output file
#'   compression or multithreading#' @return data frame describing created tiles
#' @import dplyr
#' @export
prepareTiles = function(input, targetDir, gridFile, tmpDir, method, skipExisting = TRUE, gdalOpts = '') {
  options(scipen = 100)
  prj = sf::st_crs(sf::read_sf(gridFile, quiet = TRUE))$proj4string

  inNodata = dplyr::tibble(
    band = c(sprintf('B%02d', 1:12), 'B8A', 'AOT', 'CLD', 'DEM', 'PVI', 'SCL', 'SNW', 'TCI', 'VIS', 'WVP'),
    nodata = c(rep(0, 22))
  )
  sclNodata = 0
  outNodata = c(LOG1S = 0, INT1S = 127, INT1U = 255, INT2S = 32767, INT2U = 65535, INT4S = 2147483647, INT4U = 4294967296, FLT4S = 3.4e+38, FLT8S = 1.7e+308)


  input = input %>%
    dplyr::mutate(
      outFile = getTilePath(targetDir, .data$tile, .data$period, .data$band)
    )

  skipped = processed = dplyr::tibble(period = character(), tile = character(), band = character(), tileFile = character())
  if (skipExisting) {
    tmp = file.exists(input$outFile)
    skipped = input %>%
      dplyr::filter(tmp) %>%
      dplyr::select(.data$period, .data$tile, .data$band, .data$outFile) %>%
      dplyr::rename(tileFile = .data$outFile)
    input = input %>%
      dplyr::filter(!tmp)
  }

  if (nrow(input) > 0) {
    createDirs(input$outFile)
    unlink(input$outFile)

    processed = input %>%
      dplyr::left_join(inNodata) %>%
      dplyr::group_by(.data$period, .data$tile, .data$band) %>%
      dplyr::mutate(
        method = dplyr::if_else(.data$band %in% 'SCL', 'near', method),
        inNodata = dplyr::if_else(!is.na(.data$nodata), paste0('-srcnodata ', .data$nodata), ''),
        outNodata = dplyr::if_else(.data$band %in% 'SCL', sclNodata, outNodata[raster::dataType(raster::raster(.data$tileFiles[[1]]$tileFile[1]))]),
        tmpFile = paste0(tmpDir, '/', basename(.data$outFile))
      ) %>%
      dplyr::mutate(
        command = sprintf(
          'gdalwarp %s -q -overwrite -tr 10 10 -te %s -r %s %s -dstnodata %d -t_srs "%s" %s %s && mv %s %s',
          gdalOpts, .data$bbox, .data$method, .data$inNodata, .data$outNodata, prj, paste0(shQuote(.data$tileFiles[[1]]$tileFile), collapse = ' '), shQuote(.data$tmpFile), shQuote(.data$tmpFile), shQuote(.data$outFile)
        )
      )
    tmpFiles = processed$tmpFile
    on.exit({
      unlink(tmpFiles)
    })

    processed = processed %>%
      dplyr::group_by(.data$period, .data$tile, .data$band) %>%
      dplyr::do({
        system(.data$command, ignore.stdout = TRUE)
        dplyr::as_tibble(data.frame(tileFile = .data$outFile, processed = TRUE, stringsAsFactors = FALSE))
      }) %>%
      dplyr::ungroup()
  }

  return(dplyr::bind_rows(processed, skipped))
}
