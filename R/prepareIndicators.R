#' Computes indicator tiles
#' @details
#' Computation is done using gdal_calc.py.
#' @param input a data frame describing available raw tiles
#' @param targetDir a directory where tiles should be saved (a separate
#'   subdirectory for each tile will be created)
#' @param tmpDir a directory for temporary files
#' @param indicators a data frame describing indicators to be computed obtained
#'   from the \code{\link{indicatorsToTibble}} function
#' @param skipExisting should already existing indicator tiles be skipped?
#' @return data frame describing generated indicator tiles
#' @import dplyr
#' @export
prepareIndicators = function(input, targetDir, tmpDir, indicators, skipExisting = TRUE) {
  input = input %>%
    dplyr::ungroup()

  processed = indicators %>%
    dplyr::inner_join(input) %>%
    dplyr::mutate(tileFileBak = .data$tileFile) %>%
    dplyr::group_by(.data$date, .data$tile, .data$outBand, .data$resolution, .data$factor, .data$equation) %>%
    tidyr::nest() %>%
    dplyr::inner_join(
      indicators %>%
        dplyr::group_by(.data$outBand) %>%
        dplyr::summarize(nBands = dplyr::n_distinct(.data$band))
    ) %>%
    dplyr::mutate(
      outFile = getTilePath(targetDir, .data$tile, .data$date, .data$outBand)
    ) %>%
    ungroup()

  skipped = dplyr::tibble(tileFile = character())
  if (skipExisting) {
    tmp = file.exists(processed$outFile)
    skipped = processed %>%
      dplyr::filter(tmp) %>%
      dplyr::rename(band = .data$outBand, tileFile = .data$outFile) %>%
      dplyr::select(.data$date, .data$tile, .data$band, .data$tileFile)
    processed = processed %>%
      dplyr::filter(!tmp)
  }

  if (nrow(processed) > 0) {
    createDirs(processed$outFile)
    unlink(processed$outFile)

    processed = processed %>%
      # assuring input bands resolution matches output resolution
      dplyr::mutate(
        data = purrr::map2(.data$data, .data$resolution, function(x, y){
          filter = y != sapply(x$tileFileBak, function(f){
            raster::res(raster::raster(f))[1]
          })
          x$tileFile[filter] = paste0(tmpDir, '/', y, 'm_', basename(x$tileFileBak[filter]))
          x$command = dplyr::if_else(filter, sprintf('gdalwarp -q -tr %f %f %s %s', y, -y, shQuote(x$tileFileBak), shQuote(x$tileFile)), '') # ugly hardcoding of y resolution
          return(x)
        })
      ) %>%
      dplyr::mutate(
        inputBands = as.character(purrr::map(.data$data, function(x){
          return(paste0('-', x$name, ' ', shQuote(x$tileFile), collapse = ' '))
        })),
        tmpFile = paste0(tmpDir, '/', basename(.data$outFile))
      ) %>%
      dplyr::mutate(
        command = sprintf(
          'gdal_calc.py --quiet %s --calc "%f * %s" --outfile %s --overwrite --type Int16 --NoDataValue -32768 --co "COMPRESS=DEFLATE" --co "TILED=YES" --co "BLOCKXSIZE=512" --co "BLOCKYSIZE=512" && mv %s %s',
          .data$inputBands, .data$factor, .data$equation, shQuote(.data$tmpFile), shQuote(.data$tmpFile), shQuote(.data$outFile)
        )
      )
    tmpFiles = processed %>%
      dplyr::select(.data$data) %>%
      tidyr::unnest(.data$data) %>%
      dplyr::filter(.data$command != '') %>%
      dplyr::select(.data$tileFile) %>%
      unlist() %>%
      unique()
    tmpFiles = c(processed$tmpFile, tmpFiles)
    unlink(tmpFiles)
    on.exit({
      unlink(tmpFiles)
    })

    processed = processed %>%
      dplyr::group_by(.data$date, .data$tile, .data$outBand) %>%
      dplyr::do({
        preCommand = paste0(.data$data[[1]]$command[!file.exists(.data$data[[1]]$tileFile)], collapse = '; ')
        system(preCommand, ignore.stdout = TRUE)
        system(.data$command, ignore.stdout = TRUE)
        dplyr::as_tibble(data.frame(band = .data$outBand, tileFile = .data$outFile, processed = TRUE, stringsAsFactors = FALSE))
      }) %>%
      dplyr::ungroup()
  }

  return(bind_rows(processed, skipped))
}
