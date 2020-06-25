#' Generates a band storing a date of year based on the "which" band values and
#' dates extracted from file names used to compute the "which" band.
#' @param input a data frame describing tiled images (must contain at least
#'   columns \code{period, date, tile, band, tileFile})
#' @param targetDir a directory where tiles should be saved (a separate
#'   subdirectory for each tile will be created)
#' @param tmpDir a directory for temporary files
#' @param pythonDir a directory containing the \code{which.py} python script
#'   used to compute the output
#' @param outBandPrefix prefix used to create the output band name(s)
#' @param whichBandPrefix prefix used by the which band
#' @param skipExisting should already existing images be skipped?
#' @param blockSize processing block size used during computations - larger
#'   block requires more memory but (generally) makes computations faster
#' @return data frame describing generated images
#' @export
#' @import dplyr
prepareDoy = function(input, targetDir, tmpDir, pythonDir, outBandPrefix, whichBandPrefix, skipExisting, blockSize = 2048) {
  input = input %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      whichBand = paste0(whichBandPrefix, .data$band),
      doyBand = paste0(outBandPrefix, .data$band)
    ) %>%
    dplyr::mutate(
      whichFile = getTilePath(targetDir, .data$tile, .data$period, .data$whichBand),
      outFile = getTilePath(targetDir, .data$tile, .data$period, .data$doyBand)
    )

  skipped = processed = dplyr::tibble(period = character(), tile = character(), tileFile = character())
  if (skipExisting) {
    tmp = file.exists(input$outFile)
    skipped = input %>%
      dplyr::filter(tmp) %>%
      dplyr::select(.data$period, .data$tile, .data$outFile, .data$band) %>%
      dplyr::rename(tileFile = .data$outFile) %>%
      dplyr::distinct() %>%
      dplyr::mutate(band = paste0(outBandPrefix, .data$band))
    input = input %>%
      dplyr::filter(!tmp)
  }

  if (nrow(input) > 0) {
    createDirs(input$outFile)
    unlink(input$outFile)

    processed = input %>%
      dplyr::group_by(.data$period, .data$tile, .data$band) %>%
      dplyr::arrange(.data$period, .data$tile, .data$band, .data$date) %>%
      dplyr::summarize(
        doyBand = first(.data$doyBand),
        whichFile = first(.data$whichFile),
        outFile = first(.data$outFile),
        inputFiles = paste0(shQuote(.data$tileFile), collapse = ' ')
      ) %>% mutate(
        tmpFile = paste0(tmpDir, '/', basename(.data$outFile))
      ) %>%
      mutate(
        command = sprintf(
          'python3 %s/which2doy.py --blockSize %d %s %s %s && mv %s %s',
          pythonDir, blockSize, shQuote(.data$tmpFile), shQuote(.data$whichFile), .data$inputFiles, shQuote(.data$tmpFile), shQuote(.data$outFile)
        )
      )
    tmpFiles = processed$tmpFile
    on.exit({
      unlink(tmpFiles)
    })

    processed = processed %>%
      dplyr::group_by(.data$period, .data$tile, .data$band) %>%
      do({
        system(.data$command, ignore.stdout = TRUE)
        dplyr::as_tibble(data.frame(band = .data$doyBand, tileFile = .data$outFile, processed = TRUE, stringsAsFactors = FALSE))
      }) %>%
      dplyr::ungroup()
  }

  return(dplyr::bind_rows(processed, skipped))
}