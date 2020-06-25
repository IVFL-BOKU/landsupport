#' Generates composites for periods
#' @details
#' Composites are made by taking value for a given pixel corresponding
#' to a date indicated by a value from a separate file.
#' @param input a data frame describing tiles to be composited (must contain
#'   columns \code{date, tile, band, period, tileFile, whichFile})
#' @param targetDir a directory where computed aggregates should be stored
#' @param tmpDir a directory for temporary files
#' @param pythonDir a directory containing the \code{which.py} python script
#'   used to compute the output
#' @param skipExisting should already existing images be skipped?
#' @param blockSize processing block size used during computations - larger
#'   block requires more memory but (generally) makes computations faster
#' @return data frame describing computed aggregated images
#' @import dplyr
#' @export
prepareComposites = function(input, targetDir, tmpDir, pythonDir, skipExisting = TRUE, blockSize = 2048) {
  input = input %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      outFile = getTilePath(targetDir, .data$tile, .data$period, .data$band)
    )

  skipped = processed = dplyr::tibble(period = character(), tile = character(), band = character(), tileFile = character())
  if (skipExisting) {
    tmp = file.exists(input$outFile)
    skipped = input %>%
      dplyr::filter(tmp) %>%
      dplyr::select(.data$period, .data$tile, .data$band, .data$outFile) %>%
      dplyr::rename(tileFile = .data$outFile) %>%
      dplyr::distinct()
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
        outFile = dplyr::first(.data$outFile),
        whichFile = dplyr::first(.data$whichFile),
        inputFiles = paste0(shQuote(.data$tileFile), collapse = ' ')
      ) %>% mutate(
        tmpFile = paste0(tmpDir, '/', basename(.data$outFile))
      ) %>%
      dplyr::mutate(
        command = sprintf(
          'python3 %s/at-which.py --blockSize %d %s %s %s && mv %s %s',
          pythonDir, blockSize, shQuote(.data$tmpFile), shQuote(.data$whichFile), .data$inputFiles, shQuote(.data$tmpFile), shQuote(.data$outFile)
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
