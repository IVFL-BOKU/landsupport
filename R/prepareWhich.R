#' Generates a band storing a date within a period with maximum value of a given
#' band.
#' @details The date is encoded per pixel using an order of a (sorted) date
#' within a period, e.g. if a given period for a given tile consists of three
#' dates \code{2019-01-06, 2019-01-01 & 2019-01-11}, then \code{1} corresponds
#' to \code{2019-01-01}, \code{2} to \code{2019-01-06} and \code{3} to
#' {2019-01-11}.
#'
#' Processing is done within groups defined by \code{period & band}
#'
#' Processing is divided into two steps. \enumerate{ \item In the first step a
#' maximum value among all dates in a period is computed. \item In the second
#' step an output band is computed by comparing values at particular dates to
#' the maximum one. If the maximum value occurs for many dates, the last date is
#' taken. }
#' @param input a data frame describing tiled images (must contain at least
#'   columns \code{period, date, tile, band, tileFile})
#' @param targetDir a directory where tiles should be saved (a separate
#'   subdirectory for each tile will be created)
#' @param tmpDir a directory for temporary files
#' @param pythonDir a directory containing the \code{which.py} python script
#'   used to compute the output
#' @param outBandPrefix prefix used to create the output band name(s)
#' @param skipExisting should already existing images be skipped?
#' @param blockSize processing block size used during computations - larger
#'   block requires more memory but (generally) makes computations faster
#' @return data frame describing generated images
#' @export
#' @import dplyr
prepareWhich = function(input, targetDir, tmpDir, pythonDir, outBandPrefix, skipExisting, blockSize = 2048) {
  input = input %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      whichBand = paste0(outBandPrefix, .data$band)
    ) %>%
    dplyr::mutate(
      outFile = getTilePath(targetDir, .data$tile, .data$period, .data$whichBand)
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
        whichBand = first(.data$whichBand),
        outFile = first(.data$outFile),
        inputFiles = paste0(shQuote(.data$tileFile), collapse = ' ')
      ) %>% mutate(
        tmpFile = paste0(tmpDir, '/', basename(.data$outFile))
      ) %>%
      mutate(
        command = sprintf(
          'python3 %s/which.py --blockSize %d %s %s && mv %s %s',
          pythonDir, blockSize, shQuote(.data$tmpFile), .data$inputFiles, shQuote(.data$tmpFile), shQuote(.data$outFile)
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
        dplyr::as_tibble(data.frame(band = .data$whichBand, tileFile = .data$outFile, processed = TRUE, stringsAsFactors = FALSE))
      }) %>%
      dplyr::ungroup()
  }

  return(dplyr::bind_rows(processed, skipped))
}