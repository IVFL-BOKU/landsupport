#' Computes winter/summer crop mask by comparing day of maximum NDVI value with
#' a threshold
#' @param input data frame describing input - must provide \code{period},
#'   \code{tile}, \code{thresholdFile} and \code{doyFile} columns
#' @param targetDir directory storing computed masks
#' @param tmpDir directory for temporary files
#' @param bandName output band name to be used for computed masks
#' @param skipExisting should already existing masks be skipped
#' @import dplyr
#' @export
prepareWinterSummer = function(input, targetDir, tmpDir, bandName, skipExisting = TRUE) {
  input = input %>%
    dplyr::mutate(
      band = bandName,
      tileFile = getTilePath(targetDir, .data$tile, .data$period, bandName)
    )

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
      ) %>%
      dplyr::mutate(
        command = sprintf(
          'gdal_calc.py --quiet --overwrite -A %s -B %s --calc "1 + (A > B)" --type=Byte --outfile=%s --co="COMPRESS=DEFLATE" --co="TILED=YES" --co="BLOCKXSIZE=512" --co="BLOCKYSIZE=512" && mv %s %s',
          shQuote(.data$doyFile), shQuote(.data$thresholdFile), shQuote(.data$tmpFile), shQuote(.data$tmpFile), shQuote(.data$tileFile)
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
        dplyr::as_tibble(data.frame(tileFile = .data$tileFile, processed = TRUE, stringsAsFactors = FALSE))
      }) %>%
      dplyr::ungroup()
  }

  return(dplyr::bind_rows(processed, skipped))
}
