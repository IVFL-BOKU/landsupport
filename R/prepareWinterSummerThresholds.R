#' Computes winter/summer threshold mask
#' @param input data frame describing input - must provide \code{period},
#'   \code{tile}, \code{lcFile} and \code{<climVar>File} columns
#' @param targetDir directory storing computed masks
#' @param tmpDir directory for temporary files
#' @param thresholdBand output band name to be used for computed masks
#' @param skipExisting should already existing masks be skipped
#' @import dplyr
#' @export
prepareWinterSummerThresholds = function(input, targetDir, tmpDir, thresholdBand, skipExisting = TRUE) {
  if (nrow(input) > 0) {
    climateVars = utils::read.csv(input$modelFile[1], stringsAsFactors = FALSE) %>%
      dplyr::filter(coef != 'intercept') %>%
      dplyr::select(coef) %>%
      unlist()
  }

  input = input %>%
    dplyr::mutate(
      band = thresholdBand,
      tileFile = getTilePath(targetDir, .data$tile, .data$period, thresholdBand)
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

    coef = input %>%
      dplyr::group_by(.data$modelFile) %>%
      dplyr::do({
        utils::read.csv(.data$modelFile[1], stringsAsFactors = FALSE) %>%
          tidyr::spread('coef', 'value')
      })
    input = input %>%
      inner_join(coef)

    processed = input %>%
      dplyr::mutate(
        tmpFile = paste0(tmpDir, '/', basename(.data$tileFile))
      ) %>%
      dplyr::group_by(.data$period, .data$tile, .data$band, .data$tileFile, .data$tmpFile) %>%
      dplyr::do({
        inputLetters = LETTERS[1:(length(climateVars) + 1)]
        inputs = c(.data$lcFile, as.character(.data[, paste0(climateVars, 'File')]))
        inputs = paste0('-', inputLetters, ' ', shQuote(inputs), collapse = ' ')

        calcLetters = LETTERS[1 + 1:length(climateVars)]
        calcValues = as.numeric(.data[, climateVars])
        calc = paste0('(A >= 200) * (A < 300) * (', .data$intercept, ' + ', paste0(calcLetters, ' * ', calcValues, collapse = ' + '), ')')
        command = sprintf(
          'gdal_calc.py --quiet --overwrite --NoDataValue 0 %s --calc %s --type=Int16 --outfile=%s --co="COMPRESS=DEFLATE" --co="TILED=YES" --co="BLOCKXSIZE=512" --co="BLOCKYSIZE=512" && mv %s %s',
          inputs, shQuote(calc), shQuote(.data$tmpFile), shQuote(.data$tmpFile), shQuote(.data$tileFile)
        )
        dplyr::tibble(command = command)
      })
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
