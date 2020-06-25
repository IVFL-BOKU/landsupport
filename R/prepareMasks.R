#' Prepares valid pixel masks
#' @description Creates a binary valid pixels mask frpm an S2's SCL image.
#'
#' It is a two-steps procedure. The final mask marks pixels as valid if they
#' weren't marked invalid on any any of steps.
#'
#' In the first step groups of \code{bufferedValues} pixels are buffered with a
#' radius of a \code{bufferSize}. This step is applied only to groups of pixels
#' having area greater or equal to \code{minArea}. Pixels belonging to the
#' created buffers are considered invalid.
#'
#' In the second step all pixels in the \code{invalidValues} range are marked as
#' invalid.
#'
#' @param input a data frame describing tiled images obtained from
#'   \code{\link{prepareTiles}}
#' @param targetDir a directory where tiles should be saved (a separate
#'   subdirectory for each tile will be created)
#' @param tmpDir a directory for temporary files
#' @param bandName name of the created band
#' @param minArea min \code{bufferedPixels} group area to include the area in
#'   the buffering step (0 for no area-based pixel exclusion, see the
#'   description for details)
#' @param bufferSize size (in pixels) of a buffer created along areas having
#'   \code{bufferedPixels} values (use 0 for no buffer)
#' @param invalidValues a set of values in the S2's SCL file considered invalid
#' @param bufferedValues a set of values in th2 S2's SCL file processed in the
#'   buffering step (see the description)
#' @param skipExisting should already existing tiles be skipped?
#' @param gdalOutputOpts additional options passed to gdal commands, e.g. "-co something"
#' @param gdalCache gdal cache size (crucial for memory usage)
#' @return data frame describing created masks
#' @import dplyr
#' @export
prepareMasks = function(input, targetDir, tmpDir, bandName, minArea, bufferSize, invalidValues, bufferedValues, skipExisting = TRUE, gdalOutputOpts = '', gdalCache = 4096) {
  # integer arithmetic is much faster
  minArea = as.integer(minArea)
  bufferSize = as.integer(bufferSize)
  invalidValues = as.integer(invalidValues)
  bufferedValues = as.integer(bufferedValues)

  processed = input %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$band == 'SCL') %>%
    dplyr::rename(file = .data$tileFile) %>%
    dplyr::mutate(tileFile = getTilePath(targetDir, .data$tile, .data$date, bandName))

  skipped = dplyr::tibble(file = character(), tileFile = character())
  if (skipExisting) {
    tmp = file.exists(processed$tileFile)
    skipped = processed %>%
      dplyr::filter(tmp)
    processed = processed %>%
      dplyr::filter(!tmp)
  }

  if (nrow(processed) > 0L) {
    unlink(processed$tileFile)

    processed = processed %>%
      dplyr::group_by(.data$date, .data$tile) %>%
      do({
        mask = raster::raster(.data$file)

        if (minArea > 0L | bufferSize > 0L) {
          buffered = raster::getValues(mask)
          buffered = as.integer(buffered %in% bufferedValues)
          buffered = matrix(buffered, nrow = raster::nrow(mask), ncol = raster::ncol(mask))
        } else {
          buffered = 2L
        }

        if (minArea > 0L) {
          segments = mmand::components(buffered, mmand::shapeKernel(c(3L, 3L), type = 'diamond'))
          area = tabulate(segments)
          exclude = segments %in% which(area < minArea)
          buffered[exclude] = 0L
        }

        if (bufferSize > 0L) {
          # buffer with gdal_proximity cause it's 100 times faster then mmand::dilate() for large kernels
          tmp = raster::raster(mask)
          tmp = raster::setValues(tmp, as.vector(buffered))
          tmpFileIn = paste0(tmpDir, '/', .data$date, '_CLOUDS_', .data$tile, '.tif')
          raster::writeRaster(tmp, tmpFileIn, overwrite = TRUE, datatype = 'INT1U', NAflag = 255L)
          tmpFileOut = paste0(tmpDir, '/', .data$date, '_BUFFERED_', .data$tile, '.tif')
          command = sprintf(
            'GDAL_CACHEMAX=%d gdal_proximity.py -q %s %s -ot Byte -maxdist 10 -distunits PIXEL -fixed-buf-val 1 -values 1 -nodata 2 %s', 
            as.integer(gdalCache), shQuote(tmpFileIn), shQuote(tmpFileOut), gdalOutputOpts
          )
          unlink(tmpFileOut)
          system(command, ignore.stdout = TRUE)
          buffered = raster::getValues(raster::raster(tmpFileOut))
          unlink(c(tmpFileIn, tmpFileOut))
        }

        invalid = raster::getValues(mask)
        invalid = 255L * as.integer(invalid %in% invalidValues | is.na(invalid) | buffered < 2L)

        mask = raster::setValues(mask, invalid)
        tmpFile = paste0(tmpDir, '/', basename(.data$tileFile))
        raster::writeRaster(mask, tmpFile, overwrite = TRUE, datatype = 'INT1U', NAflag = 255L)

        tmpFile2 = paste0(tmpDir, '/mask_', basename(tmpFile))
        createDirs(.data$tileFile)
        command = sprintf(
          'GDAL_CACHEMAX=%d gdalwarp -q -overwrite -tr 10 10 %s -r near %s %s && mv %s %s',
          as.integer(gdalCache), gdalOutputOpts, shQuote(tmpFile), shQuote(tmpFile2), shQuote(tmpFile2), shQuote(.data$tileFile)
        )
        system(command, ignore.stdout = TRUE)
        unlink(tmpFile)

        dplyr::as_tibble(data.frame(band = bandName, tileFile = .data$tileFile, processed = TRUE, stringsAsFactors = FALSE))
      }) %>%
      dplyr::ungroup()
  }

  ret = processed %>%
    dplyr::bind_rows(skipped) %>%
    dplyr::select(-file)
  return(ret)
}
