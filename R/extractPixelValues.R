#' Extracts pixel values from a given file
#' @param x pixel column coordinates
#' @param y pixel row coordinates
#' @param file path to the file
#' @return vector of values
#' @export
extractPixelValues = function(x, y, file) {
  r = raster::raster(file)
  xy = raster::extract(r, y * nrow(r) + x + 1)
  return(xy)
}