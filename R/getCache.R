#' Reads the raw image cache
#' @param region region name
#' @param dateFrom period beginning date
#' @param dateTo dateTo period ending date
#' @param cfgFile path to the config file
#' @return data frame describing raw images
getCache = function(region, dateFrom, dateTo, cfgFile) {
  source(cfgFile, local = TRUE)
  cachePath = getCachePath(get('cacheTmpl'), region, dateFrom, dateTo, get('cloudCov'), get('bands'))
  images = dplyr::as_tibble(utils::read.csv(cachePath, stringsAsFactors = FALSE))
  return(images)
}
