args = commandArgs(TRUE)
args = c('~/roboty/BOKU/cube/cubeR/scripts/config/configZozlak.R', '_33UXP', '2018-03-01', '2018-04-30')
if (length(args) < 4) {
  stop('This scripts takes parameters: settingsFilePath regionId dateFrom dateTo')
}
names(args) = c('cfgFile', 'region', 'from', 'to')
t0 = Sys.time()
cat(paste0(c('Running landcover.R', args, as.character(Sys.time()), '\n'), collapse = '\t'))
source(args[1])

devtools::load_all(cubeRpath, quiet = TRUE)
library(sentinel2, quietly = TRUE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

data = readr::read_csv(landcoverTrainDataPath)
period = paste0(substr(args['from'], 1, 4), 'y1')
modelFile = prepareLandcoverModel(modelsDir, tmpDir, landcoverModelName, data, landcoverTargetVar, landcoverFeaturesList, period, nCores, landcoverSkipExistingModel)

images = getCache(args['region'], args['from'], args['to'], args['cfgFile']) %>%
  dplyr::filter(.data$band == 'SCL') %>%
  dplyr::mutate(period = substr(.data$date, 1, 4)) %>%
  dplyr::group_by(utm, period) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::rename(tileFile = file) %>%
  dplyr::ungroup()
regionFile = getCachePath(cacheTmpl, args['region'], args['from'], args['to'], cloudCov, bands, 'geojson')
tiles = suppressMessages(mapTilesGrid(images, gridFile, regionFile)) %>%
  dplyr::select(.data$tile, .data$period) %>%
  dplyr::mutate(period = paste0(.data$period, 'y1'))
imgs = prepareLandcover(images, tilesDir, tmpDir, modelFile, landcoverModelName, nCores, landcoverSkipExisting, landcoverGdalOpts)
logProcessingResults(imgs, t0)
