#args = c('../config/config_monthly.R', 'LU_cube', '2020-01-01', '2020-01-31')
args = commandArgs(TRUE)
if (length(args) < 4) {
  stop('This scripts takes parameters: settingsFilePath regionId dateFrom dateTo')
}
names(args) = c('cfgFile', 'region', 'from', 'to')
cat(paste0(c('Running rasdaman.R', args, as.character(Sys.time()), '\n'), collapse = '\t'))
source(args[1])

devtools::load_all(cubeRpath, quiet = TRUE)
library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

regionFile = getCachePath(cacheTmpl, args['region'], args['from'], args['to'], cloudCov, bands, 'geojson')
tilesRaw = suppressMessages(
  getCache(args['region'], args['from'], args['to'], args['cfgFile']) %>%
  select(date, utm) %>%
  distinct() %>%
  group_by(utm) %>%
  filter(row_number() == 1) %>%
  ungroup()
)
if (length(tileRawBands) > 0) {
  images = suppressMessages(imagesToTiles(tilesRaw, rawDir, tileRawBands[1]))
} else {
  for (i in seq_along(tilePeriodBands)) {
    if (length(tilePeriodBands[[i]]) > 0) {
      images = suppressMessages(
        tilesRaw %>%
          mapTilesPeriods(names(tilePeriodBands)[i], args['from']) %>%
          group_by(utm) %>%
          summarize(date = min(period)) %>%
          ungroup() %>%
          imagesToTiles(periodsDir, tilePeriodBands[[i]][1])
      )
      break
    }
  }  
}
tiles = images %>%
  mapTilesGrid(gridFile, regionFile) %>%
  select(tile) %>%
  distinct()
tilesFile = tempfile('rasdaman', tmpDir, 'tilelist')
writeLines(tiles$tile, tilesFile)

bands = c(tileRawBands, unlist(tilePeriodBands))
passed = logical(length(bands)) %>%
  setNames(bands)
dirNames = sprintf('/%s%s_%s_%s_%s', rasdamanPrefix, bands, args['region'], args['from'], args['to']) %>%
  setNames(bands)
for (i in bands) {
  cmd = sprintf(
    'python3 %s --dataDir %s --dateFrom %s --dateTo %s --tilesFile %s %s %s',
    shQuote(paste0(cubeRpath, '/python/rename2rasdaman.py')), shQuote(tilesDir), 
    shQuote(args['from']), shQuote(args['to']),
    shQuote(tilesFile),
    shQuote(i), shQuote(paste0(rasdamanRasterDirHost, dirNames[i]))
  )
  res = system(cmd, ignore.stdout = TRUE)
  passed[i] = res == 0
}
unlink(tilesFile)

ingDirs = character(length(bands)) %>%
  setNames(bands)
for (i in bands) {
  ingDirs[i] = sprintf('%s/%s%s_%s_%s_%s', rasdamanIngredientDir, rasdamanPrefix, i, args['region'], args['from'], args['to'])
  cmd = sprintf(
    'python3 %s --serviceUrl %s --tmpDir %s --crsResolver %s --targetFile %s %s %s',
    shQuote(paste0(cubeRpath, '/python/createIngredient.py')), 
    shQuote(rasdamanUrl), shQuote(rasdamanTmpDir), shQuote(rasdamanCrsUrl),
    shQuote(paste0(rasdamanIngredientDir, dirNames[i], dirNames[i], '.json')),
    shQuote(paste0(rasdamanRasterDirContainer, dirNames[i])), shQuote(paste0(rasdamanPrefix, i)) 
  )
  res = system(cmd, ignore.stdout = TRUE)
  passed[i] = res == 0 & passed[i]
}

cat(sprintf('%d/%d/%d\ttotal/ok/processed\n', length(passed), sum(passed), length(passed)))
