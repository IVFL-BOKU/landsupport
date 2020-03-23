cubeRpath = '/home/zozlak/roboty/BOKU/cube/cubeR'
gridFile = '/home/zozlak/roboty/BOKU/cube/data/shapes/Grid_LAEA5210_100K_polygons.shp'
lcFile = '/home/zozlak/roboty/BOKU/cube/data/shapes/CLC2018_CLC2018_V2018_20.tif'
tmpDir = '/home/zozlak/roboty/BOKU/cube/data/tmp'
rawDir = '/home/zozlak/roboty/BOKU/cube/data/raw'
periodsDir = '/home/zozlak/roboty/BOKU/cube/data/periods'
tilesDir = '/home/zozlak/roboty/BOKU/cube/data/tiles'
overviewsDir = '/home/zozlak/roboty/BOKU/cube/data/overviews'
modelsDir = '/home/zozlak/roboty/BOKU/cube/data/models'
rasdamanDir = '/home/zozlak/roboty/BOKU/cube/data/upload'
cacheTmpl = '/home/zozlak/roboty/BOKU/cube/data/cache/{region}_{dateFrom}_{dateTo}_{cloudCovMax}_{bands}'

bands = c('B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B11', 'B12', 'SCL', 'LAI', 'TCI', 'FAPAR', 'FCOVER')
cloudCov = 0.4
nCores = 6
chunksPerCore = 10

dwnldMethod = 'copy'
dwnldDbParam = list(host = '127.0.0.1', port = 5433, user = 'zozlak', dbname = 'bokudata')
dwnldMaxRemovals = 2
dwnldNCores = 4
dwnldTimeout = 120
dwnldSkipExisting = 'samesize'
dwnldTries = 2

maskParam = list(
  list(bandName = 'CLOUDMASK1', minArea = 9L, bufferSize = 5L, invalidValues = c(0L:3L, 7L:11L), bufferedValues = c(3L, 8L:10L)),
  list(bandName = 'CLOUDMASK2', minArea = 0L, bufferSize = 0L, invalidValues = c(0L:3L, 7L:11L), bufferedValues = integer())
)
maskSkipExisting = FALSE

indicatorIndicators = list(
  list(bandName = 'NDVI2',  resolution = 10, mask = 'CLOUDMASK2', factor = 10000, bands = c('A' = 'B08', 'B' = 'B04'), equation = '(A.astype(float) - B) / (0.0000001 + A + B)')
)
indicatorSkipExisting = TRUE

whichBands = c('NDVI2')
whichPrefix = 'NMAX'
whichDoyPrefix = 'DOYMAX'
whichBlockSize = 2048
whichSkipExisting = FALSE

compositeBands = list(
  band      = c('NDVI1',     'LAI',       'TCI',       'NDVI2',     'LAI',       'TCI'),
  whichBand = c('NMAXNDVI1', 'NMAXNDVI1', 'NMAXNDVI1', 'NMAXNDVI2', 'NMAXNDVI2', 'NMAXNDVI2'),
  outBand   = c('NDVI1',     'LAI1',      'TCI1',      'NDVI2',     'LAI2',      'TCI2')
)
compositeBlockSize = 2048
compositeSkipExisting = FALSE

aggregateBands = c('NDVI2', 'NDTI2', 'MNDWI2', 'NDBI2', 'BSI2', 'BLFEI2')
aggregateBlockSize = 512
aggregateQuantiles = c(0.05, 0.5, 0.95)
aggregateCounts = TRUE
aggregateCountsBand = 'NDVI2'
aggregateCountsOutBand = 'N2'
aggregateSkipExisting = FALSE

wintersummerModelName = 'WS'
wintersummerClimateFiles = c(
  temp = '/home/zozlak/roboty/BOKU/cube/data/shapes/wc2.0_2.5m_bio/eu_wc2.0_bio_01_30s.tif',
  rain = '/home/zozlak/roboty/BOKU/cube/data/shapes/wc2.0_2.5m_bio/eu_wc2.0_bio_10_30s.tif'
)
wintersummerDoyBand = 'DOYMAXNDVI2'
wintersummerNdviMaxBand = 'NDVI2q98'
wintersummerLcBand = 'LC'
wintersummerResamplingMethod = 'med'
wintersummerNdviMin = -10000
wintersummerGdalOpts = '--config GDAL_CACHEMAX 4096 -wm 2048 -multi -wo NUM_THREADS=2 -co "COMPRESS=DEFLATE" -co "TILED=YES" -co "BLOCKXSIZE=512" -co "BLOCKYSIZE=512"'
wintersummerSkipExisting = TRUE

tileRawBands = character()
tilePeriodBands = list(
  '1 month' = c('LAI1', 'NDVI1', 'TCI1', 'LAI2', 'NDVI2', 'TCI2'),
  '1 year' = c('NDVI2q05', 'NDVI2q50', 'NDVI2q95')
)
tileResamplingMethod = 'near'
tileGdalOpts = '--config GDAL_CACHEMAX 4096 -wm 2048 -multi -wo NUM_THREADS=2 -co "COMPRESS=DEFLATE" -co "TILED=YES" -co "BLOCKXSIZE=512" -co "BLOCKYSIZE=512"'
tileSkipExisting = FALSE

