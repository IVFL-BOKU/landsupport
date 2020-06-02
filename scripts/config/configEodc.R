# directory where the package was cloned
cubeRpath = '/eodc/private/boku/software/cubeR'
# file containing the grid (every feture must provide the TILE property)
gridFile = '/eodc/private/boku/ACube2/shapes/Grid_LAEA5210_100K_polygons.shp'
# file with the Corine Land Cover map
lcFile = '/eodc/private/boku/ACube2/auxiliary/CLC_2018/CLC2018_CLC2018_V2018_20.tif'
# directory for storing temporary files
tmpDir = '/eodc/private/boku/ACube2/tmp'
# directory storing rasters before retiling
rawDir = '/eodc/private/boku/ACube2/raw'
# directory storing rasters aggregated to periods
periodsDir = '/eodc/private/boku/ACube2/periods'
# directory storing rasters after retiling to the target grid (see the gridFile parameter)
tilesDir = '/eodc/private/boku/ACube2/tiles'
# directory storing overviews
overviewsDir = '/eodc/private/boku/ACube2/overviews'
# directory storing models data
modelsDir = '/eodc/private/boku/ACube2/models'
# directory storing files organized for a rasdaman input
rasdamanDir = '/eodc/private/boku/ACube2/upload'
# raw images cache file path template (see `?getCachePath`)
cacheTmpl = '/eodc/private/boku/ACube2/cache/{region}_{dateFrom}_{dateTo}_{cloudCovMax}_{bands}'

# list of bands to be downloaded and tiled
bands = c('B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B11', 'B12', 'SCL', 'LAI', 'TCI', 'FAPAR', 'FCOVER')
# maximal accepted granules' cloud coverage
cloudCov = 0.4
# number of workers (cores)
nCores = 40
# each worker (core) is assigned chunksPerCore data chunks (generally you shouldn't need to tune this property)
chunksPerCore = 10

# download method "download", "copy" or "symlink"
# (two latter ones work only on machines with a direct access to the BOKU's EODC storage)
dwnldMethod = 'symlink'
# maximum accepted local files to be deleted during the download (to avoid hitting own foot)
dwnldMaxRemovals = 10000
# s2.boku.eodc.eu database connection paramerters required for the "symlink" download method
dwnldDbParam = list(host = '10.250.16.131', port = 5432, user = 'eodc', dbname = 'bokudata')
## parameters required for the "download" download method
# number of parallel downloads
dwnldNCores = 4
# see `?sentinel2::S2_download`
dwnldTimeout = 120
dwnldSkipExisting = 'samesize'
dwnldTries = 2

# description of cloud masks - parameters passed to prepareMasks() - see `?prepareMasks` (remember minArea and bufferSize unit is a 20m pixel)
maskParam = list(
#  list(bandName = 'CLOUDMASK1', minArea = 9L, bufferSize = 5L, invalidValues = c(0L:3L, 7L:11L), bufferedValues = c(3L, 8L:10L)),
  list(bandName = 'CLOUDMASK2', minArea = 0L, bufferSize = 0L, invalidValues = c(0L:3L, 7L:11L), bufferedValues = integer())
)
# should already existing masks be skipped (TRUE) or reprocessed anyway (FALSE)
maskSkipExisting = TRUE
# gdal creation options used for masks
maskGdalOpts = '-co "COMPRESS=DEFLATE" -co "TILED=YES" -co "BLOCKXSIZE=512" -co "BLOCKYSIZE=512"'
# gdal cache size used for mask creation
maskGdalCache = 1024

# indicators definitions
indicatorIndicators = list(
#  list(bandName = 'NDVI1',   resolution = 10, mask = 'CLOUDMASK1', factor = 10000, bands = c('A' = 'B08', 'B' = 'B04'), equation = '(A.astype(float) - B) / (0.0000001 + A + B)'),
  list(bandName = 'NDVI2',   resolution = 10, mask = 'CLOUDMASK2', factor = 10000, bands = c('A' = 'B08', 'B' = 'B04'), equation = '(A.astype(float) - B) / (0.0000001 + A + B)'),
  list(bandName = 'NDVI20',  resolution = 20, mask = 'CLOUDMASK2', factor = 10000, bands = c('A' = 'B08', 'B' = 'B04'), equation = '(A.astype(float) - B) / (0.0000001 + A + B)'),
  list(bandName = 'NDTI2',   resolution = 20, mask = 'CLOUDMASK2', factor = 10000, bands = c('A' = 'B11', 'B' = 'B12'), equation = '(A.astype(float) - B) / (0.0000001 + A + B)'),
  list(bandName = 'MNDWI2',  resolution = 20, mask = 'CLOUDMASK2', factor = 10000, bands = c('A' = 'B03', 'B' = 'B11'), equation = '(A.astype(float) - B) / (0.0000001 + A + B)'),
  list(bandName = 'NDBI2',   resolution = 20, mask = 'CLOUDMASK2', factor = 10000, bands = c('A' = 'B11', 'B' = 'B8A'), equation = '(A.astype(float) - B) / (0.0000001 + A + B)'),
  list(bandName = 'BSI2',    resolution = 20, mask = 'CLOUDMASK2', factor = 10000, bands = c('A' = 'B12', 'B' = 'B04', 'C' = 'B8A', 'D' = 'B02'), equation = '((A.astype(float) + B) - (C + D) ) / (0.0000001 + A + B + C + D)'),
  list(bandName = 'BLFEI2',  resolution = 20, mask = 'CLOUDMASK2', factor = 10000, bands = c('A' = 'B03', 'B' = 'B04', 'C' = 'B12', 'D' = 'B11'), equation = '((A.astype(float) + B + C) / 3 - D) / (0.0000001 + (A + B + C) / 3 + D)')
)
# should already existing indicator images be skipped (TRUE) or reprocessed anyway (FALSE)
indicatorSkipExisting = TRUE

# band names of bands used to compute within-a-period maxima (can be more than one band)
whichBands = list(
  '1 month' = c('NDVI2', 'NDVI20'),
  '1 year'  = c('NDVI2')
)
# prefix preppended to the orignal band name to get the target "which band name"
whichPrefix = 'NMAX'
# prefix preppended to the orignal band name to get the "day of year with a within-a-period maximum value band name" (if empty string this indicator is not computed)
whichDoyPrefix = 'DOYMAX'
# processing block size (affects memory usage)
whichBlockSize = 1024
# should already existing "which" images be skipped (TRUE) or reprocessed anyway (FALSE)
whichSkipExisting = TRUE

# band names of bands for which composites should be computed
compositeBands = list(
  band      = c('NDVI2',     'LAI',       'TCI',       'B02',       'B03',       'B04',       'B05',        'B06',        'B07',        'B08',       'B8A',        'B11',        'B12',        'FAPAR',     'FCOVER'),
  whichBand = c('NMAXNDVI2', 'NMAXNDVI2', 'NMAXNDVI2', 'NMAXNDVI2', 'NMAXNDVI2', 'NMAXNDVI2', 'NMAXNDVI20', 'NMAXNDVI20', 'NMAXNDVI20', 'NMAXNDVI2', 'NMAXNDVI20', 'NMAXNDVI20', 'NMAXNDVI20', 'NMAXNDVI2', 'NMAXNDVI2'),
  outBand   = c('NDVI2',     'LAI2',      'TCI2',      'B02',       'B03',       'B04',       'B05',        'B06',        'B07',        'B08',       'B8A',        'B11',        'B12',        'FAPAR2',    'FCOVER2')
)
# processing block size (affects memory usage)
compositeBlockSize = 1024
# should already existing composite images be skipped (TRUE) or reprocessed anyway (FALSE)
compositeSkipExisting = TRUE

# bands to be aggregated into quantiles
aggregateBands = c('NDVI2', 'NDTI2', 'MNDWI2', 'NDBI2', 'BSI2', 'BLFEI2')
# processing block size (affects memory usage)
aggregateBlockSize = 512
# quantiles to be computed
aggregateQuantiles = c(0.05, 0.5, 0.98)
# should rasters with valid acquisition counts be computed?
aggregateCounts = TRUE
# band which should be used to compute counts
aggregateCountsBand = 'NDVI2'
# counts output band name
aggregateCountsOutBand = 'N2'
# should already computed quantile images be skipped (TRUE) or reprocessed anyway (FALSE)
aggregateSkipExisting = TRUE

wintersummerModelName = 'WS'
wintersummerClimateFiles = c(
  temp = '/eodc/private/boku/ACube2/auxiliary/wc2.0/eu_wc2.0_bio_30s_01.tif',
  rain = '/eodc/private/boku/ACube2/auxiliary/wc2.0/eu_wc2.0_bio_30s_12.tif'
)
wintersummerDoyBand = 'DOYMAXNDVI2'
wintersummerNdviMaxBand = 'NDVI2q98'
wintersummerLcBand = 'LC'
wintersummerResamplingMethod = 'med'
wintersummerNdviMin = 2000
wintersummerGdalOpts = '--config GDAL_CACHEMAX 1024 -wm 1024 -multi -wo NUM_THREADS=2 -co "COMPRESS=DEFLATE" -co "TILED=YES" -co "BLOCKXSIZE=512" -co "BLOCKYSIZE=512"'
wintersummerSkipExisting = FALSE

tileRawBands = character()
tilePeriodBands = list(
  '1 month' = c('NDVI2', 'LAI2', 'TCI2', 'B02', 'B03', 'B04', 'B05', 'B06', 'B07', 'B08', 'B8A', 'B11', 'B12', 'FAPAR2', 'FCOVER2', 'DOYMAXNDVI2'),
  '1 year' = c(
    'DOYMAXNDVI2', 'N2',
    'NDVI2q05',  'NDVI2q50',  'NDVI2q98',
    'NDTI2q05',  'NDTI2q50',  'NDTI2q98',
    'MNDWI2q05', 'MNDWI2q50', 'MNDWI2q98',
    'NDBI2q05',  'NDBI2q50',  'NDBI2q98',
    'BSI2q05',   'BSI2q50',   'BSI2q98',
    'BLFEI2q05', 'BLFEI2q50', 'BLFEI2q98'
  )
)
# should already existing tiles be skipped (TRUE) or reprocessed anyway (FALSE)
tileSkipExisting = TRUE
# reprojection resampling algorithm - see `man gdalwap``
tileResamplingMethod = 'near'
# additional gdalwarp parameters used while reprojection & retiling - see `man gdalwap`
tileGdalOpts = '--config GDAL_CACHEMAX 1024 -multi -wo NUM_THREADS=2 -co "COMPRESS=DEFLATE" -co "TILED=YES" -co "BLOCKXSIZE=512" -co "BLOCKYSIZE=512"'

# list of indicator band names used in the urban classification
urbanIndicators = c(
  'BLFEI2q05', 'BLFEI2q50', 'BLFEI2q95',
  'NDVI2q05' , 'NDVI2q50' , 'NDVI2q98' ,
  'MNDWI2q05', 'MNDWI2q50', 'MNDWI2q95',
  'NDTI2q05' , 'NDTI2q50' , 'NDTI2q95' ,
  'NDBI2q05' , 'NDBI2q50' , 'NDBI2q95' ,
  'BSI2q05'  , 'BSI2q50'  , 'BSI2q95'
)
# directory with model files
urbanModelsDir = '/eodc/private/boku/ACube2_myscripts/RF/'
# urban classification output band name
urbanBand = 'RFClassProb'
# urban classification processing block size
urbanBlockSize = 2048
# additional gdalwarp parameters used while reprojection & retiling - see `man gdalwap`
urbanGdalOpts = '--config GDAL_CACHEMAX 1024 -multi -wo NUM_THREADS=2 -co "COMPRESS=DEFLATE" -co "TILED=YES" -co "BLOCKXSIZE=512" -co "BLOCKYSIZE=512"'
# should already existing output files be skipped (TRUE) or reprocessed anyway (FALSE)
urbanSkipExisting = TRUE

landcoverModelName = 'LC'
landcoverTrainDataPath = '/eodc/private/boku/ACube2/auxiliary/merged_EU_2018_training.csv'
landcoverTargetVar = 'classname'
landcoverFeaturesList = list(
  'april-august' = c('ndvi2q05_2018y1', 'ndti2q05_2018y1', 'bsi2q98_2018y1', 'blfei2q50_2018y1', 'ndti2q50_2018y1', 'b12_2018.08m1', 'ndvi2q50_2018y1', 'b03_2018.08m1', 'ndbi2q98_2018y1', 'b11_2018.08m1', 'b05_2018.08m1', 'b04_2018.08m1', 'blfei2q05_2018y1', 'bsi2q50_2018y1', 'b02_2018.08m1', 'b03_2018.07m1', 'ndti2q98_2018y1', 'b05_2018.07m1', 'b12_2018.07m1', 'ndvi2_2018.08m1', 'b03_2018.06m1', 'b04_2018.07m1', 'b11_2018.07m1', 'b02_2018.07m1', 'mndwi2q50_2018y1', 'b05_2018.06m1', 'b12_2018.05m1', 'b04_2018.06m1', 'ndvi2_2018.07m1', 'b03_2018.05m1', 'ndbi2q50_2018y1', 'fcover2_2018.08m1', 'blfei2q98_2018y1', 'b12_2018.06m1', 'fapar2_2018.08m1', 'lai2_2018.08m1', 'b02_2018.06m1', 'lai2_2018.05m1', 'b05_2018.05m1', 'ndbi2q05_2018y1', 'b03_2018.04m1', 'b11_2018.05m1', 'fcover2_2018.05m1', 'b12_2018.04m1', 'ndvi2q98_2018y1', 'b11_2018.06m1', 'ndvi2_2018.06m1', 'lai2_2018.07m1', 'fapar2_2018.05m1', 'mndwi2q05_2018y1', 'fcover2_2018.04m1', 'b05_2018.04m1', 'bsi2q05_2018y1'),
  'may-august'   = c('ndvi2q05_2018y1', 'bsi2q98_2018y1', 'ndti2q05_2018y1', 'blfei2q50_2018y1', 'ndti2q50_2018y1', 'ndvi2q50_2018y1', 'b12_2018.08m1', 'b03_2018.08m1', 'ndbi2q98_2018y1', 'b11_2018.08m1', 'b05_2018.08m1', 'b04_2018.08m1', 'bsi2q50_2018y1', 'blfei2q05_2018y1', 'b03_2018.07m1', 'b02_2018.08m1', 'ndti2q98_2018y1', 'b05_2018.07m1', 'b12_2018.07m1', 'ndvi2_2018.08m1', 'b03_2018.06m1', 'b04_2018.07m1', 'b11_2018.07m1', 'b02_2018.07m1', 'b03_2018.05m1', 'b05_2018.06m1', 'b04_2018.06m1', 'b12_2018.05m1', 'fcover2_2018.08m1', 'ndvi2_2018.07m1', 'mndwi2q50_2018y1', 'ndbi2q50_2018y1', 'b12_2018.06m1', 'fapar2_2018.08m1', 'lai2_2018.08m1', 'blfei2q98_2018y1', 'lai2_2018.05m1'),
  'yearly'       = c('ndvi2q05_2018y1', 'blfei2q50_2018y1', 'ndti2q05_2018y1', 'bsi2q98_2018y1', 'ndti2q50_2018y1', 'ndvi2q50_2018y1', 'blfei2q05_2018y1', 'ndbi2q98_2018y1', 'ndti2q98_2018y1', 'bsi2q50_2018y1', 'mndwi2q50_2018y1', 'blfei2q98_2018y1', 'ndvi2q98_2018y1', 'ndbi2q50_2018y1', 'mndwi2q05_2018y1', 'ndbi2q05_2018y1', 'bsi2q05_2018y1', 'doymaxndvi2_2018y1', 'temp_1900.01.01', 'mndwi2q98_2018y1', 'rain_1900.01.01')
)
landcoverGdalOpts = '--config GDAL_CACHEMAX 4096 -wm 2048 -multi -wo NUM_THREADS=2 -co "COMPRESS=DEFLATE" -co "TILED=YES" -co "BLOCKXSIZE=512" -co "BLOCKYSIZE=512"'
landcoverSkipExistingModel = TRUE
landcoverSkipExisting = TRUE

