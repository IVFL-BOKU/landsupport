#!/usr/bin/python3

import argparse
import datetime
from osgeo import gdal
import itertools
import logging
import math
import multiprocessing
import numpy
import os
import osgeo
import re
import sys

from numba import jit


parser = argparse.ArgumentParser(description='Computes a gap-filled data seriesi for a given input geometry and time span from the cubeR package raw data image file structure.')
parser.add_argument('--verbose', action='store_true')
parser.add_argument('--blockSize', type=int, default=512, help='[defaults to %(default)s] Processing block size. The bigger the block, the more memory is needed (it is roughly `blockSize*(dateTo-dateFrom+1)*20` bytes) but processing should be done faster. On the other hand if input files are internally tiled using blockSize equal to the internal tile size is the best choice and using bigger blockSize is unlikely to give better performance.')
parser.add_argument('--gdalCacheSize', type=int, default=2048, help='[defaults to %(default)s] Maximum size of internal GDAL cache in MB. Consider setting a lower value if you run into memory problems.')
parser.add_argument('--nCores', type=int, default=1, help='[defaults to %(default)s] Number of parallel processes to use. The execution time benefits greatly from using multiple cores but memory consumption also scales lineary with the number of processes.')
parser.add_argument('--lmbd', type=float, default=1, help='[defaults to %(default)s] Whittaker smoother lambda parameter value.')
parser.add_argument('--period', type=int, default=10, help='[defaults to %(default)s] Output data period in days. Outputs will be generated for `dateOut_n = dateFrom + n*period [days]` for n from 0 up to `dateOut_n <= dateTo`.')
parser.add_argument('--formatOptions', nargs='*', default=['COMPRESS=DEFLATE', 'TILED=YES', 'BLOCKXSIZE=512', 'BLOCKYSIZE=512'], help='[defaults to %(default)s] Output format options.')
parser.add_argument('--algorithm', default='near', help='[defaults to %(default)s] Algorithm to be used to reproject input data to the input shape projection')
parser.add_argument('--maskBand', default='CLOUDMASK2', help='[defaults to %(default)s] Mask band name')
parser.add_argument('--nodataValue', default=32767, help='[defaults to %(default)s] Output data nodata value')
parser.add_argument('--dataDir', default='/eodc/private/boku/ACube2/raw', help='[defaults to %(default)s] Directory storying cubeR package raw image file structure.')
parser.add_argument('--geomCacheFile', default='cache.geojson', help='[defaults to %(default)s] Path to a file storying raw data tiles geometry cache. If it does not exist, the cache will be built and saved to this file.')
parser.add_argument('--tmpDir', default='/eodc/private/boku/ACube2/tmp', help='[defaults to %(default)s] Temporary dir location.')
parser.add_argument('shapeFileName', help='Path to a vector file defining the target geometry. Output files will be generated in the same projection as the shapeFileName projection.')
parser.add_argument('band', help='Name of band to be processed')
parser.add_argument('dateFrom')
parser.add_argument('dateTo')
parser.add_argument('outputDir', help='Directory to save output data to. Output file names are {date}_{band}_{tile}.tif where {tile} is taken from the outputTileName parameter.')
parser.add_argument('outputTileName', help='{tile} part value of the output file names - see the outputDir parameter description.')
args = parser.parse_args()

logging.basicConfig(stream=sys.stdout, level=logging.DEBUG if args.verbose else logging.INFO)

if args.gdalCacheSize is not None:
    gdal.SetCacheMax(args.gdalCacheSize * 1024 * 1024)

####################
# Read the input geometry
####################

shpSrc   = osgeo.ogr.Open(args.shapeFileName)
layerSrc = shpSrc.GetLayer()
geomSrc  = None
for ftr in layerSrc:
    if geomSrc is None:
        geomSrc = ftr.GetGeometryRef()
    else:
        geomSrc = geomSrc.Union(ftr.GetGeometryRef())
geomSrc  = geomSrc.Clone()
prjSrc   = layerSrc.GetSpatialRef().Clone()
layerSrc = None
shpSrc   = None
# prepare a cutline geometry
fileCut    = os.path.join(args.tmpDir, 'geom.geojson')
driverCut  = osgeo.ogr.GetDriverByName('GeoJSON')
shpCut     = driverCut.CreateDataSource(fileCut)
layerCut   = shpCut.CreateLayer('cutline', prjSrc, geomSrc.GetGeometryType())
featureCut = osgeo.ogr.Feature(layerCut.GetLayerDefn())
featureCut.SetGeometry(geomSrc)
layerCut.CreateFeature(featureCut)
featureCut = None
layerCut   = None
shpCut     = None

####################
# Maintain cache
####################

def cacheTileGeom(dataDir, tile):
    logging.debug('  %s' % (tile))

    for fl in os.scandir(os.path.join(dataDir, tile)):
        if fl.name.endswith('.tif'):
            # cache tile's geometry
            rastTile    = gdal.Open(fl.path)
            coordTile   = rastTile.GetGeoTransform()
            xminTile    = coordTile[0]
            yminTile    = coordTile[3]
            xmaxTile    = coordTile[0] + (rastTile.RasterXSize * coordTile[1]) + (rastTile.RasterYSize * coordTile[2])
            ymaxTile    = coordTile[3] + (rastTile.RasterXSize * coordTile[4]) + (rastTile.RasterYSize * coordTile[5])
            wktTile     = 'POLYGON ((%f %f, %f %f, %f %f, %f %f, %f %f))' % (xminTile, yminTile, xmaxTile, yminTile, xmaxTile, ymaxTile, xminTile, ymaxTile, xminTile, yminTile)
            return (tile, wktTile, rastTile.GetProjection())

if not os.path.exists(args.geomCacheFile):
    with open(args.geomCacheFile, 'w') as f:
        f.write('{"type": "FeatureCollection","features": []}')
shpCache   = osgeo.ogr.Open(args.geomCacheFile, 1)
layerCache = shpCache.GetLayer()
prjCache   = layerCache.GetSpatialRef()
# assure tile field exists
if layerCache.FindFieldIndex('tile', False) < 0:
    tileField = osgeo.ogr.FieldDefn('tile', osgeo.ogr.OFTString)
    layerCache.CreateField(tileField)
# find tiles missing in cache
utmsData   = os.listdir(args.dataDir)
tilesInCache = []
for ftr in layerCache:
    tilesInCache.append(ftr.GetField('tile'))
tilesToCache = set(os.listdir(args.dataDir)) - set(tilesInCache)
# iterate trough missing tiles
if len(tilesToCache) > 0:
    logging.info('Building cache for %d tiles' % len(tilesToCache))
blocks = []
for tile in tilesToCache:
    blocks.append((args.dataDir, tile))
with multiprocessing.Pool(args.nCores) as pool:
    newTiles = pool.starmap(cacheTileGeom, blocks)
# add missing cache entries
layerDefCache = layerCache.GetLayerDefn()
for i in newTiles:
    featureTile = osgeo.ogr.Feature(layerDefCache)
    featureTile.SetField('tile', i[0])
    geomTile    = osgeo.ogr.CreateGeometryFromWkt(i[1])
    prjTile     = osgeo.osr.SpatialReference()
    prjTile.ImportFromWkt(i[2])
    geomTile.Transform(osgeo.osr.CoordinateTransformation(prjTile, prjCache))
    featureTile.SetGeometry(geomTile)
    layerCache.CreateFeature(featureTile)

####################
# Find matching source files
####################

geomSrc.Transform(osgeo.osr.CoordinateTransformation(prjSrc, prjCache))
layerCache.SetSpatialFilter(geomSrc)
tiles = []
for layerTile in layerCache:
    tiles.append(layerTile.GetField('tile'))
logging.info('Matched %d tiles: %s' % (len(tiles), ', '.join(tiles)))
dates = {}
n = 0
for tile in tiles:
    for fn in os.listdir(os.path.join(args.dataDir, tile)):
        fnp = re.sub('[.][^.]+$', '', fn).split('_')
        if fnp[0] >= args.dateFrom and fnp[0] <= args.dateTo:
            if fnp[1] == args.band or fnp[1] == args.maskBand:
                if fnp[0] not in dates:
                    dates[fnp[0]] = {'bands': [], 'masks': []}
                if fnp[1] == args.band:
                    dates[fnp[0]]['bands'].append({'file': os.path.join(args.dataDir, tile, fn), 'band': fnp[1], 'date': fnp[0], 'tile': tile})
                    n += 1
                else:
                    dates[fnp[0]]['masks'].append({'file': os.path.join(args.dataDir, tile, fn), 'band': fnp[1], 'date': fnp[0], 'tile': tile})
dates = {key:value for (key, value) in dates.items() if len(value['bands']) == len(value['masks'])}
logging.info('Matched %d files on %d dates' % (n, len(dates)))
if n == 0:
    quit()

####################
# Cut and reproject
####################

def reproject(files, tmpDir, date, bandOut, algorithm, prjWkt, fileCut, xRes, yRes, blockSize):
    try:
        with nBlock.get_lock():
            nBlock.value  += 1
            logging.debug('Input %d / %d (%d%%)' % (nBlock.value, nBlocks, int(100 * nBlock.value / nBlocks)))
    except NameError: pass

    dstFile = os.path.join(args.tmpDir, '%s_%s.tif' % (date, bandOut))
    res = gdal.Warp(
        dstFile, 
        [x['file'] for x in files], 
        format='GTiff', # output format ("GTiff", etc...) 
        #outputBounds=None, # output bounds as (minX, minY, maxX, maxY) in target SRS
        #outputBoundsSRS=None, # SRS in which output bounds are expressed, in the case they are not expressed in dstSRS
        xRes=xRes, yRes=yRes, # output resolution in target SRS
        #targetAlignedPixels=False, # whether to force output bounds to be multiple of output resolution
        #width=0, height=0, # dimensions of the output raster in pixel
        #srcSRS=None, # source SRS
        dstSRS=prjWkt,  # output SRS
        #srcAlpha=False, # whether to force the last band of the input dataset to be considered as an alpha band
        #dstAlpha=False, # whether to force the creation of an output alpha band
        #warpOptions=None, # https://gdal.org/api/gdalwarp_cpp.html#_CPPv415GDALWarpOptions
        #errorThreshold=None, # error threshold for approximation transformer (in pixels)
        #warpMemoryLimit=None, # size of working buffer in bytes
        creationOptions=['TILED=YES', 'BLOCKXSIZE=%d' % blockSize, 'BLOCKYSIZE=%d' % blockSize], # list of creation options
        #outputType=GDT_Unknown, # output type (gdal.GDT_Byte, etc...)
        #workingType=GDT_Unknown, # working type (gdal.GDT_Byte, etc...)
        resampleAlg=algorithm, # resampling mode - https://gdal.org/programs/gdalwarp.html#cmdoption-gdalwarp-r
        #srcNodata=None, # source nodata value(s)
        #dstNodata=None, # output nodata value(s)
        multithread=True, # whether to multithread computation and I/O operations
        #tps=False, # whether to use Thin Plate Spline GCP transformer
        #rpc=False, # whether to use RPC transformer
        #geoloc=False, # whether to use GeoLocation array transformer
        #polynomialOrder=None, # order of polynomial GCP interpolation
        #transformerOptions=None, # list of transformer options
        cutlineDSName=fileCut, # cutline dataset name
        cutlineLayer='cutline', # cutline layer name
        #cutlineWhere=None, # cutline WHERE clause
        #cutlineSQL=None, # cutline SQL statement
        #cutlineBlend=None, # cutline blend distance in pixels
        cropToCutline=True, # whether to use cutline extent for output bounds
        #copyMetadata=True, # whether to copy source metadata
        #metadataConflictValue=None, # metadata data conflict value
        #setColorInterpretation=False, # whether to force color interpretation of input bands to output bands
    )
    if res:
        return (date, dstFile)
    else:
        return None

def reprojectPoolInit(nBlocks_, nBlock_):
    global nBlocks, nBlock
    nBlocks = nBlocks_
    nBlock  = nBlock_

prjSrcWkt = prjSrc.ExportToWkt()
dateTmp, fileTmp = reproject(dates[list(dates)[0]]['bands'], args.tmpDir, list(dates)[0], 'bands', args.algorithm, prjSrcWkt, fileCut, None, None, args.blockSize)
rastDest  = gdal.Open(fileTmp)
geotrDest = rastDest.GetGeoTransform()
prjDest   = rastDest.GetProjection()
dtypeDest = rastDest.GetRasterBand(1).DataType
X         = rastDest.RasterXSize
Y         = rastDest.RasterYSize
xRes      = geotrDest[1]
yRes      = geotrDest[5]
rastDest  = None

blocksBands = []
blocksMasks = []
for date, val in dates.items():
    if date != dateTmp:
        blocksBands.append((val['bands'], args.tmpDir, date, 'bands', args.algorithm, prjSrcWkt, fileCut, xRes, yRes, args.blockSize))
    blocksMasks.append((val['masks'], args.tmpDir, date, 'masks', args.algorithm, prjSrcWkt, fileCut, xRes, yRes, args.blockSize))

logging.info('Preparing inputs (%d x %d)' % (X, Y))
nBlock = multiprocessing.Value('i')
nBlock.value = 1
with multiprocessing.Pool(args.nCores, initializer=reprojectPoolInit, initargs=(len(blocksBands) + len(blocksMasks) + 1, nBlock)) as pool:
    inputBands = pool.starmap(reproject, blocksBands)
    inputMasks = pool.starmap(reproject, blocksMasks)
inputBands = dict(zip([x[0] for x in inputBands], [x[1] for x in inputBands]))
inputMasks = dict(zip([x[0] for x in inputMasks], [x[1] for x in inputMasks]))
inputBands[dateTmp] = fileTmp

####################
# Prepare outputs
####################

def createOutputFile(fileOut, X, Y, dtypeDest, formatOptions, geotrDest, prjDest, nodataValue):
    driver = gdal.GetDriverByName('GTiff')
    rastOut = driver.Create(fileOut, X, Y, 1, dtypeDest, formatOptions)
    rastOut.SetGeoTransform(geotrDest)
    rastOut.SetProjection(prjDest)
    rastOut.GetRasterBand(1).SetNoDataValue(nodataValue)

dateMin = datetime.datetime.strptime(min(list(dates)), '%Y-%m-%d').date()
dateMax = datetime.datetime.strptime(max(list(dates)), '%Y-%m-%d').date()
T = (dateMax - dateMin).days + 1

outputBands = []
blocksBands = []
for t in range(0, T, args.period):
    dateOut = (dateMin + datetime.timedelta(days=t)).isoformat()
    fileOut = os.path.join(args.outputDir, '%s_%s_%s.tif' % (dateOut, args.band, args.outputTileName))
    blocksBands.append((fileOut, X, Y, dtypeDest, args.formatOptions, geotrDest, prjDest, args.nodataValue))
    outputBands.append(fileOut)
with multiprocessing.Pool(args.nCores) as pool:
    pool.starmap(createOutputFile, blocksBands)

####################
# Compute
####################

@jit(nopython=True)
def whittakerC(l, T, y, w, z, P):
    c = numpy.empty((P, T), numpy.float32)
    d = numpy.empty((P, T), numpy.float32)
    for p in range(0, P):
        d[p][0] = w[p][0] + l
        c[p][0] = -l / d[p][0]
        z[p][0] = w[p][0] * y[p][0]
        for t in range(1, T - 1):
            d[p][t] = w[p][t] + 2 * l - c[p][t - 1] * c[p][t - 1] * d[p][t - 1]
            c[p][t] = -l / d[p][t]
            z[p][t] = w[p][t] * y[p][t] - c[p][t - 1] * z[p][t - 1]
        d[p][T - 1] = w[p][T - 1] + l - c[p][T - 2] * c[p][T - 2] * d[p][T - 2]
        if d[p][T - 1] > 0:
            z[p][T - 1] = (w[p][T - 1] * y[p][T - 1] - c[p][T - 2] * z[p][T - 2]) / d[p][T - 1]
        else:
            z[p][T - 1] = numpy.nan
        for t in range(T - 2, -1, -1):
            z[p][t] = z[p][t] / d[p][t] - c[p][t] * z[p][t + 1]

def processBlock(px, py, bsx, bsy):
    with nBlock.get_lock():
        nBlock.value  += 1
        logging.debug('Block %d / %d (%d%%)' % (nBlock.value, nBlocks, int(100 * nBlock.value / nBlocks)))

    P   = bsx * bsy

    y = numpy.zeros((P, T), numpy.float32)
    w = numpy.zeros((P, T), numpy.float32)
    z = numpy.empty((P, T), numpy.float32)

    # read input data
    for date, fn in inputBands.items():
        rastMask    = gdal.Open(inputMasks[date])
        bandMask    = rastMask.GetRasterBand(1)

        di         = (datetime.datetime.strptime(date, '%Y-%m-%d').date() - dateMin).days
        rastIn     = gdal.Open(fn)
        bandIn     = rastIn.GetRasterBand(1)
        y[:, di] = bandIn.ReadAsArray(px, py, bsx, bsy).reshape((P))
        w[:, di] = 1
        w[y[:, di] == bandIn.GetNoDataValue(), di] = 0
        w[bandMask.ReadAsArray(px, py, bsx, bsy).reshape((P)) == bandMask.GetNoDataValue(), di]   = 0

        bandMask = None
        rastMask = None
        bandIn   = None
        rastIn   = None

    # apply the whitakker smoother
    whittakerC(args.lmbd, T, y, w, z, P)

    # write output
    z[numpy.isnan(z)] = args.nodataValue
    di = 0
    while di < T:
        di2 = int(di / args.period)
        locks[di2].acquire()
        rastOut = gdal.Open(outputBands[di2], 1)
        bandOut = rastOut.GetRasterBand(1)
        bandOut.WriteArray(z[:, di].reshape((bsy, bsx)), px, py)
        bandOut = None
        rastOut = None
        locks[di2].release()
        di += period

# prepare common context for all worker processes
def poolInit(locks_, T_, inputBands_, inputMasks_, outputBands_, period_, dateMin_, nBlocks_, nBlock_):
    global locks, T, inputBands, inputMasks, outputBands, period, dateMin, nBlocks, nBlock
    locks       = locks_
    T           = T_
    inputBands  = inputBands_
    outputBands = outputBands_
    period      = period_
    dateMin     = dateMin_
    nBlocks     = nBlocks_
    nBlock      = nBlock_

blocks = []
px = 0
while px < X:
    bsx = min(args.blockSize, X - px)
    py = 0
    while py < Y:
        bsy = min(args.blockSize, Y - py)
        blocks.append((px, py, bsx, bsy))
        py += bsy
    px += bsx
logging.info('Computing outputs')
locks  = [multiprocessing.Lock() for x in inputBands]
nBlock = multiprocessing.Value('i')
nBlock.value = 0
with multiprocessing.Pool(args.nCores, initializer=poolInit, initargs=(locks, T, inputBands, inputMasks, outputBands, args.period, dateMin, len(blocks), nBlock)) as pool:
    pool.starmap(processBlock, blocks)

####################
# Cleanup
####################

for date, fn in inputBands.items():
    os.unlink(fn)
for date, fn in inputMasks.items():
    os.unlink(fn)

