#!/usr/bin/python3

import argparse
import datetime
from osgeo import gdal
import numpy
import os
import re


parser = argparse.ArgumentParser()
parser.add_argument('--verbose', action='store_true')
parser.add_argument('--blockSize', type=int, default=4096)
parser.add_argument('--gdalCacheSize', type=int, default=2147483648)
parser.add_argument('--formatOptions', nargs='*', default=['COMPRESS=DEFLATE', 'TILED=YES', 'BLOCKXSIZE=512', 'BLOCKYSIZE=512'], help='output format options')
parser.add_argument('outFile')
parser.add_argument('whichFile')
parser.add_argument('inputFile', nargs='+')
args = parser.parse_args()

if args.gdalCacheSize is not None:
    gdal.SetCacheMax(args.gdalCacheSize)


tmpSrcFile = gdal.Open(args.inputFile[0])
tmpSrcBand = tmpSrcFile.GetRasterBand(1)
X = tmpSrcFile.RasterXSize
Y = tmpSrcFile.RasterYSize
nodata = tmpSrcBand.GetNoDataValue()
nBands = tmpSrcFile.RasterCount

which = gdal.Open(args.whichFile)
whichBand = which.GetRasterBand(1)
nodataWhich = whichBand.GetNoDataValue()

if X != which.RasterXSize or Y != which.RasterYSize:
    raise Exception('Source files dimentions do not match')

driver = gdal.GetDriverByName('GTiff')
dst = driver.Create(args.outFile, X, Y, nBands, tmpSrcBand.DataType, args.formatOptions)
dst.SetGeoTransform(tmpSrcFile.GetGeoTransform())
dst.SetProjection(tmpSrcFile.GetProjection())
dstBands = []
for i in range(nBands):
    tmpBand = dst.GetRasterBand(i + 1)
    if nodata is not None:
        tmpBand.SetNoDataValue(nodata)
    dstBands.append(tmpBand)

tmpSrcBand = None
tmpSrcFile = None

t = datetime.datetime.now()
px = 0
while px < X:
    bsx = min(args.blockSize, X - px)
    py = 0
    while py < Y:
        if args.verbose:
            print('%d %d (%s)' % (px, py, datetime.datetime.now() - t))
        t = datetime.datetime.now()
        bsy = min(args.blockSize, Y - py)

        dataWhich = whichBand.ReadAsArray(px, py, bsx, bsy)
        # dataWhich has shape (bsy, bsx)!

        for band in range(nBands):
            dataDst = numpy.zeros((bsy, bsx))

            for i in range(len(args.inputFile)):
                tmp = gdal.Open(args.inputFile[i])
                dataSrc = tmp.GetRasterBand(band + 1).ReadAsArray(px, py, bsx, bsy)
                dataDst = dataDst + dataSrc * (dataWhich == i)
                tmp = None
       
            if nodata is not None:
                dataDst[dataWhich == nodataWhich] = nodata
            dstBands[band].WriteArray(dataDst, px, py)

        py += bsy
    px += bsx

