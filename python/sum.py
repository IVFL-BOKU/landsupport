#!/usr/bin/python3

import argparse
import datetime
from osgeo import gdal
import numpy
import os
import re


parser = argparse.ArgumentParser()
parser.add_argument('--verbose', action='store_true')
parser.add_argument('--binary', action='store_true', help='should any value other than 0 be counted as 1?')
parser.add_argument('--includeZero', action='store_true', help='should 0 be counted as 1? (with --binary counts valid values for a pixel excluding nodata)')
parser.add_argument('--blockSize', type=int, default=512)
parser.add_argument('--gdalCacheSize', type=int, default=2147483648)
parser.add_argument('--formatOptions', nargs='*', default=['COMPRESS=DEFLATE', 'TILED=YES', 'BLOCKXSIZE=512', 'BLOCKYSIZE=512'], help='output format options')
parser.add_argument('outputFileName')
parser.add_argument('inputFile')
args = parser.parse_args()

if args.gdalCacheSize is not None:
    gdal.SetCacheMax(args.gdalCacheSize)

with open(args.inputFile) as fi:
    srcFiles = [i.strip() for i in fi.readlines()]
tmpSrcFile = gdal.Open(srcFiles[0])
tmpSrcBand = tmpSrcFile.GetRasterBand(1)
X = tmpSrcFile.RasterXSize
Y = tmpSrcFile.RasterYSize
nodata = tmpSrcBand.GetNoDataValue()

driver = gdal.GetDriverByName('GTiff')
dst = driver.Create(args.outputFileName, X, Y, 1, gdal.GDT_UInt16, args.formatOptions)
dst.SetGeoTransform(tmpSrcFile.GetGeoTransform())
dst.SetProjection(tmpSrcFile.GetProjection())
dstBand = dst.GetRasterBand(1)

tmpSrcBand = None
tmpSrcFile = None

t = datetime.datetime.now()
px = 0
while px < X:
    bsx = min(args.blockSize, X - px)
    py = 0
    while py < Y:
        bsy = min(args.blockSize, Y - py)
        if args.verbose:
            print('%d %d %d %d (%s)' % (px, py, bsx, bsy, datetime.datetime.now() - t))
        t = datetime.datetime.now()

        # read source data into array [y, x, time]
        dataSrc = []
        for i in srcFiles:
            tmp = gdal.Open(i)
            dataSrc.append(tmp.GetRasterBand(1).ReadAsArray(px, py, bsx, bsy))
        dataSrc = numpy.stack(dataSrc, -1)

        if args.includeZero and nodata != 0:
            dataSrc[dataSrc == 0] = 1
        dataSrc[dataSrc == nodata] = 0
        if args.binary:
            dataSrc = dataSrc != 0

        # sum along [y, x] and write back
        dataSrc = numpy.sum(dataSrc, -1)
        dstBand.WriteArray(dataSrc, px, py)

        py += bsy
    px += bsx

