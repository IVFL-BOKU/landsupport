#!/usr/bin/python3

import argparse
import datetime
import osgeo.gdal
import numpy
import os
import re


parser = argparse.ArgumentParser(description='Converts which file output into day of years based on dates (YYYY-MM-DD) contained in the input file names')
parser.add_argument('--verbose', action='store_true')
parser.add_argument('--blockSize', type=int, default=3072)
parser.add_argument('--gdalCacheSize', type=int, default=2147483648)
parser.add_argument('--formatOptions', nargs='*', default=['COMPRESS=DEFLATE', 'TILED=YES', 'BLOCKXSIZE=512', 'BLOCKYSIZE=512'], help='output format options')
parser.add_argument('outFileName')
parser.add_argument('whichFileName')
parser.add_argument('inputFile', nargs='+')
args = parser.parse_args()

if args.gdalCacheSize is not None:
    gdal.SetCacheMax(args.gdalCacheSize)

valuesMap = [re.search('[0-9]{4}-[0-9]{2}-[0-9]{2}', i).group(0) for i in args.inputFile]
valuesMap = [int(datetime.datetime.strptime(i, '%Y-%m-%d').strftime('%-j')) for i in valuesMap]
outMax = max(valuesMap)

src = gdal.Open(args.whichFileName)
srcBand = src.GetRasterBand(1)
nodataSrc = int(srcBand.GetNoDataValue())

dataType = gdal.GDT_Byte if outMax < 255 else gdal.GDT_UInt16
nodataDst = 255 if outMax < 255 else 65535
valuesMap = valuesMap + [0] * (nodataSrc - len(valuesMap)) + [nodataDst]
valuesMap = numpy.array(valuesMap)

driver = gdal.GetDriverByName('GTiff')
dst = driver.Create(args.outFileName, src.RasterXSize, src.RasterYSize, 1, dataType, args.formatOptions)
dst.SetGeoTransform(src.GetGeoTransform())
dst.SetProjection(src.GetProjection())
dstBand = dst.GetRasterBand(1)
dstBand.SetNoDataValue(nodataDst)

t = datetime.datetime.now()
px = 0
while px < src.RasterXSize:
    bsx = min(args.blockSize, src.RasterXSize - px)
    py = 0
    while py < src.RasterYSize:
        if args.verbose:
            print('%d %d (%s)' % (px, py, datetime.datetime.now() - t))
        t = datetime.datetime.now()
        bsy = min(args.blockSize, src.RasterYSize - py)

        dataDst = valuesMap.take(srcBand.ReadAsArray(px, py, bsx, bsy))
        dstBand.WriteArray(dataDst, px, py)
        del dataDst
  
        py += bsy
    px += bsx

