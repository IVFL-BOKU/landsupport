#!/usr/bin/python3

import argparse
import datetime
from osgeo from osgeo import gdal
import numpy
import os
import re


parser = argparse.ArgumentParser(description='Computes a raster file denoting which input file contained a maximum value for a given pixel.')
parser.add_argument('--verbose', action='store_true')
parser.add_argument('--blockSize', type=int, default=3072)
parser.add_argument('--gdalCacheSize', type=int, default=2147483648)
parser.add_argument('--formatOptions', nargs='*', default=['COMPRESS=DEFLATE', 'TILED=YES', 'BLOCKXSIZE=512', 'BLOCKYSIZE=512'], help='output format options')
parser.add_argument('outFileName')
parser.add_argument('inputFile', nargs='+')
args = parser.parse_args()

if args.gdalCacheSize is not None:
    gdal.SetCacheMax(args.gdalCacheSize)

tmpSrcFile = gdal.Open(args.inputFile[0])
tmpSrcBand = tmpSrcFile.GetRasterBand(1)
X = tmpSrcFile.RasterXSize
Y = tmpSrcFile.RasterYSize
nodataSrc = tmpSrcBand.GetNoDataValue()

dataType = gdal.GDT_Byte if len(args.inputFile) < 255 else gdal.GDT_UInt16
nodataDst = 255 if len(args.inputFile) < 255 else 65535
driver = gdal.GetDriverByName('GTiff')
dst = driver.Create(args.outFileName, X, Y, 1, dataType, args.formatOptions)
dst.SetGeoTransform(tmpSrcFile.GetGeoTransform())
dst.SetProjection(tmpSrcFile.GetProjection())
dstBand = dst.GetRasterBand(1)
dstBand.SetNoDataValue(nodataDst)

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

        dataSrc = []
        for i in args.inputFile:
            tmp = gdal.Open(i)
            dataSrc.append(tmp.GetRasterBand(1).ReadAsArray(px, py, bsx, bsy))
            tmp = None

        dataSrc = numpy.stack(dataSrc)
        nodataMask = dataSrc == nodataSrc
        dataSrc[nodataMask] = numpy.iinfo(dataSrc.dtype).min
        dataMax = dataSrc.max(0)
        dataDst = numpy.zeros(dataSrc.shape, numpy.uint8)
        for i in range(len(args.inputFile)):
            dataDst[i, :, :] = numpy.where(dataSrc[i, :, :] == dataMax, i, nodataDst)
        del dataSrc, dataMax
        dataDst[nodataMask] = nodataDst
        del nodataMask
        dataDst = dataDst.min(0)
        dstBand.WriteArray(dataDst, px, py)
        del dataDst
  
        py += bsy
    px += bsx

