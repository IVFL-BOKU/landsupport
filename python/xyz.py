#!/usr/bin/python3

import argparse
import datetime
from osgeo from osgeo import gdal
import math
import multiprocessing
import ogr
import os
import osr

def step1(param):
  XYZ.step1(**param)
  
def step2(param):
  XYZ.step2(**param)

class XYZ(object):
  spatRefs = {}
  spatTrans = {}

  rasterFile = None
  cursor = None
  query = None

  def __init__(self, rasterFile):
    self.rasterFile = rasterFile

  def makeTiles(self, targetDir, resAlg='bilinear', resAlg2='mean', maxZoom=None, minZoom = 0, formatOptions=[], verbose=1, parallel = 1):
    raster = gdal.Open(self.rasterFile)
    proj = raster.GetProjection()
    (x0, y0, x1, y1) = self.getBBox(raster)
    z = self.raster2z(raster)
    if maxZoom is not None:
      z = min(z, maxZoom)
    (X0, Y0) = self.coord2xy(x0, y0, proj, z) # first tile
    (X1, Y1) = self.coord2xy(x1, y1, proj, z) # last tile
    res = self.z2res(z)

    pool = multiprocessing.Pool(parallel)

    # most detailed tiles
    args = []
    for x in range(X0, X1 + 1):
      for y in range(Y0, Y1 + 1):
          args.append({'x': int(x), 'y': int(y), 'z': z, 'X0': X0, 'X1': X1, 'Y0': Y0, 'Y1': Y1, 'targetDir': targetDir, 'rasterFile': self.rasterFile, 'resAlg': resAlg, 'res': res, 'formatOptions': formatOptions, 'verbose': verbose})
    pool.map(step1, args, 1)
  
    # building piramids
    for zz in range(z - 1, minZoom - 1, -1):
      args = []
      (X0, Y0, X1, Y1) = (int(X0 / 2), int(Y0 / 2), int(X1 / 2), int(Y1 / 2))
      for x in range(X0, X1 + 1):
        for y in range(Y0, Y1 + 1):
          args.append({'x': int(x), 'y': int(y), 'z': int(zz), 'X0': X0, 'X1': X1, 'Y0': Y0, 'Y1': Y1, 'targetDir': targetDir, 'rasterFile': self.rasterFile, 'resAlg': resAlg, 'res': res, 'formatOptions': formatOptions, 'verbose': verbose})
      pool.map(step2, args, 1)

  @staticmethod
  def step1(x, y, z, X0, X1, Y0, Y1, targetDir, rasterFile, resAlg, res, formatOptions, verbose):
    if verbose >= 1:
      print('Zoom level %d, x %d/%d, y %d/%d, time %s' % (z, x - X0, X1 - X0, y - Y0, Y1 - Y0, str(datetime.datetime.now())))
    (x0, y0) = XYZ.xy2coord(x, y + 1, 3857, z)
    (x1, y1) = XYZ.xy2coord(x + 1, y, 3857, z)
    dstFile = XYZ.xyz2path(targetDir, z, x, y)
    gdal.Warp(dstFile, rasterFile, resampleAlg=resAlg, outputBounds=(x0, y0, x1, y1), dstSRS='EPSG:3857', xRes=res, yRes=res, format='GTiff', creationOptions=formatOptions)

  @staticmethod
  def step2(x, y, z, X0, X1, Y0, Y1, targetDir, rasterFile, resAlg, res, formatOptions, verbose):
    if verbose >= 1:
      print('Zoom level %d, x %d/%d, y %d/%d, time %s' % (z, x - X0, X1 - X0, y - Y0, Y1 - Y0, str(datetime.datetime.now())))
    inputFiles = []
    for xx in xrange(2):
      for yy in xrange(2):
        path = XYZ.xyz2path(targetDir, z + 1, 2 * x + xx, 2 * y + yy)
        if os.path.isfile(path):
          inputFiles.append(path)
    if len(inputFiles) > 0:
      (x0, y0) = XYZ.xy2coord(x, y + 1, 3857, z)
      (x1, y1) = XYZ.xy2coord(x + 1, y, 3857, z)
      res = 40075016.686 / 256.0 / (2 ** z)
      dstFile = XYZ.xyz2path(targetDir, z, x, y)
      gdal.Warp(dstFile, inputFiles, resampleAlg=resAlg, outputBounds=(x0, y0, x1, y1), xRes=res, yRes=res, format='GTiff', creationOptions=formatOptions)

  """ Returns a properly orientated bounding box (x0, y0, x1, y1) in raster's projection
  where x0 < x1 and y0 > y1 (the XYZ tiles go from north to south!)
  """
  def getBBox(self, raster):
    (x0, dx, sx, y0, sy, dy) = raster.GetGeoTransform()
    x1 = x0 + dx * raster.RasterXSize
    y1 = y0 + dy * raster.RasterYSize
    return (min(x0, x1), max(y0, y1), max(x0, x1), min(y0, y1))

  @staticmethod
  def xyz2path(targetDir, z, x, y):
    z = int(z)
    x = int(x)
    y = int(y)
    d = os.path.join(targetDir, str(z), '%d_%d' % (int(x / 100), int(y / 100)))
    path = os.path.join(d, '%d_%d_%d.tif' % (z, x, y))
    try:
      os.makedirs(d, 0o770)
    except OSError as e:
      if e.errno != 17:
        raise e
    return path

  @staticmethod
  def raster2z(raster):
    (x0, dx, sx, y0, sy, dy) = raster.GetGeoTransform()
    proj = raster.GetProjection()
    (mx0, my0) = XYZ.transform(x0, y0, proj, 3857)
    (mx1, my1) = XYZ.transform(x0 + dx * raster.RasterXSize, y0 + dy * raster.RasterYSize, proj, 3857)
    res = abs(mx1 - mx0) / raster.RasterXSize
    z = XYZ.res2z(res)
    return z

  @staticmethod
  def z2res(z):
    return 40075016.686 / 256.0 / (2 ** z)

  @staticmethod
  def res2z(resInM):
    return int(math.ceil(math.log(40075016.686 / 256.0 / resInM, 2)))

  @staticmethod
  def coord2xy(lon, lat, proj, z):
    (lon, lat) = XYZ.transform(lon, lat, proj, 4326)
    lat = math.radians(lat)
    n = 2 ** z
    x = int((lon + 180.0) / 360.0 * n)
    y = int((1.0 - math.log(math.tan(lat) + (1.0 / math.cos(lat))) / math.pi) / 2.0 * n)
    return (x, y)

  @staticmethod
  def xy2coord(x, y, proj, z):
    n = 2.0 ** z
    lon = x / n * 360.0 - 180.0
    lat = math.degrees(math.atan(math.sinh(math.pi * (1 - 2 * y / n))))
    return XYZ.transform(lon, lat, 4326, proj)

  @staticmethod
  def transform(x, y, inProj, outProj):
    for proj in (inProj, outProj):
      if not str(proj) in XYZ.spatRefs:
        tmp = osr.SpatialReference()
        if isinstance(proj, (int, long)):
          tmp.ImportFromEPSG(proj)
        else:
          tmp.ImportFromWkt(proj)
        XYZ.spatRefs[str(proj)] = tmp

    t = str(inProj) + '=>' + str(outProj)
    if t not in XYZ.spatTrans:
      XYZ.spatTrans[t] = osr.CoordinateTransformation(XYZ.spatRefs[str(inProj)], XYZ.spatRefs[str(outProj)])

    point = ogr.Geometry(ogr.wkbPoint)
    point.AddPoint(x, y)
    point.Transform(XYZ.spatTrans[t])
    return (point.GetX(), point.GetY())

parser = argparse.ArgumentParser(description='Cuts given file into XYZ tiles')
parser.add_argument('--algorithm', default='bilinear', help='resampling algorithm to be used for the highest resolution')
parser.add_argument('--algorithm2', default='average', help='resampling algorithm to be used for downsampling')
parser.add_argument('--minZoom', type=int, default=3, help='minimal zoom level')
parser.add_argument('--maxZoom', type=int, help='maximal zoom level (derived from the input data resolution if not provided)')
parser.add_argument('--verbose', action='store_true')
parser.add_argument('--gdalCacheSize', type=int, default=1024)
parser.add_argument('--formatOptions', nargs='*', default=['COMPRESS=DEFLATE', 'TILED=YES', 'BLOCKXSIZE=512', 'BLOCKYSIZE=512'], help='output format options')
parser.add_argument('--parallel', type=int, default=1)
parser.add_argument('inputFile')
parser.add_argument('outputDir')
args = parser.parse_args()

if args.gdalCacheSize is not None:
    gdal.SetCacheMax(args.gdalCacheSize)

xyz = XYZ(args.inputFile)
xyz.makeTiles(args.outputDir, args.algorithm, args.algorithm2, args.maxZoom, args.minZoom, args.formatOptions, int(args.verbose), args.parallel)

