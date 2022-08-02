#!/usr/bin/python3

import argparse
import os
import re


parser = argparse.ArgumentParser(description='Creates a directory with rasdaman import-ready files structure by creating symlinks to the cubeR native file structure.')
parser.add_argument('--dataDir', default='/media/GFTP/landsupport/cubeR/tiles/', help='directory containing tiles subdirs')
parser.add_argument('--dateFrom', default='1900-01-01')
parser.add_argument('--dateTo', default='3000-01-01')
parser.add_argument('--tilesFile', help='path to a file storing a list of tiles to be processed (one per line)')
parser.add_argument('--tiles', help='comma-separated list of tiles to be processed')
parser.add_argument('band', help='name of a band to be processed')
parser.add_argument('targetDir', help='directory to write symbolic links to')
args = parser.parse_args()

bandF = '_%s_' % args.band

tiles = []
if args.tilesFile is not None:
    with open(args.tilesFile) as f:
        tiles = f.readlines()
    tiles = [x.strip() for x in tiles]
if args.tiles is not None:
    tiles += args.tiles.split(',')

if not os.path.exists(args.targetDir):
    os.makedirs(args.targetDir, 0o770)

ext = None
try: from osgeo from osgeo import gdal
except: ext = '.tif'

utms = os.listdir(args.dataDir)
utms.sort()
if len(tiles) > 0:
  utms = [x for x in utms if x in tiles]
targetPaths = []
for utm in utms:
    if not os.path.isdir(os.path.join(args.dataDir, utm)):
        continue
    files = os.listdir(os.path.join(args.dataDir, utm))
    files.sort()
    for fl in files:
        if bandF in fl:
            localPath = os.path.join(args.dataDir, utm, fl)
            (date, band, tile) = fl[0:-4].split('_')
            if len(tiles) > 0 and tile not in tiles:
                continue

            period = re.sub('^([0-9]+(-[0-9]+)?(-[0-9]+)?)', '', date)
            if period == '':
                period = 'none'

            date = re.sub('^([0-9]+(-[0-9]+)?(-[0-9]+)?).*$', '\\1', date)
            if date > args.dateTo[0:len(date)] or date < args.dateFrom[0:len(date)]:
                continue
            if len(date) == 4:
                date += '-01'
            if len(date) == 7:
                date += '-01'

            # assume all files have same format
            if ext is None:
                ext = '.tif'
                if 'JP2' in gdal.Open(localPath).GetDriver().ShortName:
                    ext = '.jp2'

            targetPath = os.path.join(args.targetDir, '_'.join((date, period, band, tile)) + ext)

            if os.path.islink(targetPath) or os.path.isfile(targetPath):
                os.unlink(targetPath)
            os.symlink(localPath, targetPath)
            targetPaths.append(os.path.basename(targetPath))
toDel = set(os.listdir(args.targetDir)) - set(targetPaths)
for i in toDel:
    try:
        os.unlink(os.path.join(args.targetDir, i))
    except: pass

