#!/usr/bin/python3

import argparse
import gdal
import os
import re


parser = argparse.ArgumentParser()
parser.add_argument('--dataDir', default='/eodc/private/boku/ACube2/tiles', help='directory containing tiles subdirs')
parser.add_argument('--targetDir', default='/eodc/private/boku/ACube2/upload', help='base directory for band name-based rasdaman collection subdirs')
parser.add_argument('--dateFrom', default='1900-01-01')
parser.add_argument('--dateTo', default='3000-01-01')
parser.add_argument('match', help='string to be matched against raster file names (typically a band name, sometimes combined with a period indication, e.g. "NDVI2q98" or "m1_DOYMAXNDVI2")')
parser.add_argument('--tiles', nargs='*', default=[])
args = parser.parse_args()

ext = None

utms = os.listdir(args.dataDir)
utms.sort()
for utm in utms:
    if not os.path.isdir(os.path.join(args.dataDir, utm)):
        continue
    files = os.listdir(os.path.join(args.dataDir, utm))
    files.sort()
    for fl in files:
        if re.search(args.match, fl):
            localPath = os.path.join(args.dataDir, utm, fl)
            (date, band, tile) = fl[0:-4].split('_')
            if len(args.tiles) > 0 and tile not in args.tiles:
                continue

            period = re.sub('^([0-9]+(-[0-9]+)?(-[0-9]+)?)', '', date)
            if period == '':
                period = 'none'

            date = re.sub('^([0-9]+(-[0-9]+)?(-[0-9]+)?).*$', '\\1', date)
            if date > args.dateTo or date < args.dateFrom:
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

            collectionDir = os.path.join(args.targetDir, band)
            targetPath = os.path.join(collectionDir, '_'.join((date, period, band, tile)) + ext)

            if not os.path.exists(collectionDir):
                os.makedirs(collectionDir)
            if os.path.islink(targetPath):
                os.unlink(targetPath)
            os.symlink(localPath, targetPath)

