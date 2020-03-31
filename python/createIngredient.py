#!/usr/bin/python
import argparse
import os


parser = argparse.ArgumentParser(description='Generates Rasdaman ingridient files for directories containing single coverage(band) data in the "{YYYY-MM-DD}_{whatever}" file naming schema')
parser.add_argument('--serviceUrl', default='http://localhost:9009/rasdaman/ows')
parser.add_argument('--tmpDir', default='/tmp')
parser.add_argument('--crsResolver', default='http://www.opengis.net/def/')
parser.add_argument('--mock', action="store_true")
parser.add_argument('--noWms', action="store_true")
parser.add_argument('--targetFile', help='location of a ingredient file to be created (by default "{dataDir}.json")')
parser.add_argument('dataDir')
parser.add_argument('coverageName')
args = parser.parse_args()

tmpl = """{{
  "config": {{
    "service_url": "{serviceUrl}",
    "tmp_directory": "{tmpDir}",
    "crs_resolver": "{crsResolver}",
    "default_crs": "http://www.opengis.net/def/crs/EPSG/0/3035",
    "mock": {mock},
    "automated": true
  }},
  "input": {{
    "coverage_id": "{name}",
    "paths": [
      "{dataDir}/*"
    ]
  }},
  "recipe": {{
    "name": "time_series_irregular",
    "options": {{
      "time_parameter": {{
        "filename": {{
          "regex": "^.*/([-0-9]+)_[^/]*$",
          "group": 1
        }},
        "datetime_format": "YYYY-MM-DD"
      }},
      "time_crs": "http://www.opengis.net/def/crs/OGC/0/AnsiDate",
      "wms_import": {wmsImport}
    }}
  }}
}}"""

targetFile = args.targetFile
if targetFile is None:
    targetFile = args.dataDir + '.json'

if not os.path.isdir(os.path.dirname(targetFile)):
    os.makedirs(os.path.dirname(targetFile), 0o770)
with open(targetFile, 'w') as f:
    f.write(tmpl.format(
        serviceUrl=args.serviceUrl, tmpDir=args.tmpDir, dataDir=args.dataDir, 
        crsResolver=args.crsResolver, 
        mock='true' if args.mock else 'false', 
        name=args.coverageName, 
        wmsImport='false' if args.noWms else 'true'
    ))

