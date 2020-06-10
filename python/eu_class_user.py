import argparse
import itertools
import json
import os
import psycopg2
import re
import requests
import shutil
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument('--dbConf')
parser.add_argument('--rasdamanUrl', default='http://rasdaman:9009/rasdaman/ows')
parser.add_argument('--coveragePrefix')
parser.add_argument('--tmpDir', default='/cuber/tmp')
parser.add_argument('--modelDir', default='/cuber/cubeR/scripts')
parser.add_argument('--rasdamanRasterDir', default='/geodata/runs')
parser.add_argument('--rasterDir', default='/ras_data/runs')
parser.add_argument('--recipeDir', default='/ras_recipe')
parser.add_argument('--legendDir', default='/legends')
parser.add_argument('--monthFrom', type=int, default=4)
parser.add_argument('--monthTo', type=int, default=10)
parser.add_argument('--maxFeatures', type=int, default=50)
parser.add_argument('--fontSize', type=int, default=10)
parser.add_argument('--minCoverage', type=int, default=98)
parser.add_argument('--minClassProb', type=int, default=20)
parser.add_argument('--blockSize', type=int, default=20000)
parser.add_argument('--nCores', type=int, default=4)
parser.add_argument('--noCleanup', action='store_true')
parser.add_argument('runId', type=int)
args = parser.parse_args()

conn = psycopg2.connect(args.dbConf)
cur = conn.cursor()

try:
  # 1. Prepare data for the model

  cur.execute("UPDATE system.runs SET status = 1, starttime = now(), endtime = null WHERE id = %s", (args.runId, ))
  conn.commit()
  cur.execute("SELECT payload FROM system.runs WHERE id = %s", (args.runId, ))
  inputParam = json.loads(cur.fetchone()[0])
  inputParam['roi'] = json.loads(inputParam['roi'])

  runName = 'eu_class_user_' + str(args.runId)
  inputParam['runId'] = args.runId
  inputParam['dbConf'] = args.dbConf
  inputParam['rasdamanUrl'] = args.rasdamanUrl
  inputParam['coveragePrefix'] = args.coveragePrefix
  inputParam['tmpDir'] = os.path.join(args.tmpDir, runName)
  inputParam['rasterDir'] = os.path.join(args.rasterDir, runName)
  inputParam['legendFilePng'] = os.path.join(args.legendDir, runName + '_class.png')
  inputParam['legendFileJson'] = os.path.join(inputParam['tmpDir'], 'legend.json')
  inputParam['roiFile'] = os.path.join(inputParam['tmpDir'], 'roi.geojson')
  inputParam['logFile'] = os.path.join(inputParam['tmpDir'], 'log')
  inputParam['validationFile'] = os.path.join(inputParam['tmpDir'], 'validation.json')  # leave empty for no validation
  inputParam['minDataCoverage'] = args.minCoverage / 100
  inputParam['minClassProb'] = args.minClassProb / 100
  inputParam['blockSize'] = args.blockSize # in meters
  inputParam['monthMin'] = args.monthFrom
  inputParam['monthMax'] = args.monthTo
  inputParam['maxFeatures'] = args.maxFeatures
  inputParam['nCores'] = args.nCores
  inputParam['resx'] = 10 # in meters
  inputParam['resy'] = -10 # in meters
  inputParam['projection'] = 'EPSG:3035'
  inputParam['fontSize'] = args.fontSize

  cur.execute(
    """
    SELECT st_x(st_transform(the_geom, 3035)) AS x, st_y(st_transform(the_geom, 3035)) AS y, value 
    FROM public.eu_boku1_input 
    WHERE runs_id = %s
    """,
    (args.runId, )
  )
  inputParam['referencePoints'] = [{"x": i[0], "y": i[1], "label": i[2]} for i in cur.fetchall()]

  os.makedirs(inputParam['tmpDir'], exist_ok=True)

  paramFile = os.path.join(inputParam['tmpDir'], 'param.json')
  with open(paramFile, 'w') as f:
    json.dump(inputParam, f)

  cur.execute(
    """
    SELECT st_asgeojson(st_union(st_transform(geom, 3035))) 
    FROM system.roi 
    WHERE gid IN (%s)
    """ % (', '.join(['%s'] * len(inputParam['roi']))),
    inputParam['roi']
  )
  with open(inputParam['roiFile'], 'w') as f:
    json.dump({
     'type': 'FeatureCollection',
     'name': 'roi',
     'crs': {'type': 'name', 'properties': {'name': 'urn:ogc:def:crs:EPSG::3035'}},
     'features': [{'type': 'Feature', 'properties': {}, 'geometry': json.loads(cur.fetchone()[0])}]
    }, f)
  conn.commit() # to end "idle in transaction state"

  # 2. Calling classification algorithm implemented in R
  print('\n#Running the model\n\n')
  result = subprocess.run(['/usr/bin/Rscript', args.modelDir + '/eu_class_user.R', paramFile])
  print(result)
  if result.returncode != 0:
    raise Exception('eu_class_user.R has non-zero exit status')

  # 3. Postprocessing
  recipe = {
    "config": {"service_url": 'http://127.0.0.1:9009/rasdaman/ows', "mock": False, "automated": True},
    "input": {"coverage_id": '', "paths": ['']},
    "recipe": {"name": "map_mosaic", "options": {"wms_import": True}}
  }
  print('\n# Importing results to the rasdaman\n\n')
  for i in ['class', 'prob']:
    runName2 = runName + '_' + i
    recipe['input'] = {'coverage_id': runName + '_' + i, 'paths': [os.path.join(args.rasdamanRasterDir, runName, i + '*tif')]}
    recipeFile = os.path.join(args.recipeDir, runName2 + '.json')
    recipeLogFile = recipeFile + '.log'
    recipeResumeFile = re.sub('json$', 'resume.json', recipeFile)
    wmsStyleFile = os.path.join(args.recipeDir, runName + '_style.json')
    os.makedirs(os.path.dirname(recipeFile), exist_ok=True)
    with open(recipeFile, 'w') as f:
      json.dump(recipe, f)
    if os.path.exists(recipeLogFile):
      os.unlink(recipeLogFile)
    if os.path.exists(recipeResumeFile):
      os.unlink(recipeResumeFile)

    print('\nDeleting existing coverage\n')
    resp = requests.get(args.rasdamanUrl, params = {'SERVICE': 'WCS', 'VERSION': '2.0.1', 'REQUEST': 'DeleteCoverage', 'COVERAGEID': runName2})
    print(resp.url)
    print(resp.status_code)
    print(resp.text)

    print('\nImporting coverage\n')
    resp = requests.get('http://rasdaman/docker_run.php', params = {'model': 'wcst_import', 'runid': str(args.runId) + '_' + i, 'modelkey': 'eu_class_user'})
    print(resp.url)
    print(resp.status_code)
    print(resp.text)
    if int(resp.status_code / 100) != 2:
      raise Exception('Importing results into rasdaman failed')

    if i == 'class':
      print('\nImporting SLD style\n')
      sld = """<?xml version="1.0" encoding="ISO-8859-1"?>
<StyledLayerDescriptor version="1.0.0" xsi:schemaLocation="http://www.opengis.net/sld StyledLayerDescriptor.xsd" xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  <NamedLayer>
    <Name>Fixed color palette</Name>
    <UserStyle>
      <Title>Fixed color palette</Title>
      <FeatureTypeStyle>
        <Rule>
          <RasterSymbolizer>
            <ColorMap>
              %s
            </ColorMap>
          </RasterSymbolizer>
        </Rule>
      </FeatureTypeStyle>
    </UserStyle>
  </NamedLayer>
</StyledLayerDescriptor>"""
      with open(inputParam['legendFileJson'], 'r') as f:
        legend = json.load(f)
      colorTable = ''
      for i in range(len(legend)):
        colorTable += '<ColorMapEntry color="%s" quantity="%d" />' % (legend[i], i + 1)
      colorTable += '<ColorMapEntry color="#FFFFFF" quantity="255" opacity="0" />'
      sld = sld % colorTable
      resp = requests.post(args.rasdamanUrl, data = {'service': 'WMS', 'version': '1.3.0', 'request': 'InsertStyle', 'name': 'default', 'abstract': 'default style', 'layer': runName + '_class', 'ColorTableType': 'SLD', 'ColorTableDefinition': sld})
      print(resp.url)
      print(resp.status_code)
      print(resp.text)
      if int(resp.status_code / 100) != 2:
        raise Exception('Importing raster style failed')

  print('\n# Updating the GUI database\n\n')
  with open(inputParam['validationFile'], 'r') as f:
    valRes = json.load(f)

  result = {
    'data': {
      'grid': [{
        'name': 'Training validation results (3-folded cross validation)',
        'fields': [{'name': 'fold', 'field': 'fold', 'type': 'string'}, {'name': 'accuracy', 'field': 'accuracy', 'type': 'number'}],
        'data': [{'fold': 1, 'accuracy': valRes['Min.']}, {'fold': 2, 'accuracy': valRes['Median']}, {'fold': 3, 'accuracy': valRes['Max.']}]
      }],
      'raster': [
        {
          'name': 'Classification results', 
          'layer': runName + '_class',
          'wms': 'rasdaman',
          'style': 'default',
          'pnglegend': runName + '.png',
          'sld': sld
        },
        {
          'name': 'Classification probability', 
          'layer': runName + '_prob',
          'wms': 'rasdaman'
        }
      ]
    }
  }
  cur.execute(
    "UPDATE system.runs SET status = 2, endtime = clock_timestamp(), errorlog = '', result = %s WHERE id = %s", 
    (json.dumps(result), args.runId)
  )
  conn.commit()

  if not args.noCleanup:
    print('\n# Cleaning up\n\n')
    shutil.rmtree(inputParam['tmpDir'], True)
    shutil.rmtree(inputParam['rasterDir'], True)
except Exception as e:
  cur.execute(
    "UPDATE system.runs SET status = 3, endtime = clock_timestamp(), errorlog = %s, result = '' WHERE id = %s",
    (json.dumps({'error': str(e)}), args.runId)
  )
  conn.commit()

