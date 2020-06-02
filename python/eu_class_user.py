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
parser.add_argument('--rasdamanUrl', default='http://192.168.30.11:8090/rasdaman/ows')
parser.add_argument('--coveragePrefix', default='CA')
parser.add_argument('--tmpDir', default='/cuber/tmp')
parser.add_argument('--modelDir', default='/cuber')
parser.add_argument('--rasdamanRasterDir', default='/geodata/runs')
parser.add_argument('--rasterDir', default='/ras_data/runs')
parser.add_argument('--recipeDir', default='/ras_recipe')
parser.add_argument('--legendDir', default='/legends')
parser.add_argument('--monthFrom', type=int, default=4)
parser.add_argument('--monthTo', type=int, default=10)
parser.add_argument('--fontSize', type=int, default=10)
parser.add_argument('--minCoverage', type=int, default=98)
parser.add_argument('--blockSize', type=int, default=20000)
parser.add_argument('runId', type=int)
args = parser.parse_args()

conn = psycopg2.connect(args.dbConf)
cur = conn.cursor()

# 1. Prepare data for the model

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
inputParam['legendFilePng'] = os.path.join(args.legendDir, runName + '.png')
inputParam['legendFileJson'] = os.path.join(inputParam['tmpDir'], 'legend.json')
inputParam['roiFile'] = os.path.join(inputParam['tmpDir'], 'roi.geojson')
inputParam['logFile'] = os.path.join(inputParam['tmpDir'], 'log')
inputParam['validationFile'] = os.path.join(inputParam['tmpDir'], 'validation.json')  # leave empty for no validation
inputParam['minDataCoverage'] = args.minCoverage / 100
inputParam['blockSize'] = args.blockSize # in meters
inputParam['monthMin'] = args.monthFrom
inputParam['monthMax'] = args.monthTo
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

# 2. Calling classification algorithm implemented in R
print('\n#Running the model\n\n')
result = subprocess.run(['/usr/bin/Rscript', args.modelDir + '/eu_class_user.R', paramFile])
print(result)

# 3. Postprocessing
recipe = {
  "config": {
    "service_url": 'http://127.0.0.1:9009/rasdaman/ows',
    "mock": False,
    "automated": True
  },
  "input": {
    "coverage_id": 'eu_class_user_' + str(args.runId),
    "paths": [os.path.join(args.rasdamanRasterDir, runName, '*tif')]
  },
  "recipe": {"name": "map_mosaic", "options": {"wms_import": True}}
}
recipeFile = os.path.join(args.recipeDir, runName + '.json')
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

with open(inputParam['legendFileJson'], 'r') as f:
  legend = json.load(f)
colorTable = {'255': [255, 255, 255, 0]}
for i in legend:
  colorTable[str(len(colorTable))] = [int(i[1:3], 16), int(i[3:5], 16), int(i[5:7], 16), 255]
with open(wmsStyleFile, 'w') as f:
  json.dump({'ColorTableType': 'ColorMap', 'ColorTableDefinition': json.dumps({'type': 'values', 'colorTable': colorTable})}, f)

print('\n# Importing results to the rasdaman\n\n')
resp = requests.get('http://rasdaman/docker_run.php', params = {'model': 'wcst_import', 'runid': args.runId, 'modelkey': 'eu_class_user'})
print(resp.url)
print(resp.status_code)
print(resp.text)

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
    'raster': [{'name': 'Classification results', 'layer': recipe['input']['coverage_id']}]
  }
}
cur.execute("UPDATE system.runs SET status = 2, errorlog = '', result = %s WHERE id = %s", (json.dumps(result), args.runId))
conn.commit()

print('\n# Cleaning up\n\n')
shutil.rmtree(inputParam['tmpDir'], True)
shutil.rmtree(inputParam['rasterDir'], True)
