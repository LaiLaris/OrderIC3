"""Check input manifests, paired records, invariant exports, and frozen source."""
from pathlib import Path
import csv,json,hashlib,argparse
R=Path(__file__).resolve().parents[1]
p=argparse.ArgumentParser();p.add_argument('--checksums',action='store_true');args=p.parse_args()
def sha(f):
 h=hashlib.sha256()
 with f.open('rb') as stream:
  for chunk in iter(lambda:stream.read(1024*1024),b''):h.update(chunk)
 return h.hexdigest()
inputs=list(csv.DictReader((R/'benchmarks/manifest.csv').open()));assert len(inputs)==786
for row in inputs:assert sha(R/'benchmarks/inputs'/row['model'])==row['sha256'],row['model']
runs=[json.loads(s) for s in (R/'data/invariant_structure/results.jsonl').read_text().splitlines()];assert len(runs)==1540
assert len({(x['repeat'],x['model'],x['variant']) for x in runs})==1540
assert sum(x['status']=='Safe' for x in runs)==1539
assert sum(x['status']=='Timeout' for x in runs)==1
assert sum(len(x['exports']) for x in runs)==1539
assert len(list((R/'data/performance/logs').rglob('*.log')))==3144
assert len(list((R/'data/invariant_structure/runs').rglob('run.log')))==1540
for row in runs:
 assert (R/'data/invariant_structure'/row['log']).is_file()
 assert sha(R/'benchmarks/inputs'/row['model'])==row['input_sha256']
 for export in row['exports']:assert (R/'data/invariant_structure'/export).is_file()
manifest=json.loads((R/'provenance/measurement/source_manifest.json').read_text())
for name,h in manifest.items():assert sha(R/'source/measurement'/name)==h,name
cfg=json.loads((R/'provenance/measurement/config.json').read_text());assert sha(R/'bin/measurement-kind2')==cfg['binary_sha256']
assert sha(R/'source/performance_IC3.ml')==cfg['original_ic3_sha256']
assert sha(R/'provenance/measurement/selection.csv')==cfg['selection_sha256']
original_run=json.loads((R/'data/invariant_structure/run_manifest.json').read_text())
assert sha(R/'provenance/measurement/original_scripts/run_measurement.py')==original_run['runner_sha256']
assert sha(R/'provenance/measurement/config.json')==original_run['config_sha256']
for v,h in cfg['original_raw_sha256'].items():assert sha(R/'data/performance/raw'/(v+'.csv'))==h
if args.checksums:
 for line in (R/'checksums.sha256').read_text().splitlines():
  h,name=line.split('  ',1);assert sha(R/name)==h,name
print('PASS: 786 input hashes; 1540 measurement runs; 1539 exports; frozen measurement source; release config, runner, binary and CSV hashes.')
