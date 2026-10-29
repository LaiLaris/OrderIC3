"""Serial randomized/interleaved measurement runner; dry-run unless --execute."""
import argparse,csv,fcntl,hashlib,json,os,random,re,signal,subprocess,time
from pathlib import Path
H=Path(__file__).resolve().parent

def sha(p):return hashlib.sha256(p.read_bytes()).hexdigest()
def main():
 ap=argparse.ArgumentParser();ap.add_argument('--execute',action='store_true');ap.add_argument('--limit',type=int);ap.add_argument('--repeats',type=int,default=1);ap.add_argument('--output',default='invariant_round01');ap.add_argument('--resume',action='store_true');args=ap.parse_args()
 assert args.repeats>0 and (args.limit is None or args.limit>0)
 cfg=json.loads((H/'prepared/config.json').read_text());assert sha(H/'prepared/kind2')==cfg['binary_sha256'];assert sha(H/'prepared/selection.csv')==cfg['selection_sha256'];assert sha(Path(cfg['z3_path']))==cfg['z3_sha256']
 rows=list(csv.DictReader((H/'prepared/selection.csv').open()));rows=rows[:args.limit] if args.limit else rows
 for r in rows:assert sha(H/'prepared/dataset'/r['model'])==r['sha256']
 jobs=[]
 for rep in range(1,args.repeats+1):
  rs=list(rows);random.Random(cfg['schedule_seed']+rep).shuffle(rs)
  for row in rs:
   vs=list(cfg['variants']);random.Random(f"{cfg['schedule_seed']}:{rep}:{row['model']}").shuffle(vs)
   jobs.extend((rep,row,v) for v in vs)
 print(f'{len(rows)} models x 4 variants x {args.repeats} repeats = {len(jobs)} runs',flush=True)
 print(f'Old measured cohort runtime: {cfg["old_safe_wall_seconds"]/60:.1f} minutes per full repeat; not a guarantee.',flush=True)
 if not args.execute:return
 out=(H/args.output).resolve();assert out.parent==H and out!=H/'prepared'
 out.mkdir(exist_ok=args.resume)
 lock=(H/'run.lock').open('w');fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
 identity={'config_sha256':sha(H/'prepared/config.json'),'runner_sha256':sha(Path(__file__)),'repeats':args.repeats,'limit':args.limit}
 manifest=out/'run_manifest.json'
 if args.resume:assert json.loads(manifest.read_text())==identity
 else:manifest.write_text(json.dumps(identity,indent=2)+'\n')
 result=out/'results.jsonl';done=set()
 if args.resume:
  for line in result.read_text().splitlines() if result.exists() else []:
   r=json.loads(line);assert r['status']!='Interrupted','Interrupted run must be inspected before resuming';done.add((r['repeat'],r['model'],r['variant']))
 def stop(p):
  try:os.killpg(p.pid,signal.SIGTERM)
  except ProcessLookupError:pass
  try:p.wait(timeout=3)
  except subprocess.TimeoutExpired:
   os.killpg(p.pid,signal.SIGKILL);p.wait()
 for i,(rep,row,var) in enumerate(jobs,1):
  key=(rep,row['model'],var)
  if key in done:continue
  rd=out/'runs'/f'repeat_{rep}'/var/Path(row['model']).with_suffix('');rd.mkdir(parents=True,exist_ok=args.resume)
  for old in rd.glob('invariant_*.json'):old.unlink()
  command=[str(H/'prepared/kind2'),*cfg['common_args'],'--z3_bin',cfg['z3_path'],'--timeout',str(cfg['timeout_seconds']),*cfg['variants'][var],str(H/'prepared/dataset'/row['model'])]
  env=os.environ.copy();env['KIND2_MEASUREMENT_DIR']=str(rd)
  print(f'[{i}/{len(jobs)}] START {row["model"]} {var} repeat={rep}',flush=True)
  start=time.perf_counter();external=False;interrupted=False
  with (rd/'run.log').open('w') as log:
   p=subprocess.Popen(command,cwd=rd,env=env,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
   try:code=p.wait(timeout=cfg['timeout_seconds']+15)
   except subprocess.TimeoutExpired:external=True;stop(p);code=p.returncode
   except KeyboardInterrupt:interrupted=True;stop(p);code=p.returncode
  text=(rd/'run.log').read_text(errors='replace')
  reported=bool(re.search(r'\b(?:Wallclock|CPU) timeout\.|<Timeout>',text,re.I))
  status='Interrupted' if interrupted else 'Timeout' if external or reported else 'Safe' if code==0 else 'Unsafe' if code==40 else 'Unknown' if code==30 else 'Error'
  if re.search(r'Child process .* killed by signal SIG(?:SEGV|ABRT|BUS|ILL|FPE)',text):status='Error'
  metrics={k.strip():float(v) for k,v in re.findall(r'^\s*(Solver|k|Total time|Inductive generalization time)\s*:\s*([\d.]+)\s*$',text,re.M)}
  frames=re.findall(r'^\s*Frame sizes\s*:\s*([0-9 ]*)$',text,re.M)
  metrics['Frame sizes']=frames[-1].strip() if frames else ''
  exports=sorted(rd.glob('invariant_*.json'))
  if status=='Safe' and not exports:status='MissingExport'
  r={'repeat':rep,'model':row['model'],'variant':var,'status':status,'exit_code':code,'wall_seconds':time.perf_counter()-start,'external_timeout':external,'metrics':metrics,'exports':[str(p.relative_to(out)) for p in exports],'log':str((rd/'run.log').relative_to(out)),'command':command,'input_sha256':row['sha256']}
  with result.open('a') as f:f.write(json.dumps(r)+'\n');f.flush();os.fsync(f.fileno())
  print(f'[{i}/{len(jobs)}] {status} {r["wall_seconds"]:.3f}s exports={len(exports)}',flush=True)
  if interrupted:return
  if status in ('Error','MissingExport','Unsafe'):raise RuntimeError(f'Batch stopped for inspection: {status}; see {rd / "run.log"}')
 subprocess.run(['python3',str(H/'analyze_measurement.py'),str(out)],check=True)
if __name__=='__main__':main()
