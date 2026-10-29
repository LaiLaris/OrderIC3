"""Portable replay using retained Linux binaries. Dry-run by default; new output only."""
import argparse,csv,json,os,random,shutil,signal,subprocess,time,re
from pathlib import Path
R=Path(__file__).resolve().parents[1]
a=argparse.ArgumentParser();a.add_argument('--experiment',choices=['performance','invariant_structure'],required=True);a.add_argument('--execute',action='store_true');a.add_argument('--binary',type=Path);a.add_argument('--limit',type=int);a.add_argument('--output',type=Path,default=R/'replays/new_run');a.add_argument('--timeout',type=int,default=300);a.add_argument('--z3',default='z3');args=a.parse_args()
if args.limit is not None and args.limit<1:a.error('--limit must be positive')
if args.timeout<1:a.error('--timeout must be positive')
vs={'base':[],'eqcan':['--ic3qe_ge_eq_can'],'order':['--ic3qe_freq_sort','--ic3qe_ast_complexity','--ic3qe_ast_complexity_order','asc'],'eqcan_order':['--ic3qe_ge_eq_can','--ic3qe_freq_sort','--ic3qe_ast_complexity','--ic3qe_ast_complexity_order','asc']}
file=R/('benchmarks/manifest.csv' if args.experiment=='performance' else 'benchmarks/safe385.csv')
models=[r['model'] for r in csv.DictReader(file.open())];models=models[:args.limit] if args.limit else models
binary=R/'bin'/('performance-kind2' if args.experiment=='performance' else 'measurement-kind2');jobs=[]
if args.binary:binary=args.binary.resolve()
random.Random(20261013).shuffle(models)
for m in models:
 variants=list(vs);random.Random('20261013:'+m).shuffle(variants);jobs.extend((m,v) for v in variants)
print(f'{len(models)} models x 4 configurations = {len(jobs)} runs; timeout upper bound {len(jobs)*args.timeout/3600:.2f} hours (plus overhead).')
print('Retained binaries; new interleaved replay schedule. Existing observations remain untouched.')
if not args.execute:raise SystemExit()
z3=shutil.which(args.z3)
if not z3:raise SystemExit('Z3 executable not found; use --z3 /path/to/z3')
out=args.output.resolve();out.mkdir(parents=True,exist_ok=False)
(out/'configuration.json').write_text(json.dumps({'experiment':args.experiment,'timeout':args.timeout,'binary':str(binary),'z3':z3,'variants':vs},indent=2))
errors=[]
for i,(m,v) in enumerate(jobs,1):
 rd=out/v/Path(m).with_suffix('');rd.mkdir(parents=True);cmd=[str(binary),'--enable','IC3QE','--smt_solver','z3','--z3_bin',z3,'-vv','--color','false','--timeout',str(args.timeout),*vs[v],str(R/'benchmarks/inputs'/m)]
 env=os.environ.copy()
 if args.experiment=='invariant_structure':env['KIND2_MEASUREMENT_DIR']=str(rd)
 start=time.monotonic();external=False
 with (rd/'run.log').open('w') as log:
  p=subprocess.Popen(cmd,cwd=str(rd),env=env,stdout=log,stderr=subprocess.STDOUT,start_new_session=True)
  try:code=p.wait(timeout=args.timeout+15)
  except (subprocess.TimeoutExpired,KeyboardInterrupt) as exc:
   os.killpg(p.pid,signal.SIGTERM)
   try:p.wait(timeout=3)
   except subprocess.TimeoutExpired:os.killpg(p.pid,signal.SIGKILL);p.wait()
   if isinstance(exc,KeyboardInterrupt):raise
   external=True;code=p.returncode
 text=(rd/'run.log').read_text(errors='replace')
 status='Timeout' if external or re.search(r'\b(?:Wallclock|CPU) timeout\.|<Timeout>',text,re.I) else {0:'Safe',40:'Unsafe',30:'Unknown'}.get(code,'Error')
 exports=sorted(rd.glob('invariant_*.json'))
 if args.experiment=='invariant_structure' and status=='Safe' and not exports:status='MissingExport'
 if status in ('Error','MissingExport','Unknown'):errors.append((m,v,status))
 row={'status':status,'exports':[str(p.relative_to(out)) for p in exports],'model':m,'variant':v,'exit_code':code,'external_timeout':external,'wall_seconds':time.monotonic()-start,'command':cmd,'log':str((rd/'run.log').relative_to(out))}
 with (out/'results.jsonl').open('a') as f:f.write(json.dumps(row)+'\n')
 print(f'[{i}/{len(jobs)}] {v} {m}: exit={code}, {row["wall_seconds"]:.3f}s',flush=True)

if errors:raise SystemExit('Replay errors: '+repr(errors))
