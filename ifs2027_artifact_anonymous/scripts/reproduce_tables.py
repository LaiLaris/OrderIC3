"""Recompute evaluation tables from retained CSVs and original logs; no solver runs."""
from pathlib import Path
import csv,json,math,re
R=Path(__file__).resolve().parents[1];O=R/'generated';O.mkdir(exist_ok=True)
VS=['base','eq_can_only','freq_ast_asc','eq_can_freq_ast_asc']
D={v:{r['filename']:r for r in csv.DictReader((R/'data/performance/raw'/(v+'.csv')).open())} for v in VS}
def solved(r):return r['property_result'] in ('Safe','Unsafe') and r['timed_out']!='true'
keys=list(D['base']);assert all(len(d)==786 and d.keys()==D['base'].keys() for d in D.values())
assert all(len({d[k]['property_result'] for d in D.values() if solved(d[k])})<=1 for k in keys)
common=[k for k in keys if all(solved(d[k]) for d in D.values())];safe=[k for k in common if D['base'][k]['property_result']=='Safe']
assert len(common)==697 and len(safe)==385
out={'coverage':{},'groups':{},'proof':{},'status_migrations':[]}
for v,d in D.items():
 out['coverage'][v]={'solved':sum(map(solved,d.values())),'Safe':sum(solved(r) and r['property_result']=='Safe' for r in d.values()),'Unsafe':sum(solved(r) and r['property_result']=='Unsafe' for r in d.values()),'PAR2_IC3':sum(float(r['total_time']) if solved(r) else 600 for r in d.values()),'common_IC3':sum(float(d[k]['total_time']) for k in common)}
 frames=sum(sum(map(int,d[k]['frame_sizes'].split())) for k in safe)
 events=0; marker_events=0
 for k in safe:
  log=R/'data/performance/logs'/v/Path(k).with_suffix('.log');text=log.read_text(errors='replace')
  marker_events+=text.count('New clause from inductive generalization')
  # Exact historical counter: scans following lines for a complete clause body.
  # Same-line clauses can cause it to consume a subsequent event. Preserve
  # this legacy value for paper reproduction and report all markers separately.
  it=iter(text.splitlines())
  for line in it:
   if 'New clause from inductive generalization' not in line:continue
   body=[]
   for nxt in it:
    body.append(nxt)
    if '}' in nxt:break
   printed=' '.join(body)
   events+=int('{' in printed and '}' in printed)
 out['proof'][v]={'frame_entries':frames,'generalization_events':marker_events,'historical_generalization_events':events,'all_marker_events':marker_events,'SMT_queries':sum(int(float(d[k]['solver'])) for k in safe)}
for name,ks in {'all':common,'Safe':safe,'Unsafe':[k for k in common if k not in set(safe)],'Base>=1s':[k for k in common if float(D['base'][k]['total_time'])>=1],'Base>=10s':[k for k in common if float(D['base'][k]['total_time'])>=10]}.items():
 out['groups'][name]={'n':len(ks),'variants':{}}
 for v in VS:
  q={}
  for label,col in [('IC3','total_time'),('wall','execution_time(s)')]:
   x=[float(D['base'][k][col]) for k in ks];y=[float(D[v][k][col]) for k in ks]
   q[label]={'seconds':sum(y),'ratio':sum(y)/sum(x),'geomean_speedup':math.exp(sum(math.log(a/b) for a,b in zip(x,y))/len(x))}
  out['groups'][name]['variants'][v]=q
for k in keys:
 if len({solved(D[v][k]) for v in VS})>1:out['status_migrations'].append({'model':k,**{v:D[v][k]['property_result'] for v in VS}})
assert [out['coverage'][v]['solved'] for v in VS]==[706,707,708,708]
assert [out['proof'][v]['historical_generalization_events'] for v in VS]==[5264,5024,4990,4976],out['proof']
assert [out['proof'][v]['generalization_events'] for v in VS]==[5270,5031,4995,4982],out['proof']
assert [out['proof'][v]['frame_entries'] for v in VS]==[7521,7887,6314,5935]
assert [out['proof'][v]['SMT_queries'] for v in VS]==[373412,306187,328769,286390]
assert len(out['status_migrations'])==20
(O/'performance_tables.json').write_text(json.dumps(out,indent=2)+'\n')
lines=['# Recomputed evaluation tables','','| Configuration | Solved | IC3-time PAR-2 (s) | Common IC3 time (s) | Frame entries | Generalization events | SMT queries |','|---|---:|---:|---:|---:|---:|---:|']
for v in VS:
 c=out['coverage'][v];p=out['proof'][v];lines.append(f"| {v} | {c['solved']} | {c['PAR2_IC3']:.1f} | {c['common_IC3']:.1f} | {p['frame_entries']} | {p['generalization_events']} | {p['SMT_queries']} |")
(O/'performance_tables.md').write_text('\n'.join(lines)+'\n');print('\n'.join(lines))
