"""Wall-clock metrics with cohorts defined by Base wall-clock time."""
from pathlib import Path
import csv,json,math
R=Path(__file__).resolve().parents[1];vs=['base','eq_can_only','freq_ast_asc','eq_can_freq_ast_asc'];d={v:{x['filename']:x for x in csv.DictReader((R/'data/performance/raw'/(v+'.csv')).open())} for v in vs}
ok=lambda r:r['property_result'] in ('Safe','Unsafe') and r['timed_out']!='true'
ks=[m for m in d['base'] if all(ok(d[v][m]) for v in vs)];wall=lambda v,m:float(d[v][m]['execution_time(s)'])
out={'coverage':{},'groups':{}}
for v in vs:out['coverage'][v]={'solved':sum(map(ok,d[v].values())),'par2':math.fsum(float(x['execution_time(s)']) if ok(x) else 600 for x in d[v].values()),'common_wall':math.fsum(wall(v,m) for m in ks)}
for name,group in {'all':ks,'Safe':[m for m in ks if d['base'][m]['property_result']=='Safe'],'Unsafe':[m for m in ks if d['base'][m]['property_result']=='Unsafe'],'>=1':[m for m in ks if wall('base',m)>=1],'>=10':[m for m in ks if wall('base',m)>=10],'<1':[m for m in ks if wall('base',m)<1],'1-10':[m for m in ks if 1<=wall('base',m)<10]}.items():
 out['groups'][name]={'n':len(group)}
 for v in vs:
  x=[wall('base',m) for m in group];y=[wall(v,m) for m in group];out['groups'][name][v]={'seconds':math.fsum(y),'ratio':math.fsum(y)/math.fsum(x),'G':math.exp(sum(math.log(a/b) for a,b in zip(x,y))/len(x))}
i=json.loads((R/'generated/invariant_structure/results.json').read_text());out['invariant_wall']={v:i['totals'][v]['wall'] for v in i['totals']}
rows=list(csv.DictReader((R/'generated/invariant_structure/per_model.csv').open()));out['rtp_vt']=[x for x in rows if x['model'].endswith('/rtp_vt.lus') and x['variant'] in ('base','eqcan_order')]
(R/'generated/wall_metrics.json').write_text(json.dumps(out,indent=2)+'\n')
lines=['# Wall-clock evaluation tables','','| Configuration | Solved | Wall PAR-2 (s) | Common wall time (s) |','|---|---:|---:|---:|']
for v in vs:
 x=out['coverage'][v];lines.append(f"| {v} | {x['solved']} | {x['par2']:.1f} | {x['common_wall']:.1f} |")
(R/'generated/wall_tables.md').write_text('\n'.join(lines)+'\n');print('\n'.join(lines))
