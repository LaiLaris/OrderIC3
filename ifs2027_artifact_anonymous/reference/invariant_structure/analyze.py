"""Read-only analysis of the completed common-Safe measurement experiment."""
from pathlib import Path
import json,csv,hashlib,statistics,math,re
P=Path('/home/artifact/kind2-exp/OrderIC3_Auto/runs/ifs2027_safe385_measurement/invariant_round01')
H=Path(__file__).resolve().parent
runs=[json.loads(x) for x in (P/'results.jsonl').read_text().splitlines()]
vs=['base','eqcan','order','eqcan_order']
assert len(runs)==1540 and len({(r['repeat'],r['model'],r['variant']) for r in runs})==1540
manifest=json.loads((P/'run_manifest.json').read_text())
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest()
assert sha(P.parent/'run_measurement.py')==manifest['runner_sha256']
assert sha(P.parent/'prepared/config.json')==manifest['config_sha256']
cfg=json.loads((P.parent/'prepared/config.json').read_text());assert sha(P.parent/'prepared/kind2')==cfg['binary_sha256']
index={(r['model'],r['variant']):r for r in runs}
models=sorted({r['model'] for r in runs});assert len(models)==385
common=[m for m in models if all(index[m,v]['status']=='Safe' for v in vs)]
data={}
for m in common:
 for v in vs:
  r=index[m,v];assert len(r['exports'])==1;e=json.loads((P/r['exports'][0]).read_text());cs=e['support_clauses'];unique={re.sub(r'\s+',' ',c['clause']['term']).strip():c for c in cs}
  assert all(c['literal_count']==len(c['literals']) and c['literal_ast_sum']==sum(t['ast_nodes'] for t in c['literals']) for c in cs)
  queries=e['indgen_initial_queries']+e['indgen_relative_queries'];assert queries<=r['metrics']['Solver']
  data[m,v]={'model':m,'variant':v,'clauses':len(cs),'unique_clauses':len(unique),'literals':sum(c['literal_count'] for c in cs),'literal_ast':sum(c['literal_ast_sum'] for c in cs),'clause_ast':sum(c['clause']['ast_nodes'] for c in cs),'unique_clause_ast':sum(c['clause']['ast_nodes'] for c in unique.values()),'support_term_ast':e['support_term']['ast_nodes'],'certificate_term_ast':e['certificate_term']['ast_nodes'],'queries':queries,'solver':r['metrics']['Solver'],'wall':r['wall_seconds'],'internal':r['metrics']['Total time'],'frames':sum(map(int,r['metrics']['Frame sizes'].split())),'k':int(r['metrics']['k'])}
fields=[k for k in next(iter(data.values())) if k not in ('model','variant')]
totals={v:{k:sum(data[m,v][k] for m in common) for k in fields} for v in vs}
for v,t in totals.items():
 t['mean_literals']=t['literals']/t['clauses'];t['mean_literal_ast_per_clause']=t['literal_ast']/t['clauses'];t['mean_ast_per_literal']=t['literal_ast']/t['literals'];t['percent_vs_base']={k:100*(t[k]/totals['base'][k]-1) for k in fields}
pairs={}
for a,b in [('base',v) for v in vs[1:]]+[('eqcan','eqcan_order'),('order','eqcan_order')]:
 key=a+' -> '+b;pairs[key]={}
 for f in fields:
  x=[data[m,a][f] for m in common];y=[data[m,b][f] for m in common]
  pairs[key][f]={'lower':sum(t<s for s,t in zip(x,y)),'equal':sum(t==s for s,t in zip(x,y)),'higher':sum(t>s for s,t in zip(x,y)),'total_ratio':sum(y)/sum(x)}
 cases=[]
 for m in common:
  a=data[m,'base'];b=data[m,'eqcan_order'];cases.append({'model':m,'base_clauses':a['clauses'],'combined_clauses':b['clauses'],'base_ast':a['literal_ast'],'combined_ast':b['literal_ast'],'ast_delta':b['literal_ast']-a['literal_ast'],'base_queries':a['queries'],'combined_queries':b['queries'],'base_internal':a['internal'],'combined_internal':b['internal']})
 out={'integrity':{'runs':len(runs),'models':385,'repeats':manifest['repeats'],'common_safe':len(common),'non_safe':[{'model':r['model'],'variant':r['variant'],'status':r['status'],'wall':r['wall_seconds']} for r in runs if r['status']!='Safe'],'config_binary_runner_hashes_verified':True},'totals':totals,'paired':pairs,'largest_ast_reductions':sorted(cases,key=lambda r:r['ast_delta'])[:5],'largest_ast_increases':sorted(cases,key=lambda r:r['ast_delta'],reverse=True)[:5]}
(H/'results.json').write_text(json.dumps(out,indent=2)+'\n')
with (H/'per_model.csv').open('w',newline='') as f:w=csv.DictWriter(f,fieldnames=list(next(iter(data.values()))));w.writeheader();w.writerows(data.values())
print(json.dumps(out,indent=2))
