"""Summarize final support clauses and literal AST sizes; no model-checker runs."""
import csv,json,sys
from pathlib import Path
from collections import Counter

def main():
 out=Path(sys.argv[1]).resolve();runs=[json.loads(x) for x in (out/'results.jsonl').read_text().splitlines()];measured=[]
 for r in runs:
  if r['status']!='Safe':continue
  clauses=[];exports=[json.loads((out/n).read_text()) for n in r['exports']]
  for e in exports:clauses.extend(e['support_clauses'])
  # Raw returned support-clause list, then syntactic deduplication within each run.
  unique={c['clause']['term']:c for c in clauses}
  rec={k:r[k] for k in ('repeat','model','variant','wall_seconds')}
  rec.update(support_clause_entries=len(clauses),unique_support_clauses=len(unique),literal_count=sum(c['literal_count'] for c in clauses),literal_ast_sum=sum(c['literal_ast_sum'] for c in clauses),clause_ast_sum=sum(c['clause']['ast_nodes'] for c in clauses),unique_clause_ast_sum=sum(c['clause']['ast_nodes'] for c in unique.values()),initial_queries=max(e['indgen_initial_queries'] for e in exports),relative_queries=max(e['indgen_relative_queries'] for e in exports),indgen_calls=max(e['indgen_calls'] for e in exports),solver_queries=r['metrics'].get('Solver'))
  rec['indgen_queries']=rec['initial_queries']+rec['relative_queries'];measured.append(rec)
 # Same model/repeat must be Safe under all four configurations for comparisons.
 vs=['base','eqcan','order','eqcan_order'];index={(r['repeat'],r['model'],r['variant']):r for r in measured}
 common={ (r['repeat'],r['model']) for r in measured if all((r['repeat'],r['model'],v) in index for v in vs)}
 summary={}
 for v in vs:
  rr=[index[(rep,m,v)] for rep,m in sorted(common)];n=sum(r['support_clause_entries'] for r in rr)
  fields=['support_clause_entries','unique_support_clauses','literal_count','literal_ast_sum','clause_ast_sum','unique_clause_ast_sum','initial_queries','relative_queries','indgen_queries','indgen_calls','solver_queries','wall_seconds']
  summary[v]={'status_counts':dict(Counter(r['status'] for r in runs if r['variant']==v)),'common_model_repeats':len(rr),**{k:sum(r[k] for r in rr) for k in fields},'mean_literals_per_support_clause':sum(r['literal_count'] for r in rr)/n if n else None,'mean_literal_ast_sum_per_support_clause':sum(r['literal_ast_sum'] for r in rr)/n if n else None}
 if measured:
  with (out/'per_run_metrics.csv').open('w',newline='') as f:
   w=csv.DictWriter(f,fieldnames=list(measured[0]));w.writeheader();w.writerows(measured)
 (out/'summary.json').write_text(json.dumps(summary,indent=2)+'\n')
 print(json.dumps(summary,indent=2))
if __name__=='__main__':main()
