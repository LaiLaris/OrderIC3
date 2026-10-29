"""Compare paper evaluation tables and central numerical claims to retained data.
Usage: python3 scripts/audit_manuscript.py /path/to/main.tex
"""
from pathlib import Path
import csv,json,re,sys,math
R=Path(__file__).resolve().parents[1];paper=Path(sys.argv[1]);s=paper.read_text();P=json.loads((R/'generated/performance_tables.json').read_text());W=json.loads((R/'generated/wall_metrics.json').read_text());I=json.loads((R/'generated/invariant_structure/results.json').read_text());A=json.loads((R/'generated/representation/summary.json').read_text());VS=['base','eq_can_only','freq_ast_asc','eq_can_freq_ast_asc'];names=['Base','EqCan','Order','EqCan+Order'];checks=[]
def rows(label):
 a=s.index('\\label{tab:'+label+'}');a=s.index('\\midrule',a)+len('\\midrule');b=s.index('\\bottomrule',a)
 return [[x.strip().replace('\\_','_').replace('\\%','').replace(',','') for x in line.strip().strip('\\').split('&')] for line in s[a:b].split('\\\\') if '&' in line]
def eq(actual,expected,label):
 assert len(actual)==len(expected),(label,actual,expected)
 for x,y in zip(actual,expected):
  if isinstance(y,str):assert x==y,(label,x,y)
  else:assert abs(float(x)-y)<1e-7,(label,x,y)
 checks.append(label)
for v,name,r in zip(VS,names,rows('performance')):
 c=P['coverage'][v];base=P['coverage']['base']
 eq(r,[name,c['solved'],'---' if v=='base' else '+'+str(c['solved']-base['solved']),round(W['coverage'][v]['par2'],1),round(W['coverage'][v]['par2']/W['coverage']['base']['par2'],3),round(W['coverage'][v]['common_wall'],1),round(W['coverage'][v]['common_wall']/W['coverage']['base']['common_wall'],3)],'performance/'+v)
for v,name,r in zip(VS,names,rows('proof')):
 p=P['proof'][v];eq(r,[name,p['frame_entries'],round(p['frame_entries']/P['proof']['base']['frame_entries'],3),p['generalization_events'],p['SMT_queries']],'proof/'+v)
for v,name,r in zip(VS,names,rows('rerun')):
 q=I['totals'][{'base':'base','eq_can_only':'eqcan','freq_ast_asc':'order','eq_can_freq_ast_asc':'eqcan_order'}[v]]
 eq(r,[name,q['unique_clauses'],round(q['mean_literals'],3),round(q['mean_literal_ast_per_clause'],3),q['queries'],round(q['wall'],3)],'invariant/'+v)
for v,name,r in zip(VS,names,rows('strata')):
 expected=[name]
 for g in ['Safe','Unsafe','>=1','>=10']:
  q=W['groups'][g][v];expected.append(round(q['ratio'],3))
 eq(r,expected,'strata/'+v)
for kind,name,r in zip(['eq','ineq'],['Equality','Inequality'],rows('representation')):
 q=A['solved'][kind];eq(r,[name,q['keys'],q['alias_keys'],round(100*q['alias_keys']/q['keys'],1)],'representation/'+kind)
status={'Safe':'S','Unsafe':'U','Timeout':'T'}
expected={x['model'].replace('FMCAD08/Int/','').removesuffix('.lus') if hasattr(str,'removesuffix') else x['model'].replace('FMCAD08/Int/','')[:-4]:[status[x[v]] for v in VS] for x in P['status_migrations']}
rs=rows('migrations');assert len(rs)==20
for row in rs:eq(row[1:],expected[row[0]],'migration/'+row[0])
# Cross-check numeric claims rounded exactly as printed.
def claim(value,decimals,token,label):
 assert round(value,decimals)==float(token),(label,value,token);checks.append(label)
b=W['coverage']['base'];c=W['coverage']['eq_can_freq_ast_asc'];claim(100*(1-c['common_wall']/b['common_wall']),1,'27.2','overall wall reduction');claim(100*(1-c['par2']/b['par2']),1,'3.7','PAR2 reduction')
for g,wall in [('Safe','25.3'),('Unsafe','28.6')]:
 q=W['groups'][g]['eq_can_freq_ast_asc'];claim(100*(1-q['ratio']),1,wall,g+' wall')
q=W['groups']['>=10']['eq_can_freq_ast_asc'];claim(100*(1-q['ratio']),1,'41.9','expensive time reduction')
claim(100*(1-P['proof']['eq_can_freq_ast_asc']['generalization_events']/P['proof']['base']['generalization_events']),1,'5.5','event reduction')
for key,val in [('wall','21.5'),('queries','23.1'),('unique_clauses','9.3'),('mean_literals','2.4'),('mean_literal_ast_per_clause','9.7'),('literal_ast','13.8')]:claim(100*(1-I['totals']['eqcan_order'][key]/I['totals']['base'][key]),1,val,'invariant '+key)
D={v:{x['filename']:x for x in csv.DictReader((R/'data/performance/raw'/(v+'.csv')).open())} for v in VS};sol=lambda r:r['property_result'] in ('Safe','Unsafe') and r['timed_out']!='true';common=[k for k in D['base'] if all(sol(D[v][k]) for v in VS)]
assert sum(sol(D['base'][k]) and sol(D[VS[-1]][k]) for k in D['base'])==699
assert sum(not sol(D['base'][k]) and sol(D[VS[-1]][k]) for k in D['base'])==9
assert sum(sol(D['base'][k]) and not sol(D[VS[-1]][k]) for k in D['base'])==7
for lo,hi,n,ratio in [(0,1,616,1.034),(1,10,60,1.002)]:
 ks=[k for k in common if lo<=float(D['base'][k]['execution_time(s)'])<hi];assert len(ks)==n
 claim(sum(float(D[VS[-1]][k]['execution_time(s)']) for k in ks)/sum(float(D['base'][k]['execution_time(s)']) for k in ks),3,str(ratio),'runtime stratum '+str(lo))
# Per-instance structural counts and the stated counterexample.
rowsI=list(csv.DictReader((R/'generated/invariant_structure/per_model.csv').open()));idx={(r['model'],r['variant']):r for r in rowsI};models={r['model'] for r in rowsI}
for key,counts in [('unique_clauses',[73,274,37]),('literal_ast',[165,179,40])]:
 pairs=[(int(idx[m,'base'][key]),int(idx[m,'eqcan_order'][key])) for m in models];assert [sum(y<x for x,y in pairs),sum(y==x for x,y in pairs),sum(y>x for x,y in pairs)]==counts
m='FMCAD08/Int/protocol/rtp_vt.lus';assert int(idx[m,'base']['literal_ast'])==2271 and int(idx[m,'eqcan_order']['literal_ast'])==851;assert round(float(idx[m,'base']['wall']),3)==56.925 and round(float(idx[m,'eqcan_order']['wall']),3)==59.929
subset=[m for m in models if float(idx[m,'base']['wall'])>1];assert len(subset)==52
for key,counts,reduction in [('unique_clauses',[28,6,18],14.6),('literal_ast',[32,1,19],15.2)]:
 pairs=[(int(idx[m,'base'][key]),int(idx[m,'eqcan_order'][key])) for m in subset]
 assert [sum(y<x for x,y in pairs),sum(y==x for x,y in pairs),sum(y>x for x,y in pairs)]==counts
 claim(100*(1-sum(y for x,y in pairs)/sum(x for x,y in pairs)),1,str(reduction),'Base>1s structural '+key)
for key,n,fast in [('unique_clauses',274,268),('literal_ast',179,178)]:
 unchanged=[m for m in models if idx[m,'base'][key]==idx[m,'eqcan_order'][key]];assert len(unchanged)==n
 assert sum(float(idx[m,'base']['wall'])<=1 for m in unchanged)==fast
checks.append('Unchanged structure and Base wall-clock <=1s')
checks.extend(['pairwise cohort and gain/loss counts','per-instance structural distributions','rtp_vt example'])
out={'passed':True,'checks':checks,'scope':'Evaluation numerical table rows, central rounded claims, representation statistics, status migrations, structural distributions and named counterexample. No claim of exhaustive proof of every sentence or original execution protocol.'}
(R/'checks/manuscript_numeric_audit.json').write_text(json.dumps(out,indent=2)+'\n');print('PASS:',len(checks),'numerical checks against manuscript/data.')
