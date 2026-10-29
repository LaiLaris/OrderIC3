"""Count within-run representation aliases, not semantic equality across runs.
Read original retained Base logs only; no Kind2 runs. Integer affine fragment.
"""
from pathlib import Path
import csv,json,re,runpy,math,collections,time,functools
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[1]
RUN=Path('/home/artifact/kind2-exp/OrderIC3_Auto/runs/eq_can_freq_ast_asc_all786_timeout300_paper_artifact_clean')
parse=runpy.run_path('/home/artifact/.codex/skills/ic3-log-compare/scripts/compare_ic3_logs.py')['parse_clause_blocks']
def tree(s):
 ts=re.findall(r'\(|\)|[^\s()]+',s); i=0
 def go():
  nonlocal i
  t=ts[i];i+=1
  if t!='(':return t
  a=[]
  while ts[i]!=')':a.append(go())
  i+=1;return a
 t=go()
 if i!=len(ts):raise ValueError()
 return t

def add(a,b,k=1):
 c=a.copy()
 for v,x in b.items():c[v]=c.get(v,0)+k*x
 return {v:x for v,x in c.items() if x}
def scale(a,k):return {v:x*k for v,x in a.items() if x*k}
def lin(t):
 if isinstance(t,str):
  if re.fullmatch(r'-?\d+',t):return {'':int(t)} if int(t) else {}
  if t.startswith('top.'):return {t:1}
  raise ValueError()
 op,*args=t
 if op=='+':
  a={}
  for x in args:a=add(a,lin(x))
  return a
 if op=='-':
  if len(args)==1:return scale(lin(args[0]),-1)
  a=lin(args[0])
  for x in args[1:]:a=add(a,lin(x),-1)
  return a
 if op=='*':
  a={'':1}
  for x in args:
   b=lin(x)
   if set(a)<={''}:a=scale(b,a.get('',0))
   elif set(b)<={''}:a=scale(a,b.get('',0))
   else:raise ValueError()
  return a
 raise ValueError()
def norm(s):
 t=tree(s);neg=False
 while isinstance(t,list) and t and t[0]=='not':neg=not neg;t=t[1]
 if not isinstance(t,list) or len(t)!=3 or t[0] not in ['=','>','>=','<','<=']:return None
 op=t[0];kind='eq' if op=='=' else 'ineq'
 try:a=add(lin(t[1]),lin(t[2]),-1)
 except ValueError:return ('unsupported',kind)
 vs=sorted(v for v in a if v)
 if not vs:return ('constant',kind)
 if kind=='eq':
  if a[vs[0]]<0:a=scale(a,-1)
  k=(neg,tuple(sorted(a.items())))
  return kind,k,k
 if neg:op={'>':'<=','>=':'<','<':'>=','<=':'>'}[op]
 if op in ['<','<=']:a=scale(a,-1)
 strict=op in ['<','>']
 g=functools.reduce(math.gcd, (abs(x) for x in a.values()), 0)
 realkey=(strict,tuple(sorted((v,x//g) for v,x in a.items())))
 if strict:a=add(a,{'':-1})
 # gcd only variable coefficients; exact integer rounding of bound.
 g=functools.reduce(math.gcd, (abs(a[v]) for v in vs), 0)
 intkey=(tuple((v,a[v]//g) for v in vs),a.get('',0)//g)
 return kind,intkey,realkey

def remove_zero(t):
 if isinstance(t,str):return t
 op,*args=t;args=[remove_zero(x) for x in args]
 if op=='+':
  args=[x for x in args if x!='0']
  if not args:return '0'
  if len(args)==1:return args[0]
 return [op]+args

def inspect(model,row):
 f=RUN/'logs/base'/Path(model).with_suffix('.log')
 blocks=parse(f);cache={};groups={'eq':{},'ineq':{}};learned={'eq':{},'ineq':{}}
 lines=f.read_text(errors='replace').splitlines()
 count=collections.Counter();examples=[];same=[]
 for b in blocks:
  inlearn=b.line>=2 and 'New clause from inductive generalization' in lines[b.line-2]
  local=collections.defaultdict(set)
  for lit in b.literals:
   if lit not in cache:
    try:cache[lit]=norm(lit)
    except (ValueError,IndexError):cache[lit]=('parse_error','unknown')
   n=cache[lit]
   if n is None:continue
   if n[0] not in groups:count[n[0]+'_'+n[1]]+=1;continue
   kind,k,rk=n;count[kind+'_occurrences']+=1
   groups[kind].setdefault(k,{}).setdefault(lit,(b.line,rk))
   local[(kind,k)].add(lit)
   if inlearn:learned[kind].setdefault(k,set()).add(lit)
  for (kind,k),forms in local.items():
   if len(forms)>1:
    count[kind+'_same_clause_groups']+=1
    if kind=='ineq' and len(same)<2:same.append({'line':b.line,'clause':b.cid,'forms':sorted(forms)})
 result={'model':model,'status':row['property_result'],'solved':row['timed_out']!='true' and row['property_result'] in ['Safe','Unsafe'],'clause_blocks':len(blocks),**count}
 for kind,gs in groups.items():
  result[kind+'_keys']=len(gs);result[kind+'_forms']=sum(len(v) for v in gs.values())
  result[kind+'_alias_keys']=sum(len(v)>1 for v in gs.values())
  result[kind+'_learned_alias_keys']=sum(len(v)>1 for v in learned[kind].values())
  result[kind+'_learned_keys']=len(learned[kind])
  if kind=='ineq':
   result['ineq_zero_only_alias_keys']=sum(len(v)>1 and len({repr(remove_zero(tree(t))) for t in v})==1 for v in gs.values())
   result['ineq_integer_boundary_keys']=sum(len({r for ln,r in v.values()})>1 for v in gs.values())
   for k,v in gs.items():
    if len(v)>1:examples.append({'forms':[{'text':t,'line':ln} for t,(ln,r) in v.items()],'integer_boundary':len({r for ln,r in v.values()})>1})
 return result,{'model':model,'log':str(f),'aliases':examples,'same_clause':same}

def main():
 rows=list(csv.DictReader((ROOT/'source/raw/base.csv').open()));results=[];examples=[];start=time.monotonic()
 for i,row in enumerate(rows,1):
  r,e=inspect(row['filename'],row);results.append(r)
  if e['aliases'] or e['same_clause']:examples.append(e)
  if i%100==0:print(i,'/',len(rows),'logs',round(time.monotonic()-start,1),'s',flush=True)
 fields=sorted(set().union(*(r.keys() for r in results)))
 with (HERE/'per_model.csv').open('w') as f:
  w=csv.DictWriter(f,fieldnames=fields);w.writeheader();w.writerows(results)
 def aggregate(rs):
  out={'models':len(rs)}
  for kind in ['eq','ineq']:
   out[kind]={k:sum(r.get(kind+'_'+k,0) for r in rs) for k in ['keys','forms','alias_keys','occurrences','same_clause_groups','learned_alias_keys','learned_keys']}
   out[kind]['models_with_literals']=sum(r.get(kind+'_keys',0)>0 for r in rs)
   out[kind]['models_with_aliases']=sum(r.get(kind+'_alias_keys',0)>0 for r in rs)
   out[kind]['models_with_same_clause_aliases']=sum(r.get(kind+'_same_clause_groups',0)>0 for r in rs)
  out['zero_only_alias_keys']=sum(r.get('ineq_zero_only_alias_keys',0) for r in rs)
  out['integer_boundary_keys']=sum(r.get('ineq_integer_boundary_keys',0) for r in rs)
  out['excluded']={k:sum(r.get(k,0) for r in rs) for k in fields if k.startswith(('unsupported_','constant_','parse_error_'))}
  return out
 out={'protocol':'Base retained logs; parsed printed clause blocks; keys local to each model; completed and timeout logs separated. Integer affine comparisons only. Equality keys sign-normalized without gcd; inequality keys include exact integer bound tightening.', 'solved':aggregate([r for r in results if r['solved']]),'timeout_or_other':aggregate([r for r in results if not r['solved']])}
 (HERE/'summary.json').write_text(json.dumps(out,indent=2)+'\n');(HERE/'examples.json').write_text(json.dumps(examples,indent=2)+'\n');print(json.dumps(out,indent=2))
if __name__=='__main__':main()
