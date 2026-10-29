"""Independent counting of exported syntax and cross-checking measurement identities."""
import json,re,sys
from pathlib import Path

def parse(s):
 tokens=re.findall(r'\(|\)|[^\s()]+',s);i=0
 def node():
  nonlocal i
  t=tokens[i];i+=1
  if t!='(':return t
  a=[]
  while tokens[i]!=')':a.append(node())
  i+=1;return a
 x=node();assert i==len(tokens);return x

def nodes(x):
 if isinstance(x,str):return 1
 return 1+sum(nodes(y) for y in x[1:])

def main():
 out=Path(sys.argv[1]);rr=[json.loads(x) for x in (out/'results.jsonl').read_text().splitlines()]
 assert len(rr)==4 and {r['variant'] for r in rr}=={'base','eqcan','order','eqcan_order'}
 count=0
 for r in rr:
  assert r['status']=='Safe' and r['exit_code']==0 and len(r['exports'])==1
  e=json.loads((out/r['exports'][0]).read_text())
  assert e['indgen_initial_queries']>=e['indgen_relative_queries']>0
  assert e['indgen_initial_queries']+e['indgen_relative_queries']<=r['metrics']['Solver']
  cs=e['support_clauses'];assert cs
  for c in cs:
   assert c['literal_count']==len(c['literals'])
   assert c['literal_ast_sum']==sum(l['ast_nodes'] for l in c['literals'])
   clause=parse(c['clause']['term']);ll=[parse(l['term']) for l in c['literals']]
   assert clause==(['or']+ll if len(ll)>1 else ll[0])
   for t in [c['clause'],*c['literals']]:assert nodes(parse(t['term']))==t['ast_nodes'];count+=1
  terms=[parse(c['clause']['term']) for c in cs];support=parse(e['support_term']['term'])
  assert support==(terms[0] if len(terms)==1 else ['and']+terms)
  for t in [e['support_term'],e['certificate_term'],*e['properties']]:assert nodes(parse(t['term']))==t['ast_nodes'];count+=1
 result={'passed':True,'runs':4,'independently_recounted_terms':count,'checks':['all four Safe; one final export each','literal count and AST sum identities','AST count independently reconstructed from printed syntax, including negative numeral constants','support term matches returned support clauses','initial/relative query counts positive and bounded by total SMT queries'],'scope':'Instrumentation smoke check, not performance evidence or standalone semantic verification of a certificate.'}
 (out/'verification.json').write_text(json.dumps(result,indent=2)+'\n');print(json.dumps(result,indent=2))
if __name__=='__main__':main()
