from pathlib import Path
import json,re,subprocess,runpy,itertools
p=Path(__file__).resolve().parent;ns=runpy.run_path(str(p/'analyze.py'));norm=ns['norm']
# Boundary and polarity checks against independently understood identities.
pairs=[('(> top.x@0 0)','(>= top.x@0 1)',True),('(> top.x@0 0)','(>= top.x@0 0)',False),('(not (> top.x@0 0))','(<= top.x@0 0)',True),('(< top.x@0 top.y@0)','(> top.y@0 top.x@0)',True),('(>= (* 2 top.x@0) 1)','(>= top.x@0 1)',True),('(<= (* 2 top.x@0) 1)','(<= top.x@0 0)',True),('(not (> top.x@0 (+ top.y@0 1)))','(<= top.x@0 (+ top.y@0 1))',True)]
for a,b,eq in pairs:assert (norm(a)[1]==norm(b)[1])==eq,(a,b)
examples=json.loads((p/'examples.json').read_text());tests=[]
for e in examples:
 for alias in e['aliases']:
  texts=[f['text'] for f in alias['forms']]
  for t in texts[1:]:tests.append((texts[0],t))
variables=set(re.findall(r'top[\w.@]+',' '.join(a+b for a,b in tests)))
script='(set-logic QF_LIA)\n'+'\n'.join('(declare-const '+v+' Int)' for v in sorted(variables))+'\n'
for a,b in tests:script+='(push)\n(assert (xor '+a+' '+b+'))\n(check-sat)\n(pop)\n'
(p/'alias_equivalence_checks.smt2').write_text(script)
r=subprocess.run(['z3','-in'],input=script,text=True,capture_output=True,check=True)
results=r.stdout.splitlines();assert len(results)==len(tests) and set(results)=={'unsat'},r.stdout[:1000]
result={'normalizer_boundary_tests':len(pairs),'inequality_alias_pairs_checked':len(tests),'all_equivalent':True,'z3_version':subprocess.check_output(['z3','--version'],text=True).strip()}
(p/'validation.json').write_text(json.dumps(result,indent=2)+'\n');print(result)
