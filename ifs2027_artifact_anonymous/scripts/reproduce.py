"""Recompute retained-result tables and figures; never launches Kind2."""
from pathlib import Path
import subprocess,sys,os
R=Path(__file__).resolve().parents[1];(R/'figures').mkdir(exist_ok=True);(R/'generated').mkdir(exist_ok=True)
env=os.environ.copy();env.setdefault('MPLCONFIGDIR','/tmp/ifs-artifact-matplotlib')
for name in ['verify.py','reproduce_tables.py','analyze_invariants.py','analyze_wall.py','plot_runtime.py','plot_structure.py','plot_structure_gt1s.py','analyze_representation.py']:
 with (R/'generated'/(name+'.log')).open('w') as log:subprocess.run([sys.executable,str(R/'scripts'/name)],cwd=str(R),env=env,check=True,stdout=log,stderr=subprocess.STDOUT)
 print('PASS:',name,flush=True)
print('Results: generated/ and figures/.')
