from pathlib import Path
import csv, collections, json
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.ticker import FixedLocator, FuncFormatter
root=Path(__file__).resolve().parents[1]
rows=list(csv.DictReader((root/'generated/invariant_structure/per_model.csv').open()))
data=collections.defaultdict(dict)
for r in rows:
 assert r['variant'] not in data[r['model']]
 data[r['model']][r['variant']]=r
assert len(data)==384 and all(set(v)=={'base','eqcan','order','eqcan_order'} for v in data.values())
plt.rcParams.update({'font.family':'serif','font.size':9,'axes.labelsize':9,'axes.titlesize':10,'pdf.fonttype':42,'ps.fonttype':42})
fig,axes=plt.subplots(1,2,figsize=(6.25,3.05))
metrics={}
for ax,key,title in zip(axes,['unique_clauses','literal_ast'],['(a) Unique support clauses','(b) Total literal AST size']):
 pairs=[(int(v['base'][key]),int(v['eqcan_order'][key])) for v in data.values()]
 counts=collections.Counter(pairs)
 lo=sum(y<x for x,y in pairs);eq=sum(y==x for x,y in pairs);hi=sum(y>x for x,y in pairs)
 metrics[key]={'instances':len(pairs),'lower':lo,'equal':eq,'higher':hi,'base_total':sum(x for x,y in pairs),'combined_total':sum(y for x,y in pairs)}
 for cond,color,marker in [(lambda x,y:y<x,'#24699a','o'),(lambda x,y:y==x,'#929292','o'),(lambda x,y:y>x,'#bb6242','^')]:
  pts=[(x,y,n) for (x,y),n in counts.items() if cond(x,y)]
  if pts:ax.scatter([x for x,y,n in pts],[y for x,y,n in pts],s=[14+5*n**.5 for x,y,n in pts],c=color,marker=marker,alpha=.72,edgecolors='white',linewidths=.3,zorder=3)
 limit=max(max(x,y) for x,y in pairs)*1.35
 ax.plot([0,limit],[0,limit],color='#555555',lw=.8,ls='--',zorder=1)
 ax.set_xscale('symlog',linthresh=1,linscale=.5);ax.set_yscale('symlog',linthresh=1,linscale=.5)
 ax.set_xlim(0,limit);ax.set_ylim(0,limit);ax.set_aspect('equal',adjustable='box')
 ticks=[v for v in [0,1,10,100,1000,10000] if v<=limit]
 for axis in [ax.xaxis,ax.yaxis]:axis.set_major_locator(FixedLocator(ticks));axis.set_major_formatter(FuncFormatter(lambda x,p:f'{x:g}'))
 ax.set_xlabel('Base');ax.set_ylabel('EqCan+Order');ax.set_title(title,pad=8)
 ax.text(.04,.96,f'Reduced: {lo}\nUnchanged: {eq}\nIncreased: {hi}',transform=ax.transAxes,va='top',fontsize=7.5,linespacing=1.2,bbox={'facecolor':'white','edgecolor':'none','alpha':.9,'pad':2})
 reduction=100*(1-metrics[key]['combined_total']/metrics[key]['base_total'])
 metrics[key]['total_reduction_percent']=reduction
 label='Total clauses' if key=='unique_clauses' else 'Total AST size'
 ax.text(.04,.73,f'{label}: \N{MINUS SIGN}{reduction:.1f}%',transform=ax.transAxes,va='top',fontsize=8,fontweight='bold',color='#24699a',bbox={'facecolor':'white','edgecolor':'none','alpha':.9,'pad':2})
 ax.grid(which='major',color='#e7e7e7',lw=.5,zorder=0)
 ax.spines[['top','right']].set_visible(False)
fig.tight_layout(pad=.65,w_pad=2)
fig.savefig(root/'figures/invariant_structure.pdf',bbox_inches='tight')
fig.savefig(root/'figures/invariant_structure.png',dpi=180,bbox_inches='tight')
(root/'generated/structure_figure_metrics.json').write_text(json.dumps(metrics,indent=2)+'\n')
print(json.dumps(metrics,indent=2))
