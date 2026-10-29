"""English adaptation of the original paper runtime figure, retaining its inset/layout.
Data: retained 786-instance, 300-second experiment. No solver execution.
"""
from __future__ import annotations
from pathlib import Path
import csv,json,hashlib,math,statistics as st
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
HERE=Path(__file__).resolve().parents[1]
RAW=HERE/'data/performance/raw'
LABEL={'base':'Base','eq_can_only':'EqCan','freq_ast_asc':'Order','eq_can_freq_ast_asc':'EqCan+Order'}
OVERVIEW_PALETTE=dict(zip(LABEL,['#475569','#2563eb','#0d9488','#e76f51']))
OVERVIEW_STYLES={'base':(0,(4,2)),'eq_can_only':(0,(5,1.5,1.2,1.5)),'freq_ast_asc':(0,(1.2,1.5)),'eq_can_freq_ast_asc':'-'}
CACTUS_MIN_BUDGET=1.0
plt.rcParams.update({'font.family':'DejaVu Serif','font.size':9,'axes.labelsize':9,'axes.titlesize':9.5,'legend.fontsize':8,'xtick.labelsize':8,'ytick.labelsize':8,'axes.linewidth':.7,'axes.grid':True,'grid.linewidth':.4,'grid.alpha':.35,'pdf.fonttype':42,'ps.fonttype':42,'figure.dpi':150,'savefig.dpi':400,'savefig.bbox':'tight','savefig.pad_inches':.02})
def titles_below(fig, axes, titles, gap=0.022, fontsize=9.5, fontweight=None):
    """Caption each panel underneath it, 软件学报 style, instead of above it.

    The y position is read from the panels' drawn extent (ticks and x label
    included) rather than assumed, and the lowest panel in the row wins so the
    captions of a row share one baseline.  Panels are centred on their own axes
    box, which for an aspect-equal panel is narrower than its grid slot.
    """
    fig.canvas.draw()
    renderer = fig.canvas.get_renderer()
    inv = fig.transFigure.inverted()
    y = min(ax.get_tightbbox(renderer).transformed(inv).y0 for ax in axes) - gap
    for ax, text in zip(axes, titles):
        x = sum(ax.get_position().intervalx) / 2
        fig.text(x, y, text, ha="center", va="top", fontsize=fontsize,
                 fontweight=fontweight)

def _paired(P, v, band=None):
    """Base-vs-v pairs where both solved and agreed on Safe/Unsafe."""
    Db, Dv = P["base"][0], P[v][0]
    out = []
    for f in Db:
        a, b = Db[f], Dv[f]
        if not (a["solved"] and b["solved"]) or a["res"] != b["res"]:
            continue
        if band is not None and not (band[0] <= a["cost"] < band[1]):
            continue
        out.append((a["cost"], b["cost"]))
    return out

def _solved_curve(P, v, grid):
    """Mean number of solved instances at each budget, over replicates."""
    curves = [
        sorted(d["cost"] for d in D.values() if d["solved"]) for D in P[v]
    ]
    return [st.mean(sum(1 for t in ts if t <= g) for ts in curves) for g in grid]

def _tint(color: str, frac: float) -> tuple[float, float, float]:
    """Blend `color` toward white by `frac`, keeping the palette's own hue."""
    r, g, b = matplotlib.colors.to_rgb(color)
    return tuple(c + (1.0 - c) * frac for c in (r, g, b))

def fig_runtime_overview(P, outdir: Path):
    """Coverage growth and paired runtime in one visually unified figure."""
    variant = "eq_can_freq_ast_asc"
    shown = ["base", "eq_can_only", "freq_ast_asc", variant]
    palette = OVERVIEW_PALETTE
    styles = OVERVIEW_STYLES

    fig, axes = plt.subplots(1, 2, figsize=(7.4, 3.5))
    # Headroom at the top holds panel (a)'s legend (the curves converge into the
    # upper-left corner, so it cannot go inside); the bottom margin holds the
    # panel captions, which sit under the axes rather than above them.
    fig.subplots_adjust(left=0.075, right=0.985, bottom=0.22, top=0.90,
                        wspace=0.27)

    # (a) Exact cumulative-solve steps over the region where search dominates.
    ax = axes[0]
    lo, hi = CACTUS_MIN_BUDGET, 300.0
    event_times = {lo, hi}
    for v in shown:
        for D in P[v]:
            event_times.update(
                d["cost"] for d in D.values()
                if d["solved"] and lo <= d["cost"] <= hi
            )
    grid = sorted(event_times)
    curves = {v: _solved_curve(P, v, grid) for v in shown}
    for i, v in enumerate(shown):
        ax.step(grid, curves[v], where="post", color=palette[v],
                lw=1.9 if v == variant else 1.35, ls=styles[v],
                label=LABEL[v], zorder=3 + i)
    all_counts = [n for v in shown for n in curves[v]]
    ymin = 10 * math.floor(min(all_counts) / 10)
    ymax = 10 * math.ceil(max(all_counts) / 10) + 2
    ax.set_xscale("log")
    ax.set_xlim(lo, hi)
    ax.set_ylim(ymin, ymax)
    ax.set_xlabel("Wall-clock time (s)")
    ax.set_ylabel("Cumulative solved instances")
    # Legend above the axes: the curves converge into the upper-left corner, so
    # any in-axes placement collides with them.
    ax.legend(loc="lower left", bbox_to_anchor=(0.0, 1.005), ncol=4,
              columnspacing=1.0, handlelength=2.2, handletextpad=0.5,
              borderaxespad=0.0, frameon=False, fontsize=7.0)

    gap = [v - b for b, v in zip(curves["base"], curves[variant])]
    # Lifted clear of the outer x tick labels; the main curves are all above
    # y≈0.7 in this x range, so there is room up to ~0.5.
    inset = ax.inset_axes([0.55, 0.22, 0.40, 0.29])
    inset.step(grid, gap, where="post", color=palette[variant], lw=1.15)
    inset.fill_between(grid, 0, gap, step="post",
                       color=_tint(palette[variant], 0.35), alpha=0.5)
    inset.axhline(0, color="#94a3b8", lw=0.55)
    inset.set_xscale("log")
    inset.set_xlim(lo, hi)
    gap_pad = max(1.0, (max(gap) - min(gap)) * 0.08)
    inset.set_ylim(min(0, min(gap)) - gap_pad, max(0, max(gap)) + gap_pad)
    inset.set_title("Additional solves over Base", fontsize=6.6, pad=2)
    inset.set_xlabel("Time (s)", fontsize=6)
    inset.set_ylabel("Additional solves", fontsize=6)
    inset.tick_params(axis="both", which="major", labelsize=5.7, length=2)

    # (b) Common-solved paired runtimes, with regions encoding who is faster.
    ax = axes[1]
    pairs = _paired(P, variant)
    if not pairs:
        raise ValueError(f"no common solved instances for base and {variant}")
    base_times, variant_times = zip(*pairs)
    positive_times = [t for pair in pairs for t in pair if t > 0]
    scatter_lo = 10 ** math.floor(math.log10(min(positive_times)))
    scatter_hi = 10 ** math.ceil(math.log10(max(positive_times)))
    diagonal = [scatter_lo, scatter_hi]
    # No region fills: the diagonal plus the two corner labels already say which
    # side is which, and a clean ground keeps the points readable.
    ax.scatter(base_times, variant_times, s=14, color=palette[variant],
               alpha=0.55, edgecolors="white", linewidths=0.25, zorder=3)
    ax.plot(diagonal, diagonal, color="#64748b", lw=1.0,
            ls=(0, (5, 2.5)), zorder=2)
    ax.set_xscale("log")
    ax.set_yscale("log")
    ax.set_xlim(scatter_lo, scatter_hi)
    ax.set_ylim(scatter_lo, scatter_hi)
    ax.set_aspect("equal", adjustable="box")
    ax.set_xlabel("Base wall-clock time (s)")
    ax.set_ylabel("EqCan+Order wall-clock time (s)")
    ax.text(0.60, 0.10, "EqCan+Order faster", transform=ax.transAxes,
            fontsize=6.4, color=palette[variant], rotation=34)
    ax.text(0.08, 0.76, "Base faster", transform=ax.transAxes,
            fontsize=6.4, color=palette["base"], rotation=34)

    # A clean, open-frame style separates this figure from the earlier paper.
    for panel in axes:
        panel.set_facecolor("#fbfcfd")
        panel.spines["top"].set_visible(False)
        panel.spines["right"].set_visible(False)
        panel.grid(True, which="major", color="#cbd5e1", lw=0.45,
                   alpha=0.62)
        panel.grid(False, which="minor")
    inset.set_facecolor("#ffffff")
    for spine in inset.spines.values():
        spine.set_color("#cbd5e1")
        spine.set_linewidth(0.55)
    inset.grid(True, which="major", color="#e2e8f0", lw=0.35)

    titles_below(fig, axes,
                 ["(a) Solved coverage over time", "(b) Paired wall-clock times"],
                 fontweight="semibold")
    fig.savefig(outdir / "runtime_overview.pdf")
    fig.savefig(outdir / "runtime_overview.png")
    plt.close(fig)
data={v:{r['filename']:r for r in csv.DictReader((RAW/(v+'.csv')).open())} for v in LABEL}
def solved(r):return r['property_result'] in ('Safe','Unsafe') and r['timed_out']!='true'
assert all(len(d)==786 for d in data.values())
assert [sum(solved(r) for r in d.values()) for d in data.values()]==[706,707,708,708]
P={v:[{m:{'solved':solved(r),'res':r['property_result'],'cost':float(r['execution_time(s)'])} for m,r in d.items()}] for v,d in data.items()}
assert len(_paired(P,'eq_can_freq_ast_asc'))==699
(HERE/'figures').mkdir(exist_ok=True)
fig_runtime_overview(P,HERE/'figures')
common=[m for m in data['base'] if all(solved(data[v][m]) for v in LABEL)]
assert len(common)==697
checks={'source_files':[{'file':str((RAW/(v+'.csv')).relative_to(HERE)),'sha256':hashlib.sha256((RAW/(v+'.csv')).read_bytes()).hexdigest()} for v in LABEL],'all_four_common_solved':len(common),'pairwise_common_solved':699,'counts':{v:sum(solved(r) for r in d.values()) for v,d in data.items()},'style':'English adaptation of original fig_runtime_overview with incremental-coverage inset','metrics':{v:{'wall_PAR2':sum(float(r['execution_time(s)']) if solved(r) else 600 for r in d.values()),'internal_PAR2':sum(float(r['total_time']) if solved(r) else 600 for r in d.values()),'common_IC3_seconds':sum(float(d[m]['total_time']) for m in common)} for v,d in data.items()}}
(HERE/'generated/runtime_figure_provenance.json').write_text(json.dumps(checks,indent=2)+'\n')
print('Original figure layout restored in English; 786 instances, 699 paired solves.')
