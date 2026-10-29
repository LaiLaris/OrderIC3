# Paper-to-artifact result map

Use LaTeX labels rather than table numbers, which can change during editing.

| Manuscript result | Source | Reproduction |
|---|---|---|
| `tab:representation` | Base verbose logs, 706 completed cases | `scripts/analyze_representation.py` |
| `tab:configs` | `data/performance/config_snapshot.json` | Configuration metadata |
| `tab:performance` | Four raw performance CSVs | `scripts/analyze_wall.py` |
| `tab:migrations` | Per-instance solving status in four CSVs | `generated/performance_tables.json`, `status_migrations` |
| `tab:strata` | Paired common solved instances | `generated/wall_metrics.json`, `groups` |
| `tab:proof` | CSV frame/solver fields and verbose logs | `generated/performance_tables.json`, `proof`; corrected marker counts; see counter audit |
| `tab:rerun` | Recorded statuses and final invariant JSON exports | `scripts/analyze_invariants.py` |
| `fig:runtime` | Paired wall-clock measurements | `scripts/plot_runtime.py` |
| `fig:invariant-structure` | 384 common Safe invariant records | `scripts/plot_structure.py` |

The paper's motivating example and algorithm diagrams are outside this evaluation-data reproduction. The motivating logs and audits remain in the manuscript workspace and can be added separately if a full-paper artifact is desired.
