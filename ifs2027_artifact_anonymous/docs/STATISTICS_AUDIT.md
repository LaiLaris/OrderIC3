# Historical generalization-event counter audit

The historical paper analysis counts a marker by scanning **subsequent lines** for a complete printed clause body. Some logs print a clause on the marker's own line. For such events the old scanner starts too late and can consume a later event. The corrected raw-marker count includes all occurrences of `New clause from inductive generalization` on the same 385 common Safe instances.

| Configuration | Historical table value | Complete marker count | Difference |
|---|---:|---:|---:|
| Base | 5,264 | 5,270 | +6 |
| EqCan | 5,024 | 5,031 | +7 |
| Order | 4,990 | 4,995 | +5 |
| EqCan+Order | 4,976 | 4,982 | +6 |

Both counting methods round the Base-to-combination reduction to 5.5%. Frame-entry counts, runtime, coverage, invariant exports, AST measurements, and SMT query totals are unaffected by this parser issue.

`generated/performance_tables.json` records both `generalization_events` (corrected complete count), `historical_generalization_events` (legacy reproduction), and `all_marker_events` (complete marker count). The manuscript's four counts were corrected on 2026-10-16 with author authorization. The default reproduction now reports corrected counts; legacy values remain available for provenance. The paper's marker-occurrence definition already matches the corrected count.

The statistic counts logged shorter-clause events, including repeated events and preprocessing reductions. It is not a count of all generalization calls or unique final invariant clauses.
