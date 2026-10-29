# Runtime convention in the current manuscript

All manuscript runtime comparisons now use elapsed wall-clock time. The overall experiment reads `execution_time(s)` from the retained CSVs; the invariant experiment reads `wall_seconds` from the recorded run results. Timeout PAR-2 penalties remain 600 seconds.

The common solved cohort remains 697. The Base wall-clock >=1 s group has 81 instances; the >=10 s group has 21. On the full common solved cohort, cumulative wall-clock time decreases by 27.2%; on the instrumented common Safe cohort it decreases by 21.5%. No experiments were rerun for this change.

`generated/wall_metrics.json` and `generated/wall_tables.md` supply the manuscript runtime values. The earlier internal-IC3-time fields and their analyses remain as explicitly labelled provenance and are not the manuscript's runtime convention. Final-support structural plots contain no time metric.
