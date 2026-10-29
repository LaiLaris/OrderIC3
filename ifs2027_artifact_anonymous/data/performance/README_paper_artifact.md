# Paper Artifact

This directory is the clean paper artifact for the OrderIC3 timeout-300 experiment.

Source run: `/home/artifact/kind2-exp/OrderIC3_Auto/runs/eq_can_freq_ast_asc_all786_timeout300_rerun`
Artifact id: `eq_can_freq_ast_asc_all786_timeout300_paper_artifact_clean`

Included final artifacts:

- `raw/`: per-variant raw result CSVs
- `compare/`: aligned, pairwise, and summary comparison CSVs
- `logs/`: Kind2 logs, one `.log` per benchmark and variant
- `bin/kind2`: binary snapshot used for the artifact-local reruns
- `report.md`: regenerated experiment report
- `manifest.json`, `config_snapshot.json`, `dataset_snapshot.txt`: metadata and dataset snapshot

A small set of timeout outlier rows was rerun under Kind2 `--timeout 300`; the corresponding logs, raw rows, comparison CSVs, and report were regenerated. Helper timing files from those reruns were moved to `provenance/rerun_time_files/` and are not mixed with Kind2 `.log` files.

Final timeout distribution check:

| raw csv | timeout rows | 120s timeout rows | 300s timeout rows | odd timeout rows |
| --- | ---: | ---: | ---: | ---: |
| base.csv | 80 | 0 | 80 | 0 |
| eq_can_only.csv | 79 | 0 | 79 | 0 |
| freq_ast_asc.csv | 78 | 0 | 78 | 0 |
| eq_can_freq_ast_asc.csv | 78 | 0 | 78 | 0 |
