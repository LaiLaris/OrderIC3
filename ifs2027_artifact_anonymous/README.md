# Equality Canonicalization and Adaptive Literal Ordering: Evaluation Artifact

This anonymous review copy contains experiment records, source snapshots, executable snapshots, and portable analysis scripts. Personal path prefixes and identifying revision metadata have been redacted. Executables have had debug information removed using `strip --strip-debug`. Hashes refer to the files in this release. The original records remain preserved separately. See `docs/ANONYMIZATION.md` for the scope of these changes and `docs/READINESS.md` for remaining publication checks.

## Quick start: reproduce retained results

Use Python 3.8 or newer with Matplotlib. The analyses use local files and do not invoke Kind2 or access the network.

```bash
python3 scripts/verify.py --checksums
python3 scripts/reproduce.py
```

Outputs are written to `generated/` and `figures/`:

- `wall_metrics.json` / `wall_tables.md`: paper wall-clock PAR-2, common-set time and groups defined by Base wall-clock runtime.
- `performance_tables.json` / `.md`: retained internal-time cross-checks, status migrations, frame entries, generalization events and SMT queries. Internal-time results are preserved for provenance; manuscript runtime comparisons use wall-clock time.
- `invariant_structure/results.json` and `per_model.csv`: exported final support clauses, literal lengths, AST sizes, and deletion-stage queries.
- `representation/summary.json`: relation-representation audit on Base logs, including the 37.0% and 3.6% figures.
- `runtime_overview.pdf` and `invariant_structure.pdf`: paper evaluation plots on the full comparison cohorts. The 52-instance `invariant_structure_gt1s.pdf` is retained as an additional analysis, outside the paper.

The runtime and structural figures are regenerated from paired input records. PDF byte hashes can differ across runs because of metadata; the numerical measurements are the reproducibility target.

## Evaluation records

| Experiment | Instances | Configurations | Retained runs | Comparison sets |
|---|---:|---:|---:|---|
| Overall performance | 786 | 4 | 3,144 | 697 common solved; 385 common Safe |
| Invariant structure | 385 | 4 | 1,540 | 384 common Safe |

Performance coverage is 706 / 707 / 708 / 708. The combination's common-set wall-clock time is 1,077.390 s versus 1,479.750 s for Base. The invariant experiment has one EqCan timeout; all other runs are Safe. Its combination totals are 1,416 unique support clauses, 23,201 literal AST occurrences, 169,651 generalization queries, and 606.791 s wall-clock time.

`data/performance/` and `data/invariant_structure/` remain distinct. The latter does not replace the former. Generalization queries in the invariant experiment are the initiality and relative-inductiveness queries within sequential deletion; the performance CSV's `solver` field covers the entire IC3 run.

**Counter audit:** the historical generalization-event counter has a same-line-printing issue. The manuscript and default table output now use corrected marker counts; the script also records historical values for provenance. See `docs/STATISTICS_AUDIT.md` before using these values in a submission.

## Contents

- `data/`: original CSVs, logs, invariant exports, and provenance records.
- `benchmarks/`: fixed input manifest, hashes, locally copied inputs, and Safe-cohort selection.
- `bin/`: Linux x86-64 executable snapshots for the two experiments, with debug information removed for anonymous review.
- `source/measurement/`: complete frozen source of the instrumented experiment.
- `source/performance_IC3.ml`: retained IC3 file from the performance experiment.
- `source/performance_reconstruction/`: explicitly labelled reconstruction using the frozen source tree with that retained IC3 file restored; not a complete historical source archive.
- `provenance/`: original measurement configuration, instrumentation patch, and runner.
- `reference/`: retained analysis outputs used for numerical cross-checks.
- `scripts/`: portable data verification, analysis, plotting, and replay scripts.
- `environment/`: dependency notes and current-machine version capture.

## Executing new runs

The retained executables require Linux x86-64, Z3, and their shared libraries, including ZeroMQ (`libzmq.so.5`). They need local interprocess communication. A sandbox denying ZeroMQ networking can abort with `Operation not permitted (src/ip_resolver.cpp:518)`.

Use `ldd bin/measurement-kind2` to inspect shared-library resolution. The recorded Z3 version for the invariant experiment is 4.8.7 (64-bit). Provide a Z3 path through `--z3` if necessary.

Dry runs print the job count without starting solvers:

```bash
python3 scripts/run_experiment.py --experiment performance --limit 1
python3 scripts/run_experiment.py --experiment invariant_structure --limit 1
```

Small executable checks, with new output directories:

```bash
python3 scripts/run_experiment.py --experiment performance --limit 1 --timeout 15 --execute --output replays/performance_smoke
python3 scripts/run_experiment.py --experiment invariant_structure --limit 1 --timeout 15 --execute --output replays/invariant_smoke
```

Full reruns (serial, default 300-second limit):

```bash
python3 scripts/run_experiment.py --experiment performance --execute --output replays/performance_full
python3 scripts/run_experiment.py --experiment invariant_structure --execute --output replays/invariant_full
```

A full performance repeat has 3,144 jobs: timeout-only upper bound 262 hours plus overhead. The invariant experiment has 1,540 jobs: upper bound 128.3 hours plus overhead. The original retained main experiment manifest reports about 15.6 hours including its execution history; the measured Safe cohort previously used about 38 minutes per repeat. These observations do not guarantee new runtimes.

The portable replay schedule is a new fixed interleaved schedule. It does not recreate the performance experiment's historical scheduling/resume sequence. Exit code 40 is Kind2's normal Unsafe result. New replay logs remain separate from all retained observations. Fresh timings and solver trajectories can differ. Replay outputs are JSONL and logs; `reproduce.py` analyzes the retained paper records, not arbitrary replay directories.

## Building the frozen source

See `environment/BUILD.md`. The measurement source was successfully rebuilt in the preparation environment. The reconstructed performance tree is provided with an explicit provenance limitation; equivalence to the retained historical executable is not established.

Clean-environment validation passed on 2026-10-29 in a fresh Ubuntu 24.04.4 rootfs. Dependencies were installed anew, retained analyses were reproduced, both anonymous executables ran successfully, and a new OCaml 4.14.0 installation rebuilt the frozen measurement source. All 12 short runs and eight invariant exports passed; 22,329 numerical fields matched the retained results within floating-point tolerance. See `environment/CLEAN_ENVIRONMENT.md` and `checks/clean_environment.json`.

## Before publication

Review benchmark redistribution rights, anonymization of historical paths, the historical rerun policy, and the counter audit in `docs/READINESS.md`. Preserve upstream licenses and acknowledgments. A public archive DOI and corresponding manuscript data-availability statement can be added after an archive is actually published.
