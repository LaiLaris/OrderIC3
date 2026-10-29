# Preparation status and publication checks

Original preparation completed on 2026-10-16; anonymous review copy prepared on 2026-10-29. No archive was uploaded during this preparation. See `ANONYMIZATION.md` for the release transformations.

## Completed

- Copied the four retained performance CSVs and 3,144 logs.
- Copied the invariant experiment's 1,540 run records/logs and 1,539 invariant exports.
- Verified the original measurement configuration, runner, binary, source-manifest hashes, and original performance CSV hashes.
- Verified 786 benchmark input hashes and their match to the measurement records.
- Recomputed coverage, penalized runtime, Safe/Unsafe and expensive-instance strata, frame statistics, SMT counts, final support-invariant statistics, and relation-representation frequencies.
- Reproduced the runtime and final-support-structure plots from local inputs.
- Repeated the analysis from an unrelated temporary working directory with copied scripts and linked immutable inputs; tables, invariant per-model CSV, and representation summaries match.
- Matched the regenerated invariant per-model CSV byte-for-byte with retained analysis, and matched aggregate invariant and representation statistics exactly.
- Built the complete frozen measurement source with Dune (one pre-existing unused-value warning).
- Executed one instance under all four configurations for each retained binary: four Unsafe results for the performance smoke model; four Safe results and four invariant exports for the invariant smoke model.
- Kept full-run results, original provenance, new checks, and the two experimental datasets separate.

## Resolve before publication

1. **Resolved: manuscript event-count correction.** The four counts and the default reproduction output were corrected on 2026-10-16. The wall-clock manuscript numerical checks passed; see `checks/manuscript_numeric_audit.json` and `STATISTICS_AUDIT.md`. Historical values remain recorded for provenance.
2. **Historical performance-source limitation.** Only the original performance IC3 file and executable are verified historical snapshots. The supplied performance source tree is a reconstruction. Do not describe it as the complete original source or promise binary-identical rebuilding. The complete measurement tree has stronger provenance.
3. **Historical selected reruns.** Six timing files document particular reruns under the common 300-second limit. Their filenames and corresponding retained rows are recorded in `rerun_inventory.json`. The retained metadata says selected timeout outliers were rerun; it does not record a full predeclared selection/replacement protocol or all historical attempts. Preserve this limitation and provide any additional historical records that can establish it. Do not invent a best-of-N or first-run policy.
4. **Benchmark redistribution.** The local input copies are ready for checking, but no top-level redistribution license was found in the local benchmark directory. Confirm permission before including the inputs in a public artifact, or provide a benchmark acquisition procedure with exact hash checks. Public availability alone does not establish redistribution rights.
5. **Anonymization prepared.** This separate copy replaces personal paths and identifying revision metadata, removes executable debug information, and updates release hashes. Original records are preserved separately. Upstream third-party copyright notices remain intact. Release checks are recorded in `identity_scan.json` and `anonymization.json`.
6. **Resolved: clean-environment installation.** On 2026-10-29, dependencies were installed in a fresh Ubuntu 24.04.4 rootfs. Retained analyses and both executables passed; a new OCaml 4.14.0 installation rebuilt the frozen measurement source and the rebuilt executable passed four configuration checks. All 12 short runs and eight invariant exports passed. See `checks/clean_environment.json` and `environment/CLEAN_ENVIRONMENT.md`. `environment/current_versions.json` remains the original preparation-machine capture.
7. **Archive identity.** Add the final public URL/DOI and align the paper's data-availability statement after choosing and publishing an archive. Do not claim public archival while files remain local.

## Scope of reproducibility

The default analysis regenerates the retained paper results and does not run solvers. Portable replay creates independent new records using a fixed interleaved schedule. It does not reconstruct the original performance schedule/resume history and does not automatically substitute new observations into the manuscript. Full-suite reruns have not been performed during artifact preparation.
