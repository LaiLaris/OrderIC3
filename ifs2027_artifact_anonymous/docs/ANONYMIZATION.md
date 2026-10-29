# Anonymous review copy

Prepared on 2026-10-29. The original artifact is preserved separately.

- Personal home-directory prefixes in logs, commands, configuration records, and documentation are replaced with `/home/artifact`.
- Identifying historical repository revision metadata is withheld for anonymous review.
- The two Linux executables are processed with GNU `strip --strip-debug` to remove embedded build paths from debug information. Their executable code is retained. Original and release binary hashes are recorded in `anonymization.json`.
- Build caches, Python caches, Git metadata, and old generated/replay outputs are omitted. Analysis outputs can be regenerated with `python3 scripts/reproduce.py`.
- Hash references in metadata and the release checksum manifest are recalculated for the sanitized files. The original artifact retains the original hashes and records.
- Upstream copyright notices, contributor credits, and license files are retained.

Historical absolute paths in metadata describe the recorded runs. They are not installation paths. Current replay and analysis commands resolve inputs relative to this artifact's root.

Release checks cover personal identity strings in text and binary files, retained numerical results, input hashes, analysis reproduction, and short executable runs. This preparation does not resolve benchmark redistribution permission or establish a clean-environment installation.
