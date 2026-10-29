# Build and runtime environment

The preserved instrumented source is in `source/measurement`. Its file hashes are checked against the original source manifest. It builds using OCaml 4.14.0 and Dune 3.20.2 in the tested environment. `current_versions.json` records the preparation environment, not a historical lockfile for the original performance experiment.

The source dependency declarations are `kind2.opam`, `dune-project`, and `src/dune`. In addition to the declared Kind2 dependencies, this frozen tree links `graphicspdf`; include it in a new environment. A clean network installation and source rebuild passed in a fresh Ubuntu 24.04.4 rootfs on 2026-10-29; see `CLEAN_ENVIRONMENT.md` and `checks/clean_environment.json`.

An environment can be provisioned with OCaml 4.14 and the packages `dune`, `dune-build-info`, `menhir`, `num`, `yojson`, `zmq`, and `graphicspdf`, including their transitive dependencies. System libraries for ZeroMQ are required.

Build within the source directory, explicitly selecting it as the Dune root:

```bash
cd source/measurement
dune build --root . src/kind2.exe
```

The executable is `_build/default/src/kind2.exe`. To replay using a rebuilt executable from the artifact root:

```bash
python3 scripts/run_experiment.py --experiment invariant_structure --binary source/measurement/_build/default/src/kind2.exe --limit 1 --timeout 15 --execute --output replays/rebuilt_smoke
```

Upstream Kind2's Apache-2.0 license is preserved as `source/measurement/LICENSE.rst`. Do not infer permission to redistribute the independently sourced benchmark collection from this source license.
