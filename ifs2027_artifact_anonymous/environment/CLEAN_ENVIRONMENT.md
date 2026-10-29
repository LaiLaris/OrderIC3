# Clean-environment validation

The validation protocol uses a fresh Ubuntu 24.04.4 amd64 user space, populated from Ubuntu Base and run in a separate user/mount/PID namespace. The host kernel is shared. Host Conda environments, Python packages, OCaml installations, and shared-library directories are not mounted into this environment.

Base image: `ubuntu-base-24.04.4-base-amd64.tar.gz`.

SHA-256: `c1e67ef7b17a6300e136118bd1dc04725009cb376c1aad10abcf8cd453628d58`.

The image was downloaded from the Tsinghua Ubuntu mirror and checked against [Ubuntu's official checksum list](https://cdimage.ubuntu.com/ubuntu-base/releases/24.04/release/SHA256SUMS). The host DNS configuration and a CA certificate bundle were supplied for network bootstrap. Dependencies were then installed inside the new root filesystem.

## Install dependencies in a disposable Ubuntu environment

Run the following package-installation commands as root inside the disposable environment:

```bash
apt-get update
apt-get install -y --no-install-recommends \
  ca-certificates python3 python3-matplotlib z3 libzmq3-dev \
  build-essential opam pkg-config m4 curl git rsync unzip
```

The single-ID user namespace used for this test cannot assign files to additional Ubuntu groups. `fontconfig-config` therefore required installing `fakeroot` and running `fakeroot dpkg --configure -a`. Package consistency was then checked with `apt-get check`. This workaround concerns package ownership during rootless installation.

## Validate retained analysis and executables

From the artifact directory:

```bash
python3 scripts/verify.py --checksums
python3 scripts/reproduce.py
python3 scripts/run_experiment.py --experiment performance \
  --limit 1 --timeout 20 --execute --output replays/clean_performance
python3 scripts/run_experiment.py --experiment invariant_structure \
  --limit 1 --timeout 20 --execute --output replays/clean_invariants
```

The output directories must be new. The performance model has an Unsafe result; Kind2 exit code 40 is expected. The invariant model is Safe and must produce one invariant export per configuration. Recomputed numerical outputs are compared against the retained analysis, allowing floating-point rounding differences between Python versions.

## Install a new compiler and build the frozen measurement source

Inside the same disposable environment:

```bash
export OPAMROOT=/opt/opam
export OPAMJOBS=4
opam init default https://opam.ocaml.org --bare --disable-sandboxing --no-setup -y
opam switch create 4.14.0 ocaml-base-compiler.4.14.0 -y --jobs=4
opam install --switch=4.14.0 -y --jobs=4 \
  dune.3.20.2 dune-build-info.3.20.2 \
  menhir.20250912 menhirLib.20250912 num.1.6 yojson.2.2.2 \
  zmq.5.3.0 graphicspdf.2.2.1
opam exec --switch=4.14.0 -- dune build --root source/measurement src/kind2.exe
python3 scripts/run_experiment.py --experiment invariant_structure \
  --binary source/measurement/_build/default/src/kind2.exe \
  --limit 1 --timeout 20 --execute --output replays/clean_rebuilt
```

Opam's nested sandbox is disabled because the whole installation already runs inside the disposable outer namespace. The compiler and all installed OCaml packages are downloaded and built anew.

## Evidence and scope

The recorded outcome is in `checks/clean_environment.json`; supporting logs are in `checks/clean_environment/`. The clean-environment test covers result analysis, both retained executables, and rebuilding the complete frozen measurement source. Main-experiment source reconstruction retains the provenance limitation described in `docs/READINESS.md`.

The installed distribution Z3 version differs from the historical experiment. Short runs check operation and result/export handling. Historical performance measurements remain the retained paper observations.
