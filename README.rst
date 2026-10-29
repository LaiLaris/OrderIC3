OrderIC3
========

Equality canonicalization and adaptive literal ordering for SMT-based IC3/PDR inductive generalization, implemented in Kind 2 v3.0.0's IC3QE engine.

This repository accompanies **Optimizing IC3 Inductive Generalization with Structural Features and Historical Feedback**. EqCan aligns supported linear integer equalities, enabling syntactic duplicate elimination and shared history keys. Order attempts literal deletion in ascending order of historical retention score, AST size, and original position. Deletion decisions use the existing initiality and relative-inductiveness checks.

Start with the `evaluation artifact <ifs2027_artifact_anonymous/README.md>`_ to reproduce the paper's tables and figures or run the retained experimental executables.

Reproduce the paper results
--------------------------

Run each command block below from the repository root. Analysis requires Python 3.8 or newer with Matplotlib installed. Parenthesized blocks keep directory changes local to that block:

.. code-block:: bash

   (
     cd ifs2027_artifact_anonymous &&
     python3 scripts/verify.py --checksums &&
     python3 scripts/reproduce.py
   )

The first command checks the input files, experimental records, source snapshot, executables, and release checksums. The second regenerates tables in ``generated/`` and figures in ``figures/`` from the retained records. This analysis does not launch model-checking experiments.

See `environment setup <ifs2027_artifact_anonymous/environment/BUILD.md>`_ for dependencies and `clean-environment validation <ifs2027_artifact_anonymous/environment/CLEAN_ENVIRONMENT.md>`_ for the tested installation procedure.

Try the executables
-------------------

The artifact includes two Linux x86-64 executables:

* ``bin/performance-kind2``: the retained overall-performance executable.
* ``bin/measurement-kind2``: the instrumented executable that exports final support clauses and records generalization queries.

They require Z3 and shared libraries including ``libzmq.so.5``. From the repository root, run one Safe instance under all four paper configurations:

.. code-block:: bash

   (
     cd ifs2027_artifact_anonymous &&
     python3 scripts/run_experiment.py \
       --experiment invariant_structure \
       --limit 1 --timeout 15 --execute \
       --output replays/quickcheck
   )

The output directory must be new. Results, logs, and invariant exports are written there. Omitting ``--execute`` prints the planned job count. Full-suite commands and expected run counts are documented in the artifact README.

To check the included ``microwave01.lus`` model directly with EqCan+Order, from the repository root:

.. code-block:: bash

   ifs2027_artifact_anonymous/bin/performance-kind2 \
     --enable IC3QE --smt_solver z3 --color false --timeout 300 \
     --ic3qe_ge_eq_can \
     --ic3qe_freq_sort --ic3qe_ast_complexity \
     --ic3qe_ast_complexity_order asc \
     ifs2027_artifact_anonymous/benchmarks/inputs/FMCAD08/Int/large/microwave01.lus

This example should return Safe. Replace the final path with another Lustre model to check your own input.

Configurations
--------------

All four paper configurations use ``--enable IC3QE --smt_solver z3``. Their additional flags are:

.. list-table:: Paper configurations
   :header-rows: 1
   :widths: 20 80

   * - Configuration
     - Additional flags
   * - Base
     - None
   * - EqCan
     - ``--ic3qe_ge_eq_can``
   * - Order
     - ``--ic3qe_freq_sort --ic3qe_ast_complexity --ic3qe_ast_complexity_order asc``
   * - EqCan+Order
     - All EqCan and Order flags above

The development source additionally provides ``--ic3qe_indgen_literal_order asc`` and ``desc`` for the motivating example. These fixed textual orders take precedence over adaptive ordering. The score decay factor is 0.99, and each eligible reduced clause distributes a total retention reward of one among its literals.

Evaluation and validation
-------------------------

The overall evaluation contains 786 integer-arithmetic Lustre instances and 3,144 retained runs. EqCan+Order solves 708 instances versus 706 under Base and reduces cumulative wall-clock time by 27.2% on the 697 instances solved by all four configurations. The invariant-structure experiment retains 1,540 runs on 385 selected Safe instances, with a 384-instance common Safe comparison set.

Clean-environment validation passed in a fresh Ubuntu 24.04.4 user space with independently installed Python, Matplotlib, Z3, OCaml 4.14.0, and build dependencies. The checks reproduced the retained analyses, ran both artifact executables, rebuilt the frozen measurement source, and exercised the rebuilt executable. All 12 short runs and eight invariant exports passed; 22,329 numerical fields matched within floating-point tolerance. The detailed `validation report <ifs2027_artifact_anonymous/checks/clean_environment.json>`_ and `logs <ifs2027_artifact_anonymous/checks/clean_environment/>`_ are included.

Source and build
----------------

The development implementation is in ``src/``. The artifact preserves the experimental executable snapshots, the original performance IC3 file, and the complete frozen measurement source. The reconstructed performance source tree is identified separately in the artifact's provenance documentation.

To build the frozen measurement source after installing the dependencies listed in the environment instructions:

.. code-block:: bash

   (
     cd ifs2027_artifact_anonymous/source/measurement &&
     dune build --root . src/kind2.exe
   )

Then test the rebuilt executable, again from the repository root:

.. code-block:: bash

   (
     cd ifs2027_artifact_anonymous &&
     python3 scripts/run_experiment.py \
       --experiment invariant_structure \
       --binary source/measurement/_build/default/src/kind2.exe \
       --limit 1 --timeout 15 --execute \
       --output replays/rebuilt_quickcheck
   )

Both quick checks should produce four Safe results and four invariant exports. Use new output directory names when repeating them. Historical records remain available for reproducing the reported measurements.

Repository layout
-----------------

.. code-block:: text

   src/                              Development implementation
   ifs2027_artifact_anonymous/
     README.md                       Detailed artifact instructions
     bin/                            Experimental executables
     benchmarks/                     Input manifest, hashes, and inputs
     data/                           Performance logs and invariant exports
     scripts/                        Verification, analysis, plotting, replay
     source/                         Frozen source and reconstruction
     environment/                    Build and clean-install instructions
     checks/                         Validation reports and logs
     docs/                           Result mapping, provenance, upstream README
   LICENSE.rst                       Kind 2 Apache-2.0 license

Upstream project and licenses
-----------------------------

OrderIC3 builds on `Kind 2 <https://github.com/kind2-mc/kind2>`_. The original project description, general engine options, and upstream installation information are preserved in the `upstream README <ifs2027_artifact_anonymous/docs/README.kind2.rst>`_. Upstream copyright notices and the `Apache-2.0 license <LICENSE.rst>`_ are retained.

Benchmarks originate from the `public Kind 2 benchmark repository <https://github.com/kind2-mc/kind2-benchmarks>`_. Its redistribution terms are considered separately from the Kind 2 software license; see the artifact's `publication checks <ifs2027_artifact_anonymous/docs/READINESS.md>`_.
