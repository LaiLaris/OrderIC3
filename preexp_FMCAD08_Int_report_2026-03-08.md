# Pre-Experiment Report (FMCAD08/Int, sampled logs)

## Data Scope
- Input directory: `/home/lyh/kind2-exp/OrderIC3/log_pre_exp/FMCAD08/Int`
- Parsed logs: 276 files = 92 cases x 3 orders (`l1l2`, `l2l1`, `original`)
- Complete triples: 92/92
- Cases where all 3 runs reached `<Done>`: 86

## Experimental Protocol
Following the earlier plan, we focused on **early-process features** (not only final runtime):
1. Winner definition per case: min `<Done> ... in Xs` among 3 orders.
2. Early window: first 8 occurrences of `New clause at frontier`.
3. Extracted features:
   - Set-level: arithmetic ratio, boolean ratio, diff-like ratio, etc.
   - **Order-sensitive**: arithmetic frontload in clause literal positions.
4. Winner-vs-loser paired comparison across cases.

## Core Findings

### 1) Winner distribution (all-3-done cases, n=86)
- `l1l2`: 51 wins
- `l2l1`: 22 wins
- `original`: 13 wins

### 2) Early arithmetic **position** is the strongest stable signal
Winner - loser deltas (paired over all winner-loser pairs):
- `front_arith` (arith literals appear earlier in each clause):
  - mean `+0.1704`, median `+0.2630`, positive ratio `0.669`
- `first_lit_arith` (first literal arithmetic):
  - mean `+0.3623`, median `+1.0`, positive ratio `0.581`

Sign test (winner-loser pairs, n=118 on valid subset) for `front_arith > 0`:
- positive ratio: `0.678`
- p-value: `0.000138`

Interpretation:
- Across cases, the faster order is usually the one that pushes arithmetic constraints to earlier literal positions inside new clauses.
- This is stronger and more stable than raw arithmetic/boolean proportion alone.

### 3) Process-level effects align with this mechanism
Winner tends to have lower:
- `k`
- `Solver`
- `Neg_state`
- `frame_sum`
- `Forward propagations`
- `checksat_share`

This matches the earlier hypothesis: better early literal ordering -> faster frontier tightening -> less downstream search.

### 4) Pairwise predictive utility of `front_arith`
Using only rule “higher `front_arith` likely faster”:
- `l1l2` vs `l2l1`: accuracy `0.643`
- `l1l2` vs `original`: accuracy `0.753`
- `l2l1` vs `original`: accuracy `0.442`

Interpretation:
- This single signal is useful mainly when deciding whether to prefer `l1l2`.
- It is insufficient to robustly separate `l2l1` and `original`; additional features are needed.

## Practical Rule Candidate (for adaptive sorting)
Use a two-stage selector:
1. Early-stage probe (first N clauses) computes `front_arith`.
2. If arithmetic frontload is high, prefer an arithmetic-first ordering (currently closer to `l1l2`).
3. Else route to a secondary decision rule to separate `l2l1` vs `original` (needs more features).

## Why this is a "general" phenomenon (in this sample)
- It appears across multiple categories (`memory1`, `misc`, `simulation`, etc.).
- It is tied to **clause-internal position**, not domain-specific variable names.
- It remains visible under winner-loser paired analysis.

## Limits
- This is a representative subset (92 cases), not full 800+.
- Current feature set still cannot reliably choose between `l2l1` and `original`.

## Next step on full 800x3
1. Keep `front_arith` as a primary feature.
2. Add secondary features for `l2l1` vs `original` split:
   - boolean-gating frontload
   - template diversity in first N clauses
   - per-clause family transition entropy
3. Train/evaluate a simple interpretable selector and report worst-case slowdown bounds.
