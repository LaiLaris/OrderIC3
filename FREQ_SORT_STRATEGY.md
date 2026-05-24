# Current `freq_sort` Strategy

This note summarizes the current `freq_sort` literal ordering rule used in
IC3QE inductive generalization.

## Scope

`freq_sort` is applied only when both conditions hold:

```ocaml
Flags.IC3QE.freq_sort () && frame <> []
```

So it is used only for relative inductive generalization against a non-empty
frame. It is not applied in `frame = []` global inductiveness checks.

## Trigger

The pass does not reorder every clause. It first computes a structural cluster
score and an expensive-arithmetic marker for each literal. The literals are
reordered only if at least one of these conditions holds:

```text
some literal has cluster = 2
some literal has exp = 1
```

Otherwise the original order is preserved.

## Structural Clusters

Each literal is assigned either `cluster = 1` or `cluster = 2`.

```text
cluster = 1: ordinary literal, tried earlier for deletion
cluster = 2: structural pair literal, tried later for deletion
```

Because `linear_search` tries literals in order, `cluster = 2` effectively means
the literal is protected or delayed.

### Single-variable literals

A literal is treated as `Single` when it contains exactly one state variable.

```text
key = that state variable
cluster = 2 iff the same key occurs exactly twice in the current clause
```

If the same single-variable key appears once, or more than twice, it remains
`cluster = 1`.

### Multi-variable comparison literals

A literal is treated as `Affine` when:

- it contains more than one state variable, and
- after removing a top-level negation, its outer symbol is one of
  `=`, `<=`, `>=`, `<`, or `>`.

```text
key = set of state variables in the literal
cluster = 2 iff the same key occurs exactly twice in the current clause
```

This key currently ignores coefficients, constants, polarity, and normalized
affine shape. For example, literals over the same `{g, t}` variables may share
one key even if they are `g - t = 0`, `g - t - 1 = 0`, or `-g + t + 1 = 0`.

## Expensive Arithmetic Guardrail

Each literal is also assigned either `exp = 0` or `exp = 1`.

```text
exp = 0: ordinary arithmetic shape
exp = 1: expensive arithmetic shape, tried earlier for deletion
```

A literal is currently marked `exp = 1` if either condition holds:

```text
the literal contains a let binding
the literal contains more than one div/intdiv/mod operator
```

This is intentionally narrower than "any div/mod". A simple single-variable
boundary such as:

```text
(= (div steps_remaining 60) 0)
```

is not marked expensive. This preserves the good `microwave19` path, where this
literal acts like a useful boundary condition. More complex `microwave29` shapes,
especially `let` terms containing nested `div`, are still marked expensive and
are tried early for deletion.

Expensive literals are also excluded from structural-pair protection: they do
not receive a `Single` or `Affine` structural key. This prevents a complex
arithmetic term from being delayed only because it shares a state-variable set
with another literal.

## Sorting Key

When sorting is triggered, literals are ordered by:

```text
1. expensive arithmetic rank, ascending
2. cluster, ascending
3. boundary delay, ascending
4. frequency, ascending
5. original position, ascending
```

Since earlier literals are attempted for deletion first:

```text
exp = 1 literals are tried before exp = 0 literals
cluster = 1 literals are tried before cluster = 2 literals
low-boundary-delay literals are tried before high-boundary-delay literals
low-frequency literals are tried before high-frequency literals
original order is the final tie-breaker
```

## Boundary Delay

The current boundary-delay rule is:

```text
boundary_delay = 0 for equality literals and ordinary literals
boundary_delay = 1 for inequality/boundary literals: <=, >=, <, >
```

Thus boundary literals are delayed relative to equalities when all earlier sort
keys tie. The name is intentionally literal: a larger value means the literal is
tried later for deletion and is therefore more likely to survive
generalization.

## Frequency

The frequency table stores floating-point counts. After each newly generalized
clause is learned under `freq_sort`, all existing counts decay and the literals
in the new clause are incremented:

```text
all frequencies *= 0.99
each literal in the new clause += 1.0
```

During sorting:

```text
lower frequency  -> tried earlier for deletion
higher frequency -> tried later for deletion
```

## Practical Effect

The current `freq_sort` strategy can be read as:

```text
Try to delete complex arithmetic, ordinary, low-frequency, equality literals first.
Delay structural pairs, high-frequency literals, and boundary inequalities.
```

In IC3/PDR terms, delaying a literal makes it more likely to survive
generalization. Since learned clauses are disjunctions, preserving extra
literals can make a clause weaker, so the structural protection must remain
conservative.

## Current Benchmark Lessons

`microwave19` is sensitive to simple division boundary literals. Marking every
`div` literal as expensive changed the temporary inductive-generalization query
order for a clause like:

```text
(= (div steps_remaining 60) 0)
```

Even when the final generalized clause was unchanged, the extra temporary SMT
queries perturbed the next frontier model and led to a slower proof path. The
current rule therefore keeps simple single-div boundaries out of `exp = 1`.

`microwave29` is sensitive in the opposite direction. It can learn clauses whose
useful-looking arithmetic literal is actually a complex `let`/nested-`div`
expression over `steps_remaining`. Those clauses make later forward propagation
queries expensive. The current rule still marks these shapes as `exp = 1` and
removes their structural protection.
