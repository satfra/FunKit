# FunKit C++ library

A C++ engine for deriving functional equations in QFT: it takes functional
derivatives of a master equation (e.g. the Wetterich flow), expands the result
over a field-content truncation, and merges equivalent diagrams — the
counterpart of the Mathematica FunKit pipeline
`FTakeDerivatives // FTruncate // FSimplify`.

## Building & running

```sh
mkdir -p build && cd build && cmake .. && make      # needs CMake >= 3.20, C++20, OpenMP
ctest                                               # run the test suite
./funkit <input.{toml,json}>                        # run the pipeline on an input file
```

The executable runs `resolve_derivatives → truncate → simplify` on the parsed
equation and prints the result (or writes it to `outputFile`). The stages can
be toggled per input file; see `setup` below.

## Input files

Inputs are TOML or JSON (chosen by file extension); both map onto the same
structure. A file has up to four top-level entries:

| key           | required | meaning                                            |
| ------------- | -------- | -------------------------------------------------- |
| `equation`    | yes      | the starting expression, a sum of terms            |
| `derivatives` | no       | the derivative legs, declaring external symmetry   |
| `symmetries`  | no       | explicit external-leg symmetries (cycle notation)  |
| `setup`       | yes      | fields, truncation, object types, options          |

Working examples live next to this file (`ex.toml`, `yang-mills.toml`) and in
`tests/boilerplate/`.

### Index conventions

Every leg of every object carries an integer *superindex*:

- The **sign** encodes the index position: positive = upper, negative = lower.
  Zero is invalid.
- An index label that appears **twice** in a term is *closed* (summed over —
  an internal line); a label that appears **once** is *open* (an external
  leg). More than two occurrences is an error.
- The special field name `"AnyField"` is a placeholder for "any field in the
  theory" (e.g. the trace legs of the Wetterich equation). Truncation expands
  it over the allowed field content; `simplify` refuses equations that still
  contain it.

### `equation`

A sum of terms; each term is an array of object tables, read as an ordered
product. Two kinds of entries:

```toml
equation = [
  [                                                          # one term
  { prefactor = 0.5 },                                       # scalar coefficient (float)
  { type = "FDOp",       legs = [ [ "phi", 101 ] ] },        # derivative operator
  { type = "Propagator", legs = [ [ "AnyField", 1 ], [ "AnyField", 2 ] ] },
  { type = "Rdot",       legs = [ [ "AnyField", -1 ], [ "AnyField", -2 ] ] }
  ]
]
```

- `{ prefactor = <float> }` multiplies the term's coefficient (default 1).
  In TOML, write it as a float (`0.5`, `1.0`).
- `{ type = "<name>", legs = [ [field, index], ... ] }` is one indexed object.
  `type` is a built-in (`FDOp`, `Propagator`, `GammaN`, `FMinus`) or a name
  declared under `setup.ordered` / `setup.correlators`.

`FDOp` objects are functional derivatives ∂/∂field(index); prepending n of
them to the master term produces the n-point flow. Their index labels become
the external legs of the result.

### `derivatives`

```toml
derivatives = [ [ "phi", 101 ], [ "phi", 102 ] ]
```

Declares which derivatives the equation's `FDOp`s represent — one
`[field, label]` pair per derivative, labels matching the `FDOp` legs. This is
an *analytic statement* that the underlying functional is (graded-)symmetric
in these derivatives; it cannot be inferred from the equation itself, which is
why it is an explicit input.

`simplify` uses it to identify diagrams that differ only by a permutation of
identical external legs: identical **commuting** legs are treated as freely
interchangeable (handled structurally, so many identical legs cost nothing —
no S_n enumeration), and identical **Grassmann** legs are exchanged pairwise
with a factor −1 each.

### `symmetries`

```toml
[[symmetries]]
cycles = [ [ 101, 102 ] ]   # disjoint cycles over external-leg labels
factor = -1                 # ±1, default +1
```

Explicit external-leg symmetries in disjoint-cycle notation, for invariances
that do not come from derivative exchange. Each entry permutes open-leg labels
(`(a b c)` means a→b→c→a) and contributes its `factor` when it identifies two
terms. All labels of a cycle must be external legs carrying the same field.
Composable with `derivatives`.

### `setup`

```toml
[setup]
debug = 1                       # 0 = silent, higher = more progress output
outputFile = "out.m"            # write the result here (default: stdout)
output_format = ""              # "json" for structured output; empty = Mathematica-
                                # syntax text, unless outputFile ends in ".json"
in_deriv_trunc = true           # truncate already while taking derivatives
do_truncate = true              # run the truncation stage
do_simplify = true              # run the simplification stage
ordered = [ "Rdot" ]            # user object types with ordered legs (e.g. regulators)
correlators = [ ]               # user correlation-function types

[setup.unordered]               # pinned trailing legs per user type (optional)
Phidot = 1                      # Phidot's last leg (the "field" slot) is never reordered

  [[setup.cFields]]             # one table per commuting field (or pair)
  phi = [ ]                     # field name = list of internal index names

  [[setup.gFields]]             # one table per Grassmann PAIR: (antifield, field)
  psibar = [ "a" ]              # e.g. a Dirac index "a"
  psi = [ "a" ]

[setup.truncation]              # allowed field content per object type
Rdot = [ [ "phi", "phi" ], [ "psi", "psibar" ] ]
Propagator = [ [ "phi", "phi" ], [ "psi", "psibar" ] ]
GammaN = [
  [ "phi" ],
  [ "psi", "psibar" ],
  [ "phi", "phi" ],
  [ "psi", "psibar", "phi" ],
]
```

Field declarations: each `[[setup.cFields]]` / `[[setup.gFields]]` table holds
one field, or a conjugate pair written as two entries in **(antifield, field)
order** (e.g. `psibar` before `psi`, `cb` before `c`). The value of each entry
lists the field's internal indices (Lorentz, color, ...) — informational names
for printing.

Truncation: `[setup.truncation]` maps an object type to the list of field
contents it may carry. During truncation, `AnyField` legs are expanded over
these rules and objects with contents outside the rules drop the whole term.
A type with no listed rule for `Field` objects means "all fields allowed".

Unordered legs: `[setup.unordered]` gives, per user type, the number of
trailing legs that are pinned in place — canonical leg ordering only sorts the
legs before them. This is the counterpart of Mathematica's
`FSetUnorderedIndices` and is what makes objects like `Phidot` (the flowing
field expectation value, whose last leg is the "field" slot rather than a
derivative leg) work: derivatives prepend their new leg at the front, so the
pinned slot stays last through the entire pipeline. Truncation and diagram
matching are unaffected — rule matching is order-insensitive, and the
upper/lower index discipline already keeps the pinned (upper) slot from ever
being aligned with a (lower) derivative leg.

### JSON equivalent

The same structure, with tables as objects and `[[...]]` arrays-of-tables as
arrays of objects:

```json
{
  "equation": [
    [
      { "prefactor": 0.5 },
      { "type": "Propagator", "legs": [["AnyField", 1], ["AnyField", 2]] },
      { "type": "Rdot", "legs": [["AnyField", -1], ["AnyField", -2]] }
    ]
  ],
  "derivatives": [["phi", 101], ["phi", 102]],
  "setup": {
    "debug": 0,
    "cFields": [ { "phi": [] } ],
    "gFields": [ { "psibar": ["a"], "psi": ["a"] } ],
    "ordered": ["Rdot"],
    "truncation": {
      "Rdot": [["phi", "phi"]],
      "Propagator": [["phi", "phi"]],
      "GammaN": [["phi", "phi", "phi"], ["phi", "phi", "phi", "phi"]]
    }
  }
}
```

## JSON output

With `output_format = "json"` (or an `outputFile` ending in `.json`) the
result is written in a structured schema instead of Mathematica-syntax text.
The `equation` entry reuses the *input* term/object tables, so a result can be
inspected — or re-fed — with the same mental model:

```json
{
 "funkit_output_version": 1,
 "input_file": "scalar-flow.toml",
 "stages": {"derivatives": true, "truncate": true, "simplify": true},
 "equation": [
  [{"prefactor": 1},
   {"type": "Propagator", "legs": [["phi",103],["phi",104]]},
   {"type": "GammaN", "legs": [["phi",-105],["phi",-103],["phi",-101]]},
   "..."]
 ]
}
```

Coefficients are printed in the shortest form that round-trips the `double`
exactly. Untruncated runs may contain the engine-internal object types
`Field` (a bare field), `gamma` (a metric contraction) and `FMinus` /
`SymmFactor` (unresolved sign/symmetry factors on `AnyField` legs); all of
them are also accepted on input. Note that the *text* printer emits the head
`FEq[...]`, which corresponds to `FEx` on the Mathematica side.

This is the wire format of the FunKit Mathematica package's C++ backend
(`FSetBackendCpp[]`, module CoBra), which drives this engine via
JSON-in/JSON-out; `FExportToml`/`FExportCppInput` produce compatible input
files from Mathematica-side setups and expressions.

## A complete example

The two-point flow of a scalar theory (`tests/boilerplate/scalar-flow.toml`):
two derivatives of the Wetterich master equation ½ G^{ab} Ṙ_{ba}, with the
external-leg symmetry declared:

```toml
equation = [
  [
  { prefactor = 0.5 },
  { type = "FDOp", legs = [ [ "phi", 101 ] ] },
  { type = "FDOp", legs = [ [ "phi", 102 ] ] },
  { type = "Propagator", legs = [ [ "AnyField", 1 ], [ "AnyField", 2 ] ] },
  { type = "Rdot", legs = [ [ "AnyField", -1 ], [ "AnyField", -2 ] ] }
  ]
]

derivatives = [ [ "phi", 101 ], [ "phi", 102 ] ]

[setup]
ordered = [ "Rdot" ]

  [[setup.cFields]]
  phi = [ ]

[setup.truncation]
Rdot = [ [ "phi", "phi" ] ]
Propagator = [ [ "phi", "phi" ] ]
GammaN = [
  [ "phi" ],
  [ "phi", "phi" ],
  [ "phi", "phi", "phi" ],
  [ "phi", "phi", "phi", "phi" ],
]
```

Running `./funkit` on this yields the textbook result — the polarization
diagram at coefficient 1 and the tadpole at −1/2:

```
FEq[
  FTerm[1,   Propagator[...], GammaN[{phi,phi,phi},...], Propagator[...], GammaN[{phi,phi,phi},...], Propagator[...], Rdot[...]],
  FTerm[-0.5, Propagator[...], GammaN[{phi,phi,phi,phi},...], Propagator[...], Rdot[...]]
 ]
```

For a larger run, `ex.toml` computes the 8-point scalar flow (385 560 terms
after truncation, simplified to 21 within seconds), and `yang-mills.toml` the
4-gluon flow of pure Yang-Mills with ghosts.
