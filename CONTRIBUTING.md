# How to contribute

## Important rules

### Import hierarchy

Plutarch has several module groups, which must maintain a specific import
hierarchy. More specifically, modules in certain groups may only import modules
in other groups. The groups are as follows:

- `Plutarch.Backend`
- `Plutarch.Primitive`
- `Plutarch.Helpers`

The rules are as follows:

- Modules in any group may import other modules in the same group. Thus, for
  example, a module in `Plutarch.Backend` may import other modules also in
  `Plutarch.Backend`.
- A module `X` in any group may import modules in the `Plutarch.Helpers` group,
  providing those modules import only those modules that `X` could import.
- `Plutarch.Primitive` modules may import `Plutarch.Backend` modules.
- `Plutarch.Helpers` modules may import `Plutarch.Backend` modules.
- `Plutarch.Helpers` modules may import `Plutarch.Primitive` modules.

### Naming conventions

Plutarch types should have names beginning with `P`, as should Plutarch-specific
type classes. Plutarch identifiers, unless they are not visible at the module
level, should also begin with `p`. Identifiers specifically should keep the word
immediately following `p` lowercase.

Some examples follow:

- `punsafeCoerce` (correct identifier)
- `PInteger` (correct type)
- `PEq` (correct type class)
- `pIsEqual` (wrong identifier, correct would be `pisEqual`)

Any functions whose use requires invariants which the type system does not
guarantee, and which could cause 'impossible' situations or invariant violations
that would impede normal code generation, must have names starting with
`punsafe`. For example:

- `punsafeCoerce` (as it can 're-tag' a `Term` with any type, even one that
  makes no sense)
- `punsafeCase` (as it allows case-matching on any `Term`, whether this is
  sensible or not)
- `punsafeSpecialize` (as it allows treating a more general type as a more
  specialized one, whether this is sensible for a given `Term` or not)

### Helper modules

If a piece of functionality is used across multiple modules, but does not come
from a dependency, it should be put into a module in `Plutarch.Helpers`. If no
existing module in that group makes sense, create another.
