# PythonMLIR-- Specification

## Goal

PythonMLIR-- is a deliberately tiny, statically-typed dialect of Python designed for straightforward lowering into MLIR.

It is built on top of an existing Python 3 parser but **aggressively restricts** the language to a minimal, predictable subset.

---

## Feasibility

This design is **fully supported** by the parser grammar:

- function annotations (`->`)
- parameter annotations (`x: int`)
- variable annotations (`x: int = ...`)

So:
> Mandatory type annotations are feasible, including for variables.

---

## Core Design Principles

- Single primitive type: `int`
- Mandatory type annotations for declarations
- Reassignment allowed, but **type cannot change after declaration**
- Minimal control flow
- Simple lowering to MLIR

---

## Target MLIR Dialects

PythonMLIR-- lowers to:

- `func` → functions and returns
- `arith` → integer operations and comparisons
- `scf` → structured control flow (`if`, `for`)

---

## Type System

### Allowed Types

- `int` (only)

### Rules

- Every variable must be declared with a type annotation
- After declaration, variables may be reassigned
- **Type is immutable after declaration**
- All expressions must evaluate to `int`

### Examples

```python
x: int = 5
x = x + 1
```

Invalid:

```python
x: int = 5
x = "hello"  # invalid
```

---

## Functions

### Syntax

```python
def add(x: int, y: int) -> int:
    z: int = x + y
    return z
```

### Constraints

- All parameters must be annotated as `int`
- Return type must be `int`
- No defaults, decorators, async, or varargs

### MLIR

- `func.func`
- `func.return`

---

## Variables

### Declaration

```python
x: int = 10
```

### Reassignment

```python
x = x + 1
```

### Constraints

- Declaration must include annotation
- Reassignment allowed only after declaration
- No chained assignments
- No augmented assignments (`+=`)

---

## Arithmetic

Supported operators:

- `+`
- `-`
- `*`
- `//`

### MLIR Mapping

| Python | MLIR |
|--------|------|
| `+`    | `arith.addi` |
| `-`    | `arith.subi` |
| `*`    | `arith.muli` |
| `//`   | `arith.divsi` |

---

## Comparisons

Supported:

- `<`, `<=`, `>`, `>=`, `==`, `!=`

### Rules

- Only allowed in control-flow conditions
- No boolean variables
- No `and`, `or`, `not`

### MLIR

- `arith.cmpi`

---

## If / Elif / Else

### Syntax

```python
if x < y:
    z: int = x
elif x == y:
    z: int = 0
else:
    z: int = y
```

### Constraints

- Condition must be a single comparison
- No boolean chaining

### MLIR

- `scf.if`

---

## For Loops

### Syntax

```python
for i in range(n):
    x = i + 1
```

or

```python
for i in range(0, n):
    x = i + 1
```

### Constraints

- Only `range(...)` allowed
- Forms:
  - `range(stop)`
  - `range(start, stop)`
- No step argument
- No `break`, `continue`, or `else`

### MLIR

- `scf.for`

---

## Return

```python
return x
```

- Must return `int`
- All paths must return

---

## Expressions

Allowed:

- integer literals
- variable references
- arithmetic expressions
- comparisons (in conditions)
- parentheses

Not allowed:

- strings, floats
- lists, dicts, sets
- comprehensions
- lambdas
- attribute access
- subscripting
- boolean operators

---

## Minimal Example

```python
def sum_to(n: int) -> int:
    total: int = 0
    for i in range(n):
        total = total + i
    return total
```

---

## Compilation Pipeline

1. Parse using Python parser
2. Validate against PythonMLIR-- subset
3. Build typed IR
4. Lower to MLIR (`func`, `arith`, `scf`)

---

## Summary

### Supported

- Functions (`int -> int`)
- Annotated declarations
- Reassignment (type-safe)
- Arithmetic
- Comparisons
- `if/elif/else`
- `for range(...)`

### Unsupported

- Any non-int types
- Complex expressions
- Python dynamic features
- Exceptions, imports, async, decorators

---

## Final Design Rule

> Declare once with a type. Reassign freely, but never change type.
