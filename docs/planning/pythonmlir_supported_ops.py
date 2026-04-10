"""
Literal supported operations/examples for PythonMLIR--.

This file is not an implementation. It is a concrete list of source-language
forms that are intended to be accepted by the PythonMLIR-- validator.

Rules reflected here:
- only `int` type exists
- declarations must be annotated
- reassignment is allowed after declaration
- a variable's type cannot change after declaration
- control flow is aggressively limited
"""

# ============================================================
# 1. INTEGER DECLARATIONS
# ============================================================

x: int = 5
y: int = 10
z: int = 0

# More examples
a: int = 1
b: int = 2
counter: int = 0
total: int = 0


# ============================================================
# 2. REASSIGNMENT AFTER DECLARATION
# ============================================================

z = 20
x = y
counter = counter + 1
total = total + x


# ============================================================
# 3. ARITHMETIC EXPRESSIONS
# Supported operators: +, -, *, //
# ============================================================

sum1: int = x + y
diff1: int = x - y
prod1: int = x * y
quot1: int = y // x

nested1: int = (x + y) * 2
nested2: int = (y - x) // 1
nested3: int = x * (y + 3)

z = x + y
z = x - y
z = x * y
z = y // x


# ============================================================
# 4. COMPARISON EXPRESSIONS
# Supported operators: <, <=, >, >=, ==, !=
# Only intended for control-flow conditions.
# ============================================================

if x < y:
    z = x
else:
    z = y

if x <= y:
    z = 1
else:
    z = 0

if x > y:
    z = x
else:
    z = y

if x >= y:
    z = x
else:
    z = y

if x == y:
    z = 1
else:
    z = 0

if x != y:
    z = 1
else:
    z = 0


# ============================================================
# 5. IF / ELIF / ELSE
# Condition should be a single comparison.
# ============================================================

if x < y:
    z = x
elif x == y:
    z = 0
else:
    z = y


# ============================================================
# 6. FOR LOOPS
# Only these forms are supported:
#   range(stop)
#   range(start, stop)
# ============================================================

for i in range(5):
    total = total + i

for j in range(0, 10):
    total = total + j


# ============================================================
# 7. FUNCTIONS
# All parameters must be annotated as int.
# Return type must be int.
# ============================================================


def add(x: int, y: int) -> int:
    z: int = x + y
    return z


def inc(n: int) -> int:
    value: int = n + 1
    return value


def sum_to(n: int) -> int:
    total: int = 0
    for i in range(n):
        total = total + i
    return total


def max2(x: int, y: int) -> int:
    result: int = 0
    if x > y:
        result = x
    else:
        result = y
    return result


# ============================================================
# 8. RETURNS
# ============================================================


def give_five() -> int:
    value: int = 5
    return value


def choose(x: int, y: int) -> int:
    out: int = 0
    if x < y:
        out = x
    else:
        out = y
    return out


# ============================================================
# 9. MINIMAL VALID PROGRAM SHAPES
# ============================================================


def main() -> int:
    x: int = 5
    y: int = 10
    z: int = x + y
    z = 20
    return z


def loop_example(n: int) -> int:
    acc: int = 0
    for i in range(n):
        acc = acc + i
    return acc


def branch_example(x: int, y: int) -> int:
    out: int = 0
    if x < y:
        out = x + y
    elif x == y:
        out = x
    else:
        out = x - y
    return out


# ============================================================
# 10. SUPPORTED SOURCE FORMS SUMMARY
# ============================================================
#
# Variable declaration:
#   x: int = 5
#
# Reassignment:
#   x = 6
#   x = y
#   x = x + 1
#
# Arithmetic:
#   x + y
#   x - y
#   x * y
#   x // y
#
# Comparisons in conditions:
#   x < y
#   x <= y
#   x > y
#   x >= y
#   x == y
#   x != y
#
# Control flow:
#   if / elif / else
#   for i in range(stop)
#   for i in range(start, stop)
#
# Functions:
#   def f(x: int) -> int: ...
#
# Return:
#   return expr
