# final project
* Write my own AST of my sub language (compared to the haskell parser AST)
* Virtual keyword
* force type annotation

Generate an MD file for PythonMLIR--, a dialect of python that I'm going to be able to lower to mlir given the parser generator
you just looked at.
Here are the constraints:

* force type annotation
* Lower to mlir arith or func (Not relevant to MD but valuable context, Also advise here)
* Control flow - means we need basic booleans and evaluation of expressions right after "if/elif" keywords
and limit the scope aggresively here as to what we can parse
* Obviously arithmetic
* Limited to int types
* Type annotations are mandatory in this language.
* Basic for loops 

A note here: Everything needs to be aggressively scoped down. The idea of being to take the most basic use case of all of these.

Generate additionally a Python file with the supported ops

Stretch goals:
* Be able to perform operations on lists of numbers (vector ops) - requires more thinking 
* Reject unsupported ops
