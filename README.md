# Parsing Python with Haskell and Lowering to MLIR

This project parses a small subset of Python using Haskell and converts it into
a project-specific AST. The intended next step is lowering that AST into MLIR.

## Main Project

The main project is in `pmm/`.

Important files:

- `pmm/Main.hs`: entry point for parsing and conversion
- `pmm/ConvertAST.hs`: converts the `language-python` AST into the project AST
- `pmm/PMMAST.hs`: defines the smaller Python AST
- `pmm/LowerMLIR.hs`: partial, incomplete MLIR lowering work

## Planning Files

- `docs/planning/PythonMLIR--.md`: describes the supported Python subset
- `docs/planning/pythonmlir_supported_ops.py`: gives examples of supported operations

## Example Input

- `pmm/examples/int_declarations.py`

## Dependencies

The project expects `language-python` and `mlir-hs` to be available at the
repository root, as sibling directories of `pmm/`.

```text
repo/
  pmm/
  language-python/
  mlir-hs/
```

`language-python` is used for parsing Python source code.

`mlir-hs` was intended for MLIR AST construction and emission. Not much of that has been finished.

## Build and Run

From the `pmm/` directory:

```bash
./build_pmm.bash
./run_pmm.bash
```

The executable parses the example Python file and prints the converted project
AST. MLIR emission is not fully implemented.
