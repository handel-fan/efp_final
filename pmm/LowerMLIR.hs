module LowerMLIR (lowerModule) where
import qualified PMMAST as PMM
import MLIR.AST
import MLIR.AST.Builder

lowerModule :: PMM.Module a -> Operation
lowerModule (Module stmts) = 
