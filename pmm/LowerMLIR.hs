{-# LANGUAGE OverloadedStrings #-}

module LowerMLIR25
  ( lowerModule
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified PMMAST as PMM
import qualified MLIR.AST as MLIR


data LowerState = LowerState
  { nextFreshNumber :: Int
  , variableToMLIRValue :: Map.Map SourceVariableName MLIRValueName
  , emmittedBindings :: [Binding]
  }

emptyLowerState :: LowerState


-- Main entry Point

lowerModule :: PMM.Module a -> Either ErrorMessage MLIR.Operation
lowerModule (PMM.Module statements) = do
  topLevelBindings <- lowerTopLevelStatements statements

  let moduleBodyBlock = MLIR.Block
        { MLIR.blockName = "0"
        , MLIR.blockArgs = []
        , MLIR.blockBody = topLevelBindings
        }

  Right MLIR.Operation
    { MLIR.opName = "builtin.module"
    , MLIR.opLocation = MLIR.UnknownLocation
    , MLIR.opResultTypes = MLIR.Explicit []
    , MLIR.opOperands = []
    , MLIR.opRegions = [MLIR.Region [moduleBodyBlock]]
    , MLIR.opSuccessors = []
    , MLIR.opAttributes = Map.empty
    }

