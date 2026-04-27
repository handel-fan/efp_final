{-# LANGUAGE OverloadedStrings #-}

module LowerMLIR
  ( lowerModule
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import qualified PMMAST as PMM

import MLIR.AST
  ( Attribute(..)
  , Binding(..)
  , Block(..)
  , Location(..)
  , NamedAttributes
  , Operation(..)
  , Region(..)
  , ResultTypes(..)
  , Signedness(..)
  , Type(..)
  , pattern ModuleOp
  , pattern FuncOp
  )

--------------------------------------------------------------------------------
-- What this file does
--------------------------------------------------------------------------------

-- This file converts your PythonMinusMinus AST into the AST used by mlir-hs.
--
-- Input:
--   PMM.Module a
--
-- Output:
--   MLIR.AST.Operation
--
-- The output Operation will usually be a builtin.module operation.
--
-- Important:
--   This file does NOT print MLIR text.
--   This file does NOT call the MLIR C API.
--   This file only builds Haskell values from MLIR.AST.
--
-- Later, mlir-hs can turn those MLIR.AST values into real MLIR.

--------------------------------------------------------------------------------
-- Beginner-friendly names
--------------------------------------------------------------------------------

-- In your PythonMinusMinus program, variables have names like:
--
--   x
--   y
--   result
--
-- We store those names as ordinary Haskell Strings.
type SourceVariableName = String

-- In MLIR, values are SSA values. They look like:
--
--   %v0
--   %v1
--   %arg0
--
-- In mlir-hs, the percent sign is not stored in the name.
-- So the Haskell name "v0" eventually prints like %v0.
type MLIRValueName = BS.ByteString

-- Error messages are plain Strings for now.
type ErrorMessage = String

--------------------------------------------------------------------------------
-- Lowering state
--------------------------------------------------------------------------------

-- While lowering a function body, we need to remember three things:
--
-- 1. What fresh MLIR name should we make next?
--
-- 2. Which MLIR value currently represents each source variable?
--
--    Example:
--      PythonMinusMinus variable z  --->  MLIR value v0
--
-- 3. Which MLIR operations have we emitted so far?
--
--    Example:
--      v0 = arith.addi x, y
--      func.return v0

-- Contains the state while lowering for reference to build
-- the rest
data LowerState = LowerState
  { nextFreshNumber :: Int
  , variableToMLIRValue :: Map.Map SourceVariableName MLIRValueName
  , emittedBindings :: [Binding]
  }

-- TODO: Delete and Change the location where he gets called
emptyLowerState :: LowerState
emptyLowerState = LowerState
  { nextFreshNumber = 0
  , variableToMLIRValue = Map.empty
  , emittedBindings = []
  }

--------------------------------------------------------------------------------
-- Main entry point
--------------------------------------------------------------------------------

-- This is the function Main.hs will eventually call.
--
-- It converts:
--
--   PMM.Module
--
-- into:
--
--   MLIR builtin.module operation
--
-- For now, this only allows function definitions at top level.

lowerModule :: PMM.Module a -> Either ErrorMessage Operation
lowerModule (PMM.Module statements) = do
  topLevelBindings <- lowerStatements statements

  let moduleBodyBlock = Block
        { blockName = "0"
        , blockArgs = []
        , blockBody = topLevelBindings
        }

  let mlirModuleOperation = ModuleOp moduleBodyBlock

  Right mlirModuleOperation

--------------------------------------------------------------------------------
-- Top-level statements
--------------------------------------------------------------------------------

-- A PythonMinusMinus module contains a list of statements.
-- At the top level, we currently only support functions.

lowerTopLevelStatements :: [PMM.Statement a] -> Either ErrorMessage [Binding]
lowerTopLevelStatements [] =
  Right []

lowerTopLevelStatements (statement : remainingStatements) = do
  firstBinding <- lowerTopLevelStatement statement
  remainingBindings <- lowerTopLevelStatements remainingStatements
  Right (firstBinding : remainingBindings)

lowerTopLevelStatement :: PMM.Statement a -> Either ErrorMessage Binding
lowerTopLevelStatement statement =
  case statement of
    PMM.FunDef{} -> do
      functionOperation <- lowerFunction statement

      -- Bind [] means:
      --
      --   This operation does not produce an SSA result name.
      --
      -- A function definition is not like:
      --
      --   %v0 = func.func ...
      --
      -- It is just:
      --
      --   func.func @foo(...) ...
      Right (Bind [] functionOperation)

    _ ->
      Left "Only function definitions are supported at the top level."

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- This lowers a PMM function definition into an MLIR func.func operation.
--
-- Example source idea:
--
--   def add(x: int, y: int) -> int:
--       z: int = x + y
--       return z
--
-- MLIR shape idea:
--
--   func.func @add(%x: i64, %y: i64) -> i64 {
--     %v0 = arith.addi %x, %y : i64
--     func.return %v0 : i64
--   }

lowerFunction :: PMM.Statement a -> Either ErrorMessage Operation
lowerFunction statement =
  case statement of
    PMM.FunDef
      { PMM.fun_name = functionName
      , PMM.fun_params = parameters
      , PMM.fun_return_type = returnType
      , PMM.fun_body = functionBody
      } -> do

        let mlirParameterTypes = lowerParameterTypes parameters
        let mlirReturnType = lowerType returnType
        let mlirFunctionType = FunctionType mlirParameterTypes [mlirReturnType]

        let parameterBindings = makeParameterBindings parameters
        let startingVariableMap = makeStartingVariableMap parameters

        let startingState = emptyLowerState
              { variableToMLIRValue = startingVariableMap
              }

        finalState <- lowerStatementList functionBody startingState

        let entryBlock = Block
              { blockName = "0"
              , blockArgs = parameterBindings
              , blockBody = emittedBindings finalState
              }

        let functionRegion = Region [entryBlock]

        let mlirFunctionOperation = FuncOp
              UnknownLocation
              (stringToMLIRName (getIdentName functionName))
              mlirFunctionType
              functionRegion

        Right mlirFunctionOperation

    _ ->
      Left "Internal error: lowerFunction was called on something that is not a function."

lowerParameterTypes :: [PMM.Parameter a] -> [Type]
lowerParameterTypes [] =
  []

lowerParameterTypes (parameter : remainingParameters) =
  lowerType (PMM.param_type parameter) : lowerParameterTypes remainingParameters

-- MLIR blocks can have arguments.
-- Function parameters become arguments of the function's entry block.
--
-- Example:
--
--   def add(x: int, y: int) -> int:
--
-- becomes a block with arguments:
--
--   x : i64
--   y : i64

makeParameterBindings :: [PMM.Parameter a] -> [(MLIRValueName, Type)]
makeParameterBindings [] =
  []

makeParameterBindings (parameter : remainingParameters) =
  let sourceName = getIdentName (PMM.param_name parameter)
      mlirName = stringToMLIRName sourceName
      mlirType = lowerType (PMM.param_type parameter)
   in (mlirName, mlirType) : makeParameterBindings remainingParameters

-- At the start of a function, each source parameter is represented by
-- the corresponding MLIR block argument.
--
-- Example:
--
--   x maps to x
--   y maps to y

makeStartingVariableMap :: [PMM.Parameter a] -> Map.Map SourceVariableName MLIRValueName
makeStartingVariableMap [] =
  Map.empty

makeStartingVariableMap (parameter : remainingParameters) =
  let sourceName = getIdentName (PMM.param_name parameter)
      mlirName = stringToMLIRName sourceName
      restOfMap = makeStartingVariableMap remainingParameters
   in Map.insert sourceName mlirName restOfMap

--------------------------------------------------------------------------------
-- Statements inside function bodies
--------------------------------------------------------------------------------

-- This lowers a list of statements one by one.
--
-- We avoid foldl/foldM here on purpose because explicit recursion is easier
-- to understand when learning Haskell.

lowerStatementList :: [PMM.Statement a] -> LowerState -> Either ErrorMessage LowerState
lowerStatementList [] currentState =
  Right currentState

lowerStatementList (statement : remainingStatements) currentState = do
  stateAfterFirstStatement <- lowerStatement statement currentState
  lowerStatementList remainingStatements stateAfterFirstStatement

lowerStatement :: PMM.Statement a -> LowerState -> Either ErrorMessage LowerState
lowerStatement statement currentState =
  case statement of
    PMM.AnnAssign
      { PMM.ann_assign_name = variableName
      , PMM.ann_assign_type = annotatedType
      , PMM.ann_assign_expr = expression
      } ->
        lowerAnnotatedAssignment variableName annotatedType expression currentState

    PMM.Assign
      { PMM.assign_name = variableName
      , PMM.assign_expr = expression
      } ->
        lowerAssignment variableName expression currentState

    PMM.Return
      { PMM.return_expr = expression
      } ->
        lowerReturn expression currentState

    PMM.ExprStmt{} ->
      Left "Expression statements are not supported yet."

    PMM.If{} ->
      Left "If statements are not supported yet. Later, lower them with cf.cond_br and multiple blocks."

    PMM.For{} ->
      Left "For loops are not supported yet. Later, lower them with loop/header/body/after blocks."

    PMM.FunDef{} ->
      Left "Nested functions are not supported."

lowerAnnotatedAssignment :: PMM.Ident a -> PMM.Type -> PMM.Expr a -> LowerState -> Either ErrorMessage LowerState
lowerAnnotatedAssignment variableName annotatedType expression currentState = do
  let expectedMLIRType = lowerType annotatedType

  expressionResult <- lowerExpression expression currentState

  let actualMLIRType = loweredValueType expressionResult
  let mlirValueName = loweredValueName expressionResult
  let stateAfterExpression = loweredValueState expressionResult

  if actualMLIRType == expectedMLIRType
    then do
      let sourceName = getIdentName variableName
      let newState = rememberVariable sourceName mlirValueName stateAfterExpression
      Right newState
    else
      Left "Annotated assignment type mismatch."

lowerAssignment :: PMM.Ident a -> PMM.Expr a -> LowerState -> Either ErrorMessage LowerState
lowerAssignment variableName expression currentState = do
  expressionResult <- lowerExpression expression currentState

  let mlirValueName = loweredValueName expressionResult
  let stateAfterExpression = loweredValueState expressionResult
  let sourceName = getIdentName variableName

  let newState = rememberVariable sourceName mlirValueName stateAfterExpression

  Right newState

lowerReturn :: PMM.Expr a -> LowerState -> Either ErrorMessage LowerState
lowerReturn expression currentState = do
  expressionResult <- lowerExpression expression currentState

  let mlirValueName = loweredValueName expressionResult
  let stateAfterExpression = loweredValueState expressionResult

  let returnOperation = makeFuncReturnOperation [mlirValueName]
  let returnBinding = Bind [] returnOperation
  let newState = emitBinding returnBinding stateAfterExpression

  Right newState

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

-- When we lower an expression, we get three things:
--
-- 1. The MLIR value name that now holds the expression result.
-- 2. The MLIR type of that value.
-- 3. The updated LowerState, because lowering the expression may emit ops.
--
-- Example:
--
--   x + y
--
-- might emit:
--
--   %v0 = arith.addi %x, %y : i64
--
-- and then return:
--
--   name  = v0
--   type  = i64
--   state = state with the new operation added

data LoweredValue = LoweredValue
  { loweredValueName :: MLIRValueName
  , loweredValueType :: Type
  , loweredValueState :: LowerState
  }

lowerExpression :: PMM.Expr a -> LowerState -> Either ErrorMessage LoweredValue
lowerExpression expression currentState =
  case expression of
    PMM.Var { PMM.var_ident = variableName } ->
      lowerVariable variableName currentState

    PMM.IntLit { PMM.int_value = integerValue } ->
      lowerIntegerLiteral integerValue currentState

    PMM.Paren { PMM.paren_expr = innerExpression } ->
      lowerExpression innerExpression currentState

    PMM.BinaryOp
      { PMM.operator = arithmeticOperator
      , PMM.left_op_arg = leftExpression
      , PMM.right_op_arg = rightExpression
      } ->
        lowerBinaryOperation arithmeticOperator leftExpression rightExpression currentState

lowerVariable :: PMM.Ident a -> LowerState -> Either ErrorMessage LoweredValue
lowerVariable variableName currentState =
  let sourceName = getIdentName variableName
      currentVariableMap = variableToMLIRValue currentState
   in case Map.lookup sourceName currentVariableMap of
        Just mlirValueName ->
          Right LoweredValue
            { loweredValueName = mlirValueName
            , loweredValueType = intType
            , loweredValueState = currentState
            }

        Nothing ->
          Left ("Unbound variable: " ++ sourceName)

lowerIntegerLiteral :: Integer -> LowerState -> Either ErrorMessage LoweredValue
lowerIntegerLiteral integerValue currentState =
  let (freshMLIRName, stateAfterFreshName) = makeFreshMLIRValueName currentState
      constantOperation = makeArithConstantOperation integerValue intType
      constantBinding = Bind [freshMLIRName] constantOperation
      stateAfterEmit = emitBinding constantBinding stateAfterFreshName
   in Right LoweredValue
        { loweredValueName = freshMLIRName
        , loweredValueType = intType
        , loweredValueState = stateAfterEmit
        }

lowerBinaryOperation :: PMM.ArithOp -> PMM.Expr a -> PMM.Expr a -> LowerState -> Either ErrorMessage LoweredValue
lowerBinaryOperation arithmeticOperator leftExpression rightExpression currentState = do
  leftResult <- lowerExpression leftExpression currentState

  let leftName = loweredValueName leftResult
  let leftType = loweredValueType leftResult
  let stateAfterLeft = loweredValueState leftResult

  rightResult <- lowerExpression rightExpression stateAfterLeft

  let rightName = loweredValueName rightResult
  let rightType = loweredValueType rightResult
  let stateAfterRight = loweredValueState rightResult

  if leftType /= rightType
    then
      Left "Binary operation type mismatch."
    else do
      binaryOperation <- makeArithmeticOperation arithmeticOperator leftType leftName rightName

      let (resultName, stateAfterFreshName) = makeFreshMLIRValueName stateAfterRight
      let binaryBinding = Bind [resultName] binaryOperation
      let stateAfterEmit = emitBinding binaryBinding stateAfterFreshName

      Right LoweredValue
        { loweredValueName = resultName
        , loweredValueType = leftType
        , loweredValueState = stateAfterEmit
        }

--------------------------------------------------------------------------------
-- Type lowering
--------------------------------------------------------------------------------

-- PythonMinusMinus only has one type right now: int.
-- We lower PMM int to MLIR i64.

intType :: Type
intType = IntegerType Signless 64

lowerType :: PMM.Type -> Type
lowerType pmmType =
  case pmmType of
    PMM.TInt ->
      intType

--------------------------------------------------------------------------------
-- MLIR operation constructors
--------------------------------------------------------------------------------

-- These functions create raw MLIR.AST.Operation values.
--
-- The fields mean roughly:
--
--   opName        = MLIR operation name, like "arith.addi"
--   opResultTypes = result type list
--   opOperands    = SSA values used as inputs
--   opRegions     = nested regions, used by operations like func.func
--   opSuccessors  = control-flow block targets
--   opAttributes  = compile-time constants / metadata

makeArithConstantOperation :: Integer -> Type -> Operation
makeArithConstantOperation integerValue resultType = Operation
  { opName = "arith.constant"
  , opLocation = UnknownLocation
  , opResultTypes = Explicit [resultType]
  , opOperands = []
  , opRegions = []
  , opSuccessors = []
  , opAttributes = Map.fromList
      [ ("value", IntegerAttr resultType (fromInteger integerValue))
      ]
  }

makeArithmeticOperation :: PMM.ArithOp -> Type -> MLIRValueName -> MLIRValueName -> Either ErrorMessage Operation
makeArithmeticOperation arithmeticOperator resultType leftName rightName =
  case arithmeticOperator of
    PMM.Add ->
      Right (makeSimpleOperation "arith.addi" [resultType] [leftName, rightName])

    PMM.Sub ->
      Right (makeSimpleOperation "arith.subi" [resultType] [leftName, rightName])

    PMM.Mul ->
      Right (makeSimpleOperation "arith.muli" [resultType] [leftName, rightName])

    PMM.FloorDiv ->
      Left "Floor division is not supported yet. Need to choose signed or unsigned division semantics first."

makeFuncReturnOperation :: [MLIRValueName] -> Operation
makeFuncReturnOperation operandNames = Operation
  { opName = "func.return"
  , opLocation = UnknownLocation
  , opResultTypes = Explicit []
  , opOperands = operandNames
  , opRegions = []
  , opSuccessors = []
  , opAttributes = emptyAttributes
  }

makeSimpleOperation :: BS.ByteString -> [Type] -> [MLIRValueName] -> Operation
makeSimpleOperation operationName resultTypes operandNames = Operation
  { opName = operationName
  , opLocation = UnknownLocation
  , opResultTypes = Explicit resultTypes
  , opOperands = operandNames
  , opRegions = []
  , opSuccessors = []
  , opAttributes = emptyAttributes
  }

emptyAttributes :: NamedAttributes
emptyAttributes = Map.empty

--------------------------------------------------------------------------------
-- State helper functions
--------------------------------------------------------------------------------

rememberVariable :: SourceVariableName -> MLIRValueName -> LowerState -> LowerState
rememberVariable sourceName mlirValueName currentState =
  let oldMap = variableToMLIRValue currentState
      newMap = Map.insert sourceName mlirValueName oldMap
   in currentState { variableToMLIRValue = newMap }

emitBinding :: Binding -> LowerState -> LowerState
emitBinding newBinding currentState =
  let oldBindings = emittedBindings currentState
      newBindings = oldBindings ++ [newBinding]
   in currentState { emittedBindings = newBindings }

makeFreshMLIRValueName :: LowerState -> (MLIRValueName, LowerState)
makeFreshMLIRValueName currentState =
  let currentNumber = nextFreshNumber currentState
      nextNumber = currentNumber + 1
      freshNameAsString = "v" ++ show currentNumber
      freshNameAsMLIRName = stringToMLIRName freshNameAsString
      newState = currentState { nextFreshNumber = nextNumber }
   in (freshNameAsMLIRName, newState)

--------------------------------------------------------------------------------
-- Small helper functions
--------------------------------------------------------------------------------

getIdentName :: PMM.Ident a -> String
getIdentName ident =
  PMM.ident_string ident

stringToMLIRName :: String -> MLIRValueName
stringToMLIRName string =
  BS.pack string

  
