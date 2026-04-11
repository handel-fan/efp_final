module ConvertAST (
    convertModule
  , convertStmt
  , convertParam
  , convertExpr
  , convertCondition
  , convertRange
  , convertTypeExpr
  ) where

import qualified Language.Python.Common.AST as Py
import qualified PMMAST as PMM

convertModule :: Py.Module a -> Either String (PMM.Module a)
convertModule (Py.Module stmts) = PMM.Module <$> mapM convertStmt stmts

convertStmt :: Py.Statement a -> Either String (PMM.Statement a)
convertStmt stmt =
  case stmt of
    Py.Fun
      { Py.fun_name = name
      , Py.fun_args = params
      , Py.fun_result_annotation = Just retTy
      , Py.fun_body = body
      , Py.stmt_annot = ann
      } -> do
          params' <- mapM convertParam params
          retTy' <- convertTypeExpr retTy
          body' <- mapM convertStmt body
          pure PMM.FunDef
            { PMM.fun_name = convertIdent name
            , PMM.fun_params = params'
            , PMM.fun_return_type = retTy'
            , PMM.fun_body = body'
            , PMM.stmt_annot = ann
            }

    Py.Fun { Py.fun_result_annotation = Nothing } ->
      Left "function is missing a return type annotation"

    Py.AnnotatedAssign
      { Py.ann_assign_annotation = tyExpr
      , Py.ann_assign_to = target
      , Py.ann_assign_expr = Just expr
      , Py.stmt_annot = ann
      } -> do
          name <- expectVarIdent "annotated assignment target must be a variable" target
          ty <- convertTypeExpr tyExpr
          expr' <- convertExpr expr
          pure PMM.AnnAssign
            { PMM.ann_assign_name = name
            , PMM.ann_assign_type = ty
            , PMM.ann_assign_expr = expr'
            , PMM.stmt_annot = ann
            }

    Py.AnnotatedAssign { Py.ann_assign_expr = Nothing } ->
      Left "annotated assignment must have an initializer"

    Py.Assign
      { Py.assign_to = [target]
      , Py.assign_expr = expr
      , Py.stmt_annot = ann
      } -> do
          name <- expectVarIdent "assignment target must be a variable" target
          expr' <- convertExpr expr
          pure PMM.Assign
            { PMM.assign_name = name
            , PMM.assign_expr = expr'
            , PMM.stmt_annot = ann
            }

    Py.Assign {} ->
      Left "assignment must have exactly one target"

    Py.Conditional
      { Py.cond_guards = guards
      , Py.cond_else = elseBody
      , Py.stmt_annot = ann
      } -> do
          guards' <- mapM convertGuard guards
          elseBody' <- mapM convertStmt elseBody
          pure PMM.If
            { PMM.if_guards = guards'
            , PMM.if_else = elseBody'
            , PMM.stmt_annot = ann
            }

    Py.For
      { Py.for_targets = [target]
      , Py.for_generator = gen
      , Py.for_body = body
      , Py.for_else = []
      , Py.stmt_annot = ann
      } -> do
          target' <- expectVarIdent "for-loop target must be a variable" target
          range' <- convertRange gen
          body' <- mapM convertStmt body
          pure PMM.For
            { PMM.for_target = target'
            , PMM.for_range = range'
            , PMM.for_body = body'
            , PMM.stmt_annot = ann
            }

    Py.For { Py.for_else = _ : _ } ->
      Left "for-else is not supported in PythonMinusMinus"

    Py.For {} ->
      Left "for-loop must have exactly one variable target"

    Py.Return
      { Py.return_expr = Just expr
      , Py.stmt_annot = ann
      } -> do
          expr' <- convertExpr expr
          pure PMM.Return
            { PMM.return_expr = expr'
            , PMM.stmt_annot = ann
            }

    Py.Return { Py.return_expr = Nothing } ->
      Left "return must have an expression"

    Py.StmtExpr
      { Py.stmt_expr = expr
      , Py.stmt_annot = ann
      } -> do
          expr' <- convertExpr expr
          pure PMM.ExprStmt
            { PMM.stmt_expr = expr'
            , PMM.stmt_annot = ann
            }

    _ -> Left "unsupported statement in PythonMLIR--"

convertGuard :: (Py.Expr a, Py.Suite a) -> Either String (PMM.Condition a, PMM.Suite a)
convertGuard (cond, body) = do
  cond' <- convertCondition cond
  body' <- mapM convertStmt body
  pure (cond', body')

convertParam :: Py.Parameter a -> Either String (PMM.Parameter a)
convertParam param =
  case param of
    Py.Param
      { Py.param_name = name
      , Py.param_py_annotation = Just tyExpr
      , Py.param_default = Nothing
      , Py.param_annot = ann
      } -> do
          ty <- convertTypeExpr tyExpr
          pure PMM.Parameter
            { PMM.param_name = convertIdent name
            , PMM.param_type = ty
            , PMM.param_annot = ann
            }

    Py.Param { Py.param_py_annotation = Nothing } ->
      Left "parameter is missing a type annotation"

    Py.Param { Py.param_default = Just _ } ->
      Left "default parameter values are not supported in PythonMinusMinus"

    _ -> Left "unsupported parameter form in PythonMinusMinus"

convertExpr :: Py.Expr a -> Either String (PMM.Expr a)
convertExpr expr =
  case expr of
    Py.Var { Py.var_ident = ident, Py.expr_annot = ann } ->
      pure PMM.Var
        { PMM.var_ident = convertIdent ident
        , PMM.expr_annot = ann
        }

    Py.Int { Py.int_value = value, Py.expr_annot = ann } ->
      pure PMM.IntLit
        { PMM.int_value = value
        , PMM.expr_annot = ann
        }

    Py.Paren { Py.paren_expr = inner, Py.expr_annot = ann } -> do
      inner' <- convertExpr inner
      pure PMM.Paren
        { PMM.paren_expr = inner'
        , PMM.expr_annot = ann
        }

    Py.BinaryOp
      { Py.operator = op
      , Py.left_op_arg = lhs
      , Py.right_op_arg = rhs
      , Py.expr_annot = ann
      } -> do
          op' <- convertArithOp op
          lhs' <- convertExpr lhs
          rhs' <- convertExpr rhs
          pure PMM.BinaryOp
            { PMM.operator = op'
            , PMM.left_op_arg = lhs'
            , PMM.right_op_arg = rhs'
            , PMM.expr_annot = ann
            }

    _ -> Left "unsupported expression in PythonMLIR--"

convertCondition :: Py.Expr a -> Either String (PMM.Condition a)
convertCondition expr =
  case expr of
    Py.BinaryOp
      { Py.operator = op
      , Py.left_op_arg = lhs
      , Py.right_op_arg = rhs
      , Py.expr_annot = ann
      } -> do
          cmp <- convertCmpOp op
          lhs' <- convertExpr lhs
          rhs' <- convertExpr rhs
          pure PMM.Compare
            { PMM.cmp_op = cmp
            , PMM.cmp_left = lhs'
            , PMM.cmp_right = rhs'
            , PMM.cond_annot = ann
            }

    Py.Paren { Py.paren_expr = inner } ->
      convertCondition inner

    _ -> Left "condition must be a single comparison"

convertRange :: Py.Expr a -> Either String (PMM.ForRange a)
convertRange expr =
  case expr of
    Py.Call
      { Py.call_fun = Py.Var { Py.var_ident = ident }
      , Py.call_args = args
      , Py.expr_annot = ann
      }
      | Py.ident_string ident == "range" ->
          convertRangeArgs ann args

    _ -> Left "for-loop generator must be range(stop) or range(start, stop)"

-- TODO: Understand this better
convertRangeArgs :: a -> [Py.Argument a] -> Either String (PMM.ForRange a)
convertRangeArgs ann args =
  case args of
    [arg1] -> do
      stop <- convertPositionalArg arg1
      pure PMM.RangeStop
        { PMM.range_stop = stop
        , PMM.range_annot = ann
        }

    [arg1, arg2] -> do
      start <- convertPositionalArg arg1
      stop <- convertPositionalArg arg2
      pure PMM.RangeStartStop
        { PMM.range_start = start
        , PMM.range_stop = stop
        , PMM.range_annot = ann
        }

    _ -> Left "range must have one or two positional arguments"

convertPositionalArg :: Py.Argument a -> Either String (PMM.Expr a)
convertPositionalArg arg =
  case arg of
    Py.ArgExpr { Py.arg_expr = expr } -> convertExpr expr
    _ -> Left "only positional range arguments are supported"

convertArithOp :: Py.Op a -> Either String PMM.ArithOp
convertArithOp op =
  case op of
    Py.Plus {}        -> pure PMM.Add
    Py.Minus {}       -> pure PMM.Sub
    Py.Multiply {}    -> pure PMM.Mul
    Py.FloorDivide {} -> pure PMM.FloorDiv
    _                 -> Left "unsupported arithmetic operator in PythonMLIR--"

convertCmpOp :: Py.Op a -> Either String PMM.CmpOp
convertCmpOp op =
  case op of
    Py.LessThan {}          -> pure PMM.Lt
    Py.LessThanEquals {}    -> pure PMM.Lte
    Py.GreaterThan {}       -> pure PMM.Gt
    Py.GreaterThanEquals {} -> pure PMM.Gte
    Py.Equality {}          -> pure PMM.Eq
    Py.NotEquals {}         -> pure PMM.Neq
    _                       -> Left "unsupported comparison operator in PythonMLIR--"

convertTypeExpr :: Py.Expr a -> Either String PMM.Type
convertTypeExpr expr =
  case expr of
    Py.Var { Py.var_ident = ident }
      | Py.ident_string ident == "int" -> pure PMM.TInt
    _ -> Left "only the type int is supported in PythonMinusMinus"

expectVarIdent :: String -> Py.Expr a -> Either String (PMM.Ident a)
expectVarIdent err expr =
  case expr of
    Py.Var { Py.var_ident = ident } -> pure (convertIdent ident)
    _                               -> Left err

convertIdent :: Py.Ident a -> PMM.Ident a
convertIdent ident = PMM.Ident
  { PMM.ident_string = Py.ident_string ident
  , PMM.ident_annot = Py.ident_annot ident
  }
