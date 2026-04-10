{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module PythonMinusMinusAST (
    Annotated(..)
  , Module(..)
  , Suite
  , Ident(..)
  , Type(..)
  , Parameter(..)
  , Statement(..)
  , Expr(..)
  , ArithOp(..)
  , Condition(..)
  , CmpOp(..)
  , ForRange(..)
  ) where

import Data.Data (Data, Typeable)

-- | Convenient access to annotations in annotated types.
class Annotated t where
  annot :: t a -> a

-- | Identifier.
data Ident annot = Ident
  { ident_string :: !String
  , ident_annot  :: annot
  }
  deriving (Eq, Ord, Show, Typeable, Data, Functor, Foldable, Traversable)

instance Annotated Ident where
  annot = ident_annot

-- | PythonMinusMinus has exactly one type.
data Type
  = TInt
  deriving (Eq, Ord, Show, Typeable, Data)

-- | A module is a sequence of top-level statements.
data Module annot = Module [Statement annot]
  deriving (Eq, Ord, Show, Typeable, Data, Functor, Foldable, Traversable)

-- | A block of statements.
type Suite annot = [Statement annot]

-- | Function parameter.
--
-- Parameters must be explicitly annotated in PythonMinusMinus.
data Parameter annot = Parameter
  { param_name  :: Ident annot
  , param_type  :: Type
  , param_annot :: annot
  }
  deriving (Eq, Ord, Show, Typeable, Data, Functor, Foldable, Traversable)

instance Annotated Parameter where
  annot = param_annot

-- | Statements supported by PythonMinusMinus.
data Statement annot
  = FunDef
    { fun_name        :: Ident annot
    , fun_params      :: [Parameter annot]
    , fun_return_type :: Type
    , fun_body        :: Suite annot
    , stmt_annot      :: annot
    }
  | AnnAssign
    { ann_assign_name :: Ident annot
    , ann_assign_type :: Type
    , ann_assign_expr :: Expr annot
    , stmt_annot      :: annot
    }
  | Assign
    { assign_name :: Ident annot
    , assign_expr :: Expr annot
    , stmt_annot  :: annot
    }
  | If
    { if_guards   :: [(Condition annot, Suite annot)]
    , if_else     :: Suite annot
    , stmt_annot  :: annot
    }
  | For
    { for_target  :: Ident annot
    , for_range   :: ForRange annot
    , for_body    :: Suite annot
    , stmt_annot  :: annot
    }
  | Return
    { return_expr :: Expr annot
    , stmt_annot  :: annot
    }
  | ExprStmt
    { stmt_expr   :: Expr annot
    , stmt_annot  :: annot
    }
  deriving (Eq, Ord, Show, Typeable, Data, Functor, Foldable, Traversable)

instance Annotated Statement where
  annot = stmt_annot

-- | Arithmetic expressions.
--
-- Comparisons are intentionally *not* part of Expr because the language only
-- allows them in control-flow conditions.
data Expr annot
  = Var
    { var_ident  :: Ident annot
    , expr_annot :: annot
    }
  | IntLit
    { int_value  :: Integer
    , expr_annot :: annot
    }
  | BinaryOp
    { operator     :: ArithOp
    , left_op_arg  :: Expr annot
    , right_op_arg :: Expr annot
    , expr_annot   :: annot
    }
  | Paren
    { paren_expr :: Expr annot
    , expr_annot :: annot
    }
  deriving (Eq, Ord, Show, Typeable, Data, Functor, Foldable, Traversable)

instance Annotated Expr where
  annot = expr_annot

-- | Arithmetic operators supported by PythonMinusMinus.
data ArithOp
  = Add
  | Sub
  | Mul
  | FloorDiv
  deriving (Eq, Ord, Show, Typeable, Data)

-- | Conditions supported by PythonMinusMinus.
--
-- A condition must be a single comparison.
data Condition annot = Compare
  { cmp_op      :: CmpOp
  , cmp_left    :: Expr annot
  , cmp_right   :: Expr annot
  , cond_annot  :: annot
  }
  deriving (Eq, Ord, Show, Typeable, Data, Functor, Foldable, Traversable)

instance Annotated Condition where
  annot = cond_annot

-- | Comparison operators supported by PythonMinusMinus.
data CmpOp
  = Lt
  | Lte
  | Gt
  | Gte
  | Eq
  | Neq
  deriving (Eq, Ord, Show, Typeable, Data)

-- | The only supported loop generator is range(...).
data ForRange annot
  = RangeStop
    { range_stop  :: Expr annot
    , range_annot :: annot
    }
  | RangeStartStop
    { range_start :: Expr annot
    , range_stop  :: Expr annot
    , range_annot :: annot
    }
  deriving (Eq, Ord, Show, Typeable, Data, Functor, Foldable, Traversable)

instance Annotated ForRange where
  annot = range_annot
