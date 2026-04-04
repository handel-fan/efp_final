module Main where

import Language.Python.Common.AST
import Language.Python.Common.SrcLocation (SrcSpan)
import qualified Language.Python.Version3 as V3

main :: IO ()
main = do
  let file = "language-python-test/examples/foo.py"
  contents <- readFile file

  case V3.parseModule contents file of
    Left err -> print err
    Right (mod0, _comments) -> do
      let mod1 = renameModule mod0
      putStrLn "Original AST:"
      print mod0
      putStrLn "\nRenamed AST:"
      print mod1

renameModule :: ModuleSpan -> ModuleSpan
renameModule (Module stmts) =
  Module (map renameStmt stmts)

renameStmt :: StatementSpan -> StatementSpan
renameStmt stmt =
  case stmt of
    Assign tos rhs a ->
      Assign (map renameExpr tos) (renameExpr rhs) a

    AugmentedAssign to op rhs a ->
      AugmentedAssign (renameExpr to) op (renameExpr rhs) a

    AnnotatedAssign ann to rhs a ->
      AnnotatedAssign (renameExpr ann) (renameExpr to) (fmap renameExpr rhs) a

    StmtExpr e a ->
      StmtExpr (renameExpr e) a

    Return me a ->
      Return (fmap renameExpr me) a

    While cond body els a ->
      While (renameExpr cond) (map renameStmt body) (map renameStmt els) a

    For targets gen body els a ->
      For (map renameExpr targets) (renameExpr gen) (map renameStmt body) (map renameStmt els) a

    Conditional guards els a ->
      Conditional
        [ (renameExpr g, map renameStmt suite) | (g, suite) <- guards ]
        (map renameStmt els)
        a

    _ ->
      stmt

renameExpr :: ExprSpan -> ExprSpan
renameExpr expr =
  case expr of
    Var ident a ->
      Var (renameIdent ident) a

    Call f args a ->
      Call (renameExpr f) (map renameArg args) a

    Subscript e ix a ->
      Subscript (renameExpr e) (renameExpr ix) a

    SlicedExpr e sls a ->
      SlicedExpr (renameExpr e) (map renameSlice sls) a

    CondExpr t c f a ->
      CondExpr (renameExpr t) (renameExpr c) (renameExpr f) a

    BinaryOp op l r a ->
      BinaryOp op (renameExpr l) (renameExpr r) a

    UnaryOp op e a ->
      UnaryOp op (renameExpr e) a

    Dot e attr a ->
      Dot (renameExpr e) (renameIdent attr) a

    Lambda params body a ->
      Lambda (map renameParam params) (renameExpr body) a

    Tuple es a ->
      Tuple (map renameExpr es) a

    Yield marg a ->
      Yield (fmap renameYieldArg marg) a

    Generator comp a ->
      Generator (renameComp comp) a

    Await e a ->
      Await (renameExpr e) a

    ListComp comp a ->
      ListComp (renameComp comp) a

    List es a ->
      List (map renameExpr es) a

    Dictionary pairs a ->
      Dictionary (map renameDictPair pairs) a

    DictComp comp a ->
      DictComp (renameComp comp) a

    Set es a ->
      Set (map renameExpr es) a

    SetComp comp a ->
      SetComp (renameComp comp) a

    Starred e a ->
      Starred (renameExpr e) a

    Paren e a ->
      Paren (renameExpr e) a

    StringConversion e a ->
      StringConversion (renameExpr e) a

    Int{} -> expr
    LongInt{} -> expr
    Float{} -> expr
    Imaginary{} -> expr
    Bool{} -> expr
    None{} -> expr
    Ellipsis{} -> expr
    ByteStrings{} -> expr
    Strings{} -> expr
    UnicodeStrings{} -> expr

renameIdent :: IdentSpan -> IdentSpan
renameIdent ident
  | ident_string ident == "x" = ident { ident_string = "y" }
  | otherwise = ident

renameArg :: ArgumentSpan -> ArgumentSpan
renameArg arg =
  case arg of
    ArgExpr e a ->
      ArgExpr (renameExpr e) a
    ArgVarArgsPos e a ->
      ArgVarArgsPos (renameExpr e) a
    ArgVarArgsKeyword e a ->
      ArgVarArgsKeyword (renameExpr e) a
    ArgKeyword kw e a ->
      ArgKeyword (renameIdent kw) (renameExpr e) a

renameSlice :: SliceSpan -> SliceSpan
renameSlice s =
  case s of
    SliceProper lo hi stride a ->
      SliceProper
        (fmap renameExpr lo)
        (fmap renameExpr hi)
        (fmap (fmap renameExpr) stride)
        a
    SliceExpr e a ->
      SliceExpr (renameExpr e) a
    SliceEllipsis a ->
      SliceEllipsis a

renameParam :: ParameterSpan -> ParameterSpan
renameParam p =
  case p of
    Param nm ann def a ->
      Param (renameIdent nm) (fmap renameExpr ann) (fmap renameExpr def) a
    VarArgsPos nm ann a ->
      VarArgsPos (renameIdent nm) (fmap renameExpr ann) a
    VarArgsKeyword nm ann a ->
      VarArgsKeyword (renameIdent nm) (fmap renameExpr ann) a
    EndPositional a ->
      EndPositional a
    UnPackTuple tup def a ->
      UnPackTuple (renameParamTuple tup) (fmap renameExpr def) a

renameParamTuple :: ParamTupleSpan -> ParamTupleSpan
renameParamTuple pt =
  case pt of
    ParamTupleName nm a ->
      ParamTupleName (renameIdent nm) a
    ParamTuple xs a ->
      ParamTuple (map renameParamTuple xs) a

renameYieldArg :: YieldArgSpan -> YieldArgSpan
renameYieldArg ya =
  case ya of
    YieldFrom e a -> YieldFrom (renameExpr e) a
    YieldExpr e -> YieldExpr (renameExpr e)

renameDictPair :: DictKeyDatumListSpan -> DictKeyDatumListSpan
renameDictPair d =
  case d of
    DictMappingPair k v ->
      DictMappingPair (renameExpr k) (renameExpr v)
    DictUnpacking e ->
      DictUnpacking (renameExpr e)

renameComp :: ComprehensionSpan -> ComprehensionSpan
renameComp (Comprehension ce cf a) =
  Comprehension (renameCompExpr ce) (renameCompFor cf) a

renameCompExpr :: ComprehensionExprSpan -> ComprehensionExprSpan
renameCompExpr ce =
  case ce of
    ComprehensionExpr e -> ComprehensionExpr (renameExpr e)
    ComprehensionDict d -> ComprehensionDict (renameDictPair d)

renameCompFor :: CompForSpan -> CompForSpan
renameCompFor (CompFor isAsync exprs inExpr iter a) =
  CompFor
    isAsync
    (map renameExpr exprs)
    (renameExpr inExpr)
    (fmap renameCompIter iter)
    a

renameCompIter :: CompIterSpan -> CompIterSpan
renameCompIter ci =
  case ci of
    IterFor cf a -> IterFor (renameCompFor cf) a
    IterIf cif a -> IterIf (renameCompIf cif) a

renameCompIf :: CompIfSpan -> CompIfSpan
renameCompIf (CompIf e iter a) =
  CompIf (renameExpr e) (fmap renameCompIter iter) a
