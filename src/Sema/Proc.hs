{-# LANGUAGE RecordWildCards, OverloadedStrings  #-}
module Sema.Proc where
import Common.Flatten
import Codegen.Expr

-- Proc semantics (monadic)
bindSemantics :: CExpr -> CExpr
bindSemantics CExprLambda { _lbody = CExprCall { _fname = "Coq_ret", _fparams = [a] }, .. } =
    CExprLambda _largs a
bindSemantics CExprCall { _fname = "Coq_bind", _fparams = [arg] }   =
    error "Datatypes.Coq_bind with one arg found, undefined behavior."
bindSemantics CExprCall { _fname = "Coq_bind", _fparams = [call, CExprLambda { _largs = [varname], .. }] } =
    CExprSeq statement $ bindSemantics _lbody
    where statement = CExprStmt CTAuto varname $ bindSemantics call
bindSemantics CExprCall { _fname = "Coq_bind", _fparams = [call, CExprLambda { _largs = args, .. }] } =
    error "Bind followed by a non-unary lambda, undefined behavior"
bindSemantics CExprCall { _fname = "Coq_bind", _fparams = a:b:arg } =
    error "Datatypes.Coq_bind with more than two args found!"
bindSemantics other                                                 =
    descend bindSemantics other

procSemantics :: CExpr -> CExpr
procSemantics = bindSemantics
