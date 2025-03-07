module Sema.Pipeline where

import CIR.Expr
import Sema.Bool
import Sema.IO
import Sema.Nat
import Sema.Prod
import Sema.String

semantics :: CExpr -> CExpr
semantics =
  ioSemantics
    . prodSemantics
    . stringSemantics
    . asciiSemantics
    . natSemantics
    . boolSemantics
