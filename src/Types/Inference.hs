{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Types.Inference where
import Types.Flatten
import CIR.Expr
import CIR.Decl
import Codegen.Rewrite
import Common.Utils
import Data.List (nub)
import Data.Text (Text)
import Data.MonoTraversable
import Data.Map.Strict (Map)
import qualified Data.Map      as M
import qualified Data.Text     as T
import qualified Common.Config as Conf
import Debug.Trace

-- Named context
type Context a = Map Text [a]

-- Print key values correctly
printCtx :: Show a => Context a -> IO ()
printCtx = putStr . concatMap (++"\n") . M.elems . M.mapWithKey (\k v -> show k ++ " : " ++ show v)

-- Merge two contexts, throw an error if conflicting definitions happen (no overloading)
mergeCtx :: Context CType -> Context CType -> Context CType
mergeCtx = M.unionWithKey (\k va vb ->
    error $ "Constructor " ++ show k ++ " has conflicting definitions " ++ show va  ++ "\n" ++ show vb)

-- Append a Decl in a context
addCtx :: Context CType -> CDecl -> Context CType
addCtx ctx CDFunc { .. } = M.insert _fn (getFuncT _ftype) ctx
addCtx ctx CDType { .. } = M.insert _tname [_tval] ctx
addCtx ctx CDInd  { .. } = let midctx = M.insert _iname [_itype] ctx in
                             mergeCtx (M.fromList _ictors) midctx

-- Unify two types
unify :: CType -> CType -> CType
-- Any type is better than undefined type
unify a b | a == b = b
unify t CTUndef {} = t
unify CTUndef {} t = t
unify CTFunc { _fret = a, _fins = ina} CTFunc { _fret = b, _fins = inb}
    | length ina == length inb = CTFunc (unify a b) $ zipWith unify ina inb
    | otherwise = error $ "Attempting to unify func types with different args" ++ show ina ++ " " ++ show inb
-- Ignore Proc monad wrapped types
unify CTExpr { _tbase = "proc" , _tins = [a] } t = unify a t
unify t CTExpr { _tbase = "proc" , _tins = [a] } = unify t a
unify CTExpr { _tbase = a , _tins = ina } CTExpr { _tbase = b , _tins = inb }
    | a == b && length ina == length inb = CTExpr a $ zipWith unify ina inb
    | otherwise = error $ "Attempting to unify list types with different args" ++ show ina ++ " " ++ show inb
unify CTBase { _base = a } CTBase { _base = b }
    | a == b  = CTBase a
    | otherwise = error $ "Cannot unify different base types " ++ show a ++ " " ++ show b
unify CTFree { _idx = a } CTFree { _idx = b }
    | a == b = CTFree a
    | otherwise = error $ "Cannot unify different indices " ++ show a ++ " " ++ show b
-- Unify free parameters, here we're assuming Coq has already type-checked this
unify CTFree { .. } t = t
unify t CTFree { .. } = t
unify a b = error $ "Unsure how to unify " ++ show a ++ " " ++ show b

-- TODO: Maximal insertion
maxinsert :: Context CType -> CExpr -> CExpr
maxinsert t = id

-- Maximally plug a type into an expression, given a type context (Gamma)
plugInExpr :: Context CType -> CType -> CExpr -> CExpr
plugInExpr ctx t CExprCall { .. }
    -- Return preserves the type
    | _fname == "return" = CExprCall "return" $ map (plugInExpr ctx t) _fparams
    -- A match preserves the type if the lambdas return it (omit matched object)
    | _fname == "match"  = CExprCall "match" $ head _fparams:map (plugInExpr ctx t) (tail _fparams)
    -- | _fname == "app"    = CExprCall "app" $ map (plugInExpr ctx t) _fparams
    | _fname `M.member` ctx =
        let params = zipWith (\tp exp -> plugInExpr ctx tp exp) (ctx M.! _fname) _fparams in
        CExprCall _fname params
    -- Function call obfuscate the return type, ignore them
    | otherwise          = CExprCall _fname _fparams
-- Or explicit if it comes from the first rule handling return calls
plugInExpr _ CTExpr { _tbase = "list" , _tins = [t] } CExprList { .. } =
    CExprList unified _elems
    where unified = unify t _etype
plugInExpr _ CTExpr { _tbase = "option" , _tins = [t] } CExprOption { .. } =
    CExprOption unified _val
    where unified = unify t _otype
plugInExpr ctx t s@CExprSeq { .. } = listToSeq $ first ++ [retexpr]
    where retexpr = plugInExpr ctx t . last . seqToList $ s
          first   = init . seqToList $ s
plugInExpr ctx t o = omap (plugInExpr ctx t) o

