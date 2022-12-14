-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Language.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Language.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transPIdent :: Language.Abs.PIdent -> Result
transPIdent x = case x of
  Language.Abs.PIdent string -> failure x

transProgram :: Language.Abs.Program -> Result
transProgram x = case x of
  Language.Abs.Prog stmts -> failure x

transStmt :: Language.Abs.Stmt -> Result
transStmt x = case x of
  Language.Abs.SAss pident exp -> failure x
  Language.Abs.SExp exp -> failure x

transExp :: Language.Abs.Exp -> Result
transExp x = case x of
  Language.Abs.ExpAdd exp1 exp2 -> failure x
  Language.Abs.ExpSub exp1 exp2 -> failure x
  Language.Abs.ExpMul exp1 exp2 -> failure x
  Language.Abs.ExpDiv exp1 exp2 -> failure x
  Language.Abs.ExpLit integer -> failure x
  Language.Abs.ExpVar pident -> failure x
