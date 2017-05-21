{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc,
      Term(..),
      isnumericval
    ) where

import Control.Exception.Safe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--例外定義
data NoRuleException = NoRuleException
  deriving(Show, Typeable)
instance Exception NoRuleException

data Term =
    TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving(Show)

isnumericval:: Term->Bool  
isnumericval t =
  case t of
    TmZero    -> True
    TmSucc t' -> isnumericval t'
    _         -> False 

eval1::Term-> Maybe Term
eval1 t =
  case t of
    TmIf TmTrue t2 t3 -> Just t2
    TmIf TmFalse t2 t3 -> Just t3
    TmIf t1 t2 t3 -> do
      t1' <- eval1 t1
      Just $ TmIf t1' t2 t3
    TmSucc t1 -> do
      t1' <- eval1 t1 
      Just $ TmSucc t1'
    TmPred TmZero -> Just $ TmZero
    TmPred (TmSucc nv1) | isnumericval nv1 -> Just nv1
                        | otherwise        -> Nothing
    TmPred t1 -> do
      t1' <- eval1 t1
      Just $ TmPred t1' 
    TmIsZero TmZero -> Just TmTrue
    TmIsZero ( TmSucc nv1) | isnumericval nv1 -> Just TmFalse
                           | otherwise        -> Nothing
    TmIsZero t1 -> do
      t1' <- eval1 t1
      Just $ TmIsZero t1'
    _ -> Nothing
      
eval :: Term -> Term
eval t = case eval1 t of
  Just t' -> eval t'
  Nothing -> t
       

