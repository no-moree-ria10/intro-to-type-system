module Lib
    ( someFunc,
      Term(..),
      isnumericval
    ) where

import Control.Exception.Safe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
    
