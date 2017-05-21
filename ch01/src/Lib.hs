module Lib
    ( someFunc,
      Term(..)
    ) where

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

isnumericval:: Term->Bool  
isnumericval t =
  case t of
    TmZero    -> True
    TmSucc t' -> isnumericval t'
    _         -> False 
    
