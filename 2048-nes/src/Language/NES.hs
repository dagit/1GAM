{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.NES
( NES
, runNES
) where

import Language.NES.Instruction

import Control.Applicative
import GHC.Generics

data AssemblerState = AssemblerState
  { instrucions :: [Instruction]
  }
  deriving (Read, Show, Eq, Ord, Generic)

-- Initially, I thought I would make a cute little
-- codegen monad, but now I'm leaning towards
-- embedding assembly using quasiquotation.
-- Then building higher level constructions on top
-- of that. I'm not sure if the monad will be useful
-- with that way of doing thinngs.
newtype NES a = NES { unNES :: a }
  deriving Functor

instance Applicative NES where
  pure    = NES
  f <*> a = case (f,a) of
    (NES f', NES a') -> NES (f' a')

instance Monad NES where
  return  = pure
  m >>= f = case m of
    NES a -> f a

runNES :: NES a -> a
runNES = unNES
  
