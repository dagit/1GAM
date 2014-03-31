{-# LANGUAGE DeriveGeneric #-}
module Language.NES.AddressMode where

import Data.Word
import GHC.Generics

data AddressMode
  = Implied
  | Accumulator
  | Absolute  !Word16
  | AbsoluteX !Word16
  | AbsoluteY !Word16
  | Indirect  !Word8
  | IndirectX !Word8
  | IndirectY !Word8
  | Immediate !Word8
  | Relative  !Word8
  | Zeropage  !Word8
  | ZeropageX !Word8
  | ZeroPageY !Word8
  deriving (Read, Show, Eq, Ord, Generic)

