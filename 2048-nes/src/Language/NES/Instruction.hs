{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.NES.Instruction where

-- import           Language.NES.AddressMode ( AddressMode )
-- import qualified Language.NES.AddressMode as A

import Data.Word
import Data.Bits
import GHC.Generics
import Control.Applicative

-- | Defines the assembly instructions for the 6502

data Instruction = Instruction Mnemonic Operand
  deriving (Read, Show, Eq, Ord, Generic)

type OpCode = Word8

data Operand
  = Implied   | Accumulator
  | Absolute  | AbsoluteX | AbsoluteY
  | Indirect  | IndirectX | IndirectY
  | Immediate | Relative
  | Zeropage  | ZeropageX | ZeropageY
  deriving (Read, Show, Eq, Ord, Generic)

data Mnemonic
  = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI
  | BNE | BPL | BRK | BVC | BVS | CLC | CLD | CLI
  | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR
  | INC | INX | INY | JMP | JSR | LDA | LDX | LDY
  | LSR | NOP | ORA | PHA | PHP | PLA | PLP | ROL
  | ROR | RTI | RTS | SBC | SEC | SED | SEI | STA
  | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
  deriving (Read, Show, Eq, Ord, Generic)

-- | As defined by: http://www.llx.com/~nparker/a2/opcodes.html
opcode :: Mnemonic -> Operand -> Maybe OpCode
opcode m op  =  fmap fromBits decode
            <|> lookup m rest

  where
  fromBits :: [Word8] -> OpCode
  fromBits = foldl (\a b -> a `shiftL` 1 + b) 0
  (<+>) = liftA2 (++)

  -- Could use a better name, basically there is a group
  -- of instructions that decode this way. Same for decodeOp2
  -- and decodeOp3. See decode for details.
  decodeOp1 :: Maybe [Word8]
  decodeOp1 = case op of
    IndirectX            -> Just [0,0,0]
    Zeropage             -> Just [0,0,1]
    Immediate | m /= STA -> Just [0,1,0]
    Absolute             -> Just [0,1,1]
    IndirectY            -> Just [1,0,0]
    ZeropageX            -> Just [1,0,1]
    AbsoluteY            -> Just [1,1,0]
    AbsoluteX            -> Just [1,1,1]
    _                    -> Nothing

  decodeOp2 :: Maybe [Word8]
  decodeOp2 = case op of
    Immediate   | m == LDX                      -> Just [0,0,0]
    Accumulator | m `elem` [ASL, ROL, LSR, ROR] -> Just [0,1,0]
    Zeropage                                    -> Just [0,0,1]
    ZeropageX   | m `notElem` [LDX, STX]        -> Just [1,0,1]
    ZeropageY   | m `elem`    [LDX, STX]        -> Just [1,0,1]
    Absolute                                    -> Just [0,1,1]
    AbsoluteX   | m /= LDX                      -> Just [1,1,1]
    AbsoluteY   | m == LDX                      -> Just [1,1,1]
    _                                           -> Nothing

  decodeOp3 :: Maybe [Word8]
  decodeOp3 = case op of
    Immediate | m `elem` [LDY, CPY, CPX]           -> Just [0,0,0]
    Zeropage  | m `elem` [BIT, STY, LDY, CPY, CPX] -> Just [0,0,1]
    Absolute                                       -> Just [0,1,1]
    ZeropageX | m `elem` [STY, LDY]                -> Just [1,0,1]
    AbsoluteX | m == LDY                           -> Just [1,1,1]
    _                                              -> Nothing

  -- Group instructions by supported addressing modes
  decode = case m of
    ORA -> Just [0,0,0] <+> decodeOp1 <+> Just [0,1]
    AND -> Just [0,0,1] <+> decodeOp1 <+> Just [0,1]
    EOR -> Just [0,1,0] <+> decodeOp1 <+> Just [0,1]
    ADC -> Just [0,1,1] <+> decodeOp1 <+> Just [0,1]
    STA -> Just [1,0,0] <+> decodeOp1 <+> Just [0,1]
    LDA -> Just [1,0,1] <+> decodeOp1 <+> Just [0,1]
    CMP -> Just [1,1,0] <+> decodeOp1 <+> Just [0,1]
    SBC -> Just [1,1,1] <+> decodeOp1 <+> Just [0,1]

    ASL -> Just [0,0,0] <+> decodeOp2 <+> Just [1,0]
    ROL -> Just [0,0,1] <+> decodeOp2 <+> Just [1,0]
    LSR -> Just [0,1,0] <+> decodeOp2 <+> Just [1,0]
    ROR -> Just [0,1,1] <+> decodeOp2 <+> Just [1,0]
    STX -> Just [1,0,0] <+> decodeOp2 <+> Just [1,0]
    LDX -> Just [1,0,1] <+> decodeOp2 <+> Just [1,0]
    DEC -> Just [1,1,0] <+> decodeOp2 <+> Just [1,0]
    INC -> Just [1,1,1] <+> decodeOp2 <+> Just [1,0]

    BIT -> Just [0,0,1] <+> decodeOp3 <+> Just [0,0]
    JMP | op == Absolute -> Just [0,1,1] <+> decodeOp3 <+> Just [0,0]
        | otherwise      -> Just [0,1,0] <+> decodeOp3 <+> Just [0,0]
    STY -> Just [1,0,0] <+> decodeOp3 <+> Just [0,0]
    LDY -> Just [1,0,1] <+> decodeOp3 <+> Just [0,0]
    CPY -> Just [1,1,0] <+> decodeOp3 <+> Just [0,0]
    CPX -> Just [1,1,1] <+> decodeOp3 <+> Just [0,0]
    _   -> Nothing

  -- Everything else is one-off, so we use a table.
  -- TODO: need to add a guard that checks the operand
  -- for each of these.
  rest = [ (BPL,0x10), (BMI,0x30), (BVC,0x50), (BVS,0x70)
         , (BCC,0x90), (BCS,0xB0), (BNE,0xD0), (BEQ,0xF0)
         , (BRK,0x00), (JSR,0x20), (RTI,0x40), (RTS,0x60)
         , (PHP,0x08), (PLP,0x28), (PHA,0x48), (PLA,0x68)
         , (DEY,0x88), (TAY,0xA8), (INY,0xC8), (INX,0xE8)
         , (CLC,0x18), (SEC,0x38), (CLI,0x58), (SEI,0x78)
         , (TYA,0x98), (CLV,0xB8), (CLD,0xD8), (SED,0xF8)
         , (TXA,0x8A), (TXS,0x9A), (TAX,0xAA), (TSX,0xBA)
         , (DEX,0xCA), (NOP,0xEA)
         ]
