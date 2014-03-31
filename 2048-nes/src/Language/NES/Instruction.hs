{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.NES.Instruction where

import           Language.NES.AddressMode ( AddressMode )
import qualified Language.NES.AddressMode as A

import Data.Word
import GHC.Generics

-- | Defines the assembly instructions for the 6502

data Instruction = Instruction OpCode AddressMode
  deriving (Read, Show, Eq, Ord, Generic)

data OpCode =
  -- | ADC, Add Memory to Accumulator with Carry
  -- A + M + C -> A, C
  -- N Z C I D V
  -- + + + - - +
    AddAC
  -- | AND, AND Memory with Accumulator
  -- A AND M -> A
  -- N Z C I D V
  -- + + - - - -
  | AndAC
  -- | ASL, Shift Left One Bit (Memory of Accumulator)
  -- C <- [76543210] <- 0
  -- N Z C I D V
  -- + + + - - -
  | ASL
  -- | BCC, Branch on Carry Clear
  -- branch if C = 0
  -- N Z C I D V
  -- - - - - - -
  | BCC
  -- | BCS, Branch On Carry Set
  -- branch if C = 1
  -- N Z C I D V
  -- - - - - - -
  | BCS
  -- | BEQ, Branch on Result Zero
  -- branch if Z = 1
  -- N Z C I D V
  -- - - - - - -
  | BEQ
  -- | BIT, Test Bits in Memory with Accumulator
  -- A AND M, M7 -> N, M6 -> V
  --  N Z C I D V
  -- M7 + - - - M6
  | BIT
  -- | BMI, Branch on Result Minus
  -- branch if N = 1
  -- N Z C I D V
  -- - - - - - -
  | BMI
  -- | BNE, Branch on Result not Zero
  -- branch if Z = 0
  -- N Z C I D V
  -- - - - - - -
  | BNE
  -- | BPL, Branch on Resutl Plus
  -- branch if N = 0
  -- N Z C I D V
  -- - - - - - -
  | BPL
  -- | BRK, Force Break
  | BRK
  -- | BVC, Break on Overflow Clear
  -- branch if V = 0
  -- N Z C I D V
  -- - - - - - -
  | BVC
  -- | BVS, Branch on Overflow Set
  -- branch if V = 1
  -- N Z C I D V
  -- - - - - - -
  | BVS
  -- | CLC, Clear Carry flag
  -- 0 -> C
  -- N Z C I D V
  -- - - 0 - - -
  | CLC
  -- | CLD, Clear Decimal Mode
  -- 0 -> D
  -- N Z C I D V
  -- - - - - 0 -
  | CLD
  -- | CLI, Clear Interrupt Disable Bit
  -- 0 -> I
  -- N Z C I D V
  -- - - - 0 - -
  | CLI
  -- | CLV, Clear Overflow Flag
  -- 0 -> V
  -- N Z C I D V
  -- - - - - - 0
  | CLV
  -- | CMP, Compare Memory with Accumulator
  -- A - M
  -- N Z C I D V
  -- + + + - - -
  | CMP
  -- | CPX, Compare Memory and Index X
  -- X - M
  -- N Z C I D V
  -- + + + - - -
  | CPX
  -- | CPY, Compare Memory and Index Y
  -- Y - M
  -- N Z C I D V
  -- + + + - - -
  | CPY
  -- | DEC, Decrement Memory by One
  -- M - 1 -> M
  -- N Z C I D V
  -- + + - - - -
  | DEC
  -- | DEX, Decrement Index X by One
  -- X - 1 -> X
  -- N Z C I D V
  -- + + - - - -
  | DEX
  -- | DEY, Decrement Index Y by One
  -- Y - 1 -> Y
  -- N Z C I D V
  -- + + - - - -
  | DEY
  -- | EOR, Exclusive-OR Memory with Accumulator
  -- A EOR M -> A
  -- N Z C I D V
  -- + + - - - -
  | EOR
  -- | INC, Increment Memory by One
  -- M + 1 -> M
  -- N Z C I D V
  -- + + - - - -
  | INC
  -- | INX, Increment Index X by One
  -- X + 1 -> X
  -- N Z C I D V
  -- + + - - - -
  | INX
  -- | INY, Increment Index Y by One
  -- Y + 1 -> Y
  -- N Z C I D V
  -- + + - - - -
  | INY
  -- | JMP, Jump to New Location
  -- (PC + 1) -> PCL
  -- (PC + 2) -> PCH
  -- N Z C I D V
  -- - - - - - -
  | JMP
  -- | JSR, Jump to New Location Saving Return Address
  -- push (PC + 2)
  -- (PC + 1) -> PCL
  -- (PC + 2) -> PCH
  -- N Z C I D V
  -- - - - - - -
  | JSR
  -- | LDA, Load Accumulator with Memory
  -- M -> A
  -- N Z C I D V
  -- + + - - - -
  | LDA
  -- | LDX, Load Index X with Memory
  -- M -> X
  -- N Z C I D V
  -- + + - - - -
  | LDX
  -- | LDY, Load Index Y with Memory
  -- M -> Y
  -- N Z C I D V
  -- + + - - - -
  | LDY
  -- | LSR, Shift One Bit Right (Memory or Accumulator)
  -- 0 -> [76543210] -> C
  -- N Z C I D V
  -- - + + - - -
  | LSR
  -- | NOP, No Operation
  | NOP
  -- | ORA, OR Memory with Accumulator
  -- A OR M -> A
  -- N Z C I D V
  -- + + - - - -
  | ORA
  -- | PHA, Push Accumulator on Stack
  -- push A
  -- N V C I D V
  -- - - - - - -
  | PHA
  -- | PHP, Push Processor Status on Stack
  -- push SR
  -- N Z C I D V
  -- - - - - - -
  | PHP
  -- | PLA, Pull Accumulator from Stack
  -- pull A
  -- N Z C I D V
  -- + + - - - -
  | PLA
  -- | PLP, Pull Processor Status from Stack
  -- pull SR
  -- N Z C I D V
  -- + + + + + +
  | PLP
  -- | ROL, Rotate One Bit Left (Memory or Accumulator)
  -- C <- [76543210] <- C
  -- N Z C I D V
  -- + + + - - -
  | ROL
  -- | ROR, Rotate One Bit Right (Memory or Accumulator)
  -- C -> [76543210] -> C
  -- N Z C I D V
  -- + + + - - -
  | ROR
  -- | RTI, Return from Interrupt
  -- pull SR, pull PC
  -- N Z C I D V
  -- - - - - - -
  | RTI
  -- | RTS, Return from Subroutine
  -- pull PC, PC + 1 -> PC
  -- N Z C I D V
  -- - - - - - -
  | RTS
  -- | SBC, Subtract Memory from Accumulator with Borrow
  -- A - M - C -> A
  -- N Z C I D V
  -- + + + - - +
  | SBC
  -- | SEC, Set Carry Flag
  -- 1 -> C
  -- N Z C I D V
  -- - - 1 - - -
  | SEC
  -- | SED, Set Decimal Flag
  -- 1 -> D
  -- N Z C I D V
  -- - - - - 1 -
  | SED
  -- | SEI, Set Interrupt Disable Status
  -- 1 -> I
  -- N Z C I D V
  -- - - - 1 - -
  | SEI
  -- | STA, Store Accumulator in Memory
  -- A -> M
  -- N Z C I D V
  -- - - - - - -
  | STA
  -- | STX, Store Index X in Memory
  -- X -> M
  -- N Z C I D V
  -- - - - - - -
  | STX
  -- | STY, Store Index Y in Memory
  -- Y -> M
  -- N Z C I D V
  -- - - - - - -
  | STY
  -- | TAX, Transfer Accumulator to Index X
  -- A -> X
  -- N Z C I D V
  -- + + - - - -
  | TAX
  -- | TAY, Transfer Accumulator to Index Y
  -- A -> Y
  -- N Z C I D V
  -- + + - - - -
  | TAY
  -- | TSX, Transfer Stack Pointer to Index x
  -- SP -> X
  -- N Z C I D V
  -- + + - - - -
  | TSX
  -- | TXA, Transfer Index X to Accumulator
  -- X -> A
  -- N Z C I D V
  -- + + - - - -
  | TXA
  -- | TXS, Transfer Index X to Stack Register
  -- X -> SP
  -- N Z C I D V
  -- + + - - - -
  | TXS
  -- | TYA -> Transfer Index Y to Accumulator
  -- Y -> A
  -- N Z C I D V
  -- + + - - - -
  | TYA
  deriving (Read, Show, Eq, Ord, Generic)

{- TODO: I don't think this is needed.
data Register
  = PC -- ^ Program Counter
  | AC -- ^ Accumulator
  | X  -- ^ X register
  | Y  -- ^ Y register
  | SR -- ^ Status register [NV-BDIZC]
  | SP -- ^ Stack pointer
  deriving (Read, Show, Eq, Ord, Generic)
-}

data PC = PC
data A  = A
data X  = X
data Y  = Y
data SR = SR
data SP = SP
data Relative  = Relative  !Word8
data Indirect  = Indirect  !Word8
data Absolute  = Absolute  !Word16
data Immediate = Immediate !Word8
data Zeropage  = Zeropage  !Word8

class Address a where
  addressMode :: a -> AddressMode

instance Address A where
  addressMode A = A.Accumulator
instance Address Absolute where
  addressMode (Absolute  x) = A.Absolute x
instance Address (Absolute, X) where
  addressMode (Absolute x, X) = A.AbsoluteX x
instance Address (Absolute, Y) where
  addressMode (Absolute y, Y) = A.AbsoluteY y
instance Address Immediate where
  addressMode (Immediate i) = A.Immediate i
instance Address Zeropage where
  addressMode (Zeropage  x) = A.Zeropage x
instance Address (Zeropage, X) where
  addressMode (Zeropage x, X) = A.ZeropageX x
instance Address (Indirect, X) where
  addressMode (Indirect x, X) = A.IndirectX x
instance Address (Indirect, Y) where
  addressMode (Indirect y, Y) = A.IndirectY y
instance Address Relative where
  addressMode (Relative w) = A.Relative w

class Address a => GenericOp a where
instance GenericOp Absolute      where
instance GenericOp (Absolute, X) where
instance GenericOp (Absolute, Y) where
instance GenericOp Immediate     where
instance GenericOp Zeropage      where
instance GenericOp (Zeropage, X) where
instance GenericOp (Indirect, X) where
instance GenericOp (Indirect, Y) where

mkI :: Address a => OpCode -> a -> Instruction
mkI x = Instruction x . addressMode

lda, and, cmp :: GenericOp a => a -> Instruction
[lda, and, cmp] = [ mkI x | x <- [LDA, AndAC, CMP] ]

class Address a => ArithOp a  where
instance ArithOp A            where
instance ArithOp Zeropage     where
instance ArithOp (Zeropage,X) where
instance ArithOp Absolute     where
instance ArithOp (Absolute,X) where

asl :: ArithOp a => a -> Instruction
asl = mkI ASL

class Address a => RelativeOp a where
instance RelativeOp Relative where

bcc, bcs, beq, bmi, bne, bpl, bvc, bvs :: RelativeOp a => a -> Instruction
[bcc, bcs, beq, bmi, bne, bpl, bvc, bvs]
  = [ mkI x | x <- [BCC, BCS, BEQ, BMI, BNE, BPL, BVC, BVS] ]

class Address a => BitOp a where
instance BitOp Zeropage where
instance BitOp Absolute where

bit, brk, clc, cld, cli :: Instruction
[bit, brk, clc, cld, cli]
  = [ Instruction x A.Implied | x <- [BIT, BRK, CLC, CLD, CLI] ]

{- TODO: instead of the syntax hacks below,
build a quasiquoter that takes the following syntax:
http://www.masswerk.at/6502/6502_instruction_set.html
OPC A
OPC $HHLL
OPC $HHLL,X
OPC $HHLL,Y
OPC #$BB
OPC
OPC ($HHLL)
OPC ($BB,X)
OPC ($LL),Y
OPC $BB
OPC $LL
OPC $LL,X
OPC $LL,Y

Combined with the type class resolution above, it should
provide with a nice way to only construct valid instructions.

(#) :: (Immediate -> a) -> Word8 -> a
x # y = x (Immediate y)

infixl 1 $$
($$) :: (Absolute -> a) -> Word16 -> a
x $$ y = x (Absolute y)
infixl 1 $^
($^) :: ((Absolute,b) -> a) -> (Word16,b) -> a
x $^ (w,y) = x (Absolute w, y)
infixl 2 ~+
(~+) :: a -> b -> (a, b)
(~+) = (,)

i :: Word8 -> Indirect
i = Indirect

immediate = lda #0xff
zeropage  = lda 00
zeropagex = lda $^00~+X
absolute  = lda $$1234
absolutex = lda $^1234~+X
absolutey = lda $^1234~+Y
indirectx = lda (i $$00,X)
indirecty = lda (i $$00,Y)
-}


{-
class Opcodes a where
  opcodes :: a -> [Word8]

instance Opcodes Word8 where
  opcodes w = [w]


class Opcodes a => LdaOp a where
  opcode :: a -> [Word8]
-}
