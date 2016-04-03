module Registers (
             Registers
           , setRegister
           , getRegister
           , addToRegister
           , addRegisters
           , subtractFromRegister
           , subtractRegisters
           , maybeIntegral
           ) where

import Data.Word (Word8,Word16)
import Data.Vector
import Data.Bits

type Registers = Vector Word8

-- Register actions http://mattmik.com/files/chip8/mastering/chip8.html
setRegister :: Registers -> Word8 -> Word8 -> Registers
setRegister v reg val = v // [(fromIntegral reg, val)]

getRegister :: Registers -> Word8 -> Maybe Word8
getRegister v reg = v !? fromIntegral reg

addToRegister :: Registers -> Word8 -> Word8 -> Registers 
addToRegister v reg val = setRegister v reg (maybeIntegral (getRegister v reg) + (fromIntegral val))

addRegisters :: Registers -> Word8 -> Word8 -> Registers
addRegisters v reg reg' = addToRegister v reg (maybeIntegral (getRegister v reg'))

subtractFromRegister :: Registers -> Word8 -> Word8 -> Registers
subtractFromRegister v reg val = setRegister v reg (maybeIntegral (getRegister v reg) - (fromIntegral val)) 

subtractRegisters :: Registers -> Word8 -> Word8 -> Registers
subtractRegisters v reg reg' = subtractFromRegister v reg (maybeIntegral (getRegister v reg')) 

maybeIntegral :: (Num a, Integral b) => Maybe b -> a
maybeIntegral Nothing = 0
maybeIntegral (Just n) = fromIntegral n

-- Can also: AND (.&.), OR (.|.), xor, shiftR, shiftL using Data.Bits


