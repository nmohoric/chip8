module CPU where

import Data.Word (Word8,Word16)
import Data.Vector
import Data.Bits

type Registers = Vector Word8

-- http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#2.0
data CPU = 
    CPU { memory     :: Vector Word8
        , registers  :: Registers 
        , delayTimer :: Word8
        , soundTimer :: Word8
        , pc         :: Word16
        , sp         :: Word8
        , stack      :: Vector Word16
        , input      :: Vector Bool
        , graphics   :: Vector Bool
         }


-- Register actions http://mattmik.com/files/chip8/mastering/chip8.html
setRegister :: Registers -> Word8 -> Word8 -> Registers
setRegister v reg val = v // [(fromIntegral reg, val)]

getRegister :: Registers -> Word8 -> Maybe Word8
getRegister v reg = v !? fromIntegral reg

addToRegister :: Registers -> Word8 -> Word8 -> Registers 
addToRegister v reg val = setRegister v reg (maybeIntegral (getRegister v reg) + (fromIntegral val))

addRegisters :: Registers -> Word8 -> Word8 -> Registers
addRegisters v reg reg' = addToRegister v reg (maybeIntegral (getRegister v reg'))

maybeIntegral :: (Num a, Integral b) => Maybe b -> a
maybeIntegral Nothing = 0
maybeIntegral (Just n) = fromIntegral n

subtractFromRegister :: Registers -> Word8 -> Word8 -> Registers
subtractFromRegister v reg val = setRegister v reg (maybeIntegral (getRegister v reg) - (fromIntegral val)) 

subtractRegisters :: Registers -> Word8 -> Word8 -> Registers
subtractRegisters v reg reg' = subtractFromRegister v reg (maybeIntegral (getRegister v reg')) 

-- Can also: AND (.&.), OR (.|.), xor, shiftR, shiftL using Data.Bits


