module OpCodes where

import Data.Word (Word8, Word16)
import Data.Bits
import CPU

handleOpCode :: Word16 -> CPU
handleOpCode code =
    case operation code of
      0x0 -> undefined 
      0x1 -> undefined
      0x2 -> undefined
      0x3 -> undefined
      0x4 -> undefined
      0x5 -> undefined
      0x6 -> undefined
      0x7 -> undefined
      0x8 -> undefined
      0x9 -> undefined
      0xA -> undefined
      0xB -> undefined
      0xC -> undefined
      0xD -> undefined
      0xE -> undefined
      0xF -> undefined

jump :: CPU -> Word16 -> CPU
jump cpu val = cpu { CPU.pc = val } 

operation :: Word16 -> Word16
operation x = (x .&. 0xF000) `shiftR` 12

addr :: Word16 -> Word16
addr x = x .&. 0x0FFF

vx :: Word16 -> Word16
vx x = (x .&. 0x0F00) `shiftR` 8

vy :: Word16 -> Word16
vy x = (x .&. 0x00F0) `shiftR` 4

byte :: Word16 -> Word16
byte x = x .&. 0x00FF

nibble :: Word16 -> Word16
nibble x = x .&. 0x000F

