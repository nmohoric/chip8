module OpCodes where

import Data.Word (Word8, Word16)
import Data.Bits
import Data.Vector ((//))
import Lens.Micro
import CPU
import Stack

handleOpCode :: CPU -> Word16 -> CPU
handleOpCode cpu code =
    case operation code of
      0x0 -> undefined -- SYS, CLS, RET 
      0x1 -> jump cpu $ addr code
      0x2 -> call cpu $ addr code
      0x3 -> skip cpu (==) (vx code) (byte code) -- not passing reg val
      0x4 -> skip cpu (/=) (vx code) (byte code) -- not passing reg val
      0x5 -> undefined -- SE vx vy
      0x6 -> undefined -- LD vx byte
      0x7 -> undefined -- ADD vx byte
      0x8 -> undefined -- LD vx vy, OR/And/XOR/ADD/SUB/SHR/SUBN/SHL
      0x9 -> undefined -- SNE vx vy
      0xA -> setI cpu $ addr code -- LD i addr
      0xB -> undefined -- JP v0 addr
      0xC -> undefined -- RND vx byte
      0xD -> undefined -- DRW vx vy nibble
      0xE -> undefined -- SKP vx, SKNP vx
      0xF -> undefined -- LD (lots of them)

jump :: CPU -> Word16 -> CPU
jump cpu val = cpu & pc .~ val

call :: CPU -> Word16 -> CPU
call cpu val = jump (addToStack cpu val) val

skip :: CPU -> (Word16 -> Word16 -> Bool) -> Word16 -> Word16 -> CPU
skip cpu f vx byte = 
      if f vx byte
        then increasePC cpu
        else cpu

operation :: Word16 -> Word16
operation x = (x .&. 0xF000) `shiftR` 12

addr :: Word16 -> Word16
addr x = x .&. 0x0FFF

vx :: Word16 -> Word16
vx x = (x .&. 0x0F00) `shiftR` 8

vy :: Word16 -> Word16
vy x = (x .&. 0x00F0) `shiftR` 4

-- I think this should be a Word8
byte :: Word16 -> Word16
byte x = x .&. 0x00FF

nibble :: Word16 -> Word16
nibble x = x .&. 0x000F

