module OpCodes where

import Data.Word (Word8, Word16)
import Data.Bits
import CPU
import Lens.Micro

handleOpCode :: Word16 -> CPU
handleOpCode code =
    case operation code of
      0x0 -> undefined -- SYS, CLS, RET 
      0x1 -> undefined -- JP addr
      0x2 -> undefined -- CALL addr
      0x3 -> undefined -- SE vx byte
      0x4 -> undefined -- SNE vx byte
      0x5 -> undefined -- SE vx vy
      0x6 -> undefined -- LD vx byte
      0x7 -> undefined -- ADD vx byte
      0x8 -> undefined -- LD vx vy, OR/And/XOR/ADD/SUB/SHR/SUBN/SHL
      0x9 -> undefined -- SNE vx vy
      0xA -> undefined -- LD i addr
      0xB -> undefined -- JP v0 addr
      0xC -> undefined -- RND vx byte
      0xD -> undefined -- DRW vx vy nibble
      0xE -> undefined -- SKP vx, SKNP vx
      0xF -> undefined -- LD (lots of them)

jump :: CPU -> Word16 -> CPU
jump cpu val = cpu & pc .~ val

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

