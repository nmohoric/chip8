module OpCodes where

import Data.Word (Word8, Word16)
import Data.Bits
import Data.Vector ((//), (!))
import Lens.Micro
import Lens.Micro.Mtl (view)
import CPU
import Stack
import Registers

handleOpCode :: CPU -> Word16 -> CPU
handleOpCode cpu code =
    case operation code of
      0x0 -> undefined -- SYS, CLS, RET 
      0x1 -> jump cpu $ addr code
      0x2 -> call cpu $ addr code
      0x3 -> skip cpu (==) (getRegister (view registers cpu) (vx code)) (Just $ byte code)
      0x4 -> skip cpu (/=) (getRegister (view registers cpu) (vx code)) (Just $ byte code)
      0x5 -> skip cpu (==) (getRegister (view registers cpu) (vx code)) (getRegister (view registers cpu) (vy code))
      0x6 -> load cpu (vx code) (byte code) 
      0x7 -> add cpu (vx code) (byte code)
      0x8 -> undefined -- LD vx vy, OR/And/XOR/ADD/SUB/SHR/SUBN/SHL
      0x9 -> skip cpu (/=) (getRegister (view registers cpu) (vx code)) (getRegister (view registers cpu) (vy code))
      0xA -> setI cpu $ addr code
      0xB -> jump cpu (maybeIntegral (getRegister (view registers cpu) 0) + (addr code))
      0xC -> undefined -- RND vx byte
      0xD -> undefined -- DRW vx vy nibble
      0xE -> undefined -- SKP vx, SKNP vx
      0xF -> undefined -- LD (lots of them)

jump :: CPU -> Word16 -> CPU
jump cpu val = cpu & pc .~ val

call :: CPU -> Word16 -> CPU
call cpu val = jump (addToStack cpu val) val

load :: CPU -> Word8 -> Word8 -> CPU
load cpu vx byte = cpu & registers %~ (// [(fromIntegral vx, byte)])

add :: CPU -> Word8 -> Word8 -> CPU
add cpu vx byte =
    cpu & registers %~
              (// [(fromIntegral vx,
                   (maybeIntegral $ getRegister (view registers cpu) vx)
                   + byte)
                   ]
              )

skip :: CPU -> (Word8 -> Word8 -> Bool) -> Maybe Word8 -> Maybe Word8 -> CPU
skip cpu f Nothing _ = cpu
skip cpu f _ Nothing = cpu
skip cpu f (Just b) (Just b') = 
      if f b b'
        then increasePC cpu
        else cpu

operation :: Word16 -> Word16
operation x = (x .&. 0xF000) `shiftR` 12

addr :: Word16 -> Word16
addr x = x .&. 0x0FFF

vx :: Word16 -> Word8
vx x = fromIntegral $ (x .&. 0x0F00) `shiftR` 8

vy :: Word16 -> Word8
vy x = fromIntegral $ (x .&. 0x00F0) `shiftR` 4

byte :: Word16 -> Word8
byte x = fromIntegral $ x .&. 0x00FF

nibble :: Word16 -> Word16
nibble x = x .&. 0x000F

