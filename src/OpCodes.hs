module OpCodes where

import Data.Word (Word8, Word16)
import Data.Bits
import Data.Vector ((//), (!))
import Lens.Micro
import Lens.Micro.Mtl (view)
import CPU
import Stack
import Registers

data BitwiseOp = 
            OR
          | XOR
          | AND
          | ADD
          | SUB
          | SHR
          | SUBN
          | SHL
          deriving (Eq)

handleOpCode :: CPU -> Word16 -> CPU
handleOpCode cpu code =
    case operation code of
      0x0 -> cpu & set message "Zero" 
                 & increasePC -- SYS, CLS, RET 
      0x1 -> jump cpu $ addr code
      0x2 -> call cpu $ addr code
      0x3 -> skip cpu (==) (getRegister (view registers cpu) (vx code)) (Just $ byte code)
      0x4 -> skip cpu (/=) (getRegister (view registers cpu) (vx code)) (Just $ byte code)
      0x5 -> skip cpu (==) (getRegister (view registers cpu) (vx code)) (getRegister (view registers cpu) (vy code))
      0x6 -> load cpu (vx code) (byte code) 
      0x7 -> add cpu (vx code) (byte code)
      0x8 -> case nibble code of
               0x0 -> load cpu (vx code) ((view registers cpu) ! (fromIntegral (vy code)))
               0x1 -> load cpu (vx code) (bitwise OR cpu code)
               0x2 -> load cpu (vx code) (bitwise AND cpu code)
               0x3 -> load cpu (vx code) (bitwise XOR cpu code)
               0x4 -> load cpu (vx code) (bitwise ADD cpu code)
               0x5 -> load cpu (vx code) (bitwise SUB cpu code)
               0x6 -> load cpu (vx code) (bitwise SHR cpu code)
               0x7 -> load cpu (vx code) (bitwise SUBN cpu code)
               0xE -> load cpu (vx code) (bitwise SHL cpu code)
      0x9 -> skip cpu (/=) (getRegister (view registers cpu) (vx code)) (getRegister (view registers cpu) (vy code))
      0xA -> setI cpu $ addr code
      0xB -> jump cpu (maybeIntegral (getRegister (view registers cpu) 0) + (addr code))
      0xC -> cpu & set message "Need random" 
                 & increasePC -- RND vx byte
      0xD -> cpu & set message "Need graphics" 
                 & increasePC -- DRW vx vy nibble
      0xE -> case byte code of
               0x9E -> skipKey cpu (==) (getRegister (view registers cpu) (vx code))
               0xA1 -> skipKey cpu (/=) (getRegister (view registers cpu) (vx code))
      0xF -> case byte code of
               0x07 -> load cpu (vx code) (fromIntegral (view delayTimer cpu)) 
               0x0A -> undefined -- LD Vx, K (keyboard)
               0x15 -> undefined
               0x18 -> undefined
               0x1E -> undefined
               0x29 -> undefined
               0x33 -> undefined
               0x55 -> undefined
               0x65 -> undefined

jump :: CPU -> Word16 -> CPU
jump cpu val = cpu & pc .~ val
                   & increasePC

call :: CPU -> Word16 -> CPU
call cpu val = jump (addToStack cpu val) val

load :: CPU -> Word8 -> Word8 -> CPU
load cpu vx byte = cpu & registers %~ (// [(fromIntegral vx, byte)])
                       & increasePC

add :: CPU -> Word8 -> Word8 -> CPU
add cpu vx byte =
    cpu & registers %~
              (// [(fromIntegral vx,
                   (maybeIntegral $ getRegister (view registers cpu) vx)
                   + byte)
                   ]
              )
        & increasePC

skip :: CPU -> (Word8 -> Word8 -> Bool) -> Maybe Word8 -> Maybe Word8 -> CPU
skip cpu f Nothing _ = cpu
skip cpu f _ Nothing = cpu
skip cpu f (Just b) (Just b') = 
      if f b b'
        then increasePC cpu
        else cpu

skipKey :: CPU -> (Bool -> Bool -> Bool) -> Maybe Word8 -> CPU
skipKey cpu f Nothing = increasePC cpu
skipKey cpu f (Just vx) = 
      if f key True 
        then cpu & increasePC & increasePC
        else cpu & increasePC
      
  where
    key = (view input cpu) ! (fromIntegral vx)

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

word8to16 :: Word8 -> Word8 -> Word16
word8to16 h l = shift (fromIntegral h) 8 .|. fromIntegral l

bitwise :: BitwiseOp -> CPU -> Word16 -> Word8 
bitwise OR cpu code   = (regVal cpu (vx code)) .|. (regVal cpu (vy code))
bitwise AND cpu code  = (regVal cpu (vx code)) .&. (regVal cpu (vy code))
bitwise XOR cpu code  = (regVal cpu (vx code)) `xor` (regVal cpu (vy code)) 
bitwise ADD cpu code  = undefined
bitwise SUB cpu code  = undefined
bitwise SHR cpu code  = undefined
bitwise SUBN cpu code = undefined
bitwise SHL cpu code  = undefined

regVal :: CPU -> Word8 -> Word8
regVal cpu i = (view registers cpu) ! (fromIntegral i)
