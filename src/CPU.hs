module CPU where

import Data.Word (Word8,Word16)
import Data.Vector
import Registers

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
