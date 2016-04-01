module CPU where

import qualified Data.Word as W (Word8,Word16)
import qualified Data.Vector as V

-- http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#2.0
data CPU = 
    CPU { memory     :: V.Vector Word8
        , registers  :: V.Vector Word8
        , delayTimer :: W.Word8
        , soundTimer :: W.Word8
        , pc         :: W.Word16
        , sp         :: W.Word8
        , stack      :: V.Vector Word16
        , input      :: V.Vector Bool
        , graphics   :: V.Vector Bool
         }


