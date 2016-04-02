{-# LANGUAGE TemplateHaskell #-}

module CPU where

import Data.Word (Word8,Word16)
import Data.Vector
import Registers
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((%~))

-- http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#2.0
data CPU = 
    CPU { _memory     :: Vector Word8
        , _registers  :: Registers 
        , _delayTimer :: Word8
        , _soundTimer :: Word8
        , _i          :: Word16
        , _pc         :: Word16
        , _sp         :: Word8
        , _stack      :: Vector Word16
        , _input      :: Vector Bool
        , _graphics   :: Vector Bool
         }

makeLenses ''CPU

increasePC :: CPU -> CPU
increasePC = pc %~ (+2)
