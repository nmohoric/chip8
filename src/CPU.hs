{-# LANGUAGE TemplateHaskell #-}

module CPU where

import Data.Word (Word8,Word16)
import Data.Vector as V
import Registers
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((.~), (%~), (&))
import Lens.Micro.Mtl (view)

-- http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#2.0
data CPU = 
    CPU { _memory     :: V.Vector Word8
        , _registers  :: Registers 
        , _delayTimer :: Word8
        , _soundTimer :: Word8
        , _i          :: Word16
        , _pc         :: Word16
        , _sp         :: Word8
        , _stack      :: V.Vector Word16
        , _input      :: V.Vector Bool
        , _graphics   :: V.Vector Bool
        , _message    :: String
        } deriving (Eq, Show)

makeLenses ''CPU


initCPU :: CPU
initCPU = CPU { _memory     = V.replicate 4096 0
              , _registers  = V.replicate 16 0
              , _delayTimer = 0
              , _soundTimer = 0
              , _i          = 0
              , _pc         = 0x200
              , _sp         = 0
              , _stack      = V.replicate 12 0
              , _input      = V.replicate 16 False
              , _graphics   = V.replicate (64 * 32) False
              , _message    = ""
              }
              
increasePC :: CPU -> CPU
increasePC = pc %~ (+2)

decreaseTimers :: CPU -> CPU
decreaseTimers cpu = cpu & delayTimer %~ (\x -> if x == 0 then 0 else x - 1)
                         & soundTimer %~ (\x -> if x == 0 then 0 else x - 1)


setI :: CPU -> Word16 -> CPU
setI cpu val = cpu & i .~ val 
