module Stack where

import Data.Word (Word16, Word8)
import Lens.Micro.Mtl (view)
import Lens.Micro (over, (.~), (%~), (&))
import CPU
import Data.Vector ((!), (//))

addToStack :: CPU -> Word16 -> CPU
addToStack cpu val = cpu & stack %~ (// [(getSP cpu + 1, val)])
                         & sp %~ (\x -> x+1)

popStack :: CPU -> (Word16, CPU)
popStack cpu = (view stack cpu ! getSP cpu
               , over sp (\x -> x-1) cpu)

getSP :: CPU -> Int
getSP = fromIntegral . view sp


