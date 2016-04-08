{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import Foreign.C.Types
import Data.Foldable (for_)
import Linear (V4(..), V2(..))
import Linear.Affine (Point(..))
import Control.Monad (unless)
import Data.ByteString as BS (readFile, ByteString, unpack) 
import CPU
import OpCodes (handleOpCode, word8to16)
import Data.Vector as V ((//), (!), fromList, update, Vector, filter)
import Data.Vector.Storable as VS (map, imap, Vector, convert)
import Lens.Micro (over, (&))
import Lens.Micro.Mtl (view)
import Data.Word (Word8, Word16)


main :: IO ()
main = do
  contents <- BS.readFile "pong.rom"
  runGame $ loadGame contents 


loadGame :: BS.ByteString -> CPU
loadGame game = cpu & over memory (`V.update` V.fromList (zip [0x200..] $ BS.unpack game))
    where
      cpu = initCPU 

runGame :: CPU -> IO () 
runGame cpu = do 
    SDL.initializeAll 

    let winConfig = SDL.defaultWindow {SDL.windowInitialSize = V2 640 320 }
    window <- SDL.createWindow "Chip8 Emulator" winConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    appLoop renderer cpu 

appLoop :: SDL.Renderer -> CPU -> IO ()
appLoop renderer cpu = do
  events <- SDL.pollEvents
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = not (null (Prelude.filter eventIsQPress events))
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0  
  --doDrawRects (view graphics cpu) renderer
  SDL.drawRect renderer Nothing --(Just (SDL.Rectangle (P $ V2 (160 ::CInt) (80 ::CInt)) (V2 (250 ::CInt) (120 ::CInt))))

  SDL.present renderer
  unless qPressed (appLoop renderer (runCPU cpu))

doDrawRects gfx r = do
    SDL.rendererDrawColor r SDL.$= V4 255 0 0 255
--    SDL.drawRects r $ createRects gfx
--    SDL.fillRects r $ createRects gfx
    
createRects :: V.Vector Bool -> VS.Vector (SDL.Rectangle CInt)
createRects gfx = VS.map (\x -> SDL.Rectangle (P $ V2 10 10) (V2 20 20)) $ VS.convert gfx
--createRects gfx = VS.imap 
--                    (\i x -> SDL.Rectangle (P $ V2 (fromIntegral (xLoc i) :: CInt) (fromIntegral (yLoc i) :: CInt))
--                                        (V2 (fromIntegral ((xLoc i) + 10) :: CInt) (fromIntegral ((yLoc i) + 10) :: CInt))
--                     ) $ VS.convert (V.filter (== True) gfx)
      where
        xLoc i = i -- (i * 10) `mod` 640
        yLoc i = i -- (i `div` 64) + 10

runCPU :: CPU -> CPU
runCPU cpu = handleOpCode cpu $ memoryPair (view memory cpu) (fromIntegral (view pc cpu))

memoryPair :: V.Vector Word8 -> Int -> Word16
memoryPair mem i = word8to16 (mem V.! i) (mem V.! (i+1)) 
