module Draw where


import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Time.Clock.POSIX
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Data.IORef
import Data.Time.Clock

import GameState

draw :: SDL.Surface -> GameState -> IO ()
draw screen state = do
  let (x,y) = playerPos state

  black <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 0 0 
  white <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 
  white <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 
  blue <- (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200

  SDL.fillRect screen Nothing black
  SDL.fillRect screen (Just $ SDL.Rect (round x) (round y) 50 50) white
  
  aaPolygon screen [(300,300), (320,300),
                    (320,320), (300,320)] white

  SDL.flip screen
  

  return ()
