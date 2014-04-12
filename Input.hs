module Input where


import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Time.Clock.POSIX
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Data.IORef
import Data.Time.Clock
import CalcGeom

type ArrowKeys = Pos

arrowKeysB :: Event t SDL.Event -> Behavior t ArrowKeys
arrowKeysB input = (,) <$> (direction <$> leftDown <*> rightDown) <*> (direction <$> upDown <*> downDown)
  where

    direction False False = 0
    direction True False = -1
    direction False True = 1
    direction True True = 0

    keys = filterE isKeyEvent input
    leftDown = stepper False $ keyDownB SDL.SDLK_LEFT keys
    rightDown = stepper False $ keyDownB SDL.SDLK_RIGHT keys
    upDown = stepper False $ keyDownB SDL.SDLK_UP keys
    downDown = stepper False $ keyDownB SDL.SDLK_DOWN keys


isKeyEvent (SDL.KeyDown _) = True
isKeyEvent (SDL.KeyUp _) = True
isKeyEvent _ = False

keyDownB k e = isDown <$> filterE (isAboutKey k) e
  where
    isAboutKey k (SDL.KeyUp (SDL.Keysym k' _ _)) | k == k' = True
    isAboutKey k (SDL.KeyDown (SDL.Keysym k' _ _)) | k == k' = True
    isAboutKey k _ = False

    isDown (SDL.KeyDown _) = True
    isDown (SDL.KeyUp _) = False
