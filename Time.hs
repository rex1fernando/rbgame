module Time where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Time.Clock.POSIX
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Data.IORef
import Data.Time.Clock


type Duration = NominalDiffTime
type Time     = NominalDiffTime
type TimeInterval = (Time, Time)

delta :: TimeInterval -> Duration
delta (x,y) = y-x

-- timing helpers
ms :: NominalDiffTime
ms = 1
fps :: Integer
fps  = 90                     -- physics framerate
dt :: NominalDiffTime
dt   = (1.0 / (fromIntegral fps)) * ms  -- physics timestep
