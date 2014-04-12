{-# LANGUAGE NoMonomorphismRestriction, ExistentialQuantification, RankNTypes, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, EmptyDataDecls #-}
module Loop where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Time.Clock.POSIX
import Control.Monad.State
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Data.IORef
import Data.Time.Clock

import Time
 
type GameNetworkDescription t
    =  Event t ()      -- ^ physics timer
    -> Behavior t Time -- ^ clock (synchronized with physics and user input)
    -> Event t SDL.Event   -- ^ user input
    -> Moment t (Behavior t (IO ())) -- ^ graphics to be sampled
 
gameLoop
    :: 
       Duration -- ^ physics time step
    -> Double   -- ^ maximal frames per second
    -> (forall t . Frameworks t => GameNetworkDescription t) -- ^ event network corresponding to the game
    -> IO ()
gameLoop dt maximalFps gameNetwork = do
    -- set up event network
    (ahInput   , fireInput)    <- newAddHandler
    (ahPhysics , firePhysics)  <- newAddHandler
    (ahGraphics, fireGraphics) <- newAddHandler
    clock <- newIORef 0
    
    network <- compile $ do
        eInput    <- fromAddHandler ahInput
        ePhysics  <- fromAddHandler ahPhysics
        bTime     <- fromPoll (readIORef clock)
        eGraphics <- fromAddHandler ahGraphics
        bGraphics <- gameNetwork ePhysics bTime eInput
        reactimate $ bGraphics <@ eGraphics
    actuate network
    
    -- game loop
    firstOld <- getPOSIXTime
    go clock 0 firstOld fireInput firePhysics fireGraphics
    
    where
    go clock acc old fireInput firePhysics fireGraphics = do
        -- acc  accumulates excess time (usually < dt)
        -- old  keeps track of the time of the previous iteration of the game loop
        input <- SDL.pollEvent
        unless (input == SDL.Quit) $ do
            new <- getPOSIXTime 
 
            -- FIXME: set clock properly for user input
            fireInput input         -- handle user input
 
            -- "physics" simulation
            -- invariant: the world time begins at 0 and is always a multiple of dt
            let n = round $ (new - old + acc) / dt
            let acc2 = (new - old + acc) - ((fromIntegral n)*dt)
            replicateM_ (fromIntegral n) $ do
                modifyIORef clock (+dt)  -- update clock
                firePhysics ()           -- handle physics
 
            -- no need to hog all the CPU
            -- FIXME: something with maximalFPS
            SDL.delay $ round $ dt*(fromIntegral 1000)
 
            -- graphics
            -- note: time might *not* be multiple of dt, for interpolation
            tempclock <- readIORef clock    -- remember multiple of dt

            modifyIORef clock (+acc2)       -- advance clock slightly
            fireGraphics ()                 -- interpolate graphics
            writeIORef  clock tempclock     -- reset clock to multiple of dt
 
            go clock acc2 new fireInput firePhysics fireGraphics


 
