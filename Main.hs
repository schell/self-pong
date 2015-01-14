{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Types
import Entity
import Renderer
import Font
import Collision
import Linear
import Prelude hiding (init)
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import Data.Time.Clock
import Data.Maybe
import Data.Monoid
import Control.Lens
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Control.Applicative
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import System.IO
import System.Exit
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Foldable as F

renderTransform :: BoxRenderer -> Transform -> IO ()
renderTransform (Box rnd) t = render rnd $ t

main :: IO ()
main = do
    setErrorCallback $ Just $ \_ -> hPutStrLn stderr
    True <- init
    defaultWindowHints
    windowHint $ WindowHint'OpenGLDebugContext True
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'OpenGLForwardCompat True
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 2
    windowHint $ WindowHint'DepthBits 16
    mwin <- createWindow 300 300 "self pong" Nothing Nothing
    makeContextCurrent mwin

    window <- case mwin of
                  Nothing  -> do putStrLn "could not create window"
                                 exitFailure
                  Just win -> return win
    enableBlending

    utc  <- getCurrentTime

    aCache <- async $ do putStrLn "Loading fonts..."
                         cache <- buildCache
                         putStrLn "Loaded fonts!"
                         return cache

    let globals = Globals utc window

    runLift $ evalState globals
            $ evalState (IM.empty :: IM.IntMap Transform)
            $ evalState (IM.empty :: IM.IntMap Body)
            $ evalState (IM.empty :: IM.IntMap Name)
            $ evalState (IM.empty :: IM.IntMap Display)
            $ evalState (IM.empty :: IM.IntMap Color)
            $ evalState (IM.empty :: IM.IntMap ParentEntity)
            $ evalState (IM.empty :: RenderCache)
            $ evalState (M.empty  :: M.Map Name ID)
            $ evalState (M.empty  :: RenderSources)
            $ flip runReader aCache
            $ flip runFresh (ID 0)
            $ start >> loop

zeroOut :: (Ord a, Fractional a) => a -> a
zeroOut x = if abs x < 0.1 then 0 else x

wrp :: Ord a => (a,a) -> a -> a
wrp (w,z) n = if n > z then w else if n < w then z else n

start :: ( Member (Fresh ID) r
         , SetMember Lift (Lift IO) r
         , Member (State (IM.IntMap Name)) r
         , Member (State (IM.IntMap Transform)) r
         , Member (State (IM.IntMap Body)) r
         , Member (State (IM.IntMap Display)) r
         , Member (State (IM.IntMap Color)) r
         , Member (State (IM.IntMap ParentEntity)) r
         , Member (State (M.Map Name ID)) r
         , Member (State Globals) r
         , Member (State RenderCache) r
         , Member (Reader (Async FontCache)) r
         , Member (State RenderSources) r)
      => Eff r ()
start = do
    entity ## (mempty & tfrmTranslation_ .~ V2 0 280)
           ## Color (V4 1 1 1 1)
           .# DisplayAsText ubuntuMono 300 12 "Hello there!"

    (ID button') <- entity ## (mempty & tfrmTranslation_ .~ V2 100 100)

    entity ## (mempty :: Transform)
           ## Color (V4 0.5 0.5 0.5 1)
           ## DisplayAsPoly [V2 0 0, V2 100 0, V2 100 50, V2 0 50]
           .# Parent button'

    entity ## (mempty & tfrmTranslation_ .~ (V2 0 50))
           ## Color (V4 1 1 1 1)
           ## DisplayAsText ubuntuMono 150 12 "Button"
           .# Parent button'


stepBodies :: TimeDelta -> IM.IntMap Body -> IM.IntMap Body
stepBodies dt = fmap updateAll
    where updateAll body = body & bodyPosition_ +~ (dt *^ (body ^. bodyVelocity_))
                                & bodyVelocity_ +~ (dt *^ (body ^. bodyAcceleration_))

wrapBodies :: Float -> Float -> IM.IntMap Body -> IM.IntMap Body
wrapBodies w h = fmap (& bodyPosition_ %~ (\(V2 x y) -> V2 (wrp (-w/2,w/2) x) (wrp (-h/2,h/2) y)))


collideBodyInto :: Body -> Body -> Maybe (V2 Float)
collideBodyInto a b = toAABB a `collidedInto` toAABB b
    where toAABB (Body p _ _ hs) = AABB p hs

transforms :: ( Member (State (IM.IntMap Transform)) r
              , Member (State (IM.IntMap ParentEntity)) r
              , SetMember Lift (Lift IO) r)
           => Eff r (IM.IntMap Transform)
transforms = do
    (ts :: IM.IntMap Transform) <- get
    parents <- get
    lift $ putStrLn "\n"
    lift $ print parents
    lift $ print ts
    let displayMap = fmap (\(Parent p) -> allParents parents p ++ [p]) parents :: IM.IntMap [Int]
        parentTfrms = fmap (foldr mappend mempty . catMaybes . fmap (flip IM.lookup ts)) displayMap :: IM.IntMap Transform
    lift $ print displayMap
    return $ IM.unionWith mappend ts parentTfrms

-- | Lists a branch of all parents, root first.
-- Beware, if a parent contains itself or another node that contains it we
-- will recurse forever!
allParents :: IM.IntMap ParentEntity -> Int -> [Int]
allParents parents uid =
    case IM.lookup uid parents of
        Nothing -> []
        Just (Parent uid') -> allParents parents uid' ++ [uid']

loop :: ( SetMember Lift (Lift IO) r
        , Member (Fresh ID) r
        , Member (State Globals) r
        , Member (State (IM.IntMap Name)) r
        , Member (State (IM.IntMap ParentEntity)) r
        , Member (State (IM.IntMap Transform)) r
        , Member (State (IM.IntMap Body)) r
        , Member (State (M.Map Name ID)) r
        , Member (State (IM.IntMap Display)) r
        , Member (State (IM.IntMap Color)) r
        , Member (Reader (Async FontCache)) r
        , Member (State RenderCache) r
        , Member (State RenderSources) r)
     => Eff r ()
loop = do
    window <- gWindow <$> get
    utc    <- gUTC <$> get

    (t, ww, wh, axes) <- lift $ do
        pollEvents
        (ww, wh) <- getWindowSize window
        t        <- getCurrentTime
        axes     <- getJoystickAxes Joystick'1
        drawClear window
        return $ (t, fromIntegral ww, fromIntegral wh, axes)

    modify $ gUTC_ .~ t

    tfrms <- transforms

    lift $ do
        drawClear window

    ds <- get
    cs <- fmap unColor <$> get
    let draws = intersectionWithKey3 drawThing cs ds tfrms
    F.sequence_ draws

    lift $ stepOut window
    loop


stepOut :: Window -> IO ()
stepOut window = do
    swapBuffers window
    shouldClose <- windowShouldClose window
    if shouldClose
    then exitSuccess
    else threadDelay 100

