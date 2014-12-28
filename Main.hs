{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import Types
import Entity
import Renderer
import Collision
import Linear
import Prelude hiding (init)
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import Data.Time.Clock
import Data.Maybe
import Data.Typeable
import Control.Lens
import Control.Concurrent
import Control.Applicative
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import System.IO
import System.Exit
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Foldable as F

deriving instance Typeable FontCache
deriving instance Show RawGlyph

emptyTransform :: Transform
emptyTransform = Transform zero (V2 1 1) 0

renderTransform :: BoxRenderer -> Transform -> IO ()
renderTransform (Box rnd) t = render rnd $ t

arial :: FontDescriptor
arial = FontDescriptor "Arial" $ FontStyle False False

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
    boxr <- newBoxRenderer window
    fontCache <- buildCache
    bs <- case findFontInCache fontCache arial of
        Nothing -> do putStrLn "Could not find the font 'Arial'."
                      exitFailure
        Just f  -> do efont <- loadFontFile f
                      case efont of
                          Left err   -> putStrLn err >> exitFailure
                          Right font -> do let cm = getCharacterGlyphsAndMetrics font '0'
                                               bs = fontyToBeziers $ getStringCurveAtPoint 300 (0,0) [(font, 12, "0")]
                                           print cm
                                           return bs

    bezr <- newBezRenderer window bs

    let globals = Globals utc boxr bezr window

    runLift $ evalState globals
            $ evalState (IM.empty :: IM.IntMap Transform)
            $ evalState (IM.empty :: IM.IntMap Body)
            $ evalState (IM.empty :: IM.IntMap Name)
            $ evalState (IM.empty :: IM.IntMap Display)
            $ evalState (IM.empty :: RenderCache)
            $ evalState (M.empty  :: M.Map Name ID)
            $ flip runReader fontCache
            $ flip runFresh (ID 0)
            $ start >> loop

fontyToBeziers contours = concatMap bsFromChars contours
    where bsFromChars cs = concatMap bsFromChar cs
          bsFromChar v = toBeziers $ map toV2 $ V.toList v
          toV2 = uncurry V2
          toBeziers [a,b,c] = [Bezier (triangleArea a b c < 0) a b c]
          toBeziers (a:b:c:ps) = Bezier (triangleArea a b c < 0) a b c : toBeziers (c:ps)
          toBeziers _ = []

zeroOut :: (Ord a, Fractional a) => a -> a
zeroOut x = if abs x < 0.1 then 0 else x

wrp :: Ord a => (a,a) -> a -> a
wrp (w,z) n = if n > z then w else if n < w then z else n

start :: (Member (Fresh ID) r, SetMember Lift (Lift IO) r, Member (State Globals) r
         ,Member (State (IM.IntMap Name)) r, Member (State (IM.IntMap Transform)) r
         ,Member (State (IM.IntMap Body)) r, Member (State (M.Map Name ID)) r
         ,Member (State (IM.IntMap Display)) r, Member (State RenderCache) r
         ,Member (Reader FontCache) r)
      => Eff r ()
start = do
    entity ## Transform (V2 150 150) (V2 10 10) 0
           ## Body (V2 150 150) (V2 100 50) zero (V2 5 5)
           ## DisplayAsBox
           `named` (Name "ball")

    entity ## Transform zero (V2 10 100) 0
           ## Body (V2 (-50) 0) zero zero (V2 5 50)
           ## DisplayAsBox
           `named` (Name "leftPaddle")

    entity ## Transform zero (V2 10 100) 0
           ## Body (V2 50 0) zero zero (V2 5 50)
           ## DisplayAsBox
           `named` (Name "rightPaddle")


    mfp <- (`findFontInCache` arial) <$> ask
    case mfp of
        Nothing -> lift $ putStrLn "Could not find 'Arial' in the font cache."
        Just fp -> do
            ef  <- lift $ loadFontFile fp
            case ef of
                Left err   -> lift $ putStrLn err
                Right font -> entity ## Transform (V2 0 20) (V2 1 1) 0
                                     .# DisplayAsText font 150 12 "0123456789\naoeusnth"

stepBodies :: TimeDelta -> IM.IntMap Body -> IM.IntMap Body
stepBodies dt = fmap updateAll
    where updateAll body = body & bodyPosition_ +~ (dt *^ (body ^. bodyVelocity_))
                                & bodyVelocity_ +~ (dt *^ (body ^. bodyAcceleration_))

wrapBodies :: Float -> Float -> IM.IntMap Body -> IM.IntMap Body
wrapBodies w h = fmap (& bodyPosition_ %~ (\(V2 x y) -> V2 (wrp (-w/2,w/2) x) (wrp (-h/2,h/2) y)))


collideBodyInto :: Body -> Body -> Maybe (V2 Float)
collideBodyInto a b = toAABB a `collidedInto` toAABB b
    where toAABB (Body p _ _ hs) = AABB p hs

loop :: (Member (Fresh ID) r, SetMember Lift (Lift IO) r, Member (State Globals) r
        ,Member (State (IM.IntMap Name)) r, Member (State (IM.IntMap Transform)) r
        ,Member (State (IM.IntMap Body)) r, Member (State (M.Map Name ID)) r
        ,Member (State (IM.IntMap Display)) r, Member (State RenderCache) r)
     => Eff r ()
loop = do
    Box boxr <- gBox <$> get
    Bez bezr <- gBez <$> get
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

    let dt = realToFrac $ diffUTCTime t utc
        (lx',ly',rx',ry') = case fmap (map (zeroOut . realToFrac)) axes of
                                Just (lx':ly':rx':ry':_) -> (lx',ly',rx',ry')
                                Nothing -> (0,0,0,0)

    -- Update the left and right paddles
    Just (ID lpaddle) <- getEntityBy $ Name "leftPaddle"
    Just (ID rpaddle) <- getEntityBy $ Name "rightPaddle"
    let vlp = 200 *^ V2 lx' (-ly')
        vrp = 200 *^ V2 rx' (-ry')
    modify $ IM.adjust (& bodyVelocity_ .~ vlp) lpaddle
    modify $ IM.adjust (& bodyVelocity_ .~ vrp) rpaddle


    (bs :: IM.IntMap Body) <- get
    let bs' = wrapBodies ww wh $ stepBodies dt bs
    (ts :: IM.IntMap Transform) <- get
    let tu  = IM.intersectionWith (\tfrm body -> tfrm & tfrmTranslation_ .~ bodyPosition body)
                                  ts bs'
        ts' = tu `IM.union` ts

    -- Update the ball
    Just (ID ball) <- getEntityBy $ Name "ball"
    let Just ballBody = IM.lookup ball bs'
        rev n = if abs n > 0 then -1.0 else 1
        ballBody' = if null ballCollisions then ballBody
                    else ballBody & bodyVelocity_ *~ (fmap rev $ head ballCollisions)
                                  & bodyPosition_ -~ head ballCollisions
        ballCollisions = catMaybes $ map (\(_, b) -> collideBodyInto b ballBody) $
                             (filter ((/= ball) . fst) $ IM.toList bs')

    put $ IM.insert ball ballBody' bs'
    put ts'

    lift $ do
        drawClear window
        (render bezr) $ Transform (V2 (-150) (150)) (V2 20 20) 0

    ds <- get
    let draws = IM.intersectionWithKey drawThing ds ts'
    F.sequence_ draws

    lift $ stepOut window
    loop

drawThing :: (Member (State RenderCache) r, SetMember Lift (Lift  IO) r
             ,Member (State Globals) r)
          => Int -> Display -> Transform -> Eff r ()
drawThing _ DisplayAsBox tfrm = do
    Box r <- gBox <$> get
    lift $ render r $ tfrm
drawThing uid (DisplayAsText font dpi psize txt) tfrm = do
    rcache <- get
    draw <- case IM.lookup uid rcache of
        Just r  -> return r
        Nothing -> do let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
                          bs = fontyToBeziers cs
                          vs = onContourPoints bs

                      window <- gWindow <$> get
                      Box boxr <- gBox <$> get
                      Bez bezr <- lift $ newBezRenderer window bs
                      let r = Renderer rendf clnup
                          rendf t@(Transform txy sxy r) = do (render bezr) t
                                                             flip mapM_ vs $ \v ->
                                                                (render boxr) $ Transform (txy ^+^ v)
                                                                                          sxy
                                                                                          r
                          clnup = mapM_ cleanup [boxr, bezr]
                      (ID uid) `hasProperty` r
                      return r
    lift $ (render draw) tfrm

stepOut window = do
    swapBuffers window
    shouldClose <- windowShouldClose window
    if shouldClose
    then exitSuccess
    else threadDelay 100

