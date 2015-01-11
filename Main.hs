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
import Font
import Collision
import Linear
import Prelude hiding (init)
import Graphics.UI.GLFW
import Graphics.Text.TrueType
import Triangulation.EarClipping as Ear
import Triangulation.KET as Ket
import Data.Time.Clock
import Data.Maybe
import Data.Typeable
import Data.Monoid
import Control.Lens
import Control.Concurrent
import Control.Applicative
import Control.Monad
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Eff.Reader.Strict
import System.IO
import System.Exit
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Foldable as F

type FontCacheVar = MVar FontCache
deriving instance Typeable FontCache
deriving instance Show RawGlyph


emptyTransform :: Transform
emptyTransform = Transform zero (V2 1 1) 0

renderTransform :: BoxRenderer -> Transform -> IO ()
renderTransform (Box rnd) t = render rnd $ t

arial :: FontDescriptor
arial = FontDescriptor "Arial" $ FontStyle False False

ubuntuMono :: FontDescriptor
ubuntuMono = FontDescriptor "Ubuntu Mono" $ FontStyle False False

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

    fontCacheVar <- newMVar emptyFontCache
    _ <- forkFinally (loadFontsInto fontCacheVar) print
    let globals = Globals utc window

    runLift $ evalState globals
            $ evalState (IM.empty :: IM.IntMap Transform)
            $ evalState (IM.empty :: IM.IntMap Body)
            $ evalState (IM.empty :: IM.IntMap Name)
            $ evalState (IM.empty :: IM.IntMap Display)
            $ evalState (IM.empty :: IM.IntMap Color)
            $ evalState (IM.empty :: RenderCache)
            $ evalState (M.empty  :: M.Map Name ID)
            $ evalState (M.empty  :: RenderSources)
            $ flip runReader fontCacheVar
            $ flip runFresh (ID 0)
            $ start >> loop

loadFontsInto :: FontCacheVar -> IO ()
loadFontsInto fontCacheVar = do
    putStrLn "Loading fonts..."
    fontCache <- buildCache
    putStrLn $ show $ enumerateFonts fontCache
    _ <- takeMVar fontCacheVar
    putMVar fontCacheVar fontCache
    putStrLn "Loaded fonts!"


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
         , Member (State (M.Map Name ID)) r
         , Member (State Globals) r
         , Member (State RenderCache) r
         , Member (Reader FontCacheVar) r
         , Member (State RenderSources) r)
      => Eff r ()
start = do
    entity ## Transform (V2 0 280) (V2 1 1) 0
           ## Color (V4 1 1 1 1)
           .# DisplayAsText ubuntuMono 300 12 "Hello there!"

withFont desc f = do
    fvar <- ask
    cache <- lift $ takeMVar fvar
    case findFontInCache cache desc of
        Nothing -> do lift $ --do putStrLn $ "Could not find " ++ (show $ _descriptorFamilyName desc)
                             --                                ++ " in the font cache."
                             --   print $ enumerateFonts cache
                                return ()

        Just fp -> do
            ef  <- lift $ loadFontFile fp
            case ef of
                Left err   -> lift $ putStrLn err
                Right font -> f font
    lift $ putMVar fvar cache


stepBodies :: TimeDelta -> IM.IntMap Body -> IM.IntMap Body
stepBodies dt = fmap updateAll
    where updateAll body = body & bodyPosition_ +~ (dt *^ (body ^. bodyVelocity_))
                                & bodyVelocity_ +~ (dt *^ (body ^. bodyAcceleration_))

wrapBodies :: Float -> Float -> IM.IntMap Body -> IM.IntMap Body
wrapBodies w h = fmap (& bodyPosition_ %~ (\(V2 x y) -> V2 (wrp (-w/2,w/2) x) (wrp (-h/2,h/2) y)))


collideBodyInto :: Body -> Body -> Maybe (V2 Float)
collideBodyInto a b = toAABB a `collidedInto` toAABB b
    where toAABB (Body p _ _ hs) = AABB p hs

loop :: ( SetMember Lift (Lift IO) r
        , Member (Fresh ID) r
        , Member (State Globals) r
        , Member (State (IM.IntMap Name)) r
        , Member (State (IM.IntMap Transform)) r
        , Member (State (IM.IntMap Body)) r
        , Member (State (M.Map Name ID)) r
        , Member (State (IM.IntMap Display)) r
        , Member (State (IM.IntMap Color)) r
        , Member (Reader FontCacheVar) r
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

    let dt = realToFrac $ diffUTCTime t utc
    --    (lx',ly',rx',ry') = case fmap (map (zeroOut . realToFrac)) axes of
    --                            Just (lx:ly:rx:ry:_) -> (lx,ly,rx,ry)
    --                            _ -> (0,0,0,0)

    -- Update the left and right paddles
    --Just (ID lpaddle) <- getEntityBy $ Name "leftPaddle"
    --Just (ID rpaddle) <- getEntityBy $ Name "rightPaddle"
    --let vlp = 200 *^ V2 lx' (-ly')
    --    vrp = 200 *^ V2 rx' (-ry')
    --modify $ IM.adjust (& bodyVelocity_ .~ vlp) lpaddle
    --modify $ IM.adjust (& bodyVelocity_ .~ vrp) rpaddle


    (bs :: IM.IntMap Body) <- get
    let bs' = wrapBodies ww wh $ stepBodies dt bs
    (ts :: IM.IntMap Transform) <- get
    let tu  = IM.intersectionWith (\tfrm body -> tfrm & tfrmTranslation_ .~ bodyPosition body)
                                  ts bs'
        ts' = tu `IM.union` ts

    -- Update the ball
    --Just (ID ball) <- getEntityBy $ Name "ball"
    --let Just ballBody = IM.lookup ball bs'
    --    rev n = if abs n > 0 then -1.0 else 1
    --    ballBody' = if null ballCollisions then ballBody
    --                else ballBody & bodyVelocity_ *~ (fmap rev $ head ballCollisions)
    --                              & bodyPosition_ -~ head ballCollisions
    --    ballCollisions = catMaybes $ map (\(_, b) -> collideBodyInto b ballBody) $
    --                         (filter ((/= ball) . fst) $ IM.toList bs')

    --put $ IM.insert ball ballBody' bs'
    put ts'

    lift $ do
        drawClear window

    ds <- get
    cs <- fmap unColor <$> get
    let draws = intersectionWithKey3 drawThing cs ds ts'
    F.sequence_ draws

    lift $ stepOut window
    loop

drawWithCache :: ( Member (State (IM.IntMap Renderer)) r
                 , SetMember Lift (Lift IO) r)
              => Int -> Eff r Renderer -> Transform -> Eff r ()
drawWithCache uid f tfrm = do
    rcache <- get
    draw <- case IM.lookup uid rcache of
        Just r -> return r
        Nothing -> do r <- f
                      (ID uid) `hasProperty` r
                      return r
    lift $ (render draw) tfrm

textDraw color font dpi psize txt = do
    let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
        bs = beziers cs
        bs' = concat $ concat bs
        ts = concatMap (concatMap (concaveTriangles . onContourPoints)) bs
    window <- gWindow <$> get
    newPolyRenderer window color bs' ts

drawThing :: ( SetMember Lift (Lift  IO) r
             , Member (State RenderCache) r
             , Member (State RenderSources) r
             , Member (State Globals) r
             , Member (Reader FontCacheVar) r)
          => Int -> V4 Float -> Display -> Transform -> Eff r ()
drawThing uid color (DisplayAsTris ts) = drawWithCache uid $ do
    window <- gWindow <$> get
    newTriRenderer window color ts
drawThing uid color (DisplayAsPoly vs) = drawWithCache uid $ do
    window <- gWindow <$> get
    newTriRenderer window color $ Ket.triangulate vs
drawThing uid color (DisplayAsLine vs) = drawWithCache uid $ do
    window <- gWindow <$> get
    newLineRenderer window color $ toLines vs
drawThing uid color (DisplayAsArrows vs) = drawWithCache uid $ do
    window <- gWindow <$> get
    newLineRenderer window color $ toArrows vs
drawThing uid color (DisplayAsText desc dpi psize txt) = \t ->
    withFont desc $ \font ->
        drawWithCache uid (textDraw color font dpi psize txt) t
drawThing uid color (DisplayAsText' desc dpi psize txt) = \t ->
    withFont desc $ \font ->
        flip (drawWithCache uid) t $ do
            txtr <- textDraw color font dpi psize txt
            let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
                bs = beziers cs
                as = concatMap (concatMap toArrows) $ map (map onContourPoints) bs
            window <- gWindow <$> get
            linr <- newLineRenderer window color as
            let r = txtr <> linr
            return r

toLines :: [V2 a] -> [Line a]
toLines (a:b:cs) = Line a b : (toLines $ b:cs)
toLines _ = []

toArrows :: Floating a => [V2 a] -> [Line a]
toArrows = concatMap toArrow . toLines
    where toArrow (Line a b) = [ Line a b
                               , Line (b - u*l + n * w) b
                               , Line (b - u*l + n * (-w)) b ]
            where n = signorm $ perp $ b - a
                  u = signorm $ b - a
                  l = 5 -- head length
                  w = 3 -- head width


stepOut window = do
    swapBuffers window
    shouldClose <- windowShouldClose window
    if shouldClose
    then exitSuccess
    else threadDelay 100

