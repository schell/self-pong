{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Linear
import Prelude hiding (init)
import Graphics.UI.GLFW
import Graphics.GL.Types
import Graphics.Text.TrueType
import Data.Time.Clock
import Data.Monoid
import Control.Lens
import Data.Typeable
import qualified Data.IntMap as IM
import qualified Data.Map as M

type ShaderProgram = GLuint
type UniformLocation = GLint

data Display = DisplayAsTris [Triangle Float]
             | DisplayAsPoly [V2 Float]
             | DisplayAsText FontDescriptor Dpi PointSize String
             | DisplayAsText' FontDescriptor Dpi PointSize String -- ^ With testing visuals
             | DisplayAsLine [V2 Float]
             | DisplayAsArrows [V2 Float]
             deriving (Show, Typeable)


type RenderCache = IM.IntMap Renderer

data RenderDef = RenderDef { rdShaders :: [(String, GLuint)] -- ^ ie [("path/to/shader.vert", GL_VERTEX_SHADER), ..]
                           , rdUniforms :: [String] -- ^ ie ["projection", "modelview", ..]
                           } deriving (Show, Eq, Ord, Typeable)

data RenderSource = RenderSource { rsProgram    :: ShaderProgram
                                 , rsAttributes :: [(String, GLint)]
                                 } deriving (Show, Typeable)

type RenderSources = M.Map RenderDef RenderSource

type RenderFunction = Transform -> IO ()

type CleanupFunction = IO ()

data Renderer = Renderer { render  :: RenderFunction
                         , cleanup :: CleanupFunction
                         } deriving (Typeable)

instance Monoid Renderer where
    mempty = Renderer (const $ return ()) (return ())
    (Renderer ar ac) `mappend` (Renderer br bc) = Renderer (\t -> ar t >> br t) (ac >> bc)

newtype BoxRenderer = Box Renderer
newtype BezRenderer = Bez Renderer

data Bezier a = Bezier { bezTriArea :: a
                       , bezA :: V2 a
                       , bezB :: V2 a
                       , bezC :: V2 a
                       } deriving (Show, Typeable)

bezWoundClockwise :: (Ord a, Num a) => Bezier a -> Bool
bezWoundClockwise = (< 0) . bezTriArea

data Triangle a = Triangle (V2 a) (V2 a) (V2 a) deriving (Show)
data Line a = Line (V2 a) (V2 a) deriving (Show)
newtype Color = Color { unColor :: V4 Float } deriving (Typeable)

newtype ID = ID { unID :: Int } deriving (Enum, Ord, Eq, Typeable)
newtype Name = Name { unName :: String } deriving (Ord, Eq, Typeable)
newtype ParentEntity = Parent { unParent :: Int } deriving (Show, Typeable)
type Acceleration = V2 Float
type Velocity = V2 Float
type Translation = V2 Float
type Position = V2 Float
type Scale = V2 Float
type Rotation = Float
type Size = V2 Float
type TimeDelta = Float
data Transform = Transform { tfrmTranslation :: Position
                           , tfrmScale       :: Scale
                           , tfrmRotation    :: Rotation
                           } deriving (Show, Typeable)
makeLensesFor [("tfrmTranslation", "tfrmTranslation_")
              ,("tfrmScale", "tfrmScale_")
              ,("tfrmRotation", "tfrmRotation_")
              ] ''Transform

instance Monoid Transform where
    mempty = Transform zero (V2 1 1) 0
    (Transform t1 s1 r1) `mappend` (Transform t2 s2 r2) = Transform (t1 + t2) (s1 * s2) (r1 + r2)

data AABB = AABB { aabbCenter     :: Position
                 , aabbHalfVector :: Size
                 } deriving (Show, Eq, Ord, Typeable)
makeLensesFor [("aabbCenter", "aabbCenter_")
              ,("aabbHalfVector", "aabbHalfVector_")
              ] ''AABB

data Body = Body { bodyPosition     :: Position
                 , bodyVelocity     :: Velocity
                 , bodyAcceleration :: Acceleration
                 , bodyHalfSize     :: Size
                 } deriving (Typeable)
makeLensesFor [("bodyPosition", "bodyPosition_")
              ,("bodyVelocity", "bodyVelocity_")
              ,("bodyAcceleration", "bodyAcceleration_")
              ,("bodyHalfSize", "bodyHalfSize_")
              ] ''Body

data Globals = Globals { gUTC          :: UTCTime
                       , gWindow       :: Window
                       } deriving (Typeable)
makeLensesFor [("gUTC", "gUTC_")
              ,("gBox", "gBox_")
              ,("gBez", "gBez_")
              ,("gWindow", "gWindow_")
              ] ''Globals

data Input = Input { inputLeftAxis    :: (Float, Float)
                   , inputRightAxis   :: (Float, Float)
                   , inputWindowSizef :: (Float, Float)
                   } deriving (Show, Eq)


type SeparatingAxis = V2 Float
