module Font where

import Types
import Renderer
import Linear
import Control.Lens
import Prelude hiding (init)
import Triangulation.EarClipping as Ear
import Triangulation.KET as KET
import Triangulation.Common
import Data.List
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- | Ephemeral types for creating polygons from font outlines.
-- Fonty gives us a [[Vector (Float, Float)]] for an entire string, which breaks down to
type Contours = [Bezier Float]
type CharacterOutline = [Contours]
type StringOutline = [CharacterOutline]
data CharPoly = CharPoly Poly [CharPoly] deriving (Show)

-- Adds a polygon either by cutting and merging or by listing them
-- together.
addPoly :: CharPoly -> Poly -> CharPoly
addPoly (CharPoly as cs) bs = if bs `insidePoly` as
                                then CharPoly (cutMerge as bs) cs
                                else CharPoly as (map (`addPoly` bs) cs)

cutMerge :: Poly -> Poly -> Poly
cutMerge as bs = (take (ndx + 1) as) ++ bs ++ [head bs] ++ (drop ndx as)
    where (ndx, _) = last $ sortBy (\a b -> snd a `compare` snd b) $
                         zip [0..] $ map (`distance` (head bs)) as

instance Monoid CharPoly where
    mempty = CharPoly [] []
    cp `mappend` (CharPoly p cs) = foldl mappend (cp `addPoly` p) cs

charToPolys :: CharPoly -> [Poly]
charToPolys (CharPoly as cs) = as: concatMap charToPolys cs

charToTris :: CharPoly -> [Triangle Float]
charToTris = concat . map toTris . charToPolys

toBeziers :: (Ord a, Fractional a) => [V2 a] -> [Bezier a]
toBeziers (a:b:c:ps) = Bezier (triangleArea a b c) a b c : toBeziers (c:ps)
toBeziers _ = []

fromFonty :: (UV.Unbox b1, Functor f1, Functor f) => ([V2 b1] -> b) -> f (f1 (UV.Vector (b1, b1))) -> f (f1 b)
fromFonty f = fmap $ fmap $ f . UV.toList . UV.map (uncurry V2)

beziers :: [[UV.Vector (Float, Float)]] -> StringOutline
beziers = fromFonty toBeziers

triFills :: [[UV.Vector (Float, Float)]] -> [Triangle Float]
triFills = concatMap toTris . concatMap fillPolys . fromFonty toBeziers

fillPolys :: CharacterOutline -> [Poly]
fillPolys cs = map onContourPoints cs
fillPolys cs = case polys of
                   [] -> []
                   p:ps -> charToPolys $ foldl addPoly (CharPoly p []) ps
    where polys = map onContourPoints cs

onContourPoints :: (Eq a, Ord a, Num a) => [Bezier a] -> [V2 a]
onContourPoints = headLast . dedupe . go
    where go [] = []
          go ((Bezier t a b c):bs) = ps ++ onContourPoints bs
                                        where ps = if t < 0 then [a,b,c] else [a,c]
          dedupe [] = []
          dedupe [a] = [a]
          dedupe (a:b:cs) = if a == b then a: dedupe cs else a: (dedupe $ b:cs)

          headLast [] = []
          headLast xs = if head xs == last xs then init xs else xs

toTris :: [V2 Float] -> [Triangle Float]
toTris = Ear.triangulate

