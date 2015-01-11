module Font
    where

import Types
import Linear hiding (trace)
import Prelude hiding (init)
import Triangulation.KET as Ket
import Triangulation.Common
import Data.List
import qualified Data.Vector.Unboxed as UV

-- | Ephemeral types for creating polygons from font outlines.
-- Fonty gives us a [[Vector (Float, Float)]] for an entire string, which breaks down to
type Contours = [Bezier Float]
type Fill = [Triangle Float]
type CharacterOutline = [Contours]
type CharacterFill = [Fill]
type StringOutline = [CharacterOutline]
type StringFill = [CharacterFill]

-- | Merges poly a into poly b by "cutting" a and inserting b.
cutMerge :: Poly -> Poly -> Poly
cutMerge as bs = (take (ndx + 1) as) ++ bs ++ [head bs] ++ (drop ndx as)
    where (ndx, _) = head $ sortBy (\a b -> snd a `compare` snd b) $
                         zip [0..] $ map (`distance` (head bs)) as

toBeziers :: (Ord a, Fractional a) => [V2 a] -> [Bezier a]
toBeziers (a:b:c:ps) = Bezier (triangleArea a b c) a b c : toBeziers (c:ps)
toBeziers _ = []

fromFonty :: (UV.Unbox b1, Functor f1, Functor f) => ([V2 b1] -> b) -> f (f1 (UV.Vector (b1, b1))) -> f (f1 b)
fromFonty f = fmap $ fmap $ f . UV.toList . UV.map (uncurry V2)

beziers :: [[UV.Vector (Float, Float)]] -> StringOutline
beziers = fromFonty toBeziers

triangles :: [[UV.Vector (Float, Float)]] -> StringFill
triangles = map (map (toTris . onContourPoints)) . beziers

-- | Turns a polygon into a list of triangles that can be rendered using the
-- Concave Polygon Stencil Test
-- @see http://www.glprogramming.com/red/chapter14.html#name13
concaveTriangles :: [V2 a] -> [Triangle a]
concaveTriangles [] = []
concaveTriangles (a:as) = tris a as
    where tris p (p':p'':ps) = Triangle p p' p'' : tris p (p'':ps)
          tris _ _ = []

onContourPoints :: (Eq a, Ord a, Num a) => [Bezier a] -> [V2 a]
onContourPoints [] = []
onContourPoints ((Bezier t a b c):bs) = ps ++ onContourPoints bs
    where ps = if t < 0 then [a,b,c] else [a,c]

dedupe :: Eq a => [a] -> [a]
dedupe [] = []
dedupe [a] = [a]
dedupe (a:b:cs) = if a == b then a: dedupe cs else a: (dedupe $ b:cs)

headLast :: Eq t => [t] -> [t]
headLast [] = []
headLast xs = if head xs == last xs then init xs else xs

windCCW :: (Ord a, Num a) => [V2 a] -> [V2 a]
windCCW xs = if signedArea xs > 0 then reverse xs else xs

toTris :: [V2 Float] -> [Triangle Float]
toTris = Ket.triangulate

