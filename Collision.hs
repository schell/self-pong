module Collision where

import Prelude hiding (sequence_, minimum)
import Types
import Linear
import Data.Foldable
import Control.Applicative
import Control.Lens

aabbAxes :: [SeparatingAxis]
aabbAxes =
    [ V2 1 0
    , V2 0 1
    ]

aabbPoints :: AABB -> [V2 Float]
aabbPoints (AABB (V2 x y) (V2 hw hh)) = [V2 l t, V2 r t, V2 l b, V2 r b]
    where (l,t,r,b) = (x - hw, y - hh, x + hw, y + hh)

aabbProjectionRange :: AABB -> V2 Float -> (Float, Float)
aabbProjectionRange aabb axis = range
    where range = Prelude.foldl (\(n,x) p' -> (min n p', max x p'))
                                (1/0, -(1/0))
                                prjs
          ps    = aabbPoints aabb
          prjs  = map (`dot` axis) ps

collidesWithRange :: (Float, Float) -> (Float, Float) -> Bool
collidesWithRange (a,b) (c,d) = not (d <= a || b <= c)

collideOnAxis :: AABB -> AABB -> V2 Float -> Maybe (V2 Float)
collideOnAxis a b axis = if col then Just $ u ^* v else Nothing
    where rA@(n1, n2) = aabbProjectionRange a axis
          rB@(m1, m2) = aabbProjectionRange b axis
          v = min (m2 - n1) (n2 - m1)
          d = (a^.aabbCenter_) ^-^ (b^.aabbCenter_)
          s = (\n -> if n >= 0 then 1 else (-1)) <$> d
          u = s * signorm axis
          col = collidesWithRange rA rB

-- | Hit test two AABBs against each other. If a collision occurs, return
-- `Just` the minimum vector to separate them, else return `Nothing`.
collidedInto :: AABB -> AABB -> Maybe (V2 Float)
collidedInto a b = (minimumBy dv) <$> (sequence axisOverlaps)
    where axisOverlaps = map (collideOnAxis a b) aabbAxes
          dv v1 v2 = compare (norm v1) (norm v2)

