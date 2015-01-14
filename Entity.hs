{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Entity where

import Types
import Prelude hiding (init)
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.State.Strict
import Data.Typeable
import qualified Data.IntMap as IM
import qualified Data.Map as M

hasProperty :: (Member (State (IM.IntMap a)) r, Typeable a) => ID -> a -> Eff r ()
hasProperty eid val = modify $ IM.insert (unID eid) val

entity :: (Member (Fresh a) r, Enum a, Typeable a) => Eff r a
entity = fresh

(##) :: (Member (State (IM.IntMap a)) r, Typeable a) => Eff r ID -> a -> Eff r ID
f ## prop = do
   eid <- f
   modify $ IM.insert (unID eid) prop
   return eid

(.#) :: (Member (State (IM.IntMap a)) r, Typeable a) => Eff r ID -> a -> Eff r ()
f .# prop = do
   eid <- f
   modify $ IM.insert (unID eid) prop
   return ()

named :: (Member (State (IM.IntMap k)) r, Member (State (M.Map k ID)) r, Typeable k, Ord k) => Eff r ID -> k -> Eff r ()
named f n = do
    eid <- f
    modify $ IM.insert (unID eid) n
    modify $ M.insert n eid
    return ()

getEntityBy :: (Member (State (M.Map k a)) r, Typeable a, Typeable k, Ord k) => k -> Eff r (Maybe a)
getEntityBy name = do
    m <- get
    return $ M.lookup name m

--------------------------------------------------------------------------------
-- Intersections
--------------------------------------------------------------------------------
intersectionWith3 f a b c = IM.intersectionWith ($) (IM.intersectionWith f a b) c
intersectionWith4 f a b c d = IM.intersectionWith ($) (intersectionWith3 f a b c) d
intersectionWith5 f a b c d e = IM.intersectionWith ($) (intersectionWith4 f a b c d) e

intersectionWithKey3 f a b c = IM.intersectionWith ($) (IM.intersectionWithKey f a b) c
intersectionWithKey4 f a b c d = IM.intersectionWith ($) (intersectionWithKey3 f a b c) d
intersectionWithKey5 f a b c d e = IM.intersectionWith ($) (intersectionWithKey4 f a b c d) e
