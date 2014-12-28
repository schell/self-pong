{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entity where

import Prelude hiding (init)
import Control.Eff
import Control.Eff.Fresh
import Control.Eff.State.Strict
import Data.Typeable
import qualified Data.IntMap as IM
import qualified Data.Map as M

newtype ID = ID { unID :: Int } deriving (Enum, Ord, Eq, Typeable)

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

