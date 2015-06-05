{-|
Module      : Data.Relational.Universe
Description : InUniverse typeclass and related classes.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Data.Relational.Universe (

    InUniverse(..)

  , allToUniverse
  , allFromUniverse
  , tagWithColumns
  , convertToRow

  ) where

import Control.Applicative
import Data.Proxy
import Data.Functor.Identity
import Data.Relational

-- | Associate some type @s@ with a universe @u@ by providing an injection
--   from @s@ into @u s@. This must really be an injection, so that
--
--     @
--       fromUniverse proxyS . toUniverse proxyU = Just
--     @
--
--   The associated type @UniverseType u s@ and @toUniverseAssociated@
--   allow for another way to create something of type @u s@. There must be
--   a bijection between @UniverseType u s@ and @u s@. 
--
--     @
--       s  |--->  u s  <--->  UniverseType u s
--     @
--
--   This gives an injection from @s@ into @UniverseType u s@, namely
--   @fromUniverseAssociated . toUniverse proxyU@ with inverse
--   @fromUniverse proxyS . toUniverseAssociated proxyU@.
--
--   Why bother? Because we can use @u@ with the type family @Fmap@ in
--   expressions like @HList (Fmap u types)@, as seen in the function
--   @allToUniverse@. Functions with domain @forall t . u t@ can then be
--   used on every element of the list.
class InUniverse (u :: * -> *) (s :: *) where
  toUniverse :: Proxy u -> s -> u s
  fromUniverse :: Proxy s -> u s -> Maybe s
  type UniverseType u s :: *
  toUniverseAssociated :: Proxy u -> UniverseType u s -> u s
  fromUniverseAssociated :: u s -> UniverseType u s

allToUniverse
  :: forall universe db types .
     ( TypeList types
     , Every (InUniverse universe) (Snds (Concat (Snds db)))
     , Contains (Snds (Concat (Snds db))) types
     )
  => Proxy universe
  -> Proxy db
  -> HList types
  -> HList (Fmap universe types)
allToUniverse proxyU _ hlist =
    case containsConstraint proxyC proxyContainer proxyContained of
        EveryConstraint ->
            typeListMap
              proxyU
              proxyG
              proxyC
              mapper
              constructor
              destructor
              hlist
              HNil
  where
    proxyC :: Proxy (InUniverse universe)
    proxyC = Proxy
    proxyG :: Proxy Identity
    proxyG = Proxy
    proxyContainer :: Proxy (Snds (Concat (Snds db)))
    proxyContainer = Proxy
    proxyContained :: Proxy types
    proxyContained = Proxy
    mapper :: InUniverse universe t => Identity t -> Identity (universe t)
    mapper = fmap (toUniverse proxyU)
    constructor :: forall t ts . Identity (universe t) -> HList ts -> HList ((universe t) ': ts)
    constructor x rest = (runIdentity x) :> rest
    destructor :: forall t ts . HList (t ': ts) -> (Identity t, HList ts)
    destructor hlst = case hlst of
        x :> rest -> (Identity x, rest)

-- This type is here for the benefit of allFromUniverse, which requires it
-- in order to use typeListUnmap.
newtype FromUniverse (ts :: [*]) = FromUniverse (Maybe (HList ts))

outFromUniverse :: FromUniverse ts -> Maybe (HList ts)
outFromUniverse (FromUniverse x) = x

allFromUniverse
  :: forall universe db types .
     ( TypeList types
     , Every (InUniverse universe) (Snds (Concat (Snds db)))
     , Contains (Snds (Concat (Snds db))) types
     )
  => Proxy universe
  -> Proxy db
  -> HList (Fmap universe types)
  -> Maybe (HList types)
allFromUniverse proxyU _ hlist =
    case containsConstraint proxyC proxyContainer proxyContained of
        EveryConstraint ->
            outFromUniverse
              ( typeListUnmap
                  proxyU
                  proxyMaybe
                  proxyC
                  unmapper
                  constructor
                  destructor
                  hlist
                  (FromUniverse (Just HNil))
              )
  where
    proxyC :: Proxy (InUniverse universe)
    proxyC = Proxy
    proxyMaybe :: Proxy Maybe
    proxyMaybe = Proxy
    proxyContainer :: Proxy (Snds (Concat (Snds db)))
    proxyContainer = Proxy
    proxyContained :: Proxy types
    proxyContained = Proxy
    unmapper :: forall universe t . InUniverse universe t => Maybe (universe t) -> Maybe t
    unmapper = (=<<) (fromUniverse (Proxy :: Proxy t))
    constructor :: forall t ts . Maybe t -> FromUniverse ts -> FromUniverse (t ': ts)
    constructor mx (FromUniverse mrest) = FromUniverse (ConsHList <$> mx <*> mrest)
    destructor
      :: forall t ts .
         Proxy ts
      -> HList ((universe t) ': (Fmap universe ts))
      -> (Maybe (universe t), HList (Fmap universe ts))
    destructor _ hlst = case hlst of
        x :> rest -> (Just x, rest)

tagWithColumns
  :: Project projected
  -> HList (Snds projected)
  -> Row projected
tagWithColumns project hlist = case (project, hlist) of
    -- This pattern match is exhaustive, because
    --   length x = length (Snds x)
    (EndProject, HNil) -> EndRow
    (col :+| prest, h :> hrest) ->
        (fromColumnAndValue col h) :&| (tagWithColumns prest hrest)

convertToRow
  :: ( Every (InUniverse universe) (Snds (Concat (Snds db)))
     , Contains (Snds (Concat (Snds db))) (Snds projected)
     , TypeList (Snds projected)
     )
  => Proxy universe
  -> Proxy db
  -> Project projected
  -> HList (Fmap universe (Snds projected))
  -> Maybe (Row projected)
convertToRow proxyU proxyDB proj =
    fmap (tagWithColumns proj) . allFromUniverse proxyU proxyDB
