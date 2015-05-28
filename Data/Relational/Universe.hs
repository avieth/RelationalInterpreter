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

  , AllToUniverse
  , allToUniverse

  , AllFromUniverse
  , allFromUniverse

  , ConvertToRow
  , convertToRow

  ) where

import GHC.TypeLits (Symbol)
import Control.Applicative
import Data.Proxy
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

-- It's important that we do not mention a universe here in the class head.
-- That allows us to use this constraint on the GADT constructors of
-- RelationalF.
class AllToUniverse (db :: [(Symbol, [(Symbol, *)])]) (types :: [*]) where
    allToUniverse
      :: forall universe .
         ( Every (InUniverse universe) (Snds (Concat (Snds db)))
         , Contains (Snds (Concat (Snds db))) types
         )
      => Proxy universe
      -> Proxy db
      -> HList types
      -> HList (Fmap universe types)

instance AllToUniverse db '[] where
    allToUniverse _ _ _ = HNil

instance (AllToUniverse db ts) => AllToUniverse db (t ': ts) where
    allToUniverse (proxyU :: Proxy universe) proxyDB lst = case lst of
        -- In order to do this we must produce two proofs:
        --   1. Every element of t ': ts is InUniverse, since everything in the
        --      db is InUniverse and db contains t ': ts.
        --   2. The tail of t ': ts is also contained by db, which allows us to
        --      recurse.
        x :> rest -> case containsConstraint proxyC proxyContainer proxyContained of
            EveryConstraint -> case tailContainsProof proxyContainer proxyContained of
                ContainsProof -> 
                     (elemConstraint proxyC proxyT proxyContained (toUniverse proxyU x))
                  :> (allToUniverse proxyU proxyDB rest)
          where
            proxyC :: Proxy (InUniverse universe)
            proxyC = Proxy
            proxyT :: Proxy t
            proxyT = Proxy
            proxyTS :: Proxy ts
            proxyTS = Proxy
            proxyContained :: Proxy (t ': ts)
            proxyContained = Proxy
            proxyContainer :: Proxy (Snds (Concat (Snds db)))
            proxyContainer = Proxy

class AllFromUniverse (db :: [(Symbol, [(Symbol, *)])]) (types :: [*]) where
    allFromUniverse
      :: forall universe .
         ( Every (InUniverse universe) (Snds (Concat (Snds db)))
         , Contains (Snds (Concat (Snds db))) types
         )
      => Proxy universe
      -> Proxy db
      -> HList (Fmap universe types)
      -> Maybe (HList types)

instance AllFromUniverse db '[] where
    allFromUniverse _ _ _ = pure HNil

instance (AllFromUniverse db ts) => AllFromUniverse db (t ': ts) where
    allFromUniverse (proxyU :: Proxy universe) proxyDB lst = case lst of
        x :> rest -> case containsConstraint proxyC proxyContainer proxyContained of
            EveryConstraint -> case tailContainsProof proxyContainer proxyContained of
                ContainsProof ->
                        (:>)
                    <$> (elemConstraint proxyC proxyT proxyContained (fromUniverse proxyT x))
                    <*> (allFromUniverse proxyU proxyDB rest)
          where
            proxyC :: Proxy (InUniverse universe)
            proxyC = Proxy
            proxyT :: Proxy t
            proxyT = Proxy
            proxyContained :: Proxy (t ': ts)
            proxyContained = Proxy
            proxyContainer :: Proxy (Snds (Concat (Snds db)))
            proxyContainer = Proxy

class ConvertToRow (db :: [(Symbol, [(Symbol, *)])]) (projected :: [(Symbol, *)]) where
    convertToRow
      :: forall universe .
         ( Every (InUniverse universe) (Snds (Concat (Snds db)))
         , Contains (Snds (Concat (Snds db))) (Snds projected)
         )
      => Proxy universe
      -> Proxy db
      -> Project projected
      -> HList (Fmap universe (Snds projected))
      -> Maybe (Row projected)

instance ConvertToRow db '[] where
    convertToRow proxyU proxyDB EndProject HNil = Just EndRow

instance ConvertToRow db ts => ConvertToRow db ( '(sym, t) ': ts) where
    convertToRow (proxyU :: Proxy universe) proxyDB (x :+| prest) (v :> hrest) =
        case containsConstraint proxyC proxyContainer proxyContained of
            EveryConstraint -> case tailContainsProof proxyContainer proxyContained of
                ContainsProof -> case elemConstraint proxyC proxyT proxyContained (fromUniverse (Proxy :: Proxy t) v) of
                    Nothing -> Nothing
                    Just v' ->
                            (:&|)
                        <$> (pure (fromColumnAndValue x v'))
                        <*> (convertToRow proxyU proxyDB prest hrest)
      where
        proxyC :: Proxy (InUniverse universe)
        proxyC = Proxy
        proxyT :: Proxy t
        proxyT = Proxy
        proxyContained :: Proxy ( t ': (Snds ts) )
        proxyContained = Proxy
        proxyContainer :: Proxy (Snds (Concat (Snds db)))
        proxyContainer = Proxy
