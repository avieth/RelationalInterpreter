{-|
Module      : 
Description : 
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Relational.RelationalF (

    RelationalF(..)

  , rfrelation
  , rfinsert
  , rfupdate
  , rfdelete

  ) where

import GHC.TypeLits
import Control.Monad.Free
import Data.Relational
import Data.Relational.Universe

-- First parameter describes the database on which we act.
-- List of table names, and their associated schemas.
data RelationalF (universe :: *) (db :: [(Symbol, [(Symbol, *)])]) a where

  RFRelation
    :: ( -- Contains (Snds (Concat (Snds db))) (Snds schema)
       -- TODO might be nice to take a smaller db in the Relation type and
       -- assert that it's contained in the RelationalF's db type.
       )
    => Relation universe db tableName schema
    -> ([Row schema] -> a)
    -> RelationalF universe db a

  RFInsert
    :: ( Elem '(tableName, schema) db
         -- , Contains (Snds (Concat (Snds db))) (Snds schema)
       )
    => Insert universe db '(tableName, schema)
    -> a
    -> RelationalF universe db a

  RFUpdate
    :: ( Elem '(tableName, schema) db
         -- , Contains (Snds (Concat (Snds db))) (ConditionTypeList condition)
         -- , Contains (Snds (Concat (Snds db))) (Snds row)
       )
    => Update universe db '(tableName, schema) row condition
    -> a
    -> RelationalF universe db a

  RFDelete
    :: ( Elem '(tableName, schema) db
         -- , Contains (Snds (Concat (Snds db))) (ConditionTypeList condition)
       )
    => Delete universe db '(tableName, schema) condition
    -> a
    -> RelationalF universe db a

instance Functor (RelationalF universe db) where
  fmap f term = case term of
      RFRelation relation next -> RFRelation relation (fmap f next)
      RFInsert insert next -> RFInsert insert (f next)
      RFUpdate update next -> RFUpdate update (f next)
      RFDelete delete next -> RFDelete delete (f next)

rfrelation
    :: forall universe db name schema .
       Relation universe db name schema
    -> RelationalF universe db [Row schema]
rfrelation relation = RFRelation relation id

rfinsert
    :: forall universe db name schema .
       ( Elem '(name, schema) db )
    => Insert universe db '(name, schema)
    -> RelationalF universe db ()
rfinsert insert = RFInsert insert ()

rfupdate
    :: forall universe db name schema row condition .
       ( Elem '(name, schema) db )
    => Update universe db '(name, schema) row condition
    -> RelationalF universe db ()
rfupdate update = RFUpdate update ()

rfdelete
    :: forall universe db name schema condition .
       ( Elem '(name, schema) db )
    => Delete universe db '(name, schema) condition
    -> RelationalF universe db ()
rfdelete delete = RFDelete delete ()
