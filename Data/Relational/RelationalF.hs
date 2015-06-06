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

module Data.Relational.RelationalF (

    RelationalF(..)
  , Relational

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
data RelationalF (db :: [(Symbol, [(Symbol, *)])]) a where

  RFRelation
    :: ( Contains (Snds (Concat (Snds db))) (Snds schema)
       )
    => Relation db schema
    -> ([Row schema] -> a)
    -> RelationalF db a

  RFInsert
    :: ( Elem '(tableName, schema) db
       , Contains (Snds (Concat (Snds db))) (Snds schema)
       )
    => Insert '(tableName, schema)
    -> a
    -> RelationalF db a

  RFUpdate
    :: ( Elem '(tableName, schema) db
       , Contains (Snds (Concat (Snds db))) (Snds row)
       , Contains (Snds (Concat (Snds db))) (Snds (Concat condition))
       )
    => Update '(tableName, schema) row condition
    -> a
    -> RelationalF db a

  RFDelete
    :: ( Elem '(tableName, schema) db
       , Contains (Snds (Concat (Snds db))) (Snds (Concat condition))
       )
    => Delete '(tableName, schema) condition
    -> a
    -> RelationalF db a

instance Functor (RelationalF db) where
  fmap f term = case term of
      RFRelation relation next -> RFRelation relation (fmap f next)
      RFInsert insert next -> RFInsert insert (f next)
      RFUpdate update next -> RFUpdate update (f next)
      RFDelete delete next -> RFDelete delete (f next)

type Relational db = Free (RelationalF db)

rfrelation relation = liftF (RFRelation relation id)

rfinsert insert = liftF (RFInsert insert ())

rfupdate update = liftF (RFUpdate update ())

rfdelete delete = liftF (RFDelete delete ())
