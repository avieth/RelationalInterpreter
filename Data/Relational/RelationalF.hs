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

  , rfselect
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

  RFSelect
    :: ( Elem '(tableName, schema) db
       , Contains (Snds (Concat (Snds db))) (Snds schema)
       , Contains (Snds (Concat (Snds db))) (Snds projection)
       , Contains (Snds (Concat (Snds db))) (Snds (Concat condition))
       , AllToUniverse db (Snds (Concat condition))
       , AllFromUniverse db (Snds projection)
       , ConvertToRow db projection
       )
    => Select '(tableName, schema) projection condition
    -> ([Row projection] -> a)
    -> RelationalF db a

  RFInsert
    :: ( Elem '(tableName, schema) db
       , Contains (Snds (Concat (Snds db))) (Snds schema)
       , RowToHList schema
       , AllToUniverse db (Snds schema)
       )
    => Insert '(tableName, schema)
    -> a
    -> RelationalF db a

  RFUpdate
    :: ( Elem '(tableName, schema) db
       , Contains (Snds (Concat (Snds db))) (Snds row)
       , Contains (Snds (Concat (Snds db))) (Snds (Concat condition))
       , RowToHList row
       , AllToUniverse db (Snds row)
       , AllToUniverse db (Snds (Concat condition))
       )
    => Update '(tableName, schema) row condition
    -> a
    -> RelationalF db a

  RFDelete
    :: ( Elem '(tableName, schema) db
       , Contains (Snds (Concat (Snds db))) (Snds (Concat condition))
       , AllToUniverse db (Snds (Concat condition))
       )
    => Delete '(tableName, schema) condition
    -> a
    -> RelationalF db a

instance Functor (RelationalF db) where
  fmap f term = case term of
      RFSelect select next -> RFSelect select (fmap f next)
      RFInsert insert next -> RFInsert insert (f next)
      RFUpdate update next -> RFUpdate update (f next)
      RFDelete delete next -> RFDelete delete (f next)

type Relational db = Free (RelationalF db)

rfselect select = liftF (RFSelect select id)

rfinsert insert = liftF (RFInsert insert ())

rfupdate update = liftF (RFUpdate update ())

rfdelete delete = liftF (RFDelete delete ())
