{-|
Module      : Data.Relational.Interpreter
Description : The RelationalInterpreter typeclass.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Data.Relational.Interpreter (

    RelationalInterpreter(..)

  , Interpreter
  , interpreter

  ) where

import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol)
import Control.Applicative
import Data.Proxy
import Data.Relational
import Data.Relational.Universe
import Data.Relational.RelationalF

type Interpreter f m = forall t . f (m t) -> m t

interpreter
  :: forall t (db :: [(Symbol, [(Symbol, *)])]) .
     ( RelationalInterpreter t
     , Monad (InterpreterMonad t)
     , Every (InUniverse (Universe t)) (Snds (Concat (Snds db)))
     , InterpreterSelectConstraint t db
     , InterpreterInsertConstraint t db
     , InterpreterUpdateConstraint t db
     , InterpreterDeleteConstraint t db
     , Unique (TableNames db)
     )
  => Proxy t
  -> Interpreter (RelationalF db) (InterpreterMonad t)
interpreter proxyT term = case term of
    RFSelect select next -> interpretSelect proxyT proxyDB select >>= next
    RFInsert insert next -> interpretInsert proxyT proxyDB insert >>  next
    RFUpdate update next -> interpretUpdate proxyT proxyDB update >>  next
    RFDelete delete next -> interpretDelete proxyT proxyDB delete >>  next
  where
    proxyDB :: Proxy db
    proxyDB = Proxy

class RelationalInterpreter t where

  data Universe t :: * -> *
  type InterpreterMonad t :: * -> *
  type InterpreterSelectConstraint t (db :: [(Symbol, [(Symbol, *)])]) :: Constraint
  type InterpreterDeleteConstraint t (db :: [(Symbol, [(Symbol, *)])]) :: Constraint
  type InterpreterInsertConstraint t (db :: [(Symbol, [(Symbol, *)])]) :: Constraint
  type InterpreterUpdateConstraint t (db :: [(Symbol, [(Symbol, *)])]) :: Constraint

  interpretSelect
    :: forall tableName schema projected conditions (db :: [(Symbol, [(Symbol, *)])]) .
       ( Every (InUniverse (Universe t)) (Snds (Concat (Snds db)))
       , Contains (Snds (Concat (Snds db))) (Snds (Concat conditions))
       , Contains (Snds (Concat (Snds db))) (Snds projected)
       , AllFromUniverse db (Snds projected)
       , AllToUniverse db (Snds (Concat conditions))
       , ConvertToRow db projected
       , InterpreterSelectConstraint t db
       )
    => Proxy t
    -> Proxy db
    -> Select '(tableName, schema) projected conditions
    -> (InterpreterMonad t) [Row projected]

  interpretDelete
    :: forall tableName schema conditions (db :: [(Symbol, [(Symbol, *)])]) .
       ( Every (InUniverse (Universe t)) (Snds (Concat (Snds db)))
       , Contains (Snds (Concat (Snds db))) (Snds (Concat conditions))
       , AllToUniverse db (Snds (Concat conditions))
       , InterpreterDeleteConstraint t db
       )
    => Proxy t
    -> Proxy db
    -> Delete '(tableName, schema) conditions
    -> (InterpreterMonad t) ()

  interpretInsert
    :: forall tableName schema (db :: [(Symbol, [(Symbol, *)])]) .
       ( Every (InUniverse (Universe t)) (Snds (Concat (Snds db)))
       , Contains (Snds (Concat (Snds db))) (Snds schema)
       , RowToHList schema
       , AllToUniverse db (Snds schema)
       , InterpreterInsertConstraint t db
       )
    => Proxy t
    -> Proxy db
    -> Insert '(tableName, schema)
    -> (InterpreterMonad t) ()

  interpretUpdate
    :: forall tableName schema projected conditions (db :: [(Symbol, [(Symbol, *)])]) .
       ( Every (InUniverse (Universe t)) (Snds (Concat (Snds db)))
       , Contains (Snds (Concat (Snds db))) (Snds (Concat conditions))
       , Contains (Snds (Concat (Snds db))) (Snds projected)
       , RowToHList projected
       , AllToUniverse db (Snds projected)
       , AllToUniverse db (Snds (Concat conditions))
       , InterpreterUpdateConstraint t db
       )
    => Proxy t
    -> Proxy db
    -> Update '(tableName, schema) projected conditions
    -> (InterpreterMonad t) ()
