{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module EventSourcing where

import qualified Control.Monad.State.Lazy as State
import Control.Monad.State.Lazy (State, StateT, MonadState)
import Control.Monad.Trans.Class

import qualified Control.Monad.Writer (WriterT)
import Control.Monad.Writer as Writer

import Data.Foldable as Foldable

import qualified Data.IORef as IORef
import Data.IORef (IORef)

import qualified Control.Monad as Monad

import Cards
import Shuffle
import Game

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

import Debug.Trace (trace)
