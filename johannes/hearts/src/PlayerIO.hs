{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module PlayerIO where

import Polysemy
import Polysemy.Internal (send)
import Polysemy.State (State)
import qualified Polysemy.State as State

import Data.IORef (IORef)
import qualified Data.IORef as IORef

import GameEvent
import Cards
import Player

playerIO ::
  Player ->
  Strategy '[State (PlayerState player), Embed IO, Tty] ->
  IO (GameEvent -> IO [GameCommand])
playerIO player strategy =
  do ref <- IORef.newIORef (makeEmptyPlayerStateFor player)
     return (runM . 
             State.runStateIORef ref . 
             strategy player)
