{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Monad
  ( M (..)
  , Version
  , Options (..)
  , UpdateEnv (..)
  ) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Text (Text)
import Prelude hiding (FilePath)
import Shelly (FilePath)
import Shelly.Lifted (MonadShControl, MonadSh)

type Version = Text

data Options = Options
  { dryRun :: Bool
  , workingDir :: FilePath
  , githubToken :: Text
  }

data UpdateEnv = UpdateEnv
  { packageName :: Text
  , oldVersion :: Version
  , newVersion :: Version
  , options :: Options
  }

type M m =
  ( MonadReader UpdateEnv m
  , MonadError Text m
  , MonadSh m
  , MonadShControl m
  , MonadIO m
  )
