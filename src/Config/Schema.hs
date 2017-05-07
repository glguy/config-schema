{-|
Module      : Config.Schema
Description : Top-level module rexporting child modules
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This package makes it possible to define schemas for configuration files.
These schemas can be used to generate a validating configuration file
loader, and to produce documentation about the supported format.

"Config.Schema.Spec" provides definitions used to make new schemas.

"Config.Schema.Load" uses schemas to match schemas against configuration values.

"Config.Schema.Docs" generates textual documentation for a schema.

-}
module Config.Schema
  ( module Config.Schema.Spec
  , module Config.Schema.Docs
  , module Config.Schema.Load
  ) where

import Config.Schema.Docs
import Config.Schema.Load
import Config.Schema.Spec
