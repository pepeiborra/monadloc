{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 702)
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
 This package defines a MonadLoc class for monads which support Monadic Call Traces.
 See http://pepeiborra.posterous.com/monadic-stack-traces-that-make-a-lot-of-sense

 * Traces are only provided for explicitly annotated program points.

 * This package installs the MonadLoc preprocessor for this purpose.
   To enable it include the following pragma at the top of your haskell source files:

@
     { -\# OPTIONS_GHC -F -pgmF MonadLoc \#- }
@

 * There is also the TH macro 'withLocTH' exported by "Control.Monad.Loc.TH"
-}
module Control.Monad.Loc (MonadLoc(..)) where

-- | Generating stack traces for failures
class Monad m => MonadLoc m where
  -- | 'withLoc' records the given source location in the failure trace
  --   if the underlying monad supports recording location traces
  --
  withLoc :: String -> m a -> m a

instance Monad m => MonadLoc m where withLoc _ = id;
