{-# LANGUAGE TemplateHaskell #-}
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

 * There is also the TH macro 'withLocTH' to manually annotate program points,
   but you should always use the preprocessor if possible.
-}
module Control.Monad.Loc (MonadLoc(..), withLocTH) where
import Language.Haskell.TH.Syntax (qLocation, Loc(..), Q, Exp)
import Text.PrettyPrint
import Prelude hiding (mod)

-- | Generating stack traces for failures
class Monad m => MonadLoc m where
  -- | 'withLoc' records the given source location in the failure trace
  --   if the underlying monad supports recording location traces
  --
  withLoc :: String -> m a -> m a


instance Monad m => MonadLoc m where withLoc _ = id;


-- | 'withLocTH' is a convenient TH macro which expands to 'withLoc' @\<source location\>@
--    It should only be used when the MonadLoc preprocessor is not available.
--    Usage:
--
--    > f x = $withLocTH $ do
--    >          $withLocTH $ something
--    >          x < -$withLocTH $ something-else
--    >          ...
--
--   NOTE: unfortunately type signatures are necessary when using withLocTH

withLocTH :: Q Exp
withLocTH = do
  loc <- qLocation
  let loc_msg = showLoc loc
  [| withLoc loc_msg |]

showLoc :: Loc -> String
showLoc Loc{loc_module=mod, loc_filename=filename, loc_start=start} = render $
                     {- text package <> char '.' <> -}
                     text mod <> parens (text filename) <> colon <+> text (show start)
