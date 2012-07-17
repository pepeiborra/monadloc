{-# LANGUAGE TemplateHaskell #-}

-- | The TH macro 'withLocTH' to manually annotate program points,
--   but you should always use the preprocessor if possible.

module Control.Monad.Loc.TH (withLocTH) where

import Prelude hiding (mod)
import Language.Haskell.TH.Syntax (qLocation, Loc(..), Q, Exp)
import Text.Printf
import Control.Monad.Loc

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
showLoc Loc{loc_module=mod, loc_filename=filename, loc_start=start} =
                     {- text package <> char '.' <> -}
                     printf "%s (%s). %s" mod filename (show start)
