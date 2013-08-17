monadloc
========

monadloc is a Haskell package that defines a class for monads which can keep a monadic call trace.

http://pepeiborra.wordpress.com/2009/11/01/monadic-stack-traces-that-make-a-lot-of-sense for more information.

A preprocessor is available (see the package monadloc-pp) which inserts calls to Control.Monad.Loc.withLoc before every monadic statement in a module. To invoke the preprocessor, add the pragma OPTIONS_GHC -F -pgmF MonadLoc at the top of your Haskell files together with an import for the Control.Monad.Loc module

Changelog
=========

0.7 - Extracted Template Haskell macro to separate module to allow Control.Monad.Loc to be Safe. (thanks to Deian Stefan)

0.6 - Extracted the preprocessor to a separate package monadloc-pp to minimize the set of dependencies.
