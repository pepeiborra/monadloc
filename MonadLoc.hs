import Control.Applicative
import Data.Generics
import Language.Haskell.Exts.Annotated
import System.Environment
import Text.PrettyPrint

main :: IO ()
main = do
  (fn:inp:outp:_) <- getArgs
  Module l mhead opt imports decls <- fromParseResult <$>
                                      parseFileWithMode ourParseMode{parseFilename = fn} inp
  let mod'   = Module l mhead opt imports decls'
      mname  = case mhead of
                 Nothing -> ""
                 Just (ModuleHead _ mn _ _) -> prettyPrint mn
      decls' = everywhere (mkT (annotateStatements mname)) decls
  writeFile outp $ prettyPrintWithMode ourPrintMode mod'

ourParseMode :: ParseMode
ourParseMode = defaultParseMode { extensions =
                                        [MultiParamTypeClasses
                                        ,FunctionalDependencies
                                        ,ExplicitForall
                                        ,ExistentialQuantification
                                        ,PatternGuards
                                        ,ViewPatterns
                                        ,Arrows
                                        ,NamedFieldPuns
                                        ,DisambiguateRecordFields
                                        ,StandaloneDeriving
                                        ,GeneralizedNewtypeDeriving
                                        ,ScopedTypeVariables
                                        ,PatternSignatures
                                        ,PackageImports
                                        ,TemplateHaskell
                                        ,QuasiQuotes
                                        ,TransformListComp
                                        ,PostfixOperators]
                                }

ourPrintMode :: PPHsMode
ourPrintMode = defaultMode { linePragmas = True }

annotateStatements :: String -> Exp SrcSpanInfo -> Exp SrcSpanInfo

annotateStatements m (Do loc stmts)  = App loc (withLocCall m loc) (Paren loc $ Do  loc $ map (annotateStmt m) stmts)
annotateStatements m (MDo loc stmts) = App loc (withLocCall m loc) (Paren loc $ MDo loc $ map (annotateStmt m) stmts)
annotateStatements _ e = e

annotateStmt :: String -> Stmt SrcSpanInfo -> Stmt SrcSpanInfo
annotateStmt m (Generator loc pat exp) = Generator loc pat $ App loc (withLocCall m loc) (Paren loc exp)
annotateStmt m (Qualifier loc exp)     = Qualifier loc $ App loc (withLocCall m loc) (Paren loc exp)
annotateStmt m (RecStmt loc stmts)     = RecStmt loc $ map (annotateStmt m) stmts
annotateStmt _ stmt                    = stmt

withLocCall :: String -> SrcSpanInfo -> Exp SrcSpanInfo
withLocCall m loc = App loc (Var loc withLoc) (Lit loc srclocLit)
  where
   withLoc   = Qual loc (ModuleName loc "Control.Monad.Loc") (Ident loc "withLoc")
   srclocLit = String loc (render locString) ""
   locString = (text m <> parens(text (fileName loc)) <> colon <+> parens (int (startLine loc) <> comma <+> int(startColumn loc)))


