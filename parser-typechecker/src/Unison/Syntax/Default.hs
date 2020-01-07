module Unison.Syntax.Default (defaultSyntax) where

import           Unison.DataDeclaration          ( DataDeclaration', EffectDeclaration' )
--import qualified Unison.DeclPrinter             as DeclPrinter
import           Unison.HashQualified            ( HashQualified )
import           Unison.Parser                   ( Ann )
import           Unison.PrettyPrintEnv           ( PrettyPrintEnv )
import           Unison.Reference                ( Reference )
import           Unison.Syntax                   ( Syntax(Syntax) )
import qualified Unison.Syntax.Default.Parsers  as Parsers
import qualified Unison.TermPrinter             as TermPrinter
import qualified Unison.TypePrinter             as TypePrinter
import           Unison.Util.Pretty              ( Pretty, ColorText )
import           Unison.Var                      ( Var)

defaultSyntax :: Var v => Syntax v
defaultSyntax = Syntax
  Parsers.parseFile
  Parsers.parseType
  TermPrinter.pretty
  TypePrinter.pretty
  prettyData
  prettyAbility

prettyData    :: PrettyPrintEnv -> Reference -> HashQualified -> DataDeclaration' v Ann -> Pretty ColorText
prettyData = undefined -- DeclPrinter.prettyGADT returns a Pretty SyntaxText

prettyAbility :: PrettyPrintEnv -> Reference -> HashQualified -> EffectDeclaration' v Ann -> Pretty ColorText
prettyAbility = undefined -- DeclPrinter.prettyEffectDeclaration returns a Pretty SyntaxText
