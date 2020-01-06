module Unison.Syntax.Default (defaultSyntax) where

import           Unison.DataDeclaration         (DataDeclaration', EffectDeclaration')
import           Unison.Parser                  ( Ann )
import           Unison.PrettyPrintEnv          ( PrettyPrintEnv )
import           Unison.Syntax
import qualified Unison.Syntax.Default.Parsers  as Parsers
import qualified Unison.TermPrinter             as TermPrinter
import qualified Unison.TypePrinter             as TypePrinter
import           Unison.Util.Pretty             (Pretty, ColorText)
import           Unison.Var                     (Var)

defaultSyntax :: Var v => Syntax v
defaultSyntax = Syntax Parsers.parseFile Parsers.parseType prettyPr

prettyPr :: (Var v, Construct v c) => PrettyPrintEnv -> c -> Pretty ColorText
prettyPr env =
  switch
    (TermPrinter.pretty env)
    (TypePrinter.pretty env)
    (prettyData env)
    (prettyAbility env)

-- TODO implement this
prettyData :: PrettyPrintEnv -> DataDeclaration' v Ann -> Pretty ColorText
prettyData _env _decl = undefined

-- TODO implement this
prettyAbility :: PrettyPrintEnv -> EffectDeclaration' v Ann -> Pretty ColorText
prettyAbility _env _decl = undefined
