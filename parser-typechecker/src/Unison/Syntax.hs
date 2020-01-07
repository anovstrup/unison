{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Syntax where

--import Unison.Prelude

--import Unison.Names3 (Names)
--import qualified Unison.Names3 as Names
import Unison.DataDeclaration (DataDeclaration', EffectDeclaration')
import Unison.HashQualified (HashQualified)
import Unison.Parser ( Ann
                     --, UniqueName
                     , Err
                     , ParsingEnv
                     )
import           Unison.PrettyPrintEnv          ( PrettyPrintEnv )
import Unison.Reference (Reference)
import Unison.Term (AnnotatedTerm)
import Unison.Type (Type)
import Unison.UnisonFile
import Unison.Util.Pretty (Pretty, ColorText)

data Syntax v = Syntax {
  -- arguments are file path, file contents, and a "parsing environment" (composed of a Names3 and a UniqueName)
  -- question: should the file path be part of the parsing environment, instead of a separate argument?
    parseFile     :: FilePath -> String -> ParsingEnv -> Either (ParsingErr v) (UnisonFile v Ann)
  , parseType     :: String -> ParsingEnv -> Either (ParsingErr v) (Type v Ann)
  -- Pretty (ColorText, Ann) instead of Pretty ColorText would support adding location-based highlighting after the fact
  , prettyTerm    :: PrettyPrintEnv -> AnnotatedTerm v Ann -> Pretty ColorText
  , prettyType    :: PrettyPrintEnv -> Type v Ann -> Pretty ColorText
  , prettyData    :: PrettyPrintEnv -> Reference -> HashQualified -> DataDeclaration' v Ann -> Pretty ColorText
  , prettyAbility :: PrettyPrintEnv -> Reference -> HashQualified -> EffectDeclaration' v Ann -> Pretty ColorText }

-- this type should be defined in this module, rather than in Parser (which will move to Syntax/Default)
--data ParsingEnv = ParsingEnv {
--    uniqueNames :: UniqueName
--  , names       :: Names }

-- TODO move from Parser module
type ParsingErr v = Err v -- Parser.Err
