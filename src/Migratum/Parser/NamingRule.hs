{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications  #-}
module Migratum.Parser.NamingRule where

import           Import                        hiding (FilePath)

-- mtl
import           Control.Monad.Except

-- text
import qualified Data.Text                     as T

-- turtle
import           Turtle                        (FilePath)
import qualified Turtle

-- parsec
import           Text.ParserCombinators.Parsec (GenParser, ParseError)
import qualified Text.ParserCombinators.Parsec as Parsec

-- migratum
import           Migratum.Feedback

data FilenameStructure
  = FilenameStructure FileVersion Underscore Underscore FileName Dot FileExtension
  deriving ( Eq, Show )

newtype FileVersion = FileVersion Text
  deriving ( Eq, Show )

newtype FileName = FileName Text
  deriving ( Eq, Show )

newtype FileExtension = FileExtension Text
  deriving ( Eq, Show )

newtype Underscore = Underscore Text
  deriving ( Eq, Show )

newtype Dot = Dot Text
  deriving ( Eq, Show )

instance Ord FilenameStructure where
  compare fileS1 fileS2 = compare ( fileVersion fileS1 ) ( fileVersion fileS2 )

instance Ord FileVersion where
  compare fv1 fv2 = compare ( fileVersionToInt fv1 ) ( fileVersionToInt fv2 )

fileVersionToInt :: FileVersion -> Maybe Int
fileVersionToInt fv = readMaybe $ filter ( /='V' ) $ T.unpack $ versionToText fv

fileVersion :: FilenameStructure -> FileVersion
fileVersion ( FilenameStructure v _ _ _ _ _ ) = v

versionToText :: FileVersion -> Text
versionToText = coerce

fileVersionParser :: GenParser Char st FileVersion
fileVersionParser = do
  vChar <- Parsec.char 'V'
  vNum <- Parsec.digit
  pure $ FileVersion $ T.pack $ ( vChar : vNum : [] )

fileNameParser :: GenParser Char st FileName
fileNameParser = FileName . T.pack
  <$> Parsec.many ( Parsec.alphaNum <|> Parsec.satisfy isUnderscore )

isUnderscore :: Char -> Bool
isUnderscore char = any @[] (char==) "_"

underScoreParser :: GenParser Char st Underscore
underScoreParser = Underscore . T.pack . pure <$> Parsec.satisfy isUnderscore

dotParser :: GenParser Char st Dot
dotParser = Dot . T.pack . pure <$> Parsec.char '.'

fileExtensionParser :: GenParser Char st FileExtension
fileExtensionParser = FileExtension . T.pack <$> Parsec.string "sql"

namingConventionParser :: GenParser Char st FilenameStructure
namingConventionParser = FilenameStructure
  <$>  fileVersionParser
  <*> underScoreParser
  <*> underScoreParser
  <*> fileNameParser
  <*> dotParser
  <*> fileExtensionParser

parseNamingConvention :: String -> Either ParseError FilenameStructure
parseNamingConvention =
  Parsec.parse namingConventionParser "Not following naming convention"

parseHandler
  :: MonadError MigratumError m
  => Either ParseError FilenameStructure
  -> m String
parseHandler res = case res of
  Left err -> throwError $ MigratumError $ show err
  Right r -> pure $ Turtle.encodeString $ toFilePath r

toFilePath :: FilenameStructure -> FilePath
toFilePath ( FilenameStructure v  u  u' name dot ext ) = Turtle.fromText
  $ coerce v
  <> coerce u
  <> coerce u'
  <> coerce name
  <> coerce dot
  <> coerce ext

