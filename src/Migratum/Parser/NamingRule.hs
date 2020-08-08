{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Migratum.Parser.NamingRule where

import           Import                        hiding (FilePath)

-- mtl
import           Control.Monad.Except

-- text
import qualified Data.Text                     as T

-- microlens
import           Lens.Micro

-- turtle
import           Turtle                        (FilePath)
import qualified Turtle

-- parsec
import           Text.ParserCombinators.Parsec (GenParser, ParseError)
import qualified Text.ParserCombinators.Parsec as Parsec

-- migratum
import           Migratum.Feedback

data FilenameStructure = FilenameStructure
  { _filenameStructureVersion :: Text
  , _filenameStructureName    :: Text
  , _filenameStructureExt     :: Text
  } deriving ( Eq, Show )

parseHandler
  :: MonadError MigratumError m
  => Either ParseError FilenameStructure
  -> m String
parseHandler res =  case res of
  Left err -> throwError $ MigratumError $ show err
  Right r  -> pure $ Turtle.encodeString $ toFilePath r

parseNamingConvention :: String -> Either ParseError FilenameStructure
parseNamingConvention =
  Parsec.parse namingConventionParser "Not following naming rules"

toFilePath :: FilenameStructure -> FilePath
toFilePath FilenameStructure {..} = Turtle.fromText
  $ _filenameStructureVersion
  <> _filenameStructureName
  <> _filenameStructureExt

vCharParser :: GenParser Char st Char
vCharParser = Parsec.char 'V'

versionNumParser :: GenParser Char st String
versionNumParser = Parsec.many Parsec.digit

underScoreParser :: GenParser Char st Char
underScoreParser = Parsec.char '_'

fileNameParser :: GenParser Char st String
fileNameParser = Parsec.many ( Parsec.satisfy isFileName )
  where
    isFileName :: Char -> Bool
    isFileName char = any
      ( char== )
      ( "abcdefghijklmnopqrstuvwxyz1234567890_" :: String )

sqlExtParser :: GenParser Char st String
sqlExtParser = do
  dot <- Parsec.char '.'
  ext <- Parsec.string "sql"
  pure $ ( dot : [] ) <> ext

namingConventionParser :: GenParser Char st FilenameStructure
namingConventionParser = do
  v <- vCharParser
  vNum <- versionNumParser
  u1 <- underScoreParser
  u2 <- underScoreParser
  name <- fileNameParser
  ext <- sqlExtParser
  pure $ FilenameStructure
    ( T.pack $ ( v : []) <> vNum <> ( u1 : u2 : [] ) )
    ( T.pack name )
    ( T.pack ext )

-- * Lens
filenameStructureVersion :: Lens' FilenameStructure Text
filenameStructureVersion = lens
  _filenameStructureVersion
  (\s newVersion -> s { _filenameStructureVersion = newVersion })

filenameStructureName :: Lens' FilenameStructure Text
filenameStructureName = lens _filenameStructureName
  (\s newName -> s { _filenameStructureName = newName })

filenameStructureExt :: Lens' FilenameStructure Text
filenameStructureExt = lens _filenameStructureExt
  (\s newExt -> s { _filenameStructureExt = newExt })
