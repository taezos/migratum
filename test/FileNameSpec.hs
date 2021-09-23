module FileNameSpec where

import           TestImport

-- migratum
import           Migratum.Parser.NamingRule
spec :: Spec
spec = do
  describe "FilenameSpec" $ do
    it "follows naming rules" $ do
      successResult <- runExceptT
        $ parseHandler ( parseNamingConvention "V1__uuid_extension.sql" )

      emptyFileName <- runExceptT
        $ parseHandler ( parseNamingConvention "" )

      noFileExt <- runExceptT
        $ parseHandler ( parseNamingConvention "V1__uuid_extension" )

      noVersion <- runExceptT
        $ parseHandler ( parseNamingConvention "uuid_extension.sql" )

      upperCaseFilename <- runExceptT
        $ parseHandler ( parseNamingConvention "V1__UUID_extension.sql" )

      symbolFilename <- runExceptT
        $ parseHandler ( parseNamingConvention "V1__UUID+extension.sql" )

      shouldBe ( isRight successResult ) True
      shouldBe ( isLeft emptyFileName ) True
      shouldBe ( isLeft noFileExt ) True
      shouldBe ( isLeft noVersion ) True
      shouldBe ( isRight upperCaseFilename ) True
      shouldBe ( isLeft symbolFilename ) True
