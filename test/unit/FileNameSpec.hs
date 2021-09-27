module FileNameSpec where

import           TestImport

-- migratum
import           Migratum.Capability.Migration
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

    it "will order MigratumScript" $ do
      let mg1 = MigratumScript "V1__table1.sql" "file content"
      let mg2 = MigratumScript "V2__table2.sql" "file content"
      let mg3 = MigratumScript "V3__table3.sql" "file content"
      shouldBe ( sort [mg3, mg1, mg2] ) [ mg1, mg2, mg3 ]
