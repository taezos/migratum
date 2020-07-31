module FileNameSpec where

import           TestImport

-- migratum
import           Migratum.Parser.NamingRule
spec :: Spec
spec = do
  describe "Filename" $ do
    it "follows naming rules" $ do
      successResult <- runExceptT $ parseHandler ( parseNamingConvention "V1__uuid_extension.sql" )
      emptyFileName <- runExceptT $ parseHandler ( parseNamingConvention "" )
      noFileExt <- runExceptT $ parseHandler ( parseNamingConvention "V1__uuid_extension" )
      noVersion <- runExceptT $ parseHandler ( parseNamingConvention "uuid_extension.sql" )

      shouldBe ( isRight successResult ) True
      shouldBe ( isLeft emptyFileName ) True
      shouldBe ( isLeft noFileExt ) True
      shouldBe ( isLeft noVersion ) True
