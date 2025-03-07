module Parser.Mod where
import Parser.Decl
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text

---- For casting JSON to Module
data Module = Module { name :: Text, used_modules :: [Text], declarations :: [Declaration] }
    deriving (Show, Eq)

instance FromJSON Module where
  parseJSON (Object v) =
      case KM.lookup "what" v of
        Just "module"        -> Module <$> v .:  "name"
                                       <*> v .:? "used_modules" .!= []
                                       <*> v .:? "declarations" .!= []
        _                    -> fail $ "Bad 'what' quantifier for: " ++ show v

  parseJSON _ = fail "Unknown JSON for Mod object"
