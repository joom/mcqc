module Parser.Decl where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import Parser.Expr

-- Declarations
data Declaration
  = IndDecl {name :: Text, iargs :: [Text], constructors :: [Ind]}
  | TypeDecl {name :: Text, targs :: [Text], tval :: Type}
  | FixDecl {fixlist :: [Fix]}
  | TermDecl {name :: Text, typ :: Type, val :: Expr}
  deriving (Show, Eq)

instance FromJSON Declaration where
  parseJSON (Object v) =
    case KM.lookup "what" v of
      Just "decl:ind" ->
        IndDecl
          <$> v .: "name"
          <*> v .:? "argnames" .!= []
          <*> v .:? "constructors" .!= []
      Just "decl:type" ->
        TypeDecl
          <$> v .: "name"
          <*> v .:? "argnames" .!= []
          <*> v .: "value"
      Just "decl:fixgroup" -> FixDecl <$> v .:? "fixlist" .!= []
      Just "decl:term" ->
        TermDecl
          <$> v .: "name"
          <*> v .: "type"
          <*> v .: "value"
      _ -> fail $ "Unknown declaration type: " ++ show v
  parseJSON _ = fail "Unknown declaration JSON representation"
