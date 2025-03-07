module Common.Pretty where

import Control.Monad
import Prettyprinter
import Prettyprinter.Render.Text

instance Eq (Doc ann) where
  x == y = render x == render y
    where
      render = renderLazy . layoutPretty defaultLayoutOptions

-- Add parenteses if the argument needs them
maybeparens :: Doc ann -> Doc ann
maybeparens x = if show x == "" then mempty else parens x

-- Format and pretty print as a breakable comma-separated list
breakcommatize :: (Pretty a) => [a] -> Doc ann
breakcommatize [] = mempty
breakcommatize [a] = pretty a
breakcommatize (a : args) =
  softcommatize (pretty a) $
    align . tab $
      concatWith softcommatize prettyargs
  where
    softcommatize x y = x <> "," <> softline <> y
    prettyargs = map pretty args

-- Format as an unbreakable comma-separated list
commatize :: [Doc ann] -> Doc ann
commatize [] = mempty
commatize args = concatWith (\x y -> x <> "," <+> y) args

-- Add a tab
tab :: Doc ann -> Doc ann
tab = indent 2

-- Always the same
overloadedDoc :: Doc ann
overloadedDoc =
  "template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };"
    <> line
    <> "template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;"

-- Ptr declaration
ptrDoc :: Doc ann
ptrDoc =
  "template<typename T>"
    <> line
    <> "using Ptr = std::shared_ptr<T>;"

stringsFromTo :: Char -> Char -> [[Char]]
stringsFromTo t z = stringsFromToM 1 t z
  where
    ts n = replicate n t
    zs n = replicate n z
    stringsFromToM n t z = zipWithM (\a b -> [a .. b]) (ts n) (zs n) ++ stringsFromToM (n + 1) t z
