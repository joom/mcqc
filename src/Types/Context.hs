module Types.Context where

import Data.Map
import Data.Text (Text)
import Debug.Trace
import System.IO.Unsafe

-- Named context
type Context a = Map Text a

-- Print key values correctly
printCtx :: (Show a) => Context a -> IO ()
printCtx = putStr . unlines . elems . mapWithKey (\k v -> show k ++ " : " ++ show v)

-- Print Ctx unsafe
unsafePrintCtx :: (Show a) => Bool -> Context a -> Bool
unsafePrintCtx b = const b . unsafePerformIO . printCtx

-- Merge two contexts, always prefer old definitions
mergeCtx :: (Show a) => Context a -> Context a -> Context a
mergeCtx = unionWithKey (\k _ vb -> trace ("Warning: Polymorphism in context for " ++ show k ++ " keeping this declaration " ++ show vb) vb)
