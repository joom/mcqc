module Main where
import System.Environment
import Data.ByteString.Lazy.Char8 (ByteString)
import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Prettyprinter
import Prettyprinter.Render.Text
import Classes.Compilable
import Ops.Flags
import Types.Context
import CIR.File
import Parser.Mod ( Module(used_modules) )
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Common.Config as Conf

-- Render from Pretty printer Doc to bytestring
render :: Doc ann -> ByteString
render = B.pack . T.unpack . renderStrict . layoutPretty layoutOptions
    where layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 40 1 }

main :: IO ()
main = do
    args <- getArgs
    (flags, fn) <- getFlags args
    -- Read AST
    ast <- readAst fn
    -- Handle flags
    forM_ flags $ \case
        Output outfn -> do
            let modules = Conf.filterMod . used_modules $ ast
            let extfn = map (`T.append` ".json") modules
            externals <- mapM (readAst . T.unpack) extfn
            -- All the compiling in one line
            let bigfile = evalState (mconcat <$> mapM comp (externals ++ [ast])) Conf.nativeContext
            B.writeFile outfn . render . pretty $ (bigfile :: CFile)
        Debug -> do
            putStrLn . header $ "Args"
            print args
            putStrLn . header $ "Modules Imported"
            let modules = Conf.filterMod . used_modules $ ast
            print modules
            putStrLn . header $ "JSON dump"
            let extfn = map (`T.append` ".json") modules
            externals <- mapM (readAst . T.unpack) extfn
            print externals
            -- All the compiling in one line
            let (bigfile, st) = runState (mconcat <$> mapM comp (externals ++ [ast])) Conf.nativeContext
            putStrLn . B.unpack . encodePretty $ (bigfile :: CFile)
            putStrLn . header $ "Context after compiling"
            printCtx st
        -- Default
        o -> error $ "Unhandled flag " ++ show o
    where header s = replicate 12 '=' ++ concat [" ", s, " "] ++ replicate 12 '='
          readAst fn = B.readFile fn >>=
                        (\json -> case eitherDecode json of
                            (Right r) -> return r
                            (Left s) -> error s)
