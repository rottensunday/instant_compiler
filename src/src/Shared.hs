{-# LANGUAGE OverloadedStrings #-}

module Shared ( 
    getProgram, 
    getPathDirectory,
    getPathFileNameWithoutExtension,
    parseArgs
    ) where
import Language.Abs (Program)
import Language.Par (pProgram, myLexer)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.Environment
import System.Exit (exitSuccess, exitWith, ExitCode(ExitFailure))

getProgram :: FilePath -> IO (Either String Program)
getProgram x = fmap (pProgram . myLexer . T.unpack) (TIO.readFile x)

getPathDirectory = T.unpack . T.intercalate "/" . init . T.splitOn "/" . T.pack
getPathFileNameWithoutExtension = skipExtension . last . T.splitOn "/" . T.pack
skipExtension = concatMap T.unpack . (\x -> if length x == 1 then x else init x) . T.splitOn "."

parseArgs :: [String] -> String -> IO String
parseArgs ["-h"] command = usage command >> exit
parseArgs [] command = usage command >> die
parseArgs [path] _ = return path
parseArgs _ _ = exit

usage command   = putStrLn $ "Usage: " ++ command ++ " filePath"
exit    = exitSuccess
die     = exitWith (ExitFailure 1)