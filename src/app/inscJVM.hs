{-# LANGUAGE OverloadedStrings #-}

import JvmGenerator
import System.Environment
import Shared (parseArgs, getPathDirectory, getPathFileNameWithoutExtension)

main :: IO ()
main = do
    args <- getArgs
    programInputPath <- parseArgs args "insc_jvm"
    generateJVM programInputPath (getPathDirectory programInputPath) (getPathFileNameWithoutExtension programInputPath)