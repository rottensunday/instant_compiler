{-# LANGUAGE OverloadedStrings #-}

import LLVMGenerator
import System.Environment
import Shared (parseArgs, getPathDirectory, getPathFileNameWithoutExtension)

main :: IO ()
main = do
    args <- getArgs
    programInputPath <- parseArgs args "insc_llvm"
    generateLLVM programInputPath (getPathDirectory programInputPath) (getPathFileNameWithoutExtension programInputPath)