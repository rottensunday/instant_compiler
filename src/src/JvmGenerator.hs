module JvmGenerator (
    generateJVM
) where

import Data.Foldable
import Data.List(isPrefixOf)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import JvmConsts
import Shared(getProgram)
import Language.Abs

import System.Process

type Instruction = String
type Height = Int
type Variables = M.Map String Int

data ExpWithHeight
    = ExpAddWithHeight ExpWithHeight ExpWithHeight Height
    | ExpSubWithHeight ExpWithHeight ExpWithHeight Height
    | ExpMulWithHeight ExpWithHeight ExpWithHeight Height
    | ExpDivWithHeight ExpWithHeight ExpWithHeight Height
    | ExpLitWithHeight Integer Height
    | ExpVarWithHeight PIdent Height
  deriving (Eq, Ord, Show, Read)

data ProcessingResult = ProcessingResult [[Instruction]] Variables

generateJVM :: String -> String -> String -> IO ()
generateJVM inputPath outputDirectory fileName = do
    programResult <- fmap (processParsingResult fileName) program
    TIO.writeFile jpath (T.pack programResult)
    callCommand $ "java -jar lib/jasmin.jar -d " ++ outputDirectory ++ " " ++ jpath
    putStrLn "Successfully saved result files"
    where
        program = getProgram inputPath
        jpath = outputDirectory ++ "/" ++ fileName ++ ".j"

processParsingResult :: String -> Either String Program -> String
processParsingResult _ (Left err) = error err
processParsingResult fileName (Right program) = resultList
    where 
        (ProcessingResult instructions variables) = processProgram program
        instructionsFlattened = concatMap reverse (reverse instructions)
        variablesCount = max (M.size variables) 1
        stackSize = determineStackSize instructionsFlattened [0]
        resultList = 
            programPrefix fileName ++
            unlines [emitStack stackSize, emitVariables variablesCount] ++
            unlines instructionsFlattened ++
            programSuffix

determineStackSize :: [Instruction] -> [Int] -> Int
determineStackSize [] res = maximum res
determineStackSize (x:xs) res
    | any (`isPrefixOf` x) ["iconst", "bipush", "sipush", "ldc", "iload", "getstatic"] = determineStackSize xs (head res + 1 : res)
    | any (`isPrefixOf` x) ["iadd", "isub", "imul", "idiv", "istore"] = determineStackSize xs (head res - 1 : res)
    | "invokevirtual" `isPrefixOf` x = determineStackSize xs (head res - 2 : res)
    | otherwise = determineStackSize xs res

processProgram :: Program -> ProcessingResult
processProgram (Prog stmts) = foldl' (flip processStmt) (ProcessingResult [] M.empty) stmts

processStmt :: Stmt -> ProcessingResult -> ProcessingResult
processStmt (SAss identifier expression) (ProcessingResult instructions variables) = 
    processAssignment identifier expression instructions variables
processStmt (SExp expression) (ProcessingResult instructions variables)
    = ProcessingResult 
        ([getPrintLn] : expressionInstructions : [getPrintStream] : instructions)
        variables 
    where 
        expressionInstructions = processExpression (decorateExpWithHeight expression) variables []

processAssignment :: PIdent -> Exp -> [[Instruction]] -> Variables -> ProcessingResult
processAssignment (PIdent(_, identifier)) expression instructions variables = 
    ProcessingResult 
        ([emitStore variableIndex] : expressionInstructions : instructions)
        (M.insert identifier variableIndex variables)
    where 
        expressionInstructions = processExpression (decorateExpWithHeight expression) variables []
        variableIndex = case M.lookup identifier variables of
                            Nothing -> M.size variables
                            Just i -> i

processExpression :: ExpWithHeight -> Variables -> [Instruction] -> [Instruction]
processExpression (ExpLitWithHeight value _) _ instructions = emitPushConst value : instructions
processExpression (ExpAddWithHeight left right _) variables instructions = genericProcessExpression left right False emitAdd variables instructions
processExpression (ExpSubWithHeight left right _) variables instructions = genericProcessExpression left right True emitSub variables instructions
processExpression (ExpMulWithHeight left right _) variables instructions = genericProcessExpression left right False emitMul variables instructions
processExpression (ExpDivWithHeight left right _) variables instructions = genericProcessExpression left right True emitDiv variables instructions
processExpression (ExpVarWithHeight (PIdent((line, column), identifier)) _) variables instructions = emitLoad variableIndex : instructions
    where
        variableIndex = case M.lookup identifier variables of
                            Nothing -> error $ "Can't access identifier " ++ identifier ++ " - undefined in line: " ++ show line ++ " column: " ++ show column
                            Just x -> x

genericProcessExpression :: ExpWithHeight -> ExpWithHeight -> Bool -> Instruction -> Variables -> [Instruction] -> [Instruction]
genericProcessExpression left right shouldEmitDup instruction variables ins
    | getHeight left >= getHeight right = instruction : leftFirst
    | shouldEmitDup = instruction : emitSwap : rightFirst
    | otherwise = instruction : rightFirst
    where
        leftFirst = processExpression right variables (processExpression left variables ins)
        rightFirst = processExpression left variables (processExpression right variables ins)

decorateExpWithHeight :: Exp -> ExpWithHeight
decorateExpWithHeight (ExpLit value) = ExpLitWithHeight value 1
decorateExpWithHeight (ExpVar value) = ExpVarWithHeight value 1
decorateExpWithHeight e = case e of
                            (ExpAdd left right) -> uncurry3 ExpAddWithHeight (buildArgs left right)
                            (ExpSub left right) -> uncurry3 ExpSubWithHeight (buildArgs left right)
                            (ExpMul left right) -> uncurry3 ExpMulWithHeight (buildArgs left right)
                            (ExpDiv left right) -> uncurry3 ExpDivWithHeight (buildArgs left right)
                          where
                            decorate = decorateExpWithHeight
                            buildArgs left right =
                                let leftDecorated = decorate left
                                    rightDecorated = decorate right
                                    height = max (getHeight leftDecorated) (getHeight rightDecorated) + 1
                                in (leftDecorated, rightDecorated, height)
                            uncurry3 f (arg1, arg2, arg3) = f arg1 arg2 arg3

getHeight :: ExpWithHeight -> Height
getHeight (ExpAddWithHeight _ _ h) = h
getHeight (ExpSubWithHeight _ _ h) = h
getHeight (ExpMulWithHeight _ _ h) = h
getHeight (ExpDivWithHeight _ _ h) = h
getHeight (ExpLitWithHeight _ h) = h
getHeight (ExpVarWithHeight _ h) = h