module LLVMGenerator (
    generateLLVM
) where

import Data.Foldable
import System.Process

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Shared(getProgram)
import Language.Abs
import LLVMConsts

type Instruction = String
type Variables = M.Map String String
type RegistersUsed = Int
type Register = String

data ProcessingResult = ProcessingResult [[Instruction]] Variables RegistersUsed
data ExpressionProcessingResult = 
    TreeProcessingResult [Instruction] RegistersUsed Register
    | Constant String

generateLLVM :: String -> String -> String -> IO ()
generateLLVM inputPath outputDirectory fileName = do
    programResult <- fmap processParsingResult program
    TIO.writeFile llpath (T.pack programResult)
    callCommand $ "llvm-as -o " ++ bcpath ++ " " ++ llpath
    putStrLn "Successfully saved result files"
    where
        program = getProgram inputPath
        bcpath = outputDirectory ++ "/" ++ fileName ++ ".bc"
        llpath = outputDirectory ++ "/" ++ fileName ++ ".ll"

processParsingResult :: Either String Program -> String
processParsingResult (Left err) = error err
processParsingResult (Right program) = resultList
    where 
        (ProcessingResult instructions _ _) = processProgram program
        instructionsFlattened = concatMap reverse (reverse instructions)
        resultList = 
            programPrefix ++
            unlines instructionsFlattened ++
            programSuffix

processProgram :: Program -> ProcessingResult
processProgram (Prog stmts) = foldl' (flip processStmt) (ProcessingResult [] M.empty 0) stmts

processStmt :: Stmt -> ProcessingResult -> ProcessingResult
processStmt (SAss identifier expression) (ProcessingResult instructions variables registersUsed) = 
    processAssignment identifier expression instructions variables registersUsed
processStmt (SExp expression) (ProcessingResult instructions variables registersUsed) = result
    where 
        expressionProcessingResult 
            = processExpression expression variables (TreeProcessingResult [] registersUsed "")
        result = case expressionProcessingResult of
                    Constant value ->
                        ProcessingResult
                            ([emitPrint value] : instructions)
                            variables
                            registersUsed
                    TreeProcessingResult expressionInstructions registersUsed' latestRegister ->
                        ProcessingResult
                            ([emitPrint latestRegister] : expressionInstructions : instructions)
                            variables
                            (registersUsed' + 1)

processAssignment :: PIdent -> Exp -> [[Instruction]] -> Variables -> RegistersUsed -> ProcessingResult
processAssignment (PIdent(_, identifier)) expression instructions variables registersUsed = result
    where 
        expressionProcessingResult = processExpression expression variables (TreeProcessingResult [] registersUsed "")
        result = case expressionProcessingResult of
                    Constant value ->
                        let newInstructionsList = case variableRegisterNameMaybe of
                              Nothing -> [emitStore value variableRegisterName] : [emitAlloc variableRegisterName] : instructions
                              Just _ -> [emitStore value variableRegisterName] : instructions
                        in ProcessingResult 
                             newInstructionsList
                             (M.insert identifier variableRegisterName variables)
                             registersUsed
                    TreeProcessingResult expressionInstructions registersUsed' latestRegister ->
                        let newInstructionsList = case variableRegisterNameMaybe of
                              Nothing -> [emitStore latestRegister variableRegisterName] : [emitAlloc variableRegisterName] : expressionInstructions : instructions
                              Just _ -> [emitStore latestRegister variableRegisterName] : expressionInstructions : instructions
                        in ProcessingResult
                             newInstructionsList
                             (M.insert identifier variableRegisterName variables)
                             registersUsed'
        variableRegisterNameMaybe = M.lookup identifier variables
        variableRegisterName = case variableRegisterNameMaybe of
                                 Nothing -> "%variable_" ++ identifier
                                 Just i -> i

processExpression :: Exp -> Variables -> ExpressionProcessingResult -> ExpressionProcessingResult
processExpression (ExpLit value) _ _ = Constant $ show value
processExpression (ExpAdd left right) variables tpr = genericProcessExpression left right emitAdd variables tpr
processExpression (ExpSub left right) variables tpr = genericProcessExpression left right emitSub variables tpr
processExpression (ExpMul left right) variables tpr = genericProcessExpression left right emitMul variables tpr
processExpression (ExpDiv left right) variables tpr = genericProcessExpression left right emitDiv variables tpr
processExpression (ExpVar (PIdent((line, column), identifier))) variables (TreeProcessingResult instructions regUsed _)
    = TreeProcessingResult (emitLoad variableRegister nextRegister : instructions) (regUsed + 1) nextRegister 
    where
        variableRegister = case M.lookup identifier variables of
                            Nothing -> error $ "Can't access identifier " ++ identifier ++ " - undefined in line: " ++ show line ++ " column: " ++ show column
                            Just x -> x
        nextRegister = getNextRegister regUsed
processExpression _ _ _ = undefined

genericProcessExpression :: Exp -> Exp -> BinaryEmitFn -> Variables -> ExpressionProcessingResult -> ExpressionProcessingResult
genericProcessExpression left right emitFn variables tpr@(TreeProcessingResult {})
    = TreeProcessingResult (emitFn leftResultString rightResultString newRegister : instructions) (registersUsed + 1) newRegister
    where
        leftResult = case left of
                        (ExpLit value) -> Constant $ show value
                        _ -> processExpression left variables tpr
        rightResult = case (leftResult, right) of
                        (_, ExpLit value) -> Constant $ show value
                        (Constant _, _) -> processExpression right variables tpr
                        (e'@(TreeProcessingResult {}), _) -> processExpression right variables e'
        leftResultString = case leftResult of
                             (Constant value) -> value
                             (TreeProcessingResult _ _ register) -> register
        rightResultString = case rightResult of
                              (Constant value) -> value
                              (TreeProcessingResult _ _ register) -> register
        TreeProcessingResult instructions registersUsed _ 
            = case (leftResult, rightResult) of
                (_, e'@(TreeProcessingResult {})) -> e'
                (e'@(TreeProcessingResult {}), _) -> e'
                _ -> tpr
        newRegister = getNextRegister registersUsed
genericProcessExpression _ _ _ _ _ = undefined

getNextRegister :: RegistersUsed -> Register
getNextRegister usedCount = "%f_" ++ show usedCount