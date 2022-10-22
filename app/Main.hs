module Main (main) where

import Lib(getProgram)
import Language.Abs
import qualified Data.Map as M
import Data.Foldable

type Instruction = String
type Height = Int
type Variables = M.Map Ident Int

data LocalProcessingResult = LocalProcessingResult [Instruction] Height
data GlobalProcessingResult = GlobalProcessingResult [Instruction] Variables

prog1Path = "/Users/rotten/Learn/Compilers/Instant/instant/programs/prog1.ins"

emitPushConst :: Integer -> String
emitPushConst x = "push" ++ show x

emitStore :: Int -> String
emitStore x 
    | x < 4 = "istore_" ++ show x
    | otherwise = "istore " ++ show x

emitLoad :: Int -> String
emitLoad x
    | x < 4 = "iload_" ++ show x
    | otherwise = "iload " ++ show x

emitAdd = "iadd"
emitSub = "isub"
emitMul = "imul"
emitDiv = "idiv"
emitSwap = "swap"

main :: IO ()
main = generateJVM

generateJVM :: IO ()
generateJVM = program >>= processParsingResult
    where
        program = getProgram prog1Path

processParsingResult :: Either String Program -> IO ()
processParsingResult (Left err) = putStrLn err
processParsingResult (Right program) = mapM_ putStrLn instructions
    where (GlobalProcessingResult instructions _) = processProgram program

processProgram :: Program -> GlobalProcessingResult
processProgram (Prog stmts) = foldl' (flip processStmt) (GlobalProcessingResult [] M.empty) stmts

processStmt :: Stmt -> GlobalProcessingResult -> GlobalProcessingResult
processStmt (SAss identifier expression) (GlobalProcessingResult instructions variables) = 
    processAssignment identifier expression instructions variables
processStmt (SExp expression) (GlobalProcessingResult instructions variables) = GlobalProcessingResult (instructions ++ expressionInstructions) variables 
    where 
       LocalProcessingResult expressionInstructions _ = processExpression expression variables

processAssignment :: Ident -> Exp -> [Instruction] -> Variables -> GlobalProcessingResult
processAssignment identifier expression instructions variables = 
    GlobalProcessingResult (instructions ++ expressionInstructions ++ [emitStore variableIndex]) (M.insert identifier variableIndex variables)
    where 
        LocalProcessingResult expressionInstructions _ = processExpression expression variables
        variableIndex = case M.lookup identifier variables of
                            Nothing -> M.size variables
                            Just i -> i

processExpression :: Exp -> Variables -> LocalProcessingResult
processExpression (ExpLit value) _ = LocalProcessingResult [emitPushConst value] 1
processExpression (ExpAdd left right) variables = genericProcessExpression left right False emitAdd variables
processExpression (ExpSub left right) variables = genericProcessExpression left right True emitSub variables
processExpression (ExpMul left right) variables = genericProcessExpression left right False emitMul variables
processExpression (ExpDiv left right) variables = genericProcessExpression left right True emitDiv variables
processExpression (ExpVar identifier) variables = LocalProcessingResult [emitLoad variableIndex] 1
    where
        variableIndex = case M.lookup identifier variables of
                            Nothing -> error "Can't access identifier: undefined"
                            Just x -> x

genericProcessExpression :: Exp -> Exp -> Bool -> Instruction -> Variables -> LocalProcessingResult
genericProcessExpression left right shouldEmitDup instruction variables =
        if leftHeight >= rightHeight then 
            LocalProcessingResult (leftInstructions ++ rightInstructions ++ [instruction]) (leftHeight + 1)
        else
            LocalProcessingResult (rightInstructions ++ leftInstructions ++ if shouldEmitDup then [emitSwap, instruction] else [instruction]) (rightHeight + 1)
    where
        LocalProcessingResult leftInstructions leftHeight = processExpression left variables
        LocalProcessingResult rightInstructions rightHeight = processExpression right variables