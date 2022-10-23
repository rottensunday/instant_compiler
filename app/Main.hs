module Main (main) where

import Lib(getProgram)
import Language.Abs
import qualified Data.Map as M
import Data.Foldable
import Data.List
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type Instruction = String
type Height = Int
type Variables = M.Map Ident Int

data LocalProcessingResult = LocalProcessingResult [Instruction] Height
data GlobalProcessingResult = GlobalProcessingResult [Instruction] Variables

prog1Path = "/Users/rotten/Learn/Compilers/Instant/instant/programs/prog1.ins"
prog1JvmResultPath = "/Users/rotten/Learn/Compilers/Instant/instant/programs/prog1.j"

emitPushConst :: Integer -> String
emitPushConst x 
    | x < 6 = "iconst_" ++ show x
    | x == -1 = "iconst_m1"
    | x >= fromIntegral (minBound :: Int8) && x <= fromIntegral (maxBound :: Int8) =  "bipush " ++ show x
    | x >= fromIntegral (minBound :: Int16) && x <= fromIntegral (maxBound :: Int16) =  "sipush " ++ show x
    | otherwise =  "ldc " ++ show x

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

emitStack :: Int -> String
emitStack x = ".limit stack " ++ show x

emitVariables :: Int -> String
emitVariables x = ".limit locals " ++ show x

programPrefix = ".class  public SumTo \n\
\.super  java/lang/Object \n\

\; standard initializer \n\
\.method public <init>()V \n\
\   aload_0 \n\
\   invokespecial java/lang/Object/<init>()V \n\ 
\   return \n\
\.end method \n\
\\n\
\.method public static main([Ljava/lang/String;)V\n"

programPostfix = "return \n\
\.end method\n"

main :: IO ()
main = generateJVM

generateJVM :: IO ()
generateJVM = TIO.writeFile prog1JvmResultPath . T.pack . processParsingResult =<< program
    where
        program = getProgram prog1Path


processParsingResult :: Either String Program -> String
processParsingResult (Left err) = err
processParsingResult (Right program) = programPrefix ++ unlines [emitStack stackSize, emitVariables variablesCount] ++ unlines instructions ++ programPostfix
    where 
        (GlobalProcessingResult instructions variables) = processProgram program
        variablesCount = M.size variables
        stackSize = determineStackSize instructions [0]

determineStackSize :: [Instruction] -> [Int] -> Int
determineStackSize [] res = maximum res
determineStackSize (x:xs) res
    | any (`isPrefixOf` x) ["iconst", "bipush", "sipush", "ldc", "iload"] = determineStackSize xs (head res + 1 : res)
    | any (`isPrefixOf` x) ["iadd", "isub", "imul", "idiv", "istore"] = determineStackSize xs (head res - 1 : res)
    | otherwise = determineStackSize xs res

processProgram :: Program -> GlobalProcessingResult
processProgram (Prog stmts) = foldl' (flip processStmt) (GlobalProcessingResult [] M.empty) stmts

processStmt :: Stmt -> GlobalProcessingResult -> GlobalProcessingResult
processStmt (SAss identifier expression) (GlobalProcessingResult instructions variables) = 
    processAssignment identifier expression instructions variables
processStmt (SExp expression) (GlobalProcessingResult instructions variables) = GlobalProcessingResult (instructions ++ ["getstatic  java/lang/System/out Ljava/io/PrintStream;"] ++ expressionInstructions ++ ["invokevirtual java/io/PrintStream/println(I)V"]) variables 
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