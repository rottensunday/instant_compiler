module JvmConsts (
    emitPushConst,
    emitStore,
    emitLoad,
    emitAdd,
    emitSub,
    emitMul,
    emitDiv,
    emitSwap,
    emitStack,
    emitVariables,
    programPrefix,
    programSuffix,
    getPrintStream,
    getPrintLn
) where
    
import Data.Int

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

programPrefix x = ".class public " ++ x ++ " \n\
\.super  java/lang/Object \n\

\; standard initializer \n\
\.method public <init>()V \n\
\   aload_0 \n\
\   invokespecial java/lang/Object/<init>()V \n\ 
\   return \n\
\.end method \n\
\\n\
\.method public static main([Ljava/lang/String;)V\n"

programSuffix = "return \n\
\.end method\n"

getPrintStream = "getstatic java/lang/System/out Ljava/io/PrintStream;"

getPrintLn = "invokevirtual java/io/PrintStream/println(I)V"