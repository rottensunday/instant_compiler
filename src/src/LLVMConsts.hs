module LLVMConsts (
    emitAlloc,
    emitStore,
    emitLoad,
    emitAdd,
    emitSub,
    emitMul,
    emitDiv,
    BinaryEmitFn,
    programPrefix,
    programSuffix,
    emitPrint
) where

type BinaryEmitFn = (String -> String -> String -> String)

emitAlloc :: String -> String
emitAlloc x = x ++ " = alloca i32"

emitStore :: String -> String -> String
emitStore source dest = "store i32 " ++ source ++ ", i32* " ++ dest

emitLoad :: String -> String -> String
emitLoad source dest = dest ++ " = load i32, i32* " ++ source

emitAdd :: String -> String -> String -> String
emitAdd = genericEmitBinary "add"

emitSub :: String -> String -> String -> String
emitSub = genericEmitBinary "sub"

emitMul :: String -> String -> String -> String
emitMul = genericEmitBinary "mul"

emitDiv :: String -> String -> String -> String
emitDiv = genericEmitBinary "sdiv"

genericEmitBinary :: String -> String -> String -> String -> String
genericEmitBinary op left right dest = dest ++ " = " ++ op ++ " i32 " ++ left ++ ", " ++ right

emitPrint :: String -> String
emitPrint register = "call void @printInt(i32 " ++ register ++ ")"

programPrefix = "declare i32 @printf(i8*, ...) \n\
\@dnl = internal constant [4 x i8] c\"%d\\0A\\00\" \n\
\define void @printInt(i32 %x) { \n\
\       %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0 \n\
\       call i32 (i8*, ...) @printf(i8* %t0, i32 %x) \n\
\       ret void \n\
\} \n\
\define i32 @main() { \n"

programSuffix = "\n\
\ret i32 0 \n\
\} "