.class  public SumTo 
.super  java/lang/Object 
; standard initializer 
.method public <init>()V 
   aload_0 
   invokespecial java/lang/Object/<init>()V 
   return 
.end method 

.method public static main([Ljava/lang/String;)V
.limit stack 5
.limit locals 2
getstatic  java/lang/System/out Ljava/io/PrintStream;
iconst_3
iconst_4
iadd
iconst_2
iadd
bipush 123
iadd
invokevirtual java/io/PrintStream/println(I)V
getstatic  java/lang/System/out Ljava/io/PrintStream;
iconst_2
iconst_3
imul
iconst_4
idiv
invokevirtual java/io/PrintStream/println(I)V
getstatic  java/lang/System/out Ljava/io/PrintStream;
iconst_2
iconst_3
isub
bipush 112
swap
idiv
bipush 10
imul
invokevirtual java/io/PrintStream/println(I)V
iconst_5
iconst_5
iadd
istore_0
iload_0
iconst_2
idiv
istore_1
getstatic  java/lang/System/out Ljava/io/PrintStream;
iload_1
iload_1
idiv
iload_1
idiv
iload_1
idiv
iload_1
idiv
iload_0
idiv
invokevirtual java/io/PrintStream/println(I)V
return 
.end method
