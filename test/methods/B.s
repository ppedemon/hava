Compiled from B.java
public class test.methods.B extends test.methods.A {
    public static int si;
    protected int i;
    public double pi;
    char name[];
    static {};
    public test.methods.B();
    public test.methods.B(int,int,double);
    public int fact(double, int);
    public java.lang.String getString(java.lang.String[], int) throws test.methods.E;
    public int im(double);
    public int lookupswitch(int);
    public int m();
    public static void main(java.lang.String[]);
    public static int shiftR(int, int);
    public int tableswitch(int);
    private float toFloat(int);
}

Method static {}
   0 iconst_5
   1 putstatic #15 <Field int si>
   4 return

Method test.methods.B()
   0 aload_0
   1 invokespecial #6 <Method test.methods.A()>
   4 return

Method test.methods.B(int,int,double)
   0 aload_0
   1 iload_1
   2 invokespecial #8 <Method test.methods.A(int)>
   5 aload_0
   6 iload_2
   7 putfield #11 <Field int i>
  10 aload_0
  11 dload_3
  12 putfield #14 <Field double pi>
  15 aload_0
  16 iconst_5
  17 newarray char
  19 dup
  20 iconst_0
  21 bipush 80
  23 castore
  24 dup
  25 iconst_1
  26 bipush 97
  28 castore
  29 dup
  30 iconst_2
  31 bipush 98
  33 castore
  34 dup
  35 iconst_3
  36 bipush 108
  38 castore
  39 dup
  40 iconst_4
  41 bipush 111
  43 castore
  44 putfield #13 <Field char name[]>
  47 return

Method int fact(double, int)
   0 iload_3
   1 ifne 11
   4 iconst_1
   5 dload_1
   6 d2i
   7 idiv
   8 goto 21
  11 iload_3
  12 aload_0
  13 dload_1
  14 iload_3
  15 iconst_1
  16 isub
  17 invokevirtual #10 <Method int fact(double, int)>
  20 imul
  21 ireturn

Method java.lang.String getString(java.lang.String[], int)
   0 aload_1
   1 iload_2
   2 aaload
   3 astore_3
   4 jsr 28
   7 aload_3
   8 areturn
   9 pop
  10 new #5 <Class test.methods.E>
  13 dup
  14 ldc #1 <String "Out of Bounds!">
  16 invokespecial #9 <Method test.methods.E(java.lang.String)>
  19 athrow
  20 astore 4
  22 jsr 28
  25 aload 4
  27 athrow
  28 astore 5
  30 aload_0
  31 bipush 17
  33 putfield #11 <Field int i>
  36 ret 5
Exception table:
   from   to  target type
     0     9     9   <Class java.lang.ArrayIndexOutOfBoundsException>
     0    20    20   any

Method int im(double)
   0 dload_1
   1 d2i
   2 istore_3
   3 iload_3
   4 iinc 3 1
   7 ireturn

Method int lookupswitch(int)
   0 iload_1
   1 lookupswitch 3: default=51
	    0: 36
	 1024: 41
	 2048: 46
  36 iconst_1
  37 istore_2
  38 goto 54
  41 iconst_2
  42 istore_2
  43 goto 54
  46 iconst_3
  47 istore_2
  48 goto 54
  51 bipush 9
  53 istore_2
  54 iload_2
  55 ireturn

Method int m()
   0 aload_0
   1 invokespecial #12 <Method int m()>
   4 istore_1
   5 iinc 1 1
   8 iload_1
   9 ireturn

Method void main(java.lang.String[])
   0 new #4 <Class test.methods.B>
   3 dup
   4 invokespecial #7 <Method test.methods.B()>
   7 astore_1
   8 aload_1
   9 aload_1
  10 ldc2_w #16 <Double 0.5>
  13 getstatic #15 <Field int si>
  16 invokevirtual #10 <Method int fact(double, int)>
  19 putfield #11 <Field int i>
  22 return

Method int shiftR(int, int)
   0 iload_0
   1 iload_1
   2 ishr
   3 ireturn

Method int tableswitch(int)
   0 iload_1
   1 tableswitch 0 to 2: default=43
	    0: 28
	    1: 33
	    2: 38
  28 iconst_1
  29 istore_2
  30 goto 46
  33 iconst_2
  34 istore_2
  35 goto 46
  38 iconst_3
  39 istore_2
  40 goto 46
  43 bipush 9
  45 istore_2
  46 iload_2
  47 ireturn

Method float toFloat(int)
   0 iload_1
   1 i2f
   2 freturn
