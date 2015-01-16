Compiled from A.java
public class methods.A extends java.lang.Object implements methods.I {
    protected static final int si;
    protected int i;
    public methods.A();
    public methods.A(int);
    public int fact(double, int);
    public int im(double);
    public int m();
    public static int modulus(java.lang.String[], int, int);
}

Method methods.A()
   0 aload_0
   1 invokespecial #6 <Method java.lang.Object()>
   4 return

Method methods.A(int)
   0 aload_0
   1 invokespecial #6 <Method java.lang.Object()>
   4 aload_0
   5 iload_1
   6 putfield #7 <Field int i>
   9 return

Method int fact(double, int)
   0 iconst_0
   1 ireturn

Method int im(double)
   0 iconst_0
   1 ireturn

Method int m()
   0 bipush 50
   2 ireturn

Method int modulus(java.lang.String[], int, int)
   0 aload_0
   1 iconst_2
   2 ldc #2 <String "Yeah!">
   4 aastore
   5 iload_1
   6 iload_2
   7 irem
   8 ireturn
