Compiled from String.java
public final class java.lang.String extends java.lang.Object implements java.lang.Comparable {
    private char value[];
    private int offset;
    private int count;
    public java.lang.String();
    public java.lang.String(java.lang.String);
    public java.lang.String(byte[]);
    public java.lang.String(byte[],int,int);
    public java.lang.String(char[],int,int);
    public char charAt(int);
    public int compareTo(java.lang.Object);
    public int compareTo(java.lang.String);
    public java.lang.String concat(java.lang.String);
    public boolean endsWith(java.lang.String);
    public boolean equals(java.lang.Object);
    public void getChars(int, int, byte[], int);
    public void getChars(int, int, char[], int);
    public int indexOf(int);
    public int indexOf(int, int);
    public int lastIndexOf(int);
    public int lastIndexOf(int, int);
    public int length();
    public java.lang.String replace(char, char);
    public boolean startsWith(java.lang.String);
    public boolean startsWith(java.lang.String, int);
    public java.lang.String substring(int);
    public java.lang.String substring(int, int);
    public char toCharArray()[];
    public java.lang.String trim();
}

Method java.lang.String()
   0 aload_0
   1 invokespecial #4 <Method java.lang.Object()>
   4 aload_0
   5 iconst_0
   6 newarray char
   8 putfield #16 <Field char value[]>
  11 return

Method java.lang.String(java.lang.String)
   0 aload_0
   1 invokespecial #4 <Method java.lang.Object()>
   4 aload_0
   5 aload_1
   6 invokevirtual #12 <Method int length()>
   9 putfield #8 <Field int count>
  12 aload_0
  13 aload_0
  14 getfield #8 <Field int count>
  17 newarray char
  19 putfield #16 <Field char value[]>
  22 aload_1
  23 iconst_0
  24 aload_0
  25 getfield #8 <Field int count>
  28 aload_0
  29 getfield #16 <Field char value[]>
  32 iconst_0
  33 invokevirtual #9 <Method void getChars(int, int, char[], int)>
  36 return

Method java.lang.String(byte[])
   0 aload_0
   1 aload_1
   2 iconst_0
   3 aload_1
   4 arraylength
   5 invokespecial #5 <Method java.lang.String(byte[],int,int)>
   8 return

Method java.lang.String(byte[],int,int)
   0 aload_0
   1 invokespecial #4 <Method java.lang.Object()>
   4 iload_2
   5 istore 4
   7 goto 25
  10 aload_0
  11 getfield #16 <Field char value[]>
  14 iload 4
  16 aload_1
  17 iload 4
  19 baload
  20 i2c
  21 castore
  22 iinc 4 1
  25 iload 4
  27 iload_3
  28 if_icmplt 10
  31 aload_0
  32 iload_2
  33 putfield #13 <Field int offset>
  36 aload_0
  37 iload_3
  38 putfield #8 <Field int count>
  41 return

Method java.lang.String(char[],int,int)
   0 aload_0
   1 invokespecial #4 <Method java.lang.Object()>
   4 iload_2
   5 istore 4
   7 goto 24
  10 aload_0
  11 getfield #16 <Field char value[]>
  14 iload 4
  16 aload_1
  17 iload 4
  19 caload
  20 castore
  21 iinc 4 1
  24 iload 4
  26 iload_3
  27 if_icmplt 10
  30 aload_0
  31 iload_2
  32 putfield #13 <Field int offset>
  35 aload_0
  36 iload_3
  37 putfield #8 <Field int count>
  40 return

Method char charAt(int)
   0 aload_0
   1 getfield #16 <Field char value[]>
   4 iload_1
   5 aload_0
   6 getfield #13 <Field int offset>
   9 iadd
  10 caload
  11 ireturn

Method int compareTo(java.lang.Object)
   0 aload_0
   1 aload_1
   2 checkcast #3 <Class java.lang.String>
   5 invokevirtual #7 <Method int compareTo(java.lang.String)>
   8 ireturn

Method int compareTo(java.lang.String)
   0 aload_0
   1 getfield #8 <Field int count>
   4 istore_2
   5 aload_1
   6 getfield #8 <Field int count>
   9 istore_3
  10 iload_2
  11 iload_3
  12 if_icmpgt 19
  15 iload_2
  16 goto 20
  19 iload_3
  20 istore 4
  22 aload_0
  23 getfield #16 <Field char value[]>
  26 astore 5
  28 aload_1
  29 getfield #16 <Field char value[]>
  32 astore 6
  34 aload_0
  35 getfield #13 <Field int offset>
  38 istore 7
  40 aload_1
  41 getfield #13 <Field int offset>
  44 istore 8
  46 goto 82
  49 aload 5
  51 iload 7
  53 iinc 7 1
  56 caload
  57 istore 9
  59 aload 6
  61 iload 8
  63 iinc 8 1
  66 caload
  67 istore 10
  69 iload 9
  71 iload 10
  73 if_icmpeq 82
  76 iload 9
  78 iload 10
  80 isub
  81 ireturn
  82 iload 4
  84 iinc 4 -1
  87 ifne 49
  90 iload_2
  91 iload_3
  92 isub
  93 ireturn

Method java.lang.String concat(java.lang.String)
   0 aload_1
   1 invokevirtual #12 <Method int length()>
   4 istore_2
   5 iload_2
   6 ifne 11
   9 aload_0
  10 areturn
  11 aload_0
  12 getfield #8 <Field int count>
  15 iload_2
  16 iadd
  17 newarray char
  19 astore_3
  20 aload_0
  21 iconst_0
  22 aload_0
  23 getfield #8 <Field int count>
  26 aload_3
  27 iconst_0
  28 invokevirtual #9 <Method void getChars(int, int, char[], int)>
  31 aload_1
  32 iconst_0
  33 iload_2
  34 aload_3
  35 aload_0
  36 getfield #8 <Field int count>
  39 invokevirtual #9 <Method void getChars(int, int, char[], int)>
  42 new #3 <Class java.lang.String>
  45 dup
  46 aload_3
  47 iconst_0
  48 aload_0
  49 getfield #8 <Field int count>
  52 iload_2
  53 iadd
  54 invokespecial #6 <Method java.lang.String(char[],int,int)>
  57 areturn

Method boolean endsWith(java.lang.String)
   0 aload_0
   1 aload_1
   2 aload_0
   3 getfield #8 <Field int count>
   6 aload_1
   7 getfield #8 <Field int count>
  10 isub
  11 invokevirtual #14 <Method boolean startsWith(java.lang.String, int)>
  14 ireturn

Method boolean equals(java.lang.Object)
   0 aload_0
   1 aload_1
   2 if_acmpne 7
   5 iconst_1
   6 ireturn
   7 aload_1
   8 ifnull 93
  11 aload_1
  12 instanceof #3 <Class java.lang.String>
  15 ifeq 93
  18 aload_1
  19 checkcast #3 <Class java.lang.String>
  22 astore_2
  23 aload_0
  24 getfield #8 <Field int count>
  27 istore_3
  28 iload_3
  29 aload_2
  30 getfield #8 <Field int count>
  33 if_icmpne 93
  36 aload_0
  37 getfield #16 <Field char value[]>
  40 astore 4
  42 aload_2
  43 getfield #16 <Field char value[]>
  46 astore 5
  48 aload_0
  49 getfield #13 <Field int offset>
  52 istore 6
  54 aload_2
  55 getfield #13 <Field int offset>
  58 istore 7
  60 goto 84
  63 aload 4
  65 iload 6
  67 iinc 6 1
  70 caload
  71 aload 5
  73 iload 7
  75 iinc 7 1
  78 caload
  79 if_icmpeq 84
  82 iconst_0
  83 ireturn
  84 iload_3
  85 iinc 3 -1
  88 ifne 63
  91 iconst_1
  92 ireturn
  93 iconst_0
  94 ireturn

Method void getChars(int, int, byte[], int)
   0 iload_1
   1 istore 5
   3 goto 26
   6 aload_3
   7 iload 4
   9 iload 5
  11 iadd
  12 iload_1
  13 isub
  14 aload_0
  15 getfield #16 <Field char value[]>
  18 iload 5
  20 caload
  21 i2b
  22 bastore
  23 iinc 5 1
  26 iload 5
  28 iload_2
  29 if_icmplt 6
  32 return

Method void getChars(int, int, char[], int)
   0 iload_1
   1 istore 5
   3 goto 25
   6 aload_3
   7 iload 4
   9 iload 5
  11 iadd
  12 iload_1
  13 isub
  14 aload_0
  15 getfield #16 <Field char value[]>
  18 iload 5
  20 caload
  21 castore
  22 iinc 5 1
  25 iload 5
  27 iload_2
  28 if_icmplt 6
  31 return

Method int indexOf(int)
   0 aload_0
   1 iload_1
   2 iconst_0
   3 invokevirtual #10 <Method int indexOf(int, int)>
   6 ireturn

Method int indexOf(int, int)
   0 aload_0
   1 getfield #13 <Field int offset>
   4 aload_0
   5 getfield #8 <Field int count>
   8 iadd
   9 istore_3
  10 aload_0
  11 getfield #16 <Field char value[]>
  14 astore 4
  16 iload_2
  17 ifge 25
  20 iconst_0
  21 istore_2
  22 goto 35
  25 iload_2
  26 aload_0
  27 getfield #8 <Field int count>
  30 if_icmplt 35
  33 iconst_m1
  34 ireturn
  35 aload_0
  36 getfield #13 <Field int offset>
  39 iload_2
  40 iadd
  41 istore 5
  43 goto 66
  46 aload 4
  48 iload 5
  50 caload
  51 iload_1
  52 if_icmpne 63
  55 iload 5
  57 aload_0
  58 getfield #13 <Field int offset>
  61 isub
  62 ireturn
  63 iinc 5 1
  66 iload 5
  68 iload_3
  69 if_icmplt 46
  72 iconst_m1
  73 ireturn

Method int lastIndexOf(int)
   0 aload_0
   1 iload_1
   2 aload_0
   3 getfield #8 <Field int count>
   6 iconst_1
   7 isub
   8 invokevirtual #11 <Method int lastIndexOf(int, int)>
  11 ireturn

Method int lastIndexOf(int, int)
   0 aload_0
   1 getfield #13 <Field int offset>
   4 istore_3
   5 aload_0
   6 getfield #16 <Field char value[]>
   9 astore 4
  11 aload_0
  12 getfield #13 <Field int offset>
  15 iload_2
  16 aload_0
  17 getfield #8 <Field int count>
  20 if_icmplt 32
  23 aload_0
  24 getfield #8 <Field int count>
  27 iconst_1
  28 isub
  29 goto 33
  32 iload_2
  33 iadd
  34 istore 5
  36 goto 59
  39 aload 4
  41 iload 5
  43 caload
  44 iload_1
  45 if_icmpne 56
  48 iload 5
  50 aload_0
  51 getfield #13 <Field int offset>
  54 isub
  55 ireturn
  56 iinc 5 -1
  59 iload 5
  61 iload_3
  62 if_icmpge 39
  65 iconst_m1
  66 ireturn

Method int length()
   0 aload_0
   1 getfield #8 <Field int count>
   4 ireturn

Method java.lang.String replace(char, char)
   0 iload_1
   1 iload_2
   2 if_icmpeq 143
   5 aload_0
   6 getfield #8 <Field int count>
   9 istore_3
  10 iconst_m1
  11 istore 4
  13 aload_0
  14 getfield #16 <Field char value[]>
  17 astore 5
  19 aload_0
  20 getfield #13 <Field int offset>
  23 istore 6
  25 goto 43
  28 aload 5
  30 iload 6
  32 iload 4
  34 iadd
  35 caload
  36 iload_1
  37 if_icmpne 43
  40 goto 52
  43 iinc 4 1
  46 iload 4
  48 iload_3
  49 if_icmplt 28
  52 iload 4
  54 iload_3
  55 if_icmpge 143
  58 iload_3
  59 newarray char
  61 astore 7
  63 iconst_0
  64 istore 8
  66 goto 85
  69 aload 7
  71 iload 8
  73 aload 5
  75 iload 6
  77 iload 8
  79 iadd
  80 caload
  81 castore
  82 iinc 8 1
  85 iload 8
  87 iload 4
  89 if_icmplt 69
  92 goto 125
  95 aload 5
  97 iload 6
  99 iload 4
 101 iadd
 102 caload
 103 istore 9
 105 aload 7
 107 iload 4
 109 iload 9
 111 iload_1
 112 if_icmpne 119
 115 iload_2
 116 goto 121
 119 iload 9
 121 castore
 122 iinc 4 1
 125 iload 4
 127 iload_3
 128 if_icmplt 95
 131 new #3 <Class java.lang.String>
 134 dup
 135 aload 7
 137 iconst_0
 138 iload_3
 139 invokespecial #6 <Method java.lang.String(char[],int,int)>
 142 areturn
 143 aload_0
 144 areturn

Method boolean startsWith(java.lang.String)
   0 aload_0
   1 aload_1
   2 iconst_0
   3 invokevirtual #14 <Method boolean startsWith(java.lang.String, int)>
   6 ireturn

Method boolean startsWith(java.lang.String, int)
   0 aload_0
   1 getfield #16 <Field char value[]>
   4 astore_3
   5 aload_0
   6 getfield #13 <Field int offset>
   9 iload_2
  10 iadd
  11 istore 4
  13 aload_0
  14 getfield #13 <Field int offset>
  17 aload_0
  18 getfield #8 <Field int count>
  21 iadd
  22 istore 5
  24 aload_1
  25 getfield #16 <Field char value[]>
  28 astore 6
  30 aload_1
  31 getfield #13 <Field int offset>
  34 istore 7
  36 aload_1
  37 getfield #8 <Field int count>
  40 istore 8
  42 iload_2
  43 iflt 57
  46 iload_2
  47 aload_0
  48 getfield #8 <Field int count>
  51 iload 8
  53 isub
  54 if_icmple 79
  57 iconst_0
  58 ireturn
  59 aload_3
  60 iload 4
  62 iinc 4 1
  65 caload
  66 aload 6
  68 iload 7
  70 iinc 7 1
  73 caload
  74 if_icmpeq 79
  77 iconst_0
  78 ireturn
  79 iinc 8 -1
  82 iload 8
  84 ifge 59
  87 iconst_1
  88 ireturn

Method java.lang.String substring(int)
   0 aload_0
   1 iload_1
   2 aload_0
   3 getfield #8 <Field int count>
   6 invokevirtual #15 <Method java.lang.String substring(int, int)>
   9 areturn

Method java.lang.String substring(int, int)
   0 iload_1
   1 ifne 16
   4 iload_2
   5 aload_0
   6 getfield #8 <Field int count>
   9 if_icmpne 16
  12 aload_0
  13 goto 36
  16 new #3 <Class java.lang.String>
  19 dup
  20 aload_0
  21 getfield #16 <Field char value[]>
  24 aload_0
  25 getfield #13 <Field int offset>
  28 iload_1
  29 iadd
  30 iload_2
  31 iload_1
  32 isub
  33 invokespecial #6 <Method java.lang.String(char[],int,int)>
  36 areturn

Method char toCharArray()[]
   0 aload_0
   1 getfield #8 <Field int count>
   4 newarray char
   6 astore_1
   7 aload_0
   8 iconst_0
   9 aload_0
  10 getfield #8 <Field int count>
  13 aload_1
  14 iconst_0
  15 invokevirtual #9 <Method void getChars(int, int, char[], int)>
  18 aload_1
  19 areturn

Method java.lang.String toString()
   0 aload_0
   1 areturn

Method java.lang.String trim()
   0 aload_0
   1 getfield #8 <Field int count>
   4 istore_1
   5 iconst_0
   6 istore_2
   7 aload_0
   8 getfield #13 <Field int offset>
  11 istore_3
  12 aload_0
  13 getfield #16 <Field char value[]>
  16 astore 4
  18 goto 24
  21 iinc 2 1
  24 iload_2
  25 iload_1
  26 if_icmpge 46
  29 aload 4
  31 iload_3
  32 iload_2
  33 iadd
  34 caload
  35 bipush 32
  37 if_icmple 21
  40 goto 46
  43 iinc 1 -1
  46 iload_2
  47 iload_1
  48 if_icmpge 64
  51 aload 4
  53 iload_3
  54 iload_1
  55 iadd
  56 iconst_1
  57 isub
  58 caload
  59 bipush 32
  61 if_icmple 43
  64 iload_2
  65 ifgt 76
  68 iload_1
  69 aload_0
  70 getfield #8 <Field int count>
  73 if_icmpge 85
  76 aload_0
  77 iload_2
  78 iload_1
  79 invokevirtual #15 <Method java.lang.String substring(int, int)>
  82 goto 86
  85 aload_0
  86 areturn
