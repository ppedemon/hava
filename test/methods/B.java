package test.methods;

public class B extends A {

  public static int si;
  
  protected int i;
  public double pi;
  char[] name;

  static {
    try {
      si = (A.si + 3) / 0;
    } 
    catch(Exception e) {
      si = 3;
    }
    finally {
      si = 5;
    }
  }

  public static int shiftR(int i, int j) {
    return i >> j;
  }

  public B() {
    super();
  }
  
  public B(int i1, int i2, double pi) {
    super(i1);
    this.i = i2;
    this.pi = pi;
    name = new char[] {'P','a','b','l','o' };
  }
  
  private float toFloat(int i) {
    return (float)i;
  }
  
  public int m() {
    int i = super.m();
    return ++i;
  }
  
  public int im(double d) {
    int i = (int)d;
    return i++;
  }
  
  public int fact(double d, int i) {
    return i == 0? 1/(int)d : i * fact(d,i - 1);
  }
  
  public String getString(String[] argv, int i) throws E {
  try {
      return argv[i];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      throw new E("Out of Bounds!");
    }
    finally {
      this.i = 17;
    }
  }
  
  public int tableswitch(int i) {
    int j;
  
    switch(i) {
      case 0 : j = 1; break;
      case 1 : j = 2; break;
      case 2 : j = 3; break;
      default: j = 9; break;
    }
    
    return j;
  }
  
  public int lookupswitch(int i) {
    int j;
  
    switch(i) {
      case 0   : j = 1; break;
      case 1024: j = 2; break;
      case 2048: j = 3; break;
      default  : j = 9; break;
    }
    
    return j;
  }
  
  
  public static void main(String argv[]) throws E {
    B b = new B();
    b.i = b.fact(0.5,B.shiftR(B.si,1));
    //throw new E("Error");
  }
    
  /*
  public static void main(String[] argv) throws E {
    B b = new B();
    b.i = b.lookupswitch(2048);
  }
  */
  
  /*
  public static void main(String[] argv) throws E {
    int[][][] a = new int[4][3][2];
    
    for (int i = 0; i < a.length; i++)
      for (int j = 0; j< a[i].length; j++)
        for (int k = 0; k < a[i][j].length; k++)
          a[i][j][k] = i + j + k;
  }
  */
  
  /*
  public static void main(String[] argv) throws E {
    B b = new B();
    String res = b.getString(argv,0);
  }
  */
  
  /*
  public static void main(String[] argv) throws E {
    B b1 = new B(1, 2, 3.14);
    boolean[] b = new boolean[] {false, false, false, true, true};
    long [] l = new long[3];
    String[] s = new String[] {"AA","BB"};
    A[] as = new A[2];
    
    for (int i = 0; i < 3;)
      l[i++] = i;
  }
  */
  
  /*
  public static void main(String[] argv) {
    Object a = new A();
    Object b = new B();
    B res    = new B();
    
    if (a instanceof A)      res.i++;
    if (a instanceof B)      res.i++;
    if (a instanceof Object) res.i++;
    // res.i = 2
    if (b instanceof A) res.i++;
    if (b instanceof I) res.i++;
    // res.i = 4
    Object[] o = (Object[])argv; 
    if (o instanceof Object)     res.i++;
    if (o instanceof Cloneable)  res.i++;
    if (o instanceof Object[])   res.i++;
    if (o instanceof String[])   res.i++;
    if (o instanceof A[])        res.i++; //Fails!
    if (o instanceof Object[][]) res.i++; // Fails!
    //res.i = 8
  }
  */
  
  /*
  public static void main(String[] argv) {
    B b  = new B(1,2,Math.PI);
    argv[b.fact(Math.E,((A)b).i + b.i) % 3] = "Fact";
  }
  */
    
  /*
  public static void main(String[] argv) {
    //A.si = 3;
    B.si = A.si + 1;
    A b = new B();
    argv[b.fact(Math.PI,B.si) % 3] = "Fact";
    argv[b.fact(Math.PI,++B.si) % 3] = "Fact";
  }
  */
  
  /*
  public static void main(String[] argv) {
    I it = new B();
    I it1 = new A();
    B b1 = new B();
    int i = b1.m();
    int j = it.im(Math.PI);
    argv[b1.fact(Math.PI,j) % 3] = "Fact";
    argv[it1.im(Math.PI)] = "PT"; 
    //float f = b1.toFloat(8);
  }
  */
  
  /*
  public static void main(String[] argv) {
    argv[B.shiftR(3,1)] = "It";
    
    try {
      argv[B.modulus(argv,4,2)] = "Works!";
      argv[shiftR(3,1) - (-1)]  = "Again!";
    }
    catch(ArrayIndexOutOfBoundsException e) {
      argv[0] = null;
    }
  }
  */
}
