package test.methods;

public class A implements I {

  protected final static int si = 1;

  protected int i;

  public A() {
    super();
  }

  public A(int i) {
    this.i = i;
  }

  public int m() {
    return 50;
  }

  public int fact(double d, int i) { return 0; }
  
  public int im(double d) { return 0; }

  public static int modulus(String[] argv, int i, int j) {
    argv[2] = "Yeah!";
    return i % j;
  }
}
