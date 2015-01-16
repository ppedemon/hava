package test.reftest;

public class B extends A {
  private static int fs;
  private int fi;
  public byte b;
  
  public static Object sm () {
    return new Object();
  }
  
  protected static double bsm() {
    return Math.PI;
  }
  
  public int am(int i) {
    return 0;
  }
  
  protected void bm() {
    fi++;
  }
};
