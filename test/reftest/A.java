package test.reftest;

public class A implements I,L {
  protected static int fs;
  protected int fi;
  
  public static Object sm() {
    return null;
  }
  
  public int im() {
    return 0;
  }
  
  public void jm() {
  }
  
  public boolean km() {
    return false;
  }
  
  public int am(int i) {
    return i++;
  }
  
  private int am1(String s) {
    return s.length();
  }
  
  public String toString() {
    return super.toString();
  }
};
