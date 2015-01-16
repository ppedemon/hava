package abs;

public class B extends A {
  private int i;
  
  public void n() {
    i = 17;
  }
  
  public static void main(String[] argv) {
    B b = new B();
    b.m();
  }
}