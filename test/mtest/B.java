package test.mtest;

public class B extends A implements K {
  public void a1() {}
  public int  a2(String s) { return s.length(); }
  public void m2(long l)   { l--; }
  public void m3(float f)  { f++; }
  public void m4(double d) { d *= 2; }
  public void k1() {}
  public void k2(Object o) { System.out.println(o.toString()); };
}
