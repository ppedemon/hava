Compiled from Throwable.java
public class java.lang.Throwable extends java.lang.Object {
    private java.lang.String detailMessage;
    public throwable.Throwable();
    public throwable.Throwable(java.lang.String);
    public java.lang.String getLocalizedMessage();
    public java.lang.String getMessage();
}

Method throwable.Throwable()
   0 aload_0
   1 invokespecial #3 <Method java.lang.Object()>
   4 return

Method throwable.Throwable(java.lang.String)
   0 aload_0
   1 invokespecial #3 <Method java.lang.Object()>
   4 aload_0
   5 aload_1
   6 putfield #4 <Field java.lang.String detailMessage>
   9 return

Method java.lang.String getLocalizedMessage()
   0 aload_0
   1 invokevirtual #5 <Method java.lang.String getMessage()>
   4 areturn

Method java.lang.String getMessage()
   0 aload_0
   1 getfield #4 <Field java.lang.String detailMessage>
   4 areturn
