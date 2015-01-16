package java.lang;

/*
 * @(#)Throwable.java   1.38 98/08/24
 *
 * Copyright 1994-1998 by Sun Microsystems, Inc.,
 * 901 San Antonio Road, Palo Alto, California, 94303, U.S.A.
 * All rights reserved.
 *
 * This software is the confidential and proprietary information
 * of Sun Microsystems, Inc. ("Confidential Information").  You
 * shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement
 * you entered into with Sun.
 */

/**
 * The <code>Throwable</code> class is the superclass of all errors 
 * and exceptions in the Java language. Only objects that are 
 * instances of this class (or of one of its subclasses) are thrown 
 * by the Java Virtual Machine or can be thrown by the Java 
 * <code>throw</code> statement. Similarly, only this class or one of 
 * its subclasses can be the argument type in a <code>catch</code> 
 * clause. 
 * <p>
 * Instances of two subclasses, {@link java.lang.Error} and 
 * {@link java.lang.Exception}, are conventionally used to indicate 
 * that exceptional situations have occurred. Typically, these instances 
 * are freshly created in the context of the exceptional situation so 
 * as to include relevant information (such as stack trace data).
 * <p>
 * By convention, class <code>Throwable</code> and its subclasses have 
 * two constructors, one that takes no arguments and one that takes a 
 * <code>String</code> argument that can be used to produce an error 
 * message.
 * <p>
 * A <code>Throwable</code> class contains a snapshot of the 
 * execution stack of its thread at the time it was created. It can 
 * also contain a message string that gives more information about 
 * the error. 
 * <p>
 * Here is one example of catching an exception: 
 * <p><blockquote><pre>
 *     try {
 *         int a[] = new int[2];
 *         a[4];
 *     } catch (ArrayIndexOutOfBoundsException e) {
 *         System.out.println("exception: " + e.getMessage());
 *         e.printStackTrace();
 *     }
 * </pre></blockquote>
 *
 * @author  unascribed
 * @version 1.31, 01/26/97
 * @since   JDK1.0
 */
public class Throwable {

    /**
     * Specific details about the Throwable.  For example,
     * for FileNotFoundThrowables, this contains the name of
     * the file that could not be found.
     *
     * @serial
     */
    private String detailMessage;

    /**
     * Constructs a new <code>Throwable</code> with <code>null</code> as 
     * its error message string.
     */
    public Throwable() {
    }
    
    /**
     * Constructs a new <code>Throwable</code> with the specified error 
     * message.
     *
     * @param   message   the error message. The error message is saved for 
     *          later retrieval by the {@link @getMessage()} method.
     */
    public Throwable(String message) {
    detailMessage = message;
    }
    
    /**
     * Creates a localized description of this <code>Throwable</code>.
     * Subclasses may override this method in order to produce a
     * locale-specific message.  For subclasses that do not override this
     * method, the default implementation returns the same result as
     * <code>getMessage()</code>.
     *
     * @since   JDK1.1
     */
    public String getLocalizedMessage() {
    return getMessage();
    }
    
    /**
     * Returns the errort message string of this throwable object.
     *
     * @return  the error message string of this <code>Throwable</code> 
     *          object if it was {@link #Throwable(String) created} with an 
     *          error message string; or <code>null</code> if it was 
     *          {@link #Throwable() created} with no error message. 
     *            
     */
    public String getMessage() {
    return detailMessage;
    }
}
