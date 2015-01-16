package java.lang;

/**
 * An ad-hoc over-simplfied <code>String</code> class
 * suitable to be loaded for our naive implementation
 * of the JVM in Haskell.
 *
 * @see java.lang.String
 * @version 1.0 11/28/01
 * @author Pablo J. Pedemonte
 */

public final class String implements Comparable {
    /** value is used for character storage. */
    private char value[];

    /** offset is the first index of the storage that is used. */
    private int offset;

    /** count is the number of characters in the String. */
    private int count;
    
    /**
     * Initializes a newly created <code>String</code> object so that it 
     * represents an empty character sequence. 
     */
    public String() {
        value = new char[0];
    }
    
    /**
     * Construct a new <code>String</code> by converting the specified array
     * of bytes.
     *
     * @param bytes The bytes to be converted into characters
     */
    public String(byte bytes[]) {
        this(bytes, 0, bytes.length);
    }
    
    /** 
     * Construct a <code>String</code> from a byte array.
     * To mantain the implementation simple, be optimistic
     * about the indices.
     *
     * @param value  the byte array to use to contruct the string
     * @param offset the initial offset of the byte array
     * @param count  the number of bytes to convert to characters
     */
     public String(byte[] value, int offset, int count) {
         for (int i = offset; i < count; i++) {
             this.value[i] = (char)value[i];
         }
         this.offset = offset;
         this.count = count;
     }
     
     /** 
      * Construct a <code>String</code> from a char array.
      * To mantain the implementation simple, be optimistic
      * about the indices.
      *
      * @param value  the char array to use to contruct the string
      * @param offset the initial offset of the byte array
      * @param count  the number of bytes to convert to characters
      */
     public String(char[] value, int offset, int count) {
              for (int i = offset; i < count; i++) {
                  this.value[i] = value[i];
              }
              this.offset = offset;
              this.count = count;
     }
     
     /**
      * Initializes a newly created <code>String</code> object so that it 
      * represents the same sequence of characters as the argument; in other 
      * words, the newly created string is a copy of the argument string. 
      *
      * @param value a <code>String</code>.
      */
    public String(String value) {
        count = value.length();
        this.value = new char[count];
        value.getChars(0, count, this.value, 0);
    }
    
    /**
     * Returns the character at the specified index. An index ranges
     * from <code>0</code> to <code>length() - 1</code>. The first character 
     * of the sequence is at index <code>0</code>, the next at index 
     * <code>1</code>, and so on, as for array indexing. 
     * <p> To maintain this simple, we are optimistic about indexes.
     *
     * @param      index   the index of the character.
     * @return     the character at the specified index of this string.
     *             The first character is at index <code>0</code>.
     */
    public char charAt(int index) {
        return value[index + offset];
    }
    
    /**
     * Compares this String to another Object.  If the Object is a String,
     * this function behaves like <code>compareTo(String)</code>.  Otherwise,
     * it throws a <code>ClassCastException</code> (as Strings are comparable
     * only to other Strings).
     *
     * @param   o the <code>Object</code> to be compared.
     * @return  the value <code>0</code> if the argument is a string
     *      lexicographically equal to this string; a value less than
     *      <code>0</code> if the argument is a string lexicographically 
     *      greater than this string; and a value greater than
     *      <code>0</code> if the argument is a string lexicographically
     *      less than this string.
     * @exception <code>ClassCastException</code> if the argument is not a
     *        <code>String</code>. 
     * @see     java.lang.Comparable
     */
    public int compareTo(Object o) {
        return compareTo((String)o);
    }
    
    /**
     * Compare two <code>Strings</code> lexicographically.
     */
    public int compareTo(String anotherString) {
        int len1 = count;
        int len2 = anotherString.count;
        int n = len1 <= len2? len1 : len2;
        char v1[] = value;
        char v2[] = anotherString.value;
        int i = offset;
        int j = anotherString.offset;
    
        while (n-- != 0) {
            char c1 = v1[i++];
            char c2 = v2[j++];
            if (c1 != c2) {
            return c1 - c2;
            }
        }
        return len1 - len2;
    }
    
    /** 
     * Concatenate the given <code>String</code> to
     * the receiver. This operation updates the receiver.
     */
     public String concat(String str) {
         int otherLen = str.length();
         if (otherLen == 0) {
             return this;
         }
         char buf[] = new char[count + otherLen];
         getChars(0, count, buf, 0);
         str.getChars(0, otherLen, buf, count);
         return new String(buf,0, count + otherLen);
    }
    
    /**
     * Tests if this string ends with the specified suffix.
     *
     * @param   suffix   the suffix.
     * @return  <code>true</code> if the character sequence represented by the
     *          argument is a suffix of the character sequence represented by
     *          this object; <code>false</code> otherwise. Note that the 
     *          result will be <code>true</code> if the argument is the 
     *          empty string or is equal to this <code>String</code> object 
     *          as determined by the {@link #equals(Object)} method.
     * @exception java.lang.NullPointerException if <code>suffix</code> is 
     *          <code>null</code>.
     */
    public boolean endsWith(String suffix) {
         return startsWith(suffix, count - suffix.count);
    }
    
    /**
     * Compares the receiver for equallity with 
     * another <code>String</code>
     */
     public boolean equals(Object anObject) {
         if (this == anObject) {
             return true;
         }
         if ((anObject != null) && (anObject instanceof String)) {
             String anotherString = (String)anObject;
             int n = count;
             if (n == anotherString.count) {
             char v1[] = value;
             char v2[] = anotherString.value;
             int i = offset;
             int j = anotherString.offset;
             while (n-- != 0) {
                 if (v1[i++] != v2[j++]) {
                 return false;
                 }
             }
             return true;
             }
         }
         return false;
    }
    
    /**
     * Get the chars that form this <code>String</code>.
     * As usual, be optimistic about the indexes.
     */
    public void getChars(int srcBegin, int srcEnd, char dst[], int dstBegin) {
        for (int i = srcBegin; i < srcEnd; i++) {
            dst[dstBegin + i - srcBegin] = value[i];
        }
    }
    
    /**
     * Get the bytes that form this <code>String</code>.
     * As usual, be optimistic about the indexes.
     */
    public void getChars(int srcBegin, int srcEnd, byte dst[], int dstBegin) {
        for (int i = srcBegin; i < srcEnd; i++) {
            dst[dstBegin + i - srcBegin] = (byte)value[i];
        }
    }
    
    /**
     * Return the first ocurrence of the given character
     * in the receiver
     */
    public int indexOf(int ch) {
        return indexOf(ch, 0);
    }
    
    /**
     * Return the first ocurrence of the given character
     * in the receiver, starting from the given index.
     */
    public int indexOf(int ch, int fromIndex) {
        int max = offset + count;
        char v[] = value;
    
        if (fromIndex < 0) {
            fromIndex = 0;
        } else if (fromIndex >= count) {
            // Note: fromIndex might be near -1>>>1.
            return -1;
        }
        for (int i = offset + fromIndex ; i < max ; i++) {
            if (v[i] == ch) {
            return i - offset;
            }
        }
        return -1;
    }
        
    /**
     * Return the last ocurrence of the given character in
     * the receiver <code>String</code>.
     */
    public int lastIndexOf(int ch) {
        return lastIndexOf(ch, count - 1);
    }
    
    /**
     * Return the last ocurrence of the given character in
     * the receiver <code>String</code>, starting from the
     * given index.
     */
    public int lastIndexOf(int ch, int fromIndex) {
        int min = offset;
        char v[] = value;
        
        for (int i = offset + ((fromIndex >= count) ? count - 1 : fromIndex) ; i >= min ; i--) {
            if (v[i] == ch) {
            return i - offset;
            }
        }
        return -1;
    }
    
    /**
     * Returns the length of this string.
     * The length is equal to the number of 16-bit
     * Unicode characters in the string.
     *
     * @return  the length of the sequence of characters represented by this
     *          object.
     */
    public int length() {
        return count;
    }
    
    /**
     * Replace the given char with a new one
     */
    public String replace(char oldChar, char newChar) {
        if (oldChar != newChar) {
            int len = count;
            int i = -1;
            char[] val = value; /* avoid getfield opcode */
            int off = offset;   /* avoid getfield opcode */
    
            while (++i < len) {
            if (val[off + i] == oldChar) {
                break;
            }
            }
            if (i < len) {
            char buf[] = new char[len];
            for (int j = 0 ; j < i ; j++) {
                buf[j] = val[off+j];
            }
            while (i < len) {
                char c = val[off + i];
                buf[i] = (c == oldChar) ? newChar : c;
                i++;
            }
            return new String(buf, 0, len);
            }
        }
        return this;
    }
    
    /**
     * Does the receiver starts with the given prefix?
     */
    public boolean startsWith(String prefix) {
        return startsWith(prefix, 0);
    }
    
    /**
     * Return true if the receiver starts with the given
     * prefix, starting the test at the given offset.
     */
    public boolean startsWith(String prefix, int toffset) {
        char ta[] = value;
        int to = offset + toffset;
        int tlim = offset + count;
        char pa[] = prefix.value;
        int po = prefix.offset;
        int pc = prefix.count;
        // Note: toffset might be near -1>>>1.
        if ((toffset < 0) || (toffset > count - pc)) {
            return false;
        }
        while (--pc >= 0) {
            if (ta[to++] != pa[po++]) {
                return false;
            }
        }
        return true;
    }
    
    /**
     * Return a substring of the receiver, from
     * beginIndex up to the length of the string - 1.
     */
    public String substring(int beginIndex) {
        return substring(beginIndex, count);
    }
    
    /**
     * Return a substring of the receiver, from 
     * beginIndex (inclusive) up to endIndex - 1.
     */
    public String substring(int beginIndex, int endIndex) {
        return ((beginIndex == 0) && (endIndex == count)) ? this :
            new String(value,offset + beginIndex, endIndex - beginIndex);
    }
    
    /**
     * Converts this string to a new character array.
     *
     * @return  a newly allocated character array whose length is the length
     *          of this string and whose contents are initialized to contain
     *          the character sequence represented by this string.
     */
    public char[] toCharArray() {
        char result[] = new char[count];
        getChars(0, count, result, 0);
        return result;
    }
    
    /**
     * This object (which is already a string!) is itself returned. 
     *
     * @return  the string itself.
     */
    public String toString() {
        return this;
    }
    
    /**
     * Trim space characters from both extremes of the receiver.
     * @return the trimmed <code>String</code>
     */
    public String trim() {
        int len = count;
        int st = 0;
        int off = offset;      /* avoid getfield opcode */
        char[] val = value;    /* avoid getfield opcode */
    
        while ((st < len) && (val[off + st] <= ' ')) {
            st++;
        }
        while ((st < len) && (val[off + len - 1] <= ' ')) {
            len--;
        }
        return ((st > 0) || (len < count)) ? substring(st, len) : this;
    }
}