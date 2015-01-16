module ArithOps where
-- Export everything

import Int
import Bits
import Array
import Monad(when)

import VMErr
import VMHeap
import VMStack
import VMMonad
import BitUtils
import {-# SOURCE #-} VM
import BasicOps(FrameOp,VMOp)


{--------------------------------------------------------------------
  This module implements the interpreter functions for the 
  arithmetic operations defined by the JVM Specification. 
  They are the following:
  
    - Addition
    - Substraction
    - Product
    - Division
    - Remainder
    - Negation
    - Logical and aritmetical shifting
    - Logical operations (and,or,xor)
    - Integer increments
    - Numerical conversions (casts between primitive types)
--------------------------------------------------------------------}


--------------------------------------------------------------------
-- Addition
--------------------------------------------------------------------

iadd :: FrameOp
iadd _ _ f = 
  let (I v1,f1) = popOp f
      (I v2,f2) = popOp f1
  in  (pushOp (I (v2 + v1)) f2, (+1))
  
ladd :: FrameOp
ladd _ _ f = 
  let (L v1,f1) = popOp f
      (L v2,f2) = popOp f1
  in  (pushOp (L (v2 + v1)) f2, (+1))
  
fadd :: FrameOp
fadd _ _ f = 
  let (F v1,f1) = popOp f
      (F v2,f2) = popOp f1
  in  (pushOp (F (v2 + v1)) f2, (+1))
  
dadd :: FrameOp
dadd _ _ f = 
  let (D v1,f1) = popOp f
      (D v2,f2) = popOp f1
  in  (pushOp (D (v2 + v1)) f2, (+1))


--------------------------------------------------------------------
-- Substraction
--------------------------------------------------------------------  
  
isub :: FrameOp
isub _ _ f = 
  let (I v1,f1) = popOp f
      (I v2,f2) = popOp f1
  in  (pushOp (I (v2 - v1)) f2, (+1))
    
lsub :: FrameOp
lsub _ _ f = 
  let (L v1,f1) = popOp f
      (L v2,f2) = popOp f1
  in  (pushOp (L (v2 - v1)) f2, (+1))
    
fsub :: FrameOp
fsub _ _ f = 
  let (F v1,f1) = popOp f
      (F v2,f2) = popOp f1
  in  (pushOp (F (v2 - v1)) f2, (+1))
    
dsub :: FrameOp
dsub _ _ f = 
  let (D v1,f1) = popOp f
      (D v2,f2) = popOp f1
  in  (pushOp (D (v2 - v1)) f2, (+1))


--------------------------------------------------------------------
-- Product
--------------------------------------------------------------------
  
imul :: FrameOp
imul _ _ f = 
  let (I v1,f1) = popOp f
      (I v2,f2) = popOp f1
  in  (pushOp (I (v2 * v1)) f2, (+1))
    
lmul :: FrameOp
lmul _ _ f = 
  let (L v1,f1) = popOp f
      (L v2,f2) = popOp f1
  in  (pushOp (L (v2 * v1)) f2, (+1))
    
fmul :: FrameOp
fmul _ _ f = 
  let (F v1,f1) = popOp f
      (F v2,f2) = popOp f1
  in  (pushOp (F (v2 * v1)) f2, (+1))
    
dmul :: FrameOp
dmul _ _ f = 
  let (D v1,f1) = popOp f
      (D v2,f2) = popOp f1
  in  (pushOp (D (v2 * v1)) f2, (+1))


--------------------------------------------------------------------
-- Division
-- Sad but true: both intger and long division throw an
-- ArithmeticException if the divisor is zero. So, these
-- operations must be monadic.
--------------------------------------------------------------------
  
idiv :: VMOp
idiv code pc = 
  do vm <- getS
     let s          = vmgetStack vm
     let (f,s1)     = popFrame s
     let (I v1,f1)  = popOp f
     when (v1 == 0) $ raise (arithmeticException "Division by zero")
     let (I v2,f2)  = popOp f1
     let s2 = pushFrame (pushOp (I (v2 `div` v1)) f2) s1
     setS (vmsetStack vm s2)
     return (+1)
     
ldiv :: VMOp
ldiv code pc = 
  do vm <- getS
     let s          = vmgetStack vm
     let (f,s1)     = popFrame s
     let (L v1,f1)  = popOp f
     when (v1 == 0) $ raise (arithmeticException "Division by zero")
     let (L v2,f2)  = popOp f1
     let s2 = pushFrame (pushOp (L (v2 `div` v1)) f2) s1
     setS (vmsetStack vm s2)
     return (+1)
    
fdiv :: FrameOp
fdiv _ _ f = 
  let (F v1,f1) = popOp f
      (F v2,f2) = popOp f1
  in  (pushOp (F (v2 / v1)) f2, (+1))
    
ddiv :: FrameOp
ddiv _ _ f = 
  let (D v1,f1) = popOp f
      (D v2,f2) = popOp f1
  in  (pushOp (D (v2 / v1)) f2, (+1))


--------------------------------------------------------------------
-- Remainder
--------------------------------------------------------------------
  
fprem :: RealFrac a => a -> a -> a
fprem n d = 
  let r = n/d
  in  n - d * fromIntegral (if r <= 0 then ceiling r else floor r)
  
irem :: FrameOp
irem _ _ f = 
  let (I v1,f1) = popOp f
      (I v2,f2) = popOp f1
  in  (pushOp (I (v2 `mod` v1)) f2, (+1))
    
lrem :: FrameOp
lrem _ _ f = 
  let (L v1,f1) = popOp f
      (L v2,f2) = popOp f1
  in  (pushOp (L (v2 `mod` v1)) f2, (+1))
    
frem :: FrameOp
frem _ _ f = 
  let (F v1,f1) = popOp f
      (F v2,f2) = popOp f1
  in  (pushOp (F (v2 `fprem` v1)) f2, (+1))
    
drem :: FrameOp
drem _ _ f = 
  let (D v1,f1) = popOp f
      (D v2,f2) = popOp f1
  in  (pushOp (D (v2 `fprem` v1)) f2, (+1))


--------------------------------------------------------------------
-- Negation
--------------------------------------------------------------------
  
ineg :: FrameOp
ineg _ _ f = 
  let (I v1,f1) = popOp f
  in  (pushOp (I (-v1)) f1, (+1))
    
lneg :: FrameOp
lneg _ _ f = 
  let (L v1,f1) = popOp f
  in  (pushOp (L (-v1)) f1, (+1))
    
fneg :: FrameOp
fneg _ _ f = 
  let (F v1,f1) = popOp f
  in  (pushOp (F (-v1)) f1, (+1))
    
dneg :: FrameOp
dneg _ _ f = 
  let (D v1,f1) = popOp f
  in  (pushOp (D (-v1)) f1, (+1))
  

--------------------------------------------------------------------
-- Shifting
--------------------------------------------------------------------

ishl :: FrameOp
ishl _ _ f = 
  let (I d,f1) = popOp f
      (I i,f2) = popOp f1
  in  (pushOp (I (i `shiftL` (d .&. 0x1f))) f2, (+1))
  
lshl :: FrameOp
lshl _ _ f = 
  let (I d,f1) = popOp f
      (L l,f2) = popOp f1
  in  (pushOp (L (l `shiftL` (d .&. 0x3f))) f2, (+1))

ishr :: FrameOp
ishr _ _ f = 
  let (I d,f1) = popOp f
      (I i,f2) = popOp f1
  in  (pushOp (I (i `shiftR` (d .&. 0x1f))) f2, (+1))
  
lshr :: FrameOp
lshr _ _ f = 
  let (I d,f1) = popOp f
      (L l,f2) = popOp f1
  in  (pushOp (L (l `shiftR` (d .&. 0x3f))) f2, (+1))

iushr :: FrameOp
iushr _ _ f = 
  let (I d,f1) = popOp f
      (I i,f2) = popOp f1
      v1 = i `shiftR` (d .&. 0x1f)
      v2 = if i >= 0 
             then v1
             else v1 + 2 `shiftL` (complement d .&. 0x1f)
  in  (pushOp (I v2) f2, (+1))

lushr :: FrameOp
lushr _ _ f = 
  let (I d,f1) = popOp f
      (L l,f2) = popOp f1
      v1 = l `shiftR` (d .&. 0x3f)
      v2 = if l >= 0 
             then v1
             else v1 + 2 `shiftL` (complement d .&. 0x3f)
  in  (pushOp (L v2) f2, (+1))


--------------------------------------------------------------------
-- Logical operands
--------------------------------------------------------------------

iand :: FrameOp
iand _ _ f = 
  let (I v1,f1) = popOp f
      (I v2,f2) = popOp f1
  in (pushOp (I (v2 .&. v1)) f2, (+1))

land :: FrameOp
land _ _ f = 
  let (L v1,f1) = popOp f
      (L v2,f2) = popOp f1
  in (pushOp (L (v2 .&. v1)) f2, (+1))

ior :: FrameOp
ior _ _ f = 
  let (I v1,f1) = popOp f
      (I v2,f2) = popOp f1
  in (pushOp (I (v2 .|. v1)) f2, (+1))

lor :: FrameOp
lor _ _ f = 
  let (L v1,f1) = popOp f
      (L v2,f2) = popOp f1
  in (pushOp (L (v2 .|. v1)) f2, (+1))

ixor :: FrameOp
ixor _ _ f = 
  let (I v1,f1) = popOp f
      (I v2,f2) = popOp f1
  in (pushOp (I (v2 `xor` v1)) f2, (+1))

lxor :: FrameOp
lxor _ _ f = 
  let (L v1,f1) = popOp f
      (L v2,f2) = popOp f1
  in (pushOp (L (v2 `xor` v1)) f2, (+1))


--------------------------------------------------------------------
-- Increment operation
--------------------------------------------------------------------

iinc :: FrameOp
iinc pc code f = 
  let ix  = code ! (pc + 1)
      d   = sex8 (code ! (pc + 2))
      I i = getVar f ix
      f0  = putVar f ix (I (i + d))
  in (f0, (+3))


--------------------------------------------------------------------
-- Numerical conversions
--------------------------------------------------------------------

i2l :: FrameOp
i2l _ _ f = 
  let (I i,f1) = popOp f
  in  (pushOp (L (fromIntegral i)) f1, (+1))

i2f :: FrameOp
i2f _ _ f = 
  let (I i,f1) = popOp f
  in  (pushOp (F (fromIntegral i)) f1, (+1))

i2d :: FrameOp
i2d _ _ f = 
  let (I i,f1) = popOp f
  in  (pushOp (D (fromIntegral i)) f1, (+1))

l2i :: FrameOp
l2i _ _ f = 
  let (L l,f1) = popOp f
  in  (pushOp (I (fromIntegral l)) f1, (+1))

l2f :: FrameOp
l2f _ _ f = 
  let (L l,f1) = popOp f
  in  (pushOp (F (fromIntegral l)) f1, (+1))

l2d :: FrameOp
l2d _ _ f = 
  let (L l,f1) = popOp f
  in  (pushOp (D (fromIntegral l)) f1, (+1))

f2i :: FrameOp
f2i _ _ f =
  let (F _f,f1) = popOp f
  in  (pushOp (I (ceiling _f)) f1, (+1)) -- CHECK: Is it OK to use ceiling?
  
f2l :: FrameOp
f2l _ _ f =
  let (F _f,f1) = popOp f
  in  (pushOp (L (ceiling _f)) f1, (+1)) -- CHECK: Is it OK to use ceiling?
  
f2d :: FrameOp
f2d _ _ f =
  let (F _f,f1) = popOp f
  in  (pushOp (D (uncurry encodeFloat (decodeFloat _f))) f1, (+1))

d2i :: FrameOp
d2i _ _ f =
  let (D d,f1) = popOp f
  in  (pushOp (I (ceiling d)) f1, (+1))  -- CHECK: Is it OK to use ceiling?
  
d2l :: FrameOp
d2l _ _ f =
  let (D d,f1) = popOp f
  in  (pushOp (L (ceiling d)) f1, (+1))  -- CHECK: Is it OK to use ceiling?

d2f :: FrameOp
d2f _ _ f =
  let (D d,f1) = popOp f
  in  (pushOp (F (uncurry encodeFloat (decodeFloat d))) f1, (+1))

i2b :: FrameOp
i2b _ _ f =
  let (I i,f1) = popOp f
  in  (pushOp (I (sex8 i)) f1, (+1))

i2c :: FrameOp
i2c _ _ f =
  let (I i,f1) = popOp f
  in  (pushOp (I (sex16 i)) f1, (+1))

i2s :: FrameOp  
i2s = i2c
