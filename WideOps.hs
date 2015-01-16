module WideOps(wide) where

import Array

import VMHeap
import VMStack
import BitUtils(sex16)
import VMOps(int16From)
import BasicOps(FrameOp)


{--------------------------------------------------------------------
  Implementation of widened opcodes. Very seldom used.
  Moreover, this opcodes appears only when compiling huuuge
  methods. As this is a toy JVM, I'm affraid there is no practical
  way to test these functions. Perhaps only inc_w, with something 
  like:
  
  ...
  int i;
  i += 0x7FFFFFFF;
  ...
--------------------------------------------------------------------}

wide :: FrameOp
wide pc code f = 
  let op = case code ! (pc + 1) of
             21  -> iload_w
             22  -> lload_w
             23  -> fload_w
             24  -> dload_w
             25  -> aload_w
             54  -> istore_w
             55  -> lstore_w
             56  -> fstore_w
             57  -> dstore_w
             58  -> astore_w
             132 -> iinc_w
             169 -> ret_w
  in op pc code f
  
iload_w :: FrameOp
iload_w pc code f =
  let ix  = int16From code (pc + 2)
      I i = getVar f ix
  in  (pushOp (I i) f, (+4))
  
lload_w :: FrameOp
lload_w pc code f = 
  let ix  = int16From code (pc + 2)
      L l = getVar f ix
  in (pushOp (L l) f, (+4))

fload_w :: FrameOp
fload_w pc code f = 
  let ix   = int16From code (pc + 2)
      F _f = getVar f ix
  in (pushOp (F _f) f, (+4))

dload_w :: FrameOp
dload_w pc code f = 
  let ix  = int16From code (pc + 2)
      D d = getVar f ix
  in (pushOp (D d) f, (+4))

aload_w :: FrameOp
aload_w pc code f = 
  let ix  = int16From code (pc + 2)
      A a = getVar f ix
  in (pushOp (A a) f, (+4)) 
  
istore_w :: FrameOp
istore_w pc code f = 
  let ix       = int16From code (pc + 2)
      (I i,f0) = popOp f
  in  (putVar f0 ix (I i), (+4))

lstore_w :: FrameOp
lstore_w pc code f = 
  let ix       = int16From code (pc + 2)
      (L l,f0) = popOp f
  in  (putVar f0 ix (L l), (+4))

fstore_w :: FrameOp
fstore_w pc code f = 
  let ix        = int16From code (pc + 2)
      (F _f,f0) = popOp f
  in  (putVar f0 ix (F _f), (+4))

dstore_w :: FrameOp
dstore_w pc code f = 
  let ix       = int16From code (pc + 2)
      (D d,f0) = popOp f
  in  (putVar f0 ix (D d), (+4))

astore_w :: FrameOp
astore_w pc code f = 
  let ix       = int16From code (pc + 2)
      (val,f0) = popOp f
  in case val of
       A a -> (putVar f0 ix (A a), (+4))
       R a -> (putVar f0 ix (R a), (+4))
       
ret_w :: FrameOp
ret_w pc code f = 
  let R _pc = getVar f (int16From code (pc + 2)) 
  in  (f, const _pc)
  
iinc_w :: FrameOp
iinc_w pc code f = 
  let ix  = int16From code (pc + 2)
      d   = sex16 (int16From code (pc + 4))
      I i = getVar f ix
      f0  = putVar f ix (I (i + d))
  in (f0, (+6))

