module BasicOps 
  -- Utility functions and types for defining opcodes
  (VMOp
  ,FrameOp
  ,vmlift
  -- Push constants
  ,aconst_null
  ,iconst
  ,lconst
  ,fconst
  ,dconst
  ,bipush
  ,sipush
  -- Stack management
  ,pop
  ,pop2
  ,dup
  ,dup_x1
  ,dup_x2
  ,dup2
  ,dup2_x1
  ,dup2_x2
  ,swap
  -- Load values from the local variable array
  ,iload
  ,lload
  ,fload
  ,dload
  ,aload
  ,iloadn
  ,lloadn
  ,floadn
  ,dloadn
  ,aloadn
  -- Store values into the local variables array
  ,istore
  ,lstore
  ,fstore
  ,dstore
  ,astore
  ,istoren
  ,lstoren
  ,fstoren
  ,dstoren
  ,astoren
  -- Return from a method invocation
  ,ireturn
  ,lreturn
  ,freturn
  ,dreturn
  ,areturn
  ,vreturn
  -- NOP opcode
  ,nop) where

import Int
import Array

import VMHeap
import VMStack
import VMMonad
import BitUtils
import {-# SOURCE #-} VM


{--------------------------------------------------------------------
  This module implements the interpreter functions for the basic
  operations defined by the JVM Specification. They are:
  
    - Stack manipulation (push/pop const values)
    - Data movement (move from stack to variables, and viceversa)
    - Return operations
  
  Let's start with two convenience type definitions
--------------------------------------------------------------------}

type VMOp    = Int -> Array Int Int -> VM_ (Int -> Int)
type FrameOp = Int -> Array Int Int -> Frame -> (Frame, Int -> Int)


{--------------------------------------------------------------------
  Lift a simple frame operation into a full fledged vm operation.
--------------------------------------------------------------------}

vmlift :: FrameOp -> VMOp
vmlift frame_op pc code = 
  do vm <- getS
     let (f,s)    = popFrame (vmgetStack vm)
     let (f0,pc0) = frame_op pc code f
     let vm0      = vmsetStack vm (pushFrame f0 s)
     setS vm0
     return pc0

{--------------------------------------------------------------------
  Auxiliary: get the size in words of a given node. Every node
  is represented using one word, except longs and doubles, which
  use two.
--------------------------------------------------------------------}

wsize :: VMNode -> Int
wsize n = 
  case n of
    L _ -> 2
    D _ -> 2
    _   -> 1


{--------------------------------------------------------------------
  Stack operations.
--------------------------------------------------------------------}

aconst_null :: FrameOp
aconst_null _ _ f = (pushOp (A nullPtr) f, (+1))

iconst :: Int -> FrameOp
iconst op _ _ f = (pushOp (I (op - 3)) f, (+1))

lconst :: Int -> FrameOp
lconst op _ _ f = (pushOp (L (fromIntegral op - 9)) f, (+1))

fconst :: Int -> FrameOp
fconst op _ _ f = (pushOp (F (fromInt op - 11.0)) f, (+1))

dconst :: Int -> FrameOp
dconst op _ _ f = (pushOp (D (fromInt op - 14.0)) f, (+1))

bipush :: FrameOp
bipush pc code f = 
  let i = code ! (pc + 1)
  in (pushOp (I (sex8 i)) f, (+2))

sipush :: FrameOp
sipush pc code f =
  let h = code ! (pc + 1)
      l = code ! (pc + 2)
  in (pushOp (I (sex16 (getInt16 h l))) f, (+3))


--------------------------------------------------------------------
-- Auxiliary functions for stack processing depending on the
-- size in word of the pushed items
--------------------------------------------------------------------

take_ns :: Int -> [(VMNode,Frame)] -> [(VMNode,Frame)]
take_ns n ~((v,f):ps) | n == 0 = []
                      | n >  0 = (v,f) : take_ns (n - wsize v) ps
                      | n <  0 = error "take_ns: invalid stack"

chop_ns :: Int -> Int -> [(VMNode,Frame)] -> ([VMNode],[VMNode],Frame)
chop_ns h n xs = 
  let fs = take_ns n xs 
      hs = take_ns h fs
  in (map fst hs, map fst fs, snd (last fs))

_dup :: Int -> Int -> Frame -> Frame
_dup h n f = 
  let (hs,fs,_f) = chop_ns h n (extract f)
  in  foldl (flip pushOp) _f (reverse hs ++ reverse fs)


--------------------------------------------------------------------
-- Functions that manipulate the stack depending on the size
-- of the pushed items. All with type FrameOp.
--------------------------------------------------------------------

pop     _ _ f = let (_,_,_f) = chop_ns 1 1 (extract f) in (_f, (+1))
pop2    _ _ f = let (_,_,_f) = chop_ns 2 2 (extract f) in (_f, (+1))
dup     _ _ f = (_dup 1 1 f, (+1))
dup_x1  _ _ f = (_dup 1 2 f, (+1))
dup_x2  _ _ f = (_dup 1 3 f, (+1))
dup2    _ _ f = (_dup 2 2 f, (+1))
dup2_x1 _ _ f = (_dup 2 3 f, (+1))
dup2_x2 _ _ f = (_dup 2 4 f, (+1))
swap    _ _ f = let (_,ns,fs) = chop_ns 1 2 (extract f) 
                in  (foldl (flip pushOp) fs ns, (+1))

             
{--------------------------------------------------------------------
  Loading from the local variables array to the operand stack.
--------------------------------------------------------------------}

iload :: FrameOp
iload pc code f = 
  let ix  = code ! (pc + 1)
      I i = getVar f ix
  in (pushOp (I i) f, (+2))

lload :: FrameOp
lload pc code f = 
 let ix  = code ! (pc + 1)
     L l = getVar f ix
 in (pushOp (L l) f, (+2))

fload :: FrameOp
fload pc code f = 
  let ix   = code ! (pc + 1)
      F _f = getVar f ix
  in (pushOp (F _f) f, (+2))

dload :: FrameOp
dload pc code f = 
  let ix  = code ! (pc + 1)
      D d = getVar f ix
  in (pushOp (D d) f, (+2))

aload :: FrameOp
aload pc code f = 
  let ix  = code ! (pc + 1)
      A a = getVar f ix
  in (pushOp (A a) f, (+2))

iloadn :: Int -> FrameOp
iloadn op _ code f = 
  let I i = getVar f (op - 26)
  in (pushOp (I i) f, (+1))

lloadn :: Int -> FrameOp
lloadn op _ code f = 
  let L l = getVar f (op - 30)
  in (pushOp (L l) f, (+1))

floadn :: Int -> FrameOp
floadn op _ code f = 
  let F _f = getVar f (op - 34)
  in (pushOp (F _f) f, (+1))

dloadn :: Int -> FrameOp
dloadn op _ code f = 
  let D d = getVar f (op - 38)
  in (pushOp (D d) f, (+1))

aloadn :: Int -> FrameOp
aloadn op _ code f = 
  let A a = getVar f (op - 42)
  in (pushOp (A a) f, (+1))


{--------------------------------------------------------------------
  Moving from the operand stack to the local variables array.
--------------------------------------------------------------------}

istore :: FrameOp
istore pc code f = 
  let ix       = code ! (pc + 1)
      (I i,f0) = popOp f
  in  (putVar f0 ix (I i), (+2))

lstore :: FrameOp
lstore pc code f = 
  let ix       = code ! (pc + 1)
      (L l,f0) = popOp f
  in  (putVar f0 ix (L l), (+2))

fstore :: FrameOp
fstore pc code f = 
  let ix        = code ! (pc + 1)
      (F _f,f0) = popOp f
  in  (putVar f0 ix (F _f), (+2))

dstore :: FrameOp
dstore pc code f = 
  let ix       = code ! (pc + 1)
      (D d,f0) = popOp f
  in  (putVar f0 ix (D d), (+2))

astore :: FrameOp
astore pc code f = 
  let ix       = code ! (pc + 1)
      (val,f0) = popOp f
  in case val of
       A a -> (putVar f0 ix (A a), (+2))
       R a -> (putVar f0 ix (R a), (+2))
     
istoren :: Int -> FrameOp
istoren op _ _ f =
  let (I i,f0) = popOp f
  in  (putVar f0 (op - 59) (I i), (+1))

lstoren :: Int -> FrameOp
lstoren op _ _ f =
  let (L l,f0) = popOp f
  in  (putVar f0 (op - 63) (L l), (+1))

fstoren :: Int -> FrameOp
fstoren op _ _ f =
  let (F _f,f0) = popOp f
  in  (putVar f0 (op - 67) (F _f), (+1))
  
dstoren :: Int -> FrameOp
dstoren op _ _ f =
  let (D d,f0) = popOp f
  in  (putVar f0 (op - 71) (D d), (+1))

astoren :: Int -> FrameOp
astoren op _ _ f = 
  let (val,f0) = popOp f
  in case val of
       A a -> (putVar f0 (op - 75) (A a), (+1))
       R a -> (putVar f0 (op - 75) (R a), (+1))
       

{--------------------------------------------------------------------
  Return from an invocation opcode.
--------------------------------------------------------------------}

x_return :: Bool -> VMOp
x_return isVoid _ _ = do vmreturn isVoid
                         return id

ireturn = x_return False
lreturn = x_return False
freturn = x_return False
dreturn = x_return False
areturn = x_return False
vreturn = x_return True

                             
-- For debugging
--x_return isVoid pc code = 
--  do vm <- getS
--     error $ show vm

{--------------------------------------------------------------------
  Dummy NOP opcode: just increment the PC's value.
--------------------------------------------------------------------}

nop :: VMOp
nop _ _ = return (+1)
