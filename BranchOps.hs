module BranchOps
  -- Comparing values
  (lcmp
  ,fcmpl
  ,fcmpg
  ,dcmpl
  ,dcmpg
  -- branching opcodes
  ,ifeq
  ,ifne
  ,iflt
  ,ifge
  ,ifgt
  ,ifle
  ,if_icmpeq
  ,if_icmpne
  ,if_icmplt
  ,if_icmpge
  ,if_icmpgt
  ,if_icmple
  ,if_acmpeq
  ,if_acmpne
  -- Goto opcodes
  ,goto
  ,goto_w
  -- Juping to/returning from finally subroutines
  ,jsr
  ,jsr_w
  ,ret
  -- comparing pointers against null
  ,ifnull
  ,ifnonnull
  -- case/switch opcodes
  ,tableswitch
  ,lookupswitch
  ) where


import Array

import VMHeap
import VMStack
import BitUtils
import BasicOps(FrameOp)
import {-# SOURCE #-} VM

{--------------------------------------------------------------------
  Implementation of the JVM opcode supporting branching and
  loop instructions. The support for implementing try-catch 
  blocks and finally is also included in this module.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  First, a simple generic comparission function. 
--------------------------------------------------------------------}

gen_cmp :: Ord a => a -> a -> Int
gen_cmp v2 v1 = 
  case compare v2 v1 of
    GT ->  1
    EQ ->  0
    LT -> -1


{--------------------------------------------------------------------
  Compare longs, floats and doubles.
--------------------------------------------------------------------}

lcmp :: FrameOp
lcmp _ _ f =
  let (L v1,f1) = popOp f
      (L v2,f2) = popOp f1
  in  (pushOp (I (gen_cmp v2 v1)) f2, (+1))

fcmp_x :: Int -> FrameOp
fcmp_x nan_val _ _ f =
  let (F v1,f1) = popOp f
      (F v2,f2) = popOp f1
  in if isNaN v1 || isNaN v2 
       then (pushOp (I nan_val) f2, (+1))
       else (pushOp (I (gen_cmp v2 v1)) f2, (+1))

dcmp_x :: Int -> FrameOp
dcmp_x nan_val _ _ f = 
  let (D v1,f1) = popOp f
      (D v2,f2) = popOp f1
  in if isNaN v1 || isNaN v2 
       then (pushOp (I nan_val) f2, (+1))
       else (pushOp (I (gen_cmp v2 v1)) f2, (+1))
              
fcmpl = fcmp_x (-1)
fcmpg = fcmp_x 1
dcmpl = dcmp_x (-1)
dcmpg = dcmp_x 1


{--------------------------------------------------------------------
  Compare integer values against zero and another integers.
--------------------------------------------------------------------}

if_xx :: (Int -> Bool) -> FrameOp
if_xx cmp_f pc code f = 
  let (I i,f1) = popOp f
      dh = code ! (pc + 1)
      dl = code ! (pc + 2)
      d  = if cmp_f i then getInt16 dh dl else 3
  in  (f1, (+d))
  
ifeq = if_xx (==0)
ifne = if_xx (/=0)
iflt = if_xx (< 0)
ifge = if_xx (>=0)
ifgt = if_xx (> 0)
ifle = if_xx (<=0)

if_icmp_xx :: (Int -> Int -> Bool) -> FrameOp
if_icmp_xx cmp_f pc code f = 
  let (I v1,f1) = popOp f
      (I v2,f2) = popOp f1
      dh = code ! (pc + 1)
      dl = code ! (pc + 2)
      d  = if cmp_f v2 v1 then getInt16 dh dl else 3
  in  (f1, (+d))
  
if_icmpeq = if_icmp_xx (==)
if_icmpne = if_icmp_xx (/=)
if_icmplt = if_icmp_xx (< )
if_icmpge = if_icmp_xx (>=)
if_icmpgt = if_icmp_xx (> )
if_icmple = if_icmp_xx (<=)


{--------------------------------------------------------------------
  Compare pointers (references) each other.
--------------------------------------------------------------------}

if_acmp_xx :: (Ptr -> Ptr -> Bool) -> FrameOp
if_acmp_xx cmp_f pc code f = 
  let (A a1,f1) = popOp f
      (A a2,f2) = popOp f1
      dh = code ! (pc + 1)
      dl = code ! (pc + 2)
      d  = if cmp_f a2 a1 then getInt16 dh dl else 3
  in  (f1, (+d))
  
if_acmpeq = if_acmp_xx (==)
if_acmpne = if_acmp_xx (/=)


{--------------------------------------------------------------------
  Goto instructions.
--------------------------------------------------------------------}

goto :: FrameOp
goto pc code f = 
  let dh = code ! (pc + 1)
      dl = code ! (pc + 2)
  in  (f, (+ getInt16 dh dl))
  
goto_w :: FrameOp
goto_w pc code f = 
  let dhh = code ! (pc + 1)
      dhl = code ! (pc + 2)
      dlh = code ! (pc + 3)
      dll = code ! (pc + 4)
  in  (f, (+ getInt32 dhh dhl dlh dll))
  
jsr :: FrameOp
jsr pc code f = 
  let dh = code ! (pc + 1)
      dl = code ! (pc + 2)
  in  (pushOp (R (pc + 3)) f, (+ getInt16 dh dl))
  
jsr_w :: FrameOp
jsr_w pc code f = 
  let dhh = code ! (pc + 1)
      dhl = code ! (pc + 2)
      dlh = code ! (pc + 3)
      dll = code ! (pc + 4)
  in (pushOp (R (pc + 5)) f, (+ getInt32 dhh dhl dlh dll))

ret :: FrameOp
ret pc code f = let R _pc = getVar f (code ! (pc + 1)) in  (f, const _pc)


{--------------------------------------------------------------------
  Testing for null/non-null pointers.
--------------------------------------------------------------------}
  
if_xx_null :: (Ptr -> Bool) -> FrameOp
if_xx_null cmp_f pc code f = 
  let (A a,f1) = popOp f
      dh = code ! (pc + 1)
      dl = code ! (pc + 2)
      d  = if cmp_f a then getInt16 dh dl else 3 
  in  (f1, (+d))
  
ifnull    = if_xx_null (==0)
ifnonnull = if_xx_null (/=0)


{--------------------------------------------------------------------
  Interpreting case-sentencies.
--------------------------------------------------------------------}

int32From :: Array Int Int -> Int -> Int
int32From a ix = 
  let hh = a ! ix
      hl = a ! (ix + 1)
      lh = a ! (ix + 2)
      ll = a ! (ix + 3)
  in  getInt32 hh hl lh ll

tableswitch :: FrameOp
tableswitch pc code f = 
  let (I i,f1) = popOp f
      df_ix  = pc + 4 - pc `mod` 4  -- index for default option's offset
      lo_ix  = df_ix + 4            -- index for lower option's value
      hi_ix  = df_ix + 8            -- index for higher option's value
      in_ix  = df_ix + 12           -- index for intermediate option offsets
      df_off = int32From code df_ix
      lo_val = int32From code lo_ix
      hi_val = int32From code hi_ix
      d = if i < lo_val || i > hi_val 
            then df_off 
            else int32From code (in_ix + (i - lo_val) * 4)
  in (f1, (+d))


--------------------------------------------------------------------
-- Find a key within the code array. Since the keys are
-- ordered, we could do it better than a linear search.
-- Params:
--  1 - the code array
--  2 - the starting offset
--  3 - the number of keys
--  4 - a default value to return if the key is not found
--  5 - the key to find
--------------------------------------------------------------------
findKey :: Array Int Int -> Int -> Int -> Int -> Int -> Int
findKey code ix n d k 
  | n == 0    = d
  | otherwise = let curr = int32From code ix
                in if curr == k 
                     then int32From code (ix + 4)
                     else findKey code (ix + 8) (n - 1) d k
      
lookupswitch :: FrameOp
lookupswitch pc code f = 
  let (I i,f1) = popOp f
      df_ix  = pc + 4 - pc `mod` 4  -- index for default option's offset
      np_ix  = df_ix + 4            -- index for number of pairs's offset
      ko_ix  = df_ix + 8            -- index for key-offset pairs
      df_off = int32From code df_ix
      np_val = int32From code np_ix
      d      = findKey code ko_ix np_val df_off i
  in  (f1, (+d))
  
