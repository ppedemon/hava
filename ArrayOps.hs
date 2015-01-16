module ArrayOps 
  -- Loading elements from an array
  (iaload
  ,laload
  ,faload
  ,daload
  ,aaload
  ,baload
  ,caload
  ,saload
  -- Storing elements into an array
  ,iastore
  ,lastore
  ,fastore
  ,dastore
  ,aastore
  ,bastore
  ,castore
  ,sastore
  -- Creating arrays
  ,newarray
  ,anewarray
  ,multianewarray
  -- Get array size
  ,arraylength
  ) where

import Int
import Bits
import Array
import Maybe
import Monad(when)
import Char(chr,ord)

import VMErr
import VMHeap
import VMStack
import VMMonad
import BitUtils
import ClassRep
import VMOps(int16From)
import {-# SOURCE #-} VM
import BasicOps(FrameOp,VMOp)


{--------------------------------------------------------------------
  Array Operations opcodes.
--------------------------------------------------------------------}

---------------------------------------------------------------------
-- Read values from an allocated array
---------------------------------------------------------------------

areadI :: Int -> Obj -> Frame -> Frame
areadI ix o f = let I i = getArray o ix in pushOp (I i) f

areadL :: Int -> Obj -> Frame -> Frame
areadL ix o f = let L l = getArray o ix in pushOp (L l) f

areadF :: Int -> Obj -> Frame -> Frame
areadF ix o f = let F _f = getArray o ix in pushOp (F _f) f

areadD :: Int -> Obj -> Frame -> Frame
areadD ix o f = let D d = getArray o ix in pushOp (D d) f

areadA :: Int -> Obj -> Frame -> Frame
areadA ix o f = let A a = getArray o ix in pushOp (A a) f

areadB :: Int -> Obj -> Frame -> Frame
areadB ix o f = let I i = getArray o ix in pushOp (I (sex8 i)) f

areadC :: Int -> Obj -> Frame -> Frame
areadC ix o f = let C c = getArray o ix in pushOp (I (ord c)) f

areadS :: Int -> Obj -> Frame -> Frame
areadS ix o f = let I i = getArray o ix in pushOp (I (sex16 i)) f


x_aload :: (Int -> Obj -> Frame -> Frame) -> VMOp
x_aload reader _ _ = 
  do vm <- getS
     let (f,s0)   = popFrame (vmgetStack vm)
     let (I i,f0) = popOp f
     let (A p,f1) = popOp f0
     when (isNullPtr p) $ raise (nullPointerException "")
     let h        = vmgetHeap vm
     let Just o   = lookupH h p
     let size     = arraySize o
     when (i < 0 || i >= size) $ raise (arrayIndexOutOfBoundsException "")
     let f2       = reader i o f1
     let vm0      = vmsetStack vm (pushFrame f2 s0)
     setS vm0
     return (+1)

iaload = x_aload areadI
laload = x_aload areadL
faload = x_aload areadF
daload = x_aload areadD
aaload = x_aload areadA
baload = x_aload areadB
caload = x_aload areadC
saload = x_aload areadS 

---------------------------------------------------------------------
-- Store values into an allocated array
---------------------------------------------------------------------

apopI :: Frame -> (VMNode,Frame)
apopI f = let p@(I i,_) = popOp f in p

apopL :: Frame -> (VMNode,Frame)
apopL f = let p@(L l,_) = popOp f in p

apopF :: Frame -> (VMNode,Frame)
apopF f = let p@(F _f,_) = popOp f in p

apopD :: Frame -> (VMNode,Frame)
apopD f = let p@(F _f,_) = popOp f in p

apopA :: Frame -> (VMNode,Frame)
apopA f = let p@(A a,_) = popOp f in p

--apopB :: Frame -> (VMNode,Frame)
--apopB f = 
--  let p@(I i,_f) = popOp f 
--  in  (I (i .&. 0x000000ff),_f)

apopC :: Frame -> (VMNode,Frame)
apopC f = let p@(I i,_f) = popOp f in (C (chr i),_f) 

--apopS :: Frame -> (VMNode,Frame)
--apopS f = 
--  let p@(I i,_f) = popOp f 
--  in  (I (i .&. 0x0000ffff),_f)

x_astore :: (Frame -> (VMNode,Frame)) -> VMOp
x_astore reader _ _ = 
  do vm <- getS
     let (f,s)    = popFrame (vmgetStack vm)
     let (v,f0)   = reader f
     let (I i,f1) = popOp f0
     let (A a,f2) = popOp f1
     when (isNullPtr a) $ raise (nullPointerException "")
     let h      = vmgetHeap vm
     let Just o = lookupH h a
     let size   = arraySize o
     when (i < 0 || i >= size) $ raise (arrayIndexOutOfBoundsException "")
     let _h   = updateH h a (putArray o i v)
     let vm0  = vmsetHeap vm _h
     let s0   = pushFrame f2 s
     let vm1  = vmsetStack vm0 s0
     setS vm1
     return (+1)

iastore = x_astore apopI
lastore = x_astore apopL
fastore = x_astore apopF
dastore = x_astore apopD
aastore = x_astore apopA
bastore = x_astore apopI
castore = x_astore apopC
sastore = x_astore apopI


{--------------------------------------------------------------------
  Create arrays.
--------------------------------------------------------------------}

---------------------------------------------------------------------
-- Obtain a single-dimensional array ref from a class name.
-- Precondition: the class name must not be a primitive type
---------------------------------------------------------------------
arrayRef :: String -> ClassRef
arrayRef name
 | head name == '[' =  U ('[' : name)
 | otherwise        =  U ('[' : 'L' : name ++ ";")


---------------------------------------------------------------------
-- Create a new array whose descriptor is given as a class 
-- reference.
---------------------------------------------------------------------
primNewArray :: Maybe Class -> ClassRef -> VM_ ()
primNewArray c cref = 
  do f <- vmpop
     let (I s,f1) = popOp f
     (_,ptr) <- _vmnewArray c cref s
     vmpush (pushOp (A ptr) f1)


---------------------------------------------------------------------
-- Create a new array of primitive values.
---------------------------------------------------------------------
newarray :: VMOp
newarray pc code = 
  let ref = case code ! (pc + 1) of
        4  -> "[I"   -- boolean, represented as an int
        5  -> "[C"   -- char
        6  -> "[F"   -- float
        7  -> "[D"   -- double
        8  -> "[I"   -- byte, represented as an int
        9  -> "[I"   -- short, represented as an int
        10 -> "[I"   -- int
        11 -> "[J"   -- long
  in do primNewArray Nothing (U ref)
        return (+2)
        


---------------------------------------------------------------------
-- Create a new array of non-primitive values.
---------------------------------------------------------------------
anewarray :: VMOp
anewarray pc code = 
  do let ix   = int16From code (pc + 1)
     (i,c)    <- vmgetCurrClass
     let cp   = getCP c
     let cp_e = cp <@> ix
     case cp_e of
         
       CPSolvedC c_ix -> 
         do sc <- vmgetClass c_ix
            primNewArray (Just c) (arrayRef (getName sc))
            return (+3)
                
       CPClass _ -> 
         do let c_ref  = crefCP cp ix
            (s_ix,sc) <- _vmsolveClassRef (Just c) c_ref
            let _c = updateClassCP c [(ix,CPSolvedC s_ix)]
            vmreplaceClass i _c
            primNewArray (Just c) (arrayRef (getName sc))
            return (+3)


---------------------------------------------------------------------
-- Create an array of multiple dimensions
---------------------------------------------------------------------
createMultiArray :: Maybe Class -> String -> [Int] -> VM_ Ptr
createMultiArray c name (d:[]) = 
  do (_,ptr) <- _vmnewArray c (U name) d
     return ptr
createMultiArray c name (d:ds) = 
  do ptrs <- sequence (take d (repeat (createMultiArray c (tail name) ds)))
     (_,root) <- _vmnewArray c (U name) d
     vm       <- getS
     let h    = vmgetHeap vm
     let a    = fromJust (lookupH h root)
     let _a = foldr (\(i,n) a -> putArray a i n) a (zip [0..] (map A ptrs))
     setS (vmsetHeap vm (updateH h root _a))
     return root

getDimensions :: Int -> Frame -> ([Int],Frame)
getDimensions ndims f = 
  let xs = (reverse . take ndims . extract) f
  in  (map ((\(I i) -> i).fst) xs, snd (head xs))

primMultiNewArray :: Int -> Maybe Class -> String -> VM_ ()
primMultiNewArray ndims c name = 
  do f <- vmpop
     let (dims,f1) = getDimensions ndims f
     ptr <- createMultiArray c name dims
     vmpush (pushOp (A ptr) f1)

multianewarray :: VMOp
multianewarray pc code = 
  do let ix    = int16From code (pc + 1)
     let ndims = code ! (pc + 3)
     (i,c)     <- vmgetCurrClass
     let cp    = getCP c
     let cp_e  = cp <@> ix
     case cp_e of
           
       CPSolvedC c_ix -> 
         do sc <- vmgetClass c_ix
            primMultiNewArray ndims (Just c) (getName sc)
            return (+4)
                  
       CPClass _  -> 
         do let c_ref  = crefCP cp ix
            (s_ix,sc) <- _vmsolveClassRef (Just c) c_ref
            let _c = updateClassCP c [(ix,CPSolvedC s_ix)]
            vmreplaceClass i _c
            primMultiNewArray ndims (Just c) (getName sc)
            return (+4)


---------------------------------------------------------------------
-- Get the lenght of an array. This is easy!
---------------------------------------------------------------------
arraylength :: VMOp
arraylength pc _ = 
  do f <- vmpop
     let (A ptr,f1) = popOp f
     vm <- getS
     let a  = fromJust (lookupH (vmgetHeap vm) ptr)
     vmpush (pushOp (I (arraySize a)) f1)
     return (+1)

