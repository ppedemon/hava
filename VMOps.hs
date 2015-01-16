module VMOps
  -- Utility function
  (int16From
  -- CP accesing
  ,ldc
  ,ldc_w
  ,ldc2_w
  -- Invoking methods
  ,invokestatic
  ,invokespecial
  ,invokevirtual
  ,invokeinterface
  -- Accessing static and instance fields
  ,getstatic
  ,putstatic
  ,getfield
  ,putfield
  -- Check type conversions
  ,checkcast
  ,instanceof
  -- Create new objects
  ,new
  -- Throw exceptions
  ,athrow
  ) where

import List
import Array
import Maybe
import Monad(when,unless)

import MA
import VMErr
import VMHeap
import VMStack
import VMMonad
import BitUtils
import BasicOps(VMOp)
import {-# SOURCE #-} VM

import ClassRep
import ClassInit
import RefSolver
import ClassLoader


{---------------------------------------------------------------------
  This module implements the core operations of the VM. We provide
  implementations for method invocation, field and constant pool
  accessing, and for creating new objects and throwing exceptions.
  
  All these operations have a lot of effects on the VM, such as
  updating the CP of the current class (which implies to update
  also the current class), loading new classes to the VM, and
  adding/removing stack frames. Thus, this is quite a complicated 
  module.
  
  NOTE: array creation opcodes are implemented in the ArrayOps 
        module.
---------------------------------------------------------------------}


{---------------------------------------------------------------------
  Let's start with some convenience functions that will be useful
  througout the whole module.
---------------------------------------------------------------------}

----------------------------------------------------------------------
-- Get an 16-bit signed integer from an array, starting from the 
-- given position
----------------------------------------------------------------------
int16From :: Array Int Int -> Int -> Int
int16From a ix = 
  let h = a ! ix
      l = a ! (ix + 1)
  in  getInt16 h l


----------------------------------------------------------------------
-- Update the VM's PC register as a monadic action
----------------------------------------------------------------------
setPC :: (Int -> Int) -> VM_ ()
setPC f = do vm <- getS
             setS (vmsetPC vm f)


----------------------------------------------------------------------
-- Pop all the parameters for an invocation, returning the 
-- popped parameters and the resulting frame.
----------------------------------------------------------------------
params :: Int -> Frame -> ([VMNode],Frame)
params n f = 
  let xs = (reverse . take n . extract) f
  in  (map fst xs, if null xs then f else snd (head xs))


---------------------------------------------------------------------- 
-- Pop from the stack frame the pointer to and class of the object
-- receiving the message. Return also the class ix, and the resulting
-- frame.
--
-- Of course, this should be used only with instance methods.
---------------------------------------------------------------------- 
data RefData = RefData VMNode   -- Pointer to referenced object
                       Class    -- Class of referenced object
                       MAIx     -- Index of freferenced object
                       Frame    -- frame resulting after extracting reference

refData :: Frame -> VM_ RefData
refData f = 
  do let (n,f1) = popOp f
     case n of
       A ptr -> do (ix,c) <- primRefData ptr
                   return (RefData (A ptr) c ix f1)
       _     -> error "refData: invalid operand stack"

primRefData :: Ptr -> VM_ (MAIx,Class)
primRefData ptr = 
  do vm <- getS
     let ma = vmgetMA vm
     let h  = vmgetHeap vm
     let ix = objClassIx (fromJust (lookupH h ptr))
     return (ix,getClass ma ix)
     

----------------------------------------------------------------------
-- Ugly... we are ignoring native methods!
----------------------------------------------------------------------
assertNonNative :: MInfo -> MInfo
assertNonNative m 
  | isNative m = error $ "Native method invocation (" ++ show m ++ ")"
  | otherwise  = m


{--------------------------------------------------------------------
  CP processing and solving opcodes.
--------------------------------------------------------------------}
ldc :: VMOp
ldc pc code = 
  do (i,c)  <- vmgetCurrClass
     let ix = code ! (pc + 1)
     (_c,n) <- procCP c ix
     vmreplaceClass i _c
     f <- vmpop
     vmpush (pushOp n f)
     return (+2)

ldc_w :: VMOp
ldc_w pc code = 
  do (i,c)  <- vmgetCurrClass
     let ix = int16From code (pc + 1)
     (_c,n) <- procCP c ix
     vmreplaceClass i _c
     f <- vmpop
     vmpush (pushOp n f)
     return (+3)

procCP :: Class -> CPIx -> VM_ (Class,VMNode)
procCP c cp_ix = 
  let cp = getCP c
  in case cp <@> cp_ix of
       CPInt i   -> return (c,I i)
       CPFloat f -> return (c,F f)
       CPStr ix  -> let CPUtf8 str = cp <@> ix
                    in  do ptr <- _quickNewStr str
                           let _c = updateClassCP c [(cp_ix, CPSolvedS ptr)]
                           return (_c,A ptr)
       CPSolvedS ptr -> return (c,A ptr)
       _             -> error $ "invalid CP reference at: " ++ show cp_ix

ldc2_w :: VMOp
ldc2_w pc code =
  do (_,c)     <- vmgetCurrClass
     let ix    = int16From code (pc + 1)
     let n     = procCP_w c ix
     f <- vmpop 
     vmpush (pushOp n f)
     return (+3)
      
procCP_w :: Class -> CPIx -> VMNode
procCP_w c cp_ix = 
  let cp = getCP c
  in case cp <@> cp_ix of
       CPLong l   -> L l
       CPDouble d -> D d
       _ -> error $ "invalid CP reference at: " ++ show cp_ix


---------------------------------------------------------------------
-- Invoke static (non-virtual) methods
---------------------------------------------------------------------
fetchM_static :: (MAIx,Offset) -> VM_ MInfo
fetchM_static (ix,off) = 
  do c <- vmgetClass ix
     return (assertNonNative (getStaticMethods c !! off))

primInvokeStatic :: (MAIx,MInfo) -> VM_ ()
primInvokeStatic (ix,m) = 
  do setPC (+3)
     f <- vmpop
     let (args,f1) = params (getArity m) f
     vmpush f1
     vminvoke ix m args

invokestatic :: VMOp
invokestatic pc code = 
  do (i,c)    <- vmgetCurrClass
     let ix   = int16From code (pc + 1)
     let cp   = getCP c
     let cp_e = cp <@> ix
     case cp_e of
     
       -- The method is solved. Just invoke it
       CPSolvedN (c_ix,m) -> 
         do primInvokeStatic (c_ix,m)
            return id
 
       -- The method isn't solved. Solve it first, then invoke     
       CPMethod (c_ix,_)  -> 
         do let (c_ref,m_ref) = elemRefCP cp cp_e Static
            (s_ix,off) <- _vmsolveMethodRef c c_ref m_ref
            m          <- fetchM_static (s_ix,off)
            let l = [(c_ix,CPSolvedC s_ix),(ix,CPSolvedN (s_ix,m))]
            vmreplaceClass i (updateClassCP c l)
            initialize s_ix
            primInvokeStatic (s_ix,m)
            return id


---------------------------------------------------------------------
-- Invoke special (non-virtual) methods
---------------------------------------------------------------------
fetchM_special :: (MAIx,Offset) -> VM_ MInfo
fetchM_special (ix,off) = 
  do c <- vmgetClass ix
     let m = getInstanceMethods c !! off
     when (isAbstractMethod m) $ raise (abstractMethodErr (show m))
     return (assertNonNative m)

primInvokeSpecial :: (MAIx,MInfo) -> VM_ ()
primInvokeSpecial (ix,m) = 
  do setPC (+3)
     f <- vmpop
     let (args,f1) = params (getArity m) f
     RefData (A recv) _ recv_ix f2 <- refData f1
     vmpush f2
     when (isNullPtr recv) $ raise (nullPointerException "")
     vminvoke ix m (A recv : args)


invokespecial :: VMOp
invokespecial pc code = 
  do (i,c)    <- vmgetCurrClass
     let ix   = int16From code (pc + 1)
     let cp   = getCP c
     let cp_e = cp <@> ix
     case cp_e of
     
       -- The method is solved as non-virtual. Just invoke it.
       CPSolvedN (c_ix,m) -> 
         do primInvokeSpecial (c_ix,m)
            return id
            
       -- The method is solved as virtual. This means that
       -- the current class performed a virtual invocation 
       -- using this CP entry. We must ignore the VTbl offset,
       -- and invoke the solved method.
       CPSolvedV (c_ix,m,_)  ->
         do primInvokeSpecial (c_ix,m)
            return id
 
       -- The method isn't solved. Solve it first, then invoke.     
       CPMethod (c_ix,_)  -> 
         do let (c_ref,m_ref) = elemRefCP cp cp_e Instance
            (s_ix,off) <- _vmsolveMethodRef c c_ref m_ref
            m          <- fetchM_special (s_ix,off)
            let l = [(c_ix,CPSolvedC s_ix),(ix,CPSolvedN (s_ix,m))]
            vmreplaceClass i (updateClassCP c l)
            primInvokeSpecial (s_ix,m)
            return id


---------------------------------------------------------------------
-- Invoke virtual, non-interface methods.
---------------------------------------------------------------------
fetchM_virtual :: (MAIx,Offset) -> VM_ (MInfo,Offset)
fetchM_virtual (ix,off) = 
  do c <- vmgetClass ix
     let m = getInstanceMethods c !! off
     case findInVTbl (getVTbl c) m of
       Nothing   -> raise (incompClsChangeErr (show m))
       Just m_ix -> return (m,m_ix)

primInvokeVirtual :: (MInfo,Offset) -> VM_ ()
primInvokeVirtual (m,off) = 
  do setPC (+3)
     f <- vmpop
     let (args,f1) = params (getArity m) f
     RefData (A recv) recv_cls _ f2 <- refData f1
     vmpush f2
     when (isNullPtr recv) $ raise (nullPointerException "")
     let (ix,_m) = (getVTbl recv_cls) `vtblAt` off
     checkMethodV m _m 
     vminvoke ix (assertNonNative _m) (A recv : args)
     
checkMethodV :: MInfo -> MInfo -> VM_ ()
checkMethodV m found_m = 
  do when (isAbstractMethod found_m) 
          (raise (abstractMethodErr (show m)))
     when (m /= found_m) 
          (raise (incompClsChangeErr (show m)))
  
invokevirtual :: VMOp
invokevirtual pc code = 
  do (i,c)    <- vmgetCurrClass
     let ix   = int16From code (pc + 1)
     let cp   = getCP c
     let cp_e = cp <@> ix
     case cp_e of
     
       -- The method is solved as virtual. Just invoke it.
       CPSolvedV (_,m,off)      -> 
         do primInvokeVirtual (m,off)
            return id
 
       -- The method is solved as non-virtual. But we also
       -- know it's virtual, otherwise it wouldn't have been
       -- invoked here. So, find its offset in the Vtbl of 
       -- the class where the method was found, and invoke it.
       CPSolvedN (c_ix,m) -> 
         do found_cls <- vmgetClass c_ix
            let off = findInVTbl (getVTbl found_cls) m
            primInvokeVirtual (m, fromJust off)
            return id
            
       -- The method isn't solved. Solve it first, then invoke.     
       CPMethod (c_ix,_)  -> 
         do let (c_ref,m_ref) = elemRefCP cp cp_e Instance
            (s_ix,off) <- _vmsolveMethodRef c c_ref m_ref
            (m,_off)   <- fetchM_virtual (s_ix,off)
            let l = [(c_ix,CPSolvedC s_ix),(ix,CPSolvedV (s_ix,m,_off))]
            vmreplaceClass i (updateClassCP c l)
            primInvokeVirtual (m,_off)
            return id


---------------------------------------------------------------------
-- Invoke interface (virtual) methods.
--
-- Interface methods are not fully optimized (yet!). Rather than 
-- storing an offset to a Vtbl when solving an interface method, 
-- we store the abstract method, and then, we fetch a method matching 
-- the stored one from the VTbl of the class receiving the message.
---------------------------------------------------------------------
fetchM_interface :: (MAIx,Offset) -> VM_ (MInfo, MAIx, Offset)
fetchM_interface (ix,off) = 
  do c <- vmgetClass ix
     let m = getInstanceMethods c !! off
     case findInITbl (getITbl c) m of
       Nothing        -> raise (incompClsChangeErr (show m))
       Just (iix,off) -> return (m, iix, off)

primInvokeInterface :: (MInfo,MAIx,Offset) -> VM_ ()
primInvokeInterface (m,ix,off) = 
  do setPC (+5)
     f <- vmpop
     let (args,f1) = params (getArity m) f
     RefData (A recv) recv_cls _ f2 <- refData f1
     vmpush f2
     when (isNullPtr recv) $ raise (nullPointerException "")
     checkImplements recv_cls ix
     case (getITbl recv_cls) `itblAt` (ix,off) of
       Nothing      -> raise $ abstractMethodErr (show m)
       Just (m_ix,_m) -> 
         do checkMethodI _m
            vminvoke m_ix (assertNonNative _m) (A recv : args)
     
-- Sanity checks for interface methods
checkImplements :: Class -> MAIx -> VM_ ()
checkImplements c ix
  | isRoot c  = return ()
  | otherwise = 
      do vm <- getS
         found_cls <- vmgetClass ix
         let name  =  getName found_cls
         case find ((==name).getName) (getSuperInts (vmgetMA vm) c) of
           Nothing -> raise $ incompClsChangeErr (show c)
           Just _  -> return ()

checkMethodI :: MInfo -> VM_ ()
checkMethodI m = 
  case accessPerm m of
    Public -> when  (isAbstractMethod m) (raise $ abstractMethodErr (show m))
    _      -> raise (illegalAccessErr (show m))

invokeinterface :: VMOp
invokeinterface pc code = 
  do (i,c)    <- vmgetCurrClass
     let ix   = int16From code (pc + 1)
     let cp   = getCP c
     let cp_e = cp <@> ix
     case cp_e of
     
       -- The method is solved. Just invoke it
       CPSolvedI (i_ix,m,off) -> 
         do primInvokeInterface (m,i_ix,off)
            return id
 
       -- The method isn't solved. Solve it first, then invoke     
       CPIMethod (c_ix,_) -> 
         do let (c_ref,m_ref) = elemRefCP cp cp_e Instance
            (s_ix,off)  <- _vmsolveIMethodRef c c_ref m_ref
            (m,iix,mix) <- fetchM_interface (s_ix,off)
            let l = [(c_ix,CPSolvedC s_ix),(ix,CPSolvedI (iix,m,mix))]
            vmreplaceClass i (updateClassCP c l)
            primInvokeInterface (m,iix,mix)
            return id


---------------------------------------------------------------------
-- Get/set static fields
---------------------------------------------------------------------
-- Sanity check for static assignement
checkFinal :: MAIx -> FInfo -> VM_ ()
checkFinal ix f = 
  if not (isFinal f) 
    then return ()
    else do (curr_ix,_) <- vmgetCurrClass
            when (curr_ix /= ix) (raise (illegalAccessErr (show f)))

fetchF_static :: (MAIx,Offset) -> VM_ FInfo
fetchF_static (ix,off) = 
  do c <- vmgetClass ix
     let f = getStaticFields c !! off
     checkFinal ix f
     return f

primGetStatic :: (MAIx,Offset) -> VM_ ()
primGetStatic (ix,off) = 
  do field <- fetchF_static (ix,off)
     case getValue field of     
        Left cp_ix -> do c      <- vmgetClass ix
                         (_c,v) <- procCP c cp_ix
                         vmreplaceClass ix _c
                         f      <- vmpop
                         vmpush (pushOp v f)                         
        Right v    -> do f <- vmpop
                         vmpush (pushOp v f)

primPutStatic :: (MAIx,Offset) -> VM_ ()
primPutStatic (ix,off) = 
  do field <- fetchF_static (ix,off)
     f <- vmpop
     let (v,f1) = popOp f
     vmpush f1
     c <- vmgetClass ix
     vmreplaceClass ix (updateClassField c off (setValue field v))
  
access_static :: ((MAIx,Offset) -> VM_ ()) -> VMOp
access_static action pc code = 
  do (i,c)    <- vmgetCurrClass
     let ix   = int16From code (pc + 1)
     let cp   = getCP c
     let cp_e = cp <@> ix
     case cp_e of
     
       -- The field was solved. Just process it.
       CPSolvedF (c_ix,off) ->
         do action (c_ix,off)
            return (+3)
     
       -- The field wasn't solved. Solve and the process it.
       CPField (c_ix,_)     ->
         do let (c_ref,f_ref) = elemRefCP cp cp_e Static
            (s_ix,off) <- _vmsolveFieldRef c c_ref f_ref
            let l = [(c_ix,CPSolvedC s_ix),(ix,CPSolvedF (s_ix,off))]
            vmreplaceClass i (updateClassCP c l)
            initialize s_ix
            action (s_ix,off)
            return (+3)
          
getstatic = access_static primGetStatic
putstatic = access_static primPutStatic


---------------------------------------------------------------------
-- Get/set instance fields
---------------------------------------------------------------------
fetchF_instance :: (MAIx,Offset) -> VM_ FInfo
fetchF_instance (ix,off) =
  do c <- vmgetClass ix
     let f = getInstanceFields c !! off
     checkFinal ix f
     return f
     
primGetField :: (MAIx,Offset) -> VM_ ()
primGetField (ix,off) = 
  do f <- vmpop
     RefData (A recv) _ _ f1 <- refData f
     when (isNullPtr recv) $ raise (nullPointerException "")
     c          <- vmgetClass ix
     let _off   =  getVarOffset c + off
     vm         <- getS
     let h      =  vmgetHeap vm
     let Just o =  lookupH h recv 
     let v      =  getObj o _off
     vmpush (pushOp v f1)
     
primPutField :: (MAIx,Offset) -> VM_ ()
primPutField (ix,off) = 
  do f          <- vmpop
     let (v,f1) = popOp f
     RefData (A recv) _ _ f2 <- refData f1
     when (isNullPtr recv) $ raise (nullPointerException "")
     c          <- vmgetClass ix
     let _off   =  getVarOffset c + off
     vm         <- getS
     let h      = vmgetHeap vm
     let Just o = lookupH h recv
     let _h     = updateH h recv (putObj o _off v)
     let _vm    = vmsetHeap vm _h
     setS _vm
     vmpush f2

access_field :: ((MAIx,Offset) -> VM_ ()) -> VMOp
access_field action pc code = 
  do (i,c)    <- vmgetCurrClass
     let ix   = int16From code (pc + 1)
     let cp   = getCP c
     let cp_e = cp <@> ix
     case cp_e of
     
       -- The field was solved. Just process it.
       CPSolvedF (c_ix,off) ->
         do action (c_ix,off)
            return (+3)
     
       -- The field wasn't solved. Solve and the process it.
       CPField (c_ix,_)     ->
         do let (c_ref,f_ref) = elemRefCP cp cp_e Instance
            (s_ix,off) <- _vmsolveFieldRef c c_ref f_ref
            let l = [(c_ix,CPSolvedC s_ix),(ix,CPSolvedF (s_ix,off))]
            vmreplaceClass i (updateClassCP c l)
            action (s_ix,off)
            return (+3)
            
getfield = access_field primGetField
putfield = access_field primPutField


---------------------------------------------------------------------
-- checkcast/instanceof
-- We are assuming that the source and target classes are solved,
-- which is true, since it is enforced by the JVM spec.
---------------------------------------------------------------------
isSubtype :: MA -> Class -> Class -> Bool
isSubtype ma s t =
  getName s == getName t || _isSubType ma s t
  
_isSubType :: MA -> Class -> Class -> Bool
_isSubType ma s t 
  | isArrayClass s = isSubtypeA ma s t
  | isClass s      = isSubtypeC ma s t
  | isInterface s  = isSubtypeI ma s t
  
isSubtypeC :: MA -> Class -> Class -> Bool
isSubtypeC ma s t
  | isClass t     = isSubclass ma s t
  | isInterface t = isJust (find ((==getName t).getName) (getSuperInts ma s))

isSubtypeI :: MA -> Class -> Class -> Bool
isSubtypeI ma s t
  | isClass t     = getName t == "java/lang/Object"
  | isInterface t = isJust (find ((==getName t).getName) (getSuperInts ma s))
    
isSubtypeA :: MA -> Class -> Class -> Bool
isSubtypeA ma s t
  | isArrayClass t = checkArrays ma s t
  | isClass t      = getName t == "java/lang/Object"
  | isInterface t  = getName t == "java/lang/Cloneable"
  
checkArrays :: MA -> Class -> Class -> Bool
checkArrays ma s t
  | prim_s && prim_t       = getName s == getName t
  | not (prim_s || prim_t) = 
      isSubtype ma (fromJust (getClassByName ma (getArrayType (getName s))))
                   (fromJust (getClassByName ma (getArrayType (getName t))))
  | otherwise = False
    where prim_s = isPrimitive ma s
          prim_t = isPrimitive ma t  

isPrimitive :: MA -> Class -> Bool
isPrimitive ma = isNothing . (getArrayClass ma)
 
getArrayType :: String -> String
getArrayType name = 
  let nm = tail name
  in  case nm of
        ('L':n) -> takeWhile (/=';') n
        _       -> nm

checkSubtype :: (Bool -> Class -> VM_ ()) -> Class -> Class -> VM_ ()
checkSubtype f s t = 
  do vm <- getS
     f  (isSubtype (vmgetMA vm) s t) s

primSubtype :: (Bool -> Class -> VM_ ()) -> Class -> VMOp
primSubtype action s_cls pc code =      
  do (i,c)    <- vmgetCurrClass
     let ix   = int16From code (pc + 1)
     let cp   = getCP c
     let cp_e = cp <@> ix
     case cp_e of
     
       CPSolvedC t_ix -> 
         do t_cls <- vmgetClass t_ix
            checkSubtype action s_cls t_cls
            return (+3)
            
       CPClass _ -> do let t_ref = crefCP cp ix
                       (t_ix,t_cls) <- _vmsolveClassRef (Just c) t_ref
                       let _c = updateClassCP c [(ix,CPSolvedC t_ix)]
                       vmreplaceClass i _c
                       checkSubtype action s_cls t_cls
                       return (+3)

cast_action :: Bool -> Class -> VM_ ()
cast_action b c = unless b (raise $ clsCastException (show c))

instanceof_action :: Bool -> Class -> VM_ ()
instanceof_action b _ =
  do f <- vmpop
     vmpush (pushOp (if b then I 1 else I 0) f)

checkcast :: VMOp
checkcast pc code = 
  do f <- vmpop
     RefData (A s) s_cls _ f1 <- refData f
     vmpush (pushOp (A s) f1)
     if isNullPtr s
       then return (+3)
       else primSubtype cast_action s_cls pc code

instanceof :: VMOp
instanceof pc code = 
  do f <- vmpop
     RefData (A s) s_cls _ f1 <- refData f
     vmpush f1
     if isNullPtr s
       then do { f <- vmpop ; vmpush (pushOp (I 0) f); return (+3) }
       else primSubtype instanceof_action s_cls pc code


---------------------------------------------------------------------
-- Create new class (not arrays) instances. For array creation
-- see the ArrayOps module.
---------------------------------------------------------------------
new :: VMOp
new pc code = 
  do (i,c)    <- vmgetCurrClass
     let ix   = int16From code (pc + 1)
     let cp   = getCP c
     let cp_e = cp <@> ix
     case cp_e of
       
       CPSolvedC c_ix -> 
         do (_,ptr) <- _vmnewObj (Just c) (S c_ix)
            f <- vmpop
            vmpush (pushOp (A ptr) f)
            return (+3)
              
       CPClass _ -> do let c_ref  = crefCP cp ix
                       (s_ix,ptr) <- _vmnewObj (Just c) c_ref
                       let _c = updateClassCP c [(ix,CPSolvedC s_ix)]
                       vmreplaceClass i _c
                       f <- vmpop
                       vmpush (pushOp (A ptr) f)
                       return (+3)
                        

---------------------------------------------------------------------
-- Throw an exception.
---------------------------------------------------------------------
athrow :: VMOp
athrow pc _ = 
  do f <- vmpop
     RefData (A eptr) _ ix f1 <- refData f
     vmpush f1
     when (isNullPtr eptr) $ raise (nullPointerException "")
     primThrow ix eptr
     return id
