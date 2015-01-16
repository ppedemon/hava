module VM
  -- Getters/Setters for the VM
  (vmcreate
  ,vmsetMA
  ,vmgetMA
  ,vmsetHeap
  ,vmgetHeap
  ,vmsetStack
  ,vmgetStack
  ,vmgetPC
  ,vmsetPC
  ,vmcurrMeth
  ,vmcurrClass
  -- Useful type definition
  ,VM_
  -- VM monadic utility functions
  ,vmgetClass
  ,vmgetCurrClass
  ,vmreplaceClass
  ,vmpush
  ,vmpop
  -- Solving references
  ,_vmsolveClassRef
  ,_vmsolveFieldRef
  ,_vmsolveMethodRef
  ,_vmsolveIMethodRef
  -- Create new objects and arrays
  ,_vmnewObj
  ,_vmnewArray
  -- throwing exceptions
  ,primThrow
  -- Utilities for method invocation and return
  ,vminvoke
  ,vmreturn
  -- Simplify string instantiation
  ,_quickNewStr
  -- VM main loop
  ,vmloop
  -- Start the VM
  ,vmmain
  ) where

import List
import Maybe
import Monad

import MA
import VMErr
import VMHeap
import VMIntp
import VMStack
import VMMonad

import ClassRep
import ClassInit
import ClassLoader
import RefSolver


{---------------------------------------------------------------------
  Haskell Java Virtual Machine definition.  It is composed by the
  method area (aka MA, very much like the .TEXT segment), a heap,
  the activation frame stack, and the JVM's single register, the 
  program counter. Also, it stores the set of resolved string 
  literals (interned strings).
  
  The type definition is accompanied by some useful getters-setters.
---------------------------------------------------------------------}

data VMDef = VMDef MA (Heap Obj) Stack Int [(String,Ptr)] 
             deriving Show

vmcreate :: VMDef
vmcreate = VMDef newMA newH newStack 0 []

vmsetMA :: VMDef -> MA -> VMDef
vmsetMA (VMDef _ h s pc l) ma = VMDef ma h s pc l

vmgetMA :: VMDef -> MA
vmgetMA (VMDef ma _ _ _ _) = ma

vmsetHeap :: VMDef -> Heap Obj -> VMDef
vmsetHeap (VMDef ma _ s pc l) h = VMDef ma h s pc l

vmgetHeap :: VMDef -> Heap Obj
vmgetHeap (VMDef _ h _ _ _) = h

vmsetStack :: VMDef -> Stack -> VMDef
vmsetStack (VMDef ma h _ pc l) s = VMDef ma h s pc l

vmgetStack :: VMDef -> Stack
vmgetStack (VMDef _ _ s _ _) = s

vmgetPC :: VMDef -> Int
vmgetPC (VMDef _ _ _ pc _) = pc

vmsetPC :: VMDef -> (Int -> Int) -> VMDef
vmsetPC (VMDef ma h s pc l) f = VMDef ma h s (f pc) l

vmsetLiterals :: VMDef -> [(String,Ptr)] -> VMDef
vmsetLiterals (VMDef ma h s pc _) l = VMDef ma h s pc l

vmgetLiterals :: VMDef -> [(String,Ptr)]
vmgetLiterals (VMDef _ _ _ _ l) = l

vmcurrMeth :: VMDef -> MInfo
vmcurrMeth = getMInfo . topFrame . vmgetStack

vmcurrClass :: VMDef -> (MAIx,Class)
vmcurrClass hvm = (ix, getClass (vmgetMA hvm) ix)
  where ix = getMAIx (topFrame (vmgetStack hvm))


{---------------------------------------------------------------------
  Utility routines for controlling the JVM (vm functions). These 
  functions define the JVM's basic behaviour, and are used by the 
  bytecode interpreter to do its job.
  
  It is important to note the distintion between two states:
    - An error at the VM monad level, produced by the raise function
    - An error at the JVM level, produced by throwing a java exception
    
  A vm function should NEVER return in monad error level, i.e vm 
  functions that use other functions that might produce such errors
  (solve*Ref) MUST define an error handler via the handle function,
  throwing an appropriate java exception or aborting.
  However, for convenience, we allow some functions to terminate
  in monad error level.
  To highlight such an important requirement, functions returning
  an error at the monad level begin with _vm, and their should NEVER
  be used directly by the JVM. Instead, they must be called by a vm
  function that handles the possible monad error level gracefully.
---------------------------------------------------------------------}

type VM_ a = VM VMDef a

{--------------------------------------------------------------------
  Get a class from the MA given its class index
--------------------------------------------------------------------}

vmgetClass :: MAIx -> VM_ Class
vmgetClass ix = 
  do vm <- getS
     return (getClass (vmgetMA vm) ix)


{--------------------------------------------------------------------
  Get the current class, as a monadic action
--------------------------------------------------------------------}

vmgetCurrClass :: VM_ (MAIx,Class)
vmgetCurrClass = do vm <- getS
                    return (vmcurrClass vm)


{--------------------------------------------------------------------
  Replace a class in the VM with a new one.
  This modifies the VM state
--------------------------------------------------------------------}

vmreplaceClass :: MAIx -> Class -> VM_ ()
vmreplaceClass ix c = 
  do vm <- getS
     let vm0 = vmsetMA vm (replaceClass (vmgetMA vm) ix c)
     setS vm0


{--------------------------------------------------------------------
  Pop/Push the given frame onto the VM's stack, as monadic actions.
  These modifies the VM state
--------------------------------------------------------------------}

vmpush :: Frame -> VM_ ()
vmpush f = do vm <- getS
              let s = pushFrame f (vmgetStack vm)
              setS (vmsetStack vm s)

vmpop :: VM_ Frame
vmpop = do vm <- getS
           let (f,s) = popFrame (vmgetStack vm)
           setS (vmsetStack vm s)
           return f

     
{--------------------------------------------------------------------
  Get a class pointer and the class itself
  Throws an error at monad level if the class can't be solved
--------------------------------------------------------------------}

_vmsolveClassRef :: Maybe Class -> ClassRef -> VM_ (MAIx,Class)
_vmsolveClassRef c cref = 
  do vm <- getS
     ix <- inject (solveClassRef c cref) vmgetMA (vmsetMA vm)
     vm <- getS
     return (ix, getClass (vmgetMA vm) ix)


{--------------------------------------------------------------------
  Get a reference to a field, method or instance method
  Throws an error at monad level if the reference can't be solved
--------------------------------------------------------------------}

_vmsolveRef :: (Class -> ClassRef -> Ref -> VM MA (MAIx,Offset)) 
            ->  Class -> ClassRef -> Ref -> VM_ (MAIx,Offset)
_vmsolveRef f c cref ref = 
  do vm <- getS
     rs <- inject (f c cref ref) vmgetMA (vmsetMA vm)
     return rs

_vmsolveFieldRef   = _vmsolveRef solveFieldRef
_vmsolveMethodRef  = _vmsolveRef solveMethodRef 
_vmsolveIMethodRef = _vmsolveRef solveIMethodRef


{--------------------------------------------------------------------
  Create a new instance of the given class reference
  Throws an error at monad level if the reference can't be solved.
  Return the index to the given class, and the ptr to the created
  instance.
  
  NOTE: For arrays, the class of the elements (if non primitive)
        *MUST* be solved before calling the _vmnewArray function.
--------------------------------------------------------------------}

_vmnewObj :: Maybe Class -> ClassRef -> VM_ (MAIx,Ptr)
_vmnewObj c cref = 
  do (ix,cl)   <- _vmsolveClassRef c cref
     initialize ix
     when (isAbstract cl || isInterface cl) 
          (raise (instantiationErr (show cl)))
     vm        <- getS
     let cs    = reverse (cl : pathToRootCls (vmgetMA vm) cl)
     let (h,p) = allocObj (vmgetHeap vm) cs ix
     setS (vmsetHeap vm h)
     return (ix,p)

_vmnewArray :: Maybe Class -> ClassRef -> Int -> VM_ (MAIx,Ptr)
_vmnewArray c cref s = 
  do when (s < 0) $ raise (negativeArraySizeException "")
     (ix,cl)   <- _vmsolveClassRef c cref
     vm        <- getS
     let name  = tail (getName cl)
     let (h,p) = allocArray (vmgetHeap vm) s (arrayPrepareField name) ix
     setS (vmsetHeap vm h)
     return (ix,p)

allocObj :: Heap Obj -> [Class] -> MAIx -> (Heap Obj,Ptr)
allocObj h cs ix = 
  let obj = heapObj ix (map (prepareField . elemDesc) 
                            (concat (map getInstanceFields cs)))
  in allocH h obj

allocArray :: Heap Obj -> Int -> VMNode -> MAIx -> (Heap Obj,Ptr)
allocArray h s n ix = allocH h (arrayObj ix s (take s (repeat n)))

-- Hack for representing character arrays as list of characters 
-- instead of ints. This eases the debugging process. So far, this
-- is the only place where character nodes are used.
arrayPrepareField :: String -> VMNode
arrayPrepareField desc =
  case desc of
    "C" -> C '\0000'
    _   ->  prepareField desc


{--------------------------------------------------------------------
  Virtual machine's exception handling routines.
  
  vmthrow: 
    From the current class, instatiate an exception whose class
    is passed as a class ref, then throw it using primThrow.
    
  primThrow:
    Throw the exception object allocated at the given heap location,
    whose class is referenced by the given index.
      
  Postcondition:
    - Return a VM where the stack was unwinded until an appropriate
      handler was found. If the stak is empty, then no handler was
      found and the VM must abort. Otherwise, clear the operand stack
      of the top frame of the resulting VM stack, and set the pc 
      pointing to the handler.
      
  This function throws no exception, as the exception class
  was already solved when instantiated.
--------------------------------------------------------------------}

vmthrow :: Maybe Class -> ClassRef -> VM_ ()
vmthrow c eref =
  handle (do (ix,ptr)  <- _vmnewObj c eref
             primThrow ix ptr)
         (vmthrow c . getErrorRef) 

primThrow :: MAIx -> Ptr -> VM_ ()
primThrow ix eptr = 
  do vm <- getS
     let _pc = vmgetPC vm
     case unwind (handlerPC (vmgetMA vm) ix) _pc (vmgetStack vm) of
       (s1,s2,pc) -> 
         if emptyStack s2
           then vmabort (printStackTrace (vmgetMA vm) ix s1 "")
           else do let (f,s) = popFrame s2
                   let f0    = clearOps f
                   let f1    = pushOp (A eptr) f0
                   let vm0   = vmsetStack vm (pushFrame f1 s)
                   let vm1   = vmsetPC vm0 (const pc)
                   setS vm1

printStackTrace :: MA -> MAIx -> Stack -> ShowS
printStackTrace ma ix s = 
  showString "Unhandled exception "     . 
  showString (getName (getClass ma ix)) .
  showChar '\n'                         .
  _printStackTrace ma s

_printStackTrace :: MA -> Stack -> ShowS  
_printStackTrace ma s 
  | emptyStack s = id
  | otherwise    =
    let (f,_s) = popFrame s
    in showString (getName (getClass ma (getMAIx f))) .
       showString "."                       .
       showString (elemName (getMInfo f))   .
       showString (elemDesc (getMInfo f))   .
       showChar '\n'                        .
       _printStackTrace ma _s


---------------------------------------------------------------------
-- Get a catch block for a thrown exception, within the current
-- stack frame. If we found a block, return its offset.
--
-- Parameters:
--  ma: the method area
--  ix: the index to the class of the thrown exception
--  pc: the current PC
--  f : the stack frame to look for
--
-- Return:
--  maybe an int representing the offset of the catch block
--  within the code of the frame's method.
---------------------------------------------------------------------

handlerPC :: MA -> MAIx -> Int -> Frame -> Maybe Int
handlerPC ma ix pc f =
  let mi = getMInfo f                 -- current method
      et = getETbl mi                 -- current method's exception table
      cc = getClass ma (getMAIx f)    -- current class
      cp = getCP cc                   -- current class' constant pool
      ec = getClass ma ix             -- exception class
  in _head (dropWhile isNothing (map (canCatch ma ec pc cp) et))
    where _head []    = Nothing
          _head (x:_) = x
  
canCatch :: MA -> Class -> Int -> CP -> EInfo -> Maybe Int
canCatch ma ec pc cp (EInfo spc epc hpc cpix)
  | pc < spc || pc >= epc = Nothing
  | otherwise = 
    if cpix == 0
      then Just hpc
      else let CPClass ix = cp <@> cpix
               CPUtf8 nm  = cp <@> ix
           in case find (==nm) (getName ec : pathToRoot ma ec) of
                Nothing -> Nothing
                Just _  -> Just hpc


{--------------------------------------------------------------------
  Virtual machine's method invocation routines.
  
  vminvoke: 
    Given a class, a method belonging to such class, and the
    method's parameters (as a set of VMNodes), push a new stack 
    frame corresponding to such method. 
    
    IMPORTANT: the frame's current class and method are just that: 
    the class and method whose invokation is represented by such
    stack frame. The same isn't true for the PC: the stack frame's
    PC value represents the PC of the method BEFORE this one, i.e.
    the caller method's PC. Hence, we must set the new frame's PC
    with the JVM's PC value, and then set the JVM's PC to 0 
    (pointing to the first opcode of the new method)
    
  vmreturn: 
    The inverse of vminvoke. Pop the current stack frame, 
    and restore its PC value to the JVM. Depending on the <isVoid>
    flag, push the return value (always on the top of the operand
    stack of the discarded frame) onto the operand stack of the 
    newly active frame.
          
  These function throw no exception.
--------------------------------------------------------------------}

vminvoke :: MAIx -> MInfo -> [VMNode] -> VM_ ()
vminvoke ix m params = 
  do vm0 <- getS
     let pc  = vmgetPC vm0
     let f0  = newFrame pc ix m
     let f1  = addVars f0 params
     let s0  = vmgetStack vm0
     let s1  = pushFrame f1 s0
     let vm1 = vmsetPC vm0 (const 0)
     setS (vmsetStack vm1 s1)

vmreturn :: Bool -> VM_ ()
vmreturn isVoid =
  do vm0  <- getS
     let (f,s0) = popFrame (vmgetStack vm0)
     let s1     = if isVoid then s0 else passReturnVal f s0
     let pc     = getPC f
     let vm1    = vmsetStack vm0 s1
     setS (vmsetPC vm1 (const pc))
     
passReturnVal :: Frame -> Stack -> Stack
passReturnVal f s =
  let (r,_)   = popOp f
      (_f0,_s0) = popFrame s
      _f1      = pushOp r _f0
  in pushFrame _f1 _s0


{--------------------------------------------------------------------
  Convenience functions for quickly creating String instances and
  String arrays. Useful for creating the argv and for resolving
  CONSTANT_String_info constant pool entries without bothering
  in invoking a String constructor passing to it a previously 
  created character array, bla bla bla. Just efficiency!
--------------------------------------------------------------------}

_quickNewArgv :: [String] -> VM_ Ptr
_quickNewArgv argv =
  do (i,_)      <- _vmsolveClassRef Nothing (U "[Ljava/lang/String;")
     ps         <- mapM _quickNewStr argv
     vm         <- getS
     let h      = vmgetHeap vm
     let (h1,p) = allocH h (arrayObj i (length argv) (map A ps))
     setS (vmsetHeap vm h1)
     return p

_quickNewStr :: String -> VM_ Ptr
_quickNewStr str = 
  do vm <- getS
     let ls = vmgetLiterals vm
     case lookup str ls of
       Just ptr -> return ptr
       Nothing  -> do ptr <- primQuickNewStr str
                      return ptr
       
primQuickNewStr :: String -> VM_ Ptr
primQuickNewStr str = 
  do (i1,_) <- _vmsolveClassRef Nothing (U "[C")
     (i2,_) <- _vmsolveClassRef Nothing (U "java/lang/String")
     initialize i2
     vm     <- getS
     let (n,l)   = foldr (\c (n,l) -> (n+1,C c:l)) (0,[]) str
     let arr     = arrayObj i1 n l
     let h       = vmgetHeap vm
     let (h1,p1) = allocH h arr
     let obj     = heapObj i2 [A p1,I 0,I n]
     let (h2,p2) = allocH h1 obj
     let ls      = vmgetLiterals vm
     let vm1     = vmsetHeap vm h2
     let vm2     = vmsetLiterals vm1 ((str,p2):ls)
     setS vm2
     return p2


{--------------------------------------------------------------------
  Booting - starting - aborting the JVM
--------------------------------------------------------------------}

core = [U "java/lang/Object",
        U "java/lang/String",
        U "java/lang/Throwable",
        U "java/lang/Exception",
        U "java/lang/Error",
        U "java/lang/ClassFormatError",
        U "java/lang/NoClassDefFoundError",
        U "java/lang/ClassCircularityError",
        U "java/lang/IncompatibleClassChangeError",
        U "java/lang/UnsupportedClassVersionError",
        U "java/lang/IllegalAccessError",
        U "java/lang/NoSuchFieldError",
        U "java/lang/NoSuchMethodError",
        U "java/lang/AbstractMethodError",
        U "java/lang/RuntimeException",
        U "java/lang/ClassCastException",
        U "java/lang/NullPointerException",
        U "java/lang/IndexOutOfBoundsException",
        U "java/lang/ArrayIndexOutOfBoundsException",
        U "java/lang/InstantiationError",
        U "java/lang/NegativeArraySizeException",
        U "java/lang/ArithmeticException"]

vmboot :: VM_ ()
vmboot = 
  handle (do vm <- getS
             inject (mapM_ (solveClassRef Nothing) core) vmgetMA (vmsetMA vm))
         (vmabort . flip errMsg "Error booting the JVM")  
         
vmabort :: String -> VM_ ()
vmabort = error 
--vmabort str = 
--  do vm <- getS
--     error $ "Error: " ++ str ++ "\nStack dump: " ++ show (vmgetStack vm)

errMsg :: VMErr -> String -> String
errMsg (VMErr e s) h = 
  foldr (.) id [showString h,showChar '\n',showString str,showString s] ""
  where str = case e of
                ClsFormatErr       -> "ClassFormatError: "
                NoClsDefFoundErr   -> "NoClassDefFoundError: "
                ClsCircularityErr  -> "ClassCircularityError: "
                IncompClsChangeErr -> "IncompatibleClasschangeError: "

---------------------------------------------------------------------
-- Start the VM. Given the main class name, and the argv, solve
-- the main class, its main method, create the argv and then 
-- invoke it. If any of these steps fail, throw an appropriate
-- exception.
--
-- NOTE: this must be invoked once the JVM was booted.
---------------------------------------------------------------------

vmstart :: String -> [String] -> VM_ ()
vmstart main argv = 
  handle (do (i,c) <- _vmsolveClassRef Nothing (U _main)
             initialize i
             let r = newRef Static "main" "([Ljava/lang/String;)V"
             (i,o) <- _vmsolveMethodRef c (S i) r
             ptr   <- _quickNewArgv argv
             let m = getStaticMethods c !! o
             vminvoke i m [A ptr])
         (vmthrow Nothing . getErrorRef)
  where _main = map (\c -> if c == '.' then '/' else c) main


---------------------------------------------------------------------
--  VM main loop: fetch the opcode pointed by the PC register from
--  then current stack frame, then interpret it, updating the PC
--  register to the appropriate value.
--
--  Finish when there's nothing left to be executed (the VM's stack
--  is empty).
---------------------------------------------------------------------

vmloop :: VM_ ()
vmloop = 
  handle (do vm <- getS
             let code = getCode (vmcurrMeth vm)
             let pc   = vmgetPC vm
             vmintp pc code
             vm <- getS
             unless (emptyStack (vmgetStack vm)) vmloop)
         (\vmErr -> do vmthrow Nothing (getErrorRef vmErr)
                       vmloop)
   
 
---------------------------------------------------------------------
-- Putting all together: VM's main entry point. It receives the
-- main class and the argv.
---------------------------------------------------------------------

vmmain :: String -> [String] -> VM_ ()
vmmain main argv = 
  do vmboot
     vmstart main argv
     vmloop
