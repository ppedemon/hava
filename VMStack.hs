module VMStack
  -- Frame definition and functions
  (Frame
  ,newFrame
  ,popOp
  ,pushOp
  ,clearOps
  ,putVar
  ,getVar
  ,addVars
  ,putPC
  ,getPC
  ,getMAIx
  ,getMInfo
  -- VM Stack definition and functions
  ,Stack
  ,newStack
  ,emptyStack
  ,topFrame
  ,popFrame
  ,pushFrame
  ,clearStack
  ,unwind
  ,extract
  ) where

import Int

import MA
import VMHeap
import ClassRep


{--------------------------------------------------------------------
   Implementation of the VM stack and stack frames. One of the
   core components of the VM, they should have a more efficient
   implementation.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
   Operand stack. This is a simple type, just a list representing
   the stack will do.
--------------------------------------------------------------------}

type Ops = [VMNode]

_newOps :: Ops
_newOps = []

_pushOp :: VMNode -> Ops -> Ops
_pushOp = (:)

_popOp :: Ops -> (VMNode,Ops)
_popOp [] = error "_popOp: empty operand stack!"
_popOp (o:os) = (o,os)


{--------------------------------------------------------------------
   Local variables will be modeled by the VMNode type. See the 
   VMHeap.hs module for an explanation of the issues resulting 
   from this design choice.
   
   This is a HORRIBLE representation for a data type supposed 
   to be accessed repeatedly. We nned mutable arrays, or at
   least, AVL trees. Hopefully, the number of local variables
   will remain small (< 10).
--------------------------------------------------------------------}

type Vars = [(Int,VMNode)]

_newVars :: Vars
_newVars = []

_putVar :: Vars -> Int -> VMNode -> Vars
_putVar vs i n = (i,n) : removeVar vs i

_getVar :: Vars -> Int -> VMNode
_getVar vs i = 
  case lookup i vs of
    Nothing -> error $ "_getVar: invalid index " ++ show i
    Just v  -> v

removeVar :: Vars -> Int -> Vars
removeVar [] _ = []
removeVar ((i,v):vs) _i | i == _i   = vs
                        | otherwise = (i,v) : removeVar vs _i

-- Install a set of variables in the var array. This is a
-- little tricky, as the long and double types must take 
-- two positions in the array.
_addVars :: Vars -> [VMNode] -> Vars
_addVars vars vs = 
  foldl (uncurry . _putVar) vars ((snd . foldl f (0,[])) vs)
    where f (i,vs) v = case v of 
                         L l -> (i+2, (i,v):vs)
                         D d -> (i+2, (i,v):vs)
                         _   -> (i+1, (i,v):vs)


{--------------------------------------------------------------------
   A stack frame is composed by:
     - The local variable array
     - The pc register's value
     - A pointer to the current class
     - The current method
     - The operand stack
--------------------------------------------------------------------}

data Frame = Frame Vars Int MAIx MInfo Ops deriving Show

newFrame :: Int -> MAIx -> MInfo -> Frame
newFrame pc ix cm = Frame _newVars pc ix cm _newOps

popOp :: Frame -> (VMNode,Frame)
popOp (Frame vs pc ix cm os) = 
  let (_o,_os) = _popOp os
  in (_o, Frame vs pc ix cm _os)

pushOp :: VMNode -> Frame -> Frame
pushOp o (Frame vs pc ix cm os) =
  let _os = _pushOp o os
  in Frame vs pc ix cm _os

clearOps :: Frame -> Frame
clearOps (Frame vs pc ix cm _) = Frame vs pc ix cm _newOps

putVar :: Frame -> Int -> VMNode -> Frame
putVar (Frame vs pc ix cm os) i v = 
  let _vs = _putVar vs i v
  in Frame _vs pc ix cm os
    
getVar :: Frame -> Int -> VMNode
getVar (Frame vs _ _ _ _) i = _getVar vs i

addVars :: Frame -> [VMNode] -> Frame
addVars (Frame vs pc ix cm os) v = 
  let _vs = _addVars vs v
  in Frame _vs pc ix cm os

putPC :: Frame -> Int -> Frame
putPC (Frame vs _ ix cm os) pc = Frame vs pc ix cm os

getPC :: Frame -> Int
getPC (Frame _ pc _ _ _) = pc

getMAIx :: Frame -> MAIx
getMAIx (Frame _ _ ix _ _) = ix

getMInfo :: Frame -> MInfo
getMInfo (Frame _ _ _ cm _) = cm


-- A stack of frames
type Stack = [Frame]

newStack :: Stack
newStack = []

emptyStack :: Stack -> Bool
emptyStack = null

topFrame :: Stack -> Frame
topFrame [] = error "topFrame: empty stack!"
topFrame (s:ss) = s

popFrame :: Stack -> (Frame,Stack)
popFrame [] = error "popFrame: empty stack!"
popFrame (s:ss) = (s,ss)

pushFrame :: Frame -> Stack -> Stack
pushFrame = (:)

clearStack :: Stack -> Stack
clearStack = const newStack

-- Unwind a stack until the given funcion return an Int.
-- Return the unwinded part, the remaining stack, and the
-- number returned by the function, or (-1) if it returned
-- Nothing for every frame in the stack.
unwind :: (Int -> Frame -> Maybe Int) -> Int -> Stack -> (Stack, Stack, Int)
unwind _ _ [] = ([],[],-1)
unwind f pc (x:xs) =
  case f pc x of
    Nothing -> (x:ys,zs,p)
    Just p  -> ([],x:xs,p)
  where (ys,zs,p) = unwind f (getPC x) xs

-- Get an infinite list of pairs (Frame,Stack)
extract :: Frame -> [(VMNode,Frame)]
extract = iterate (popOp . snd) . popOp
