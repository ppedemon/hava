module VMHeap
  -- Nodes in the heap
  (VMNode(..)
  ,Ptr
  ,nullPtr
  ,isNullPtr
  -- Functions defining and handling objects and arrays
  ,Obj
  ,heapObj
  ,arrayObj
  ,objClassIx
  ,arraySize
  ,putArray
  ,getArray
  ,putObj
  ,getObj
  -- Heap definition and implementation
  ,Heap
  ,newH
  ,allocH
  ,updateH
  ,freeH
  ,lookupH
  ,addrsH
  ,sizeH
  ) where
  

{----------------------------------------------------------------
   A straightforward implementation of a heap ans its nodes.
   The heap is not polymorphic on the addresses, that must be 
   of type Ptr (imported from the VMNode module).
----------------------------------------------------------------}

import Int
import MA(MAIx)
import {-# SOURCE #-} ClassRep(replace)


{----------------------------------------------------------------
  A JVM node. Nodes are used both in the VM heap and stack
  frames (in the operand stack and local variable array). 
  There are some aspects to be noticed:
  
  A char node can be found only in character arrays. This kind
  of node is only a convenience workaround for inspecting strings 
  more easily, a VM doesn't provide real support for chars.
  
  On the other hand, both the operand stack and local variable 
  array might hold return addresses, this type must not appear 
  in the heap.
----------------------------------------------------------------}

data VMNode = C Char
            | I Int
            | F Float
            | L Int64
            | D Double
            | A Ptr
            | R Int     -- A return address (only in a stack frame)
            deriving Show


-- Avoid a data type definition like Ptr ::= Ptr Int | Null
-- for sake of efficiency
type Ptr = Int

nullPtr :: Ptr
nullPtr = 0

isNullPtr :: Ptr -> Bool
isNullPtr = (==0)


{--------------------------------------------------------------------
   Modelization for objects and arrays stored in the heap.
--------------------------------------------------------------------}

data Obj = Obj MAIx [VMNode] | Arr MAIx Int [VMNode] deriving Show

heapObj :: MAIx -> [VMNode] -> Obj
heapObj = Obj

arrayObj :: MAIx -> Int -> [VMNode] -> Obj
arrayObj = Arr

objClassIx :: Obj -> MAIx
objClassIx (Obj ix _) = ix
objClassIx (Arr ix _ _) = ix

arraySize :: Obj -> Int
arraySize (Obj _ _) = error "not an array"
arraySize (Arr _ size _) = size

putArray :: Obj -> Int -> VMNode -> Obj
putArray (Obj _ _) _ _ = error "not an array"
putArray (Arr cix size a) ix n = Arr cix size (replace a ix n)

getArray :: Obj -> Int -> VMNode
getArray (Obj _ _) _ = error "not an array"
getArray (Arr _ _ a) ix = a !! ix

putObj :: Obj -> Int -> VMNode -> Obj
putObj (Obj cix a) ix n = Obj cix (replace a ix n)
putObj (Arr _ _ _) _ _ = error "not an object"

getObj :: Obj -> Int -> VMNode
getObj (Obj _ a) ix = a !! ix
getObj (Arr _ _ _) _ = error "not an object"


{--------------------------------------------------------------------
   A heap is formed by the number of allocated elements, the list
   of free addresses, and the set of nodes, represented by a
   (address,value) association
--------------------------------------------------------------------}

data Heap a = Heap Int [Ptr] [(Ptr,a)]


{--------------------------------------------------------------------
   Since the heap info will be important for testing and debugging,
   show it in a kind of decent way.
--------------------------------------------------------------------}

instance Show a => Show (Heap a) where
  showsPrec n (Heap _ _ ns) = showH ns
  
  
showH :: Show a => [a] -> ShowS
showH [] = showString "[]"
showH (x:xs) = showChar '[' . shows x . _showH xs . showChar ']'
  where _showH = foldr (\x f -> showString "\n," . shows x . f) id
        

{--------------------------------------------------------------------
   The Heap implementation. Very, very straightforward.
--------------------------------------------------------------------}

newH :: Heap a
newH = Heap 0 [1..] []


allocH :: Heap a -> a -> (Heap a,Ptr)
allocH (Heap s (a:as) ns) n = (Heap (s+1) as ((a,n):ns), a)


updateH :: Heap a -> Ptr -> a -> Heap a
updateH (Heap s as ns) a n = Heap s as ((a,n):remove ns a)


freeH :: Heap a -> Ptr -> Heap a
freeH (Heap s as ns) a = Heap (s-1) (a:as) (remove ns a)


lookupH :: Heap a -> Ptr -> Maybe a
lookupH (Heap _ _ ns) = flip lookup ns


addrsH :: Heap a -> [Ptr]
addrsH (Heap _ _ ns) = map fst ns


sizeH :: Heap a -> Int
sizeH (Heap s _ _) = s


{--------------------------------------------------------------------
   Auxiliary function: remove the item at the given address.
--------------------------------------------------------------------}

remove :: [(Ptr,a)] -> Ptr -> [(Ptr,a)]
remove [] _ = []
remove ((a,n):ns) _a | a == _a   = ns
                     | otherwise = (a,n) : remove ns _a
