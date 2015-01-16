module RefSolver 
  -- Solve field reference
  (solveFieldRef
  -- Solve instance or static reference
  ,solveMethodRef
  -- Solve a reference to an interface method
  ,solveIMethodRef
  ) where

import List
import Maybe

import MA
import VMErr
import VMMonad
import ClassRep
import ClassLoader


{---------------------------------------------------------------------
 This module implements the JVM generic reference solver.
 Reference can be of two distinct kinds:
 
   - Field references (both instance and static)
   - Method references (instance, static, interface methods)
   
 The solver works inside the VM monad. Since it is intended to work
 with any kind of references, it must be parametrized with:
 
   - The class predicate, useful for ensuring some precondition
     over the class where the reference is supposed to be found.
     This is a function from Class to maybe an error, or nothing
     if the class satisfies the intended condition.
 
   - The getter function, that given the class and the reference
     kind (instance or static) returns the appropriate set of class
     elems where the reference to solve might be found.
     
   - The search function, that given the method area, the class
     reference and the intended reference kind, returns the set of
     class references where the search for the referenced ClassElem
     must continue.
     
   - An exception function to be invoked if the search function fails.
     It receives a string a returns a VMErr.
     
   - The post-check predicate: given the method area, the referencing
     class, the referenced class and the solved reference, returns
     maybe an error, or nothing if the post-check succeeds.
---------------------------------------------------------------------}


----------------------------------------------------------------------
-- Type modeling the results of reference resolution.
----------------------------------------------------------------------

data ClassElem e => Res e = Res e Offset


----------------------------------------------------------------------
-- Traverse in a DFS fashion a hierarchy of classes and interfaces.
-- We don't need to worry about cycles here, that was already checked
-- at the class loading phase.
----------------------------------------------------------------------

traverse :: (a -> Maybe b) -> (a -> [a]) -> c -> [a] -> Either (a,b) c
traverse _ _ c [] = Right c
traverse f g c (x:xs) = 
  case f x of
    Nothing -> traverse f g c (g x ++ xs)
    Just b  -> Left (x,b)


----------------------------------------------------------------------
-- Solve a reference to a ClassElem held in ClassRef from a Class
-- using the given class predicate, finding, getter, error and 
-- post-check functions.
----------------------------------------------------------------------

solveRef :: 
  ClassElem e => Class -> ClassRef -> Ref 
              -> (Class -> Maybe VMErr)
              -> (Class -> RefKind -> [e])
              -> (MA -> ClassRef -> RefKind -> [ClassRef])
              -> (String -> VMErr)
              -> (MA -> Class -> Class -> e -> Maybe VMErr)
              -> VM MA (MAIx,Offset)
solveRef c cref ref cp gf sf ef pc =
  do ix <- solveEnsuring c cref cp
     (S cix, Res r o) <- primSolveRef (S ix) ref gf sf ef
     ma <- getS
     case pc ma c (getClass ma cix) r of
       Nothing    -> return (cix,o)
       Just vmErr -> raise vmErr


primSolveRef :: 
  ClassElem e => ClassRef -> Ref 
              -> (Class -> RefKind -> [e])
              -> (MA -> ClassRef -> RefKind -> [ClassRef])
              -> (String -> VMErr)
              -> VM MA (ClassRef,Res e)
primSolveRef cref ref gf sf ef =
  do ma <- getS
     let f = flip (findElem gf ma) ref
     let g = flip (sf ma) (refKind ref)
     let c = ef (refName ref)
     case traverse f g c [cref] of
       Right vmErr -> raise vmErr
       Left res    -> return res


solveEnsuring :: Class -> ClassRef -> (Class -> Maybe VMErr) -> VM MA MAIx
solveEnsuring c cref p =
  do ix <- solveClassRef (Just c) cref
     ma <- getS
     case p (getClass ma ix) of
       Nothing    -> return ix
       Just vmErr -> raise vmErr


{---------------------------------------------------------------------
  Refinement of the generic reference solver to a field solver:
  
   1 - There's no pre-condition over the referenced class 
       (no class predicate)
   2 - Use the fGetter function, that returns the set of instance/
       static fields of a given class
   3 - Use the searchSetF function, for searching the field
       in super classes and interaces.
   4 - The post check predicate must only check access permissions,
       throwing a illegalAccessErr signal if access is denied.
---------------------------------------------------------------------}

solveFieldRef :: Class -> ClassRef -> Ref -> VM MA (MAIx,Offset)
solveFieldRef c cref fref = 
  solveRef c cref fref (const Nothing) fGetter searchSetF 
           noSuchFieldErr fieldCheck

     
fGetter :: Class -> RefKind -> [FInfo]
fGetter c Static   = getStaticFields c
fGetter c Instance = getInstanceFields c


searchSetF :: MA -> ClassRef -> RefKind -> [ClassRef]
searchSetF _ N _ = []
searchSetF ma (S ix) kind = 
  let c = getClass ma ix
  in case kind of
       Instance -> [getSuper c]
       Static   -> getSuper c : getInterfaces c


fieldCheck :: MA -> Class -> Class -> FInfo -> Maybe VMErr
fieldCheck ma from to f = 
  if refAccessible ma from to f 
    then Nothing
    else Just (illegalAccessErr (show f))


{---------------------------------------------------------------------
  Refinement of the generic reference solver to a method solver:
  
   1 - Preconditions over the referenced class: must be a class
       for method references; an interface for interface method
       references
   2 - Use the mGetter function, that returns the set of instance/
       static methods of a given class
   3 - Use the searchSetM function, for searching the method
       in super classes and interaces, searchSetI for searching
       only in superinterfaces (for interface method ref)
   4 - The post check predicate must check access permissions, and
       for method references, that if the solved method is abstract, 
       the containing class is also abstract.
---------------------------------------------------------------------}

solveMethodRef :: Class -> ClassRef -> Ref -> VM MA (MAIx,Offset)
solveMethodRef c cref mref = 
  solveRef c cref mref ensureClass mGetter searchSetM 
           noSuchMethodErr methodCheck


solveIMethodRef :: Class -> ClassRef -> Ref -> VM MA (MAIx,Offset)
solveIMethodRef c cref mref = 
 let p = solveRef c cref mref ensureInterface mGetter searchSetI
         noSuchMethodErr (\_ _ _ _ -> Nothing)
 in handle p (\vmErr ->
      case vmErr of
        VMErr NoSuchMethodErr _ -> solveMethodRef c (S 0) mref
        _                       -> raise vmErr)
           

mGetter :: Class -> RefKind -> [MInfo]
mGetter c Static   = getStaticMethods c
mGetter c Instance = getInstanceMethods c


searchSetM :: MA -> ClassRef -> RefKind -> [ClassRef]
searchSetM _ N _ = []
searchSetM ma (S ix) kind = 
  let c = getClass ma ix
  in case kind of
       Instance -> getSuper c : getInterfaces c
       Static   -> [getSuper c]


searchSetI :: MA -> ClassRef -> RefKind -> [ClassRef]
searchSetI _ N _ = []
searchSetI ma (S ix) kind = 
  let c = getClass ma ix
  in case kind of
       Instance -> getInterfaces c
       Static   -> error "searchSetI: looking for static method!"


assertProp :: (Class -> Bool) -> Class -> Maybe VMErr
assertProp p c | p c = Nothing
               | otherwise = Just (incompClsChangeErr (show c))
              

ensureClass     = assertProp isClass
ensureInterface = assertProp isInterface
              

methodCheck :: MA -> Class -> Class -> MInfo -> Maybe VMErr
methodCheck ma from to m = 
  if isAbstractMethod m && (not . isAbstract) to
    then Just (abstractMethodErr (elemName m))
    else if refAccessible ma from to m 
           then Nothing
           else Just (illegalAccessErr (show m))


----------------------------------------------------------------------
-- Utility functions:
----------------------------------------------------------------------

refAccessible :: ClassElem e => MA -> Class -> Class -> e -> Bool
refAccessible ma from to e =
  case accessPerm e of
    Public    -> True
    Protected -> getPackage from == getPackage to || isSubclass ma from to
    Private   -> getName from == getName to
    Friend    -> getName from == getName to || 
                 (getPackage from == getPackage to && isSubclass ma from to)


findElem :: ClassElem e => 
  (Class -> RefKind -> [e]) -> MA -> ClassRef -> Ref -> Maybe (Res e)
findElem _ _ N _ = Nothing
findElem f ma (S cix) (Ref kind name desc) =
 let c  = getClass ma cix
     es = f c kind
 in case findIndex (\e -> elemName e == name && elemDesc e == desc) es of
           Nothing -> Nothing
           Just ix -> Just (Res (es !! ix) ix)
