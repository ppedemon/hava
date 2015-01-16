module ClassLoader(solveClassRef) where

import List
import Maybe
import Monad(when,unless)

import MA
import VMErr
import VMMonad
import OpenFile
import ClassRep
import ClassParser


{---------------------------------------------------------------------
 This module implements a VERY naive Java class loader. Missing
 functionality highlights:
 
   - It is not thread safe! But it doesn't matter, because this is
     not a multi-threaded JVM.
   - There is no verification phase. Maybe some day...
   - There are no different class loaders nor namespaces. We are not
     adhering to the JVM specification here.
   - The class parser might fail due to invalid pattern matching,
     when it should fail with a Right ClsFormatErr instead.
---------------------------------------------------------------------}


----------------------------------------------------------------------
-- Specialization of the VM monad for this module
----------------------------------------------------------------------

type VML a = VM (MA,[String]) a


getLoaded :: VML [String]
getLoaded = do 
             (_,l) <- getS
             return l

setLoaded :: [String] -> VML ()
setLoaded l = do
               (m,_) <- getS
               setS (m,l)

addLoaded :: String -> VML ()
addLoaded l = do
               ls <- getLoaded
               setLoaded (l:ls)

getMA :: VML MA
getMA = do
         (m,_) <- getS
         return m

setMA :: MA -> VML ()
setMA m = do
           (_,l) <- getS
           setS (m,l)

getMAClass :: MAIx -> VML Class
getMAClass ix = do 
                 m <- getMA
                 return (getClass m ix)


{---------------------------------------------------------------------
 Check whether a class has access to another one.
---------------------------------------------------------------------}

checkClassAccess :: MA -> Maybe Class -> Class -> Bool
checkClassAccess _ Nothing _ = True
checkClassAccess ma f@(Just from) to 
  | isArrayClass to = 
      case getArrayClass ma to of
        Nothing -> True
        Just c  -> checkClassAccess ma f c
  | otherwise = isPublic to || getPackage from == getPackage to 


{---------------------------------------------------------------------
 Solve a class reference for a given referencing class. Such
 class will be Nothing for the first VM load operation. Basically, 
 this takes two steps:
 
   1 Load the referenced class
   2 Check access permissions
---------------------------------------------------------------------}

solveClassRef :: Maybe Class -> ClassRef -> VM MA MAIx
solveClassRef c cref = 
  do Just ix <- inject (primSolveClassRef c cref) (\m -> (m,[])) fst
     return ix
     
  
{---------------------------------------------------------------------
 Solve a reference to a given class: load it and if appropriate,
 check the access permissions.
---------------------------------------------------------------------}

primSolveClassRef :: Maybe Class -> ClassRef -> VML (Maybe MAIx)
primSolveClassRef _ N      = return Nothing
primSolveClassRef _ (S ix) = return (Just ix)
primSolveClassRef c (U nm) = 
  do res <- loadClass nm
     ma  <- getMA
     case res of
       Found ix -> 
         do let cls = getClass ma ix
            unless (checkClassAccess ma c cls)
                   (raise (illegalAccessErr (show cls)))
            return (Just ix)
       New cls  -> 
         do unless (checkClassAccess ma c cls) 
                   (raise (illegalAccessErr (show cls)))
            let (_ma,ix) = addClass ma (prepare2 ma ix cls)
            setMA _ma
            return (Just ix)
  

----------------------------------------------------------------------
-- Load the specified class. Solve its super class and interfaces 
-- references.
----------------------------------------------------------------------

data LRes = Found MAIx | New Class 


loadClass :: String -> VML LRes
loadClass name =
  do ma <- getMA
     case isLoaded ma name of
       Just ix -> return (Found ix)
       Nothing -> 
         do l <- getLoaded
            when (cyclicLoad l name) $ raise (clsCircularityErr name)
            getProperClass name
 
 
getProperClass :: String -> VML LRes
getProperClass name 
  | isArray name = loadArray name
  | otherwise    = 
      do res <- io2vm (readClass name)
         case res of
           Right vmErr -> raise vmErr
           Left bytes  -> 
             case parseClass name bytes of
               Right vmErr -> raise vmErr
               Left c      -> do addLoaded name
                                 completeLoad c 
                                     

loadArray :: String -> VML LRes
loadArray name = 
  case getElemType (tail name) of
    Nothing -> do addLoaded name
                  completeLoad (newArray name)
    Just et -> do mix <- primSolveClassRef Nothing et
                  addLoaded name
                  completeLoad (newArray name)

                    
completeLoad :: Class -> VML LRes
completeLoad c =
  do s <- primSolveClassRef (Just c) (getSuper c)
     checkSuper s
     is <- mapM (loadInterface c) (getInterfaces c)
     ma <- getMA
     return (New (prepare1 c s is ma))
 
 
checkSuper :: Maybe MAIx -> VML ()
checkSuper Nothing = return ()
checkSuper (Just ix) = 
  do s <- getMAClass ix
     when (not (isClass s)) $ raise (incompClsChangeErr (show s))
     return ()

 
loadInterface :: Class -> ClassRef -> VML MAIx
loadInterface c iref = 
  do Just ix <- primSolveClassRef (Just c) iref
     i <- getMAClass ix
     when (not (isInterface i)) $ raise (incompClsChangeErr (show i))
     return ix


{---------------------------------------------------------------------
 Auxiliary functions
---------------------------------------------------------------------}

cyclicLoad :: [String] -> String -> Bool
cyclicLoad loaded name = isJust (findIndex (==name) loaded)


isArray :: String -> Bool
isArray ('[':_) = True
isArray _       = False


getElemType :: String -> Maybe ClassRef
getElemType s@('[':_) = Just (U s)
getElemType ('L':n)   = Just (U (takeWhile (/=';') n))
getElemType _         = Nothing

