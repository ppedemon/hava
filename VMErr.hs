module VMErr
  -- Functions for putting the VM in a specific error state
  -- The internal def of VMErr and Error is used only in the RefSolver module
  (VMErr(..)
  ,Error(..)
  ,clsFormatErr
  ,noClsDefFoundErr
  ,clsCircularityErr
  ,incompClsChangeErr
  ,unsupportedClsVersionErr
  ,illegalAccessErr
  ,noSuchFieldErr
  ,noSuchMethodErr
  ,abstractMethodErr
  ,clsCastException
  ,nullPointerException
  ,arrayIndexOutOfBoundsException
  ,instantiationErr
  ,negativeArraySizeException
  ,arithmeticException
  ,getErrorRef
  ) where

import ClassRep(ClassRef(U))


-----------------------------------------------------------------
-- A compendium of the errors and exceptions a JVM 
-- can throw during its lifetime
-----------------------------------------------------------------
  
data VMErr = VMErr Error String deriving Show

data Error = ClsFormatErr
           | NoClsDefFoundErr 
           | ClsCircularityErr
           | IncompClsChangeErr
           | UnsupportedClsVersionErr
           | IllegalAccessErr
           | NoSuchFieldErr
           | NoSuchMethodErr
           | AbstractMethodErr
           | ClsCastException
           | NullPointerException
           | ArrayIndexOutOfBoundsException
           | InstantiationError
           | NegativeArraySizeException
           | ArithmeticException
           deriving (Eq,Show)


clsFormatErr :: String -> VMErr
clsFormatErr = VMErr ClsFormatErr

noClsDefFoundErr :: String -> VMErr
noClsDefFoundErr = VMErr NoClsDefFoundErr

clsCircularityErr :: String -> VMErr
clsCircularityErr = VMErr ClsCircularityErr

incompClsChangeErr :: String -> VMErr
incompClsChangeErr = VMErr IncompClsChangeErr

unsupportedClsVersionErr :: String -> VMErr
unsupportedClsVersionErr = VMErr UnsupportedClsVersionErr

illegalAccessErr :: String -> VMErr
illegalAccessErr = VMErr IllegalAccessErr

noSuchFieldErr :: String -> VMErr
noSuchFieldErr = VMErr NoSuchFieldErr

noSuchMethodErr :: String -> VMErr
noSuchMethodErr = VMErr NoSuchMethodErr

abstractMethodErr :: String -> VMErr
abstractMethodErr = VMErr AbstractMethodErr

clsCastException :: String -> VMErr
clsCastException = VMErr ClsCastException

nullPointerException :: String -> VMErr
nullPointerException = VMErr NullPointerException

arrayIndexOutOfBoundsException :: String -> VMErr
arrayIndexOutOfBoundsException = VMErr ArrayIndexOutOfBoundsException

instantiationErr :: String -> VMErr
instantiationErr = VMErr InstantiationError

negativeArraySizeException :: String -> VMErr
negativeArraySizeException= VMErr NegativeArraySizeException

arithmeticException :: String -> VMErr
arithmeticException = VMErr ArithmeticException

getErrorRef :: VMErr -> ClassRef
getErrorRef (VMErr e _) = 
  case lookup e vmErrAssoc of
    Nothing  -> error $ "getErrorRef: not a VM error " ++ show e
    Just ref -> ref 
  
vmErrAssoc :: [(Error,ClassRef)]
vmErrAssoc = [
 (ClsFormatErr,                   U "java/lang/ClassFormatError"),
 (NoClsDefFoundErr,               U "java/lang/NoClassDefFoundError"),
 (ClsCircularityErr,              U "java/lang/ClassCircularityError"),
 (IncompClsChangeErr,             U "java/lang/IncompatibleClassChangeError"),
 (UnsupportedClsVersionErr,       U "java/lang/UnsupportedClassVersionError"),
 (IllegalAccessErr,               U "java/lang/IllegalAccessError"),
 (NoSuchFieldErr,                 U "java/lang/NoSuchFieldError"),
 (NoSuchMethodErr,                U "java/lang/NoSuchMethodError"),
 (AbstractMethodErr,              U "java/lang/AbstractMethodError"),
 (ClsCastException,               U "java/lang/ClassCastException"),
 (NullPointerException,           U "java/lang/NullPointerException"),
 (ArrayIndexOutOfBoundsException, U "java/lang/ArrayIndexOutOfBoundsException"),
 (InstantiationError,             U "java/lang/InstantiationError"),
 (NegativeArraySizeException,     U "java/lang/NegativeArraySizeException"),
 (ArithmeticException,            U "java/lang/ArithmeticException")]
