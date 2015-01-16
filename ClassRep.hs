module ClassRep
  -- Data definitions (some types are exported as transparent)
  (CP
  ,CPIx
  ,CPEntry(..)
  ,FInfo
  ,EInfo(EInfo)
  ,MInfo
  ,VTbl
  ,AccPerm(..)
  ,ClassRef(..)
  ,RefKind(..)
  ,Ref(Ref)
  -- Handling references to classes and class elems (methods and fields)
  ,newRef
  ,refKind
  ,refName
  ,refDesc
  -- Generic offset type
  ,Offset
  -- Class for class elems
  ,ClassElem(..)
  -- Class initialization state and datatype
  ,CStat(..)
  ,Class
  -- Several funtions for working with java classes
  ,newClass
  ,newArray
  ,getName
  ,getPackage
  ,getSuper
  ,getState
  ,setState
  ,getCP
  ,getInterfaces
  ,getInstanceFields
  ,getStaticFields
  ,getVarOffset
  ,getStaticMethods
  ,getInstanceMethods
  ,getVTbl
  ,getITbl
  ,prepare1
  ,prepare2
  ,updateClassCP
  ,updateClassField
  -- Querying classes
  ,isClass
  ,isArrayClass
  ,isInterface
  ,isPublic
  ,isAbstract
  ,isRoot
  ,isSubclass
  -- Moving across the class hierarchy
  ,pathToRoot
  ,pathToRootCls
  ,getSuperClass
  ,getSuperInts
  ,getArrayClass
  -- Constant Pool functions
  ,newCP
  ,(<@>)
  ,(<+>)
  ,crefCP
  ,nameTypeCP
  ,elemRefCP
  -- Field functions
  ,newField
  ,getValue
  ,setValue
  ,prepareField
  -- Method functions
  ,newMethod
  ,getETbl
  ,getCode
  ,getArity
  ,isConstructor
  ,isVirtual
  ,isAbstractMethod
  ,isNative
  -- Accessing virtual tables
  ,findInVTbl
  ,vtblAt
  ,findInITbl
  ,itblAt
  -- General utility functions
  ,replace) where


import Int
import Bits
import List
import Array
import Maybe

import MA
import VMHeap


infixl 6 <@>
infixr 5 <+>


{----------------------------------------------------------------
  The internal representation of a java class. By Class, we
  mean plain classes, interfaces or arrays, i.e., every Java's
  non-primitive type.
----------------------------------------------------------------}

-----------------------------------------------------------------
-- Constant Pool definition
-----------------------------------------------------------------
  
type CP = [CPEntry]
type CPIx = Int

data CPEntry = CPClass    CPIx
             | CPField    (CPIx,CPIx)
             | CPMethod   (CPIx,CPIx)
             | CPIMethod  (CPIx,CPIx)
             | CPNameType (CPIx,CPIx)
             | CPStr      CPIx
             | CPInt      Int
             | CPFloat    Float
             | CPLong     Int64
             | CPDouble   Double
             | CPUtf8     String
             | CPPad
             | CPSolvedC  MAIx                 -- Solved class
             | CPSolvedN  (MAIx,MInfo)         -- Solved non-virtual method
             | CPSolvedV  (MAIx,MInfo,Offset)  -- Solved virtual method
             | CPSolvedI  (MAIx,MInfo,Offset)  -- Solved interface method
             | CPSolvedF  (MAIx,Offset)        -- Solved field
             | CPSolvedS  Ptr                  -- Solved string
             deriving Show


-----------------------------------------------------------------
-- Class representation
-----------------------------------------------------------------

-- Fields: instance or static
data FInfo = IInfo Int String String 
           | SInfo Int String String (Either CPIx VMNode) deriving Show


-- Exception table and methods
-- EInfo: FromPC, toPC, handlerPC, index to Exception handled
-- MInfo: method flags, name, descriptor, #args, thrown exceptions,
--        bytecodes, exception table
data EInfo = EInfo Int Int Int CPIx deriving Show
data MInfo = MInfo Int String String Int [CPIx] (Array Int Int) [EInfo]


-- Virtual tables exploit sharing: they are list of methods,
-- possibly declared in other classes. For real performance, 
-- they should be arrays. Since methods are non-mutable (at
-- least in absence of JIT compilation) even non-monadic arrays
-- would suffice. 
type VTbl = [(MAIx,MInfo)]

-- Interface tables also explit sharing, and are laid out
-- using a particular VTbl for each of the interfaces implemented
-- by the class.
type ITbl = [(MAIx,VTbl)]

-- Access permissions
data AccPerm = Private 
             | Protected 
             | Public 
             | Friend 
             deriving (Eq,Show)


-- References to classes
data ClassRef = N | U String | S MAIx deriving Show


-- References to both methods and fields
data RefKind = Static | Instance
data Ref = Ref RefKind String String

newRef :: RefKind -> String -> String -> Ref
newRef = Ref

refKind :: Ref -> RefKind
refKind (Ref kind _ _) = kind

refName :: Ref -> String
refName (Ref _ name _) = name

refDesc :: Ref -> String
refDesc (Ref _ _ desc) = desc


-- A generic offset value
type Offset = Int

 
-- Fields and Methods are class elems with common behaviour
class ClassElem e where
  isFinal    :: e -> Bool
  isStatic   :: e -> Bool
  elemName   :: e -> String
  elemDesc   :: e -> String
  accessPerm :: e -> AccPerm
  

-- Class state: initialized, initializing, or uninitialized
data CStat = Init | InProgress | UnInit deriving Show


-- A class can be an array or a class                      
data Class = Array String            -- Array name
                   ClassRef          -- Super class
                   [ClassRef]        -- Interfaces
                   VTbl              -- Virtual method table
                   
           | Class String            -- Class name
                   ClassRef          -- Superclass reference
                   Int               -- Class flags
                   [ClassRef]        -- Implemented interfaces
                   CP                -- Constant Pool
                   [FInfo]           -- Static Fields
                   [FInfo]           -- Instance Fields
                   [MInfo]           -- Static methods
                   [MInfo]           -- Instance methods
                   VTbl              -- Virtual method table
                   ITbl              -- Interface method table
                   Int               -- Super classes inst. var. count
                   CStat             -- Class initalization state
           
instance Show Class where
  showsPrec n (Class nm s _ ints cp ss is sm im vtbl itbl c state) = 
    showsPrec n nm   . showChar '\n' .
    showsPrec n s    . showChar '\n' .
    showsPrec n ints . showChar '\n' -- . 
    --showsPrec n cp   . showChar '\n' .
    -- . showsPrec n (is,ss) . showChar '\n'
    -- . showsPrec n vtbl . showChar '\n'
    -- . showsPrec n itbl . showChar '\n'
    --showsPrec n sm . showChar '\n'
    --showsPrec n im . showChar '\n'
    --showsPrec n is . showChar '\n'
    -- . showsPrec n c
    . showsPrec n state . showChar '\n'
    
  showsPrec n (Array nm s ints vtbl) = 
    showsPrec n nm   . showChar '\n' .
    showsPrec n s    . showChar '\n' .
    showsPrec n ints . showChar '\n' -- . 
    --showsPrec n vtbl . showChar '\n'

 
------------------------------------------------------------
-- Class utility functions
------------------------------------------------------------

newClass :: String -> String -> Int -> [String] -> 
            CP -> [FInfo] -> [MInfo] -> Class
newClass name super flags ints cp fs ms = 
  let s = if null super then N else U super
  in Class name s flags (map U ints) cp fs [] ms [] [] [] 0 UnInit


newArray :: String -> Class
newArray name = 
  Array name (U "java/lang/Object") [U "java/lang/Cloneable"] []


getName :: Class -> String
getName (Array name _ _ _) = name
getName (Class name _ _ _ _ _ _ _ _ _ _ _ _) = name


getPackage :: Class -> String
getPackage = reverse . dropWhile (/='/') . reverse . getName


getSuper :: Class -> ClassRef
getSuper (Array _ super _ _) = super
getSuper (Class _ super _ _ _ _ _ _ _ _ _ _ _) = super


getState :: Class -> CStat
getState (Array _ _ _ _) = error "Initializing array"
getState (Class _ _ _ _ _ _ _ _ _ _ _ _ state) = state


setState :: CStat -> Class -> Class
setState _ (Array _ _ _ _) = error "Initializing array"
setState state 
 (Class name super flags ints cp sfs ifs sms ims vtbl itbl o _) = 
  Class name super flags ints cp sfs ifs sms ims vtbl itbl o state


getCP :: Class -> CP
getCP (Array _ _ _ _) = error "getCP: getting CP of an array"
getCP (Class _ _ _ _ cp _ _ _ _ _ _ _ _) = cp


getInterfaces :: Class -> [ClassRef]
getInterfaces (Array _ _ ints _) = ints
getInterfaces (Class _ _ _ ints _ _ _ _ _ _ _ _ _) = ints


getInstanceFields :: Class -> [FInfo]
getInstanceFields (Array _ _ _ _) = error "array have no variables"
getInstanceFields (Class _ _ _ _ _ _ fs _ _ _ _ _ _) = fs


getStaticFields :: Class -> [FInfo]
getStaticFields (Array _ _ _ _) = error "array have no variables"
getStaticFields (Class _ _ _ _ _ fs _ _ _ _ _ _ _) = fs
 
 
getVarOffset :: Class -> Offset
getVarOffset (Class _ _ _ _ _ _ _ _ _ _ _ o _) = o


getStaticMethods :: Class -> [MInfo]
getStaticMethods (Array _ _ _ _) = []
getStaticMethods (Class _ _ _ _ _ _ _ ms _ _ _ _ _) = ms


getInstanceMethods :: Class -> [MInfo]
getInstanceMethods (Array _ _ _ _) = []
getInstanceMethods (Class _ _ _ _ _ _ _ _ ms _ _ _ _) = ms


getVTbl :: Class -> VTbl
getVTbl (Array _ _ _ vtbl) = vtbl
getVTbl (Class _ _ _ _ _ _ _ _ _ vtbl _ _ _) = vtbl


setVTbl :: Class -> VTbl -> Class
setVTbl (Array name super ints _) vtbl = Array name super ints vtbl
setVTbl (Class name super flags ints cp sfs ifs sms ims _ itbl o st) vtbl = 
  Class name super flags ints cp sfs ifs sms ims vtbl itbl o st


getITbl :: Class -> ITbl
getITbl (Array _ _ _ _) = error "Arrays don't have an interface table"
getITbl (Class _ _ _ _ _ _ _ _ _ _ itbl _ _) = itbl


setITbl :: Class -> ITbl -> Class
setITbl (Array _ _ _ _) _ = error "Arrays don't have an interface table"
setITbl (Class name super flags ints cp sfs ifs sms ims vtbl _ o st) itbl = 
  Class name super flags ints cp sfs ifs sms ims vtbl itbl o st


splitElems :: ClassElem e => [e] -> ([e],[e])
splitElems = 
  foldr (\f (s,i) -> if isStatic f then (f:s,i) else (s,f:i)) ([],[])
  

offset :: MA -> MAIx -> Int
offset ma ix = 
  let c = getClass ma ix 
  in getVarOffset c + length (getInstanceFields c)


computeVTbl :: MA -> MAIx -> [MAIx] -> [MInfo] -> VTbl
computeVTbl ma c_ix ixs vtbl = mkVTbl c_ix (map (getClass ma) ixs) vtbl

computeITbl :: MA -> Class -> MAIx -> [MAIx] -> [MInfo] -> ITbl
computeITbl ma c c_ix ixs ms = 
  let itbl = mkITbl c_ix (map (getClass ma) ixs) ms
  in  if isInterface c
        then itbl ++ [(c_ix, map (\m -> (c_ix,m)) (getInstanceMethods c))]
        else itbl


------------------------------------------------------------
-- First phase of preparation: 
--  1 Initialize the class components to its default values
--  2 Split static and instance fields and methods
------------------------------------------------------------
prepare1 :: Class -> Maybe MAIx -> [MAIx] -> MA -> Class
prepare1 (Array name _ _ _) (Just s) is ma = 
  Array name (S s) (map S is) (getVTbl (getClass ma s))
prepare1 c@(Class name super flags ints cp fs _ ms _ _ _ _ st) sref is ma = 
  let (sfs,ifs) = splitElems fs
      (sms,ims) = splitElems ms
  in case sref of
       Nothing -> 
         Class name N flags (map S is) cp sfs ifs sms ims 
               emptyVTbl emptyITbl 0 st
       Just s  -> 
         Class name (S s) flags (map S is) cp sfs ifs sms ims 
               emptyVTbl emptyITbl (offset ma s) st


------------------------------------------------------------
-- Second phase of preparation: 
--  compute the virtual method table
------------------------------------------------------------
prepare2 :: MA -> MAIx -> Class -> Class
prepare2 _ _ a@(Array _ _ _ _) = a
prepare2 ma ix c@(Class _ _ _ _ _ _ _ _ _ _ _ _ _) = 
  case (getSuper c) of
    N   -> setVTbl c (computeVTbl ma ix [] (getInstanceMethods c))
    S s -> let is   = map (\(S ix) -> ix) (getInterfaces c)
               refs = if isClass c then (s:is) else is
               ms   = getInstanceMethods c
               c1 = setVTbl c (computeVTbl ma ix refs ms)
               c2 = setITbl c1 (computeITbl ma c1 ix refs ms)
           in  c2


updateClassCP :: Class -> [(CPIx,CPEntry)] -> Class
updateClassCP 
  (Class name super flags ints cp sfs ifs sms ims vtbl itbl o st) es = 
  Class name super flags ints (updateCP cp es) sfs ifs sms ims vtbl itbl o st


updateClassField :: Class -> Offset -> FInfo -> Class
updateClassField 
  (Class name super flags ints cp sfs ifs sms ims vtbl itbl o st) off f = 
    let _sfs = replace sfs off f
    in  (Class name super flags ints cp _sfs ifs sms ims vtbl itbl o st)


isClass :: Class -> Bool
isClass = not . isInterface


isArrayClass :: Class -> Bool
isArrayClass (Array _ _ _ _) = True
isArrayClass (Class _ _ _ _ _ _ _ _ _ _ _ _ _) = False


isInterface :: Class -> Bool
isInterface (Array _ _ _ _) = False
isInterface (Class _ _ flags _ _ _ _ _ _ _ _ _ _) = flags .&. 0x0200 /= 0


isPublic :: Class -> Bool
isPublic (Array _ _ _ _) = True
isPublic (Class _ _ flags _ _ _ _ _ _ _ _ _ _) = flags .&. 0x0001 /= 0


isAbstract :: Class -> Bool
isAbstract (Array _ _ _ _) = False
isAbstract (Class _ _ flags _ _ _ _ _ _ _ _ _ _) = flags .&. 0x0400 /= 0


isRoot :: Class -> Bool
isRoot (Class _ N _ _ _ _ _ _ _ _ _ _ _) = True
isRoot _                             = False


--isACC_SUPER :: Class -> Bool
--isACC_SUPER (Array _ _ _ _) = True
--isACC_SUPER (Class _ _ flags _ _ _ _ _ _ _ _) = flags .&. 0x0020 /= 0


isSubclass :: MA -> Class -> Class -> Bool
isSubclass ma c1 c2 = 
  let n = getName c2 
  in (isJust . findIndex (==n) . pathToRoot ma) c1
  

pathToRoot :: MA -> Class -> [String]
pathToRoot ma = map getName . pathToRootCls ma


pathToRootCls :: MA -> Class -> [Class]
pathToRootCls ma c = 
  case getSuper c of
    N -> []
    S ix -> let _c = getClass ma ix
            in _c : pathToRootCls ma _c


getSuperClass :: MA -> Class -> Maybe Class
getSuperClass ma c = 
  case pathToRootCls ma c of
    []    -> Nothing
    (c:_) -> Just c


_getSuperInts :: MA -> Class -> [Class]
_getSuperInts ma c = 
  let ints = map ((getClass ma).(\(S ix) -> ix)) (getInterfaces c)
  in  ints ++ concat (map (getSuperInts ma) ints)


getSuperInts :: MA -> Class -> [Class]
getSuperInts ma c = 
  let cs = pathToRootCls ma c
  in  nubBy cmp (concat (map (_getSuperInts ma) (c : cs)))
    where cmp c1 c2 = getName c1 == getName c2


-- Returns nothing if the array element is a primitive type
getArrayClass :: MA -> Class -> Maybe Class
getArrayClass ma a = 
  let name = dropWhile (=='[') (getName a)
  in case name of
    'L':n -> getClassByName ma (takeWhile (/= ';') n)
    _     -> Nothing


------------------------------------------------------------
-- Constant Pool utility functions
------------------------------------------------------------

newCP :: CP
newCP = []


(<@>) :: CP -> CPIx -> CPEntry
cp <@> i = cp !! (i - 1)


(<+>) :: CPEntry -> CP -> CP
(<+>) = (:)


crefCP :: CP -> CPIx -> ClassRef
crefCP cp c_ix = 
  case cp <@> c_ix of
    CPSolvedC i -> S i
    CPClass i   -> let CPUtf8 n = cp <@> i in U n
    
    
nameTypeCP :: CP -> CPIx -> RefKind -> Ref
nameTypeCP cp nt_ix kind = 
  let CPNameType (n_ix,t_ix) = cp <@> nt_ix
      CPUtf8 n = cp <@> n_ix
      CPUtf8 t = cp <@> t_ix
  in Ref kind n t
  

elemRefCP :: CP -> CPEntry -> RefKind -> (ClassRef,Ref)
elemRefCP cp (CPField   (c_ix,nt_ix)) k = elemRefCP' cp c_ix nt_ix k
elemRefCP cp (CPMethod  (c_ix,nt_ix)) k = elemRefCP' cp c_ix nt_ix k
elemRefCP cp (CPIMethod (c_ix,nt_ix)) k = elemRefCP' cp c_ix nt_ix k


elemRefCP' :: CP -> CPIx -> CPIx -> RefKind -> (ClassRef,Ref)
elemRefCP' cp c_ix nt_ix k = (crefCP cp c_ix,nameTypeCP cp nt_ix k)


updateCP :: CP -> [(CPIx,CPEntry)] -> CP
updateCP = foldr (\(i,e) cp -> replace cp (i - 1) e)


------------------------------------------------------------
-- Field utility functions
------------------------------------------------------------

instance ClassElem FInfo where
  isFinal (SInfo flags _ _ _) = flags .&. 0x0010 /= 0
  isFinal (IInfo flags _ _)   = flags .&. 0x0010 /= 0

  isStatic (SInfo flags _ _ _) = flags .&. 0x0008 /= 0
  isStatic (IInfo flags _ _)   = flags .&. 0x0008 /= 0
  
  elemName (SInfo _ name _ _) = name
  elemName (IInfo _ name _)   = name
  
  elemDesc (SInfo _ _ desc _) = desc
  elemDesc (IInfo _ _ desc)   = desc

  accessPerm (SInfo f _ _ _) = getAccessPermFrom f
  accessPerm (IInfo f _ _ )  = getAccessPermFrom f


newField :: Int -> String -> String -> Maybe CPIx -> FInfo
newField flags name desc i = 
  if flags .&. 0x0008 == 0 
  then IInfo flags name desc
  else case i of
         Nothing -> SInfo flags name desc (Right (prepareField desc))
         Just ix -> SInfo flags name desc (Left ix)


getValue :: FInfo -> Either CPIx VMNode
getValue (SInfo _ _ _ v) = v
getValue (IInfo _ _ _) = error "VM error: static reference to instance field"


setValue :: FInfo -> VMNode -> FInfo
setValue (SInfo flags name desc _) v = SInfo flags name desc (Right v)
setValue (IInfo _ _ _) _ = error "VM error: static reference to instance field"


prepareField :: String -> VMNode
prepareField desc = 
  case desc of
    "C"   -> I 0
    "D"   -> D 0.0
    "F"   -> F 0.0
    "J"   -> L (fromIntegral 0)
    'L':_ -> A nullPtr
    '[':_ -> A nullPtr
    "B"   -> I 0
    "Z"   -> I 0
    "S"   -> I 0
    "I"   -> I 0
    _     -> error $ "prepareField: unkown type " ++ desc
    
        
------------------------------------------------------------
-- Method utility functions
------------------------------------------------------------

newMethod :: Int -> String -> String -> [CPIx] -> [Int] -> [EInfo] -> MInfo
newMethod flags name desc exc code etbl = 
  let _code = listArray (0,length code - 1) code
      _desc = tail (takeWhile (/=')') desc)
  in MInfo flags name desc (nparams _desc) exc _code etbl


nparams :: String -> Int
nparams [] = 0
nparams (x:xs) = 
  case x of
      '[' -> nparams (dropWhile (=='[') xs)
      'L' -> 1 + nparams (tail (dropWhile (/=';') xs))
      _   -> 1 + nparams xs


getETbl :: MInfo -> [EInfo]
getETbl (MInfo _ _ _ _ _ _ et) = et


getCode :: MInfo -> Array Int Int
getCode (MInfo _ _ _ _ _ c _) = c


getArity :: MInfo -> Int
getArity (MInfo _ _ _ nparams _ _ _) = nparams


isConstructor :: MInfo -> Bool
isConstructor m = elemName m == "<init>"


isVirtual :: MInfo -> Bool
isVirtual m = accessPerm m /= Private && not (isConstructor m)


isAbstractMethod :: MInfo -> Bool
isAbstractMethod (MInfo flags _ _ _ _ _ _) = flags .&. 0x0400 /= 0


isNative :: MInfo -> Bool
isNative (MInfo flags _ _ _ _ _ _) = flags .&. 0x0100 /= 0


instance ClassElem MInfo where
  isFinal    (MInfo flags _ _ _ _ _ _) = flags .&. 0x0010 /= 0
  isStatic   (MInfo flags _ _ _ _ _ _) = flags .&. 0x0008 /= 0
  elemName   (MInfo _ name _ _ _ _ _)  = name
  elemDesc   (MInfo _ _ desc _ _ _ _)  = desc
  accessPerm (MInfo f _ _ _ _ _ _)     = getAccessPermFrom f


instance Eq MInfo where
  (MInfo _ n1 d1 _ _ _ _) == (MInfo _ n2 d2 _ _ _ _) = n1 == n2 && d1 == d2


instance Show MInfo where
  showsPrec n (MInfo _ name desc argc _ c _) = 
    showsPrec n name . showChar ' ' .
    showsPrec n desc . showChar ' ' .
    showsPrec n argc . showChar ' ' .
    --showsPrec n (1 + snd (bounds c)) .
    showChar '\n'


---------------------------------------------------------------------
-- Virtual and interface method table construction
---------------------------------------------------------------------

emptyVTbl :: VTbl
emptyVTbl = []

emptyITbl :: ITbl
emptyITbl = []

mkVTbl :: MAIx -> [Class] -> [MInfo] -> VTbl
mkVTbl c_ix c ms = 
  foldl f ((nubBy cmp . concat . map getVTbl) c) (filter isVirtual ms)
    where cmp e1 e2 = snd e1 == snd e2
          f mt m    = case findIndex ((==m).snd) mt of
                        Nothing -> mt ++ [(c_ix,m)]
                        Just ix -> replace mt ix (c_ix,m)
          

mkITbl :: MAIx -> [Class] -> [MInfo] -> ITbl
mkITbl c_ix is ms = 
  foldl g ((nubBy cmp . concat . map getITbl) is) (filter isVirtual ms)
    where cmp e1 e2 = fst e1 == fst e2
          f mt m    = case findIndex ((==m).snd) mt of
	                          Nothing -> mt
                                  Just ix -> replace mt ix (c_ix,m)
          g it m    = map (\(ix, vtbl) -> (ix, f vtbl m)) it


findInVTbl :: VTbl -> MInfo -> Maybe Int
findInVTbl vtbl m =
  findIndex ((==m).snd) vtbl


vtblAt :: VTbl -> Offset -> (MAIx,MInfo)
vtblAt = (!!)


-- Note: you still have to search linearly by the
-- first index: it is NOT an offset, but the MAIx
-- of the interface. We need to implement sparse
-- ITbls if we want to use as an offset the first 
-- index
findInITbl :: ITbl -> MInfo -> Maybe (MAIx,Int)
findInITbl itbl m = 
  let r = map (\(ix,vtbl) -> (ix,findInVTbl vtbl m)) itbl
    in case dropWhile (isNothing.snd) r of
        []                  -> Nothing
        ((iix, Just mix):_) -> Just (iix, mix)
      

itblAt :: ITbl -> (MAIx,Offset) -> Maybe (MAIx, MInfo)
itblAt itbl (ix,offset) = 
  case find ((==ix).fst) itbl of
    Nothing       -> Nothing
    Just (_,vtbl) -> Just $ vtbl `vtblAt` offset
      
      
------------------------------------------------------------
-- General utility functions
------------------------------------------------------------

getAccessPermFrom :: Int -> AccPerm
getAccessPermFrom f | f .&. 0x001 /= 0 = Public
                    | f .&. 0x002 /= 0 = Private
                    | f .&. 0x004 /= 0 = Protected
                    | otherwise        = Friend


replace :: [a] -> Int -> a -> [a]
replace [] _ _ = []
replace (x:xs) n a | n == 0 = a : xs
                   | n > 0  = x : replace xs (n-1) a
                   | n < 0  = error "replace: negative index"
