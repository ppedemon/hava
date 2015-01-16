module ClassParser(parseClass) where

import VMErr
import BitUtils
import ClassRep


{----------------------------------------------------------------
  This module implements the Java class parser. Kind of ugly,
  the class file format is plenty of pointers, and akin stuff.
----------------------------------------------------------------}

type VMParse = Either Class VMErr

  
-----------------------------------------------------------------
-- The java class parser starts here
-----------------------------------------------------------------

parseClass :: String -> [Int] -> VMParse
parseClass = checkFmt
    

checkFmt :: String -> [Int] -> VMParse
checkFmt name cls = 
  case cls of 
    (0xCA:0xFE:0xBA:0xBE:cs) -> checkVersion name cs
    _                        -> Right (clsFormatErr name)


checkVersion :: String -> [Int] -> VMParse
checkVersion name cls = 
  case cls of
    (0x00:0x03:0x00:0x2D:cs) -> parse cs
    _                        -> Right (unsupportedClsVersionErr name)


parse :: [Int] -> VMParse
parse cs = 
  let (cp,cs1)      = parseCP cs
      (flags,cs2)   = parseInt16 cs1
      (this,cs3)    = parseInt16 cs2
      (super,cs4)   = parseInt16 cs3
      (ints,cs5)    = parseInterfaces cs4
      (fields,cs6)  = parseFields cs5 cp
      (methods,cs7) = parseMethods cs6 cp
      this_name     = getClsName cp this
      super_name    = getClsName cp super
      interfaces    = map (getClsName cp) ints
  in Left (newClass this_name super_name flags interfaces cp fields methods)


parseInt16 :: [Int] -> (Int,[Int])
parseInt16 (hi:lo:cs) = (getInt16 hi lo, cs)


getClsName :: CP -> CPIx -> String
getClsName _ 0 = ""
getClsName cp ix = 
  let CPClass ix' = cp <@> ix
      CPUtf8 name = cp <@> ix'
  in name


-----------------------------------------------------------------
-- Parse the java class Constant Pool (CP)
-----------------------------------------------------------------

parseCP :: [Int] -> (CP,[Int])
parseCP (cphi:cplo:cs) = parseCPEntries cs (getInt16 cphi cplo)


parseCPEntries :: [Int] -> Int -> (CP,[Int])
parseCPEntries cls 1 = (newCP,cls)
parseCPEntries (tag:es) n =
  case tag of
    0x7 -> parseClassName es n
    0x9 -> parseFieldRef es n
    0xA -> parseMethodRef es n
    0xB -> parseInterfaceMethodRef es n
    0xC -> parseNameAndType es n
    0x8 -> parseString es n
    0x3 -> parseInteger es n
    0x4 -> parseFloat es n
    0x5 -> parseLong es n
    0x6 -> parseDouble es n
    0x1 -> parseUtf8 es n
  

parseClassName :: [Int] -> Int -> (CP,[Int])
parseClassName (hi:lo:es) n = 
  let (cp,rest) = parseCPEntries es (n-1)
  in  (CPClass (getInt16 hi lo) <+> cp, rest)


parseFieldRef :: [Int] -> Int -> (CP,[Int])
parseFieldRef (hi:lo:hi1:lo1:es) n =
  let (cp,rest) = parseCPEntries es (n-1)
  in  (CPField (getInt16 hi lo, getInt16 hi1 lo1) <+> cp, rest) 
  

parseMethodRef :: [Int] -> Int -> (CP,[Int])
parseMethodRef (hi:lo:hi1:lo1:es) n =
  let (cp,rest) = parseCPEntries es (n-1)
  in  (CPMethod (getInt16 hi lo, getInt16 hi1 lo1) <+> cp, rest)  
  

parseInterfaceMethodRef :: [Int] -> Int -> (CP,[Int])
parseInterfaceMethodRef (hi:lo:hi1:lo1:es) n = 
  let (cp,rest) = parseCPEntries es (n-1)
  in  (CPIMethod (getInt16 hi lo, getInt16 hi1 lo1) <+> cp, rest) 
  

parseNameAndType :: [Int] -> Int -> (CP,[Int])
parseNameAndType (hi:lo:hi1:lo1:es) n =
  let (cp,rest) = parseCPEntries es (n-1)
  in  (CPNameType (getInt16 hi lo, getInt16 hi1 lo1) <+> cp, rest)  
  

parseString :: [Int] -> Int -> (CP,[Int])
parseString (hi:lo:es) n = 
  let (cp,rest) = parseCPEntries es (n-1)
  in  (CPStr (getInt16 hi lo) <+> cp, rest) 


parseInteger :: [Int] -> Int -> (CP,[Int])
parseInteger (i4:i3:i2:i1:es) n = 
  let (cp,rest) = parseCPEntries es (n-1)
  in  (CPInt (getInt32 i4 i3 i2 i1) <+> cp, rest) 

  
parseFloat :: [Int] -> Int -> (CP,[Int])
parseFloat (i4:i3:i2:i1:es) n = 
  let (cp,rest) = parseCPEntries es (n-1)
  in  (CPFloat (getFloat i4 i3 i2 i1) <+> cp, rest) 


parseLong :: [Int] -> Int -> (CP,[Int])
parseLong (i8:i7:i6:i5:i4:i3:i2:i1:es) n =
  let (cp,rest) = parseCPEntries es (n-2)
  in  (CPLong (getInt64 i8 i7 i6 i5 i4 i3 i2 i1) <+> CPPad <+> cp, rest) 


parseDouble :: [Int] -> Int -> (CP,[Int])
parseDouble (i8:i7:i6:i5:i4:i3:i2:i1:es) n =
  let (cp,rest) = parseCPEntries es (n-2)
  in  (CPDouble (getDouble i8 i7 i6 i5 i4 i3 i2 i1) <+> CPPad <+> cp, rest) 

 
parseUtf8 :: [Int] -> Int -> (CP,[Int])
parseUtf8 (hi:lo:es) n = 
  let (bytes,es') = splitAt (getInt16 hi lo) es 
      (cp,rest)   = parseCPEntries es' (n-1)
  in  (CPUtf8 (getUtf8 bytes) <+> cp, rest)
                    

-----------------------------------------------------------------
-- Get the interfaces implemented by the class being parsed
-----------------------------------------------------------------

parseInterfaces :: [Int] -> ([CPIx],[Int])
parseInterfaces (ichi:iclo:is) = 
  let (ints,cs) = splitAt (getInt16 ichi iclo * 2) is
  in (getInts ints, cs)
  
  
getInts :: [Int] -> [Int]
getInts cs = if null cs 
               then []
               else let (n,rs) = parseInt16 cs
                    in n : getInts rs


-----------------------------------------------------------------
-- Get the class fields (both static and instance fields)
-----------------------------------------------------------------

parseFields :: [Int] -> CP -> ([FInfo],[Int])
parseFields (hi:lo:cs) cp = parseNFields cs cp (getInt16 hi lo)


parseNFields :: [Int] -> CP -> Int -> ([FInfo],[Int])
parseNFields cs _ 0 = ([],cs)
parseNFields (fhi:flo:nhi:nlo:dhi:dlo:ahi:alo:cs) cp n =
  let flgs        = getInt16 fhi flo
      CPUtf8 name = cp <@> getInt16 nhi nlo
      CPUtf8 desc = cp <@> getInt16 dhi dlo
      (ix,rs1)    = parseFieldAttrs cs cp (getInt16 ahi alo)
      (fs,rs2)    = parseNFields rs1 cp (n-1)
  in (newField flgs name desc ix : fs, rs2)
      
     
parseFieldAttrs :: [Int] -> CP -> Int -> (Maybe CPIx,[Int])
parseFieldAttrs cs _ 0 = (Nothing,cs)
parseFieldAttrs (hi:lo:cs) cp n = 
  let CPUtf8 str =  cp <@> getInt16 hi lo
  in case str of
       "ConstantValue" -> getConstant cs cp n
       _               -> parseFieldAttrs (snd (splitAt 4 cs)) cp (n-1)
       
       
getConstant :: [Int] -> CP -> Int -> (Maybe CPIx,[Int])
getConstant (_:_:_:_:hi:lo:cs) cp n = 
  (Just (getInt16 hi lo), snd (splitAt ((n-1) * 6) cs)) 


-----------------------------------------------------------------
-- Get the class methods. This is quite a mess, as methods have
-- a lot of attributes: 
--
-- 1 The exceptions thrown by the method
-- 2 The code, who has itself two nested attributes:
--    2.1  The bytedcode stream
--    2.2  The exception table
-- 3 Local variable table, Line number table, and deprecated. 
--   These attributes are silently ignored
-----------------------------------------------------------------

data Attr = Attr [CPIx] [Int] [EInfo]


parseMethods :: [Int] -> CP -> ([MInfo],[Int])
parseMethods (mhi:mlo:cs) cp = 
  parseNMethods cs cp (getInt16 mhi mlo)


parseNMethods :: [Int] -> CP -> Int -> ([MInfo],[Int])
parseNMethods cs _ 0 = ([],cs)
parseNMethods (fhi:flo:nhi:nlo:dhi:dlo:ahi:alo:cs) cp n = 
  let flgs                = getInt16 fhi flo
      CPUtf8 name         = cp <@> getInt16 nhi nlo
      CPUtf8 desc         = cp <@> getInt16 dhi dlo
      acount              = getInt16 ahi alo
      (Attr ex c etbl,rs) = parseMethodAttrs cs cp acount (Attr [] [] [])
      (ms,ss)             = parseNMethods rs cp (n-1)
  in (newMethod flgs name desc ex c etbl : ms, ss)
  

parseMethodAttrs :: [Int] -> CP -> Int -> Attr -> (Attr, [Int])
parseMethodAttrs cs _ 0 m = (m,cs)
parseMethodAttrs (nhi:nlo:lhh:lhl:llh:lll:cs) cp n (Attr e c tbl) =
  let CPUtf8 name = cp <@> getInt16 nhi nlo
      (rs,ss)     = splitAt (getInt32 lhh lhl llh lll) cs
  in case name of
       "Code"       -> let (code,etbl) = parseCodeAttr rs
                       in parseMethodAttrs ss cp (n-1) (Attr e code etbl)
       "Exceptions" -> let e = parseExceptionsAttr rs
                       in  parseMethodAttrs ss cp (n-1) (Attr e c tbl)
       _            -> parseMethodAttrs ss cp (n-1) (Attr e c tbl)

       
parseCodeAttr :: [Int] -> ([Int],[EInfo])
parseCodeAttr (_:_:_:_:lhh:lhl:llh:lll:cs) = 
  let ccount    = getInt32 lhh lhl llh lll
      (code,rs) = splitAt ccount cs
      etbl      = let (ehi:elo:ss) = rs 
                  in parseETbl ss (getInt16 ehi elo)
  in (code,etbl)
 

parseETbl :: [Int] -> Int -> [EInfo]
parseETbl _ 0 = []
parseETbl (shi:slo:ehi:elo:hhi:hlo:ihi:ilo:cs) n = 
  let spc = getInt16 shi slo
      epc = getInt16 ehi elo
      hpc = getInt16 hhi hlo
      ix  = getInt16 ihi ilo
  in EInfo spc epc hpc ix : parseETbl cs (n-1)


parseExceptionsAttr :: [Int] -> [CPIx]
parseExceptionsAttr (_:_:cs) = parseExceptionsAttr' cs


parseExceptionsAttr' :: [Int] -> [Int]
parseExceptionsAttr' [] = []
parseExceptionsAttr' (hi:lo:cs) = 
  getInt16 hi lo : parseExceptionsAttr' cs
