{-# OPTIONS -fno-warn-deprecations #-}

module BitUtils
  (sex8
  ,sex16 
  ,getInt16 
  ,getInt32 
  ,getFloat
  ,getInt64
  ,getDouble
  ,getUtf8
  ) where

import Int
import Bits
import Char


{----------------------------------------------------------------
  Bit utilities useful for parsing a class file and for 
  interpreting java bytecodes. This module comprises utility 
  functions for retrieving integers, floats, doubles, and 
  UTF8-coded strings from a byte stream
----------------------------------------------------------------}

-- Sign extend an 8-bit integer into a 32 bit integer
sex8 :: Int -> Int
sex8 = fromIntegral . intToInt8


-- Sign extend a 16-bit integer into a 32 bit integer
sex16 :: Int -> Int
sex16 = fromIntegral . intToInt16


getInt16 :: Int -> Int -> Int
getInt16 hi lo = int16ToInt (shiftL (intToInt16 hi) 8 .|. intToInt16 lo)


getInt32 :: Int -> Int -> Int -> Int -> Int
getInt32 i4 i3 i2 i1 = 
  let
    hh = shiftL (intToInt32 i4) 24
    hl = shiftL (intToInt32 i3) 16
    lh = shiftL (intToInt32 i2) 08
    ll = intToInt32 i1
  in
    int32ToInt $ hh .|. hl .|. lh .|. ll


getFloat :: Int -> Int -> Int -> Int -> Float
getFloat i4 i3 i2 i1 = 
  let
    bits = intToInt32 $ getInt32 i4 i3 i2 i1
    s = if shiftR bits 31 == 0 then 1.0 else -1.0
    e = shiftR bits 23 .&. 0xFF
    m = if e == 0 then shiftL (bits .&. 0x7fffff) 1
                  else bits .&. 0x7fffff .|. 0x800000
  in
    s * encodeFloat (fromIntegral m) (int32ToInt e - 150)

     
getInt64 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int64
getInt64 i8 i7 i6 i5 i4 i3 i2 i1 =
  let
    hhh = shiftL (intToInt64 i8) 56
    hhl = shiftL (intToInt64 i7) 48
    hlh = shiftL (intToInt64 i6) 40
    hll = shiftL (intToInt64 i5) 32
    lhh = shiftL (intToInt64 i4) 24
    lhl = shiftL (intToInt64 i3) 16
    llh = shiftL (intToInt64 i2) 08
    lll = intToInt64 i1
  in
    hhh .|. hhl .|. hlh .|. hll .|. lhh .|. lhl .|. llh .|. lll
    

getDouble :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Double
getDouble i8 i7 i6 i5 i4 i3 i2 i1 =
  let
    bits = getInt64 i8 i7 i6 i5 i4 i3 i2 i1
    s = if shiftR bits 63 == 0 then 1.0 else -1.0
    e = shiftR bits 52 .&. 0x07FF
    m = if e == 0 then shiftL (bits .&. 0xfffffffffffff) 1
                  else bits .&. 0xfffffffffffff .|. 0x10000000000000
  in
    s * encodeFloat (fromIntegral m) (int64ToInt e - 1075)


------------------------------------------------------------
-- Decoding UTF8 characters is quite a pain!
------------------------------------------------------------

getUtf8 :: [Int] -> String
getUtf8 [] = []
getUtf8 s@(c:cs) = 
  let c' = intToInt8 c
  in case c' .&. 0x80 of
      0 -> chr c : getUtf8 cs
      _ -> case c' .&. 0x20 of
           0 -> getUtf8_1 s
           _ -> getUtf8_2 s
           

getUtf8_1 :: [Int] -> String
getUtf8_1 (c1:c2:cs) = 
  let c = shiftL (intToInt8 c1 .&. 0x1F) 6 .|. (intToInt8 c2 .&. 0x3F)
  in  chr (int8ToInt c) : getUtf8 cs

  
getUtf8_2 :: [Int] -> String
getUtf8_2 (c1:c2:c3:cs) = 
  let c = shiftL (intToInt8 c1 .&. 0xF) 12 .|. 
          shiftL (intToInt8 c2 .&. 0x3F) 6 .|.
          intToInt8 c3 .&. 0x3F
  in chr (int8ToInt c) : getUtf8 cs
