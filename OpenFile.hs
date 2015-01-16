module OpenFile(readClass) where

import IO
import List
import System
import Char(ord)
import IOExts(hSetBinaryMode)

import VMErr
import VMMonad

{----------------------------------------------------------------
  Implementation of a file reader, that uses a CLASSPATH
  environment variable for finding java classes.
  
  I'm not proud of this, it could be implemented better.
  For instance, this module parses the CLASSPATH every time 
  it is invoked. I guess the best way to implement this is
  to have an immutable state component in the VM Monad acting
  like an environment.
----------------------------------------------------------------}

-----------------------------------------------------------------
-- Platform dependent settings. We should read this from 
-- something like a property file, rather than wiring them.
-- But that's overkill...
-----------------------------------------------------------------

file_sep = '/'
path_sep = ':'  
file_ext = ".class"


-----------------------------------------------------------------
-- Parse the CLASSPATH
-----------------------------------------------------------------

parseCP :: String -> [String]
parseCP str = 
  let cp = groupBy (\x y -> y /= path_sep) str
  in  head cp : map tail (tail cp)


-----------------------------------------------------------------
-- Read the requested file
-----------------------------------------------------------------

normalize :: String -> String
normalize n = 
  map (\c -> if c == '.' || c == '/' then file_sep else c) n ++ file_ext

-- Haskell aborts if an environment variable is not set. Avoid this
safeGetEnv :: String -> IO String
safeGetEnv var = 
  do res <- try (getEnv var)
     case res of
       Left err  -> return ""
       Right val -> return val

_openFile :: String -> IOMode -> IO (Either IOError Handle)
_openFile name mode = try (openFile name mode)

ioTraverse :: (b -> IO c) -> d -> [Either a b] -> IO (Either c d)
ioTraverse _ err [] = return (Right err)
ioTraverse f err (x:xs) = 
  case x of
    Left _   -> ioTraverse f err xs
    Right h  -> do res <- f h
                   return (Left res)
                  
primReadClass :: String -> IOMode -> IO (Either String VMErr)
primReadClass cname mode = 
  do _cp <- safeGetEnv "CLASSPATH"
     let cp = parseCP _cp
     ops <- mapM (flip _openFile mode) [ c ++ (file_sep : cname) | c <- cp]
     ioTraverse read_file (noClsDefFoundErr cname) ops
  where read_file h = do { hSetBinaryMode h True ; hGetContents h }
    
readClass :: String -> IO (Either [Int] VMErr) 
readClass cname = 
  do res <- primReadClass (normalize cname) ReadMode
     case res of
       Left str  -> return (Left (map ord str))
       Right err -> return (Right err)
