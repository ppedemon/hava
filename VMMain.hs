module Main where

import System

import VM
import VMMonad(runVM)


{---------------------------------------------------------------------
  The Haskell Virtual Machine (hava) entry point
---------------------------------------------------------------------}

main :: IO ()
main = 
  do (c:argv) <- getArgs
     let f  = vmmain c argv
     (vm,_) <- runVM f vmcreate
     putStrLn $ show vm
      