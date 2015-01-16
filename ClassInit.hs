module ClassInit(initialize) where

import List(find)

import MA
import VMStack
import VMMonad
import ClassRep
import {-# SOURCE #-} VM


{--------------------------------------------------------------------
  Class initialization routines:
  
  This is a fake initialization: any exception thrown during it
  and not catched, doesn't get propagated outside the initialization
  context.
  
  Anyway, the most probable thing to happen when some init error
  arises, is that your are doomed. So this is quite acceptable.
  
  Moreover, this initialization procedure works only in a single
  threaded JVM. With multiple threads accessign concurrently the
  method area, we should be a lot more careful.
--------------------------------------------------------------------}

initialize :: MAIx -> VM_ ()
initialize ix = 
  do c <- vmgetClass ix
     case getState c of
       Init       -> return ()
       InProgress -> return ()
       UnInit     -> do vmsetClassState ix InProgress
                        case getSuper c of
                          N   -> return ()
                          S s -> initialize s
                        primInitialize ix
                        vmsetClassState ix Init

vmsetClassState :: MAIx -> CStat -> VM_ ()
vmsetClassState ix st = 
  do c <- vmgetClass ix
     vmreplaceClass ix (setState st c)

clinitMethod :: Class -> Maybe MInfo
clinitMethod c = 
  let ms = getStaticMethods c
  in  find (\m -> elemName m == "<clinit>" && elemDesc m == "()V") ms

installStack :: Stack -> VM_ Stack
installStack s = do vm <- getS
                    let old_s = vmgetStack vm
                    setS (vmsetStack vm s)
                    return old_s
               
primInitialize :: MAIx -> VM_ ()
primInitialize ix =
  do c <- vmgetClass ix 
     case clinitMethod c of
       Nothing -> return ()
       Just m  ->
         do vm    <- getS
            old_s <- installStack newStack
            vminvoke ix m []
            vmloop
            installStack old_s
            return ()
