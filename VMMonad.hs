module VMMonad 
  (VM
  -- State management
  ,getS
  ,setS
  -- Raise an error
  ,raise
  -- Inject an IO operation into the VM Monad
  ,io2vm
  -- Run a batch of VML actions
  ,runVM
  -- Provide state transformations
  ,inject
  -- Error handling
  ,handle
  ) where 

-----------------------------------------------------------------------
-- The VM monad. Tested within the Win32 Hugs environment.
-- Author       : Pablo J. Pedemonte
-- Last revision: 15-Jan-02
--
-- The Virtual Machine monad
-- Useful for several VM operations
-----------------------------------------------------------------------


import VMErr


data VM s a = VM (s -> IO (s,Either a VMErr))

unVM (VM f) = f

instance Monad (VM s) where
  return a = VM $ \s -> return (s,Left a)
  m >>= f  = VM $ \s ->
    let _f = unVM m 
    in  do (_s,a) <- _f s
           case a of
             Left _a     -> do { p <- unVM (f _a) _s ; return p }
             Right vmErr -> return (_s,Right vmErr)


-----------------------------------------------------------------------
-- State management
-----------------------------------------------------------------------

getS :: VM s s
getS = VM $ \s -> return (s, Left s)

setS :: s -> VM s ()
setS s = VM $ \_ -> return (s, Left ())


-----------------------------------------------------------------------
-- Raise an error
-----------------------------------------------------------------------

raise :: VMErr -> VM s a
raise vmErr = VM $ \s -> return (s, Right vmErr)


-----------------------------------------------------------------------
-- Inject an IO operation into the VM Monad
-----------------------------------------------------------------------

io2vm :: IO a -> VM s a
io2vm m = VM $ \s -> do { a <- m ; return (s,Left a) }


-----------------------------------------------------------------------
-- Run a batch of VML actions
-----------------------------------------------------------------------

runVM :: VM s a -> s -> IO (s, Either a VMErr)
runVM = unVM


-----------------------------------------------------------------------
-- Inject computations based on a state type r
-- into a sequence using a state type s
-----------------------------------------------------------------------

inject :: VM r a -> (s -> r) -> (r -> s) -> (VM s a)
inject m f g = VM $ \s -> 
  do (r,a) <- runVM m (f s)
     return (g r,a)


-----------------------------------------------------------------------
-- Exception handling
-----------------------------------------------------------------------

handle :: VM s a -> (VMErr -> VM s a) -> VM s a
handle p h = VM $
  \s -> do (_s,a) <- unVM p s
           case a of
             Left _a     -> return (_s,Left _a)
             Right vmErr -> do { p <- unVM (h vmErr) _s ; return p }
