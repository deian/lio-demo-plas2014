{-# LANGUAGE ScopedTypeVariables #-}
import LIO
import LIO.DCLabel
import LIO.LIORef

import LIO.Run (privInit)
import DemoHelper

-- How do you run LIO code?
--------------------------------------------------------------------

run :: Example -> IO ()
run lioExample = do
  -- TCB code, executes untrusted LIO code:
  privs <- mintPrivs
  (res, st) <- tryDC (lioExample privs)
  case res of
    Left err -> putStrLn $ "\n<"++ show (lioLabel st) ++ ">: " ++ show err
    Right r  -> putStrLn $ "\n<"++ show (lioLabel st) ++ ">"


-- Mint privileges for alice and bob
mintPrivs :: IO (DCPriv, DCPriv)
mintPrivs = do
  a <- privInit $ toCNF "alice"
  b <- privInit $ toCNF "bob"
  return (a, b)

type Example = (DCPriv, DCPriv) -> DC ()

-- Current label is the label on everything in scope
--------------------------------------------------------------------

example1 :: Example
example1 _ = do
  let (a, b) = (3, 4)
  lcur <- getLabel
  debug $ show (if True then a else b) ++ 
           " has label " ++ show lcur


-- Current label is raised to allow for permissive reads (part 1)
--------------------------------------------------------------------

example2 :: Example
example2 _ = do
  aliceRef <- newLIORef ("alice" %% True) "alice"
  debugV "Start!"

  a <- readLIORef aliceRef                -- OK!
  debugV $ "Read from alice: " ++ show a


-- Current label restricts allocations & writes (part 1)
--------------------------------------------------------------------

example3 :: Example
example3 _ = do
  aliceRef <- newLIORef ("alice" %% True) "alice"
  bobRef   <- newLIORef ("bob"   %% True) "bob"
  debugV "Start!"

  writeLIORef bobRef "bobby"              -- OK!
  debugV "Wrote to bob!"                  

  a <- readLIORef aliceRef                -- OK!
  debugV $ "Read from alice: " ++ show a

  writeLIORef aliceRef "wonderland"       -- OK!
  debugV $ "Wrote to alice!"

  writeLIORef bobRef "digital"            -- FAIL!
  debugV "Wrote to bob again!"                  



-- Current label restricts writes allocations & writes (part 2)
--------------------------------------------------------------------

-- revised example3
example4 :: Example
example4 (aP,_) = do
  aliceRef <- newLIORef ("alice" %% True) "alice"
  bobRef   <- newLIORef ("bob"   %% True) "bob"
  debugV "Start!"

  writeLIORef bobRef "bobby"              -- OK!
  debugV "Wrote to bob!"                  

  a <- readLIORef aliceRef                -- OK!
  debugV $ "Read from alice: " ++ show a

  writeLIORef aliceRef "wonderland"       -- OK!
  debugV $ "Wrote to alice!"

  writeLIORefP aP bobRef "digital"        -- NOW OK!
  debugV "Wrote to bob again!"                  



-- Current label is raised to allow for permissive reads (part 2):
--------------------------------------------------------------------

-- revised example2
example5 :: Example
example5 (aP,_) = do
  aliceRef <- newLIORef ("alice" %% True) "alice"
  debugV "Start!"

  a <- readLIORefP aP aliceRef             -- OK! (NO TAINT)
  debugV $ "Read from alice: " ++ show a


-- Context also has current clearance
--------------------------------------------------------------------

example6 :: Example
example6 _ = do
  clr <- getClearance
  debug $ "Clearance is: "++ show clr


-- Current label cannot be raised above clearance
--------------------------------------------------------------------

-- revised example2
example7 :: Example
example7 _ = do
  aliceRef <- newLIORef ("alice" %% True) "alice"
  debugVV "Start!"

  setClearance dcPublic
  debugV  "Lowered clearance!"

  a <- readLIORef aliceRef             -- NOW FAIL!
  debugVV $ "Read from alice: " ++ show a


-- Allocations above clearance not allowed
--------------------------------------------------------------------

-- revised example2
example8 :: Example
example8 _ = do
  setClearance dcPublic
  debugVV "Start!"

  newLIORef ("alice" %% True) "alice" -- FAIL!
  return ()

-- Scoping clearance
--------------------------------------------------------------------

example9 :: Example
example9 _ = do
  aliceRef <- newLIORef ("alice" %% True) "alice"
  bobRef   <- newLIORef ("bob"   %% True) "bob"
  debugVV "Start!"

  ignoreFail $ withClearance (labelOf bobRef) $ do
      -- UNTRUSTED CODE HERE: ----------------------
      writeLIORef bobRef "bobby"            -- OK!
      debugVV "Wrote to bob!"                  
     
      a <- readLIORef aliceRef              -- FAIL!
      debugVV $ "Bob read from alice: " ++ show a

  a <- readLIORef aliceRef                  -- OK!
  b <- readLIORef bobRef                    -- OK!
  debugVV $ "Read from both: " ++ show [a, b]

    where ignoreFail act = act `catch` \(e::SomeException) -> 
                             debugVV "Caught exception"
