module DemoHelper where

import LIO.TCB (ioTCB)
import LIO
import LIO.DCLabel

debug :: String -> DC ()
debug str = ioTCB $ putStrLn str

debugV :: String -> DC ()
debugV str = do
 l <- getLabel
 debug ("<" ++ show l ++">: "++str)

debugVV :: String -> DC ()
debugVV str = do
 l <- getLabel
 c <- getClearance
 debug ("<" ++ show l ++","++ show c ++">: "++str)
