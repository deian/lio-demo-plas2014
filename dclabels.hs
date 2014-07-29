{-# LANGUAGE FlexibleContexts #-}
import LIO
import LIO.Run (privInit)
import LIO.DCLabel

--------------------------------------------------------------------
-- DCLabel consists of two formula: secrecy %% integrity
--
-- secrecy: describes the authority required to make 
--          the data public.
--
-- integrity: describes the authority with which
--            data was endorsed/is required to modify.
--------------------------------------------------------------------


--------------------------------------------------------------------
-- Secrecy
--------------------------------------------------------------------

public = True    %% True -- Public, untrusted
aliceS = "alice" %% True -- Alice can make data public
bobS   = "bob"   %% True -- Bob can make data public

-- Alice or Bob can make data public:
aliceOrBobS = "alice" \/ "bob" %% True

-- Examples:
example1 = 
 [ canFlowTo public                     ("alice" %% True)  == True
  ------------------------------------------------------------------
 , canFlowTo ("alice" %% True)          public             == False
  ------------------------------------------------------------------
 , canFlowTo ("alice" %% True)          ("bob" %% True)    == False
  ------------------------------------------------------------------
 , canFlowTo ("alice" \/ "bob" %% True) ("bob" %% True)    == True ]

--------------------------------------------------------------------
-- Integrity
--------------------------------------------------------------------


aliceI = True %% "alice" -- Alice endorsed
bobI   = True %% "bob"   -- Bob endorsed

-- Alice and Bob endorsed the data:
aliceAndBobI = True %% "alice" /\ "bob"

-- Examples:
example2 = 
 [ canFlowTo public                     (True %% "alice") == False
  ------------------------------------------------------------------
 , canFlowTo (True %% "alice")          public            == True
  ------------------------------------------------------------------
 , canFlowTo (True %% "alice")          (True %% "bob")   == False
  ------------------------------------------------------------------
 , canFlowTo (True %% "alice" /\ "bob") (True %% "bob")   == True  ]


--------------------------------------------------------------------
-- What happens when we combine data? Least Upper Bound!
--------------------------------------------------------------------

alice = "alice" %% "alice" -- Alice's data
bob   = "bob"   %% "bob"   -- Bob's data

aliceBobData = alice `lub` bob

-- Examples:
example3 = 
 [ canFlowTo alice aliceBobData == True
  ------------------------------------------------------------------
 , canFlowTo aliceBobData alice == False ]

--------------------------------------------------------------------
-- How do we encode authority? Privileges.
--------------------------------------------------------------------

-- A privilege can be used to:
--  - remove secrecy restrictions (make data more public)
--  - add integrity restrictions (make data more trustworthy)

--
-- downgradeP: make data as public as possible
--

aP = toCNF "alice"

example4 = 
 [ downgradeP aP ("alice" %% "alice")
  ------------------------------------------------------------------
 , downgradeP aP ("alice" \/ "bob" %% "alice")
  ------------------------------------------------------------------
 , downgradeP aP ("alice" /\ "bob" %% "alice") ]

--
-- canFlowToP: more permisive canFlowTo check that uses privileges
--

example5 =  -- revised example1
 [ canFlowToP aP ("alice" %% True) public            == False -- (T)
  ------------------------------------------------------------------
 , canFlowToP aP ("alice" %% True) ("bob" %% True)   == False -- (T)
  ------------------------------------------------------------------
 , canFlowToP aP ("bob" %% True)   ("alice" %% True) == False ]



-- Actually ...

-- Previous functions used "privilege descriptions", actual code
-- must provide actual privileges.
--
-- A privilege is a value of type Priv, which TCB code can "mint" with
-- privInit (more on this later)
makeAlicePriv :: IO (Priv CNF)
makeAlicePriv = privInit (toCNF "alice")
