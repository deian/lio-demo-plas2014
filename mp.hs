-- Imports
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import LIO
import LIO.Error (withContext)
import LIO.DCLabel
import LIO.DB.Simple
import LIO.DB.Simple.FS

import LIO.Run (privInit)
import DemoHelper

import Data.UUID
import LIO.Data.UUID.V4
import Data.Maybe
import Data.List (nub, (\\))
import Control.Monad
import qualified System.Directory as IO

--
-- Intro
--

-- Models are instances of:
{-
class Model obj where
  -- | Unique key of object @obj@
  keyField  :: obj -> Key obj
  -- | Unique name of persistent model for @obj@
  modelName :: obj -> ModelName
-}

-- DB interface for storing/retrieving objects of type @obj@:
{-
class SimpleDB l m obj where
  withModel :: ModelHandle obj -> m a -> LIO l a
  exists    :: Key obj -> m Bool 
  findObj   :: Key obj -> m obj
  ...
-}

-- Policies are instances of:
{-
class SimpleDB l m obj => Policy l m obj where
  -- | Label policy on model containig objects of type @obj@
  modelLabel :: PrivDesc l p => Priv p -> ModelHandle obj -> m l
  -- | Label polcy on object @obj@
  objLabel   :: PrivDesc l p => Priv p -> obj -> m l
-}

--
-- MPs
--

-- User MP
----------------------------------------------------------

-- User model
----------------------------------------------------------

type UserName = String

data User = User { userName :: UserName }
                 deriving (Eq, Show, Read)

instance Model User where 
  keyField    = Key . userName  -- How do we index users?
  modelName _ = "user"          -- What is the model name?
                 
users :: ModelHandle User       -- Handle for DB model
users = ModelHandle

-- Helper for creating a new user
createUser priv name = withContext "createUser" $
  withModel users $ do
    -- Check if user exists in DB
    found <- exists (Key name :: Key User)
    -- If not, insert User with given name
    unless found $ insertP_ priv $ User { userName = name }
    return name

-- User policy
----------------------------------------------------------

instance Policy DCLabel FileDB User where
  -- Anybody can create and read from user model:
  modelLabel _ _ = return dcPublic
  -- Only user with priv can create object with that username
  objLabel _ usr = return $ 
    userName usr %% userName usr \/ "root"

-- Conference MP
----------------------------------------------------------

-- Conference model
----------------------------------------------------------

data Config = Config { pcMembers :: [UserName]
                     , status    :: ConfStatus }
                     deriving (Eq, Show, Read)

data ConfStatus = Submission | Reviewing | Done
                  deriving (Eq, Show, Read)

config :: ModelHandle Config
config = ModelHandle

instance Model Config where 
  keyField    = const $ Key "config" -- Only one key
  modelName _ = "config"

-- Helpers

getConfig = findObj $ Key "config"

addPCMember priv name = withContext "addPCMember" $ 
  withModelP config priv $ do
    cfg <- getConfig
    saveP_ priv $ cfg { pcMembers = nub (name : pcMembers cfg) }

getPCMembers = withContext "getPCMembers" $ withModel config $ do
  cfg <- getConfig
  return $ pcMembers cfg

getConfStatus = withContext "getConfStatus" $ withModel config $ 
  status `liftM` getConfig

setConfStatus priv stat = withContext "setConfStatus" $ 
  withModelP config priv $ do
    cfg <- getConfig 
    saveP_ priv $ cfg { status = stat }

-- Conference policy
----------------------------------------------------------

instance Policy DCLabel FileDB Config where
  modelLabel _ _ = return $ True %% "root"
  objLabel _ _   = return $ True %% "root"

-- Paper MP
----------------------------------------------------------

-- Paper model
----------------------------------------------------------

data Paper = Paper  { paperId        :: UUID
                    , paperAuthors   :: [UserName]
                    , paperTitle     :: String
                    , paperConflicts :: [UserName] }
                    deriving (Eq, Show, Read)

instance Model Paper where 
  keyField    = Key . toString . paperId -- How do we index papers?
  modelName _ = "paper"                  -- What is the model name?

papers :: ModelHandle Paper              -- Handle for DB model
papers = ModelHandle

genPaperId :: FileDB UUID                -- Create unique paper id
genPaperId = liftLIO nextRandom

-- Helper for submitting new paper
submitPaper priv p = withContext "submitPaper" $ 
  withModelP papers priv $ do
    _id <- genPaperId
    insertP priv $ p { paperId = _id }

-- Paper policy
----------------------------------------------------------

instance Policy DCLabel FileDB Paper where
  -- Anybody can create and read from paper model:
  modelLabel _ _ = return dcPublic

  -- Who can read paper @paper@?
  --    root, paper authors and PC members (not other authors)
  --
  -- Who can modify/create @paper@?
  --    root, and if deadline has not passed the authors
  objLabel priv paper   = do
    pc     <- liftLIO getPCMembers
    status <- liftLIO getConfStatus
    let readers = fromList $  "root" : (paperAuthors paper ++ pc)
        writers = fromList $  "root" : if status == Submission 
                                         then paperAuthors paper
                                           else []
    return $ readers %% writers

-- Review MP
----------------------------------------------------------

-- Review model
----------------------------------------------------------

data Review = Review  { reviewId      :: UUID
                      , reviewPaper   :: Key Paper
                      , reviewAuthor  :: UserName
                      , reviewContent :: String }
                      deriving (Eq, Show, Read)

instance Model Review where 
  keyField    = Key . toString . reviewId
  modelName _ = "review"

reviews :: ModelHandle Review
reviews = ModelHandle

genReviewId :: FileDB UUID
genReviewId = liftLIO nextRandom

submitReview priv rev = withModelP reviews priv $ do
  _id <- genReviewId
  insertP priv $ rev { reviewId = _id }

-- Review policy
----------------------------------------------------------

instance Policy DCLabel FileDB Review where
  -- Who can read from the review model?
  --   Anybody. Reviews are individually protected.
  --
  -- Who can write to the review model?
  --   root and PC members
  --
  modelLabel priv _ = do
    pc <- liftLIO getPCMembers
    return $ True %% (fromList $ "root" : pc)

  -- Who can create/write review @rev@?
  --   Root and reviewer
  --
  -- Who can read review @rev@?
  --   root, all non-conflicting PC members, and
  --   if review process is over: paper authors
  --
  objLabel priv rev = do
    allPC  <- liftLIO getPCMembers
    status <- liftLIO getConfStatus
    paper  <- findObjP priv $ reviewPaper rev
    let pc      = allPC \\ paperConflicts paper
        readers = if status == Done
                     then fromList $  
                            "root" : (paperAuthors paper ++ pc)
                     else fromList $  "root" : pc
        writers = "root" \/ reviewAuthor rev
    return $ readers %% writers


--
-- Examples
--

-- Execute as user (in web apps, this is done)
as :: UserName -> DC a -> DC a
as user act = withContext ("as:" ++ user) $ 
  withClearance (user %% True) act

main = do
  -- Remove existing File DB
  IO.removeDirectoryRecursive "model"

  -- Mint privileges for our users
  rootPriv    <- privInit $ toCNF "root"
  alicePriv   <- privInit $ toCNF "alice"
  bobPriv     <- privInit $ toCNF "bob"
  charliePriv <- privInit $ toCNF "charlie"
  danPriv     <- privInit $ toCNF "dan"
  evePriv     <- privInit $ toCNF "eve"
  floPriv     <- privInit $ toCNF "flo"

  evalDC $ do

    startConf rootPriv
      
    -- Create users (can be done at any time)

    alice   <-  createUser alicePriv   "alice"
--  _       <-  createUser alicePriv   "bob" -- FAIL!
    bob     <-  createUser bobPriv     "bob"
    charlie <-  createUser charliePriv "charlie"
    dan     <-  createUser danPriv     "dan"
    eve     <-  createUser evePriv     "eve"
    flo     <-  createUser floPriv     "flo"

    -- Add users to PC (can be done at any time)

    addPCMember rootPriv alice
--  addPCMember bobPriv  bob   -- FAIL!
    addPCMember rootPriv bob
    addPCMember rootPriv eve


    p1 <- submitPaper charliePriv $ 
            Paper { paperAuthors   = [charlie, alice]
                  , paperTitle     = "PLAS is great!"
                  , paperConflicts = [alice, dan]
                  , paperId        = undefined }

    setConfStatus rootPriv Reviewing

    {-
    _  <- submitPaper bobPriv $ 
            Paper { paperAuthors   = [bob]
                  , paperTitle     = "Past deadline!"
                  , paperConflicts = []
                  , paperId        = undefined }
    -- FAIL! (After submission period)
    -}

    r1 <- submitReview bobPriv $ 
              Review  { reviewPaper   = p1
                      , reviewAuthor  = bob
                      , reviewContent = "paper is okay"
                      , reviewId      = undefined }

    as "eve" $ withModel reviews $ findObjP evePriv r1

--  as "dan"     $ withModel reviews $ findObjP danPriv r1     -- FAIL!
--  as "alice"   $ withModel reviews $ findObjP alicePriv r1   -- FAIL!
--  as "charlie" $ withModel reviews $ findObjP charliePriv r1 -- FAIL!

    setConfStatus rootPriv Done

    as "alice"   $ withModel reviews $ findObjP alicePriv r1
    as "charlie" $ withModel reviews $ findObjP charliePriv r1

--  as "dan"     $ withModel reviews $ findObjP danPriv r1     -- FAIL!
--  as "flo"     $ withModel reviews $ findObjP floPriv r1     -- FAIL!




-- Helpers
----------------------------------------------------------

fromList = dFromList . map principal

startConf priv = handle (\(_::SomeException) -> return ()) $ do
    withModelP config priv $ do
      insertP_ priv $ Config { pcMembers = []
                             , status    = Submission }
    setConfStatus priv Submission
