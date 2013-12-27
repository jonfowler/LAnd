module LAnd where

import Control.Concurrent

-- Bottom represents an empty unwritten to MVar. Info a represents the written to variable
data Lifted a = Bottom | Info a deriving Show

-- Extract value v1 and v2 using function "f"
get2MVar :: (Poset a) => MVar a -> MVar b -> (Lifted a -> Lifted b -> Lifted c) -> IO c
get2MVar v1 v2 f = do
  lv1 <- toLifted `fmap` tryTakeMVar v1
  lv2 <- toLifted `fmap` tryTakeMVar v2
  case f lv1 lv2 of
    Bottom -> get2MVar v1 v2 f
    Info a -> return a 

toLifted :: Maybe a -> Lifted a
toLifted (Just a) = Info a
toLifted Nothing = Bottom

-- Get rid of "Top" (represented by Nothing) and throw error if this is the case
pureJoin :: (Poset a) => a -> a -> a
pureJoin a b = case join a b of
  Nothing -> error "Incorrect write"
  Just a -> a

-- join an MVar
joinMVar :: Poset a => MVar a -> a -> IO ()
joinMVar mvar v = v `seq` do
  cur <- tryTakeMVar mvar
  case cur of
    Nothing -> putMVar mvar v
    Just curv -> putMVar mvar (pureJoin curv v)

-- Lazy and Commutative AND!!!
(|&&|) :: Bool -> Bool -> IO Bool
a |&&| b = do
  mvar1 <- newEmptyMVar
  mvar2 <- newEmptyMVar
  t1 <- forkIO $ joinMVar mvar1 a
  t2 <- forkIO $ joinMVar mvar2 b
  res <- get2MVar mvar1 mvar2 lAnd
  killThread t1 >> killThread t2
  return res
{- some example tests (need to compile threaded):
let x = [1..]
(x==x) |&&| False -- should return False even though (x==x) = _|_
False |&&| (x==x) -- ditto 
-}

-- liftedAnd 
lAnd :: Lifted Bool -> Lifted Bool -> Lifted Bool
lAnd (Info False) _ = Info False -- note that (lAnd Bottom x) will fall through to next line
lAnd _ (Info False) = Info False
lAnd (Info True) (Info True) = Info True
lAnd _ _ = Bottom

-- Poset class
class Poset a where
  -- (<#) == joinLeq
  (<#) :: a -> a -> Bool
  -- join, the result of Nothing here refers to Top. In the same way we can lift a type with Bottom we can also lift a type with Top.
  join :: a -> a -> Maybe a 

instance Poset Bool where
  -- standard ordering on unlifted bools (i.e. a<#a <=> a==a)
  (<#) = (==) 
  join a b = if a == b then Just a else Nothing

instance Poset a => Poset (Lifted a) where
  -- Lifting a type to include Bottom. forall a. Bottom <# a
  Bottom <# _ = True
  _ <# Bottom = False
  Info a <# Info b = a<#b

  join Bottom a = return a
  join a Bottom = return a
  join (Info a) (Info b) = case join a b of
    Just a -> Just $ Info a
    Nothing -> Nothing







-- Some more unused examples of posets

instance (Poset a, Poset b) => Poset (a,b) where
  (a,b) <# (a2,b2) = a<#a2 && b<#b2
  join (a1,b1) (a2,b2) = do 
    a <- join a1 a2 
    b <- join b1 b2
    return (a,b)

instance Poset Int where
  (<#) = (==) 
  join a b = if a == b then return a else Nothing


  
  

