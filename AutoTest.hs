{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module AutoTest where

import Submission2
import Test (runTests, test, testFromFile, Test (..), Question (..))

import Lib
import Data.Map (fromList)
import qualified Data.Map as M
import Data.Coerce
import Control.Exception
import GHC.Generics
import Data.Type.Bool
import Data.List
import Data.Function
import Data.Maybe

import Debug.Trace

import Control.Monad
import Control.DeepSeq

import System.IO

deriving instance Num Target
deriving instance Num Source
deriving instance Num WormholeId
deriving instance Eq Order
deriving instance Ord Order

instance Read PageRank where
  readsPrec = coerce (readsPrec @Double)

instance Read PlanetRank where
  readsPrec = coerce (readsPrec @Double)

-- | Question n and its max mark
qn :: String -> Question
qn i = case lookup i [
    ("1", 10),
    ("2", 10),
    ("3", 10),
    ("4", 10),
    ("5", 10),
    ("6", 10),
    ("7", 10),
    ("8", 10),
    ("9", 10)
    ] of
        Just n -> Question (i, n)
        Nothing -> error $ "Question Doens't exist: " ++ i

main :: IO ()
main = runTests $ do
  testFromFile "Find enemy tests" (qn "1") findEnemyPlanet (==) "tests/findEnemyTests.txt"
  test "AIState rushTarget test"  (qn "2") (const hasRushTargetField) (==) [() :=> True]
  testFromFile "Send tests"  (qn "3") (uncurry3 send) sameOrders "tests/sendTests.txt"
  testFromFile "Attack from all tests"  (qn "4") (uncurry attackFromAll) sameOrders "tests/attackFromAllTests.txt"
  testFromFile "Zerg Rush test" (qn "5") zergRushTest sameOrdersOrNewTrg "tests/zergRushTests.txt"
  testFromFile "InitPageRank' tests" (qn "6") (initPageRank' @String @[(String, Integer)]) same "tests/initPageRankTests.txt"
  testFromFile "PageRank tests"  (qn "7") (pageRank' @PlanetId @GameState) same "tests/pageRankTests.txt"
  testFromFile "PlanetRank tests"  (qn "8") planetRank same "tests/planetRankTests.txt"
  testFromFile "PlanetRankRush tests"  (qn "9") (\x -> planetRushTest (x, initialState)) sameOrders "tests/planetRankRushTests.txt"

epsilon :: Fractional a => a
epsilon = 0.001

same :: (Ord k, Fractional a, Ord a) => M.Map k a -> M.Map k a -> Bool
same ps1 ps2 = and (M.map (< epsilon) (M.unionWith (\p1 p2 -> abs (p1 - p2)) ps1 ps2))

planetRushTest :: (GameState, AIState) -> [Order]
planetRushTest (st, ai) = os
    where 
      (os,_,ai') = planetRankRush st ai

zergRushTest :: (GameState, Maybe PlanetId) -> Either [Order] PlanetId
zergRushTest (st, trg) | hasRushTargetField = 
  case trg of
    Just _ | htrg == getAIStateRT ai' -> Left os
    _ -> case getAIStateRT ai' of
      NoRushTarget -> error "rushTarget field not defined"
      RushTarget (Just x) -> Right x
      RushTarget Nothing -> error "rushTarget value is Nothing"
      ConflictRushTarget -> error "conflicting rushTargets in AIState"
    where 
      ai = case trg of 
        Just pid -> setAIStateRT initialState pid
        _ -> initialState 
      htrg = RushTarget trg
      (os,_,ai') = zergRush st ai
zergRushTest _ = error "rushTarget field not defined"


sameOrdersOrNewTrg :: Either [Order] PlanetId -> Either [Order] PlanetId -> Bool
sameOrdersOrNewTrg (Left os1) (Left os2) = sameOrders os1 os2
sameOrdersOrNewTrg (Right i) (Right j) = i == j 
sameOrdersOrNewTrg _ _ = False

sameOrders :: [Order] -> [Order] -> Bool
sameOrders os1 os2
  = (normalise . rmZeros) os1 == (normalise . rmZeros) os2
  where normalise :: [Order] -> [Order]
        normalise os = map (foldr1 combine) (groupBy ((==) `on` wh) (sortOn wh os))
        combine :: Order -> Order -> Order
        combine (Order w s) (Order _ s') = Order w (s + s')
        wh (Order w _) = w
        rmZeros :: [Order] -> [Order]
        rmZeros = filter (\(Order _ i) -> i /= 0)

type family HasRushTargetField (s :: * -> *) :: Bool where
  HasRushTargetField (D1 _ x) = HasRushTargetField x
  HasRushTargetField (C1 _ x) = HasRushTargetField x
  HasRushTargetField (l :*: r) = HasRushTargetField l || HasRushTargetField r
  HasRushTargetField (l :+: r) = HasRushTargetField l && HasRushTargetField r
  HasRushTargetField (S1 ('MetaSel ('Just "rushTarget") _ _ _) (Rec0 (Maybe PlanetId))) = 'True
  HasRushTargetField _ = 'False

class KnownBool (b :: Bool) where
  boolVal :: Bool

instance KnownBool 'True where
  boolVal = True

instance KnownBool 'False where
  boolVal = False

hasRushTargetField :: Bool
hasRushTargetField = boolVal @(HasRushTargetField (Rep AIState))

uncurry3 :: (a->b->c -> d) -> (a,b,c) -> d
uncurry3 f ~(a, b, c) = f a b c

initPageRankGen :: Int -> M.Map String [(String, Integer)]
initPageRankGen total = bar
  where
    ids = [1..total]
    rotatel xs = case xs of
      [] -> []
      x:xs -> xs ++ [x]
    cycle = M.fromList $ zipWith (\ i j -> (i, [j])) ids (rotatel ids)
    foo = M.mapWithKey (\i js -> ((i + 7) `mod` total) + 1: js) cycle
    bar = M.map (map (\j -> (show j, 1))) $ M.mapKeys show foo

---------------------------------------------------------------------------------
-- ** Rushtarget value extraction

{-

The general idea is any type is assumed to not have a rush target. 
But this is overlappable in the case it does according to the instances below (same cases as HasRushTargetField).
Then we can ask about the rushtarget for any AIState that contains the rushTarget field.

-}

data HasRushTarget = NoRushTarget | RushTarget (Maybe PlanetId) | ConflictRushTarget
  deriving(Show, Eq, Ord)

class GetRushTarget (x :: * -> *) where
  getRushTarget :: x y -> HasRushTarget
  setRushTarget :: x y -> PlanetId -> x y

instance GetRushTarget (S1 ('MetaSel ('Just "rushTarget") a b c) (Rec0 (Maybe PlanetId))) where
  getRushTarget (M1 (K1 m_pi)) = RushTarget m_pi
  setRushTarget (M1 (K1 m_pi)) = M1 . K1 . Just

instance GetRushTarget b => GetRushTarget (D1 a b) where
  getRushTarget = getRushTarget . unM1
  setRushTarget (M1 bc) = M1 . setRushTarget bc

instance GetRushTarget b => GetRushTarget (C1 a b) where
  getRushTarget = getRushTarget . unM1
  setRushTarget (M1 bc) = M1 . setRushTarget bc

instance (GetRushTarget l, GetRushTarget r) => GetRushTarget (l :+: r) where
  getRushTarget (L1 x) = getRushTarget x
  getRushTarget (R1 x) = getRushTarget x
  setRushTarget (L1 x) = L1 . setRushTarget x
  setRushTarget (R1 x) = R1 . setRushTarget x

instance (GetRushTarget l, GetRushTarget r) => GetRushTarget (l :*: r) where
  getRushTarget (l :*: r) = case (getRushTarget l, getRushTarget r) of
    (NoRushTarget, RushTarget m) -> RushTarget m
    (RushTarget m, NoRushTarget) -> RushTarget m
    (NoRushTarget, NoRushTarget) -> NoRushTarget
    (RushTarget m, RushTarget n) | m == n -> RushTarget m
    _ -> ConflictRushTarget
  setRushTarget st@(l :*: r) pid = case (getRushTarget l, getRushTarget r) of
    (NoRushTarget, RushTarget m) -> l :*: setRushTarget r pid
    (RushTarget m, NoRushTarget) -> setRushTarget l pid :*: r
    _ -> st

-- ! TODO need to limit what aistate can range over
instance {-# Overlappable #-} GetRushTarget aistate where
  getRushTarget _ = NoRushTarget
  setRushTarget = const

getAIStateRT :: AIState -> HasRushTarget
getAIStateRT ai = getRushTarget (from ai)

setAIStateRT :: AIState -> PlanetId -> AIState
setAIStateRT ai = to . setRushTarget (from ai)

