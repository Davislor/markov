{- MDP.hs		Code for CS 533, Mini-Project 2.
 -
 - Copyright © 2012 Loren B. Davis.  ALL RIGHTS RESERVED.  I hereby release
 - this code under the MIT License: (http://www.opensource.org/licenses/MIT).
 - Since this is an assigned exercise, remember your academic integrity!
 -}

module MDP {- ( MarkovDP, State, Action, policyIterate, showUpdates, test4x3,
             showT, decodeT, whenToPass, whenToPark, parkingProblem,
             ParkingState, encodeState, decodeState ) -}
  where
import Data.Array.Unboxed
import Data.List
import Data.Ord
import Numeric.LinearAlgebra (Matrix, build, linearSolveSVD, toLists)
-- import Numeric.LinearAlgebra hiding (i)
-- import Numeric.LinearAlgebra.Algorithms hiding (i)
import Prelude hiding (pi)
{- This program uses the hmatrix package (cabal install hmatrix) rather than
 - attempt to reinvent the wheel.  You might also need to install libgsl,
 - liblapack, and possibly MKL or ATLAS.
 -}

type State = Int
type Action = Int
type MarkovDP = ( Int,
                  Int,
                  UArray State Double,
                  UArray (State,Action,State) Double
                )
{- n = The number of states.  S = {0,...,n-1}
 - m = The number of actions. A = {0,...,m-1}
 - r = An array of size n representing the reward function.  r[s] is the
 -     reward associated with state s.
 - t = A three-dimensional array representing the transition function, such
 -     that T(s,a,s') = t ! (s,a,s').  Note that the values are between 0 and
 -     1, each row sums to 1, and that the matrices will typically be sparse.
 -}
type PIUpdate = [(State, Action)]
{- The update of each step of the policy iteration algorithm is the list
 - of pairs (s,a) such that the new policy will take action a in state s.
 -}

policyIterate :: MarkovDP ->
                 Double ->
                 (UArray State Double, UArray State Action, [String])
{- Parameters:
 - process 	= The Markov decision process to solve, in the form
 -		  (|S|,|A|,R,T)
 - gamma 	= The discount parameter, between 0 and 1.
 -
 - Returns (v, pi, updates) where
 - v 		= A size-n array representing the value function.  The expect-
 -		  ed value of being in state s is v[s].
 - pi 		= A size-n array representing the policy.  The action that pi
 -		  will take in state s is pi[s].
 - updates 	= A human-readable list of update strings, one per step of the
 -		  iteration.
 -
 - This implementation always starts with a policy of taking the first action
 - in any situation.  This is not really correct (the algorithm in the text
 - says to try a "random" policy), but saves me from dealing with stateful
 - computation in this program.  To specify a different initial policy, call
 - policyIterate1.  It does not attempt any asynchronous policy iteration.
 -}
policyIterate (n, m, r, t) gamma = (v', pi'', translatePIUpdates updates'' )
  where
    ( v', pi'', updates'' ) = seq ((n,m,r,t), gamma, naive)
                                  (policyIterate1 naive [])
    naive = (initialPolicy n)
    policyIterate1 :: UArray State Action ->
                      [PIUpdate] ->
                      (UArray State Double, UArray State Action, [PIUpdate])
{- Parameters:
 - pi 		= The initial policy to attempt to improve.
 - updates	= The previous iterations' updates.
 -
 - Returns (v, pi) where
 - v 	= A size-n array representing the value function.  The expected value
 -     of being in state s is v[s].
 - pi' 	= A size-n array representing the policy.  The action that pi' will
 -       take in state s is pi[s].
 - updates' = The list of updates for all iterations of the algorithm.  When
 -            there are no further updates, the algorithm terminates.
 -}
    policyIterate1 pi updates | diff == [] = (v, pi', updates')
                              | otherwise  = policyIterate1 pi' $! updates'
      where
        states :: [State]
        states = [0..n-1]
        actions :: [Action]
        actions = [0..m-1]
        v :: UArray State Double
{- The updated value function.  This is based on eq. 17.10 in (Russell &
 - Norvig 2010), rather than the modified-policy-iteration algorithm in fig.
 - 17.7.  It uses LAPACK to solve the system of linear equations in O(n^3)
 - time.
 -}
        v = let
              a :: Matrix Double
              a = build (n,n) (\i j  -> (gamma * fromIntegral (t!(i,pi!i,j))) -
                                              (if i == j then 1.0 else 0.0))
              b :: Vector Double
              b = build n (\i -> - (fromIntegral (r!i)) )
              x :: Matrix Double
              x = linearSolveSVD a b
            in
              listArray (0,n-1) (concat . toLists $ x)
        pi' :: UArray Int Action
{- The updated policy pi' is the best action to take in any state (determined
 - by one-step lookahead).  Q.v. eq. 17.4 in (Russell & Norvig 2010).
 -}
        pi' = seq v (listArray (0, n-1)
                        (map (\i -> maximumBy (argmax i) actions)
                             states))
        argmax :: State -> Action -> Action -> Ordering
{- A helper function that compares the expected value of actions a1 and a2
 - from state s0.
 -}
        argmax s0 a1 a2 = let
                            u1 = sum .
                                 map (\s' -> t!(s0,a1,s') * v!s') $
                                 states
                            u2 = sum .
                                 map (\s' -> t!(s0,a2,s') * v!s') $
                                 states
                          in compare u1 u2
        diff :: PIUpdate
        diff = seq pi' ((assocs pi') \\ (assocs pi))
        updates' :: [PIUpdate]
        updates' = seq diff (updates ++ [diff])

initialPolicy :: Int -> UArray Int Action
{- A policy for a MDP with n states that always takes action zero.  This is
 - not the same as the "random" policy I'm supposed to use for the algorithm,
 - but is simpler to write in this language.
 -}
initialPolicy n = listArray (0, n-1) (take n (repeat 0))

initialVF :: Int -> UArray Int Double
{- A value function initialized to n zeroes.  Unlike the above, this is corr-
 - ect.  (But unused in policy iteration.)
 -}
initialVF n = listArray (0, n-1) (take n (repeat 0.0))

translatePIUpdates :: [PIUpdate] -> [String]
translatePIUpdates u =
  map (\i -> "Step " ++ show (i+1) ++ ": " ++ translatePIUpdate (u !! i))
      [0 .. (length u) - 1]

translatePIUpdate :: PIUpdate -> String
translatePIUpdate [] = "Done."
translatePIUpdate [(s,a)] = "take action " ++
                            show a ++
                            " in state " ++
                            show s ++
                            "."
translatePIUpdate ((s,a):xs) = "take action " ++
                               show a ++
                               " in state " ++
                               show s ++
                               "; " ++
                               translatePIUpdate xs

-- Display the returned update messages:
showUpdates :: (UArray State Double, UArray State Action, [String]) ->
               IO ()
showUpdates (_,_,u) = putStrLn . unlines $ u

-- A useful means of displaying only the nonzero entries of the sparse array
-- t:
showT :: MarkovDP -> [((State,Action,State),Double)]
showT (_,_,_,t) = filter ((> 0.0) . snd) (assocs t)

-- The parking-lot world:

{- The decoded values of a state of the parking-lot world.  Note that Exit
 - in this implementation is a terminal state that the park action automat-
 - ically reaches, not a third action.  Crash is another world state, always
 - and only reached by attempting to park in a filled spot.
 -
 - A i True is the state in which the agent is in front of row A, space i,
 - which is filled.  A i False is similar, but the spot is empty; likewise
 - for row B.  I take advantage of the symmetry between parking in row A and
 - in row B to define P i as the state of being parked i spaces from the
 - store.  This reduces the size of the state space from 8n to 5n+1. The
 - range of i is 1 to n.
 -}
data ParkingState = A Int Bool | B Int Bool | P Int | Exit | Crash
  deriving (Eq, Show)

encodeState :: Int -> ParkingState -> State
{- Converts a ParkingState into an index suitable for the PolicyIterate algo-
 - rithm.  The encoding depends on the number of rows, n.
 -}
encodeState _ Exit = 0
encodeState _ Crash = 1
encodeState _ (P i) = 1 + i
encodeState n (A i True) = 1 + n + i
encodeState n (A i False) = 1 + 2*n + i
encodeState n (B i True) = 1 + 3*n + i
encodeState n (B i False) = 1 + 4*n + i

decodeState :: Int -> State -> ParkingState
{- The inverse of the mapping above.
 -}
decodeState _ 0 = Exit
decodeState _ 1 = Crash
decodeState n m | m <= 1 + n = P (m-1)
                | m <= 1 + 2*n = A (m-n-1) True
                | m <= 1 + 3*n = A (m-2*n-1) False
                | m <= 1 + 4*n = B (m-3*n-1) True
                | m <= 1 + 5*n = B (m-4*n-1) False
                | otherwise = error $ "Invalid state " ++ show m ++ "."

-- Decode an association from t:
decodeT' :: Int ->
           ((State,Action,State),Double) ->
           ((ParkingState,String,ParkingState),Double)
decodeT' c ((s,a,s'),p) = ((decodeState c s,
                            if (a == 1) then "Park" else "Move",
                            decodeState c s'),
                           p)

-- Show the condensed, human-readable transition function.
decodeT :: Int ->
           [((State,Action,State),Double)] ->
           [((ParkingState,String,ParkingState),Double)]
decodeT c = map (decodeT' c)

-- Display the states in which the policy tells the agent to keep looking.
whenToPass :: Int ->
              (UArray State Double, UArray State Action, [String]) ->
              [ParkingState]
whenToPass c (_,pi,_) = map (decodeState c) .
                        filter (> 1+c) .
                        map fst .
                        filter ((==) 0 . snd) .
                        assocs $
                        pi

-- And when to park.
whenToPark :: Int ->
              (UArray State Double, UArray State Action, [String]) ->
              [ParkingState]
whenToPark c (_,pi,_) = map (decodeState c) .
                        filter (> 1+c) .
                        map fst .
                        filter ((==) 1 . snd) .
                        assocs $
                        pi

parkingProblem :: Int ->
                  Double ->
                  Double ->
                  Double ->
                  Double ->
                  Double ->
                  Double ->
                  MarkovDP
{- Generates a MDP representing a parking problem with the given parameters:
 -
 - c = The number of parking spaces per row.
 - alpha = The probability that a handicap spot will be empty.  Should be be-
 -         low, but close to, 1.0.
 - beta = The penalty for parking in a handicap spot.  Should be a large neg-
 -        ative number.
 - delta = A parameter representing the rate at which a shopper loses utility
 -         for distance.  I represent the utility of parking as inversely 
 -         proportional to the square of the distance.  Should be a positive
 -         number.
 - iota = The rate of utility loss for driving around.  Should be a
 -        small positive number.
 - kappa = The penalty for crashing into another car.  Should be a larger
 -         negative number than beta.
 - lambda = A parameter determining the probability that a non-handicap spot
 -          will be empty.  I represent this as a Poisson-like distribution, as
 -          we can model the spaces as filling in from the closest on out, with
 -          cars entering as independent events and leaving after a certain
 -          period of time.  Therefore, this parameter should be a positive
 -          number, equal to the expected number of cars parked in the lot.
 -}
parkingProblem c alpha beta delta iota kappa lambda = let
  n = c*5 + 2
  r = listArray (0,n-1) ([0, kappa, beta] ++
                         [delta/(fromIntegral i) | i <- [2..c]] ++
                         (repeat (-iota)))
  t = accumArray (+) 0 ((0,0,0),(n-1,1,n-1)) (
-- Exit is a terminal state.
    [((0,0,0),1.0), ((0,1,0),1.0)] ++
-- Crashing or parking always obviates any further reward.
    [((i,0,0),1.0) | i <- [1..c+1]] ++ [((i,1,0),1.0) | i <- [1..c+1]] ++
-- Attempting to park in any occupied space always crashes the car.
    [((encodeState c (A i True),1,1),1.0) | i <- [1..c]] ++
    [((encodeState c (B i True),1,1),1.0) | i <- [1..c]] ++
-- However, parking in a free space succeeds.
    [((encodeState c (A i False),1,(encodeState c (P i))),1.0) | i <- [1..c]] ++
    [((encodeState c (B i False),1,(encodeState c (P i))),1.0) | i <- [1..c]] ++
-- An agent at A[1] can only move to B[1], which could be free or not.  The
-- probability that it is free is alpha.
    [((encodeState c (A 1 False),0,encodeState c (B 1 True)),1.0-alpha),
     ((encodeState c (A 1 True),0,encodeState c (B 1 True)),1.0-alpha),
     ((encodeState c (A 1 False),0,encodeState c (B 1 False)),alpha),
     ((encodeState c (A 1 True),0,encodeState c (B 1 False)),alpha)] ++
-- An agent at A[2] can only move to A[1], which again is free with probabil-
-- ity alpha.
    [((encodeState c (A 2 False),0,encodeState c (A 1 True)),1.0-alpha),
     ((encodeState c (A 2 True),0,encodeState c (A 1 True)),1.0-alpha),
     ((encodeState c (A 2 False),0,encodeState c (A 1 False)),alpha),
     ((encodeState c (A 2 True),0,encodeState c (A 1 False)),alpha)] ++
-- An agent at any other A[i] can only move to A[i-1].
    [((encodeState c (A i False),0,encodeState c (A (i-1) False)),quasiPoisson lambda (i-1)) | i <- [3..c]] ++
    [((encodeState c (A i True),0,encodeState c (A (i-1) False)),quasiPoisson lambda (i-1)) | i <- [3..c]] ++
    [((encodeState c (A i False),0,encodeState c (A (i-1) True)),1.0 - quasiPoisson lambda (i-1)) | i <- [3..c]] ++
    [((encodeState c (A i True),0,encodeState c (A (i-1) True)),1.0 - quasiPoisson lambda (i-1)) | i <- [3..c]] ++
-- An agent at B[i], where i < c, can only move to B[i+1].
    [((encodeState c (B i False),0,encodeState c (B (i+1) False)),quasiPoisson lambda (i+1)) | i <- [1..(c-1)]] ++
    [((encodeState c (B i True),0,encodeState c (B (i+1) False)),quasiPoisson lambda (i+1)) | i <- [1..(c-1)]] ++
    [((encodeState c (B i False),0,encodeState c (B (i+1) True)),1.0-quasiPoisson lambda (i+1)) | i <- [1..(c-1)]] ++
    [((encodeState c (B i True),0,encodeState c (B (i+1) True)),1.0-quasiPoisson lambda (i+1)) | i <- [1..(c-1)]] ++
-- Finally, an agent at B[c] can only move to A[c].
    [((encodeState c (B c False),0,encodeState c (A c True)),1.0-quasiPoisson lambda c),
     ((encodeState c (B c True),0,encodeState c (A c True)),1.0-quasiPoisson lambda c),
     ((encodeState c (B c False),0,encodeState c (A c False)),quasiPoisson lambda c),
     ((encodeState c (B c True),0,encodeState c (A c False)),quasiPoisson lambda c)]
       )
  in (n, 2, r, t)

poisson :: Double -> Int -> Double
-- The pmf of a Poisson distribution.
poisson lambda i = (exp (-lambda)) *
                   (lambda^i) /
                   (fromIntegral (factorial i))

poissonCMF :: Double -> Int -> Double
-- The memoized cmf of a Poisson distribution.
poissonCMF lambda = ((scanl1 (+) (map (poisson lambda) [0..])) !!)

quasiPoisson :: Double -> Int -> Double
{- Take into account that there are two rows to fill.  The parameter gamma
 - should be the expected number of parked cars; the parameter i should be
 - at least 2.
 -}
quasiPoisson lambda i = ((poissonCMF lambda (i*2-4)) + 
                         (poissonCMF lambda (i*2-3)))
                        / 2.0

factorial :: Int -> Integer
-- A memoized factorial function.
factorial = ((scanl (*) 1 [1..]) !!)

-- Test data:

-- The 4x3 world in fig. 17.3 of (Russell & Norvig 2010):

test4x3 :: MarkovDP
test4x3 = ( 12,	-- The 11 states in the example, plus a terminal state 0.
            4,	-- Left, right, up, down
            listArray (0,11)
                      [0,
                       -0.04, -0.04, -0.04, 1.0,
                       -0.04,        -0.04, -1.0,
                       -0.04, -0.04, -0.04, -0.04],
            accumArray (+) 0.0 ((0,0,0),(11,3,11)) [
 -- The synthetic terminal state:
 ((0,0,0),1.0),((0,1,0),1.0),((0,2,0),1.0),((0,3,0),1.0),
 -- Position (1,1):
 ((1,0,1),1.0),((1,1,2),1.0),((1,2,1),1.0),((1,3,5),1.0),
 -- Position (1,2):
 ((2,0,1),1.0),((2,1,3),1.0),((2,2,2),1.0),((2,3,2),1.0),
 -- Position (1,3):
 ((3,0,2),1.0),((3,1,4),1.0),((3,2,3),1.0),((3,3,6),1.0),
 -- Position (1,4):
 ((4,0,0),1.0),((4,1,0),1.0),((4,2,0),1.0),((4,3,0),1.0),
 -- Position (2,1):
 ((5,0,5),1.0),((5,1,5),1.0),((5,2,1),1.0),((5,3,8),1.0),
 -- Position (2,2) does not exist.
 -- Position (2,3):
 ((6,0,6),1.0),((6,1,7),1.0),((6,2,3),1.0),((6,3,10),1.0),
 -- Position (2,4):
 ((7,0,0),1.0),((7,1,0),1.0),((7,2,0),1.0),((7,3,0),1.0),
 -- Position (3,1):
 ((8,0,8),1.0),((8,1,9),1.0),((8,2,5),1.0),((8,3,8),1.0),
 -- Position (3,2):
 ((9,0,8),1.0),((9,1,10),1.0),((9,2,9),1.0),((9,3,9),1.0),
 -- Position (3,3):
 ((10,0,9),1.0),((10,1,11),1.0),((10,2,6),1.0),((10,3,10),1.0),
 -- Position (3,4):
 ((11,0,10),1.0),((11,1,11),1.0),((11,2,7),1.0),((11,3,11),1.0)
            ]
          )

-- The three-state world in problem 17.10 in (Russell & Norvig 2010):
test3 :: MarkovDP
test3 = ( 3, -- There are three states in the example.  I rename 3 to 0.
          2, -- and two actions, a and b.
          listArray (0,2) [0,-1,-2],
          accumArray (+) 0.0 ((0,0,0),(2,1,2)) [
 -- State 0 is a terminal state.
 ((0,0,0),1.0), ((0,1,0),1.0),
 -- In state 1, action a goes to state 2 (p=0.8) or state 1 (p=0.2)
 -- Action b goes to state 0 (p=0.1) or state 1 (p=0.9)
 ((1,0,2),0.8), ((1,0,1),0.2), ((1,1,0),0.1), ((1,1,1),0.9),
 -- In state 2, action a goes to state 1 (p=0.8) or state 2 (p=0.2)
 -- Action b goes to state 0 (p=0.1) or state 1 (p=0.9)
 ((2,0,1),0.8), ((2,0,2),0.2), ((2,1,0),0.1), ((2,1,2),0.9)
          ]
        )

-- The 101 x 3 world from exercise 17.8 in (Russell & Norvig 2010):
{- This example is useful because I’ve already solved the critical values of
 - gamma for it, and because it contains a larger number of states.
 -}
test101 :: MarkovDP
test101 = ( 204, -- The start state, an immediate reward state on top followed
                 -- by 100 penalties, an immediate penalty state on the bottom
                 -- followed by 100 rewards, and a synthetic terminal state.
            2,   -- Up or down at the start.  Both move the agent right any-
                 -- where else.
            listArray (0,203)
  ((0:50.0:(take 100 (repeat (-1.0))))++(-50.0:(take 100 (repeat 1.0)))++[0]),
            accumArray (+) 0.0 ((0,0,0),(203,1,203))
-- In the start state, up goes to state 1, down to state 102.
  ([((0,0,1),1.0), ((0,1,102),1.0)] ++
-- In states 1-100, any action moves the agent to the next consecutive state.
  [((i,0,i+1), 1.0) | i <- [1..100]] ++ [((i,1,i+1), 1.0) | i <- [1..100]] ++
-- State 101 always sends the agent to terminal state 203.
  [((101,0,203),1.0),((101,1,203),1.0)] ++
-- States 102-202 also send the agent to the next consecutive state:
  [((i,0,i+1), 1.0) | i <- [102..202]] ++
  [((i,1,i+1), 1.0) | i <- [102..202]] ++
-- Finally, state 203 is terminal:
  [((203,0,203),1.0),((203,1,203),1.0)])
          )
