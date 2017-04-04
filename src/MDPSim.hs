{- MDPSim.hs        Code for CS 533, Mini-Project 2.
 -
 - Copyright © 2012 Loren B. Davis.  ALL RIGHTS RESERVED.  I hereby release
 - this code under the MIT License: (http://www.opensource.org/licenses/MIT).
 - Since this is an assigned exercise, remember your academic integrity!
 -}

{- In order to run this, you might need to install Control.Monad.Random:
 - 
 - cabal install monadrandom
 -
 - To run in the REPL:      ghci MDPSim.hs
 - (Now renamed so that GHCI can find the right object files.)
 -}

{- Note: Profiled to see which strictness annotations actually helped perfor-
 - mance.  The file now requires -XBangPatterns to compile in GHC.
 -}

module MDPSim where
import Control.Monad (liftM, replicateM)
import Control.Monad.Random (Rand, RandomGen, evalRandIO, getRandom,
                             getRandomR, getRandoms)
import Data.Array.Unboxed (UArray, (!), array, assocs, bounds)
import Data.List (findIndex, findIndices)
import Prelude hiding (pi)

import MDP

{- Unlike in project 1, policies are not necessarily deterministic.  That is,
 - a policy in part 2 maps (s,a) pairs to the probability that the agent will
 - choose action a in state s.
 -}
type Policy = UArray (State, Action) Double

{- Generate an infinite list of (pseudo-)random numbers, using (And changing
 - the state of!) the global PRNG.  Because of this side-effect, it must be
 - wrapped in an IO monad.
 -}
randvars :: IO [Double]
randvars = do
    result <- evalRandIO unirand
    return result
  where
    unirand :: (RandomGen g) => Rand g [Double]
    unirand = getRandoms

{- Given a vector representing a probability mass function p, whose cumulat-
 - ive mass function is P, and a number y in [0,1), finds the smallest x such
 - that P(x) <= y.
 -
 - For example, if p is [0.5,0.5] (which could represent a 50-50 chance of
 - transitioning to state 0 or state 1) and y is 0.62, returns 1.  For the
 - same p and y = 0.48, returns 0.
 -}
aleaIactaEst :: [Double] -> Double -> Int
aleaIactaEst pmf !y = case findIndex (> y) cmf of
                    Just x  -> x
                    Nothing -> (error $
                               "Either the vector " ++ (show pmf) ++
                               " does not sum to 1 or else " ++
                               (show y) ++ " is not in the range [0,1)." )
  where cmf = scanl1 (+) pmf

{- Represents the outcome of a run through the MDP simulator.  It consists of
 - a list of tuples, one per time step, whose elements represent the state
 - the agent reached, the next action it took, and the (discounted) reward it
 - received.
 -}
type MDPresults = [(State, Action, Double)]

{- Converts a MDPresults lists into human-readable form. -}
decodeResults :: Int -> MDPresults -> [(ParkingState, String)]
decodeResults rows results = map fromTriple $ results
  where
    fromTriple (s,a,_) = ((decodeState rows s), (fromAction a))
    fromAction 0 = "Drive"
    fromAction 1 = "Park"
    fromAction a = error "Invalid action " ++ (show a) ++ "."

{- Monadic version of the above. -}
decodeResults' :: Int -> IO MDPresults -> IO [(ParkingState, String)]
decodeResults' rows = liftM (decodeResults rows)

{- Runs an agent through a simulation of a Markov decision process.
 -
 - Parameters:
 -
 - mdp:     A description of the MDP
 - policy:      The policy for the agent to follow.
 - initial: The initial state
 - terminal:    The terminal state (assumed unique, WLOG)
 - k:       The time horizon.
 -
 - Returns a MDPresults list representing the states reached, actions taken
 - and discounted rewards at each time step.
 -
 - This is a wrapper for the simulateMDP' function, with a cleaner interface.
 - Because it modifies the state of the global random number generator, it
 - must be written as monadic code; the impure code that generates the list
 - of random numbers is, however, isolated from the pure functional code that
 - simulates the MDP given the list of random numbers, and even from the
 - pure, mondadic code that generates the next random number in a sequen-
 - ce given a seed.
 -
 - This interface requires a finite time horizon so that, even if the agent
 - never reaches a terminal state (e.g., its policy is to drive around for-
 - ever), the computation will terminate with a meaningful approximation of
 - the total utility.  See the lecture notes for an upper bound on k given an
 - error tolerance of epsilon.  For any reasonable values of gamma and epsil-
 - on, k should be managably low.
 -}
simulateMDP :: MarkovDP ->
               Policy ->
               State ->
               State ->
               Int ->
               IO MDPresults
simulateMDP !(n,m,r,t) !policy !initial !terminal !horizon = do
    randList <- randvars
    return (simulateMDP' initial horizon randList [])
  where
    simulateMDP' :: State ->
                    Int ->
                    [Double] ->
                    MDPresults ->
                    MDPresults
{- Performs the dirty (yet functionally pure) work for the above calculation.
 -
 - Parameters:
 -
 - s:   The current state of the agent
 - k:       The remaining time horizon.
 - randList:    An infinite list of (pseudo-)random variables ~ uniform(0,1).
 - before:      An accumulating list of the results from the previous steps.
 -}
    simulateMDP' !s !k (!x0:(!x1):xs) before
-- One base case is where the time horizon has expired:
             | k == 0       = before ++ [(s,a,u)]
-- Another is where the agent is in the terminal state:
             | s == terminal    = before ++ [(s,a,u)]
-- Otherwise, we have the recursive case:
             | otherwise    =  ( simulateMDP'
                                         s'
                                         (k-1)
                                         xs ) $!
                                         before ++ [(s,a,u)]
{- If the last argument is non-strict, we get a very esoteric bug where the
 - list is in reverse order.  I have no idea why.  Did I get too cute with the
 - optimizer?  In any case, this fixes it.
 -}
     where
-- The next action to take 
      a = ( aleaIactaEst (pi_s s) ) x0
-- The discounted utility of being in the current state at this time step.
      u = r!s
-- The next state we end up in:
      s' = ( aleaIactaEst (t_s s a) ) x1
{- This can’t happen, because randList is infinite, but I’ll throw this in
 - just to suppress a GHC warning message.
 -}
    simulateMDP' _ _ _ _ =
      error "An infinite list ran out of elements."
{- A column vector whose ith entry represents the probability that the agent
 - will select action a in state s.
 -}
    pi_s :: State -> [Double]
    pi_s !s = [policy ! (s,i) | i <- [0..m-1]]
{- A column vector whose ith entry represents the probability of reaching
 - state i by taking action a in state s.
 -}
    t_s :: State -> Action -> [Double]
    t_s !s !a = [t ! (s,a,i) | i <- [0..n-1]]

{- As simulateMDP, but the MDP must be deterministic.  This restriction
 - allows us to completely eliminate random number generation, and therefore
 - implement the simulator as a pure function.
 -}
simulateDMDP :: MarkovDP ->
                Double ->
                Policy ->
                State ->
                State ->
                Int ->
                MDPresults
simulateDMDP mdp gamma policy initial terminal k =
  simulateDMDP' mdp gamma policy initial terminal k 1.0

{- Same as simulateMDP' above, but does not use random numbers, because
 - the MDP in this case is assumed to be deterministic.
 -}
simulateDMDP' :: MarkovDP ->
                 Double ->
                 Policy ->
                 State ->
                 State ->
                 Int ->
                 Double ->
                 MDPresults
simulateDMDP' (n,m,r,t) gamma policy s terminal k discount
-- One base case is where the time horizon has expired:
             | k == 0       = [(s,a,u)]
-- Another is where the agent is in the terminal state:
             | s == terminal    = [(s,a,u)]
-- Otherwise, we have the recursive case:
             | otherwise    = ((s,a,u):etc)
  where
-- The next action to take 
    a = case findIndices (== 1.0) pi_s of
-- A single entry in the transition function has p = 1, as required:
         [i] -> i
-- No entry has p = 1:
         [] -> error "Not a deterministic policy."
-- Multiple entries have p = 1(!):
         _ -> error "The policy chooses multiple actions."
-- The discounted utility of being in the current state at this time step.
    u = discount * (r ! s)
-- The next state we end up in:
    s' = case findIndices (== 1.0) t_s of
-- A single entry in the transition function has p = 1, as required:
         [i] -> i
-- No entry has p = 1:
         [] -> error "Not a deterministic MDP."
-- Multiple entries have p = 1(!):
         _ -> error "The transition function is invalid."
{- A column vector whose ith entry represents the probability that the agent
 - will select action a in state s.
 -}
    pi_s = [policy ! (s,i) | i <- [0..m-1]]
{- A column vector whose ith entry represents the probability of reaching
 - state i by taking action a in state s.
 -}
    t_s = seq a [t ! (s,a,i) | i <- [0..n-1]]
    etc = simulateDMDP' (n,m,r,t)
                        gamma
                        policy
                        s'
                        terminal
                        (k-1)
                        (gamma*discount)

{- Converts a vector of deterministic actions, such as calculated for the
 - optimal policies from policyIterate, into a Policy with one entry per
 - column set to 1 and all others to 0.  We must also specify m = |A|.
 -}
fromDPolicy :: UArray State Action -> Action -> Policy
fromDPolicy u m = let (a,b) = bounds u
                      assocList = [((i, u!i), 1.0) | i <- [a..b]]
                  in array ((a,0), (b,m-1)) assocList

{- Given a MDP description and a discount factor, determine the optimal policy
 - for that MDP in a form suitable for passing to the simulator.
 -
 - Calls MDP.policyIterate.
 -}
optimalPolicy :: MarkovDP -> Double -> Policy
optimalPolicy mdp gamma = let (_,m,_,_) = mdp
                              (_,u,_) = policyIterate mdp gamma
                          in fromDPolicy u m

{- Finds the total utility of the results of a given run.
 -}
totalUtility :: MDPresults -> Double -> Double
totalUtility r gamma = sum
  [ (gamma^i) * ( (\(_,_,u) -> u) (r!!i) ) | i <- [0..((length r) - 1)] ]

{- As above, but wrapped in an IO monad so that it works with non-
 - deterministic MDPs.
 -}
totalUtility' :: IO MDPresults -> Double -> IO Double
totalUtility' r gamma = do
  r' <- r
  return $! totalUtility r' gamma

{- Similar to showT in part 1, display only the nonzero elements of the
 - sparse matrix pi.
 -}
showPi :: Policy -> [((State,Action),Double)]
showPi policy = filter ((> 0.0) . snd) . assocs $ policy

-- The calculated optimal policy for the 4x3 microworld
policy4x3 :: Policy
policy4x3 = optimalPolicy test4x3 1.0

{- This runs the simulator on the 4x3 microworld from project 1, from a given
 - initial state, using our computed optimal policy.  We verify that the
 - agent does in fact reach the goal state in a minimal number of steps, and
 - also that the total utility of the run is equal to 1 minus 0.04 per time
 - step.
 -}
run4x3 :: State -> IO MDPresults
run4x3 s = simulateMDP test4x3 policy4x3 s 0 1000

{---------------------------------------------------------------------------
 - Everything below this box relates to the parking problem specifically.  -
 ---------------------------------------------------------------------------}

{- Translates a policy into the list of states in which that policy parks.
 -}
decodePolicy :: Int -> Policy -> [ParkingState]
decodePolicy rows pi = map (decodeState rows) .
                       map fst .
                       map fst .
                       filter (\((_,a),p) -> a == 1 && p == 1.0 ) .
                       assocs $
                       pi

{- Given a number of rows and a MDP representing a parking problem with the
 - same number of rows, randomly select an initial state as follows: select a
 - parking space with uniform probability, then simulate driving past it.
 - This changes the state of the system PRNG, and therefore requires the IO
 - monad.
 -
 - This turned out to be harder than the general-case MDP simulator.
 -}
initialState :: MarkovDP -> IO State
initialState !(n,_,_,t) = do
                           atA <- evalRandIO d2
                           row <- evalRandIO dn
                           x <- evalRandIO unirand
                           return $! (driveFrom (toState atA row) x)
                         where
                           rows = div (n-2) 5
                           d2 :: (RandomGen g) => Rand g Bool
                           d2 = getRandom
                           dn :: (RandomGen g) => Rand g Int
                           dn = getRandomR (1,rows)
                           unirand :: (RandomGen g) => Rand g Double
                           unirand = getRandom
                           toState :: Bool -> Int -> State
                           toState b i = case b of
                             True  -> encodeState rows (A i True)
                             False -> encodeState rows (B i True)
                           driveFrom :: State -> Double -> State
                           driveFrom s' x' = aleaIactaEst (t_s' s') $! x'
                           t_s' :: Int -> [Double]
                           t_s' s' = [t ! (s',0,i) | i <- [0..n-1]]

{- For a given MDP, gamma and policy, simulates the result of following
 - that policy from an initial state of s.  Hence, an agent can simulate
 - the result of taking any action by setting the probability of that action
 - to 1 and all other actions to 0 in state s.  Note that the agent should
 - not be able to see the first three arguments, but only the black-boxed clo-
 - sure of type Bandit.
 -
 - The agent does not see the results of each of its choices, but only the
 - cumulative reward of its policy.
 -
 - The permutation of the parameters is to make it possible to encapsulate
 - world knowledge into the bandit, hiding it from the agent.
 -
 - Parameters:
 -   mdp
 -   gamma
 -   terminal
 -   initial
 -   policy
 -
 - Returns:
 -   Total utility of one run through the simulation with those parameters.
 -}
bandit :: MarkovDP -> Double -> State -> State -> Policy -> IO Double
bandit mdp gamma terminal initial policy = do
    toReturn <- totalUtility' (simulateMDP mdp policy initial terminal k) gamma
    return $! toReturn
  where
    k = 1000

{- As above, but monadic, to simplify evaluatePolicy. -}
bandit' :: MarkovDP -> Double -> State -> IO State -> Policy -> IO Double
bandit' mdp gamma terminal initial policy = do
  s <- initial
  result <- bandit mdp gamma terminal s policy
  return result

{- The type of a simulator that knows more about the world than the agent
 - does; in particular, the precise values of the reward and transition func-
 - tions.  The agent can choose its policy and can simulate starting in any
 - state.
 -
 - The closure of this and the initial state would yield a bandit that can
 - only simulate starting from the state it’s in.  However, limiting the agent
 - not to remember the bandits that allowed it to simulate starting from any
 - state it’s been in seems highly artificial in a functional language.
 -}
type Bandit = Double -> State -> State -> Policy -> IO Double

{- Generates a policy that, in any state of a parking problem, parks with prob-
 - ability p and drives with probability p-1.
 -}
randomPolicy :: Int -> Double -> Policy
randomPolicy rows p = array ((0,0), (n-1,m-1))
                            ( [((i,0), 1.0-p) | i <- [0..n-1]] ++
                              [((i,1), p) | i <- [0..n-1]] )
                    where
                      m = 2
                      n = 5*rows + 2

{- Generates a policy that, if the current state represents an open space,
 - parks with probability p, and otherwise always drives.
 -}
neverCrashPolicy :: Int -> Double -> Policy
neverCrashPolicy rows p = array ((0,0), (n-1,m-1)) assocList
                     where
                       m = 2
                       n = 5*rows + 2
                       q = map pPark $ map (decodeState rows) [0..n-1]
                       assocList = [((i,1), q!!i) | i <- [0..n-1]] ++
                                   [((i,0), 1.0 - (q!!i)) | i <- [0..n-1]]
                       pPark :: ParkingState -> Double
                       pPark (A _ False) = p
                       pPark (B _ False) = p
                       pPark _ = 0

{- This policy makes the following, slight improvements:
 -
 - * Never parks in a handicapped space
 - * Always parks in the closest non-handicapped space, if open
 - * Parks in row i > 2, if open, with probability p/i
 -}
lessBadPolicy :: Int -> Double -> Policy
lessBadPolicy rows p = array ((0,0), (n-1,m-1)) assocList
                     where
                       m = 2
                       n = 5*rows + 2
                       q = map pPark $ map (decodeState rows) [0..n-1]
                       assocList = [((i,1), q!!i) | i <- [0..n-1]] ++
                                   [((i,0), 1.0 - (q!!i)) | i <- [0..n-1]]
                       pPark :: ParkingState -> Double
                       pPark (A 1 _) = 0
                       pPark (B 1 _) = 0
                       pPark (A 2 False) = 1
                       pPark (B 2 False) = 1
                       pPark (A i False) = p/(fromIntegral i)
                       pPark (B i False) = p/(fromIntegral i)
                       pPark _ = 0

{- Given a MDP encoding of the problem, discount factor, policy, and
 - number of iterations, return the average cumulative reward of the policy
 - over that number of iterations.
 -}
evaluateParkingPolicy :: MarkovDP -> Double -> Policy -> Int -> IO Double
evaluateParkingPolicy !mdp !gamma !policy !k = do
  utilities <- replicateM k ( bandit' mdp gamma 0 (initialState mdp) policy )
  return ((sum utilities) / fromIntegral k)

mdp4spaces :: MarkovDP
mdp4spaces = parkingProblem 2 0.9 (-10) 1 0.01 (-100) 2

pi4spaces :: Policy
pi4spaces = optimalPolicy mdp4spaces 0.98

mdp10spaces :: MarkovDP
mdp10spaces = parkingProblem 10 0.9 (-10) 1 0.01 (-100) 6

pi10spaces :: Policy
pi10spaces = optimalPolicy mdp10spaces 0.98
