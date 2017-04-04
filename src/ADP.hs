{- ADP.hs		Code for CS 533, Mini-Project 2.
 -
 - Copyright © 2012 Loren B. Davis.  ALL RIGHTS RESERVED.  I hereby release
 - this code under the MIT License: (http://www.opensource.org/licenses/MIT).
 - Since this is an assigned exercise, remember your academic integrity!
 -}

module ADP where
import Prelude hiding (pi)
import Control.Monad
import Data.Array.Unboxed
import Data.List

import MDPSim
import MDP

{- The closure of a simulator and a world model; passing this to the learning
 - process hides the world model itself, while still allowing the agent to
 - call the simulator.  The arguments to a simulator are:
 -
 - policy:      The policy for the agent to follow.
 -
 - It returns a set of results for one run through the simulator.  The agent
 - does not get to choose the initial state.
 -}
type Simulator = Policy -> IO MDPresults

{- Given a MDP describing a parking problem, return a Simulator for that MDP.
 - One can then pass the Simulator to the learning agent, allowing it to run
 - a given policy and see the results, but not to see the details of the world
 - model or specify the initial state.
 -}
fromMDP :: MarkovDP -> Simulator
fromMDP mdp = fromMDP' (initialState mdp)
  where
    terminal = 0
    h        = 10000
    fromMDP' :: IO State -> Policy -> IO MDPresults
    fromMDP' s pi = do s' <- s
                       r  <- simulateMDP mdp pi s' terminal h
                       return r

{- A model-based reinforcement learning agent.  This agent starts with an
 - initial policy.  It then modifies that policy to be epsilon-greedy.  It 
 - simulates k runs of the epsilon-greedy policy through the world, using the
 - results to refine the model.  Finally, it determines the optimal policy for
 - its new model.
 -
 - The agent uses the results of each set of runs to estimate the reward func-
 - tion and the transition function.
 -
 - This is based on the ADP techniques in §21.2.2 of the (Russell & Norvig
 - 2010).
 -
 - Parameters:
 - n:   	The number of states
 - m:   	The number of actions
 - terminal:	The terminal state
 - pi:		An initial policy.
 - gamma:	The discount parameter for agent evaluation
 - sim:   	A simulator for the world
 - epsilon:	The probability that the agent will explore, not exploit
 - k:   	The number of runs to simulate
 - 
 -}
epsilonGreedyAgent :: Int ->
                      Int ->
                      Int ->
                      Policy -> 
                      Double ->
                      Simulator ->
                      Double ->
                      Int ->
                      IO Policy
epsilonGreedyAgent n m terminal policy gamma sim epsilon k = do
{- Generate k runs through the simulator, merge all the results from every
 - step of every run, and sort them by state, action taken and reward.  Calc-
 - ulate our initial model of the world based on these results.
 -}
    runs <- liftM concat $ replicateM k (sim pi)
-- Generate our initial model from the test runs: (|S|,|A|,r,t).
    let model = (n, m, rewardsFromRuns runs, transitionsFromRuns runs)
{-  Instrumentation: Print the model’s transition function in human-
 -  readable form, for debugging purposes.
 -}
--  putStrLn $ show $ decodeT 10 $ showT model
    return $! optimalPolicy model gamma
  where
    pi :: Policy
    pi = epsilonGreedyPolicy n m policy epsilon
    {- Given a list of results from the test runs, generates a reward
     - function covering every state reached on any run.  All states not
     - reached have reward 0 in this model.
     -}
    rewardsFromRuns :: MDPresults -> UArray State Double
    rewardsFromRuns = array (0, n-1) .
                      map ( \(s,_,u) -> (s,u) ) .
                      map head .
                      groupBy ( \(s,_,_) (s',_,_) -> s == s' ) .
                      sort
    {- Given an unsorted list of results from the test runs, generates a trans-
     - ition function whose estimate of the likelihood of reaching state s'
     - from state s and action a is the proportion of (s,a,_) triples followed
     - by (s',_,_) triples.  All actions in the terminal state and all (s,a)
     - never reached in the results are treated as leading to the terminal
     - state with p = 1.
     -}
    transitionsFromRuns :: MDPresults -> UArray (State,Action,State) Double
    transitionsFromRuns runs = listArray ((0,0,0),(n-1,m-1,n-1)) $
                                         map transitionP $
                                         [(s,a,s') | s  <- [0..n-1],
                                                     a  <- [0..m-1],
                                                     s' <- [0..n-1] ]
      where
        transitionP :: (State,Action,State) -> Double
        transitionP (s,a,s') | s == terminal && s' == terminal = 1.0
                             | s == terminal                   = 0.0
                             | [] == is && s' == terminal      = 1.0
                             | [] == is                        = 0.0
                             | otherwise                       =
                                 fromIntegral (length js) /
                                 fromIntegral (length is)
          where is :: [Int]
                is = findIndices ( \(t,b,_) -> t == s && b == a )
                                 runs
                js :: [(State,Action,Double)]
                js = filter ( \(s'',_,_) -> s' == s'' ) .
                     map (runs !!) .
                     map succ $
                     is

{- A new policy that follows the given policy with probability 1-epsilon,
 - and chooses an action with uniform probability otherwise.
 -}
epsilonGreedyPolicy :: Int -> Int -> Policy -> Double -> Policy
epsilonGreedyPolicy n m pi epsilon =
  listArray ((0,0),(n-1,m-1)) .
  map (\x -> (1.0-epsilon)*x + epsilon/fromIntegral m) $
  elems pi

