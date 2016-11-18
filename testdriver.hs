module Main where
import MDPSim

main :: IO ()
main = do
         result <-
           evaluateParkingPolicy mdp10spaces 0.98 (lessBadPolicy 10 0.6) 10000
         putStrLn $ show result
         return ()

