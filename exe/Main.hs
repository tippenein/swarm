module Main where

import qualified Swarm as Swarm

main :: IO()
main =
  Swarm.showParticles =<< Swarm.swarm 50
