{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Swarm where

-- | Particle Swarm Optimization
--
-- Goals
-- 1. optimize with a Particle Swarm to find some optimal goal within a decently complex function
-- 2. attempt to parallelize each particle trajectory
-- 3. visualize

import Data.Maybe
import Data.UUID as UUID
import Data.UUID.V4 as UUID
-- import Control.Monad.IO.Class
import System.Random
-- import Data.Ord
import Data.List
import qualified Data.Map as Map

newtype Point = Point { unPoint :: (Float, Float) }
  deriving (Eq)

instance Show Point where
  show (Point (x,y)) = "(" <> show x <> "," <> show y <> ")"

type FitnessFunction = (Point -> Float)

newtype Velocity = Velocity { unVelocity :: (Float, Float) }
  deriving (Eq)

instance Show Velocity where
  show (Velocity (x,y)) = "V(" <> show x <> "," <> show y <> ")"

type Particles = Map.Map String Particle

data SwarmOptimization
  = SwarmOptimization
  { numOfParticles :: Int
  , fitness_function :: Point -> Float
  , goal :: GoalMetric
  , rangeX :: (Float, Float)
  , rangeY :: (Float, Float)
  }

data Particle
  = Particle
  { loc :: Point
  , velocity :: Velocity
  , best :: Maybe Point
  }
  deriving (Show, Eq)


orient :: GoalMetric -> (Point -> Float) -> [Particle] -> Particle
orient g f ps = case g of
  Minimize -> minimumBy goalOrient ps
  Maximize -> maximumBy goalOrient ps
  -- XXX
  _ -> minimumBy goalOrient ps
  where
    goalOrient (Particle { loc = p1}) (Particle { loc = p2 }) = compare (f p1) (f p2)

data GoalMetric
  = Minimize
  | Maximize
  | Explore
  -- ^ maybe can use this to do the opposite of optimization and spread the particles evenly across the function

globalBest :: GoalMetric -> (Point -> Float) -> Particles -> Particle
globalBest g f = orient g f . Map.elems

-- make n particles within an rx by ry bounded square
mkParticles :: Int -> (Float, Float) -> (Float, Float) -> IO Particles
mkParticles = go Map.empty
  where
    go m 0 _ _ = pure m
    go m n rx ry = do
      x <- randomRIO rx
      y <- randomRIO ry
      vs <- randomVelocity
      guid <- UUID.toString <$> UUID.nextRandom
      let new = Map.insert guid (mkParticle (x,y) vs) m
      go new (n-1) rx ry

    -- make a fresh particle from a point and initial velocity
    mkParticle p vs = Particle { loc = Point p, velocity = Velocity vs, best = Nothing }

    randomVelocity = do
      vx <- randomRIO (-1,1)
      vy <- randomRIO (-1,1)
      return (vx, vy)

data Hyperparameters
  = Hyperparameters
  { w :: Float
  , c1 :: Float
  , c2 :: Float
  }

-- 'w' is the inertia weight. The lower it is, the stronger the convergence of the particles
-- 'c1' and 'c2' are called the acceleration coefficients and control the personal and global respectively
-- this gives us the auto hyperparameters for each iteration
-- XXX possibly add r1 and r2 for stochastic personal and global acceleration
coefficients :: Int -> Int -> Hyperparameters
coefficients t' n' = do
  Hyperparameters w c1 c2
  where
    t = fromIntegral t'
    n = fromIntegral n'
    w = (0.4/n**2) * (t - n) ** 2 + 0.4
    c1 = -3 * t / n + 3.5
    c2 =  3 * t / n + 0.5

step :: SwarmOptimization -> Int -> IO Particles
step (SwarmOptimization {..}) n = do
  -- randomly place particles with random velocity between 0.1 and 1
  particles <- mkParticles numOfParticles rangeX rangeY
  return $ go n particles
  where
    go 0 p = p
    go n' p =
      -- identify closest particle globally
      let g = globalBest goal fitness_function p
          -- auto hyperparameters so we don't have to manually pick them
          coef = coefficients n' n
          -- move_particles and update bests
          newParticles = moveParticles g fitness_function coef p
       in go (n'-1) newParticles

moveParticles :: Particle -> FitnessFunction -> Hyperparameters -> Particles -> Particles
moveParticles p f coef = Map.map (update p)
  where
    update (Particle { loc = globalLoc }) personal@(Particle { loc }) =
      let newVel = getNewVelocity personal globalLoc coef
          newLoc = addP loc newVel

          best = if f newLoc < f loc then newLoc else loc
       in personal { loc = newLoc
                   , velocity = newVel
                   , best = Just best
                   }

    -- Update the particle's velocity: newVelocity ← w currentVelocity + c1 r1 (personal - current) + c2 r2 (global - current)
    getNewVelocity (Particle { loc, velocity, best}) global (Hyperparameters w c1 c2) =
      let prevBest = fromMaybe loc best
          -- personal
          -- c1 * r1 * (pbest - current)
          pv = scaleV c1 $ diffToVel prevBest loc

          -- global
          -- c2 * r2 * (global - current)
          gv = scaleV c2 $ diffToVel global loc
          -- w*v + pv + gv

       in (w `scaleV` velocity) `addV` pv `addV` gv


showParticles :: [Particle] -> IO ()
showParticles = mapM_ (putStr . show)

swarm :: Int -> IO [Particle]
swarm s = do
  p <- step pso s
  return $ Map.elems p
  where
    (rx, ry) = mkSquareRange 20 20
    pso = SwarmOptimization
      { numOfParticles = 30
      , goal = Minimize
      , rangeX = rx
      , rangeY = ry
      , fitness_function = schafferFunc
      }

sanityFunc :: Point -> Float
sanityFunc (Point (x, y)) = x **2 - y

schafferFunc :: Point -> Float
schafferFunc (Point (x, y)) = 0.5 + (n / d)
  where
    n = (sin (x ** 2.0 - y ** 2) ** 2) - 0.5
    d = (1 + 0.001 * (x**2 + y**2)) ** 2

-- sanity check for globalBest
testBest = do
  let (rx, ry) = mkSquareRange 5 5
  ps <- mkParticles 30 rx ry
  let x = globalBest Maximize schafferFunc ps
  print x
  return x

------------------- Helpers ------------------------

-- helper function to facilitate restricting the range of search to square area
mkSquareRange :: Int -> Int -> ((Float, Float), (Float, Float))
mkSquareRange x y = ((0.0 - fromIntegral x, 0.0 + fromIntegral x), (0.0 - fromIntegral y, 0.0 + fromIntegral y))

distance (Point (x1 , y1)) (Point (x2 , y2)) = sqrt (x'**2 + y'**2)
  where
    x' = x1 - x2
    y' = y1 - y2

addP (Point (a,b)) (Velocity (c,d)) = Point (a+c, b+d)
addV (Velocity (a,b)) (Velocity (c,d)) = Velocity (a+c, b+d)
diffToVel (Point (a,b)) (Point (c,d)) = Velocity (a-c, b-d)
scaleV n (Velocity (a,b)) = Velocity (a*n, b*n)
