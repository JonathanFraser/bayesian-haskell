import Control.Monad.Bayes.Sampler
import Control.Monad (liftM2)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.RMSMC
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.List
import Numeric.Log
import Data.Csv
import qualified Data.ByteString.Lazy as B

-- | A toss of a six-sided die.
die :: MonadSample m => m Int
die = uniformD [1..6]

-- | A sum of outcomes of n independent tosses of six-sided dice.
dice :: MonadSample m => Int -> m Int
dice 1 = die
dice n = liftM2 (+) die (dice (n-1))

-- | Toss of two dice where the output is greater than 4.
dice_hard :: MonadInfer m => m Int
dice_hard = do
  result <- dice 2
  condition (result > 4)
  return result

-- | Toss of two dice with an artificial soft constraint.
dice_soft :: MonadInfer m => m Int
dice_soft = do
  result <- dice 1
  score (1 / fromIntegral result)
  return result

var :: MonadSample m => m Double
var = uniform (0.0) (20.0)

vars :: MonadSample m => m [Double]
vars = sequence $ take 10 $ repeat var

diffeq :: MonadInfer m => m (Double,Double,Double,Double)
diffeq = do 
    r1 <- var
    r2 <- var 
    r3 <- var
    r4 <- var 
    let d2r1 = -2*r1 + r2
    let d2r2 = r1 -2*r2 + r3
    let d2r3 = r2 -2*r3 + r4 
    score $ Exp (-(1e3*(d2r1 - r1)**2))
    score $ Exp (-(1e3*(d2r2 - r2)**2))
    score $ Exp (-(1e3*(d2r3 - r3)**2))
    score $ Exp (-1e4*(r4-1)**2)
    return (r1,r2,r3,r4)

--prog = smcSystematic 3 10000
prog = rmsmc 5 1000 100

dierun :: Population (SamplerIO) (Double,Double,Double,Double)
dierun = prog (diffeq)

pop :: SamplerIO [((Double,Double,Double,Double),Double)]
pop = explicitPopulation dierun


main = do
  e <- sampleIO pop 
  let t = encode $ fmap fst e
  B.writeFile "test.csv" t