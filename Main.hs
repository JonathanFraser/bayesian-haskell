import Control.Monad.Bayes.Sampler
import Control.Monad (liftM2)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Inference.SMC
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Data.List



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

dice_combo :: MonadInfer m => m Int
dice_combo = do 
    r1 <- die
    r2 <- die
    condition (r1 > 3)
    score (1/ fromIntegral r2)
    return (r1-r2)

prog = smcSystematic 10 10000

dierun :: Population (SamplerIO) Int
dierun = prog (dice_combo)

pop :: SamplerIO [(Int,Double)]
pop = explicitPopulation dierun


accum :: (Eq a,Num b) => [(a,b)] -> [(a,b)]
accum [] = []
accum ((v,r):xs) = (v,r+s):(accum rem) where 
                  (l,rem) = partition (\(v2,r2) -> v2 == v) xs
                  s = sum $ fmap snd l

r :: [(Double,Double)] -> IO ()
r sig = toFile def "hist.png" $ do
  layout_title .= "Dice Histogram"
  plot (line "Value" [sig])

main = do
  e <- sampleIO pop 
  let histdat = fmap (\(k,v) -> (fromIntegral k, v)) $ accum e
  let sorthistdat = sortBy (\(a,_) (b,_) -> compare a b) histdat
  r sorthistdat
  