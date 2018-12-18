import Control.Monad.Bayes.Sampler
import Control.Monad (liftM2)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Sequential
import Control.Monad.Bayes.Weighted
import qualified Control.Monad.Bayes.Weighted as W
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Inference.RMSMC
import Control.Monad.Bayes.Inference.PMMH
import Control.Monad.Bayes.Traced
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.List
import Numeric.Log
import Data.Csv
import qualified Data.ByteString.Lazy as B

var :: MonadSample m => m Double
var = normal 0 5 

prior :: MonadSample m => m [Double]
prior = sequence $ take 9 $ repeat var


--applies the stencil (1,-2,1) with the provided boundries
d2 ::Monad m => Double -> Double -> m [Double] -> m [Double]
d2 s e lst = do
        ext <- lst 
        return $ zipWith3 (\l m r -> (l+r-2*m)) (s:(init ext)) ext ((tail ext)++[e]) 



diffeq :: MonadInfer m => m [Double]
diffeq = do 
    r1 <- var
    r2 <- var 
    r3 <- var
    r4 <- var
    r5 <- var
    r6 <- var 
    r7 <- var
    r8 <- var
    r9 <- var

    let d2r1 = (0  -2*r1 + r2)*100
    let d2r2 = (r1 -2*r2 + r3)*100
    let d2r3 = (r2 -2*r3 + r4)*100
    let d2r4 = (r3 -2*r4 + r5)*100
    let d2r5 = (r4 -2*r5 + r6)*100
    let d2r6 = (r5 -2*r6 + r7)*100
    let d2r7 = (r6 -2*r7 + r8)*100
    let d2r8 = (r7 -2*r8 + r9)*100
    let d2r9 = (r8 -2*r9 + 1 )*100

    score . Exp $ -1*(d2r1-r1)**2
    score . Exp $ -1*(d2r2-r2)**2
    score . Exp $ -1*(d2r3-r3)**2
    score . Exp $ -1*(d2r4-r4)**2
    score . Exp $ -1*(d2r5-r5)**2
    score . Exp $ -1*(d2r6-r6)**2
    score . Exp $ -1*(d2r7-r7)**2
    score . Exp $ -1*(d2r8-r8)**2
    score . Exp $ -1*(d2r9-r9)**2

    return [r1,r2,r3,r4,r5,r6,r7,r8,r9]



--prog = smcSystematic 1 1000000
prog = rmsmc 1000 100 10

dierun ::Population (SamplerIO) [Double]
dierun = prog diffeq

pop :: SamplerIO [([Double],Log Double)]
pop =  runPopulation dierun


main = do
  print "sampling population"
  (l,v) <- sampleIO . runWeighted $ diffeq
  print (l,ln v)
  (l2,v2) <- sampleIO . runWeighted $ var 
  print (l2,ln v2)
  --print "extracting values"
  --let coords = fmap fst e
  --let probs = fmap (ln.snd) e
  --print "writing file"
  --let t = encode $ zipWith (:) probs coords
  --B.writeFile "test.csv" $ encode e