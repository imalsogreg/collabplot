module Main where

import qualified Lucid.Svg           as L
import qualified Lucid.Svg.Element   as E
import qualified Lucid.Svg.Attribute as A


main :: IO ()
main = undefined

data TaurusWedgeSpec = TaurusWedgeSpec {
    tsX  :: Double
  , tsY  :: Double
  , tsR0 :: Double
  , tsR1 :: Double
  , tsT0 :: Double
  , tsT1 :: Double
  } deriving (Eq, Show)

s :: (RealFrac a, Show a) => a -> String
s = show . floor

taurusWedge :: TaurusSpec -> Svg ()
taurusWedge TaurusSpec{..} =
  let p0 = (s $ tsR0 * cos tsT0, s $ tsR1 * sin tsT0)
      p1 = (s $ tsR1 * cos tsT0, s $ tsR1 * sin tsT0)
      p2 = (s $ tsR1 * cos tsT1, s $ tsR1 * sin tsT1)
      p3 = (s $ tsR0 * cos tsT1, s $ tsR0 * sin tsT1)
      d  = unwords ["M", fst p0, snd p0
                   ,"L", fst p1, snd p1
                   ,"A", s tsR0, s tsR0, show tsTH0 ]
    taur0 = do
        path_ [d_ ""]
  
