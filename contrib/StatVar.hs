{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


-----------------------------------------------------------------------------
-- |
-- Module      :
-- Copyright   :  (C) 2012 Soostone Inc
-- License     :  All Rights Reserved
--
-- Maintainer  :  Ozgun Ataman <oz@soostone.com>
-- Stability   :  experimental
--
-- Defines a standard set of statistics computed for collections of
-- data. For theory around combining cells monoidally, see:
--
-- http://stackoverflow.com/questions/7753002/adding-combining-standard-deviations
-- http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#On-line_algorithm
----------------------------------------------------------------------------


module Data.StatVar
    ( StatVar (..)
    , svStdDev
    , svVariance

    -- * Operating on StatVars
    , newStatVar
    , updateSV
    , updateSVRemExt
    , trimExt
    , isExtremeVal
    , listToStatVar
    , expandStatVar
    ) where

-------------------------------------------------------------------------------
import           Control.Applicative
import           Control.DeepSeq
import           Data.Aeson
import           Data.DeriveTH
import           Data.Generics
import           Data.List           (foldl')
import           Data.Monoid
import           Data.SafeCopy
import           Data.Serialize
import           Data.Text           (Text)
import qualified Data.Vector         as V
import           Statistics.Sample
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- | A set of standard stats over a collection of numbers
data StatVar = StatVar {
      svMax   :: !Double
    , svMin   :: !Double
    , svSum   :: !Double
    , svCount :: !Int
    , svMean  :: !Double
    , svSOS   :: !Double
    -- ^ sum of squares of differences from the (current) mean. Needed
    -- for Welford's method of variance estimation.
    } deriving (Eq,Show,Read,Ord,Typeable,Data)


-------------------------------------------------------------------------------
$(deriveSafeCopy 0 'base ''StatVar)
$(derives [makeSerialize] [''StatVar])
$(derives [makeNFData] [''StatVar])


-------------------------------------------------------------------------------
instance FromJSON StatVar where
    parseJSON (Object v) = StatVar
      <$> v .: "mx"
      <*> v .: "mn"
      <*> v .: "sm"
      <*> v .: "cnt"
      <*> v .: "avg"
      <*> v .: "sos"


-------------------------------------------------------------------------------
instance ToJSON StatVar where
    toJSON StatVar{..} = object
      [ "mx" .= svMax
      , "mn" .= svMin
      , "sm" .= svSum
      , "cnt" .= svCount
      , "avg" .= svMean
      , "sos" .= svSOS
      ]


-------------------------------------------------------------------------------
-- | Extract components out of a 'StatVar' for convenience in human
-- readable contexts.
expandStatVar :: StatVar -> [(String, Double)]
expandStatVar st =
    [ ("max", svMax st)
    , ("min", svMin st)
    , ("mean", svMean st)
    , ("count", fromIntegral $ svCount st)
    , ("sum", svSum st)
    , ("stdDev", svStdDev st)
    , ("var", svVariance st)
    ]


-------------------------------------------------------------------------------
-- | Convert a list of numerical data into a 'StatVar'
listToStatVar [] = Nothing
listToStatVar (x:xs) = Just $ foldl' updateSV (newStatVar x) xs


-------------------------------------------------------------------------------
newStatVar :: Double -> StatVar
newStatVar x = StatVar { svMax = x
                       , svMin = x
                       , svSum = x
                       , svCount = 1
                       , svMean = x
                       , svSOS = 0 }


-------------------------------------------------------------------------------
-- | Update the stats variable with a new datapoint
updateSV :: StatVar -> Double -> StatVar
updateSV StatVar{..} x =
    StatVar { svMax = mx
            , svMin = mn
            , svSum = sm
            , svCount = cnt
            , svMean = mean
            , svSOS = sos }
    where
      mx = max svMax x
      mn = min svMin x
      sm = svSum + x
      cnt = svCount + 1
      delta = x - svMean
      mean = sm / fromIntegral cnt
      sos = svSOS + delta * (x - mean)


-- | Add new value while trimming extreme cases.
--
-- Require that at least n samples have been collected so far. If new
-- value is outside of the 3-sigma range, it will be replaced by the
-- mean value so far observed.
updateSVRemExt n sv x = updateSV sv $ trimExt n sv x


-- | Maybe trim the value based on given StatVar with historical
-- stats.
trimExt :: Int
        -- ^ Min # of samples required for trim
        -> StatVar
        -- ^ Historical stats
        -> Double
        -- ^ New observed value
        -> Double
        -- ^ (Possibly) trimmed value
trimExt n sv x = if isExtremeVal n sv x
                 then svMean sv
                 else x


-- | Is the value extreme (+- 3 sigmas)?
isExtremeVal :: Int -> StatVar -> Double -> Bool
isExtremeVal n sv x = 
    svCount sv >= n && (x > ulimit || x < llimit) 
    where
      sdev = svStdDev sv
      mean = svMean sv
      ulimit = mean + 3 * sdev
      llimit = mean - 3 * sdev


-------------------------------------------------------------------------------
svStdDev :: StatVar -> Double
svStdDev = sqrt . svVariance


-------------------------------------------------------------------------------
svVariance :: StatVar -> Double
svVariance st
    | svCount st >= 2 = svSOS st / (fromIntegral $ svCount st - 1)
    | otherwise = 0


-------------------------------------------------------------------------------
instance Monoid StatVar where
    mempty = StatVar 0 0 0 0 0 0

    mappend a b = StatVar {
                        svMax = max (svMax a) (svMax b)
                      , svMin = min (svMin a) (svMin b)
                      , svSum = sum'
                      , svCount = cnt
                      , svMean = mean'
                      , svSOS = sos
                      }
        where
          mean' = sum' / cnt'
          cnt = svCount a + svCount b
          cnt' = fromIntegral cnt
          sum' = svSum a + svSum b

          -- variance calculation; back-compute sos from combined variance
          sos = s1 + s2
          var = (s1 + s2) / (cnt' - 1)
          s1 = (fromIntegral $ svCount a - 1) * svVariance a +
               (fromIntegral $ svCount b - 1) * svVariance b
          s2 = fromIntegral (svCount a) * ((svMean a - mean') ** 2) +
               fromIntegral (svCount b) * ((svMean b - mean') ** 2)




                                 -------------
                                 -- Testing --
                                 -------------




va = V.fromList [3.0,7,2,4,5,7,8]

vb = V.fromList [1.0,6,9,5,3,11,24]

v = V.concat [va, vb]


varVa = varianceUnbiased va
varVb = varianceUnbiased vb
varV = varianceUnbiased v

stA = mkStat va
stB = mkStat vb


-------------------------------------------------------------------------------
-- | stAll should be equal to stComb if the method to merge variances is right.
stAll = mkStat v

-------------------------------------------------------------------------------
stComb = stA `mappend` stB


-------------------------------------------------------------------------------
mkStat va = StatVar mx mn sm (floor cnt) avg sos
    where
      sos = var * (cnt - 1)
      [mx, mn, sm, cnt, avg, var] = map ($ va)
          [V.maximum, V.minimum, V.sum, fromIntegral . V.length, mean, varianceUnbiased]


buildSV = foldl updateSV (newStatVar 175) (replicate 32 133)
