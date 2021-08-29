{-# LANGUAGE RecordWildCards #-}

{-
    For:
        Seeing patterns in data
-}

module Patterns where

import qualified Data.ByteString as B
import qualified Data.Vector as V

import System.IO

import Collector (BookLog (..), TickerMovement (..), TradeLog (..))
import Files (getFilesGrouped, readFile')
{-
    * if values is high or low
    * check outliers by getting mean then std dev
    * see if outliers are an obvious indiciation for uptrend or downtrend
-}
--
data Data = Data {
    -- Ticker Increments, op :: opening price, hp :: high price, lp :: low price, vol :: volume
    lp :: Double,
    hp :: Double,
    op :: Double,
    vol :: Double,
    -- Book Log Increments, awpc :: average weighted price compare, prc :: price range compare
    awpc :: Double,
    prc :: Double,
    -- Trade Log Increments, wp :: weighted price, mtp :: market trade %, bp :: buy %
    wp :: Double,
    mtp :: Double,
    bp :: Double,
    --
    mtp'ni :: Double,
    bp'ni :: Double
} deriving (Show)

data OpeningPricePatterns = OpeningPricePatterns {opp'ut :: [Uptrend], opp'dt :: [Downtrend]}
data Uptrend =              Uptrend {uawpc :: Double, uprc :: Double, uwp :: Double, umtp :: Double, ubp :: Double, ubp'ni :: Double, umtp'ni :: Double} deriving (Show)
data Downtrend =            Downtrend {dawpc :: Double, dprc :: Double, dwp :: Double, dmtp :: Double, dbp :: Double, dbp'ni :: Double, dmtp'ni :: Double} deriving (Show)
data Pattern =              Pattern {ut :: Uptrend, dt :: Downtrend} deriving (Show)

addUT Uptrend {..} Uptrend {uawpc = uawpc', uprc = uprc', uwp = uwp', umtp = umtp', ubp = ubp', ubp'ni = ubp'ni', umtp'ni = umtp'ni'} = Uptrend {
    uawpc = uawpc+uawpc',
    uprc = uprc+uprc',
    uwp = uwp+uwp',
    umtp = umtp+umtp',
    ubp = ubp+ubp',
    ubp'ni = ubp'ni+ubp'ni',
    umtp'ni = umtp'ni+umtp'ni'}

addDT Downtrend {..} Downtrend {dawpc = dawpc', dprc = dprc', dwp = dwp', dmtp = dmtp', dbp = dbp', dbp'ni = dbp'ni', dmtp'ni = dmtp'ni'} = Downtrend {
    dawpc = dawpc+dawpc',
    dprc = dprc+dprc',
    dwp = dwp+dwp',
    dmtp = dmtp+dmtp',
    dbp = dbp+dbp',
    dbp'ni = dbp'ni+dbp'ni',
    dmtp'ni = dmtp'ni+dmtp'ni'}

divUT Uptrend {..} x = Uptrend {uawpc = uawpc/x, uprc = uprc/x, uwp = uwp/x, umtp = umtp/x, ubp = ubp/x, ubp'ni = ubp'ni/x, umtp'ni = umtp'ni/x}
divDT Downtrend {..} x = Downtrend {dawpc = dawpc/x, dprc = dprc/x, dwp = dwp/x, dmtp = dmtp/x, dbp = dbp/x, dbp'ni = dbp'ni/x, dmtp'ni = dmtp'ni/x}
sqrtUT Uptrend {..} = Uptrend {uawpc = sqrt uawpc, uprc = sqrt uprc, uwp = sqrt uwp, umtp = sqrt umtp, ubp = sqrt ubp, ubp'ni = sqrt ubp'ni, umtp'ni = sqrt umtp'ni}
sqrtDT Downtrend {..} = Downtrend {dawpc = sqrt dawpc, dprc = sqrt dprc, dwp = sqrt dwp, dmtp = sqrt dmtp, dbp = sqrt dbp, dbp'ni = sqrt dbp'ni, dmtp'ni = sqrt dmtp'ni}

--
recognize :: IO (Pattern, Uptrend, Downtrend)
recognize = do
    [orderBookFiles, tradeLogFiles, tickerMovementFiles] <- getFilesGrouped
    (orderBooks, tradeLogs, tickerMovements) <- readFiles orderBookFiles tradeLogFiles tickerMovementFiles

        -- data to work with
    let datasets = getData orderBooks tradeLogs tickerMovements
        -- get uptrends and downtrends
        OpeningPricePatterns {..} = foldl (\OpeningPricePatterns {..} Data {..} ->
                let uptrend = Uptrend {uawpc = awpc, uprc = prc, uwp = wp, umtp = mtp, ubp = bp, ubp'ni = bp'ni, umtp'ni = mtp'ni} 
                    downtrend = Downtrend {dawpc = awpc, dprc = prc, dwp = wp, dmtp = mtp, dbp = bp, dmtp'ni = mtp'ni, dbp'ni = bp'ni} 
                    (opp'ut', opp'dt') = if op > 0 then (uptrend : opp'ut, opp'dt) else (opp'ut, downtrend : opp'dt)

                in OpeningPricePatterns {opp'ut = opp'ut', opp'dt = opp'dt'}
            ) OpeningPricePatterns {opp'ut = [], opp'dt = []} datasets

        -- actual data
        n'ut = fromIntegral . length $ opp'ut
        n'dt = fromIntegral . length $ opp'dt
        a'opp@Pattern {
            ut = Uptrend {uawpc = uawpc'a, uprc = uprc'a, uwp = uwp'a, umtp = umtp'a, ubp = ubp'a, ubp'ni = ubp'ni'a, umtp'ni = umtp'ni'a}, 
            dt = Downtrend {dawpc = dawpc'a, dprc = dprc'a, dwp = dwp'a, dmtp = dmtp'a, dbp = dbp'a, dbp'ni = dbp'ni'a, dmtp'ni = dmtp'ni'a}} 
            = Pattern {ut = divUT (foldl1 addUT opp'ut) n'ut, dt = divDT (foldl1 addDT opp'dt) n'dt}
        opp'ut'std@Uptrend {uawpc = uawpc'std, uprc = uprc'std, uwp = uwp'std, umtp = umtp'std, ubp = ubp'std, ubp'ni = ubp'ni'std, umtp'ni = umtp'ni'std}
            = sqrtUT . (`divUT` n'ut) $ foldl1 (\Uptrend {uawpc = uawpc', uprc = uprc', uwp = uwp', umtp = umtp', ubp = ubp', ubp'ni = ubp'ni', umtp'ni = umtp'ni'} Uptrend {..} -> 
            Uptrend {
                uawpc =     uawpc'  + ((uawpc-uawpc'a)**2)      ,
                uprc =      uprc'   + ((uprc-uprc'a)**2)        ,
                uwp =       uwp'    + ((uwp-uwp'a)**2)          ,
                umtp =      umtp'   + ((umtp-umtp'a)**2)        ,
                ubp =       ubp'    + ((ubp-ubp'a)**2)          ,
                ubp'ni =    ubp'ni' + ((ubp'ni-ubp'ni'a)**2)    ,
                umtp'ni =   umtp'ni'+ ((umtp'ni-umtp'ni'a)**2)  }) opp'ut
        opp'dt'std@Downtrend {dawpc = dawpc'std, dprc = dprc'std, dwp = dwp'std, dmtp = dmtp'std, dbp = dbp'std, dbp'ni = dbp'ni'std, dmtp'ni = dmtp'ni'std} 
            = sqrtDT . (`divDT` n'dt) $ foldl1 (\Downtrend {dawpc = dawpc', dprc = dprc', dwp = dwp', dmtp = dmtp', dbp = dbp', dbp'ni = dbp'ni', dmtp'ni = dmtp'ni'} Downtrend {..} -> 
            Downtrend {
                dawpc =     dawpc'  + ((dawpc-dawpc'a)**2)      ,
                dprc =      dprc'   + ((dprc-dprc'a)**2)        ,
                dwp =       dwp'    + ((dwp-dwp'a)**2)          ,
                dmtp =      dmtp'   + ((dmtp-dmtp'a)**2)        ,
                dbp =       dbp'    + ((dbp-dbp'a)**2)          ,
                dbp'ni =    dbp'ni' + ((dbp'ni-dbp'ni'a)**2)    ,
                dmtp'ni =   dmtp'ni'+ ((dmtp'ni-dmtp'ni'a)**2)  }) opp'dt

    return (a'opp, opp'ut'std, opp'dt'std)

readFiles :: [FilePath] -> [FilePath] -> [FilePath] -> IO (V.Vector BookLog, V.Vector TradeLog, V.Vector TickerMovement)
readFiles orderBookFiles tradeLogFiles tickerMovementFiles = do
    orderBooks      <- V.fromList . concat <$> mapM (fmap (map read . filter (not . null) . drop 2 . lines) . readFile') orderBookFiles
    tradeLogs       <- V.fromList . concat <$> mapM (fmap (map read . filter (not . null) . drop 2 . lines) . readFile') tradeLogFiles
    tickerMovements <- V.fromList . concat <$> mapM (fmap (map read . filter (not . null) . drop 2 . lines) . readFile') tickerMovementFiles

    return (orderBooks, tradeLogs, tickerMovements)

getData :: V.Vector BookLog -> V.Vector TradeLog -> V.Vector TickerMovement -> [Data]
getData obs tls tms = map (\x -> 
            let BookLog {comp = ([awpc, prc], _), asksI = _, bidsI = _} = (V.!) obs x
                    -- awpc :: average weighted price compare, prc :: price range compare
                TradeLog {wp = wp1, mtp = mtp1, bp = bp1} = (V.!) tls (x-1)
                TradeLog {wp = wp2, mtp = mtp2, bp = bp2} = (V.!) tls x
                    -- wp :: weighted price, mtp :: market trade %, bp :: buy %
                TickerMovement {c'op = op, c'lp = lp, c'hp = hp, c'vol = vol} = (V.!) tms x
                    -- c'op :: change open price, c'lp :: change low price, c'hp :: change high price, c'vol :: change volume
        
                wp = wp2-wp1
                mtp = mtp2-mtp1
                bp = bp2-bp1
                mtp'ni = mtp2
                bp'ni = bp2

            in Data {..}
        ) [1 .. (V.length obs - 1)]

