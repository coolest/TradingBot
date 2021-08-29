{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

{-
    For: 
        Testing pattern logic
        Creating & canceling trades
        Performing the main task
-}

import Collector hiding (Trade (..))
import Patterns (recognize, readFiles, Data (..), Uptrend (..), Downtrend (..), Pattern (..))
import Files (getFilesGrouped)

import qualified Network.Wreq.Session as Sess
import Network.Wreq

import qualified Data.CaseInsensitive as CI
import qualified Data.Vector as V
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson as AE
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson.Lens
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.List
import Data.Digest.Pure.SHA
import Data.ByteString.Base64
import Data.Maybe
import Data.Either

import Text.Printf
import Text.Format

import System.Exit

import Control.Lens
import Control.Monad
import Control.Concurrent

import Numeric

import qualified Codec.Binary.UTF8.String as UTF
--

publicKey = ""
privateKey = ""
apiUrl = "https://api.kraken.com"

errWebhook = "https://discord.com/api/webhooks//"
logWebhook = "https://discord.com/api/webhooks//"
prdWebhook = "https://discord.com/api/webhooks//"
rsiWebhook = "https://discord.com/api/webhooks//"

-- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring
packStr :: String -> B.ByteString
packStr = encodeUtf8 . T.pack

packStrLB :: String -> LB.ByteString
packStrLB = LB.pack . UTF.encode

packStrCI :: String -> CI.CI B.ByteString
packStrCI = CI.mk . packStr

debugMain = True
debugBuy  = True
debugSell = True
--

data OrderError = Null | OrderError (V.Vector String) deriving (Eq, Show)
data Balance = Balance {usd :: Double, btc :: Double} deriving (Show)
data Prediction = UT | DT | U deriving (Eq)
data Settings = Settings {
    p'awpc :: Bool,
    p'prc :: Bool,
    p'wp :: Bool,
    p'mtp :: Bool,
    p'bp :: Bool,
    p'bp'ni :: Bool,
    p'mtp'ni :: Bool,
    pattern' :: (Pattern, Uptrend, Downtrend)}

optimalSettings p = genSettings p [False, False, False, False, False, True, False]
genSettings p [x1, x2, x3, x4, x5, x6, x7] = Settings {
    p'awpc = x1, 
    p'prc = x2, 
    p'wp = x3, 
    p'mtp = x4, 
    p'bp = x5, 
    p'bp'ni = x6,
    p'mtp'ni = x7, 
    pattern' = p}
    
--

instance AE.FromJSON Balance where
    parseJSON (AE.Object o) = do 
        usd <- read <$> (o AE..: T.pack "result" >>= \x -> x AE..: T.pack "ZUSD")
        btc <- read <$> (o AE..: T.pack "result" >>= \x -> x AE..: T.pack "XXBT")
        return Balance {..}

instance AE.FromJSON OrderError where
    parseJSON (AE.Object o) = do
        e <- o AE..: T.pack "error"
        if V.null e
            then return Null
            else return $ OrderError e

getBTCPrice :: Sess.Session -> IO BTCPRICE
getBTCPrice sess = do
    r <- Sess.get sess "https://www.kraken.com/api/internal/cryptowatch/markets/summary?assetName=new"
    ct <- getCurrentTime

    let code = r ^. responseStatus . statusCode
        body = r ^. responseBody
        price = AE.eitherDecode body

    when debugReq $
        printf "*\nBITCOIN PRICE REQ @ %s \n *%s\n*\n" (show ct) (show code)

    if isLeft price
        then onFail "GETTING BTC PRICE" sess
        else return . fromRight' $ price
        
--
sync :: IO ()
sync = do
    currentTime <- getCurrentTime

    let ctInt = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds $ currentTime
        nextInterval = 300 * (ctInt `div` 300) + 300
        delayAmt = nextInterval - ctInt

    when debugWaiting $
        printf "*\nDelaying %ds on %s\n*\n" delayAmt (show currentTime)
        
    threadDelay (delayAmt * 1000000 + 1000)

onFail :: String -> Sess.Session -> IO a
onFail issue sess = do
    time <- getCurrentTime
    printf "\nERROR %s @ %s\n\t SHUTTING DOWN" issue (show time)

    let opts = defaults & header (packStrCI "Content-Type") .~ [packStr "application/json"]
    Sess.postWith opts sess errWebhook ["content" := ("@everyone\nERROR " ++ issue ++ " @ " ++ show time ++ "\n*SHUTTING DOWN*")]

    die "\n\nProgram Killed Successfully."

log' :: String -> Sess.Session -> IO ()
log' msg sess = do
    balance <- getBalance sess 0
    time <- getCurrentTime

    when (isJust balance) $ do
        let Balance {..} = fromJust balance
            opts = defaults & header (packStrCI "Content-Type") .~ [packStr "application/json"]
        r <- Sess.postWith opts sess logWebhook ["content" := format ".\nLOG | {0}\n @{1}\n new balance: usd = {2}, btc = {3}\n." [msg, show time, show usd, show btc]]
        
        return ()

logPrediction :: Sess.Session -> Bool -> Double -> Double -> IO (Response LB.ByteString)
logPrediction sess pred oldp newp = do
    let opts = defaults & header (packStrCI "Content-Type") .~ [packStr "application/json"]
    if (pred && oldp < newp) || (not pred && oldp > newp) 
        then Sess.postWith opts sess prdWebhook ["content" := format ".\nCORRECT PREDICTION:\nUTP: {0}\nOP: {1}\nNP: {2}\n." [show pred, show oldp, show newp]]
        else Sess.postWith opts sess prdWebhook ["content" := format ".\nINCORRECT PREDICTION:\nUTP: {0}\nOP: {1}\nNP: {2}\n." [show pred, show oldp, show newp]]

sign :: String -> String -> String -> B.ByteString
sign uri'str payload'str nonce'str = encode . LB.toStrict . bytestringDigest 
    $ hmacSha512 (LB.fromStrict . decodeLenient $ privateKey) (LB.append uri (bytestringDigest . sha256 $ LB.append nonce payload))
    where
        uri =       packStrLB uri'str
        payload =   packStrLB payload'str
        nonce =     packStrLB nonce'str

getTime :: IO String
getTime = show . floor . (*10) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

getOpts :: B.ByteString -> Options
getOpts sig = defaults 
    & header (packStrCI "Content-Type") .~ [packStr "application/x-www-form-urlencoded"] 
    & header (packStrCI "API-Key")      .~ [publicKey] 
    & header (packStrCI "API-Sign")     .~ [sig] 

getBalance :: Sess.Session -> Int -> IO (Maybe Balance)
getBalance sess 3 = do
    onFail "GETTING ACCOUNT BALANCE" sess
    return Nothing

getBalance sess tries = do
    nonce <- getTime

    let uri = "/0/private/Balance"
        payload = format "nonce={0}" [nonce]
        signature = sign uri payload nonce
        opts = getOpts signature

    r <- Sess.postWith opts sess (apiUrl ++ uri) ["nonce" := packStr nonce]

    let body = r ^. responseBody
        balance = AE.eitherDecode body

    if isRight balance
        then return . Just . fromRight' $ balance
        else do
            threadDelay 500000
            getBalance sess (tries+1)
    
sell :: Sess.Session -> IO ()
sell sess = do
    balance <- getBalance sess 0
    nonce <- getTime

    when (isJust balance) $ do
        let Balance {..} = fromJust balance
            uri = "/0/private/AddOrder"
            volume = showFFloat Nothing btc ""
            payload = format "nonce={0}&pair=XBTUSD&type=sell&ordertype=market&volume={1}" [nonce, volume]
            signature = sign uri payload nonce
            opts = getOpts signature

        r <- Sess.postWith opts sess (apiUrl ++ uri) [
            "nonce"     := nonce, 
            "pair"      := packStr "XBTUSD", 
            "type"      := packStr "sell", 
            "ordertype" := packStr "market", 
            "volume"    := packStr volume]

        let code = r ^. responseStatus . statusCode
            body = r ^. responseBody
            err = AE.decode body
        
        log' ("SELL ORDER REQUESTED\n Response: " ++ show body ++ "\n Code: " ++ show code) sess

        when debugSell $ do
            putStrLn "*\nSold bitcoin. Check discord for log.\n*"

        when (isJust err && Null /= fromJust err) $ do
            printf "SELL ORDER FAILED!\n%s" (show err)
            onFail "SELLING" sess

buy :: Sess.Session -> IO Double
buy sess = do
    balance <- getBalance sess 0
    nonce <- getTime

    if isNothing balance
        then return 0.0
        else do
            (BTCPRICE btcprice) <- getBTCPrice sess

            let Balance {..} = fromJust balance
                volume = showFFloat Nothing ((\x -> x - x * 0.1) (usd / btcprice)) ""
                uri = "/0/private/AddOrder"
                payload = format "nonce={0}&ordertype=market&type=buy&volume={1}&pair=XBTUSD" [nonce, volume]
                signature = sign uri payload nonce
                opts = getOpts signature

            r <- Sess.postWith opts sess (apiUrl ++ uri) [
                "nonce"     := packStr nonce,  
                "ordertype" := packStr "market", 
                "type"      := packStr "buy", 
                "volume"    := packStr volume,
                "pair"      := packStr "XBTUSD"]
            
            let body = r ^. responseBody
                code = r ^. responseStatus . statusCode
                err = AE.decode body
            
            log' (format "BUY ORDER REQUESTED\n Response: {0}\n Code: {1}" [show body, show code]) sess
            
            when debugBuy $ do
                putStrLn "*\nBought bitcoin. Check discord for log.\n*"

            when (isJust err && Null /= fromJust err) $ do
                printf "BUY ORDER FAILED!\n%s" (show err)
                onFail "BUYING BTC" sess

            return btcprice

main :: IO ()
main = do
    sync

    sess <- Sess.newAPISession
    p <- Patterns.recognize

    handle p sess Nothing False

    where
        handle :: (Pattern, Uptrend, Downtrend) -> Sess.Session -> Maybe TradeLog -> Bool -> IO ()
        handle p sess pTradeLog hodling = do
            data' <- Collector.collect sess
            if isNothing data'
                then do 
                    when debugMain $ do
                        putStrLn "*\nCollector.collect failed to collect data. Selling out of positions.\n*"
                    
                    sell sess
                    delay 5
                    handle p sess Nothing False -- sell out of pos just bc not having data is weird
                else let (bookLog, tickerInfo, tradeLog) = fromJust data' in
                    if isNothing pTradeLog
                        then do 
                            when debugMain $ do
                                putStrLn "*\nNo previous data. Retrying.\n*"

                            delay 5
                            handle p sess (Just tradeLog) False
                        else do -- if there is data & a previous trade log then do the stuff
                            predictGain <- performLogic (optimalSettings p) bookLog (fromJust pTradeLog) tradeLog
                            (BTCPRICE btcprice) <- getBTCPrice sess

                            case (hodling, predictGain) of 
                                (True, False) -> sell sess; 
                                (False, True) -> do buy sess; return ()
                                _ -> return ();

                            when debugMain $ do
                                printf "*\nWORKED AS EXPECTED. (hodling, predictGain) = (%s, %s)\n*\n" (show hodling) (show predictGain)

                            delay 5

                            (BTCPRICE btcpricenew) <- getBTCPrice sess
                            logPrediction sess predictGain btcprice btcpricenew

                            handle p sess (Just tradeLog) predictGain


performLogic :: Settings -> BookLog -> TradeLog -> TradeLog -> IO Bool
performLogic Settings {..} BookLog {comp = ([awpc, prc], _), asksI = _, bidsI = _} TradeLog {..} TradeLog {wp = wp2, mtp = mtp2, bp = bp2} = do
    let (Pattern {..}, Uptrend {..}, Downtrend{..}) = pattern'
        Uptrend {uawpc = uawpc'a, uprc = uprc'a, uwp = uwp'a, umtp = umtp'a, ubp = ubp'a, ubp'ni = ubp'ni'a, umtp'ni = umtp'ni'a} = ut
        Downtrend {dawpc = dawpc'a, dprc = dprc'a, dwp = dwp'a, dmtp = dmtp'a, dbp = dbp'a, dbp'ni = dbp'ni'a, dmtp'ni = dmtp'ni'a} = dt

        wp' = wp2-wp
        mtp' = mtp2-mtp
        bp' = bp2-bp
        mtp'ni' = mtp2
        bp'ni' = bp2

        indicators = 
            [ if p'awpc then predict awpc (uawpc, uawpc'a) (dawpc, dawpc'a) else U
            , if p'prc then predict prc (uprc, uprc'a) (dprc, dprc'a) else U
            , if p'wp then predict wp' (uwp, uwp'a) (dwp, dwp'a) else U
            , if p'mtp then predictOpp mtp' (umtp, umtp'a) (dmtp, dmtp'a) else U
            , if p'bp then predict bp' (ubp, ubp'a) (dbp, dbp'a) else U
            , if p'bp'ni then predict bp'ni' (ubp'ni, ubp'ni'a) (dbp'ni, dbp'ni'a) else U
            , if p'mtp'ni then predictOpp mtp'ni' (umtp'ni, umtp'ni'a) (dmtp'ni, dmtp'ni'a) else U]

        prediction = let x = foldl (\a ind -> case ind of UT -> a+1; DT -> a-1; U -> a) 0 indicators
            in if x > 0 then UT else DT -- if uncertain go with downtrend to sell out of positions

    return $ prediction == UT

    where
        v = 0.33
        predict x (ut'std, ut'a) (dt'std, dt'a)
            | ut'a + ut'std * v <= x = UT
            | dt'a - dt'std * v >= x = DT
            | otherwise = U

        predictOpp x (ut'std, ut'a) (dt'std, dt'a)
            | ut'a + ut'std * v <= x = DT
            | dt'a - dt'std * v >= x = UT
            | otherwise = U
    
performLogicTest :: Settings -> BookLog -> TradeLog -> TradeLog -> TickerMovement -> IO Bool
performLogicTest s bl1 tl1 tl2 TickerMovement {..} = (\b -> not b || b == (c'op > 0)) <$> performLogic s bl1 tl1 tl2

test :: Settings -> IO Double
test settings = do
    [orderBookFiles, tradeLogFiles, tickerMovementFiles] <- getFilesGrouped
    (orderBooks, tradeLogs, tickerMovements) <- readFiles orderBookFiles tradeLogFiles tickerMovementFiles

    ps <- mapM (\i -> performLogicTest settings (orderBooks V.! (i - 1)) (tradeLogs V.! (i - 2)) (tradeLogs V.! (i - 1)) (tickerMovements V.! i)) [2 .. (length orderBooks - 1)]
        
    return $ (fromIntegral . length $ filter (== True) ps) / (fromIntegral . length $ ps)

testAll :: IO ()
testAll = do
    let allValues = concat
            [ map head . group . sort $ permutations [True, True, True, True, True, True, False]
            , map head . group . sort $ permutations [True, True, True, True, True, False, False]
            , map head . group . sort $ permutations [True, True, True, True, False, False, False]
            , map head . group . sort $ permutations [True, True, True, False, False, False, False]
            , map head . group . sort $ permutations [True, True, False, False, False, False, False]
            , map head . group . sort $ permutations [True, False, False, False, False, False, False]]

    p <- Patterns.recognize

    mapM_ (\s -> printf "%s \n\tP: %.5f\n" (show s) =<< test (genSettings p s)) allValues





-- RSI VERSION (predictions based on RSI)
newtype RSI = RSI Double deriving (Show)

instance AE.FromJSON RSI where
    parseJSON (AE.Object o) = do 
        xs' <- mapM (\x -> (\x -> read x :: Double) <$> x AE..: "op") . take 14 . reverse <$> (o AE..: "result" >>= \o -> V.head o AE..: "candlesticks")
        xs <- xs'

        let changes = groupBy (\a b -> (a < 0 && b < 0) || (a > 0 && b > 0)) . sort $ map fst (foldl (\xs@((_, x1):_) x2 -> (x2-x1, x2):xs) [(0, last xs)] (init xs))

        if length changes == 1
            then  -- doing rsi wrong
                if (head . head $ changes) > 0 
                    then return . RSI $ 100
                    else return . RSI $ 0
            else 
                let [avgLoss, avgGain] = [(sum . head $ changes) / (fromIntegral . length . head $ changes), (sum . last $ changes) / (fromIntegral . length . last $ changes)]
                in return (RSI (100 - 100 / (1 - avgGain/avgLoss)))

getRSI :: Sess.Session -> IO RSI
getRSI sess = do
    ct <- getCurrentTime

    let t = 60 * ((floor . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds ct) `div` 60) - 180*60
    r <- Sess.get sess ("https://www.kraken.com/api/internal/cryptowatch/markets/btcusd/ohlc?periods=60&after=" ++ show t)

    let code = r ^. responseStatus . statusCode
        body = r ^. responseBody
        rsi = AE.eitherDecode body
        
    when debugReq $
        printf "*\nRSI REQUEST @ %s \n *%s\n*\n" (show ct) (show code)

    if isRight rsi
        then return . fromRight' $ rsi
        else onFail "GETTING RSI" sess

rsiMain :: IO ()
rsiMain = do
    sess <- Sess.newAPISession

    handle sess

    where
        waitUntilRSICOND :: (Double -> Bool) -> String -> Sess.Session -> IO Double
        waitUntilRSICOND cond msg sess = do
            (RSI rsi) <- getRSI sess
            if cond rsi
                then return rsi
                else do
                    printf "*\nRETRYING: RSI OF %s DOES NOT MEET COND (%s)\n*\n" (show rsi) msg
                    delay 1; threadDelay 10000000 -- delay extra 10 secs
                    waitUntilRSICOND cond msg sess

        handle sess = do
            waitUntilRSICOND (< 30) "< 30" sess -- let it get oversold
            waitUntilRSICOND (> 30) "> 30" sess -- rising out of oversold

            buy'price <- buy sess -- buy now

            rsi <- waitUntilRSICOND (> 70) "> 70" sess -- starting to get overbought or i messed up
            when (rsi >= 26) (do waitUntilRSICOND (< 70) "< 70" sess; return ()) -- starting to drop

            (BTCPRICE sell'price) <- getBTCPrice sess
            sell sess -- sell now

            let opts = defaults & header (packStrCI "Content-Type") .~ [packStr "application/json"]
            Sess.postWith opts sess rsiWebhook ["content" := format ".\nTRADE:\nENTRY: {0}\nEXIT: {1}\n." [show buy'price, show sell'price]]

            handle sess
