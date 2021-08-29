{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
    For:
        collecting data used in logic for buying & selling
        logging data in files for pattern detection
-}

module Collector where

import qualified Network.Wreq.Session as Sess
import Network.Wreq

import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.Vector as V
import qualified Data.Aeson as AE
import Data.Aeson.Lens
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Either
import Data.Foldable
import Data.Bifunctor

import Control.Lens
import Control.Concurrent
import Control.Monad

import System.Directory
import System.PosixCompat.Files

import Text.Printf

-- debugs
debugReq = True -- prints time & req code
debugLog = True -- prints time & logged material
debugWaiting = True -- prints all delays & delay amount

-- data
newtype BTCPRICE = BTCPRICE Double deriving (Show)

-- response data -> getOrderBook
data BookLog = BookLog {comp :: ([Double], POSIXTime), asksI :: ([Double], UTCTime), bidsI :: ([Double], UTCTime)} deriving (Show, Read)
    -- BookLog {comp, asksI, bidsI :: ([avg weighted price, price range], avg time)}
data Record = Record {price :: Double, amt :: Double, time :: UTCTime} deriving (Show)
data Book = Book {asks :: [Record], bids :: [Record]}

data Ticker = Ticker {op :: Double, lp :: Double, hp :: Double, vol :: Double, t :: UTCTime} deriving (Show)
data TickerPair = TickerPair Ticker Ticker deriving (Show)
data TickerMovement = TickerMovement {c'op :: Double, c'lp :: Double, c'hp :: Double, c'vol :: Double} deriving (Show, Read)

data TradeLog = TradeLog {wp :: Double, mtp :: Double, bp :: Double} deriving (Show, Read) 
    -- weighted price, market trades percent, buy percent
data Trade = Trade {p :: Double, v :: Double, buy :: Bool, market :: Bool} deriving (Show)

newtype Trades = Trades [Trade] deriving (Show)

instance Show Book where
    show Book {..} = foldl (\acc r -> acc ++ (show r ++ "\n")) "\nAsks: \n" asks
        ++ foldl (\acc r -> acc ++ (show r ++ "\n")) "Bids: \n" bids

-- i hate parsing json am i dumb or is this overly complicated...
instance AE.FromJSON Record where
    parseJSON (AE.Array v) = do
        price   <- read                     <$> AE.parseJSON (v V.! 0)
        amt     <- read                     <$> AE.parseJSON (v V.! 1)
        time    <- posixSecondsToUTCTime    <$> AE.parseJSON (v V.! 2)
        
        return Record {..}

instance AE.FromJSON Book where
    parseJSON (AE.Object o) = Book
        <$> (o AE..: "result" >>= \o -> o AE..: "XXBTZUSD" >>= \o -> o AE..: "asks")
        <*> (o AE..: "result" >>= \o -> o AE..: "XXBTZUSD" >>= \o -> o AE..: "bids")

instance AE.FromJSON Ticker where
    parseJSON (AE.Object o) = Ticker
        <$> (read <$> o AE..: "op")
        <*> (read <$> o AE..: "lp")
        <*> (read <$> o AE..: "hp")
        <*> (read <$> o AE..: "v")
        <*> (posixSecondsToUTCTime <$> o AE..: "ct")

instance AE.FromJSON TickerPair where
    parseJSON (AE.Object o) = do
        v <- o AE..: "result" >>= \o -> V.head o AE..: "candlesticks"
        return 
            $ TickerPair (last . init $ v) (last v)

instance AE.FromJSON Trade where
    parseJSON (AE.Array vec) = do
        p       <- read      <$> AE.parseJSON (vec V.! 0)
        v       <- read      <$> AE.parseJSON (vec V.! 1)
        buy     <- (==) 'b'  <$> AE.parseJSON (vec V.! 3)
        market  <- (==) 'm'  <$> AE.parseJSON (vec V.! 4)

        return Trade {..}

instance AE.FromJSON Trades where
    parseJSON (AE.Object o) = Trades 
            <$> (o AE..: "result" >>= \o -> o AE..: "XXBTZUSD")

instance AE.FromJSON BTCPRICE where
    parseJSON (AE.Object o) = do
        v <- o AE..: "result"
        v2 <- V.mapM (\x -> do x' <- x AE..: "asset"; return (x', x)) v
        let v2' = v2 :: V.Vector (String, AE.Object)
            o2 = snd . V.head . V.filter ((==) "BTC" . fst) $ v2'
        usdprice <- (V.! 7) <$> o2 AE..: "summaries"
        price <- usdprice AE..: "price" >>= \x -> x AE..: "last"

        return . BTCPRICE . read $ price
-- like it srsly is so much work

-- ez value getting
getWeightedPriceFromRecord Record {..} =    price*amt
getPriceFromRecord Record {..} =            price
getPOSIXFromRecord Record {..} =            utcTimeToPOSIXSeconds time

-- api
getOrderBook :: Sess.Session -> IO (Int, Either String Book)
getOrderBook sess = do
    r <- Sess.get sess "https://api.kraken.com/0/public/Depth?pair=XBTUSD"
    ct <- getCurrentTime

    let code = r ^. responseStatus . statusCode
        body = r ^. responseBody
        orderBook = AE.eitherDecode body
        
    when debugReq $
        printf "*\nORDER BOOK REQUEST @ %s \n *%s\n*\n" (show ct) (show code)

    return (code, orderBook)

getTickerPrice :: Sess.Session -> IO (Int, Either String TickerPair)
getTickerPrice sess = do
    ct <- getCurrentTime

    let t = 300 * ((floor . nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds ct) `div` 300) - 360*300
    r <- Sess.get sess ("https://www.kraken.com/api/internal/cryptowatch/markets/btcusd/ohlc?periods=300&after=" ++ show t)

    let code = r ^. responseStatus . statusCode
        body = r ^. responseBody
        tickerPair = AE.eitherDecode body

    when debugReq $
        printf "*\nTICKER INFO REQUEST @ %s \n *%s\n*\n" (show ct) (show code)

    return (code, tickerPair)

getRecentTrades :: Sess.Session -> IO (Int, Either String Trades)
getRecentTrades sess = do
    ct <- getCurrentTime

    let t = (+ (-300)) . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds $ ct
    r <- Sess.get sess ("https://api.kraken.com/0/public/Trades?pair=XBTUSD&since=" ++ show t)

    let code = r ^. responseStatus . statusCode
        body = r ^. responseBody
        recentTrades = AE.eitherDecode body

    when debugReq $
        printf "*\nRECENT TRADES REQUEST @ %s \n *%s\n*\n" (show ct) (show code)

    return (code, recentTrades)
    
-- logging
createDir :: String -> FilePath -> IO FilePath
createDir creationMsg path = path <$ writeFile path creationMsg

getDir :: FilePath -> IO (Either String FilePath)
getDir path = do
    dirs <- listDirectory path

    if null dirs
        then return $ Left (path ++ "1.txt")
        else (
            do 
            sizes <- foldl (\accM x -> do
                a <- getFileSize $ path ++ x 
                acc <- accM
                return $ (x, a) : acc) (return []) dirs

            return $ let ans = minimumBy (\a b -> snd a `compare` snd b) sizes in
                if snd ans > 64000 
                    then Left (path ++ show (length dirs + 1) ++ ".txt")
                    else Right (path ++ fst ans)
        )

getBookInfo :: [Record] -> ([Double], UTCTime)
getBookInfo b = ([ truncate' 2 (foldl (\acc r -> acc+getWeightedPriceFromRecord r) 0 b / fromIntegral (length b))
    , let f = (\r1 r2 -> getPriceFromRecord r1 `compare` getPriceFromRecord r2) in 
        truncate' 2 (getPriceFromRecord (maximumBy f b) - getPriceFromRecord (minimumBy f b))]
    , posixSecondsToUTCTime $ foldl (\acc r -> acc+getPOSIXFromRecord r) 0 b / fromIntegral (length b))

logOrderBook :: Book -> IO BookLog
logOrderBook Book {..} = do
    res <- getDir "Data\\OrderBook\\"
    dir <- either (createDir "[[ORDER BOOK]]") return res
    ct  <- show <$> getCurrentTime

    let asksI = getBookInfo asks
        bidsI = getBookInfo bids
        comp = bimap (zipWith (\a b -> truncate' 2 (a-b)) (fst asksI)) (\b -> utcTimeToPOSIXSeconds (snd asksI) - utcTimeToPOSIXSeconds b) bidsI
        bookLog = BookLog {..}

    when debugLog $
        printf "*\nLOGGING BOOK INFO @ %s \n *%s\n*\n" ct (show bookLog)

    appendFile dir "\n\n"
    appendFile dir (show bookLog)

    return bookLog

logTickerMovement :: TickerPair -> IO TickerMovement
logTickerMovement (TickerPair Ticker {op = op', lp = lp', hp = hp', vol = vol', t = t'} Ticker {..}) = do
    res <- getDir "Data\\TickerMovements\\"
    dir <- either (createDir "[[TICKER INFO]]") return res
    ct  <- show <$> getCurrentTime

    let c'op  = truncate' 2 (op-op')
        c'lp  = truncate' 2 (lp-lp')
        c'hp  = truncate' 2 (hp-hp')
        c'vol = truncate' 2 (vol-vol')
        tickerMovement = TickerMovement {..}

    when debugLog $
        printf "*\nLOGGING TICKER MOVEMENTS @ %s \n *%s\n*\n" ct (show tickerMovement)

    appendFile dir "\n\n"
    appendFile dir (show tickerMovement)

    return tickerMovement

logRecentTrades :: Trades -> IO TradeLog
logRecentTrades (Trades t) = do
    res <- getDir "Data\\RecentTrades\\"
    dir <- either (createDir "[[RECENT TRADES]]") return res
    ct  <- show <$> getCurrentTime

    let i = 1 / fromIntegral (length t)
        (wp, mtp, bp) = (\(wp, mtp, bp) -> (truncate' 2 wp, truncate' 2 (mtp*100), truncate' 2 (bp*100)))
            $ foldl (\(wp, mtp, bp) Trade {..} -> (wp+v*p*i, if market then mtp+i else mtp, if buy then bp+i else bp)) (0, 0, 0) t
        tradeLog = TradeLog {..}

    when debugLog $
        printf "*\nLOGGING RECENT TRADES @ %s \n *%s\n*\n" ct (show tradeLog)

    appendFile dir "\n\n"
    appendFile dir (show tradeLog)

    return tradeLog

-- main
delay :: Int -> IO ()
delay m = do
    currentTime <- getCurrentTime

    let ctInt = floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds $ currentTime
        nextMinute = 60 * (ctInt `div` 60) + 60
        delayAmt = nextMinute - ctInt + (m-1)*60

    when debugWaiting $
        printf "*\nDelaying %ds on %s\n*\n" delayAmt (show currentTime)

    threadDelay (delayAmt * 1000000 + 1000)

truncate' :: (RealFrac a) => Int -> a -> a
truncate' n x = fromIntegral (floor (x * 10^n)) / 10^n

fromRight' (Right a) = a

collect :: Sess.Session -> IO (Maybe (BookLog, TickerMovement, TradeLog))
collect sess = do
    (ob'code, ob'r) <- getOrderBook sess
    (ti'code, ti'r) <- getTickerPrice sess
    (rt'code, rt'r) <- getRecentTrades sess

    let orderBook    = either (Left . print) (Right . logOrderBook) ob'r
        tickerInfo   = either (Left . print) (Right . logTickerMovement) ti'r
        recentTrades = either (Left . print) (Right . logRecentTrades) rt'r

    if isRight orderBook && isRight tickerInfo && isRight recentTrades
        then do
            orderBook'    <- fromRight' orderBook
            tickerInfo'   <- fromRight' tickerInfo
            recentTrades' <- fromRight' recentTrades
            return 
                . Just 
                $ (orderBook', tickerInfo', recentTrades')
        else return Nothing

gather :: IO ()
gather = do
    sess <- Sess.newAPISession

    delay 1

    replicateM_ 1000 $ do

        collect sess
        
        delay 5
