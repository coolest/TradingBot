{-
    For: 
        Moving files
        Deleting files
        Getting file list
-}

module Files where

import qualified System.IO.Strict as S
import System.IO
import System.Directory

import Control.Monad

dirs = ["Data\\OrderBook\\", "Data\\RecentTrades\\", "Data\\TickerMovements\\"]

getFiles :: IO [FilePath]
getFiles = concat 
    <$> getFilesGrouped

getFilesGrouped :: IO [[FilePath]]
getFilesGrouped = mapM (\fp -> map (fp++) <$> listDirectory fp) dirs

move :: IO ()
move = do
    f <- getFiles
    mapM_ (\fp -> renamePath fp ("Storage" ++ drop 4 fp)) f

delete :: IO ()
delete = do 
    f <- getFiles
    mapM_ removeFile f

readFile' fp = do
    f <- openFile fp ReadMode
    S.hGetContents f
