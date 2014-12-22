
module Data.IndexSearch.Type(Index(..)) where

import Data.ByteString

data Index = Index
    {idxSave :: FilePath -> IO ()
    ,idxSize :: IO Int
    ,idxLength :: IO Int
    ,idxCount :: ByteString -> IO Int
    ,idxLocate :: ByteString -> Int -> IO [Int]
    ,idxExtract :: Int -> Int -> IO ByteString
    }
