
module Data.IndexSearch(
    Index, saveIndex, sizeIndex, lengthIndex,
    countIndex, locateIndex, extractIndex
    ) where

import Data.IndexSearch.Type
import Data.ByteString


saveIndex :: FilePath -> Index -> IO ()
saveIndex file idx = idxSave idx file

sizeIndex :: Index -> IO Int
sizeIndex = idxSize

lengthIndex :: Index -> IO Int
lengthIndex = idxLength

countIndex :: Index -> ByteString -> IO Int
countIndex = idxCount

locateIndex :: Index -> ByteString -> Int -> IO [Int]
locateIndex = idxLocate

extractIndex :: Index -> Int -> Int -> IO ByteString
extractIndex = idxExtract
