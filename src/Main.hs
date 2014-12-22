{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Data.IndexSearch.FMIndex
import qualified Data.ByteString.Char8 as BS
import Control.Exception.Extra
import System.Directory
import System.FilePath


main :: IO ()
main = do
    idx <- buildIndex "mississippi"
    ignore $ removeDirectoryRecursive ".index-test"
    createDirectoryIfMissing True ".index-test"
    test idx
    saveIndex ".index-test/index" idx
    idx <- loadIndex ".index-test/index"
    test idx

test :: Index -> IO ()
test idx = do
    print =<< countIndex idx "is"
    print =<< sizeIndex idx
    print =<< lengthIndex idx
    print =<< locateIndex idx "s" 100
    print =<< try_ (extractIndex idx 2 6)
