{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module Data.IndexSearch.FMIndex(
    buildIndex, loadIndex,
    module Data.IndexSearch
    ) where

import Data.IndexSearch
import Data.IndexSearch.Type
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types


data Idx

-- foreign import ccall "wrapper" mkFree :: (Ptr Idx -> IO ()) -> IO (FunPtr (Ptr Idx -> IO ()))

-- prototypes from the library
foreign import ccall "build_index" build_index :: CString -> CLong -> CString -> Ptr (Ptr Idx) -> IO CInt
foreign import ccall "load_index" load_index :: CString -> Ptr (Ptr Idx) -> IO CInt
-- foreign import ccall "&free_index" free_index :: FunPtr (Ptr Idx -> IO ())
foreign import ccall "save_index" save_index :: Ptr Idx -> CString -> IO CInt
foreign import ccall "index_size" size_index :: Ptr Idx -> Ptr CLong -> IO CInt

foreign import ccall "count" count_index :: Ptr Idx -> CString -> CLong -> Ptr CLong -> IO CInt
foreign import ccall "locate" locate_index :: Ptr Idx -> CString -> CLong -> Ptr (Ptr CLong) -> Ptr CLong -> IO CInt

foreign import ccall "extract" extract_index :: Ptr Idx -> CLong -> CLong -> Ptr CString -> Ptr CLong -> IO CInt
foreign import ccall "get_length" length_index :: Ptr Idx -> Ptr CLong -> IO CInt

foreign import ccall "error_index" error_index :: CInt -> IO CString


buildIndex :: BS.ByteString -> IO Index
buildIndex x = BS.unsafeUseAsCStringLen x $ \(str,len) -> wrapIndex $ build_index str (int len) nullPtr

loadIndex :: FilePath -> IO Index
loadIndex file = withCString file $ \file -> wrapIndex $ load_index file

wrapIndex :: (Ptr (Ptr Idx) -> IO CInt) -> IO Index
wrapIndex f = do
    ptr <- peekResult f
    -- ptr <- newForeignPtr free_index ptr
    let withForeignPtr ptr f = f ptr
    return Index
        {idxSave = \file -> withCString file $ \file -> withForeignPtr ptr $ \ptr ->
            check $ save_index ptr file
        ,idxSize = withForeignPtr ptr $ \ptr -> intM $ peekResult $ size_index ptr
        ,idxLength = withForeignPtr ptr $ \ptr -> intM $ peekResult $ length_index ptr
        ,idxCount = \x -> BS.unsafeUseAsCStringLen x $ \(str,len) -> withForeignPtr ptr $ \ptr ->
            intM $ peekResult $ count_index ptr str (int len)
        ,idxLocate = \x n -> BS.unsafeUseAsCStringLen x $ \(str,len) -> withForeignPtr ptr $ \ptr -> do
            (arr,len) <- peekResult2 $ locate_index ptr str (int len)
            res <- map int <$> peekArray (int len) arr
            -- free arr
            return res
        ,idxExtract = \from to -> withForeignPtr ptr $ \ptr -> do
            print (from, to)
            (str,len) <- peekResult2 $ extract_index ptr (int from) (int to)
            print len
            res <- BS.packCStringLen (str, int len)
            --free str
            print res
            return res
        }


peekResult :: Storable a => (Ptr a -> IO CInt) -> IO a
peekResult f = alloca $ \res -> do check $ f res; peek res

peekResult2 :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO CInt) -> IO (a,b)
peekResult2 f = alloca $ \a -> alloca $ \b -> do check $ f a b; liftM2 (,) (peek a) (peek b)

int :: (Integral a, Integral b) => a -> b
int = fromIntegral

intM :: (Functor f, Integral a, Integral b) => f a -> f b
intM = fmap int

check :: IO CInt -> IO ()
check i = do
    i <- i
    when (i /= 0) $ do
        str <- peekCAString =<< error_index i
        error $ "Data.IndexSearch.FMIndex, " ++ show i ++ ", " ++ str
