{-# LANGUAGE RecordWildCards #-}

module SDR.CPUID (
    cpuid,
    cpuidExtended,
    CPUInfo(..),
    getCPUInfo,
    hasSSE42,
    hasAVX,
    hasAVX2
    ) where

import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

foreign import ccall unsafe "cpuid"
    cpuid_c :: Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "cpuid_extended"
    cpuidExtended_c :: Word32 -> Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()

cpuid :: Word32 -> IO (Word32, Word32, Word32, Word32)
cpuid x = 
    alloca $ \p1 -> 
    alloca $ \p2 -> 
    alloca $ \p3 -> 
    alloca $ \p4 -> do
        cpuid_c x p1 p2 p3 p4
        (,,,) <$> peek p1 <*> peek p2 <*> peek p3 <*> peek p4

cpuidExtended :: Word32 -> Word32 -> IO (Word32, Word32, Word32, Word32)
cpuidExtended x y = 
    alloca $ \p1 -> 
    alloca $ \p2 -> 
    alloca $ \p3 -> 
    alloca $ \p4 -> do
        cpuidExtended_c x y p1 p2 p3 p4
        (,,,) <$> peek p1 <*> peek p2 <*> peek p3 <*> peek p4

data CPUInfo = CPUInfo {
    features         :: Word32,
    extendedFeatures :: Maybe Word32
}

getCPUInfo :: IO CPUInfo
getCPUInfo = do
    (x, _, _, _) <- cpuid 0
    (_, _, f, _) <- cpuid 1
    if x < 7 then
        return $ CPUInfo f Nothing
    else do
        (_, e, _, _) <- cpuidExtended 7 0
        return $ CPUInfo f (Just e)

sse42 = 20
avx   = 28
avx2  = 5

hasSSE42 :: CPUInfo -> Bool
hasSSE42 CPUInfo{..} = testBit features sse42

hasAVX   :: CPUInfo -> Bool
hasAVX CPUInfo{..} = testBit features avx 

hasAVX2  :: CPUInfo -> Bool
hasAVX2 (CPUInfo _ Nothing)  = False
hasAVX2 (CPUInfo _ (Just f)) = testBit f avx2

