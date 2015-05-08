{-# LANGUAGE RecordWildCards #-}

{-| This module is for detecting which SIMD instruction sets your CPU supports. In particular, it can detect SSE4.2, AVX and AVX2 -} 
module SDR.CPUID (
    cpuid,
    cpuidExtended,
    CPUInfo(..),
    getCPUInfo,
    hasSSE42,
    hasAVX,
    hasAVX2,
    featureSelect
    ) where

import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.List
import Data.Maybe

foreign import ccall unsafe "cpuid"
    cpuid_c :: Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe "cpuid_extended"
    cpuidExtended_c :: Word32 -> Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO ()

-- | Execute the CPUID instruction
cpuid :: Word32                              -- ^ Operation (EAX)
      -> IO (Word32, Word32, Word32, Word32) -- ^ Result (EAX, EBX, ECX, EDX)
cpuid x = 
    alloca $ \p1 -> 
    alloca $ \p2 -> 
    alloca $ \p3 -> 
    alloca $ \p4 -> do
        cpuid_c x p1 p2 p3 p4
        (,,,) <$> peek p1 <*> peek p2 <*> peek p3 <*> peek p4

-- | Execute the CPUID instruction setting ECX as well
cpuidExtended :: Word32                              -- ^ Operation (EAX)
              -> Word32                              -- ^ ECX
              -> IO (Word32, Word32, Word32, Word32) -- ^ Result (EAX, EBX, ECX, EDX)
cpuidExtended x y = 
    alloca $ \p1 -> 
    alloca $ \p2 -> 
    alloca $ \p3 -> 
    alloca $ \p4 -> do
        cpuidExtended_c x y p1 p2 p3 p4
        (,,,) <$> peek p1 <*> peek p2 <*> peek p3 <*> peek p4

-- | Information about the features supported by your CPU
data CPUInfo = CPUInfo {
    features         :: Word32,
    extendedFeatures :: Maybe Word32
}

-- | Get a `CPUInfo`
getCPUInfo :: IO CPUInfo
getCPUInfo = do
    (x, _, _, _) <- cpuid 0
    (_, _, f, _) <- cpuid 1
    if x < 7 then
        return $ CPUInfo f Nothing
    else do
        (_, e, _, _) <- cpuidExtended 7 0
        return $ CPUInfo f (Just e)

-- | Feature bit for SSE4.2
sse42 = 20

-- | Feature bit for AVX
avx   = 28

-- | Extended feature bit for AVX2
avx2  = 5

-- | Check if the CPU supports SSE4.2
hasSSE42 :: CPUInfo -> Bool
hasSSE42 CPUInfo{..} = testBit features sse42

-- | Check if the CPU supports AVX
hasAVX   :: CPUInfo -> Bool
hasAVX CPUInfo{..} = testBit features avx 

-- | Check if the CPU supports AVX2
hasAVX2  :: CPUInfo -> Bool
hasAVX2 (CPUInfo _ Nothing)  = False
hasAVX2 (CPUInfo _ (Just f)) = testBit f avx2

-- | Convenience function for selecting a function based on the features that the CPU supports
featureSelect :: CPUInfo                -- ^ The CPU features
              -> a                      -- ^ Default implementation
              -> [(CPUInfo -> Bool, a)] -- ^ List of (feature, implementation) pairs
              -> a                      -- ^ The selected implementation
featureSelect info def list = maybe def snd $ find (($ info) . fst) list

