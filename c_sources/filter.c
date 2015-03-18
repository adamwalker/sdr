#include <stdio.h>
#include <stdint.h>
#include <x86intrin.h>

#include "common.h"

/*
 * Real coefficients, real inputs
 */
void filterRR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num; i++){
        float *startPtr = inBuf + i;
        outBuf[i] = dotprod_R(numCoeffs, coeffs, startPtr);
    }
}

/*
 * SIMD versions
 */
void filterSSERR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num; i++){
        float *startPtr = inBuf + i;
        __m128 accum = sse_dotprod_R(numCoeffs, coeffs, startPtr);
        accum = sse_hadd_R(accum);
        _mm_store_ss(outBuf + i, accum);
    }
}

void filterAVXRR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num; i++){
        float *startPtr = inBuf + i;
        __m256 accum  = avx_dotprod_R(numCoeffs, coeffs, startPtr);
        __m128 accum2 = avx_hadd_R(accum);
        _mm_store_ss(outBuf + i, accum2);
    }
}

/*
 * Symmetric versions
 */
void filterSSESymmetricRR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num; i++){
        float *startPtr = inBuf + i;
        __m128 accum = sse_sym_dotprod_R(numCoeffs, coeffs, startPtr);
        accum = sse_hadd_R(accum);
        _mm_store_ss(outBuf + i, accum);
    }
}

void filterAVXSymmetricRR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num; i++){
        float *startPtr = inBuf + i;
        __m256 accum  = avx_sym_dotprod_R(numCoeffs, coeffs, startPtr);
        __m128 accum2 = avx_hadd_R(accum);
        _mm_store_ss(outBuf + i, accum2);
    }
}

/*
 * Real coefficients, complex input
 */

void filterRC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num*2; i+=2){
        float *startPtr = inBuf + i;
        dotprod_C(numCoeffs, coeffs, startPtr, outBuf + i);
    }
}

/*
 * SIMD versions
 */

void filterSSERC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num*2; i+=2){
        float *startPtr = inBuf + i;
        __m128 accum = sse_dotprod_R(numCoeffs, coeffs, startPtr);
        accum        = sse_hadd_C(accum);
        store_complex(outBuf + i, accum);
    }
}

void filterSSERC2(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num*2; i+=2){
        float *startPtr = inBuf + i;
        __m128 accum = sse_dotprod_C(numCoeffs, coeffs, startPtr);
        accum        = sse_hadd_C(accum);
        store_complex(outBuf + i, accum);
    }
}

void filterAVXRC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num*2; i+=2){
        float *startPtr = inBuf + i;
        __m256 accum  = avx_dotprod_R(numCoeffs, coeffs, startPtr);
        __m128 accum2 = avx_hadd_C(accum);
        store_complex(outBuf + i, accum2);
    }
}

void filterAVXRC2(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num*2; i+=2){
        float *startPtr = inBuf + i;
        __m256 accum  = avx_dotprod_C(numCoeffs, coeffs, startPtr);
        __m128 accum2 = avx_hadd_C(accum);
        store_complex(outBuf + i, accum2);
    }
}

/*
 * Symmetric versions
 */
void filterSSESymmetricRC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num*2; i+=2){
        float *startPtr = inBuf + i;
        __m128 accum = sse_sym_dotprod_C(numCoeffs, coeffs, startPtr);
        accum = sse_hadd_C(accum);
        store_complex(outBuf + i, accum);
    }
}

void filterAVXSymmetricRC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num*2; i+=2){
        float *startPtr = inBuf + i;
        __m256 accum = avx_sym_dotprod_C(numCoeffs, coeffs, startPtr);
        __m128 accum1 = avx_hadd_C(accum);
        store_complex(outBuf + i, accum1);
    }
}
