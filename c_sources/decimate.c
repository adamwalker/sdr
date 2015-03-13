#include <stdio.h>
#include <stdint.h>
#include <x86intrin.h>

#include "common.h"

/*
 * Real coefficients, real inputs
 */
void decimateRR(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num; i++, k+=factor){
        float accum = 0;
        float *startPtr = inBuf + k;
        for(j=0; j<numCoeffs; j++){
            accum += startPtr[j] * coeffs[j];
        }
        outBuf[i] = accum;
    }
}

/*
 * SIMD versions
 */
void decimateSSERR(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, k;
    for(i=0, k=0; i<num; i++, k+=factor){
        float *startPtr = inBuf + k;
        __m128 accum = sse_dotprod_R(numCoeffs, coeffs, startPtr);
        accum = sse_hadd_R(accum);
        _mm_store_ss(outBuf + i, accum);
    }
}

void decimateAVXRR(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, k;
    for(i=0, k=0; i<num; i++, k+=factor){
        float *startPtr = inBuf + k;
        __m256 accum  = avx_dotprod_R(numCoeffs, coeffs, startPtr);
        __m128 accum2 = avx_hadd_R(accum);
        _mm_store_ss(outBuf + i, accum2);
    }
}

/*
 * Symmetric versions
 */
void decimateSSESymmetricRR(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, k;
    for(i=0, k=0; i<num; i++, k+=factor){
        float *startPtr = inBuf + k;
        __m128 accum = sse_sym_dotprod_R(numCoeffs, coeffs, startPtr);
        accum = sse_hadd_R(accum);
        _mm_store_ss(outBuf + i, accum);
    }
}

void decimateAVXSymmetricRR(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num; i++, k+=factor){
        float *startPtr = inBuf + k;
        __m256 accum  = avx_sym_dotprod_R(numCoeffs, coeffs, startPtr);
        __m128 accum2 = avx_hadd_R(accum);
        _mm_store_ss(outBuf + i, accum2);
    }
}

/*
 * Real coefficients, complex inputs
 */
void decimateRC(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        float real = 0;
        float imag = 0;
        float *startPtr = inBuf + k;
        for(j=0; j<numCoeffs; j++){
            real += startPtr[2*j] * coeffs[j];
            imag += startPtr[2*j+1] * coeffs[j];
        }
        outBuf[i] = real;
        outBuf[i+1] = imag;
    }
}

/*
 * SIMD versions
 */
void decimateSSERC(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        float *startPtr = inBuf + k;
        __m128 accum = sse_dotprod_R(numCoeffs, coeffs, startPtr);
        accum        = sse_hadd_C(accum);
        store_complex(outBuf + i, accum);
    }
}

void decimateSSERC2(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        float *startPtr = inBuf + k;
        __m128 accum = sse_dotprod_C(numCoeffs, coeffs, startPtr);
        accum        = sse_hadd_C(accum);
        store_complex(outBuf + i, accum);
    }
}


void decimateAVXRC(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        float *startPtr = inBuf + k;
        __m256 accum  = avx_dotprod_R(numCoeffs, coeffs, startPtr);
        __m128 accum2 = avx_hadd_C(accum);
        store_complex(outBuf + i, accum2);
    }
}

void decimateAVXRC2(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        float *startPtr = inBuf + k;
        __m256 accum  = avx_dotprod_C(numCoeffs, coeffs, startPtr);
        __m128 accum2 = avx_hadd_C(accum);
        store_complex(outBuf + i, accum2);
    }
}

/*
 * Symmetric versions
 */
void decimateSSESymmetricRC(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        float *startPtr = inBuf + k;
        __m128 accum = sse_sym_dotprod_C(numCoeffs, coeffs, startPtr);
        accum = sse_hadd_C(accum);
        store_complex(outBuf + i, accum);
    }
}

void decimateAVXSymmetricRC(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        float *startPtr = inBuf + k;
        __m256 accum = avx_sym_dotprod_C(numCoeffs, coeffs, startPtr);
        __m128 accum1 = avx_hadd_C(accum);
        store_complex(outBuf + i, accum1);
    }
}

