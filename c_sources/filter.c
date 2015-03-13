#include <stdio.h>
#include <stdint.h>
#include <x86intrin.h>

#include "common.h"

/*
 * Real coefficients, real inputs
 */
void filterRR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num; i++){
        float accum = 0;
        float *startPtr = inBuf + i;
        for(j=0; j<numCoeffs; j++){
            accum += startPtr[j] * coeffs[j];
        }
        outBuf[i] = accum;
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
        float real = 0;
        float imag = 0;
        float *startPtr = inBuf + i;
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

/*
 * Rational downsampling
 */
void resample(int buf_size, int coeff_size, int interpolation, int decimation, int filter_offset, float *coeffs, float *in_buf, float *out_buf){
    int j, k, l;
    int input_offset = 0;
    for(k=0; k<buf_size; k++) {
        float accum = 0;

        for(l=0, j=filter_offset; j<coeff_size; l++, j+=interpolation) {
            accum += in_buf[input_offset + l] * coeffs[j];
        }

        int filter_offset_new = interpolation - 1 - (decimation - filter_offset - 1) % interpolation;
        input_offset += (decimation - filter_offset - 1) / interpolation + 1;
        filter_offset = filter_offset_new;

        out_buf[k] = accum;
    }
}

int resample2(int buf_size, int num_coeffs, int starting_group, int num_groups, int *increments, float **coeffs, float *in_buf, float *out_buf){
    int   i, j;
    int   group      = starting_group;
    float *start_ptr = in_buf;

    for(i=0; i<buf_size; i++){
        float accum = 0;

        for(j=0; j<num_coeffs; j++){
	        accum += start_ptr[j] * coeffs[group][j];
        }
        out_buf[i]  = accum;

        start_ptr  += increments[group];
        group++;
        if(group == num_groups) 
            group = 0;
    }

    return group;
}

int resampleSSERR(int buf_size, int num_coeffs, int starting_group, int num_groups, int *increments, float **coeffs, float *in_buf, float *out_buf){
    int   i, j;
    int   group      = starting_group;
    float *startPtr  = in_buf;

    for(i=0; i<buf_size; i++){
        __m128 accum = sse_dotprod_R(num_coeffs, coeffs[group], startPtr);
        accum = sse_hadd_R(accum);
        _mm_store_ss(out_buf + i, accum);

        startPtr  += increments[group];
        group++;
        if(group == num_groups) 
            group = 0;
    }

    return group;
}

int resampleAVXRR(int buf_size, int num_coeffs, int starting_group, int num_groups, int *increments, float **coeffs, float *in_buf, float *out_buf){
    int   i, j;
    int   group      = starting_group;
    float *startPtr  = in_buf;

    for(i=0; i<buf_size; i++){
        __m256 accum = avx_dotprod_R(num_coeffs, coeffs[group], startPtr);
        __m128 res   = avx_hadd_R(accum);
        _mm_store_ss(out_buf + i, res);

        startPtr  += increments[group];
        group++;
        if(group == num_groups) 
            group = 0;
    }

    return group;
}
