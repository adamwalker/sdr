#include <stdio.h>
#include <stdint.h>
#include <x86intrin.h>

/*
 * Real horizontal addition
 */
inline __m128 sse_hadd_R(__m128 in){
    __m128 accum;
    accum = _mm_hadd_ps(in, in);
    return _mm_hadd_ps(accum, accum);
}

inline __m128 avx_hadd_R(__m256 in){
    __m128 res1 = _mm256_extractf128_ps(in, 0);
    __m128 res2 = _mm256_extractf128_ps(in, 1);

    res1 = _mm_hadd_ps(res1, res1);
    res1 = _mm_hadd_ps(res1, res1);

    res2 = _mm_hadd_ps(res2, res2);
    res2 = _mm_hadd_ps(res2, res2);

    return _mm_add_ss(res1, res2);
}

/*
 * Real dot products
 */
inline __m128 sse_dotprod_R(int num, float *a, float *b){
    int i;
    __m128 accum = _mm_setzero_ps();

    for(i=0; i<num; i+=4){
        //Load the needed vectors
        __m128 coeff = _mm_loadu_ps(a + i);
        __m128 val   = _mm_loadu_ps(b + i);

        //Multiply and acumulate
        accum = _mm_add_ps(accum, _mm_mul_ps(coeff, val));
    }
    return accum;
}

inline __m256 avx_dotprod_R(int num, float *a, float *b){
    int i;
    __m256 accum = _mm256_setzero_ps();

    for(i=0; i<num; i+=8){

        //Load the needed vectors
        __m256 coeff = _mm256_loadu_ps(a + i);
        __m256 val   = _mm256_loadu_ps(b + i);

        //Multiply and acumulate
        accum = _mm256_add_ps(accum, _mm256_mul_ps(coeff, val));
    }
    return accum;
}

/*
 * Complex horizontal addition
 */
inline __m128 sse_hadd_C(__m128 in) {
    __m128 accum = _mm_shuffle_ps(in, in, 0b11011000);
    return _mm_hadd_ps(accum, accum);
}

inline __m128 avx_hadd_C(__m256 in) {
    __m256 accum    = _mm256_permute_ps(in, _MM_SHUFFLE(3, 1, 2, 0));
    __m128 accum_hi = _mm256_extractf128_ps(accum, 1);
    __m128 accum_lo = _mm256_extractf128_ps(accum, 0);
    __m128 added    = _mm_hadd_ps(accum_lo, accum_hi);
    added           = _mm_permute_ps(added, _MM_SHUFFLE(3, 1, 2, 0));
    added           = _mm_hadd_ps(added, added);
    return added;
}

/*
 * Complex dot products
 */

inline __m128 sse_dotprod_C(int num, float *coeffs, float *startPtr){
    int i;
    __m128 accum1 = _mm_setzero_ps();
    __m128 accum2 = _mm_setzero_ps();

    for(i=0; i<num; i+=4){

        //Load the needed vectors
        __m128 coeff  = _mm_loadu_ps(coeffs + i);
        __m128 coeff1 = _mm_shuffle_ps(coeff, coeff, 0x50);
        __m128 coeff2 = _mm_shuffle_ps(coeff, coeff, 0xfa);
        __m128 val1   = _mm_loadu_ps(startPtr + 2 * i);
        __m128 val2   = _mm_loadu_ps(startPtr + 2 * i + 4);

        //Multiply and acumulate
        accum1 = _mm_add_ps(accum1, _mm_mul_ps(coeff1, val1));
        accum2 = _mm_add_ps(accum2, _mm_mul_ps(coeff2, val2));
    }
    __m128 accum = _mm_add_ps(accum1, accum2);
    return accum;
}

/*
 * Real symmetric dot products
 */

inline __m128 sse_sym_dotprod_R(int num, float *a, float *b){
    int i;
    __m128 accum = _mm_setzero_ps();

    float *startPtr = b;
    float *endPtr = b + num * 2 - 4;
    for(i=0; i<num; i+=4){

        //Load the needed vectors
        __m128 coeff = _mm_loadu_ps(a + i);
        __m128 val1  = _mm_loadu_ps(startPtr + i);
        __m128 val2  = _mm_loadu_ps(endPtr   - i);
        val2         = _mm_permute_ps(val2, _MM_SHUFFLE(0, 1, 2, 3));

        //Multiply and acumulate
        accum = _mm_add_ps(accum, _mm_mul_ps(coeff, _mm_add_ps(val1, val2)));
    }

    return accum;
}

inline __m256 avx_sym_dotprod_R(int num, float *a, float *b){
    int i;
    __m256 accum = _mm256_setzero_ps();

    float *startPtr = b;
    float *endPtr   = b + num * 2 - 8;
    for(i=0; i<num; i+=8){

        //Load the needed vectors
        __m256 coeff = _mm256_loadu_ps(a + i);
        __m256 val1  = _mm256_loadu_ps(startPtr + i);
        __m256 val2  = _mm256_loadu_ps(endPtr   - i);
        val2         = _mm256_permute2f128_ps(val2, val2, 0x01);
        val2         = _mm256_permute_ps(val2, _MM_SHUFFLE(0, 1, 2, 3));

        //Multiply and acumulate
        accum = _mm256_add_ps(accum, _mm256_mul_ps(coeff, _mm256_add_ps(val1, val2)));
    }

    return accum;
}

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

        _mm_store_ss(outBuf + i, accum);
        accum = _mm_shuffle_ps(accum, accum, 0b00000001);
        _mm_store_ss(outBuf + i + 1, accum);
    }
}

void filterSSERC2(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num*2; i+=2){
        float *startPtr = inBuf + i;
        __m128 accum = sse_dotprod_C(numCoeffs, coeffs, startPtr);
        accum        = sse_hadd_C(accum);

        _mm_store_ss(outBuf + i, accum);
        accum = _mm_shuffle_ps(accum, accum, 0b00000001);
        _mm_store_ss(outBuf + i + 1, accum);
    }
}

void filterAVXRC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i;
    for(i=0; i<num*2; i+=2){
        float *startPtr = inBuf + i;
        __m256 accum  = avx_dotprod_R(numCoeffs, coeffs, startPtr);
        __m128 accum2 = avx_hadd_C(accum);

        _mm_store_ss(outBuf + i, accum2);
        accum2 = _mm_shuffle_ps(accum2, accum2, 0b00000001);
        _mm_store_ss(outBuf + i + 1, accum2);
    }
}

