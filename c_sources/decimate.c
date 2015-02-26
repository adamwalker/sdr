#include <stdio.h>
#include <stdint.h>
#include <x86intrin.h>

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
    int i, j, k;
    for(i=0, k=0; i<num; i++, k+=factor){
        __m128 accum = _mm_setzero_ps();

        float *startPtr = inBuf + k;
        for(j=0; j<numCoeffs; j+=4){

            //Load the needed vectors
            __m128 coeff = _mm_loadu_ps(coeffs + j);
            __m128 val   = _mm_loadu_ps(startPtr + j);

            //Multiply and acumulate
            accum = _mm_add_ps(accum, _mm_mul_ps(coeff, val));
        }
        accum = _mm_hadd_ps(accum, accum);
        accum = _mm_hadd_ps(accum, accum);
        _mm_store_ss(outBuf + i, accum);
    }
}

void decimateAVXRR(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num; i++, k+=factor){
        __m256 accum = _mm256_setzero_ps();

        float *startPtr = inBuf + k;
        for(j=0; j<numCoeffs; j+=8){

            //Load the needed vectors
            __m256 coeff = _mm256_loadu_ps(coeffs + j);
            __m256 val   = _mm256_loadu_ps(startPtr + j);

            //Multiply and acumulate
            accum = _mm256_add_ps(accum, _mm256_mul_ps(coeff, val));
        }

        __m128 res1 = _mm256_extractf128_ps(accum, 0);
        __m128 res2 = _mm256_extractf128_ps(accum, 1);

        res1 = _mm_hadd_ps(res1, res1);
        res1 = _mm_hadd_ps(res1, res1);

        res2 = _mm_hadd_ps(res2, res2);
        res2 = _mm_hadd_ps(res2, res2);

        _mm_store_ss(outBuf + i, _mm_add_ss(res1, res2));
    }
}

/*
 * Symmetric versions
 */
void decimateSSESymmetricRR(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num; i++, k+=factor){
        __m128 accum = _mm_setzero_ps();

        float *startPtr = inBuf + k;
        float *endPtr = inBuf + k + numCoeffs * 2 - 4;
        for(j=0; j<numCoeffs; j+=4){

            //Load the needed vectors
            __m128 coeff = _mm_loadu_ps(coeffs   + j);
            __m128 val1  = _mm_loadu_ps(startPtr + j);
            __m128 val2  = _mm_loadu_ps(endPtr   - j);
            val2         = _mm_permute_ps(val2, _MM_SHUFFLE(0, 1, 2, 3));

            //Multiply and acumulate
            accum = _mm_add_ps(accum, _mm_mul_ps(coeff, _mm_add_ps(val1, val2)));
        }
        accum = _mm_hadd_ps(accum, accum);
        accum = _mm_hadd_ps(accum, accum);
        _mm_store_ss(outBuf + i, accum);
    }
}
void decimateAVXSymmetricRR(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num; i++, k+=factor){
        __m256 accum = _mm256_setzero_ps();

        float *startPtr = inBuf + k;
        float *endPtr   = inBuf + k + numCoeffs * 2 - 8;
        for(j=0; j<numCoeffs; j+=8){

            //Load the needed vectors
            __m256 coeff = _mm256_loadu_ps(coeffs   + j);
            __m256 val1  = _mm256_loadu_ps(startPtr + j);
            __m256 val2  = _mm256_loadu_ps(endPtr   - j);
            val2         = _mm256_permute2f128_ps(val2, val2, 0x01);
            val2         = _mm256_permute_ps(val2, _MM_SHUFFLE(0, 1, 2, 3));

            //Multiply and acumulate
            accum = _mm256_add_ps(accum, _mm256_mul_ps(coeff, _mm256_add_ps(val1, val2)));
        }

        __m128 res1 = _mm256_extractf128_ps(accum, 0);
        __m128 res2 = _mm256_extractf128_ps(accum, 1);

        res1 = _mm_hadd_ps(res1, res1);
        res1 = _mm_hadd_ps(res1, res1);

        res2 = _mm_hadd_ps(res2, res2);
        res2 = _mm_hadd_ps(res2, res2);

        _mm_store_ss(outBuf + i, _mm_add_ss(res1, res2));
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
    int i, j, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        __m128 accum = _mm_setzero_ps();

        float *startPtr = inBuf + k;
        for(j=0; j<numCoeffs; j+=4){

            //Load the needed vectors
            __m128 coeff = _mm_loadu_ps(coeffs + j);
            __m128 val   = _mm_loadu_ps(startPtr + j);

            //Multiply and acumulate
            accum = _mm_add_ps(accum, _mm_mul_ps(coeff, val));
        }
        accum = _mm_shuffle_ps(accum, accum, 0b11011000);
        accum = _mm_hadd_ps(accum, accum);
        _mm_store_ss(outBuf + i, accum);
        accum = _mm_shuffle_ps(accum, accum, 0b00000001);
        _mm_store_ss(outBuf + i + 1, accum);
    }
}

void decimateSSERC2(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        __m128 accum1 = _mm_setzero_ps();
        __m128 accum2 = _mm_setzero_ps();

        float *startPtr = inBuf + k;
        for(j=0; j<numCoeffs; j+=4){

            //Load the needed vectors
            __m128 coeff  = _mm_loadu_ps(coeffs + j);
            __m128 coeff1 = _mm_shuffle_ps(coeff, coeff, 0x50);
            __m128 coeff2 = _mm_shuffle_ps(coeff, coeff, 0xfa);
            __m128 val1   = _mm_loadu_ps(startPtr + 2 * j);
            __m128 val2   = _mm_loadu_ps(startPtr + 2 * j + 4);

            //Multiply and acumulate
            accum1 = _mm_add_ps(accum1, _mm_mul_ps(coeff1, val1));
            accum2 = _mm_add_ps(accum2, _mm_mul_ps(coeff2, val2));
        }
        __m128 accum = _mm_add_ps(accum1, accum2);
        accum = _mm_shuffle_ps(accum, accum, 0b11011000);
        accum = _mm_hadd_ps(accum, accum);
        _mm_store_ss(outBuf + i, accum);
        accum = _mm_shuffle_ps(accum, accum, 0b00000001);
        _mm_store_ss(outBuf + i + 1, accum);
    }
}


void decimateAVXRC(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        __m256 accum = _mm256_setzero_ps();

        float *startPtr = inBuf + k;
        for(j=0; j<numCoeffs; j+=8){

            //Load the needed vectors
            __m256 coeff = _mm256_loadu_ps(coeffs + j);
            __m256 val   = _mm256_loadu_ps(startPtr + j);

            //Multiply and acumulate
            accum = _mm256_add_ps(accum, _mm256_mul_ps(coeff, val));
        }
        accum           = _mm256_permute_ps(accum, _MM_SHUFFLE(3, 1, 2, 0));
        __m128 accum_hi = _mm256_extractf128_ps(accum, 1);
        __m128 accum_lo = _mm256_extractf128_ps(accum, 0);
        __m128 added    = _mm_hadd_ps(accum_lo, accum_hi);
        added           = _mm_permute_ps(added, _MM_SHUFFLE(3, 1, 2, 0));
        added           = _mm_hadd_ps(added, added);

        _mm_store_ss(outBuf + i, added);
        added = _mm_shuffle_ps(added, added, 0b00000001);
        _mm_store_ss(outBuf + i + 1, added);
    }
}

