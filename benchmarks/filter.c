#include <stdio.h>
#include <x86intrin.h>

void filterC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
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

void filterSSE(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num; i++){
        __m128 accum = _mm_setzero_ps();

        float *startPtr = inBuf + i;
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

void filterAVX(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num; i++){
        __m256 accum = _mm256_setzero_ps();

        float *startPtr = inBuf + i;
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

void decimateC(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
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

void decimateSSE(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
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

void decimateAVX(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
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

