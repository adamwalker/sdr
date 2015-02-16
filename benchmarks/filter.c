#include <stdio.h>
#include <stdint.h>
#include <x86intrin.h>

/*
 * Filtering
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

void filterRC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num*2; i+=2){
        float real = 0;
        float imag = 0;
        float *startPtr = inBuf + i;
        for(j=0; j<numCoeffs; j++){
            real += startPtr[j] * coeffs[j];
            imag += startPtr[j+1] * coeffs[j];
        }
        outBuf[i] = real;
        outBuf[i+1] = imag;
    }
}

void filterSSERR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
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

void filterSSESymmetricRR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num; i++){
        __m128 accum = _mm_setzero_ps();

        float *startPtr = inBuf + i;
        float *endPtr = inBuf + i + numCoeffs - 4;
        for(j=0; j<numCoeffs; j+=4){

            //Load the needed vectors
            __m128 coeff = _mm_loadu_ps(coeffs   + j);
            __m128 val1  = _mm_loadu_ps(startPtr + j);
            __m128 val2  = _mm_loadr_ps(endPtr   - j);

            //Multiply and acumulate
            accum = _mm_add_ps(accum, _mm_mul_ps(coeff, _mm_add_ps(val1, val2)));
        }
        accum = _mm_hadd_ps(accum, accum);
        accum = _mm_hadd_ps(accum, accum);
        _mm_store_ss(outBuf + i, accum);
    }
}

void filterSSERC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num*2; i+=2){
        __m128 accum = _mm_setzero_ps();

        float *startPtr = inBuf + i;
        for(j=0; j<numCoeffs; j+=4){

            //Load the needed vectors
            __m128 coeff = _mm_loadu_ps(coeffs + j);
            __m128 val   = _mm_loadu_ps(startPtr + j);

            //Multiply and acumulate
            accum = _mm_add_ps(accum, _mm_mul_ps(coeff, val));
        }
        outBuf[i]   = _mm_extract_epi32(accum, 0) + _mm_extract_epi32(accum, 2);
        outBuf[i+1] = _mm_extract_epi32(accum, 1) + _mm_extract_epi32(accum, 3);
    }
}

void filterAVXSymmetricRR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num; i++){
        __m256 accum = _mm256_setzero_ps();

        float *startPtr = inBuf + i;
        float *endPtr   = inBuf + i + numCoeffs - 8;
        for(j=0; j<numCoeffs; j+=8){

            //Load the needed vectors
            __m256 coeff = _mm256_loadu_ps(coeffs   + j);
            __m256 val1  = _mm256_loadu_ps(startPtr + j);
            __m256 val2  = _mm256_loadu_ps(endPtr   - j);

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

void filterAVXRR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
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

void filterAVXRC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num*2; i+=2){
        __m256 accum = _mm256_setzero_ps();

        float *startPtr = inBuf + i;
        for(j=0; j<numCoeffs; j+=8){

            //Load the needed vectors
            __m256 coeff = _mm256_loadu_ps(coeffs + j);
            __m256 val   = _mm256_loadu_ps(startPtr + j);

            //Multiply and acumulate
            accum = _mm256_add_ps(accum, _mm256_mul_ps(coeff, val));
        }
        outBuf[i]   = _mm256_extract_epi32(accum, 0) + _mm256_extract_epi32(accum, 2) + _mm256_extract_epi32(accum, 4) + _mm256_extract_epi32(accum, 6);
        outBuf[i+1] = _mm256_extract_epi32(accum, 1) + _mm256_extract_epi32(accum, 3) + _mm256_extract_epi32(accum, 5) + _mm256_extract_epi32(accum, 7);
    }
}

/*
 * Decimation
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

void decimateRC(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor*2){
        float real = 0;
        float imag = 0;
        float *startPtr = inBuf + k;
        for(j=0; j<numCoeffs; j++){
            real += startPtr[j] * coeffs[j];
            imag += startPtr[j+1] * coeffs[j];
        }
        outBuf[i] = real;
        outBuf[i+1] = imag;
    }
}

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

void decimateSSERC(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor){
        __m128 accum = _mm_setzero_ps();

        float *startPtr = inBuf + k;
        for(j=0; j<numCoeffs; j+=4){

            //Load the needed vectors
            __m128 coeff = _mm_loadu_ps(coeffs + j);
            __m128 val   = _mm_loadu_ps(startPtr + j);

            //Multiply and acumulate
            accum = _mm_add_ps(accum, _mm_mul_ps(coeff, val));
        }
        outBuf[i]   = _mm_extract_epi32(accum, 0) + _mm_extract_epi32(accum, 2);
        outBuf[i+1] = _mm_extract_epi32(accum, 1) + _mm_extract_epi32(accum, 3);
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

void decimateAVXRC(int num, int factor, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j, k;
    for(i=0, k=0; i<num*2; i+=2, k+=factor){
        __m256 accum = _mm256_setzero_ps();

        float *startPtr = inBuf + k;
        for(j=0; j<numCoeffs; j+=8){

            //Load the needed vectors
            __m256 coeff = _mm256_loadu_ps(coeffs + j);
            __m256 val   = _mm256_loadu_ps(startPtr + j);

            //Multiply and acumulate
            accum = _mm256_add_ps(accum, _mm256_mul_ps(coeff, val));
        }
        outBuf[i]   = _mm256_extract_epi32(accum, 0) + _mm256_extract_epi32(accum, 2) + _mm256_extract_epi32(accum, 4) + _mm256_extract_epi32(accum, 6);
        outBuf[i+1] = _mm256_extract_epi32(accum, 1) + _mm256_extract_epi32(accum, 3) + _mm256_extract_epi32(accum, 5) + _mm256_extract_epi32(accum, 7);
    }
}

void convertC(int num, uint8_t *in, float *out){
    int i;
    for(i=0; i<num; i++){
        out[i] = (float) in[i];
    }
}

/*
void convertSSE(int num, uint8_t *in, float *out){
    int i;
    for(i=0; i<num; i+=4){
        //__m64  dat   = _mm64_loadu_ps(in + i);
        __m128 cvted = _mm_cvtpu8_ps(dat);
        _mm_storeu_ps(out + i, cvted);
    }
}
*/

void scale(int num, float factor, float *buf){
    int i;
    for(i=0; i<num; i++){
        buf[i] *= factor;
    }
}

void scaleSSE(int num, float factor, float *buf){
    int i;
    __m128 fac = _mm_set1_ps(factor);
    for(i=0; i<num; i+=4){
        _mm_store_ps(buf + i, _mm_mul_ps(fac, _mm_loadu_ps(buf + i)));
    }
}

void scaleAVX(int num, float factor, float *buf){
    int i;
    __m256 fac = _mm256_set1_ps(factor);
    for(i=0; i<num; i+=8){
        _mm256_storeu_ps(buf + i, _mm256_mul_ps(fac, _mm256_loadu_ps(buf + i)));
    }
}

