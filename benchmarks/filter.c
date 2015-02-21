#include <stdio.h>
#include <stdint.h>
#include <x86intrin.h>

/*
 * Filtering
 */

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

/*
 * Symmetric versions
 */

void filterSSESymmetricRR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num; i++){
        __m128 accum = _mm_setzero_ps();

        float *startPtr = inBuf + i;
        float *endPtr = inBuf + i + numCoeffs * 2 - 4;
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

void filterSSESymmetricRC(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num*2; i+=2){
        __m128 accum1 = _mm_setzero_ps();
        __m128 accum2 = _mm_setzero_ps();

        float *startPtr = inBuf + i;
        float *endPtr = inBuf + i + numCoeffs * 2 - 4;
        for(j=0; j<numCoeffs; j+=4){

            //Load the needed vectors
            __m128 coeff = _mm_loadu_ps(coeffs   + j);
            __m128 coeff1 = _mm_shuffle_ps(coeff, coeff, 0x50);
            __m128 coeff2 = _mm_shuffle_ps(coeff, coeff, 0xfa);

            __m128 val1  = _mm_loadu_ps(startPtr + 2*j);
            __m128 val2  = _mm_loadu_ps(startPtr + 2*j + 4);
            __m128 val3  = _mm_loadu_ps(endPtr   - 2*j - 4);
            val3         = _mm_shuffle_ps(val3, val3, _MM_SHUFFLE(1, 0, 3, 2));
            __m128 val4  = _mm_loadu_ps(endPtr   - 2*j);
            val4         = _mm_shuffle_ps(val4, val4, _MM_SHUFFLE(1, 0, 3, 2));

            //Multiply and acumulate
            accum1 = _mm_add_ps(accum1, _mm_mul_ps(coeff1, _mm_add_ps(val1, val4)));
            accum2 = _mm_add_ps(accum2, _mm_mul_ps(coeff2, _mm_add_ps(val2, val3)));
        }
        __m128 accum = _mm_add_ps(accum1, accum2);
        accum = _mm_shuffle_ps(accum, accum, 0b11011000);
        accum = _mm_hadd_ps(accum, accum);
        _mm_store_ss(outBuf + i, accum);
        accum = _mm_shuffle_ps(accum, accum, 0b00000001);
        _mm_store_ss(outBuf + i + 1, accum);
    }
}

void filterAVXSymmetricRR(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num; i++){
        __m256 accum = _mm256_setzero_ps();

        float *startPtr = inBuf + i;
        float *endPtr   = inBuf + i + numCoeffs * 2 - 8;
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
        accum = _mm_shuffle_ps(accum, accum, 0b11011000);
        accum = _mm_hadd_ps(accum, accum);
        _mm_store_ss(outBuf + i, accum);
        accum = _mm_shuffle_ps(accum, accum, 0b00000001);
        _mm_store_ss(outBuf + i + 1, accum);
    }
}

void filterSSERC2(int num, int numCoeffs, float *coeffs, float *inBuf, float *outBuf){
    int i, j;
    for(i=0; i<num*2; i+=2){
        __m128 accum1 = _mm_setzero_ps();
        __m128 accum2 = _mm_setzero_ps();

        float *startPtr = inBuf + i;
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
            __m128 val2  = _mm_loadr_ps(endPtr   - j);

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

/*
 * Conversion
 */

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

/*
 * Scaling
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

