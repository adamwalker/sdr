/*
 * Conversion of byte IQ data from RTLSDR device to complex floats.
 * These exist because the pure Haskell implementations are slow.
 * Uses SIMD instructions for performance.
 */

#include <stdio.h>
#include <stdint.h>
#include <x86intrin.h>

/*
 * Conversion
 */

void convertC(int num, uint8_t *in, float *out){
    int i;
    for(i=0; i<num; i++){
        out[i] = ((float) in[i] - 128.0) * (1/128.0);
    }
}

void convertCSSE(int num, uint8_t *in, float *out){
    int i;
    __m128 sub = _mm_set1_ps(128.0);
    __m128 mul = _mm_set1_ps(1/128.0);
    for(i=0; i<num; i+=4){
        __m128i val  = _mm_loadu_si128((__m128i *)(in + i));
        __m128i ints = _mm_cvtepu8_epi32(val);
        __m128  cvtd = _mm_cvtepi32_ps(ints);

        __m128  res  = _mm_mul_ps(_mm_sub_ps(cvtd, sub), mul);

        _mm_storeu_ps(out + i, res);
    }
}

void convertCAVX(int num, uint8_t *in, float *out){
    int i;
    __m256 sub = _mm256_set1_ps(128.0);
    __m256 mul = _mm256_set1_ps(1/128.0);
    for(i=0; i<num; i+=8){
        __m128i val  = _mm_loadu_si128((__m128i *)(in + i));
        __m256i ints = _mm256_cvtepu8_epi32(val);
        __m256  cvtd = _mm256_cvtepi32_ps(ints);

        __m256  res  = _mm256_mul_ps(_mm256_sub_ps(cvtd, sub), mul);

        _mm256_storeu_ps(out + i, res);
    }
}

void convertCBladeRF(int num, int16_t *in, float *out){
    int i;
    for(i=0; i<num; i++){
        out[i] = (float)in[i] * (1/2048.0);
    }
}

void convertCSSEBladeRF(int num, int16_t *in, float *out){
    int i;
    __m128 mul = _mm_set1_ps(1/2048.0);
    for(i=0; i<num; i+=4){
        __m128i val  = _mm_loadu_si128((__m128i *)(in + i));
        __m128i ints = _mm_cvtepi16_epi32(val);
        __m128  cvtd = _mm_cvtepi32_ps(ints);

        __m128  res  = _mm_mul_ps(cvtd, mul);

        _mm_storeu_ps(out + i, res);
    }
}

void convertCAVXBladeRF(int num, int16_t *in, float *out){
    int i;
    __m256 mul = _mm256_set1_ps(1/2048.0);
    for(i=0; i<num; i+=8){
        __m128i val  = _mm_loadu_si128((__m128i *)(in + i));
        __m256i ints = _mm256_cvtepi16_epi32(val);
        __m256  cvtd = _mm256_cvtepi32_ps(ints);

        __m256  res  = _mm256_mul_ps(cvtd, mul);

        _mm256_storeu_ps(out + i, res);
    }
}

void convertBladeRFTransmit(int num, float *in, int16_t *out){
    int i;
    for(i=0; i<num; i++){
        float val   = in[i];        // [-1, 1)
        val         = val + 1;      // [0, 2)
        val         = val * 2048;   // [0, 4096)
        int16_t res = (int16_t)val; // [0, 4095]
        res         = res - 2048;   // [-2048, 2047]

        if(res > 2047)  res = 2047;
        if(res < -2048) res = -2048;

        out[i]      = res;
    }
}

