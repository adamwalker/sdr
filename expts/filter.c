#include <stdio.h>
#include <stdint.h>
#include <x86intrin.h>

/*
 * Scaling
 */

void scale(int num, float factor, float *in_buf, float *out_buf){
    int i;
    for(i=0; i<num; i++){
        out_buf[i] = in_buf[i] * factor;
    }
}

void scaleSSE(int num, float factor, float *in_buf, float *out_buf){
    int i;
    __m128 fac = _mm_set1_ps(factor);
    for(i=0; i<num; i+=4){
        _mm_storeu_ps(out_buf + i, _mm_mul_ps(fac, _mm_loadu_ps(in_buf + i)));
    }
}

void scaleAVX(int num, float factor, float *in_buf, float *out_buf){
    int i;
    __m256 fac = _mm256_set1_ps(factor);
    for(i=0; i<num; i+=8){
        _mm256_storeu_ps(out_buf + i, _mm256_mul_ps(fac, _mm256_loadu_ps(in_buf + i)));
    }
}

