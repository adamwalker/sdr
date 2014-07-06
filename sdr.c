#include <stdio.h>
#include <stdint.h>
#include <complex.h>
#include <assert.h>

/*
    decimation + r = interpolation * k + r'
    r' is the last r
    for k whole number and positive
    for r positive and less than k

    so
    decimation - r' = interpolation * k - r
    decimation - r' + interpolation = interpolation * k + (interpolation - r)
    decimation - r' = interpolation * k + (interpolation - r)
    decimation - r' - 1 = interpolation * k + ((interpolation - 1) - r)

    to calculate:
    (decimation - r') `mod` interpolation
    decimation > interpolation, r' < interpolation => r' < decimation
*/

int resample_onebuf_c(int interpolation, int decimation, int coeff_size, double complex *coeffs, int filter_offset, int buf_size, double complex *in_buf, double complex *out_buf){
    int j, k, l;
    int input_offset = 0;
    for(k=0; k<buf_size; k++) {
        complex double accum = 0;

        for(l=0, j=filter_offset; j<coeff_size; l++, j+=interpolation) {
            accum += in_buf[input_offset + l] * coeffs[j];
        }

        int filter_offset_new  = interpolation - 1 - (decimation - filter_offset - 1) % interpolation;
        input_offset          += (decimation - filter_offset - 1) / interpolation + 1; 
        filter_offset          = filter_offset_new;

        out_buf[k] = accum;
    }

    return filter_offset;
}

int resample_crossbuf_c(int interpolation, int decimation, int coeff_size, double complex *coeffs, int filter_offset, int remaining_input, int buf_size, double complex *last_buf, double complex *this_buf, double complex *out_buf){

    int j, k, l;
    int input_offset = 0;
    for(k=0; k<buf_size; k++) {
        complex double accum = 0;

        for(l=0, j=filter_offset; input_offset + l < remaining_input; l++, j+=interpolation) {
            accum += last_buf[input_offset + l] * coeffs[j];
        }

        for(; j<coeff_size; j+=interpolation) {
            accum += this_buf[input_offset + l - remaining_input] * coeffs[j];
        }

        int filter_offset_new = interpolation - 1 - (decimation - filter_offset - 1) % interpolation;
        input_offset         += (decimation - filter_offset - 1) / interpolation + 1;
        filter_offset         = filter_offset_new;

        out_buf[k] = accum;
    }
    return filter_offset;
}

int resample_onebuf_r(int interpolation, int decimation, int coeff_size, double *coeffs, int filter_offset, int buf_size, double *in_buf, double *out_buf){
    int j, k, l;
    int input_offset = 0;
    for(k=0; k<buf_size; k++) {
        double accum = 0;

        for(l=0, j=filter_offset; j<coeff_size; l++, j+=interpolation) {
            accum += in_buf[input_offset + l] * coeffs[j];
        }

        int filter_offset_new  = interpolation - 1 - (decimation - filter_offset - 1) % interpolation;
        input_offset          += (decimation - filter_offset - 1) / interpolation + 1; 
        filter_offset          = filter_offset_new;

        out_buf[k] = accum;
    }

    return filter_offset;
}

int resample_crossbuf_r(int interpolation, int decimation, int coeff_size, double *coeffs, int filter_offset, int remaining_input, int buf_size, double *last_buf, double *this_buf, double *out_buf){

    int j, k, l;
    int input_offset = 0;
    for(k=0; k<buf_size; k++) {
        double accum = 0;

        for(l=0, j=filter_offset; input_offset + l < remaining_input; l++, j+=interpolation) {
            assert(j <= coeff_size);
            accum += last_buf[input_offset + l] * coeffs[j];
        }

        for(; j<coeff_size; l++, j+=interpolation) {
            accum += this_buf[input_offset + l - remaining_input] * coeffs[j];
        }

        int filter_offset_new = interpolation - 1 - (decimation - filter_offset - 1) % interpolation;
        input_offset         += (decimation - filter_offset - 1) / interpolation + 1;
        filter_offset         = filter_offset_new;

        out_buf[k] = accum;
    }
    return filter_offset;
}

