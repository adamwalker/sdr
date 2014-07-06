#include <stdio.h>
#include <stdint.h>
#include <complex.h>
#include <assert.h>

void convertArray(int size, uint8_t *in, double *out){
    int i;
    for(i=0; i<size; i++){
        out[i] = (((double) in[i]) - 128.0) / 128.0;
    }
}

void fmDemod(int buf_size, double complex *last_sample, double complex *in_buf, double *out_buf){
    int i;
    complex double last = *last_sample;
    for(i=0; i<buf_size; i++){
        complex double prod = in_buf[i] * conj(last);
        last = in_buf[i];
        out_buf[i] = carg(prod);
    }
}

void decimate_onebuf_c(int factor, int coeff_size, double complex *coeffs, int buf_size, double complex *in_buf, double complex *out_buf){
    int i, j, k;
    for(i=0, k=0; k<buf_size; i+=factor, k++){
        complex double accum = 0;

        for(j=0; j<coeff_size; j++){
            accum += in_buf[i + j] * coeffs[j];
        }

        out_buf[k] = accum;
    }
}

void decimate_crossbuf_c(int factor, int coeff_size, double complex *coeffs, int remaining_input, int buf_size, double complex *last_buf, double complex *this_buf, double complex *out_buf){
    int i, j, k;
    for(i=0, k=0; k<buf_size; i+=factor, k++){
        double complex accum = 0;

        for(j=0; i+j < remaining_input; j++){
            accum += last_buf[i + j] * coeffs[j];
        }

        for(; j<coeff_size; j++){
            accum += this_buf[i + j - remaining_input] * coeffs[j];
        }

        out_buf[k] = accum;
    }
}

void decimate_onebuf_r(int factor, int coeff_size, double *coeffs, int buf_size, double *in_buf, double *out_buf){
    int i, j, k;
    for(i=0, k=0; k<buf_size; i+=factor, k++){
        double accum = 0;

        for(j=0; j<coeff_size; j++){
            accum += in_buf[i + j] * coeffs[j];
        }

        out_buf[k] = accum;
    }
}

void decimate_crossbuf_r(int factor, int coeff_size, double *coeffs, int remaining_input, int buf_size, double *last_buf, double *this_buf, double *out_buf){
    int i, j, k;
    for(i=0, k=0; k<buf_size; i+=factor, k++){
        double accum = 0;

        for(j=0; i+j < remaining_input; j++){
            accum += last_buf[i + j] * coeffs[j];
        }

        for(; j<coeff_size; j++){
            accum += this_buf[i + j - remaining_input] * coeffs[j];
        }

        out_buf[k] = accum;
    }
}

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

