#include <stdint.h>
#include <complex.h>

void convertArray(int size, uint8_t *in, double *out){
    int i;
    for(i=0; i<size; i++){
        out[i] = (((double) in[i]) - 128.0) / 128.0;
    }
}

void convertFFT(int samples, double complex *in, double complex *out){
    int i;
    double complex sign = 1;
    for(i=0; i<samples; i++){
        out[i] = sign * in[i];
        sign *= -1;
    }
}

void filter(int coeff_size, double complex *coeffs, int buf_size, double complex *last_buf, double complex *this_buf, double complex *out_buf){
    int i, j;
    
    //Use samples from last block
    for(i=0; i<coeff_size - 1; i++){
        double complex accum = 0;

        for(j=0; j<coeff_size - 1 - i; j++){
            accum += last_buf[i + j + buf_size - coeff_size] * coeffs[j];
        }

        for(; j<coeff_size; j++){
            accum += this_buf[i + j - coeff_size + 1] * coeffs[j];
        }

        out_buf[i] = accum;
    }

    //Use samples from this block
    for(; i<buf_size; i++){
        double complex accum = 0;

        for(j=0; j<coeff_size; j++){
            accum += this_buf[i + j - coeff_size + 1] * coeffs[j];
        };

        out_buf[i] = accum;
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

// It is assumed that size of out_buf * factor = size of in_buf
void decimate(int factor, int coeff_size, double complex *coeffs, int buf_size, double complex *last_buf, double complex *this_buf, double complex *out_buf){
    int i, j, k;
    
    //Use samples from last block
    for(i=0, k=0; i<coeff_size - 1; i+=factor, k++){
        double complex accum = 0;

        for(j=0; j<coeff_size - 1 - i; j++){
            accum += last_buf[i + j + buf_size - coeff_size] * coeffs[j];
        }

        for(; j<coeff_size; j++){
            accum += this_buf[i + j - coeff_size + 1] * coeffs[j];
        }

        out_buf[k] = accum;
    }

    //Use samples from this block
    for(; i<buf_size; i+=factor, k++){
        double complex accum = 0;

        for(j=0; j<coeff_size; j++){
            accum += this_buf[i + j - coeff_size + 1] * coeffs[j];
        };

        out_buf[k] = accum;
    }
}

void filter2_onebuf(int coeff_size, double complex *coeffs, int buf_size, double complex *in_buf, double complex *out_buf){
    int i, j;
    for(i=0; i<buf_size; i++){
        complex double accum = 0;

        for(j=0; j<coeff_size; j++){
            accum += in_buf[i + j] * coeffs[j];
        }

        out_buf[i] = accum;
    }
}

void filter2_crossbuf(int coeff_size, double complex *coeffs, int remaining_input, int buf_size, double complex *last_buf, double complex *this_buf, double complex *out_buf){
    int i, j;
    for(i=0; i<buf_size; i++){
        double complex accum = 0;

        for(j=0; j<remaining_input - i; j++){
            accum += last_buf[i + j] * coeffs[j];
        }

        for(; j<coeff_size; j++){
            accum += this_buf[i + j - remaining_input] * coeffs[j];
        }

        out_buf[i] = accum;
    }
}

void decimate2_onebuf(int factor, int coeff_size, double complex *coeffs, int buf_size, double complex *in_buf, double complex *out_buf){
    int i, j, k;
    for(i=0, k=0; k<buf_size; i+=factor, k++){
        complex double accum = 0;

        for(j=0; j<coeff_size; j++){
            accum += in_buf[i + j] * coeffs[j];
        }

        out_buf[k] = accum;
    }
}

void decimate2_crossbuf(int factor, int coeff_size, double complex *coeffs, int remaining_input, int buf_size, double complex *last_buf, double complex *this_buf, double complex *out_buf){
    int i, j, k;
    for(i=0, k=0; k<buf_size; i+=factor, k++){
        double complex accum = 0;

        for(j=0; j<remaining_input - i; j++){
            accum += last_buf[i + j] * coeffs[j];
        }

        for(; j<coeff_size; j++){
            accum += this_buf[i + j - remaining_input] * coeffs[j];
        }

        out_buf[k] = accum;
    }
}


