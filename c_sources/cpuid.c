#include <stdint.h>

void cpuid(uint32_t op, uint32_t *a, uint32_t *b, uint32_t *c, uint32_t *d){
    asm volatile(
        "cpuid;"
        : "=a"(*a), "=b"(*b), "=c"(*c), "=d"(*d)
        : "a"(op)
    );
}

void cpuid_extended(uint32_t op, uint32_t sub_op, uint32_t *a, uint32_t *b, uint32_t *c, uint32_t *d){
    asm volatile(
        "cpuid;"
        : "=a"(*a), "=b"(*b), "=c"(*c), "=d"(*d)
        : "a"(op), "c"(sub_op)
    );
}
