#include <stdint.h>

#include "iphash.h"

unsigned int _rotr(uint32_t value, unsigned int shift) {
    if ((shift &= sizeof(value)*8 - 1) == 0)
        return value;
    return (value >> shift) | (value << (sizeof(value)*8 - shift));
}

uint32_t hash1 
    ( uint32_t o0
    , uint32_t o1
    , uint32_t o2
    , uint32_t o3
    ) {
    uint32_t r1 = _rotr(o3,o1);
    return ((r1 + ((o3 * 2803) + (o3 + o3))) + ((o2 + 2423) ^ ((o0 + o2) + (o0 + o2))));
}

uint32_t hash2
    ( uint32_t o0
    , uint32_t o1
    , uint32_t o2
    , uint32_t o3
    ) {
    return (((o1 + o2) + (o2 ^ o2)) ^ (((2753 + o1) ^ (o3 + o1)) + ((o1 ^ 2711) + ((o0 + o0) * (o2 * o0)))));
}

