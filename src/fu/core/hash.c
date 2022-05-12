#include "fu/core/hash.h"

/*
 * Note: This is an implementation of the FNV-1a hashing function.
 * See https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
 */

HashCode hash_init() { return UINT32_C(0x811c9dc5); }

HashCode hash_ptr(HashCode h, const void* ptr) {
    return hash_uint64(h, (ptrdiff_t)ptr);
}

HashCode hash_uint8(HashCode h, uint8_t x) {
    return (h ^ x) * 0x01000193;
}

HashCode hash_uint16(HashCode h, uint16_t x) {
    return hash_uint8(hash_uint8(h, x), x >> 8);
}

HashCode hash_uint32(HashCode h, HashCode x) {
    return hash_uint16(hash_uint16(h, x), x >> 16);
}

HashCode hash_uint64(HashCode h, uint64_t x) {
    return hash_uint32(hash_uint32(h, x), x >> 32);
}

HashCode hash_str(HashCode h, const char* str) {
    while (*str) h = hash_uint8(h, *(str++));
    return h;
}

HashCode hash_raw_bytes(HashCode h, const void* ptr, size_t size) {
    for (size_t i = 0; i < size; ++i)
        h = hash_uint8(h, ((char*)ptr)[i]);
    return h;
}
