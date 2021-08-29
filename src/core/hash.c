#include "core/hash.h"

/* Note: This is an implementation of the FNV-1a hashing function.
 * See https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
 */

uint32_t hash_init() { return 0x811c9dc5; }

uint32_t hash_pointer(uint32_t h, const void* ptr) {
    return hash_uint64(h, (ptrdiff_t)ptr);
}

uint32_t hash_uint8(uint32_t h, uint8_t x) {
    return (h ^ x) * 0x01000193;
}

uint32_t hash_uint16(uint32_t h, uint16_t x) {
    return hash_uint8(hash_uint8(h, x), x >> 8);
}

uint32_t hash_uint32(uint32_t h, uint32_t x) {
    return hash_uint16(hash_uint16(h, x), x >> 16);
}

uint32_t hash_uint64(uint32_t h, uint64_t x) {
    return hash_uint32(hash_uint32(h, x), x >> 32);
}

uint32_t hash_string(uint32_t h, const char* str) {
    while (*str) h = hash_uint8(h, *(str++));
    return h;
}

uint32_t hash_raw_bytes(uint32_t h, const void* ptr, size_t size) {
    for (size_t i = 0; i < size; ++i)
        h = hash_uint8(h, ((char*)ptr)[i]);
    return h;
}
