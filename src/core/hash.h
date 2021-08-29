#ifndef FU_CORE_HASH_H
#define FU_CORE_HASH_H

#include <stdint.h>
#include <stddef.h>

uint32_t hash_init();
uint32_t hash_pointer(uint32_t, const void*);
uint32_t hash_uint8(uint32_t, uint8_t);
uint32_t hash_uint16(uint32_t, uint16_t);
uint32_t hash_uint32(uint32_t, uint32_t);
uint32_t hash_uint64(uint32_t, uint64_t);
uint32_t hash_string(uint32_t, const char*);
uint32_t hash_raw_bytes(uint32_t, const void*, size_t);

#endif
