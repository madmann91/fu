#ifndef FU_CORE_HASH_H
#define FU_CORE_HASH_H

#include <stdint.h>
#include <stddef.h>

typedef uint32_t HashCode;

HashCode hash_init();
HashCode hash_ptr(HashCode, const void*);
HashCode hash_uint8(HashCode, uint8_t);
HashCode hash_uint16(HashCode, uint16_t);
HashCode hash_uint32(HashCode, uint32_t);
HashCode hash_uint64(HashCode, uint64_t);
HashCode hash_str(HashCode, const char*);
HashCode hash_raw_bytes(HashCode, const void*, size_t);

#endif
