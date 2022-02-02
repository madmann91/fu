#ifndef FU_CORE_PRIMES_H
#define FU_CORE_PRIMES_H

#include <stddef.h>

// This sequence of primes has been designed for hash table implementations.
#define MIN_PRIME 7
#define MAX_PRIME 1048583
#define PRIMES(f) \
    f(MIN_PRIME) \
    f(17) \
    f(31) \
    f(67) \
    f(257) \
    f(1031) \
    f(4093) \
    f(8191) \
    f(16381) \
    f(32381) \
    f(65539) \
    f(131071) \
    f(262147) \
    f(524287) \
    f(MAX_PRIME)

static const size_t primes[] = {
#define f(x) x,
PRIMES(f)
#undef f
};

static const size_t prime_count = sizeof(primes) / sizeof(primes[0]);

// Returns the prime that is strictly greater than the given value.
// If there is no such prime in the list, returns MAX_PRIME.
static inline size_t next_prime(size_t i) {
    size_t j = 0, k = prime_count;
    while (j < k) {
        size_t m = (j + k) / 2;
        size_t p = primes[m];
        if (p <= i)
            j = m + 1;
        else
            k = m;
    }
    return primes[k >= prime_count ? prime_count - 1 : k];
}

// Returns the modulus of a number i by a prime p.
static inline size_t mod_prime(size_t i, size_t p) {
    switch (p) {
#define f(x) case x: return i % x;
    PRIMES(f)
#undef f
        default: return i % p;
    }
}

#endif
