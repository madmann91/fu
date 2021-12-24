#include "core/utils.h"

#include <ctype.h>
#include <stdlib.h>
#include <errno.h>

static bool convert_str_to_char_ord(const char* ptr, int base, char* res) {
    unsigned int ord = strtoul(ptr, NULL, base);
    *res = ord;
    return ord <= 255 && !errno;
}

bool convert_escape_seq(const char* ptr, size_t n, char* res) {
    if (n == 0) return 0;
    if (ptr[0] == '\\') {
        if (n <= 1)
            return 0;
        switch (ptr[1]) {
            case 'n': *res = '\n'; return n == 2;
            case 't': *res = '\t'; return n == 2;
            case 'v': *res = '\v'; return n == 2;
            case 'r': *res = '\r'; return n == 2;
            case 'a': *res = '\a'; return n == 2;
            case 'b': *res = '\b'; return n == 2;
            case 'x':
                if (n <= 2)
                    return false;
                return convert_str_to_char_ord(ptr + 2, 16, res);
            default:
                if (isdigit(ptr[1]))
                    return convert_str_to_char_ord(ptr + 1, 8, res);
                return false;
        }
    }
    *res = ptr[0];
    return n == 1;
}
