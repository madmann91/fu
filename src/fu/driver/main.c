#include "fu/driver/options.h"
#include "fu/driver/driver.h"
#include "fu/core/log.h"
#include "fu/core/utils.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    FormatState state = new_format_state("    ", !is_color_supported(stderr));
    Log log = new_log(&state);
    bool status = true;

    Options options = default_options;
    if (!parse_options(&argc, argv, &options, &log)) {
        status = false;
        goto exit;
    }

    for (int i = 1; i < argc && status; ++i)
        status &= compile_file(argv[i], &options, &log);

exit:
    write_format_state(&state, stderr);
    free_format_state(&state);
    free_log(&log);
    return status ? EXIT_SUCCESS : EXIT_FAILURE;
}
