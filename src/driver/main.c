#include "core/format.h"

int main(int argc, char** argv) {
    struct format_state state = { .tab = "    " };
    format(&state, "Hello $>\n$0$$%% %1$s$2$<\n", (union format_arg[]) {
        { .style = (struct format_style) { .style = STYLE_ITALIC, .color = COLOR_RED } },
        { .s = "World" },
        { .style = (struct format_style) { .style = STYLE_NORMAL, .color = COLOR_NORMAL } }
    });
    print_format_bufs(state.buf, stdout);
    free_format_bufs(state.buf);
    return 0;
}
