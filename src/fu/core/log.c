#include "fu/core/log.h"
#include "fu/core/utils.h"
#include "fu/core/hash.h"

#include <string.h>
#include <stdio.h>

#define DEFAULT_FILE_CACHE_CAPACITY 8
#define LINE_SEARCH_RANGE 64
#define LINE_BUF_CAPACITY 64

typedef enum {
    LOG_ERROR,
    LOG_WARNING,
    LOG_NOTE
} LogMsgType;

typedef struct {
    const char* file_name;
    FILE* file;
} FileEntry;

Log new_log(FormatState* state) {
    return (Log) {
        .file_cache = new_hash_table(DEFAULT_FILE_CACHE_CAPACITY, sizeof(FileEntry)),
        .show_diagnostics = true,
        .state = state
    };
}

void free_log(Log* log) {
    FileEntry* entries = log->file_cache.elems;
    for (size_t i = 0; i < log->file_cache.capacity; ++i) {
        if (!is_bucket_occupied(&log->file_cache, i))
            continue;
        fclose(entries[i].file);
    }
    free_hash_table(&log->file_cache);
}

static bool compare_file_entries(const void* left, const void* right) {
    return !strcmp(((FileEntry*)left)->file_name, ((FileEntry*)right)->file_name);
}

static FILE* get_cached_file(Log* log, const char* file_name) {
    uint32_t hash = hash_str(hash_init(), file_name);
    FileEntry* entry = find_in_hash_table(&log->file_cache,
        &(FileEntry) { .file_name = file_name }, hash,
        sizeof(FileEntry), compare_file_entries);
    if (entry)
        return entry->file;
    FILE* file = fopen(file_name, "rb");
    if (!file)
        return NULL;
    must_succeed(insert_in_hash_table(&log->file_cache,
        &(FileEntry) { .file_name = file_name, .file = file }, hash,
        sizeof(FileEntry), compare_file_entries));
    return file;
}

static size_t find_line_begin(FILE* file, size_t offset) {
    char data[LINE_SEARCH_RANGE];
    while (offset > 0) {
        size_t range = offset > LINE_SEARCH_RANGE ? LINE_SEARCH_RANGE : offset;
        offset -= range;
        fseek(file, offset, SEEK_SET);
        fread(data, 1, range, file);
        for (size_t i = range; i --> 0;) {
            if (data[i] == '\n')
                return offset + i + 1;
        }
    }
    return 0;
}

static size_t find_line_end(FILE* file, size_t offset) {
    char data[LINE_SEARCH_RANGE];
    fseek(file, offset, SEEK_SET);
    while (true) {
        size_t read_count = fread(data, 1, LINE_SEARCH_RANGE, file);
        for (size_t i = 0; i < read_count; ++i) {
            if (data[i] == '\n')
                return offset + i;
        }
        offset += read_count;
        if (read_count < LINE_SEARCH_RANGE)
            return offset;
    }
}

static size_t count_digits(size_t i) {
    size_t n = 1;
    while (i > 10) {
        i /= 10;
        n++;
    }
    return n;
}

static void print_empty_space(FormatState* state, size_t line_number_len) {
    if (line_number_len >= 4)
        format(state, "    ", NULL), line_number_len -= 4;
    for (size_t i = 0; i < line_number_len; ++i)
        format(state, " ", NULL);
}

static void print_file_line(FormatState* state, size_t line_begin, FILE* file) {
    char line_buf[LINE_BUF_CAPACITY];
    fseek(file, line_begin, SEEK_SET);
    while (true) {
        if (!fgets(line_buf, LINE_BUF_CAPACITY, file))
            break;
        char* end_line = strrchr(line_buf, '\n');
        if (end_line)
            *end_line = '\0';
        format(state, "{s}", (FormatArg[]) { { .s = line_buf } });
        if (end_line)
            break;
    }
    format(state, "\n", NULL);
}

static void print_line_markers(FormatState* state, FormatStyle style, size_t count) {
    format(state, "{$}", (FormatArg[]) { { .style = style } });
    for (size_t i = 0; i < count; ++i)
        format(state, "^", NULL);
    format(state, "{$}\n", (FormatArg[]) { { .style = reset_style } });
}

static bool is_multiline_file_loc(const FileLoc* file_loc) {
    return file_loc->begin.row != file_loc->end.row;
}

static void print_diagnostic(Log* log, FormatStyle style, const FileLoc* file_loc) {
    FILE* file = get_cached_file(log, file_loc->file_name);
    if (!file)
        return;

    size_t line_number_len = count_digits(file_loc->end.row);
    size_t begin_offset = find_line_begin(file, file_loc->begin.byte_offset);

    print_empty_space(log->state, line_number_len + 1);
    format(log->state, "{$}|{$}\n", (FormatArg[]) { { .style = style }, { .style = reset_style } });

    print_empty_space(log->state, line_number_len - count_digits(file_loc->begin.row));
    format(log->state, "{$}{u64}{$} {$}|{$}", (FormatArg[]) {
        { .style = loc_style },
        { .u64 = file_loc->begin.row },
        { .style = reset_style },
        { .style = style },
        { .style = reset_style }
    });
    print_file_line(log->state, begin_offset, file);

    print_empty_space(log->state, line_number_len + 1);
    format(log->state, "{$}|{$}", (FormatArg[]) { { .style = style }, { .style = reset_style } });

    print_empty_space(log->state, file_loc->begin.byte_offset - begin_offset);
    if (is_multiline_file_loc(file_loc)) {
        print_line_markers(log->state, style,
            find_line_end(file, file_loc->begin.byte_offset) - file_loc->begin.byte_offset);

        if (file_loc->begin.row + 1 < file_loc->end.row) {
            print_empty_space(log->state, line_number_len);
            format(log->state, "{$}...{$}\n", (FormatArg[]) { { .style = loc_style }, { .style = reset_style } });
        }

        format(log->state, "{$}{u64}{$} {$}|{$}", (FormatArg[]) {
            { .style = loc_style },
            { .u64 = file_loc->end.row },
            { .style = reset_style },
            { .style = style },
            { .style = reset_style }
        });
        size_t end_offset = find_line_begin(file, file_loc->end.byte_offset);
        print_file_line(log->state, end_offset, file);

        print_empty_space(log->state, line_number_len + 1);
        format(log->state, "{$}|{$}", (FormatArg[]) { { .style = style }, { .style = reset_style } });

        print_line_markers(log->state, style, file_loc->end.byte_offset - end_offset);
    } else
        print_line_markers(log->state, style, file_loc->end.byte_offset - file_loc->begin.byte_offset);
}

static void print_msg(
    Log* log,
    LogMsgType msg_type,
    const FileLoc* file_loc,
    const char* format_str,
    const FormatArg* args)
{
    static const FormatStyle header_styles[] = {
        { STYLE_BOLD, COLOR_RED },
        { STYLE_BOLD, COLOR_YELLOW },
        { STYLE_BOLD, COLOR_BLUE }
    };
    static const char* headers[] = { "error", "warning", "note" };

    if (msg_type == LOG_ERROR) log->error_count++;
    else if (msg_type == LOG_WARNING) log->warning_count++;

    format(log->state, "{$}{s}{$}: ", (FormatArg[]) {
        { .style = header_styles[msg_type] },
        { .s = headers[msg_type] },
        { .style = reset_style } });
    format(log->state, format_str, args);
    format(log->state, "\n", NULL);
    if (file_loc && file_loc->file_name) {
        format(
            log->state,
            memcmp(&file_loc->begin, &file_loc->end, sizeof(file_loc->begin))
                ? "  in {$}{s}({u32}, {u32} -- {u32}, {u32}){$}\n"
                : "  in {$}{s}({u32}, {u32}){$}\n",
            (FormatArg[]) {
                { .style = loc_style },
                { .s = file_loc->file_name ? file_loc->file_name : "<unknown>" },
                { .u32 = file_loc->begin.row },
                { .u32 = file_loc->begin.col },
                { .u32 = file_loc->end.row },
                { .u32 = file_loc->end.col },
                { .style = reset_style }
            });
        if (log->show_diagnostics)
            print_diagnostic(log, header_styles[msg_type], file_loc);
    }
    if (msg_type != LOG_NOTE)
        format(log->state, "\n", NULL);
}

void log_error(Log* log, const FileLoc* file_loc, const char* format_str, const FormatArg* args) {
    print_msg(log, LOG_ERROR, file_loc, format_str, args);
}

void log_warning(Log* log, const FileLoc* file_loc, const char* format_str, const FormatArg* args) {
    print_msg(log, LOG_WARNING, file_loc, format_str, args);
}

void log_note(Log* log, const FileLoc* file_loc, const char* format_str, const FormatArg* args) {
    print_msg(log, LOG_NOTE, file_loc, format_str, args);
}
