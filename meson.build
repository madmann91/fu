project('fu', 'c', version: '0.1')
libfu = library('libfu',
  sources: [
    'src/fu/core/alloc.c',
    'src/fu/core/format.c',
    'src/fu/core/hash.c',
    'src/fu/core/hash_table.c',
    'src/fu/core/log.c',
    'src/fu/core/mem_pool.c',
    'src/fu/core/str_pool.c',
    'src/fu/core/utils.c',
    'src/fu/lang/ast.c',
    'src/fu/lang/bind.c',
    'src/fu/lang/check.c',
    'src/fu/lang/lexer.c',
    'src/fu/lang/parser.c',
    'src/fu/lang/types.c',
    'src/fu/lang/type_table.c',
    'src/fu/driver/driver.c',
    'src/fu/driver/options.c'],
  include_directories: 'src',
  name_prefix: '')

fu = executable('fu',
  sources: ['src/fu/driver/main.c'],
  include_directories: 'src',
  link_with: libfu)

root = meson.source_root()
test('parser-pass-enums',     fu, workdir: root, args: ['--print-ast', 'test/parser/pass/enums.fu'])
test('parser-pass-structs',   fu, workdir: root, args: ['--print-ast', 'test/parser/pass/structs.fu'])
test('parser-pass-literals',  fu, workdir: root, args: ['--print-ast', 'test/parser/pass/literals.fu'])
test('parser-pass-functions', fu, workdir: root, args: ['--print-ast', 'test/parser/pass/functions.fu'])
test('parser-pass-exprs',     fu, workdir: root, args: ['--print-ast', 'test/parser/pass/exprs.fu'])
test('parser-pass-loops',     fu, workdir: root, args: ['--print-ast', 'test/parser/pass/loops.fu'])
test('parser-pass-attrs',     fu, workdir: root, args: ['--print-ast', 'test/parser/pass/attrs.fu'])