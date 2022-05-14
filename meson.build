project('fu', 'c', version: '0.1')
libfu = library('libfu',
  sources: [
    'src/fu/core/format.c',
    'src/fu/core/hash.c',
    'src/fu/core/hash_table.c',
    'src/fu/core/log.c',
    'src/fu/core/mem_pool.c',
    'src/fu/core/str_pool.c',
    'src/fu/core/dyn_array.c',
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

# General tests
test('usage', fu, workdir: root, args: ['-h'], should_fail: true)
test('missing-file',          fu, workdir: root, should_fail: true, args: [])
test('invalid-option',        fu, workdir: root, should_fail: true, args: ['--flurp'])
test('missing-option-value',  fu, workdir: root, should_fail: true, args: ['--max-errors'])
test('non-existing-file',     fu, workdir: root, should_fail: true, args: ['this-file-hopefully-does-not-exist.fu'])
test('all-options-enabled',   fu, workdir: root, args: ['--max-errors', '3', '--no-color', '--print-ast', '--no-type-check', 'test/parser/pass/empty.fu'])

# Parser tests
test('parser-pass-enums',     fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/enums.fu'])
test('parser-pass-structs',   fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/structs.fu'])
test('parser-pass-literals',  fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/literals.fu'])
test('parser-pass-functions', fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/functions.fu'])
test('parser-pass-exprs',     fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/exprs.fu'])
test('parser-pass-loops',     fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/loops.fu'])
test('parser-pass-attrs',     fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/attrs.fu'])
test('parser-pass-structs',   fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/structs.fu'])
test('parser-pass-literals',  fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/literals.fu'])
test('parser-pass-functions', fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/functions.fu'])
test('parser-pass-exprs',     fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/exprs.fu'])
test('parser-pass-loops',     fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/loops.fu'])
test('parser-pass-attrs',     fu, workdir: root, args: ['--no-type-check', '--print-ast', 'test/parser/pass/attrs.fu'])

test('parser-fail-empty-type-params', fu, workdir: root, should_fail: true, args: ['--no-type-check', '--print-ast', 'test/parser/fail/empty_enum.fu'])
test('parser-fail-empty-enum',        fu, workdir: root, should_fail: true, args: ['--no-type-check', '--print-ast', 'test/parser/fail/empty_type_params.fu'])

# Typechecker tests
test('typechecker-pass-mono-structs',        fu, workdir: root, args: ['--print-ast', 'test/typechecker/pass/mono_structs.fu'])
test('typechecker-pass-poly-structs',        fu, workdir: root, args: ['--print-ast', 'test/typechecker/pass/poly_structs.fu'])
test('typechecker-pass-tuple-like-structs',  fu, workdir: root, args: ['--print-ast', 'test/typechecker/pass/tuple_like_structs.fu'])
test('typechecker-fail-type_access_enum',    fu, workdir: root, should_fail: true, args: ['--print-ast', 'test/typechecker/fail/type_access_enum.fu'])
test('typechecker-fail-type_access_struct',  fu, workdir: root, should_fail: true, args: ['--print-ast', 'test/typechecker/fail/type_access_struct.fu'])
test('typechecker-fail-value_access_struct', fu, workdir: root, should_fail: true, args: ['--print-ast', 'test/typechecker/fail/value_access_struct.fu'])
#test('typechecker-pass-structs-inheritance', fu, workdir: root, args: ['--print-ast', 'test/typechecker/pass/struct_inheritance.fu'])

