project('fu', 'c', version: '0.1', meson_version: '>= 0.56.0')

cc = meson.get_compiler('c')
math_lib = cc.find_library('m', required : false)

libfu = library('libfu',
  sources: [
    'src/fu/core/format.c',
    'src/fu/core/hash.c',
    'src/fu/core/hash_table.c',
    'src/fu/core/log.c',
    'src/fu/core/mem_pool.c',
    'src/fu/core/str_pool.c',
    'src/fu/core/file_loc.c',
    'src/fu/core/dyn_array.c',
    'src/fu/core/utils.c',
    'src/fu/lang/ast.c',
    'src/fu/lang/bind.c',
    'src/fu/lang/check.c',
    'src/fu/lang/lexer.c',
    'src/fu/lang/parser.c',
    'src/fu/lang/types.c',
    'src/fu/lang/type_table.c',
    'src/fu/ir/node.c',
    'src/fu/ir/node_map.c',
    'src/fu/ir/node_set.c',
    'src/fu/ir/import.c',
    'src/fu/ir/module.c',
    'src/fu/ir/simplify.c',
    'src/fu/driver/driver.c',
    'src/fu/driver/options.c'],
  include_directories: 'src',
  dependencies: math_lib,
  c_args: '-DFU_VERSION="@0@"'.format(meson.project_version()),
  name_prefix: '')

fu = executable('fu',
  sources: ['src/fu/driver/main.c'],
  include_directories: 'src',
  link_with: libfu)

subdir('test')
