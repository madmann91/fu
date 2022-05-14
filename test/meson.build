# General tests
test('usage',                 fu, workdir: root, should_fail: true, args: ['-h'])
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
test('typechecker-fail-type-access-enum',    fu, workdir: root, should_fail: true, args: ['--print-ast', 'test/typechecker/fail/type_access_enum.fu'])
test('typechecker-fail-type-access-struct',  fu, workdir: root, should_fail: true, args: ['--print-ast', 'test/typechecker/fail/type_access_struct.fu'])
test('typechecker-fail-value_access-struct', fu, workdir: root, should_fail: true, args: ['--print-ast', 'test/typechecker/fail/value_access_struct.fu'])
test('typechecker-fail-bad-super-struct',    fu, workdir: root, should_fail: true, args: ['--print-ast', 'test/typechecker/fail/bad_super_struct.fu'])
test('typechecker-fail-bad-sub-enum',        fu, workdir: root, should_fail: true, args: ['--print-ast', 'test/typechecker/fail/bad_sub_enum.fu'])
#test('typechecker-pass-structs-inheritance', fu, workdir: root, args: ['--print-ast', 'test/typechecker/pass/struct_inheritance.fu'])

