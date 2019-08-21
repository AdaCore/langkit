from __future__ import absolute_import, division, print_function

import sys

import libfoolang


print('main.py: Running...')

ctx = libfoolang.AnalysisContext()


def test_case(name, content):
    u = ctx.get_from_buffer(name, content)
    if u.diagnostics:
        for d in u.diagnostics:
            print(d)
        sys.exit(1)

    u.populate_lexical_env()
    print(" ".join(str(x.text) for x in u.root.p_env_get_all))

test_case(
    'input1',
    'a b c d e f g h i j k l m n o p q r s t u v w x y z'
)

test_case(
    'input2',
    'k z a b g e h s f i p l w y o q x t j u c d r n m v'
)

test_case(
    'input3',
    'z x v t r p n l j h f d b a c e g i k m o q s u w y'
)

print('main.py: Done.')
