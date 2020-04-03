"""
Enum types validity checks.
"""

from __future__ import absolute_import, division, print_function
from langkit.diagnostics import DiagnosticError
from langkit.dsl import Enum, EnumValue


def too_many_defaults():
    class MyEnum(Enum):
        a = EnumValue(is_default=True)
        b = EnumValue(is_default=True)


def invalid_base():
    class MyEnum(Enum, list):
        a = EnumValue()
        b = EnumValue()


def invalid_field():
    class MyEnum(Enum):
        a = EnumValue()
        b = EnumValue()
        c = 12


def enumval_aliasing():
    class MyEnum(Enum):
        a = EnumValue()

    class MyEnum2(Enum):
        a = MyEnum.a


for test in (too_many_defaults, invalid_base, invalid_field, enumval_aliasing):
    try:
        print("== {} ==".format(test.__name__))
        test()
    except DiagnosticError:
        pass

print('Done')
