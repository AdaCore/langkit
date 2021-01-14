"""
Check that invalid error nodes are properly reported.
"""

import langkit
from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode, Field, abstract, synthetic

from utils import emit_and_print_errors


def test_abstract(root):
    @abstract
    class ErrorDecl(root):
        error_node = True


def test_bad_type(root):
    class Example(root):
        error_node = 1


def test_derived_not_error(root):
    class BaseError(root):
        error_node = True

    class DerivedError(BaseError):
        error_node = False


def test_error_and_list(root):
    class Example(root):
        token_node = True

    class ErrorList(Example.list):
        error_node = True


def test_error_and_token(root):
    class Example(root):
        token_node = True
        error_node = True


def test_nonnull_field(root):
    class Name(root):
        token_node = True

    class ErrorDecl(root):
        error_node = True
        f = Field(type=Name)


def test_synthetic(root):
    @synthetic
    class ErrorDecl(root):
        error_node = True


for func_name in sorted(dir()):
    if not func_name.startswith("test_"):
        continue
    print(f"== {func_name} ==")
    func = locals().get(func_name)

    class FooNode(ASTNode):
        pass

    try:
        func(FooNode)
    except DiagnosticError:
        pass
    else:
        emit_and_print_errors()
    print()

    langkit.reset()

print('Done')
