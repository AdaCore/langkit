"""
Test that the is_optional property on parse fields works as expected.
"""

from langkit.dsl import ASTNode, Field

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


class ExampleWrapper(FooNode):
    # Optional fields
    field_opt = Field()
    field_or = Field()
    field_defer = Field()
    field_null = Field()
    field_dont_skip = Field()

    # Non optional fields
    field_opt_bool = Field()
    field_transform = Field()


class HasExample(FooNode):
    enum_node = True
    qualifier = True


emit_and_print_errors(lkt_file='foo.lkt')

fields = [
    ExampleWrapper.field_opt,
    ExampleWrapper.field_or,
    ExampleWrapper.field_defer,
    ExampleWrapper.field_null,
    ExampleWrapper.field_dont_skip,

    ExampleWrapper.field_opt_bool,
    ExampleWrapper.field_transform
]

for field in fields:
    print("Field {} is {}".format(
        field.qualname,
        "optional" if field.is_optional else "not optional"
    ))

print('Done')
