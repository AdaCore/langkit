from langkit.dsl import ASTNode, Annotations, Field, synthetic

from utils import emit_and_print_errors


def construct_multiple():

    class FooNode(ASTNode):
        pass

    class Example1(FooNode):
        annotations = Annotations(ple_unit_root=True)

    class Example2(FooNode):
        annotations = Annotations(ple_unit_root=True)

    return 'multiple.lkt'


def construct_derived():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        annotations = Annotations(ple_unit_root=True)

    class DerivedExample(Example):
        pass

    return 'derived.lkt'


def construct_synthetic():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        pass

    @synthetic
    class SyntheticNode(FooNode):
        annotations = Annotations(ple_unit_root=True)

    return 'synthetic.lkt'


def construct_bad_main_rule_1():

    class FooNode(ASTNode):
        pass

    class ExampleWrapper(FooNode):
        example = Field()

    class Example(FooNode):
        annotations = Annotations(ple_unit_root=True)

    return 'bad_main_rule_1.lkt'


def construct_bad_main_rule_2():

    class FooNode(ASTNode):
        pass

    class ExampleWrapper(FooNode):
        example = Field()

    class Example(FooNode):
        annotations = Annotations(ple_unit_root=True)

    return 'bad_main_rule_2.lkt'


def construct_non_root_list():

    class FooNode(ASTNode):
        pass

    class Subunit(FooNode):
        annotations = Annotations(ple_unit_root=True)
        fields = Field()

    return 'non_root_list.lkt'


def construct_multiple_lists():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        annotations = Annotations(ple_unit_root=True)

    class ListOfExample(Example.list):
        pass

    return 'multiple_lists.lkt'


def construct_ple_unit_root_field():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        annotations = Annotations(ple_unit_root=True)
        child = Field()

    return 'ple_unit_root_field.lkt'


for constructor in (
    construct_multiple,
    construct_derived,
    construct_synthetic,
    construct_bad_main_rule_1,
    construct_bad_main_rule_2,
    construct_non_root_list,
    construct_multiple_lists,
    construct_ple_unit_root_field
):
    print('= {} ='.format(constructor.__name__[10:]))
    lkt_file = constructor()
    emit_and_print_errors(lkt_file=lkt_file)
    print('')

print('Done')
