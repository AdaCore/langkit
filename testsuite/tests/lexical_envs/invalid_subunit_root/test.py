from __future__ import absolute_import, division, print_function

from langkit.dsl import Annotations, ASTNode, Field, synthetic
from langkit.parsers import Grammar, List, Opt, Or

from utils import emit_and_print_errors


def construct_multiple():

    class FooNode(ASTNode):
        pass

    class Example1(FooNode):
        annotations = Annotations(subunit_root=True)

    class Example2(FooNode):
        annotations = Annotations(subunit_root=True)

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=Or(Example1('example', ','),
                                   Example2('example', ';')))
    return grammar


def construct_derived():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        annotations = Annotations(subunit_root=True)

    class DerivedExample(Example):
        pass

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=Or(Example('example', ','),
                                   DerivedExample('example', ';')))
    return grammar


def construct_synthetic():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        pass

    @synthetic
    class SyntheticNode(FooNode):
        annotations = Annotations(subunit_root=True)

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=Example('example'))
    return grammar


def construct_bad_main_rule_1():

    class FooNode(ASTNode):
        pass

    class ExampleWrapper(FooNode):
        example = Field()

    class Example(FooNode):
        annotations = Annotations(subunit_root=True)

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=ExampleWrapper(List(Example('example'))))
    return grammar


def construct_bad_main_rule_2():

    class FooNode(ASTNode):
        pass

    class ExampleWrapper(FooNode):
        example = Field()

    class Example(FooNode):
        annotations = Annotations(subunit_root=True)

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=List(ExampleWrapper(Example('example'))))
    return grammar


def construct_non_root_list():

    class FooNode(ASTNode):
        pass

    class Subunit(FooNode):
        annotations = Annotations(subunit_root=True)
        fields = Field()

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=List(Subunit('example', grammar.main_rule)))
    return grammar


def construct_multiple_lists():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        annotations = Annotations(subunit_root=True)

    class ListOfExample(Example.list):
        pass

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=List(Example('example'),
                                     list_cls=ListOfExample))
    return grammar


def construct_subunit_root_field():

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        annotations = Annotations(subunit_root=True)
        child = Field()

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=List(grammar.example),
                      example=Example('example', Opt(grammar.example)))
    return grammar


for constructor in (
    construct_multiple,
    construct_derived,
    construct_synthetic,
    construct_bad_main_rule_1,
    construct_bad_main_rule_2,
    construct_non_root_list,
    construct_multiple_lists,
    construct_subunit_root_field
):
    print('= {} ='.format(constructor.__name__[10:]))
    grammar = constructor()
    emit_and_print_errors(grammar)
    print('')

print('Done')
