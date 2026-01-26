from langkit.dsl import ASTNode, LexicalEnv, T
from langkit.expressions import DynamicVariable, Literal, No, Property, Self

from utils import emit_and_print_errors


Env = DynamicVariable('env', LexicalEnv)


def run(name, lhs, rhs):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in Example.
    """

    global FooNode

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        prop = Property(lhs.equals(rhs), dynamic_vars=[Env])
        use_prop = Property(Env.bind(Self.node_env, Self.prop), public=True)

    class Lit(FooNode):
        token_node = True

    emit_and_print_errors(lkt_file='foo.lkt')
    Env.unfreeze()
    print('')


def bool_expr():
    return Literal(True)


def int_expr():
    return Literal(0)


def example_expr():
    return Self


def example_entity():
    return Env.get('foo').at(0).cast(T.Example.entity)


def lit_expr():
    return No(T.Lit)


def lit_entity():
    return Env.get('foo').at(0).cast(T.Lit.entity)


def foo_expr():
    return Self.parent


def foo_entity():
    return Env.get('foo').at(0)


run('Correct: Boolean <-> Boolean', bool_expr(), bool_expr())
run('Correct: ASTNode <-> ASTNode', foo_expr(), foo_expr())
run('Correct: ASTNode <-> ASTNode (subclass)', foo_expr(), example_expr())
run('Correct: Entity <-> Entity', foo_entity(), foo_entity())
run('Correct: Entity <-> Entity (subclass)', foo_entity(), example_entity())

run('Boolean <-> ASTNode', bool_expr(), foo_expr())
run('ASTNode <-> Boolean', foo_expr(), bool_expr())
run('ASTNode <-> Entity', foo_expr(), foo_entity())
run('Boolean <-> Entity', bool_expr(), foo_entity())
run('ASTNode <-> Boolean', foo_expr(), bool_expr())
run('Long <-> Boolean', int_expr(), bool_expr())

run('ASTNode <-> ASTNode (never equal)', example_expr(), lit_expr())
run('Entity <-> Entity (never equal)', example_entity(), lit_entity())

print('Done')
