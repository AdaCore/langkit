"""
General test for the named lexical environment mechanism.

The goal of this testcase is to check the named lexical environments mechanism
updates envs/nodes as expected when units are loaded/unloaded.

To do so, we define below a language spec for a severely dubbed down version of
Ada (child units, package decl/body, subprogram decl/body, incomplete and
regular type declarations, declare blocks plus a "ref" node to check name
resolution) and a couple of navigation properties (go to spec, go to body). The
purpose of defining such language constructs is to build non-trivial
environment graphs, whose cross-unit relations are maintained through the named
environment mechanism.

While it's impractical to check the validity of these cross-unit relations,
hidden from public APIs, from a test program, we can use name resolution and
navigation properties to exercize the env lookup process, whose behavior relies
on the environment graph being properly constructed/updated. The test program
checks that these properties behave as expected regardless of the order of
units loading.
"""

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import (EnvSpec, add_env, add_to_env_kv,
                          set_initial_env_by_name)
from langkit.expressions import (AbstractKind, If, Let, No, Not, Self, Var,
                                 langkit_property)

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):

    @langkit_property(return_type=T.Bool, memoized=True)
    def can_have_name():
        """
        Return whether this node can define a named environment.
        """
        return If(
            Self.is_a(PackageDecl, PackageBody, SubpDecl, SubpBody,
                      SubpBodyDecls, PublicPart, PrivatePart),

            # All nodes that can define a named environment are supposed to
            # live in lists, so use Self.parent.parent to get the node that
            # owns that list.
            Self.parent.is_a(T.FooNode.list)
            & (Self.parent.parent.is_null | Self.parent.parent.can_have_name),

            False,
        )

    @langkit_property(return_type=T.Name,
                      kind=AbstractKind.abstract_runtime_check)
    def self_name():
        """
        Assuming this node can define a named environment (see
        ``can_have_name``), return its syntactic name.
        """
        pass

    @langkit_property(return_type=T.Symbol.array)
    def full_name():
        """
        Assuming this node can define a named environment (see
        ``can_have_name``), return its fully qualified namem.
        """
        return Self.name_parent.then(
            lambda np: np.full_name.concat(
                [Self.self_name.cast(T.Identifier).base_name]
            ),
            default_val=Self.self_name.scope_name,
        )

    @langkit_property(return_type=T.Symbol.array)
    def suffixed_full_name(suffix=T.Symbol.array):
        return Self.full_name.then(lambda n: n.concat(suffix))

    @langkit_property(return_type=T.FooNode)
    def name_parent():
        """
        Assuming this node can define a named environment, return the syntactic
        parent node that defines the parent scope for Self. If there is no such
        node (because Self is in the root list), return null.
        """
        return If(
            Self.parent.is_null
            | Not(Self.parent.is_a(T.FooNode.list))
            | Self.parent.parent.is_null
            | Not(Self.parent.parent.is_a(T.DeclarativePart))
            | Self.parent.parent.parent.is_null,
            No(T.FooNode),

            # All nodes that can define a named environment are supposed to
            # live in lists (Self.parent is a list), so Self.parent.parent is
            # supposed to be a declarative part, and finally Self.parent.parent
            # gets the node that owns that declarative part.
            Let(
                lambda owner=Self.parent.parent.parent:
                If(
                    owner.is_a(
                        T.PackageDecl, T.PackageBody, T.SubpDecl, T.SubpBody,
                        T.SubpBodyDecls, T.PublicPart, T.PrivatePart
                    ),
                    owner.match(
                        lambda pp=T.PublicPart: pp.parent,
                        lambda pp=T.PrivatePart: pp.parent,
                        lambda n: n,
                    ),
                    No(T.FooNode),
                )
            )
        )

    @langkit_property(return_type=T.Symbol.array, memoized=True)
    def decl_parent_scope_name():
        """
        If this node can define a named environment (see ``can_have_name``),
        return the name of its parent scope. Return an empty list otherwise.
        """
        return If(
            Self.can_have_name,

            Self.name_parent.then(
                lambda np: np.full_name,
                default_val=Self.self_name.parent_scope_name,
            ),

            No(T.Symbol.array),
        )


@abstract
class Name(FooNode):

    @langkit_property(return_type=T.Symbol)
    def base_name():
        """
        Innermost suffix for this name.
        """
        return Self.match(
            lambda id=Identifier: id.symbol,
            lambda dn=DottedName: dn.suffix.base_name
        )

    @langkit_property(return_type=T.Symbol.array)
    def scope_name():
        """
        Absolute name of the scope that this name defines, assuming that
        ``prefix`` is the implicit prefix for that name.
        """
        return Self.match(
            lambda id=Identifier: [id.symbol],
            lambda dn=DottedName: dn.prefix.scope_name.concat(
                [dn.suffix.symbol]
            ),
        )

    @langkit_property(return_type=T.Symbol.array)
    def parent_scope_name():
        """
        Absolute name of the scope that defines this name, assuming that
        ``prefix`` is the implicit prefix for that name.
        """
        return Self.match(
            lambda _=Identifier: No(T.Symbol.array),
            lambda dn=DottedName: dn.prefix.scope_name,
        )

    @langkit_property(return_type=T.LexicalEnv)
    def scope(from_env=T.LexicalEnv):
        """
        Innermost scope for this name. The lookup starts from the given
        environment.
        """
        return Self.resolve(from_env).children_env

    @langkit_property(return_type=T.LexicalEnv)
    def parent_scope(from_env=T.LexicalEnv):
        """
        Return the scope that contains the declaration that this name defines.
        """
        return Self.match(
            lambda _=Identifier: from_env,
            lambda dn=DottedName: dn.prefix.scope(from_env)
        )

    @langkit_property(return_type=T.FooNode.entity)
    def resolve(from_env=T.LexicalEnv):
        """
        Return the list of declarations that define this name. The lookup
        starts from the given environment.
        """
        return Self.parent_scope(from_env).get_first(Self.base_name)


class Identifier(Name):
    token_node = True


class DottedName(Name):
    prefix = Field(type=T.Name)
    suffix = Field(type=T.Identifier)


class DeclarativePart(FooNode):
    decls = Field(type=T.FooNode.list)


class SubpBodyDecls(DeclarativePart):
    """
    Top-level list of declaration in a subprogam body.
    """
    env_spec = EnvSpec(
        # The only case where we need a declarative part to have a name is when
        # it is top-level in subprogram body (A), so that separate subprogram
        # bodies (B) can use this environment as their initial env. Note that
        # this case is legal only when that (A) is the syntactic root.
        add_env(names=If(
            Self.parent.is_a(T.SubpBody) & Self.parent.parent.is_null,
            [Self.parent.cast(T.SubpBody).name.scope_name],
            No(T.Symbol.array.array),
        ))
    )


class PublicPart(DeclarativePart):
    pass


class PrivatePart(DeclarativePart):

    env_spec = EnvSpec(
        add_env(names=[Self.parent.suffixed_full_name(['__privatepart'])],
                transitive_parent=True),
    )


class PackageDecl(FooNode):
    name = Field(type=T.Name)
    public_part = Field(type=T.PublicPart)
    private_part = Field(type=T.PrivatePart)

    @langkit_property(return_type=T.Name)
    def self_name():
        return Self.name

    @langkit_property(return_type=T.Symbol.array.array)
    def new_env_names():
        """
        Return the names for the environment that this package creates.
        """
        # Always register the "regular" name for this package
        result = Self.full_name

        # If there is a private part, that's all we need. Otherwise, register
        # this environment with the name of the private part as well, so that
        # package bodies can assume there is always a private part.
        return If(
            result.length == 0,
            No(T.Symbol.array.array),

            If(
                Self.private_part.is_null,
                [result, result.concat(['__privatepart'])],
                [result],
            )
        )

    env_spec = EnvSpec(
        set_initial_env_by_name(Self.decl_parent_scope_name,
                                Self.parent.children_env),
        add_to_env_kv(Self.name.base_name, Self),
        add_env(names=Self.new_env_names)
    )

    @langkit_property(return_type=T.PackageBody.entity, public=True)
    def body_part():
        """
        Return the PackageBody node corresponding to this PackageDecl.
        """
        return Self.private_part.then(
            lambda pp: pp,
            default_val=Self,
        ).children_env.get_first('__nextpart').cast(T.PackageBody)


class PackageBody(FooNode):
    name = Field(type=T.Name)
    decls = Field(type=T.DeclarativePart)

    @langkit_property(return_type=T.Name)
    def self_name():
        return Self.name

    @langkit_property(return_type=T.LexicalEnv)
    def get_initial_env():
        """
        Assuming this PackageBody cannot define a named environment, return the
        environment in which this declaration should be registered.
        """
        pkg_decl = Var(
            Self.parent.children_env.get_first(Self.name.base_name)
            .cast(T.PackageDecl)
        )
        return If(
            pkg_decl.private_part.is_null,
            pkg_decl.children_env,
            pkg_decl.private_part.children_env
        )

    env_spec = EnvSpec(
        # The initial environment for package bodies is the private part of the
        # correspoding package specs (or the public part if there is no private
        # part).
        set_initial_env_by_name(Self.suffixed_full_name(['__privatepart']),
                                Self.get_initial_env),
        add_to_env_kv('__nextpart', Self),
        add_env(names=[Self.suffixed_full_name(['__body'])]),
    )

    @langkit_property(return_type=T.PackageDecl.entity, public=True)
    def decl_part():
        """
        Return the PackageDecl node corresponding to this PackageBody.
        """
        env = Var(Self.parent.children_env)
        return Self.name.resolve(env).cast(T.PackageDecl)


class IncompleteTypeDecl(FooNode):
    name = Field(type=T.Identifier)

    env_spec = EnvSpec(add_to_env_kv(Self.name.symbol, Self))


class TypeDecl(FooNode):
    name = Field(type=T.Identifier)

    env_spec = EnvSpec(add_to_env_kv(Self.name.symbol, Self))


class SubpDecl(FooNode):
    name = Field(type=T.Name)

    @langkit_property(return_type=T.Name)
    def self_name():
        return Self.name

    env_spec = EnvSpec(
        set_initial_env_by_name(Self.decl_parent_scope_name,
                                Self.parent.children_env),
        add_to_env_kv(Self.name.base_name, Self),
    )


class SubpBody(FooNode):
    name = Field(type=T.Name)
    decls = Field(type=T.SubpBodyDecls)
    stmts = Field(type=T.FooNode.list)

    @langkit_property(return_type=T.Name)
    def self_name():
        return Self.name

    env_spec = EnvSpec(
        set_initial_env_by_name(Self.decl_parent_scope_name,
                                Self.parent.children_env),
        # TODO: add __nextpart to the spec
    )


class DeclareBlock(FooNode):
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.FooNode.list)


class Ref(FooNode):
    name = Field(type=T.Name)

    @langkit_property(return_type=T.FooNode.entity, public=True)
    def resolve():
        return Self.name.resolve(Self.node_env)


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              unparse_script=unparse_all_script,  ada_main='main.adb')
print('Done')
