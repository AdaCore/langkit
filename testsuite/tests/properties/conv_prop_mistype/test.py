"""
Check the correct propagation of PropertyError exceptions when property
references in logic equations are called on mismatching node types.
"""

from langkit.dsl import ASTNode, Field, T, UserField
from langkit.expressions import (
    Bind,
    Entity,
    NPropagate,
    Self,
    ignore,
    langkit_property,
)

from utils import build_and_run


class FooNode(ASTNode):
    lvar = UserField(T.LogicVar, public=False)


class Examples(FooNode):
    re = Field(type=T.RegularExample)
    pe = Field(type=T.PlusExample)

    @langkit_property()
    def domains():
        """
        Helper to assign PlusExample/RegularExample nodes to the corresponding
        logic vars.
        """
        return Self.re.lvar.domain([Self.re]) & Self.pe.lvar.domain([Self.pe])

    # PropertyError expected when PlusExample.conv_prop is called on a
    # RegularExample.

    @langkit_property(public=True)
    def fail_bind_conv_prop():
        return (
            Bind(Self.pe.lvar, Self.re.lvar, conv_prop=PlusExample.conv_prop)
            & Self.domains
        ).solve

    @langkit_property(public=True)
    def fail_nprop_single():
        return (
            NPropagate(Self.pe.lvar, PlusExample.conv_prop, Self.re.lvar)
            & Self.domains
        ).solve

    # Successful example of a NPropagate (as a sanity check)

    @langkit_property(public=True)
    def ok_nprop_multi():
        return (
            NPropagate(
                Self.pe.lvar,
                PlusExample.comb_prop,
                Self.pe.lvar,
                Self.re.lvar,
                Self.pe.lvar,
            )
            & Self.domains
        ).solve

    # PropertyError expected when PlusExample.comb_prop is called on a
    # RegularExample.

    @langkit_property(public=True)
    def fail_nprop_multi_1():
        return (
            NPropagate(
                Self.pe.lvar,
                PlusExample.comb_prop,
                Self.re.lvar,
                Self.re.lvar,
                Self.pe.lvar,
            )
            & Self.domains
        ).solve

    # PropertyError expected when PlusExample.comb_prop is called with a
    # PlusExample for its "re" argument.

    @langkit_property(public=True)
    def fail_nprop_multi_2():
        return (
            NPropagate(
                Self.pe.lvar,
                PlusExample.comb_prop,
                Self.pe.lvar,
                Self.pe.lvar,
                Self.pe.lvar,
            )
            & Self.domains
        ).solve

    # PropertyError expected when PlusExample.comb_prop is called with a
    # RegularExample for its "pe" argument.

    @langkit_property(public=True)
    def fail_nprop_multi_3():
        return (
            NPropagate(
                Self.pe.lvar,
                PlusExample.comb_prop,
                Self.pe.lvar,
                Self.re.lvar,
                Self.re.lvar,
            )
            & Self.domains
        ).solve

    # Successful example of a NPropagate with a varargs combiner property (as a
    # sanity check).

    @langkit_property(public=True)
    def ok_nprop_varargs():
        return (
            NPropagate(
                Self.pe.lvar,
                PlusExample.ncomb_prop,
                [Self.pe.lvar, Self.pe.lvar],
            )
            & Self.domains
        ).solve

    # PropertyError expected when PlusExample.ncomb_prop is called with at
    # least one RegularExample in its "pe_list" argument.

    @langkit_property(public=True)
    def fail_nprop_varargs():
        return (
            NPropagate(
                Self.pe.lvar,
                PlusExample.ncomb_prop,
                [Self.pe.lvar, Self.re.lvar],
            )
            & Self.domains
        ).solve


class RegularExample(FooNode):
    pass


class PlusExample(FooNode):

    @langkit_property()
    def conv_prop():
        return Entity

    @langkit_property()
    def comb_prop(re=T.RegularExample.entity, pe=T.PlusExample.entity):
        ignore(re)
        return pe

    @langkit_property()
    def ncomb_prop(pe_list=T.PlusExample.entity.array):
        return pe_list.at(0)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print('Done')
