from __future__ import annotations

import abc

from langkit import names
from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType,
    AbstractNodeData,
    BaseStructType,
    CompiledType,
    EntityType,
)
from langkit.diagnostics import (
    DiagnosticContext,
    Location,
    error,
)


class GenericArgument:
    """
    Holder for generic methods arguments.
    """

    def __init__(
        self,
        name: str,
        type: CompiledType | BaseGenericInterface,
    ):
        """
        :param name: Argument name formatted in lower case.
        :param type: Argument type.
        """
        self.name = names.Name.from_lower(name)
        self.type = type


class InterfaceMethodProfile:
    """
    Generic interface method profile.
    """

    def __init__(
        self,
        name: str,
        args: list[GenericArgument],
        return_type: CompiledType | BaseGenericInterface,
        owner: GenericInterface,
        doc: str,
    ) -> None:
        """
        :param name: Method name formatted in lower case.
        :param args: Arguments of the method.
        :param return_type: Return type of the method.
        :param owner: Interface that owns this method.
        :param doc: Documentation of the method.
        """
        self.name: names.Name = names.Name.from_lower(name)
        self.args = args
        self.return_type = return_type
        self.owner = owner
        self.doc = doc

    @property
    def qualname(self) -> str:
        return f"{self.owner.lkt_name}.{self.name.lower}"


class BaseGenericInterface:
    """
    Base class for all generic interface related types: generic interfaces
    themselves, and arrays of generic interfaces.
    """

    @property
    def array(self) -> ArrayInterface:
        """
        Return a generic interface array type whose element type is ``self``.
        """
        return ArrayInterface(self)

    @abc.abstractproperty
    def lkt_name(self) -> str:
        pass


class GenericInterface(BaseGenericInterface):
    """
    Interface specification class to generate interfaces for the generic API.
    """

    def __init__(
        self,
        name: str,
        ctx: CompileCtx,
        is_always_node: bool = True,
        doc: str = "",
    ) -> None:
        """
        :param name: Name of the interface formatted in camel case.
        :param ctx: CompileCtx in which the interface will be registed.
        :param is_always_node: Whether this interface should only be
            implemented by a node.
        :param doc: Documentation of the generic interface.
        """
        self.name = names.Name.from_camel(name)
        self.doc = doc
        self.is_always_node = is_always_node
        self.methods: dict[str, InterfaceMethodProfile] = {}

        if name in ctx._interfaces:
            error(
                f"Interface {name} already exists", location=Location.nowhere
            )
        ctx._interfaces[name] = self

    @property
    def lkt_name(self) -> str:
        return self.name.camel

    def add_method(
        self,
        name: str,
        args: list[GenericArgument],
        return_type: BaseGenericInterface | CompiledType,
        doc: str = "",
    ) -> None:
        """
        Create a method to the interface with the corresponding name formatted
        in lower case, arguments, return type and documentation.
        """
        if name in self.methods:
            error(
                f"{self.name.camel} already has a method named {name}",
                location=Location.nowhere,
            )
        self.methods[name] = InterfaceMethodProfile(
            name=name, args=args, return_type=return_type, owner=self, doc=doc
        )


class ArrayInterface(BaseGenericInterface):
    """
    An array in the generic interface.
    """

    def __init__(self, element_type: BaseGenericInterface) -> None:
        """
        :param element_type: Element type contained in the array.
        """
        self.element_type = element_type

    @property
    def lkt_name(self) -> str:
        return f"Array[{self.element_type.lkt_name}]"


def type_implements_interface(
    t: BaseStructType,
    interface: GenericInterface,
) -> bool:
    """
    Return whether a type implements an interface.
    """
    # Entity types cannot implement interfaces by themselves: they implement
    # exactly the interfaces that their bare node implements.
    if isinstance(t, EntityType):
        t = t.astnode
    return interface in t.implemented_interfaces()


def matches_interface(
    actual: CompiledType,
    formal: BaseGenericInterface | CompiledType,
) -> bool:
    """
    Return whether ``actual`` matches ``formal``.
    """
    match formal:
        case GenericInterface():
            if not isinstance(actual, BaseStructType):
                return False
            return type_implements_interface(actual, formal)
        case ArrayInterface():
            return actual.is_array and matches_interface(
                actual.element_type, formal.element_type
            )
        case _:
            assert isinstance(formal, CompiledType)
            return actual.matches(formal)


def check_interface_method(
    profile: InterfaceMethodProfile,
    prop: AbstractNodeData,
) -> None:
    """
    Verify that a property signature matches an interface method.
    """
    diag_ctx = DiagnosticContext(prop.location)
    diag_ctx.check_source_language(
        matches_interface(prop.type, profile.return_type),
        "{} returns {}, which does not match return type of {}: {}".format(
            prop.qualname,
            prop.type.lkt_name,
            profile.qualname,
            profile.return_type.lkt_name,
        ),
    )
    base_args = profile.args
    args = prop.natural_arguments
    diag_ctx.check_source_language(
        len(args) == len(base_args),
        "Interface and method implementation don't have the same number of"
        " arguments. Interface has {}, implemetation has {}".format(
            len(base_args), len(args)
        ),
    )

    for arg, base_arg in zip(args, base_args):
        # Check that argument types are consistent with the base
        # method.
        diag_ctx.check_source_language(
            matches_interface(arg.type, base_arg.type),
            f'Argument "{arg.lkt_name}" does not have the same type as in'
            f" interface. Interface has {arg.type.lkt_name}, implementation"
            f" has {base_arg.type.lkt_name}",
        )


def check_interface_field(
    profile: InterfaceMethodProfile,
    field: AbstractNodeData,
) -> None:
    """
    Verify that the field correctly implements the interface method.
    """
    diag_ctx = DiagnosticContext(field.location)
    diag_ctx.check_source_language(
        matches_interface(field.type, profile.return_type),
        "{} returns {}, which does not match return type of {}: {}".format(
            field.qualname,
            field.type.lkt_name,
            profile.qualname,
            profile.return_type.lkt_name,
        ),
    )
    base_args = profile.args
    # Calls to methods implemented by a field cannot take arguments
    diag_ctx.check_source_language(
        len(base_args) == 0,
        "{} takes arguments, but {} is a field".format(
            profile.qualname, field.qualname
        ),
    )


def find_implementations_of_method(
    method: InterfaceMethodProfile,
    astnode: ASTNodeType,
) -> list[AbstractNodeData]:
    """
    Return the list of properties and fields of node, excluding overriden
    ones, that implement the given method.
    """
    implementations = {}
    node: ASTNodeType | None = astnode
    while node is not None:
        class_members = node.get_properties(
            include_inherited=False
        ) + node.get_parse_fields(include_inherited=False)
        for m in class_members:
            if (
                not m.is_internal
                and m.original_name not in implementations
                and m.implements == method
            ):
                implementations[m.original_name] = m
        node = node.base
    return list(implementations.values())


def check_astnode_interface_implementation(
    astnode: ASTNodeType,
    method: InterfaceMethodProfile,
) -> None:
    """
    Verify that the ASTNode correctly implements the given method.
    """
    implementations = find_implementations_of_method(method, astnode)
    diag_ctx = DiagnosticContext(astnode.location)
    if len(implementations) == 0:
        diag_ctx.error(
            "Missing implementation for method {} in class {}".format(
                method.qualname, astnode.lkt_name
            )
        )
    if len(implementations) > 1:
        diag_ctx.error(
            "{} is implementend by multiple properties in class {}:"
            " {}".format(
                method.qualname,
                astnode.lkt_name,
                ", ".join([x.qualname for x in implementations]),
            )
        )
    member = implementations[0]
    # Properties that implement a method interface need to be public in
    # order for bindings to exist.
    if member.is_private:
        error(
            f"Implementation of method {method.qualname} in class"
            f" {astnode.lkt_name} needs to be public",
            location=member.location,
        )
    if member.is_property:
        check_interface_method(method, member)
    else:
        check_interface_field(method, member)


def check_interface_implementations(ctx: CompileCtx) -> None:
    """
    Check that nodes and structs implementing an interface implement the
    corresponding methods.
    """
    for astnode in ctx.node_types:
        diag_ctx = DiagnosticContext(astnode.location)
        for interface in astnode.implemented_interfaces(include_parents=False):
            # Check if the class implements multiple times the same
            # interface.
            diag_ctx.check_source_language(
                astnode.implemented_interfaces(include_parents=False).count(
                    interface
                )
                < 2,
                "{} is implemented multiple times by {}".format(
                    interface.lkt_name,
                    astnode.lkt_name,
                ),
            )
            # Check if the interface is already implemented by a parent
            base = astnode.base
            while base is not None:
                diag_ctx.check_source_language(
                    interface
                    not in base.implemented_interfaces(include_parents=False),
                    "{} implements {}, but it already is implemented"
                    " by its parent class {}".format(
                        astnode.lkt_name,
                        interface.lkt_name,
                        base.lkt_name,
                    ),
                )
                base = base.base

        # Verify that node members implement only methods that belong to
        # interfaces the nodes implement.
        for prop in astnode.get_properties(
            include_inherited=False
        ) + astnode.get_fields(include_inherited=False):
            if prop.implements is None:
                continue
            if not type_implements_interface(astnode, prop.implements.owner):
                diag_ctx.error(
                    f"{prop.qualname} implements {prop.implements.qualname},"
                    f" but {astnode.lkt_name} does not implement"
                    f" {prop.implements.owner.lkt_name}",
                )

        # Verify all interface implementions by the ASTNode
        for interface in astnode.implemented_interfaces():
            for method in interface.methods.values():
                check_astnode_interface_implementation(astnode, method)

    for struct in ctx.struct_types:
        diag_ctx = DiagnosticContext(struct.location)
        for interface in struct.implemented_interfaces():
            # Check if the struct implements multiple times the same
            # interface.
            diag_ctx.check_source_language(
                struct.implemented_interfaces().count(interface) < 2,
                "{} is implemented multiple times by {}".format(
                    interface.lkt_name,
                    struct.lkt_name,
                ),
            )
            if interface.is_always_node:
                diag_ctx.error(
                    "{}: {} should always be implemented by a node".format(
                        struct.lkt_name, interface.lkt_name
                    )
                )

            for method in interface.methods.values():
                fields = [
                    x for x in struct.get_fields() if x.implements == method
                ]
                diag_ctx.check_source_language(
                    len(fields) != 0,
                    "Missing implementation for field {} in struct"
                    " {}".format(method.qualname, struct.lkt_name),
                )
                diag_ctx.check_source_language(
                    len(fields) == 1,
                    "{} is implemented by multiple fields in struct {}:"
                    " {}".format(
                        method.qualname,
                        struct.lkt_name,
                        ", ".join([x.qualname for x in fields]),
                    ),
                )

                check_interface_field(method, fields[0])
