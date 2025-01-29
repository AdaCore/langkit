from __future__ import annotations

from langkit import names
from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType, AbstractNodeData, BaseStructType, CompiledType, EntityType, T,
    resolve_type
)
from langkit.diagnostics import (
    Location, check_source_language, diagnostic_context, error
)


class GenericArgument:
    """
    Holder for generic methods arguments.
    """

    def __init__(
        self,
        name: str,
        type: CompiledType | GenericInterface | ArrayInterface,
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
        return_type: CompiledType | GenericInterface | ArrayInterface,
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
        return f"{self.owner.dsl_name}.{self.name.lower}"


class GenericInterface:
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
            with diagnostic_context(Location.nowhere):
                error(f"Interface {name} already exists")
        ctx._interfaces[name] = self

    def get_method(self, name: str) -> InterfaceMethodProfile:
        """
        Return the InterfaceMethodProfile with the corresponding name in lower
        case.
        """
        try:
            return self.methods[name]
        except KeyError:
            error(f"{self.name.camel} has no {name} method")

    @property
    def dsl_name(self) -> str:
        return self.name.camel

    def add_method(
        self,
        name: str,
        args: list[GenericArgument],
        return_type: GenericInterface | ArrayInterface | CompiledType,
        doc: str = "",
    ) -> None:
        """
        Create a method to the interface with the corresponding name formatted
        in lower case, arguments, return type and documentation.
        """
        if name in self.methods:
            with diagnostic_context(Location.nowhere):
                error(f"{self.name.camel} already has a method named {name}")
        self.methods[name] = InterfaceMethodProfile(
            name=name,
            args=args,
            return_type=return_type,
            owner=self,
            doc=doc
        )


class ArrayInterface:
    """
    An array in the generic interface.
    """

    def __init__(self, element_type: GenericInterface) -> None:
        """
        :param element_type: Element type contained in the array.
        """
        self.element_type = element_type

    @property
    def name(self) -> str:
        return f"ArrayInterface[{self.element_type.dsl_name}]"

    @property
    def dsl_name(self) -> str:
        return self.name


def create_builtin_interfaces(ctx: CompileCtx) -> None:
    """
    Create all the generic interfaces.
    """

    # TODO: Enforce that no user defined type can implement interfaces reserved
    # for builtin types.

    # Reserved for builtin types.
    #
    # Defines the interface for Logic_Context. The type is not manually defined
    # in the generated libraries, so we need to define this interface in order
    # to have the corresponding generated class implement the interface from
    # the support library.
    logic_context_interface = GenericInterface(
        name="LogicContextInterface",
        ctx=ctx,
        is_always_node=False,
        doc="""
        Describes an interpretation of a reference. Can be attached
        to logic atoms (e.g. Binds) to indicate under which interpretation
        this particular atom was produced, which can in turn be used to
        produce informative diagnostics for resolution failures.
        """
    )

    # Reserved for builtin types.
    #
    # Defines the interface for Solver_Diagnostic. The type is not manually
    # defined in the generated libraries, so we need to define this interface
    # in order to have the corresponding generated class implement the
    # interface from the support library.
    solver_diagnostic_interface = GenericInterface(
        name="SolverDiagnosticInterface",
        ctx=ctx,
        is_always_node=False,
        doc="""
        A raw diagnostic produced by a solver resolution failure.
        This contains as much information as possible to allow formatters
        down the chain to filter/choose which diagnostics to show among
        a set of diagnostics produced for a single equation.
        """
    )

    # This interface is automatically implemented by the root node. It musts
    # not be implemented in the language spec. Unlike ``LspNodeInterface``
    # below, this interface does have any method. It is only used as a type so
    # that other interface may refer to the root node type without forcing the
    # user to implement the methods of ``LspNodeInterface``.
    root_node_interface = GenericInterface(
        name="NodeInterface",
        ctx=ctx,
        doc="""
        Interface reprensenting the root node.
        """
    )

    lsp_node_interface = GenericInterface(
        name="LspNodeInterface",
        ctx=ctx,
        doc="""
        Interface to be implemented by all nodes that will support common LSP
        requests: run name resolution or code completion.
        """
    )

    defining_name_interface = GenericInterface(
        name="DefiningNameInterface",
        ctx=ctx,
        doc="""
        Interface representing identifiers that define an entity in the
        analyzed source code.
        """
    )

    type_interface = GenericInterface(
        name="TypeInterface",
        ctx=ctx,
        doc="""
        Interface representing types in the analyzed source code.
        """
    )

    typable_node_interface = GenericInterface(
        name="TypableNodeInterface",
        ctx=ctx,
        doc="""
        Interface representing nodes that can have a type in the analyzed
        source code.
        """
    )

    reference_interface = GenericInterface(
        name="ReferenceInterface",
        ctx=ctx,
        doc="""
        Interface representing nodes that reference a definition.
        """
    )

    declaration_interface = GenericInterface(
        name="DeclarationInterface",
        ctx=ctx,
        doc="""
        Interface representing a declaration containing at least one defined
        name.
        """
    )

    completion_item_interface = GenericInterface(
        name="CompletionItemInterface",
        ctx=ctx,
        is_always_node=False,
        doc="""
        Interface representing a struct that contains completion information.
        """
    )

    ref_result_interface = GenericInterface(
        name="RefResultInterface",
        ctx=ctx,
        is_always_node=False,
        doc="""
        Interface representing a struct that contains the result of resolving
        an entity reference.
        """
    )

    # LogicContextInterface methods

    logic_context_interface.add_method(
        'ref_node', args=[], return_type=root_node_interface
    )
    logic_context_interface.add_method(
        'decl_node', args=[], return_type=root_node_interface
    )

    # SolverDiagnosticInterface methods

    solver_diagnostic_interface.add_method(
        'message_template', args=[], return_type=T.String,
        doc="""
        Return the string explaining the error, which may contain holes
        represented by the ``{}`` characters.
        """
    )

    solver_diagnostic_interface.add_method(
        'args',
        args=[],
        return_type=ArrayInterface(root_node_interface),
        doc="""
        Return an array of nodes, which are to be plugged in the holes of the
        template in the same order (i.e. the first argument goes into the first
        hole of the template, etc.).
        """
    )

    solver_diagnostic_interface.add_method(
        'location',
        args=[],
        return_type=root_node_interface,
        doc="""
        Return the node which indicates the location of the error.
        """
    )

    solver_diagnostic_interface.add_method(
        'contexts',
        args=[],
        return_type=ArrayInterface(logic_context_interface),
        doc="""
        Return the array of contexts that were deemed relevant for this error.
        """
    )

    # LspNodeInterface methods

    lsp_node_interface.add_method(
        'xref_entry_point',
        args=[],
        return_type=T.Bool,
        doc="""
        Return True if this node is an entry point for name resolution.
        """
    )

    lsp_node_interface.add_method(
        'complete_items',
        args=[],
        return_type=ArrayInterface(completion_item_interface),
        doc="""
        Return a list of completion item.
        """
    )

    lsp_node_interface.add_method(
        'nameres_diagnostics',
        args=[],
        return_type=ArrayInterface(solver_diagnostic_interface),
        doc="""
        Run name resolution on this node if it is an entry point and return
        all raised diagnostics, if any.
        """
    )

    # DefiningNameInterface methods

    defining_name_interface.add_method(
        'full_name',
        args=[],
        return_type=T.String,
        doc="""
        Return the full name defined by this DefiningName.
        """,
    )

    defining_name_interface.add_method(
        'decl_detail',
        args=[],
        return_type=T.String,
        doc="""
        Return a string containing the detail of this defining name to display
        when hovering or in completion items (generally the type or prototype).
        """
    )

    defining_name_interface.add_method(
        'completion_item_kind',
        args=[],
        return_type=T.Int,
        doc="""
        Return an integer correspoding to the LSP's CompletionItemKind type.
        """
    )

    defining_name_interface.add_method(
        'documentation',
        args=[],
        return_type=T.String,
        doc="""
        Return the documentation associated to this defining name.
        """
    )

    defining_name_interface.add_method(
        'get_type',
        args=[],
        return_type=type_interface,
        doc="""
        Return the type associated to this defining name.
        """
    )

    defining_name_interface.add_method(
        'find_all_references',
        args=[GenericArgument("units", T.AnalysisUnit.array)],
        return_type=ArrayInterface(ref_result_interface),
        doc="""
        Return an array of RefResults containing all references in the given
        AnalysisUnits.
        """
    )

    defining_name_interface.add_method(
        'find_implementations',
        args=[GenericArgument("units", T.AnalysisUnit.array)],
        return_type=ArrayInterface(defining_name_interface),
        doc="""
        Return an array of DefiningName containing all implementations of the
        DefiningName in the given units.
        """
    )

    # TypeInterface methods

    type_interface.add_method(
        'full_name',
        args=[],
        return_type=T.String,
        doc="""
        Return the full name of the type.
        """
    )

    type_interface.add_method(
        'defining_name',
        args=[],
        return_type=defining_name_interface,
        doc="""
        Return the DefiningName of the type associated to this type.
        """
    )

    # TypableNodeInterface methods

    typable_node_interface.add_method(
        'expr_type',
        args=[],
        return_type=type_interface,
        doc="""
        Return the type of the associated to this node.
        """
    )

    # ReferenceInterface methods

    reference_interface.add_method(
        'referenced_defining_name',
        args=[],
        return_type=defining_name_interface,
        doc="""
        Return the DefiningName referenced by this object.
        """
    )

    # DeclarationInterface methods

    declaration_interface.add_method(
        'defining_names',
        args=[],
        return_type=ArrayInterface(defining_name_interface),
        doc="""
        Return the list of names defined by this definition.
        """
    )

    # CompletionItemInterface methods

    completion_item_interface.add_method(
        'decl',
        args=[],
        return_type=declaration_interface,
        doc="""
        Return the declaration for this completion item.
        """
    )

    # RefResultInterface methods

    ref_result_interface.add_method(
        'ref',
        args=[],
        return_type=reference_interface,
        doc="""
        Return the reference of this RefResult.
        """
    )


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
    return interface in t.implements()


def matches_interface(
    actual: CompiledType,
    formal: GenericInterface | ArrayInterface | CompiledType,
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
            return (
                actual.is_array
                and matches_interface(actual.element_type, formal.element_type)
            )
        case _:
            return actual.matches(resolve_type(formal))


def check_interface_method(
    profile: InterfaceMethodProfile,
    prop: AbstractNodeData,
) -> None:
    """
    Verify that a property signature matches an interface method.
    """
    with diagnostic_context(prop.location):
        check_source_language(
            matches_interface(prop.type, profile.return_type),
            "{} returns {}, which does not match return type of {}: {}"
            .format(
                prop.qualname,
                prop.type.dsl_name,
                profile.qualname,
                profile.return_type.dsl_name,
            )
        )
        base_args = profile.args
        args = prop.natural_arguments
        check_source_language(
            len(args) == len(base_args),
            "Interface and method implementation don't have the same"
            " number of arguments. Interface has {}, implemetation has {}"
            .format(len(base_args), len(args))
        )

        for arg, base_arg in zip(args, base_args):
            # Check that argument types are consistent with the base
            # method.
            check_source_language(
                matches_interface(arg.var.type, base_arg.type),
                'Argument "{}" does not have the same type as in'
                ' interface. Interface has {}, implementation has {}'
                .format(
                    arg.dsl_name,
                    arg.var.type.dsl_name,
                    base_arg.type.dsl_name,
                )
            )


def check_interface_field(
    profile: InterfaceMethodProfile,
    field: AbstractNodeData,
) -> None:
    """
    Verify that the field correctly implements the interface method.
    """
    with diagnostic_context(field.location):
        check_source_language(
            matches_interface(field.type, profile.return_type),
            "{} returns {}, which does not match return type of {}: {}"
            .format(
                field.qualname,
                field.type.dsl_name,
                profile.qualname,
                profile.return_type.dsl_name,
            )
        )
        base_args = profile.args
        # Calls to methods implemented by a field cannot take arguments
        check_source_language(
            len(base_args) == 0,
            "{} takes arguments, but {} is a field"
            .format(profile.qualname, field.qualname)
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
        class_members = (
            node.get_properties(include_inherited=False)
            + node.get_parse_fields(include_inherited=False)
        )
        for m in class_members:
            if (
                m.original_name not in implementations
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
    with diagnostic_context(astnode.location):
        if len(implementations) == 0:
            error(
                "Missing implementation for method {} in class {}"
                .format(method.qualname, astnode.dsl_name)
            )
        if len(implementations) > 1:
            error(
                "{} is implementend by multiple properties in class {}: {}"
                .format(
                    method.qualname,
                    astnode.dsl_name,
                    ", ".join([x.qualname for x in implementations])
                )
            )
    member = implementations[0]
    # Properties that implement a method interface need to be public in
    # order for bindings to exist.
    if member.is_private:
        with diagnostic_context(member.location):
            error(
                "Implementation of method {} in class {} needs to be public"
                .format(method.qualname, astnode.dsl_name)
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
    for astnode in ctx.astnode_types:
        with diagnostic_context(astnode.location):
            for interface in astnode.implements(include_parents=False):
                # Check if the class implements multiple times the same
                # interface.
                check_source_language(
                    astnode
                    .implements(include_parents=False)
                    .count(interface) < 2,
                    "{} is implemented multiple times by {}".format(
                        interface.dsl_name,
                        astnode.dsl_name,
                    )
                )
                # Check if the interface is already implemented by a parent
                base = astnode.base
                while base is not None:
                    check_source_language(
                        interface not in
                        base.implements(include_parents=False),
                        "{} implements {}, but it already is implemented"
                        " by its parent class {}".format(
                            astnode.dsl_name,
                            interface.dsl_name,
                            base.dsl_name,
                        )
                    )
                    base = base.base

        # Verify that node members implement only methods that belong to
        # interfaces the nodes implement.
        for method in (
            astnode.get_properties(include_inherited=False)
            + astnode.get_fields(include_inherited=False)
        ):
            if method.implements is None:
                continue
            if method.implements.owner not in astnode.implements():
                with diagnostic_context(astnode.location):
                    error(
                        "{} implements {}, but {} does not implement {}"
                        .format(
                            method.qualname,
                            method.implements.qualname,
                            astnode.dsl_name,
                            method.implements.owner.dsl_name,
                        )

                    )

        # Verify all interface implementions by the ASTNode
        for interface in astnode.implements():
            for method in interface.methods.values():
                check_astnode_interface_implementation(astnode, method)

    for struct in ctx.struct_types:
        with diagnostic_context(struct.location):
            for interface in struct.implements():
                # Check if the struct implements multiple times the same
                # interface.
                check_source_language(
                    struct.implements().count(interface) < 2,
                    "{} is implemented multiple times by {}".format(
                        interface.dsl_name,
                        struct.dsl_name,
                    )
                )
                if interface.is_always_node:
                    error(
                        "{}: {} should always be implemented by a node"
                        .format(struct.dsl_name, interface.dsl_name)
                    )

                for method in interface.methods.values():
                    fields = [
                        x for x in struct.get_fields()
                        if x.implements == method
                    ]
                    check_source_language(
                        len(fields) != 0,
                        "Missing implementation for field {} in struct {}"
                        .format(method.qualname, struct.dsl_name)
                    )
                    check_source_language(
                        len(fields) == 1,
                        "{} is implemented by multiple fields in struct {}: {}"
                        .format(
                            method.qualname,
                            struct.dsl_name,
                            ", ".join([x.qualname for x in fields])
                        )
                    )

                    check_interface_field(method, fields[0])
