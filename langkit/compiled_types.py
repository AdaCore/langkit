from __future__ import absolute_import, division, print_function

from collections import OrderedDict
import difflib
from itertools import count
import pipes

from langkit import names
from langkit.c_api import CAPIType
from langkit.common import get_type, null_constant, is_keyword
from langkit.diagnostics import (
    Context, check_source_language, extract_library_location
)
from langkit.template_utils import common_renderer
from langkit.utils import (common_ancestor, issubtype, memoized,
                           not_implemented_error, type_check)


def get_context(*args, **kwargs):
    """
    Return the current compilation context, see
    langkit.compile_context.get_context.

    TODO: this function exists only to workaround circular dependency issues.
    We should get rid of them.

    :rtype: CompileCtx
    """
    from langkit.compile_context import get_context
    return get_context(*args, **kwargs)


class GeneratedFunction(object):
    """
    Simple holder for functions' declaration/implementation generated code.
    """
    def __init__(self, name, declaration=None, implementation=None):
        self.name = name
        self.declaration = declaration
        self.implementation = implementation


class AbstractFieldAccessor(GeneratedFunction):
    """Generated function that expose field read access."""
    def __init__(self, name, field, c_declaration, **kwargs):
        super(AbstractFieldAccessor, self).__init__(name, **kwargs)
        self.field = field
        self.c_declaration = c_declaration


def c_node_type(capi):
    return CAPIType(capi, 'base_node')


def library_public_field(field):
    """
    Return whether we must generate accessors in APIs for this field.

    :param AbstractNodeData field: Field to test.
    :rtype: bool
    """
    return field.is_public or get_context().library_fields_all_public


def gdb_helper(*args):
    """
    Format given arguments into a special Ada comment for GDB helpers.

    :param list[str] args: Elements of the special comment.
    :rtype: str
    """
    return '--# {}'.format(' '.join(pipes.quote(a) for a in args))


def gdb_bind_var(var):
    """
    Output a GDB helper directive to bind a variable. This does nothing if the
    variable has no source name.

    :param ResolvedExpression var: The variable to bind.
    :rtype: str
    """
    return (gdb_helper('bind', var.abstract_var.source_name.lower,
                       var.name.camel_with_underscores)
            if var.abstract_var and var.abstract_var.source_name else '')


@memoized
def make_renderer(base_renderer=None):
    """
    Create a template renderer with common helpers.

    :param Renderer base_renderer: The renderer to base the resulting
        renderer on.
    """
    from langkit.expressions import PropertyDef

    if base_renderer is None:
        base_renderer = common_renderer

    template_args = {
        'is_enum':                type_check(_EnumType),
        'is_logic_var':           type_check(LogicVarType),
        'is_long':                type_check(LongType),
        'is_bool':                type_check(BoolType),
        'is_analysis_unit':       type_check(AnalysisUnitType),
        'is_analysis_kind':       type_check(AnalysisUnitKind),
        'is_struct':              type_check(StructType),
        'is_sloc_range':          type_check(SourceLocationRangeType),
        'is_token_type':          type_check(Token),
        'is_symbol_type':         type_check(Symbol),
        'is_array_type':          type_check(ArrayType),
        'is_lexical_env':         type_check(LexicalEnvType),
        'is_logic_var_type':      type_check(LogicVarType),
        'is_equation_type':       type_check(EquationType),
        'is_env_rebindings_type': type_check(EnvRebindingsType),
        'is_struct_type':         type_check(StructType),
        'no_builtins':
            lambda ts: filter(lambda t: not t.is_builtin(), ts),
        'LexicalEnvType':         LexicalEnvType,
        'EnvRebindingsType':      EnvRebindingsType,
        'PropertyDef':            PropertyDef,
    }
    if get_context():
        ctx = get_context()
        capi = ctx.c_api_settings

        # Name of the root AST node access type
        type_name = ctx.root_grammar_class.name()

        # Name of the root AST node record type
        value_type = type_name + names.Name("Type")

        # Name of the root AST node kind type
        kind_name = type_name + names.Name("Kind_Type")

        # Likewise, for the generic list type
        glist_type_name = ctx.generic_list_type.name()
        glist_value_type = ctx.generic_list_type.name() + names.Name("Type")

        template_args.update({
            'root_node_type_name':   type_name,
            'root_node_value_type':  value_type,
            'root_node_kind_name':   kind_name,
            'generic_list_type_name': glist_type_name,
            'generic_list_value_type': glist_value_type,
            'ctx':                   get_context(),
            'ada_lib_name':          get_context().ada_api_settings.lib_name,
            'T':                     T,
            'ada_api':               get_context().ada_api_settings,
            'capi':                  capi,
            'bool_type':             BoolType.c_type(capi).name,
            'analysis_context_type': CAPIType(capi, 'analysis_context').name,
            'analysis_unit_type':    AnalysisUnitType.c_type(capi).name,
            'node_kind_type':        CAPIType(capi, 'node_kind_enum').name,
            'node_type':             c_node_type(capi).name,
            'lexical_env_type':      LexicalEnvType.c_type(capi).name,
            'logic_var_type':        LogicVarType.c_type(capi).name,
            'equation_type':         EquationType.c_type(capi).name,
            'env_rebindings_type':   EnvRebindingsType.c_type(capi).name,
            'unit_kind_type':        AnalysisUnitKind.c_type(capi).name,
            'unit_provider_type':    CAPIType(capi, 'unit_provider').name,
            'unit_provider_destroy_type':
                CAPIType(capi, 'unit_provider_destroy_callback').name,
            'unit_provider_get_unit_from_node_type':
                CAPIType(capi,
                         'unit_provider_get_unit_from_node_callback').name,
            'unit_provider_get_unit_from_name_type':
                CAPIType(capi,
                         'unit_provider_get_unit_from_name_callback').name,
            'token_kind':            CAPIType(capi, 'token_kind').name,
            'token_type':            CAPIType(capi, 'token').name,
            'sloc_type':             CAPIType(capi, 'source_location').name,
            'sloc_range_type':       SourceLocationRangeType.c_type(capi).name,
            'text_type':             CAPIType(capi, 'text').name,
            'diagnostic_type':       CAPIType(capi, 'diagnostic').name,
            'exception_type':        CAPIType(capi, 'exception').name,
            'library_public_field':  library_public_field,
            'gdb_helper':            gdb_helper,
            'gdb_bind_var':          gdb_bind_var,
        })
    return base_renderer.update(template_args)


def render(*args, **kwargs):
    return make_renderer().render(*args, **kwargs)


class CompiledTypeMetaclass(type):
    """
    Metaclass for every compiled type. This is used to have a comprehensive
    list of every compiled type, so that you can use a TypeRepo instance to
    refer to any compiled type.
    """

    types = []
    """
    List of compiled types
    :type: [CompiledType]
    """

    type_dict = {}
    """
    Mapping: type name -> CompiledType subclass. Used in TypeRepo for type
    lookup by name.

    :rtype: dict[str, CompiledType]
    """

    def __new__(mcs, name, bases, dct):
        cls = type.__new__(mcs, name, bases, dct)

        # Always _internal here so its value is never inherited
        dct.setdefault("_internal", False)
        if not dct["_internal"]:
            mcs.types.append(cls)
            mcs.type_dict[cls.__name__] = cls

        if 'location' not in dct:
            dct["location"] = extract_library_location()

        return cls


class CompiledType(object):
    """
    Base class used to describe types in the generated code.

    It is intended to be subclassed in order to create now compiled types.
    However, subclasses are not intended to be instantiated.
    """

    __metaclass__ = CompiledTypeMetaclass

    _internal = True
    """
    Whether a type is a real type in code emission, or just an intermediate
    class used for code factoring purposes in python
    """

    location = None
    """
    Location of the declaration of this compiled type.

    :type: langkit.diagnostics.Location
    """

    is_ptr = True
    """
    Whether this type is handled through pointers only in the generated code
    """

    has_special_storage = False
    """
    Whether this type uses a special type for storage in structs and AST nodes.
    If this is true, the following class methods may be overriden:

    * storage_type_name;
    * storage_nullexpr;
    * extract_from_storage_expr;
    * convert_to_storage_expr.
    """

    is_struct_type = False
    """
    Whether this type is a subclass of StructType.
    """

    is_ast_node = False
    """
    Whether this type represents an AST node type.
    """

    is_list_type = False
    """
    Whether this type represents an instantiation of ASTList (i.e. a list of
    AST nodes).
    """

    is_entity_type = False
    """
    Whether this type represents an entity type.
    """

    should_emit_array_type = True
    """
    Whether the array type for this CompiledType is to be automatically
    generated in the AST.Types generated package. If not, hard-wired code in
    the AST package should emit do it. See CompileContext.array_types for more
    information.
    """

    _exposed = True
    """
    Whether the type should be exposed to the C and Python APIs. Note that all
    types are exposed anyway when the current context has the
    "library_all_fields_public" attribute set to True.
    """

    null_allowed = False
    """
    Whether a client is allowed to create a null value for this type. Note that
    a type might have a nullexpr and still have null disallowed, because
    nullexpr might make sense for initialization purposes in the parsers, but
    not as a real null value.
    """

    is_ada_record = False
    """
    Whether the type used in the C API is implemented as an Ada record. If so,
    we must pass them by reference in for C API interface functions.
    """

    def __init__(self):
        assert False, (
            'CompiledType subclasses are not meant to be instantiated'
        )

    @classmethod
    def diagnostic_context(cls):
        ctx_message = 'in {}'.format(cls.name().camel)
        return Context(ctx_message, cls.location)

    @classmethod
    def is_collection(cls):
        return False

    @classmethod
    def is_refcounted(cls):
        """
        Return whether this type matters for the ref-counting mechanism.

        This returns whether this type is dynamically allocated and thus
        ref-counted, or whether it owns ref-counted values. "dynamically" in
        this context means: with a lifetime that is different from the owning
        analysis unit.

        If it is ref-counted, there must exist Inc_Ref and Dec_Ref primitives.
        The Dec_Ref primitive must handle null values correctly (i.e. ignore
        them).

        :rtype: bool
        """
        return False

    @classmethod
    def element_type(cls):
        """
        CompiledType subclass for the type of elements contained in this list
        type. Must be overriden in subclasses.

        :rtype: CompiledType
        """
        raise NotImplementedError("element_type used on a non collection type")

    @classmethod
    def name(cls):
        """
        Return a names.Name instance to be used in code generation to reference
        this type. Must be overriden in subclasses.

        :rtype: names.Name
        """
        raise not_implemented_error(cls, cls.name)

    @classmethod
    def storage_type_name(cls):
        """
        Return the name of the type that is used to store instances of this
        type in structs and ASTNodes. See documentation for
        has_special_storage.

        :rtype: str
        """
        return cls.name()

    @classmethod
    def extract_from_storage_expr(cls, node_expr, base_expr):
        """
        Turn a storage value into a public value. See documentation for
        has_special_storage.

        :param str node_expr: Expression that yields the node that owns the
            storage value.
        :param str base_expr: Expression that yields the storage value.
        :return: An expression that yields the public value.
        :rtype: str
        """
        return base_expr

    @classmethod
    def convert_to_storage_expr(cls, node_expr, base_expr):
        """
        Turn a public value into a storage value. See documentation for
        has_special_storage.

        :param str node_expr: Expression that yields the node that will own the
            resulting storage value.
        :param str base_expr: Expression that yields the public value:
        :return: An expression that yields the storage value.
        :rtype: str
        """
        return base_expr

    @classmethod
    def nullexpr(cls):
        """
        Return a string to be used in code generation for "null" expressions.

        Must be overriden in subclasses.

        :rtype: str
        """
        raise not_implemented_error(cls, cls.nullexpr)

    @classmethod
    def py_nullexpr(cls):
        """
        Return a string to be used in Python code gen for "null" expressions.

        Must be overridden in subclasses... for which we need the Python null
        expression.

        :rtype: str
        """
        raise not_implemented_error(cls, cls.py_nullexpr)

    @classmethod
    def storage_nullexpr(cls):
        """
        Return the nullexpr that is used for fields of this type in structs and
        ASTNodes. See documentation for has_special_storage.

        :rtype: str
        """
        return cls.nullexpr()

    @classmethod
    def doc(cls):
        """
        Return the user documentation for this type, or None if there is no
        documentation.

        :rtype: None|str
        """
        return None

    @classmethod
    def c_type(cls, c_api_settings):
        """Return a CAPIType instance for this type.

        Must be overriden in subclasses.

        :param CAPISettings c_api_settings: The settings for the C API.
        """
        raise not_implemented_error(cls, cls.c_type)

    @classmethod
    def unify(cls, other, error_msg=None):
        """
        If "cls" and "other" are types that match, return the most general type
        to cover both. Create an error diagnostic if they don't match.

        :param CompiledType cls: Type parameter.
        :param CompiledType other: Type parameter.
        :param str|None error_msg: Diagnostic message for mismatching types. If
            None, a generic one is used, otherwise, we call .format on it with
            the `cls` and `other` keys being the names of mismatching types.
        :rtype: CompiledType
        """

        # ASTNodeType subclasses (and thus entities) always can be unified:
        # just take the most recent common ancestor.
        if cls.is_entity_type and other.is_entity_type:
            return common_ancestor(cls.el_type, other.el_type).entity()
        elif cls.is_ast_node and other.is_ast_node:
            return common_ancestor(cls, other)

        # Otherwise, we require a strict subtyping relation
        check_source_language(
            cls.matches(other),
            (error_msg or 'Mismatching types: {cls} and {other}').format(
                cls=cls.name().camel, other=other.name().camel
            )
        )
        return cls

    @classmethod
    def matches(cls, formal):
        """
        Return whether the "cls" type matches "formal".

        This is mere equality for all types but AST nodes, in which "cls" is
        allowed to be a subclass for "formal".

        :param CompiledType cls: Type parameter.
        :param CompiledType formal: Type parameter.
        :rtype: bool
        """

        if cls.is_entity_type and formal.is_entity_type:
            return cls.el_type.matches(formal.el_type)

        if formal.is_ast_node:
            return issubclass(cls, formal)
        else:
            return cls == formal

    # Likewise for array types
    @classmethod
    @memoized
    def array_type(cls):
        """
        Create an array type for "cls".

        :rtype: ArrayType
        """
        element_type = cls
        cls = type(
            b'{}ArrayType'.format(element_type.name().camel), (ArrayType, ), {
                'element_type': classmethod(lambda cls: element_type),
            }
        )
        ctx = get_context(True)
        if ctx:
            ctx.array_types.add(cls)
        else:
            StructMetaclass.pending_array_types.append(cls)
        return cls


class NoCompiledType(CompiledType):
    """
    Special CompiledType subclass to mean the absence of type. This is useful
    in special resolved expressions.
    """

    @classmethod
    def is_refcounted(cls):
        raise NotImplementedError()

    @classmethod
    def name(cls):
        return names.Name('No_Compiled_Type')


class BasicType(CompiledType):
    """
    Base class used to describe simple types that do not need declaration code
    generation.
    """
    _name = None
    _nullexpr = None
    _storage_nullexpr = None
    _external = False
    _internal = True
    _exposed = True

    @classmethod
    def name(cls):
        return names.Name(cls._name)

    @classmethod
    def nullexpr(cls):
        return cls._nullexpr

    @classmethod
    def storage_nullexpr(cls):
        return (cls._storage_nullexpr
                if cls._storage_nullexpr
                else cls.nullexpr())

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, cls.name(), external=cls._external)


class AnalysisUnitType(BasicType):
    _name = "Analysis_Unit"
    _nullexpr = "null"
    is_ptr = True
    should_emit_array_type = True
    null_allowed = True

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'analysis_unit')


class AnalysisUnitKind(BasicType):
    _name = 'Unit_Kind'
    is_ptr = False

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'unit_kind')


class LexicalEnvType(BasicType):
    _name = "Lexical_Env"
    _nullexpr = "Empty_Env"
    is_ptr = True
    should_emit_array_type = False
    null_allowed = True
    _exposed = False

    @classmethod
    def is_refcounted(cls):
        return True

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'lexical_env')

    @classmethod
    def py_nullexpr(cls):
        return 'LexicalEnv.Empty'


class LogicVarType(BasicType):
    """
    This type represents a logical variable. A logical variable is a reference,
    that can take a specific value when it is bound to a specific Equation, and
    that equation is solved for a certain problem set.

    It is a special type in Langkit at this point, in that its value is
    initially undefined, and is mutated after the solve of an equation to a
    specific value. It is thus the only type in Langkit for the moment for
    which variables of the type can be mutated after creation.

    TODO: For the moment a logic variable is necessarily a holder around an
    ASTNodeType. At some point we might want to make it generic, like list and
    array types.
    """
    _name = "Logic_Var"
    _nullexpr = "null"
    _storage_nullexpr = "Null_Var_Record"
    is_ptr = False
    has_special_storage = True
    _exposed = False

    @classmethod
    def storage_type_name(cls):
        return names.Name("Logic_Var_Record")

    @classmethod
    def extract_from_storage_expr(cls, node_expr, base_expr):
        del node_expr
        return "{}'Unrestricted_Access".format(base_expr)

    @classmethod
    def convert_to_storage_expr(cls, node_expr, base_expr):
        raise not_implemented_error(cls, cls.convert_to_storage_expr)

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'logic_var_type')


class EquationType(BasicType):
    """
    An EquationType instance is an equation where logic variables (of type
    LogicVarType) are involved in logical expressions.

    An equation can be solved, and the variables instances will then be bound
    to specific values.

    Equations instance will typically be produced by expressions involving
    logic variables.
    """
    _name = "Logic_Equation"
    _nullexpr = "Null_Logic_Equation"
    is_ptr = True
    _exposed = False
    null_allowed = True

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'equation_type')

    @classmethod
    def is_refcounted(cls):
        return True


class EnvRebindingsType(BasicType):
    """
    Data type for environment rebinding.
    """
    _name = "Env_Rebindings"
    is_ptr = True
    null_allowed = True
    _nullexpr = "null"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'env_rebindings_type')

    @classmethod
    def is_refcounted(cls):
        return True

    @classmethod
    def py_nullexpr(cls):
        return 'None'

    @classmethod
    def c_inc_ref(cls, capi):
        """
        Name of the C API function to inc-ref an env rebindings value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(cls.name() + names.Name('Inc_Ref'))

    @classmethod
    def c_dec_ref(cls, capi):
        """
        Name of the C API function to dec-ref an env rebindings value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(cls.name() + names.Name('Dec_Ref'))


class BoolType(BasicType):
    is_ptr = False
    _name = get_type(bool)
    _nullexpr = "false"

    @classmethod
    def c_type(cls, c_api_settings):
        # "bool" is not a built-in type in C: we define our own type based on
        # uint8_t.
        return CAPIType(c_api_settings, 'bool')

    @classmethod
    def py_nullexpr(cls):
        return 'False'


class LongType(BasicType):
    is_ptr = False
    _name = get_type(long)
    _nullexpr = "0"
    _external = True

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'int', external=cls._external)


class SourceLocationRangeType(BasicType):
    is_ptr = False
    _name = "SourceLocationRange"
    _nullexpr = "SourceLocationRange()"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'source_location_range')


class Token(BasicType):
    is_ptr = False
    _name = "Token_Type"
    _nullexpr = "No_Token"
    is_ada_record = True

    has_special_storage = True

    @classmethod
    def storage_type_name(cls):
        return 'Token_Index'

    @classmethod
    def storage_nullexpr(cls):
        return 'No_Token_Index'

    @classmethod
    def extract_from_storage_expr(cls, node_expr, base_expr):
        return 'Token ({}, {})'.format(node_expr, base_expr)

    @classmethod
    def convert_to_storage_expr(cls, node_expr, base_expr):
        return 'Stored_Token ({}, {})'.format(node_expr, base_expr)

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'token')


class Symbol(BasicType):
    is_ptr = True
    _name = "Symbol_Type"
    _nullexpr = "null"

    # See below: symbols are represented in the C API as text records
    is_ada_record = True

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'text')


class Argument(object):
    """
    Holder for properties arguments.
    """

    def __init__(self, name, type, is_optional=False, is_artificial=False,
                 abstract_var=None):
        """
        :param names.Name name: Argument name.
        :param CompiledType type: Argument type.
        :param bool is_optional: Whether the argument is optional. Note that
            only properties can accept optional arguments, and such arguments
            must be artificial.
        :param bool is_artificial: Whether the argument was automatically
            created by Langkit, i.e. the language specification did not mention
            it.
        :param AbstractVariable|None abstract_var: For properties only. If
            provided, use it as the abstract variable to reference this
            argument. If not provided, an AbstractVariable instance is
            automatically created.
        """
        from langkit.expressions.base import AbstractVariable
        self.name = name
        self.var = (abstract_var
                    or AbstractVariable(name, type, source_name=name))
        self.is_optional = is_optional
        self.is_artificial = is_artificial
        assert not is_optional or is_artificial

    @property
    def type(self):
        return self.var.type

    def __repr__(self):
        return '<Argument {} : {}>'.format(self.name.lower,
                                           self.type.name().camel)


class AbstractNodeData(object):
    """
    This class defines an abstract base class for fields and properties on
    AST nodes.

    It defines the basis of what is needed to bind them in other languages
    bindings: a type and a name.
    """

    # Hack: the field declarations order in AST nodes matters.  The simple and
    # very handy syntax we use here for such declarations doesn't preserve this
    # order in Python2, however.  Waiting for the move to Python3, we use a
    # hack here: the following counter will help us to recover the declaration
    # order (assuming it is the same as the Field instantiation order).
    _counter = iter(count(0))

    is_property = False
    """
    Whether this class is Property (to be overriden in the Property subclass).
    :type: bool
    """

    PREFIX_FIELD = names.Name('F')
    PREFIX_PROPERTY = names.Name('P')
    PREFIX_INTERNAL = names.Name('Internal')

    prefix = None
    """
    This can be overriden in subclasses of AbstractNodeData to add a prefix to
    the name of AbstractNodeData instances.

    :type: names.Name|None
    """

    def __init__(self, name=None, public=True, access_needs_incref=False):
        """
        :param names.Name|None name: Name for this field. Most of the time,
            this is initially unknown at field creation, so it is filled only
            at struct creation time.

        :param bool|None public: Whether this AbstractNodeData instance is
            supposed to be public or not.

            In the context of properties only, None is also allowed: in this
            case, inherit vibility from parents. If there is no property to
            override and None is passed, make the property private. This is
            computed in the "compute" pass.

        :param bool access_needs_incref: If True, field access evaluation does
            not create an ownership share: callers must call Inc_Ref
            themselves. See the eponym property.
        """
        self._index = next(self._counter)
        self._is_public = public

        self.location = extract_library_location()

        self._name = name

        self.struct = None
        """
        StructType subclass that declared this field. Initialized when creating
        StructType subclasses.
        :type: StructType
        """

        self.arguments = []
        """
        Code generation-wise, all node data can be considered as functions
        which take at least a mandatory Self argument and return the
        corresponding data.

        This is a list that describes all other arguments. Note that only
        Property instances accept other arguments.

        :type: list[Argument]
        """

        self._cached_name = None
        """
        Cache for the name property.

        :type: None|names.Name
        """

        self.uses_entity_info = False
        self._access_needs_incref = access_needs_incref

    @property
    def is_overriding(self):
        """
        Return whether this field overrides an inheritted one in a base class.

        :rtype: bool
        """
        from langkit.expressions import PropertyDef

        assert self._name and self.struct
        if not self.struct.is_ast_node:
            return False
        parent_cls = self.struct.base()
        properties_to_override = [p._name
                                  for p in parent_cls.get_properties()]
        return (isinstance(self, PropertyDef) and
                self._name in properties_to_override)

    def diagnostic_context(self):
        ctx_message = 'in {}.{}'.format(self.struct.name().camel,
                                        self._name.lower)
        return Context(ctx_message, self.location)

    @property
    def is_public(self):
        """
        Whether this field is private.
        :rtype: bool
        """
        assert self._is_public is not None
        return self._is_public

    @property
    def is_private(self):
        """
        Whether this field is public.

        This is a shortcut for::
            not self.is_public

        :rtype: bool
        """
        return not self.is_public

    @property
    def is_internal(self):
        """
        Whether this property is internal.
        :rtype: bool
        """
        return self._name.base_name.startswith('_')

    @property
    def type(self):
        """
        Type of the abstract node field.
        :rtype: langkit.compiled_types.CompiledType
        """
        raise not_implemented_error(self, type(self).type)

    @type.setter
    def type(self, type):
        raise not_implemented_error(self, type(self).type)

    def c_type_or_error(self, capi):
        """
        Within a diagnostic context for this field, return its C API type.

        :rtype: CAPIType
        """
        with self.diagnostic_context():
            return self.type.c_type(capi)

    @property
    def name(self):
        """
        :rtype: names.Name
        """
        if not self._cached_name:
            assert self._name

            # If this is an internal property, the name has an underscore
            # prefix that we want to get rid of for code generation.
            radix = (names.Name(self._name.base_name[1:])
                     if self.is_internal else
                     self._name)

            self._cached_name = self.prefix + radix if self.prefix else radix
        return self._cached_name

    @property
    def qualname(self):
        """
        Return the qualified name for this field, i.e. the name of the owning
        type plus the name of the field itself. This is useful for diagnostic
        messages.
        :rtype: str
        """
        return '{}.{}'.format(
            self.struct.name().camel if self.struct else '<unresolved>',
            self.name.lower if self._name else '<unresolved>'
        )

    def __repr__(self):
        return '<{} {}>'.format(
            type(self).__name__,
            self.qualname
        )

    @name.setter
    def name(self, name):
        assert isinstance(name, names.Name)
        self._name = name

    def doc(self):
        """
        Documentation for the abstract node field.
        :rtype: str
        """
        raise not_implemented_error(self, type(self).doc)

    @property
    def accessor_basename(self):
        """
        Return the base name for the accessor we generate for this field.

        Note that this is available only for fields attached to AST nodes.

        :rtype: names.Name
        """
        assert self.struct
        return self.struct.name() + self.name

    @property
    def exposed_optional_arguments(self):
        """
        Return the subset of "self.arguments" that are optional arguments for
        which the type is exposed in public APIs.

        This property makes sense in a code generation context.

        :rtype: list[Argument]
        """
        return []

    @property
    def mandatory_arguments(self):
        """
        Return the subset of "self.arguments" that are mandatory arguments,
        i.e. excluding optional arguments.

        This property makes sense in a code generation context.

        :rtype: list[Argument]
        """
        return self.arguments

    @property
    def natural_arguments(self):
        """
        Return the subset of "self.arguments" that are non-artificial
        arguments, that is to say the subset that users actually handle in
        expressions.

        This property makes sense in user-facing layers.

        :rtype: list[Argument]
        """
        return self.arguments

    @property
    def exposed_arguments(self):
        """
        Shortcut for mandatory_arguments + exposed_optional_arguments.

        :rtype: list[Argument]
        """
        return self.mandatory_arguments + self.exposed_optional_arguments

    @classmethod
    def filter_fields(cls, mapping):
        """
        Return a list of tuples (name, value) for all fields in mapping.

        :type mapping: dict[str, AbstractNodeData]
        :rtype: list[(str, AbstractNodeData)]
        """
        return [(f_n, f_v)
                for f_n, f_v in mapping.items()
                if isinstance(f_v, cls)]

    @property
    def access_needs_incref(self):
        """
        Return whether field access evaluation does not create an ownership
        share. In this case, users must call Inc_Ref themselves. This returns
        always False for node data whose type is not ref-counted.

        :rtype: bool
        """
        return self.type.is_refcounted() and self._access_needs_incref


class AbstractField(AbstractNodeData):
    """
    Placeholder descriptors used to associate data to AST nodes (see below).
    """

    concrete = False
    """
    Field used to prevent instantiation of the class. Concrete descendants
    of AbstractField must put that field to True in their definition.
    """

    prefix = AbstractNodeData.PREFIX_FIELD

    def __init__(self, repr=True, doc=None, type=None,
                 access_needs_incref=False):
        """
        Create an AST node field.

        :param bool repr: If true, the field will be displayed when
            pretty-printing the embedding AST node.
        :param str|None doc: User documentation for this field.
        :param bool access_needs_incref: See AbstractNodeData's constructor.
        """

        if not self.concrete:
            raise NotImplementedError()

        super(AbstractField, self).__init__(
            public=True, access_needs_incref=access_needs_incref
        )

        self.repr = repr
        self._name = None
        self._doc = doc

        self.should_emit = True
        """
        Subclasses can change that variable to trigger wether the field
        should be emitted or not.
        """

        self._type = type
        """
        Type of the field. If not set, it will be set to a concrete
        CompiledType subclass after type resolution. If set, it will be
        verified at type resolution time.
        :type: CompiledType
        """

    @property
    def type(self):
        """
        :rtype: CompiledType
        """
        self._type = resolve_type(self._type)
        return self._type

    @type.setter
    def type(self, type):
        assert issubclass(type, CompiledType)
        self._type = type

    def __repr__(self):
        return '<ASTNode {} Field({})>'.format(self._index, self.qualname)

    def doc(self):
        return self._doc


class Field(AbstractField):
    """
    Fields that are meant to store parsing results. Can be used only on
    subclasses of ASTNodeType.
    """
    concrete = True


class UserField(AbstractField):
    """
    Fields that are not meant to store parsing results. Can be used on any
    Node type, will be ignored by the parsing code.
    """

    def __init__(self, type, repr=False, doc=None, public=True,
                 access_needs_incref=False):
        """
        See inherited doc. In this version we just ensure that a type is
        passed because it is mandatory for data fields. We also set repr to
        False because most of the time you don't want User fields to show up in
        the pretty printer.

        :type type: CompiledType
        :type doc: str

        :param bool is_public: Whether this field is public in the generated
            APIs.

        :param bool access_needs_incref: See AbstractNodeData's constructor.
        """
        super(UserField, self).__init__(
            repr, doc, type, access_needs_incref=access_needs_incref
        )
        self._is_public = public

    concrete = True


class BuiltinField(UserField):
    """
    A built-in field is just like a UserField, except that its name has no
    prefix. It is disregarded by the parsing machinery too. It is typically
    used for fields on the root node that don't really exist/are added
    manually.
    """

    prefix = None

    def __init__(self, *args, **kwargs):
        super(BuiltinField, self).__init__(*args, **kwargs)
        self.should_emit = False


# These will be replaced by true class definitions. Before this happens,
# StructMetaclass will see these None values.
StructType = None
ASTNodeType = None


class StructMetaclass(CompiledTypeMetaclass):
    """
    Internal metaclass for AST nodes, used to ease fields handling during code
    generation.
    """

    astnode_types = []
    """
    List of ASTNodeType types. This list is updated every time a new astnode
    type is created.

    :type: list[ASTNodeType]
    """

    struct_types = []
    """
    List of all plain struct types.

    :type: list[StructType]
    """

    pending_list_types = []
    """
    Set of ASTNodeType subclasses for list types that are created while there
    is no context.

    :type: list[ASTNodeType]
    """

    pending_array_types = []
    """
    Set of ArrayType subclasses that are created while there is no context.

    :type: list[langkit.compiled_types.ArrayType]
    """

    root_grammar_class = None
    """
    The class used as a root for the whole ASTNodeType hierarchy for the
    currently compiled grammar. Every AST node must derive directly or
    indirectly from that class.

    :type: ASTNodeType
    """

    env_metadata = None
    """
    The class designing the type used as metadata for the lexical environments
    values. Must be a pure struct type.

    :type: StructType
    """

    entity_info = None
    """
    StructType subclass to contain all entity information, except the node
    itself.

    :type: StructType
    """

    def __new__(mcs, name, bases, dct):
        # The two following booleans are mutually exclusive and at least one
        # will be True.
        is_astnode = False
        is_struct = False

        # The following are also mutually exclusive but they can be all False
        is_base = False  # Base StructType/ASTNodeType?
        is_root_grammar_class = False  # Root grammar class?

        location = dct.get('_location', extract_library_location())
        diag_ctx = Context('in {}'.format(name), location)

        def field_ctx(field_name):
            return Context('in {}.{}'.format(name, field_name), location)

        assert len(bases) == 1, (
            "Multiple inheritance for AST nodes is not supported"
        )
        base, = bases

        # No matter what, reset all caches so that subclass don't "magically"
        # inherit their parent's.
        dct['_cached_user_name'] = None
        dct['_abstract_fields_dict_cache'] = {}

        # We want to check various inheritance facts:
        #
        # * Every type deriving from StructType must derive from StructType
        #   itself (no further subclassing).
        #
        # * Every type deriving from ASTNodeType must derive from a single user
        #   defined subclass of ASTNodeType: the root grammar class.
        #
        # Of course this does not apply to StructType and ASTNodeType
        # themselves, which are created before all other classes. The root
        # grammar class also requires special handling.
        base_classes = [StructType, ASTNodeType]
        with diag_ctx:
            if not all(base_classes):
                is_base = True
                is_struct = StructType is None
                is_astnode = not is_struct and ASTNodeType is None

            elif base.is_ast_node:
                is_astnode = True
                # If we have no root grammar class yet and reach this point,
                # the type necessarily derives from ASTNodeType. It's the root
                # grammar class.
                if mcs.root_grammar_class is None:
                    assert base is ASTNodeType
                    is_root_grammar_class = True

                else:
                    # Check that it does indeed derives from the root grammar
                    # class.
                    assert issubclass(base, mcs.root_grammar_class)

                dct['is_root_node'] = is_root_grammar_class

            else:
                assert base is StructType
                is_struct = True

        # This is a formal explanation for the top comments:
        assert sum(1 for b in [is_astnode, is_struct] if b) == 1
        assert sum(1 for b in [is_base, is_root_grammar_class] if b) <= 1

        # Get the fields this class defines. Remove them as class members: we
        # want them to be stored in their own dict (see "cls.fields" below).
        dct_fields = [] if is_base else dct['_fields']

        env_spec = dct.get('_env_spec', None)
        assert env_spec is None or is_astnode
        dct['is_env_spec_inherited'] = env_spec is None
        dct['env_spec'] = env_spec

        dct['should_emit_array_type'] = (
            dct.get('should_emit_array_type', True) and
            not is_root_grammar_class
        )
        dct['location'] = location
        dct['_doc'] = dct.get('_doc')

        # List types are resolved by construction: we create list types to
        # contain specific ASTNodeType subclasses. All other types are not
        # resolved, only the grammar will resolve them.
        dct['is_type_resolved'] = (
            is_astnode and
            (base.is_list_type or
             dct.get('is_list_type', False) or
             dct.get('is_generic_list_type', False))
        )

        # By default, ASTNodeType subtypes aren't abstract. The "abstract"
        # decorator may change this attribute later. Likewise for synthetic
        # nodes and nodes whose root list type is abstract.
        dct['abstract'] = (
            is_root_grammar_class or dct.get('_is_abstract', False)
        )
        dct['synthetic'] = dct.get('_is_synthetic', False)
        dct['has_abstract_list'] = dct.get('_has_abstract_list', False)

        # This metaclass will register subclasses automatically
        dct['subclasses'] = []

        cls = CompiledTypeMetaclass.__new__(mcs, name, bases, dct)

        # Now we have a class object, register it wherever it needs to be
        # registered.
        if is_root_grammar_class:
            mcs.root_grammar_class = cls

        elif is_astnode:
            base.subclasses.append(cls)

        if not is_base:
            if is_struct:
                mcs.struct_types.append(cls)
            else:
                mcs.astnode_types.append(cls)

        # This builds a list of fields in a specific order: first builtin
        # fields, then fields from `dct`, sorted by field number (see
        # AbstractNodeData).
        fields = mcs.merge_fields(
            ([mcs.builtin_properties()] if is_root_grammar_class else [])
            + [dct_fields]
        )

        # Associate each field and property to this ASTNodeType subclass, and
        # assign them their name. Likewise for the environment specification.
        for f_n, f_v in fields.items():
            f_v.struct = cls
            f_v.name = names.Name.from_lower(f_n)
        if env_spec:
            env_spec.ast_node = cls

        cls._fields = fields

        # If this is the root grammar type, create the generic list type name
        if is_root_grammar_class:
            generic_list_type_name = (
                dct.pop('_generic_list_type', None)
                or cls.__name__ + 'BaseList'
            )

            @classmethod
            def element_type(cls):
                # The generic list type is not a real list type: only its
                # subclasses will have a specific element type.
                raise not_implemented_error(cls, cls.element_type)

            cls.generic_list_type = type(
                generic_list_type_name,
                (cls, ),
                {
                    'nullexpr': classmethod(lambda cls: null_constant()),
                    'is_generic_list_type': True,
                    'element_type': element_type,
                    '_fields': [],
                    '_is_abstract': True,
                }
            )

        return cls

    @classmethod
    def builtin_properties(mcs):
        """
        Return properties available for all AST nodes.

        Note that mcs.root_grammar_class must be defined first.

        :rtype: list[(str, AbstractNodeData)]
        """
        from langkit.expressions import PropertyDef

        assert mcs.root_grammar_class
        # Note that we must not provide implementation for them here (no
        # expression) since the implementation comes from the hard-coded root
        # AST node type definition.
        return [
            # The following fields return LexicalEnvType values, which are
            # ref-counted. However these specific envs are owned by the
            # analysis unit, so they are not ref-counted.

            ("node_env", PropertyDef(
                expr=None, prefix=None, type=LexicalEnvType,
                public=False, external=True, uses_entity_info=True,
                force_dispatching=True, warn_on_unused=False,
                doc='For nodes that introduce a new environment, return the'
                    ' parent lexical environment. Return the "inherited"'
                    ' environment otherwise.'
            )),
            ("children_env", PropertyDef(
                expr=None, prefix=None, type=LexicalEnvType,
                public=False, external=True, uses_entity_info=True,
                warn_on_unused=False,
                doc='For nodes that introduce a new environment, return it.'
                    ' Return the "inherited" environment otherwise.'
            )),

            ("parent", BuiltinField(
                type=mcs.root_grammar_class,
                doc="Return the lexical parent for this node. Return null for"
                    " the root AST node or for AST nodes for which no one has"
                    " a reference to the parent."
            )),

            # The following builtin fields are implemented as a property, so
            # there is no need for an additional inc-ref.
            ("parents", BuiltinField(
                type=mcs.root_grammar_class.array_type(),
                doc="Return an array that contains the lexical parents (this"
                    " node included). Nearer parents are first in the list."
            )),
            ("children", BuiltinField(
                type=mcs.root_grammar_class.array_type(),
                doc="Return an array that contains the direct lexical "
                    "children "
            )),
            ("token_start", PropertyDef(
                expr=None, prefix=None, type=Token,
                public=True, external=True, uses_entity_info=False,
                doc="Return the first token used to parse this node."
            )),
            ("token_end", PropertyDef(
                expr=None, prefix=None, type=Token,
                public=True, external=True, uses_entity_info=False,
                doc="Return the last token used to parse this node."
            )),
            ("previous_sibling", BuiltinField(
                type=mcs.root_grammar_class,
                doc="Return the node's previous sibling, if there is one"
            )),
            ("next_sibling", BuiltinField(
                type=mcs.root_grammar_class,
                doc="Return the node's next sibling, if there is one"
            )),
        ]

    @staticmethod
    def merge_fields(fields_groups):
        """
        Merge groups of AbstractNodeData instances into an OrderedDict.

        The resulting OrderedDict preserves the input order.

        :type fields_groups: list[list[(str, AbstractNodeData)]]
        :rtype: dict[str, AbstractNodeData]
        """
        return OrderedDict(sum(
            [sorted(group, key=lambda (_, f): f._index)
             for group in fields_groups],
            []
        ))


class TypeDeclaration(object):
    """Simple holder for generated type declarations."""

    def __init__(self, type, public_part, private_part):
        """

        :param ASTNodeType type: The type that this TypeDeclaration holds onto.
        :param str public_part: The generated code for the public part of
            the type declaration.
        :param private_part: The generated code for the private part of the
            type declaration.
        """
        self.type = type
        self.public_part = public_part
        self.private_part = private_part

    @staticmethod
    def render(template_name, t_env, type, **kwargs):
        """
        Helper to create a TypeDeclaration out of the instantiations of a
        single template.

        :param str template_name: The name of the template.
        :param TemplateEnvironment|None t_env: The environment to use for
            rendering.
        :param CompiledType type: Type parameter. The type to render.
        :param dict kwargs: Additional arguments to pass to the mako render
            function.
        """
        return TypeDeclaration(
            type,
            render(template_name, t_env, private_part=False, **kwargs),
            render(template_name, t_env, private_part=True, **kwargs)
        )


class StructType(CompiledType):
    """
    Base class for all user struct-like composite types, such as POD structs
    and AST nodes.

    User subclasses deriving from StructType will define by-value POD types
    that cannot be subclassed themselves.
    """

    dsl_decl = None
    """
    Struct subclass coming from the language specification DSL, from which this
    StructType was created. None if this is a synthetic struct type.

    :type: langkit.dsl.Struct|None
    """

    env_spec = None
    ":type: langkit.compiled_types.EnvSpec"

    _fields = OrderedDict()
    """
    The fields for this StructType, instantiated by the metaclass
    :type: dict[str, Field]
    """

    __metaclass__ = StructMetaclass
    is_ptr = False
    null_allowed = True

    is_struct_type = True

    _cached_user_name = None
    """
    Cache for the _user_name method.
    :type: None|names.Name
    """

    _abstract_fields_dict_cache = {}
    """
    Cache for the get_abstract_fields_dict class method.

    For each couple of parameters (include_inherited : bool, field_class :
    AbsractNodeData

    :type: dict[(bool, AbstractNodeData), dict[str, AbstractField]]
    """

    is_ada_record = True

    # ASTNodeType subclasses are exposed by default, and a compile pass will
    # tag all StructType subclasses that are exposed through the public API.
    _exposed = False

    @classmethod
    @memoized
    def is_refcounted(cls):
        return any(f.type.is_refcounted() for f in cls._fields.values())

    @classmethod
    def is_builtin(cls):
        """
        Some structs are considered "built-in", which means that either no
        code needs to be emitted for them, either special code will be
        emitted on a special path, and we can omit them from regular code
        generation.

        :rtype: bool
        """
        return cls in (
            # The root grammar class and the generic list types are emitted
            # separately from the others.
            StructMetaclass.root_grammar_class,
            StructMetaclass.root_grammar_class.generic_list_type,

            # The env metadata struct is emitted separately from the others
            T.env_md,

            # Entitiy info and the root node's entity type is not emitted per
            # se, because it is a generic instantiation from
            # Langkit_Support.Lexical_Env.
            StructMetaclass.root_grammar_class.entity_info(),
            StructMetaclass.root_grammar_class.entity(),
        )

    @classmethod
    def set_types(cls, types):
        """
        Associate `types` (a list of CompiledType) to fields in `cls` . It is
        valid to perform this association multiple times as long as types are
        consistent.

        :type types: list[CompiledType]
        """
        fields = cls.get_parse_fields()

        check_source_language(
            len(fields) == len(types), "{} has {} fields ({} types given). You"
            " probably have inconsistent grammar rules and type "
            "declarations".format(cls.name().camel, len(fields), len(types))
        )

        # TODO: instead of expecting types to be subtypes, we might want to
        # perform type unification (take the nearest common ancestor for all
        # field types). But then again, maybe not, it might be too confusing.
        for field, f_type in zip(fields, types):
            if field.type:
                check_source_language(
                    issubclass(f_type, field.type),
                    "Field {} already had type {}, got {}".format(
                        field.qualname, field.type.name(), f_type.name()
                    )
                )

        # Only assign types if cls was not yet typed. In the case where it
        # was already typed, we checked above that the new types were
        # consistent with the already present ones.
        if not cls.is_type_resolved:
            cls.is_type_resolved = True

            for inferred_type, field in zip(types, fields):

                # At this stage, if the field has a type, it means that the
                # user assigned it one originally. In this case we will use the
                # inferred type for checking only (raising an assertion if it
                # does not correspond).
                if field.type:
                    with field.diagnostic_context():
                        check_source_language(
                            # Using matches here allows the user to annotate a
                            # field with a more general type than the one
                            # inferred.
                            inferred_type.matches(field.type),
                            'Expected type {} but type inferenced yielded type'
                            ' {}'.format(
                                field.type.name().camel,
                                inferred_type.name().camel
                            )
                        )
                else:
                    field.type = inferred_type

    @classmethod
    def add_field(cls, field):
        """
        Append a field to this Struct/AST node. This is not valid if a field
        lookup has already been done on this Struct/AST node.

        :param AbstractNodeData field: Field to append.
        """
        assert not cls._abstract_fields_dict_cache
        cls._fields[field._name.lower] = field
        field.struct = cls

    @classmethod
    def get_inheritance_chain(cls):
        """
        Return a list for all classes from ASTNodeType to `cls` in the
        inheritance chain.

        :rtype: list[ASTNodeType]
        """
        return reversed([base_class for base_class in cls.mro()
                         if getattr(base_class, 'is_ast_node', False)])

    @classmethod
    def get_properties(cls, predicate=None, include_inherited=True):
        """
        Return the list of all the fields `cls` has.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[langkit.expressions.base.PropertyDef]
        """
        return cls.get_abstract_fields(
            lambda f: f.is_property and (predicate is None or predicate(f)),
            include_inherited
        )

    @classmethod
    def get_memoized_properties(cls, include_inherited=False):
        """
        Return the list of all memoized properties `cls` has.

        :param bool include_inherited: If true, include inheritted properties
            in the returned list. Return only properties that were part of the
            declaration of this node otherwise.

        :rtype: list[langkit.expressions.base.PropertyDef]
        """
        return cls.get_properties(lambda p: p.memoized, include_inherited)

    @classmethod
    def get_parse_fields(cls, predicate=None, include_inherited=True):
        """
        Return the list of all the parse fields `cls` has, including its
        parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[Field]
        """
        return cls.get_abstract_fields(predicate, include_inherited,
                                       field_class=Field)

    @classmethod
    def get_user_fields(cls, predicate=None, include_inherited=True):
        """
        Return the list of all the user fields `cls` has, including its
        parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[UserField]
        """
        return cls.get_abstract_fields(predicate, include_inherited,
                                       field_class=UserField)

    @classmethod
    def get_fields(cls, predicate=None, include_inherited=True):
        """
        Return the list of all the fields `cls` has, including its parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[AbstractField]
        """
        return cls.get_abstract_fields(predicate, include_inherited,
                                       field_class=AbstractField)

    @classmethod
    def get_abstract_fields(cls, predicate=None, include_inherited=True,
                            field_class=AbstractNodeData):
        """
        Get all AbstractField instances for the class.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(AbstractNodeData) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :param type field_class: The field class to use to filter fields.

        :rtype: list[AbstractNodeData]
        """
        return filter(
            predicate or (lambda f: True),
            cls.get_abstract_fields_dict(include_inherited,
                                         field_class).values()
        )

    @classmethod
    def get_abstract_fields_dict(cls, include_inherited=True,
                                 field_class=AbstractNodeData):
        """
        Get all AbstractField instances for the class.

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :param type field_class: The field class to use to filter fields.

        :rtype: dict[str, AbstractNodeData]
        """
        assert issubclass(field_class, AbstractNodeData)

        # First, see if we have a cached result for this
        key = (include_inherited, field_class)
        try:
            return cls._abstract_fields_dict_cache[key]
        except KeyError:
            pass

        # No cached result, we have to compute it
        if field_class == AbstractNodeData:
            # If we don't filter by class (i.e. if we want the most general
            # class field: AbstractNodeData), do the base class recursion.
            if include_inherited and cls.is_ast_node:
                result = OrderedDict()
                for base_class in cls.get_inheritance_chain():
                    result.update(base_class._fields)
            else:
                result = OrderedDict(cls._fields)

        # Otherwise, just rely on the potentially already cached whole list of
        # fields and do filtering.
        else:
            all_fields = cls.get_abstract_fields_dict(include_inherited)
            result = OrderedDict(
                filter(lambda (k, v): isinstance(v, field_class),
                       all_fields.items())
            )

        cls._abstract_fields_dict_cache[key] = result
        return result

    @classmethod
    def is_typed(cls):
        """
        Helper to determine whether the StructType is typed or not, eg. whether
        every field has a definite type.

        :rtype: bool
        """
        return all(
            f.type for f in cls.get_fields(include_inherited=False)
        )

    @classmethod
    def _user_name(cls):
        """
        Turn the name of the "cls" class into a Name instance.

        The result is intended to be used for code generation.

        :rtype: names.Name
        """
        if not cls._cached_user_name:
            name = names.Name.from_camel(cls.__name__)
            cls._cached_user_name = (name + names.Name('Node')
                                     if is_keyword(name) else name)
        return cls._cached_user_name

    @classmethod
    def name(cls):
        """
        Return the name that will be used in code generation for this AST node
        type.
        """
        return cls._user_name()

    @classmethod
    def repr_name(cls):
        """Return a name that will be used when serializing this AST node."""
        # This name is used by pretty printers-like code: we need the
        # "original" node name here, not keyword-escaped ones.
        result = getattr(cls, "_repr_name") or cls.__name__
        return result

    @classmethod
    def nullexpr(cls):
        """
        Return a value that can be considered as "null" for this AST node type.
        It indicates the absence of AST node.

        :rtype: str
        """
        if cls.is_ptr:
            return null_constant()
        else:
            return (names.Name('No') + cls.name()).camel_with_underscores

    @classmethod
    def doc(cls):
        return cls._doc

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, cls.name())

    @classmethod
    def c_inc_ref(cls, capi):
        """
        Name of the C API function to inc-ref structure value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(cls.name() + names.Name('Inc_Ref'))

    @classmethod
    def c_dec_ref(cls, capi):
        """
        Name of the C API function to dec-ref structure value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(cls.name() + names.Name('Dec_Ref'))

    @classmethod
    def py_nullexpr(cls):
        return '{}({})'.format(cls.name().camel, ', '.join(
            f.type.py_nullexpr() for f in cls.get_fields()
        ))


class ASTNodeType(StructType):
    """
    Base class for all user AST nodes.

    Subclasses can define new AST node types, but they also can be abstract
    themselves (to form a true tree of AST node types).  Each subclass can
    define a list of fields (see the above Field class), so that each concrete
    class' fields are the sum of all its subclass' fields plus its own.

    This base class defines utilities to emit native code for the AST node
    types: type declaration and type usage (to declare AST node variables).
    """

    is_ptr = True
    abstract = False
    synthetic = False
    is_root_node = False
    is_bool_node = False
    is_enum_node = False
    is_generic_list_type = False
    is_list_type = False
    is_root_list_type = False
    is_ada_record = False
    is_ast_node = True

    parser = None
    """
    Canonical parser for this ASTNodeType. Determined during the NodesToParsers
    pass.

    :type: Parser
    """

    generic_list_type = None
    """
    Root grammar class subclass. It is abstract, generated automatically when
    the root grammar class is known. All root list types subclass it.
    :type: ASTNodeType
    """

    has_abstract_list = False
    """
    Whether the automatically generated list type for this ASTNodeType (the
    "root list type") is abstract.
    """

    subclasses = []
    """
    List of subclasses. Overriden in the root grammar class and its children.
    :type: list[ASTNodeType]
    """

    _exposed = True

    def __new__(cls, *args):
        # TODO: We might want to find a way to limit that magic to the scope of
        # the grammar in some way.
        from langkit.parsers import Row, Opt

        if cls.is_bool_node and len(args) > 0:
            # If the node is a boolean node, then we want to parse the
            # sub-parsers as an optional parser that will be booleanized.
            return Opt(*args).as_bool(cls)
        else:
            # Else, we want to parse the sub-parsers as a row + transform
            return Row(*args) ^ cls

    @classmethod
    def is_refcounted(cls):
        return False

    @classmethod
    def base(cls):
        """
        Helper to return the base class of this ASTNodeType subclass.
        :rtype: ASTNodeType
        """
        assert cls != ASTNodeType, (
            'Base is not meant to be called on ASTNodeType itself, only on'
            ' subclasses of it'
        )
        return cls.__base__

    @classmethod
    @memoized
    def concrete_subclasses(cls):
        """
        Return the list of all (direct or indirect) subclasses for cls that are
        not abstract, sorted by hierarchical name.

        :rtype: list[ASTNodeType]
        """
        sorted_direct_subclasses = sorted(
            cls.subclasses, key=lambda subcls: subcls.hierarchical_name()
        )
        return sum(
            ((subcls.concrete_subclasses() if subcls.abstract else [subcls])
             for subcls in sorted_direct_subclasses),
            []
        )

    @classmethod
    def fields_with_accessors(cls):
        """
        Return a list of fields for which we must generate accessors in APIs.

        This list excludes inherited/overriden fields so that they are not
        generated multiple times. This list also excludes private fields unless
        the context requires them to be public in the generated library.
        """
        return [f
                for f in cls.get_abstract_fields(
                    include_inherited=False,
                    predicate=library_public_field
                )
                if not f.is_overriding]

    @classmethod
    def c_type(cls, c_api_settings):
        return c_node_type(c_api_settings)

    @classmethod
    def hierarchical_name(cls):
        """
        Return a name that contains all the base classes for this node type.

        For instance, if C derives from B which itself derives from A, this
        returns "A.B.C".

        :rtype: str
        """
        return '.'.join(cls.name().base_name
                        for cls in cls.get_inheritance_chain())

    @classmethod
    def ada_kind_name(cls):
        """
        Return the name of the Ada enumerator to represent this kind of node.
        :rtype: str
        """
        return (get_context().lang_name + cls.name()).camel_with_underscores

    @classmethod
    def value_type_name(cls):
        """
        Return the name of the Ada type for the record that implements this AST
        node. The main type name designates the class-wide access to this
        record.

        :rtype: str
        """
        return (cls.name() + names.Name('Type')).camel_with_underscores

    # We want structural equality on lists whose elements have the same types.
    # Memoization is one way to make sure that, for each CompiledType subclass
    # X: X.list_type() is X.list_type().
    @classmethod
    @memoized
    def list_type(cls):
        """
        Return an ASTNodeType subclass that represent a list of "cls".

        :rtype: CompiledType
        """
        element_type = cls

        def name(cls):
            return (cls.element_type().name() + names.Name('List')
                    if cls.is_root_list_type else
                    cls._user_name())

        cls = type(
            b'{}List'.format(element_type.name().camel),
            (StructMetaclass.root_grammar_class.generic_list_type, ), {
                'name': classmethod(name),

                '_is_abstract': element_type.has_abstract_list,
                'is_generic_list_type': False,
                'is_list_type': True,
                'is_root_list_type': True,
                'is_collection': classmethod(lambda cls: True),
                'element_type': classmethod(lambda cls: element_type),
                '_fields': [],
            }
        )

        ctx = get_context(True)
        if ctx:
            ctx.list_types.add(element_type)
        else:
            StructMetaclass.pending_list_types.append(cls)

        return cls

    @classmethod
    def entity_info(cls):
        """
        Return the entity info type, which is a record that contains semantic
        information which, when added to an AST node, makes an entity.
        """
        # This is manual memoization. It is necessary because memoization does
        # not play well with class method when we want the memoization to be
        # common to the whole class hierarchy.
        if not StructMetaclass.entity_info:
            StructMetaclass.entity_info = type(b'EntityInfo', (StructType, ), {
                '_fields': [
                    ('MD', BuiltinField(
                        # Use a deferred type so that the language spec. can
                        # reference entity types even before it declared the
                        # metadata class.
                        T.defer_env_md,
                        doc='The metadata associated to the AST node'
                    )),
                    ('rebindings', BuiltinField(EnvRebindingsType,
                                                access_needs_incref=True,
                                                doc=""))
                ],
            })
        return StructMetaclass.entity_info

    @classmethod
    @memoized
    def entity(cls):
        """
        Return the entity type, which is a node type with assorted semantic
        information.
        """

        entity_klass = type(
            b'Entity{}'.format('' if cls.is_root_node else cls.name().camel),
            (StructType, ), {
                'is_entity_type': True,
                'el_type': cls,

                '_fields': [
                    ('el', BuiltinField(cls, doc='The stored AST node')),
                    ('info', BuiltinField(cls.entity_info(),
                                          access_needs_incref=True,
                                          doc='Entity info for this node')),
                ]
            }
        )

        if cls.is_root_node:
            # LexicalEnv.get, which is bound in the AST.C generate package,
            # returns arrays of root node entities, so the corresponding
            # array type must be declared manually there.
            entity_klass.should_emit_array_type = False

        return entity_klass

    @classmethod
    def check_resolved(cls):
        """
        Emit a non-fatal error if this ASTNodeType subclass is not type
        resolved.
        """
        # Consider that AST nodes with type annotations for all their fields
        # are type resolved: they don't need to be referenced by the grammar.
        cls.is_type_resolved = (
            cls.is_type_resolved
            or all(f._type is not None for f in cls.get_parse_fields())
        )

        check_source_language(
            cls.is_type_resolved,
            'Unresolved ASTNode subclass. Use it in the grammar or provide a'
            ' type annotation for all its fields'
        )

    @classmethod
    def check_homonym_fields(cls):
        """
        Emit non-fatal errors if some fields in this subclass have conflicting
        homonym fields in a superclass.
        """
        inherited_fields = cls.base().get_abstract_fields_dict()
        for f_n, f_v in cls._fields.items():
            with f_v.diagnostic_context():
                homonym_fld = inherited_fields.get(f_n)
                if homonym_fld:
                    check_source_language(
                        f_v.is_property and homonym_fld.is_property,
                        '"{}" must be renamed as it conflicts with'
                        ' {}'.format(
                            f_n, homonym_fld.qualname
                        )
                    )

    @classmethod
    def py_nullexpr(cls):
        return 'None'


# We tag the ASTNodeType class as abstract here, because of the circular
# dependency between the @abstract decorator and the ASTNodeType class, which
# is caused by the assert statement that is inside the decorator.
ASTNodeType.abstract = True


def create_astnode_class(cls):
    """
    Create an ASTNodeType subclass for this ASTNode subclass.

    :param langkit.dsl.ASTNode cls: Subclass to translate.
    :rtype: ASTNodeType
    """
    from langkit.dsl import _ASTNodeList, _ASTNodeMetaclass

    if cls._base is _ASTNodeList:
        # This is supposed to be a root list type, use the
        # ASTNodeType.list_type class method to create it.
        element_type = cls._element_type._type
        assert element_type
        astnode_type = element_type.list_type()

    else:
        # This is a regular AST node: go through the regular subclassing
        # machinery.
        is_root = cls is _ASTNodeMetaclass.root_type

        # Create the ASTNodeType subclass itself
        base_cls = ASTNodeType if is_root else cls._base._type

        # TODO: make explicit the list of fields that are forwarded to compiled
        # type class constructor.
        dct = {k: v for k, v in cls.__dict__.items()
               if ((not k.startswith('__') or not k.endswith('__'))
                   and not isinstance(v, AbstractNodeData))}

        dct['is_root_list_type'] = False
        astnode_type = type(cls.__name__, (base_cls, ), dct)

    astnode_type.dsl_decl = cls
    return astnode_type


class ArrayType(CompiledType):
    """
    Base class for array types.
    """

    is_ptr = True

    # A compile pass will tag all array types that are exposed through the
    # public API.
    _exposed = False

    @classmethod
    def is_refcounted(cls):
        return True

    @classmethod
    def name(cls):
        """
        Name of the type for general values in the Ada generated code.

        :rtype: names.Name
        """
        return cls.element_type().name() + names.Name('Array_Access')

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, cls.api_name())

    @classmethod
    def nullexpr(cls):
        return null_constant()

    @classmethod
    def is_collection(cls):
        return True

    @classmethod
    def api_name(cls):
        """
        Name of the type for general values in our bindings.
        """
        return cls.element_type().name() + names.Name('Array')

    @classmethod
    def pointed(cls):
        """
        Name of the type for values that are pointed to by general values.

        :rtype: names.Name
        """
        return cls.element_type().name() + names.Name('Array_Record')

    @classmethod
    def pkg_vector(cls):
        """
        Name of the Langkit_Support.Vectors package instantiation corresponding
        to this element_type.

        :rtype: names.Name
        """
        return cls.element_type().name() + names.Name('Vectors')

    @classmethod
    def index_type(cls):
        """
        Name of the Index type for this array type.

        :rtype: str
        """
        pkg_vector_name = cls.element_type().name() + names.Name('Vectors')
        return '{}.Index_Type'.format(pkg_vector_name.camel_with_underscores)

    @classmethod
    def vector(cls):
        """
        Name of the type for vector values.

        :rtype: names.Name
        """
        return names.Name(cls.pkg_vector().camel_with_underscores + '.Vector')

    @classmethod
    def array(cls):
        """
        Name of the type for array values.

        :rtype: names.Name
        """
        return cls.api_name()

    @classmethod
    def c_inc_ref(cls, capi):
        """
        Name of the C API function to inc-ref an array value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(cls.api_name() + names.Name('Inc_Ref'))

    @classmethod
    def c_dec_ref(cls, capi):
        """
        Name of the C API function to dec-ref an array value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(cls.api_name() + names.Name('Dec_Ref'))


class _EnumType(CompiledType):
    """
    Base class for compiled types that hold a single value in a set of possible
    ones.
    """

    is_ptr = False

    alternatives = []
    """
    The list of alternatives for this EnumType subclass.

    :type: list[str]
    """

    # Suffix to use for the alternatives when they are invalid identifiers in
    # some target language.
    suffix = ''

    class Alternative(object):
        """
        Specific enum value.
        """

        def __init__(self, enum_type, alt):
            self.enum_type = enum_type
            self.alt = alt
            assert alt in self.enum_type.alternatives

        @property
        def enumerator(self):
            """
            Return `get_enumerator` for this alternative.
            :rtype: names.Name
            """
            return self.enum_type.get_enumerator(self.alt)

    @classmethod
    def base_name(cls):
        """
        Return a names.Name instance holding the unescaped name for this type.

        :rtype: names.Name
        """
        return names.Name.from_camel(cls.__name__)

    @classmethod
    def name(cls):
        return names.Name.from_camel('{}Type'.format(cls.__name__))

    @classmethod
    def nullexpr(cls):
        return "Uninitialized"

    @classmethod
    def doc(cls):
        return cls._doc

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, cls.base_name())

    @classmethod
    def get_enumerator(cls, alt):
        """
        Return a names.Name instance for alt's enumerator name.

        This is used in Ada code generation.

        :param str alt: The alternative for which we want the enumerator name.
        :rtype: names.Name
        """
        result = names.Name(alt)
        return (result + names.Name.from_lower(cls.suffix)
                if is_keyword(result) else result)

    @classmethod
    def alternatives_for(cls, language_settings):
        """
        Return the sequence of names to use for alternatives in the language
        corresponding to language_settings.

        :param AbstractAPISettings language_settings: The language for which we
            want the enum names.
        """
        type_name = cls.base_name()
        return [
            language_settings.get_enum_alternative(
                type_name, names.Name.from_lower(alt),
                names.Name.from_lower(cls.suffix)
            )
            for alt in cls.alternatives
        ]


def create_enum_type(cls):
    """
    Create an _EnumType subclass for this EnumType subclass.

    :param langkit.dsl.EnumType cls: Subclass to translate.
    :rtype: _EnumType
    """
    dct = {'_doc': cls._doc,
           'alternatives': cls.alternatives,
           'suffix': cls.suffix}
    return type(cls.__name__, (_EnumType, ), dct)


def create_enum_node_classes(cls):
    """
    Create the ASTNodeType subclasses to implement a EnumNode.

    :param langkit.dsl.EnumNode cls: EnumNode subclass that describes this
        type.
    """
    from langkit.expressions import Property, AbstractProperty

    is_bool_node = bool(cls._qualifier)

    fields = list(cls._fields)
    base_enum_dct = {
        'alternatives': cls._alternatives,
        'is_enum_node': True,
        'is_bool_node': is_bool_node,
        'is_type_resolved': True,

        '_doc': cls._doc,
        '_fields': fields,
        '_is_abstract': True,

        # List of `base_enum_node` subclass we create here, one for each
        # alternative.
        '_alternatives': [],
    }
    if is_bool_node:
        prop = AbstractProperty(type=BoolType, public=True)
        prop.location = cls._location
        fields.append(('as_bool', prop))

    # Add other supplied fields to the base class dict
    base_enum_dct.update(dict(cls._fields))

    # Generate the abstract base node type
    base_enum_node = type(cls._name.camel, (T.root_node, ), base_enum_dct)
    cls._type = base_enum_node

    for alt in cls._alternatives:
        alt_name = cls._name + alt.name

        # Generate the derived class corresponding to this alternative
        fields = []
        dct = {'_fields': fields}
        if is_bool_node:
            prop = Property(alt.name.lower == 'present')
            prop.location = cls._location
            fields.append(('as_bool', prop))

        alt_type = type(alt_name.camel, (base_enum_node, ), dct)
        alt._type = alt_type

        # Make the alternative derived class accessible from the root node for
        # the enum.
        base_enum_node._alternatives.append(alt_type)


class TypeRepo(object):
    """
    Repository of types. Used to be able to do early references to not yet
    declared types, in this fashion::

        T = TypeRepo()

        class A(ASTNode):
            p = AbstractProperty(type=T.B)

        class B(ASTNode):
            pass

    Only Struct and AST node types are reachable through the type repository.
    """

    class Defer(object):
        """
        Internal class representing a not-yet resolved type.
        """
        def __init__(self, getter):
            """
            :param () -> CompiledType getter: A function that will return
                the resolved type when called.
            """
            self.getter = getter

        def get(self):
            """
            Resolve the internally referenced type.

            :rtype: CompiledType
            """
            return self.getter()

        def __getattr__(self, name):
            def get():
                prefix = self.get()
                if (
                    name in ('array_type', 'list_type', 'entity')
                    or not issubtype(prefix, StructType)
                ):
                    return getattr(prefix, name)
                else:
                    return prefix._fields[name]
            return TypeRepo.Defer(get)

        def __call__(self, *args, **kwargs):
            return TypeRepo.Defer(lambda: self.get()(*args, **kwargs))

    def __getattr__(self, type_name):
        """
        Build and return a Defer type that references the above type.

        :param str type_name: The name of the rule.
        """
        def resolve():
            type_dict = CompiledTypeMetaclass.type_dict

            try:
                return type_dict[type_name]
            except KeyError:
                close_matches = difflib.get_close_matches(type_name, type_dict)
                check_source_language(
                    False,
                    'Invalid type name: {}{}'.format(
                        type_name,
                        ', did you one of the following? {}'.format(
                            ', '.join(close_matches)
                        ) if close_matches else ''
                    )
                )
        return TypeRepo.Defer(resolve)

    @property
    def root_node(self):
        """
        Shortcut to get the root AST node.
        :rtype: ASTNodeType
        """
        result = get_context().root_grammar_class
        assert result
        return result

    @property
    def defer_root_node(self):
        return self.Defer(lambda: self.root_node)

    @property
    def env_md(self):
        """
        Shortcut to get the lexical environment metadata type.
        :rtype: StructType
        """
        assert StructMetaclass.env_metadata is not None
        return StructMetaclass.env_metadata

    @property
    def defer_env_md(self):
        return self.Defer(lambda: self.env_md)

    @property
    def entity_info(self):
        """
        Shortcut to get the entity information type.
        :rtype: StructType
        """
        return StructMetaclass.root_grammar_class.entity_info()

    @property
    def entity(self):
        """
        This property returns the root type used to describe an AST node with
        semantic information attached.
        """
        return self.root_node.entity()

    @property
    def EnumNode(self):
        # TODO: update all uses in testsuites and Libadalang to use the global
        # one.
        from langkit.dsl import EnumNode
        return EnumNode

    @property
    @memoized
    def env_assoc(self):
        """
        EnvAssoc type, used to add associations of key and value to the lexical
        environments, via the add_to_env primitive.
        """
        class EnvAssoc(StructType):
            _fields = [
                ('key', UserField(type=Symbol)),
                ('val', UserField(type=self.defer_root_node)),
            ]

        return EnvAssoc


def resolve_type(typeref):
    """
    Resolve a type reference to the actual CompiledType subclass.

    :param typeref: Type reference to resolve. It can be either:
        * None: it is directly returned;
        * a CompiledType subclass: it is directly returned;
        * a TypeRepo.Defer instance: it is deferred;
        * a DSLType subclass: the corresponding CompiledType subclass is
          retrieved;
        * an EnumNode.Alternative instance: the type corresponding to this
          alternative is retrieved.

    :rtype: CompiledType
    """
    from langkit.dsl import DSLType, EnumNode

    if typeref is None or issubtype(typeref, CompiledType):
        return typeref

    elif isinstance(typeref, TypeRepo.Defer):
        return typeref.get()

    elif issubtype(typeref, DSLType):
        assert typeref._type
        return typeref._type

    elif isinstance(typeref, EnumNode.Alternative):
        return typeref.type

    else:
        check_source_language(False,
                              'Invalid type reference: {}'.format(typeref))


T = TypeRepo()
"""
Default type repository instance, to be used to refer to a type before its
declaration
"""
