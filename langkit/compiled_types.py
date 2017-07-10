from __future__ import absolute_import, division, print_function

from collections import OrderedDict
import difflib
from itertools import count, takewhile
import pipes

from langkit import names
from langkit.c_api import CAPIType
from langkit.common import get_type, null_constant, is_keyword
from langkit.diagnostics import (
    Context, check_source_language, extract_library_location
)
from langkit.template_utils import common_renderer
from langkit.utils import (issubtype, memoized, not_implemented_error,
                           type_check_instance)


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

    def type_is(compiled_type):
        return lambda t: t is compiled_type

    template_args = {
        'is_enum':                type_check_instance(_EnumType),
        'is_logic_var':           type_is(logic_var_type),
        'is_logic_var_type':      type_is(logic_var_type),
        'is_long':                type_is(long_type),
        'is_bool':                type_is(bool_type),
        'is_analysis_unit':       type_is(analysis_unit_type),
        'is_analysis_kind':       type_is(analysis_unit_kind),
        'is_struct':              type_check_instance(StructType),
        'is_struct_type':         type_check_instance(StructType),
        'is_sloc_range':          type_is(source_location_range_type),
        'is_token_type':          type_is(token_type),
        'is_symbol_type':         type_is(symbol_type),
        'is_array_type':          type_check_instance(ArrayType),
        'is_lexical_env':         type_is(lexical_env_type),
        'is_equation_type':       type_is(equation_type),
        'is_env_rebindings_type': type_is(env_rebindings_type),
        'no_builtins':
            lambda ts: filter(lambda t: not t.is_builtin(), ts),
        'LexicalEnvType':         lexical_env_type,
        'EnvRebindingsType':      env_rebindings_type,
        'PropertyDef':            PropertyDef,
    }
    if get_context():
        ctx = get_context()
        capi = ctx.c_api_settings

        # Name of the root AST node access type
        type_name = ctx.root_grammar_class.name

        # Name of the root AST node record type
        value_type = type_name + names.Name("Type")

        # Name of the root AST node kind type
        kind_name = type_name + names.Name("Kind_Type")

        # Likewise, for the generic list type
        glist_type_name = ctx.generic_list_type.name
        glist_value_type = ctx.generic_list_type.name + names.Name("Type")

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
            'bool_type':             bool_type.c_type(capi).name,
            'analysis_context_type': CAPIType(capi, 'analysis_context').name,
            'analysis_unit_type':    analysis_unit_type.c_type(capi).name,
            'node_kind_type':        CAPIType(capi, 'node_kind_enum').name,
            'node_type':             c_node_type(capi).name,
            'lexical_env_type':      lexical_env_type.c_type(capi).name,
            'logic_var_type':        logic_var_type.c_type(capi).name,
            'equation_type':         equation_type.c_type(capi).name,
            'env_rebindings_type':   env_rebindings_type.c_type(capi).name,
            'unit_kind_type':        analysis_unit_kind.c_type(capi).name,
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
            'sloc_range_type':
                source_location_range_type.c_type(capi).name,
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


class CompiledTypeMetaclass(object):
    """
    Metaclass for every compiled type. This is used to have a comprehensive
    list of every compiled type instance, so that you can use a TypeRepo
    instance to refer to any compiled type.
    """

    types = []
    """
    List of compiled types.
    :type: [CompiledType]
    """

    type_dict = {}
    """
    Mapping: type name -> CompiledType instance. Used in TypeRepo for type
    lookup by name.

    :rtype: dict[str, CompiledType]
    """

    astnode_types = []
    """
    List of ASTNodeType instances. This list is updated every time a new
    instance is created.

    :type: list[ASTNodeType]
    """

    struct_types = []
    """
    List of all StructType instances.

    :type: list[StructType]
    """

    pending_list_types = []
    """
    Set of ASTNodeType instances for list types that are created while there
    is no context.

    :type: list[ASTNodeType]
    """

    pending_array_types = []
    """
    Set of ArrayType instances that are created while there is no context.

    :type: list[langkit.compiled_types.ArrayType]
    """

    root_grammar_class = None
    """
    The ASTNodeType instances used as a root type. Every other ASTNodeType
    instances must derive directly or indirectly from that class.

    :type: ASTNodeType
    """

    env_metadata = None
    """
    The StrucType instances used as metadata for the lexical environments
    values.

    :type: StructType
    """

    entity_info = None
    """
    The StructType instances to contain all entity information, except the node
    itself.

    :type: StructType
    """


class CompiledType(object):
    """
    Descriptor for a type in the generated code.
    """

    def __init__(self, name, location=None, doc=None, is_ptr=True,
                 has_special_storage=False, is_list_type=False,
                 is_entity_type=False, should_emit_array_type=True,
                 exposed=True, c_type_name=None, external=False,
                 null_allowed=False, is_ada_record=False, is_refcounted=False,
                 nullexpr=None, py_nullexpr=None, element_type=None,
                 type_repo_name=None):
        """
        :param names.Name|str name: Type name. If a string, it must be
            camel-case.

        :param langkit.diagnostics.Location|None location: Location of the
            declaration of this compiled type, or None if this type does not
            come from a language specficication.

        :param str|None doc: User documentation for this type.

        :param bool is_ptr: Whether this type is handled through pointers only
            in the generated code.

        :param bool has_special_storage: Whether this type uses a special type
            for storage in structs and AST nodes.  If this is true, the
            following methods may be overriden:

              * storage_type_name;
              * storage_nullexpr;
              * extract_from_storage_expr;
              * convert_to_storage_expr.

        :param bool is_list_type: Whether this type is an AST node that is a
            list of AST nodes.

        :param bool is_entity_type: Whether this type represents an entity
            type.

        :param bool should_emit_array_type: Whether declarations for this
            compiled typed's array type are generated along with all regular
            array types. It must be False for several special types (for
            instance, the root AST node), for which the array type declarations
            are hard-wired in the $.Analysis package.

        :param bool exposed: Whether the type should be exposed to the C and
            Python APIs. Note that all types are exposed anyway when the
            current context has the "library_all_fields_public" attribute set
            to True.

        :param str|None c_type_name: Name for the type as it will appear in the
            C API. If left to None, `name` will be used instead.

        :param bool external: See CAPIType.__init__: this will be forwarded to
            it.

        :param bool null_allowed: Whether a client is allowed to create a null
            value for this type. Note that a type might have a nullexpr and
            still have null disallowed, because nullexpr might make sense for
            initialization purposes in the parsers, but not as a real null
            value.

        :param bool is_ada_record: Whether the type used in the C API is
            implemented as an Ada record. If so, we must pass them by reference
            in for C API interface functions.

        :param bool is_refcounted: Return whether this type matters for the
            ref-counting mechanism. See the "is_refcounted" method.

        :param str|None nullexpr: Null expression to use in Ada for this type.
            See the "nullexpr" method.

        :param str|None py_nullexpr: Null expression to use in Python for this
            type. See the "py_nullexpr" method.

        :param CompiledType|None element_type: If this is a collection type,
            must be the corresponding element type. Must be None otherwise.

        :param str|None type_repo_name: Name to use for registration in
            TypeRepo. The camel-case of "name" is used if left to None.
    """
        if isinstance(name, str):
            name = names.Name.from_camel(name)

        self._name = name
        self.location = location
        self._doc = doc
        self.is_ptr = is_ptr
        self.has_special_storage = has_special_storage
        self.is_list_type = is_list_type
        self.is_entity_type = is_entity_type
        self.should_emit_array_type = should_emit_array_type
        self.c_type_name = c_type_name
        self.external = external
        self._exposed = exposed
        self.null_allowed = null_allowed
        self.is_ada_record = is_ada_record
        self._is_refcounted = is_refcounted
        self._nullexpr = nullexpr
        self._py_nullexpr = py_nullexpr
        self._element_type = element_type

        type_repo_name = type_repo_name or name.camel
        CompiledTypeMetaclass.types.append(self)
        CompiledTypeMetaclass.type_dict[type_repo_name] = self

    @property
    def name(self):
        """
        Name of the type for general values in the Ada generated code.

        :rtype: names.Name
        """
        return self._name

    def __repr__(self):
        return '<CompiledType {}>'.format(self.name.camel)

    @property
    def diagnostic_context(self):
        ctx_message = 'in {}'.format(self.name.camel)
        return Context(ctx_message, self.location)

    @property
    def doc(self):
        """
        Return the user documentation for this type, or None if there is no
        documentation.

        :rtype: None|str
        """
        return self._doc

    @property
    def is_collection(self):
        """
        Return whether this is a collection type.

        :rtype: bool
        """
        return self._element_type is not None

    @property
    def element_type(self):
        """
        Assuming this is a collection type (array or list), return the
        corresponding element type.

        :rtype: CompiledType
        """
        assert self._element_type
        return self._element_type

    @property
    def is_refcounted(self):
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
        return self._is_refcounted

    @property
    def storage_type_name(self):
        """
        Return the name of the type that is used to store instances of this
        type in structs and AST nodes. See documentation for
        has_special_storage.

        :rtype: str
        """
        return self.name

    def extract_from_storage_expr(self, node_expr, base_expr):
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

    def convert_to_storage_expr(self, node_expr, base_expr):
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

    @property
    def nullexpr(self):
        """
        Return a string to be used in code generation for "null" expressions.

        Must be overriden in subclasses.

        :rtype: str
        """
        if self._nullexpr is None:
            raise not_implemented_error(self, self.nullexpr)
        else:
            return self._nullexpr

    @property
    def py_nullexpr(self):
        """
        Return a string to be used in Python code gen for "null" expressions.

        Must be overridden in subclasses... for which we need the Python null
        expression.

        :rtype: str
        """
        if self._py_nullexpr is None:
            raise not_implemented_error(self, self.py_nullexpr)
        else:
            return self._py_nullexpr

    @property
    def storage_nullexpr(self):
        """
        Return the nullexpr that is used for fields of this type in structs and
        ASTNodes. See documentation for has_special_storage.

        :rtype: str
        """
        return self.nullexpr

    def c_type(self, c_api_settings):
        """
        Return a CAPIType instance for this type.

        :param CAPISettings c_api_settings: The settings for the C API.
        """
        return CAPIType(c_api_settings, self.c_type_name or self.name,
                        external=self.external)

    def unify(self, other, error_msg=None):
        """
        If `self` and `other` are types that match, return the most general
        type to cover both. Create an error diagnostic if they don't match.

        :param CompiledType other: Type to unify with `self`.
        :param str|None error_msg: Diagnostic message for mismatching types. If
            None, a generic one is used, otherwise, we call .format on it with
            the `self` and `other` keys being the names of mismatching types.
        :rtype: CompiledType
        """

        # ASTNodeType instances (and thus entities) always can be unified:
        # just take the most recent common ancestor.
        if self.is_entity_type and other.is_entity_type:
            return ASTNodeType.common_ancestor(self.el_type,
                                               other.el_type).entity
        elif self.is_ast_node and other.is_ast_node:
            return ASTNodeType.common_ancestor(self, other)

        # Otherwise, we require a strict subtyping relation
        check_source_language(
            self.matches(other),
            (error_msg or 'Mismatching types: {self} and {other}').format(
                self=self.name.camel, other=other.name.camel
            )
        )
        return self

    def matches(self, formal):
        """
        Return whether `self` matches `formal`.

        This is mere equality for all types but AST nodes, in which `self` is
        allowed to be a subtype for `formal`.

        :param CompiledType formal: Type to match. `self` is checked to be a
            subtype of it.
        :rtype: bool
        """

        if self.is_entity_type and formal.is_entity_type:
            return self.el_type.matches(formal.el_type)

        if formal.is_ast_node and self.is_ast_node:
            return formal in self.get_inheritance_chain()
        else:
            return self is formal

    # Memoize so that we have only one array type for each element type
    @property
    @memoized
    def array(self):
        """
        Create an array type whose element type is `self`.

        :rtype: ArrayType
        """
        return ArrayType(name=self.name + names.Name('Array_Type'),
                         element_type=self)

    @property
    def is_base_struct_type(self):
        return isinstance(self, BaseStructType)

    @property
    def is_struct_type(self):
        return isinstance(self, StructType)

    @property
    def is_ast_node(self):
        return isinstance(self, ASTNodeType)

    def new(self, *args, **kwargs):
        """
        Shortcut to the New expression, allowing type.new(..) syntax.

        :rtype: AbstractExpression
        """
        from langkit.expressions.structs import New
        return New(self, *args, **kwargs)


class NoCompiledType(CompiledType):
    """
    Special CompiledType singleton. Used to mean the absence of type. This is
    useful in special resolved expressions.
    """

    def is_refcounted(self):
        raise NotImplementedError()

no_compiled_type = NoCompiledType('NoCompiledType')


analysis_unit_type = CompiledType(
    'AnalysisUnit',
    nullexpr='null',
    should_emit_array_type=True,
    null_allowed=True,
)

analysis_unit_kind = CompiledType(
    'UnitKind',
    is_ptr=False,
)

lexical_env_type = CompiledType(
    'LexicalEnv',
    nullexpr='Empty_Env',
    should_emit_array_type=False,
    null_allowed=True,
    exposed=False,
    is_refcounted=True,
    py_nullexpr='LexicalEnv.Empty',
)


class LogicVarType(CompiledType):
    """
    Singleton for the logic variable type.

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

    def __init__(self):
        super(LogicVarType, self).__init__(
            name='LogicVar',
            nullexpr='null',
            is_ptr=False,
            has_special_storage=True,
            exposed=False,
            c_type_name='logic_var_type',
        )

    @property
    def storage_type_name(self):
        return names.Name('Logic_Var_Record')

    @property
    def storage_nullexpr(self):
        return 'Null_Var_Record'

    def extract_from_storage_expr(self, node_expr, base_expr):
        del node_expr
        return "{}'Unrestricted_Access".format(base_expr)

    def convert_to_storage_expr(self, node_expr, base_expr):
        raise not_implemented_error(self, self.convert_to_storage_expr)

logic_var_type = LogicVarType()

equation_type = CompiledType(
    'LogicEquation',
    nullexpr='Null_Logic_Equation',
    exposed=False,
    null_allowed=True,
    c_type_name='equation_type',
    is_refcounted=True,
)


class EnvRebindingsType(CompiledType):
    """
    Singleton for the environment rebinding type.
    """

    def __init__(self):
        super(EnvRebindingsType, self).__init__(
            name='EnvRebindings',
            null_allowed=True,
            nullexpr='null',
            c_type_name='env_rebindings_type',
            is_refcounted=True,
            py_nullexpr='None'
        )

    def c_inc_ref(self, capi):
        """
        Name of the C API function to inc-ref an env rebindings value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.name + names.Name('Inc_Ref'))

    def c_dec_ref(self, capi):
        """
        Name of the C API function to dec-ref an env rebindings value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.name + names.Name('Dec_Ref'))

env_rebindings_type = EnvRebindingsType()

bool_type = CompiledType(
    name=get_type(bool),
    is_ptr=False,
    nullexpr='false',
    py_nullexpr='False',

    # "bool" is not a built-in type in C: we define our own type based on
    # uint8_t.
    c_type_name='bool'
)

long_type = CompiledType(
    name=get_type(long),
    is_ptr=False,
    nullexpr='0',
    external=True,
    c_type_name='int'
)

source_location_range_type = CompiledType(
    'SourceLocationRange',
    is_ptr=False,
    nullexpr='SourceLocationRange()',
)


class TokenType(CompiledType):
    """
    Singleton for the token data type.
    """

    def __init__(self):
        super(TokenType, self).__init__(
            name='TokenType',
            is_ptr=False,
            nullexpr='No_Token',
            is_ada_record=True,
            has_special_storage=True,
            c_type_name='token',
            type_repo_name='Token'
        )

    @property
    def storage_type_name(self):
        return 'Token_Index'

    @property
    def storage_nullexpr(self):
        return 'No_Token_Index'

    def extract_from_storage_expr(self, node_expr, base_expr):
        return 'Token ({}, {})'.format(node_expr, base_expr)

    def convert_to_storage_expr(self, node_expr, base_expr):
        return 'Stored_Token ({}, {})'.format(node_expr, base_expr)

token_type = TokenType()

symbol_type = CompiledType(
    'SymbolType',
    nullexpr='null',

    # See below: symbols are represented in the C API as text records
    is_ada_record=True,
    c_type_name='text',
)


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
                                           self.type.name.camel)


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

        self._uses_entity_info = False
        self._access_needs_incref = access_needs_incref

    @property
    def is_overriding(self):
        """
        Return whether this field overrides an inheritted one in a base class.

        :rtype: bool
        """
        from langkit.expressions import PropertyDef

        assert self._name and self.struct and self.struct.is_ast_node
        parent_cls = self.struct.base()
        properties_to_override = ([p._name
                                   for p in parent_cls.get_properties()]
                                  if parent_cls else [])
        return (isinstance(self, PropertyDef) and
                self._name in properties_to_override)

    @property
    def uses_entity_info(self):
        """
        Return whether evaluating this field requires entity info.

        :rtype: bool
        """
        assert self._uses_entity_info is not None
        return self._uses_entity_info

    @property
    def diagnostic_context(self):
        ctx_message = 'in {}.{}'.format(self.struct.name.camel,
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
        with self.diagnostic_context:
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
            self.struct.name.camel if self.struct else '<unresolved>',
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

    @property
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
        return self.struct.name + self.name

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

    @property
    def access_needs_incref(self):
        """
        Return whether field access evaluation does not create an ownership
        share. In this case, users must call Inc_Ref themselves. This returns
        always False for node data whose type is not ref-counted.

        :rtype: bool
        """
        return self.type.is_refcounted and self._access_needs_incref


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

        assert self.concrete, 'AbstractField itself cannot be instantiated'

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
    def type(self, typ):
        assert isinstance(typ, CompiledType)
        self._type = typ

    def __repr__(self):
        return '<ASTNode {} Field({})>'.format(self._index, self.qualname)

    @property
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
                 access_needs_incref=True):
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


class BaseStructType(CompiledType):
    """
    Base class to share common behavior between StructType and ASTNodeType.
    """

    def __init__(self, name, location, doc, **kwargs):
        """
        See CompiledType.__init__ for a description of arguments.
        """
        type_repo_name = name.camel
        if is_keyword(name):
            name = name + names.Name('Node')

        super(BaseStructType, self).__init__(
            name, location, doc,
            type_repo_name=type_repo_name,
            **kwargs
        )

        self._abstract_fields_dict_cache = {}
        """
        Cache for the get_abstract_fields_dict class method.

        :type: dict[(bool, AbstractNodeData), dict[str, AbstractField]]
        """

    def _init_fields(self, fields):
        """
        Bind input fields to `self` and initialize their name.

        :param list[(str, AbstractNodeData)] fields: List of (name, field) for
            this struct's fields. Inheritted fields must not appear in this
            list.
        """
        for f_n, f_v in fields:
            f_v.name = names.Name.from_lower(f_n)
            f_v.struct = self
        self._fields = OrderedDict(fields)

    @property
    def py_nullexpr(self):
        return '{}({})'.format(self.name.camel, ', '.join(
            f.type.py_nullexpr for f in self.get_fields()
        ))

    def add_field(self, field):
        """
        Append a field to this Struct/AST node. This is not valid if a field
        lookup has already been done on this Struct/AST node.

        :param AbstractNodeData field: Field to append.
        """
        assert not self._abstract_fields_dict_cache
        self._fields[field._name.lower] = field
        field.struct = self

    def get_user_fields(self, predicate=None, include_inherited=True):
        """
        Return the list of all the user fields `self` has, including its
        parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[UserField]
        """
        return self.get_abstract_fields(predicate, include_inherited,
                                        field_class=UserField)

    def get_fields(self, predicate=None, include_inherited=True):
        """
        Return the list of all the fields `self` has, including its parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[AbstractField]
        """
        return self.get_abstract_fields(predicate, include_inherited,
                                        field_class=AbstractField)

    def get_abstract_fields(self, predicate=None, include_inherited=True,
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
            self.get_abstract_fields_dict(include_inherited,
                                          field_class).values()
        )

    def get_abstract_fields_dict(self, include_inherited=True,
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
            return self._abstract_fields_dict_cache[key]
        except KeyError:
            pass

        # No cached result, we have to compute it
        if field_class == AbstractNodeData:
            # If we don't filter by class (i.e. if we want the most general
            # class field: AbstractNodeData), do the base class recursion.
            if include_inherited and self.is_ast_node:
                result = OrderedDict()
                for base_class in self.get_inheritance_chain():
                    result.update(base_class._fields)
            else:
                result = OrderedDict(self._fields)

        # Otherwise, just rely on the potentially already cached whole list of
        # fields and do filtering.
        else:
            all_fields = self.get_abstract_fields_dict(include_inherited)
            result = OrderedDict(
                filter(lambda (k, v): isinstance(v, field_class),
                       all_fields.items())
            )

        self._abstract_fields_dict_cache[key] = result
        return result


class StructType(BaseStructType):
    """
    POD composite type.
    """

    def __init__(self, name, location, doc, fields, **kwargs):
        """
        :param name: See CompiledType.__init__.

        :param list[(str, AbstractNodeData)] fields: List of (name, field) for
            this struct's fields. Inheritted fields must not appear in this
            list.
        """
        super(StructType, self).__init__(
            name, location, doc,
            is_ptr=False,
            null_allowed=True,
            nullexpr=(names.Name('No') + name).camel_with_underscores,
            is_ada_record=True,

            # A compile pass will tag all StructType subclasses that are
            # exposed through the public API.
            exposed=False,

            **kwargs
        )
        self._init_fields(fields)
        CompiledTypeMetaclass.struct_types.append(self)

    @property
    def is_refcounted(self):
        return any(f.type.is_refcounted for f in self._fields.values())

    def is_builtin(self):
        """
        Some structs are considered "built-in", which means that either no code
        needs to be emitted for them, either special code will be emitted on a
        special path, and we can omit them from regular code generation.

        :rtype: bool
        """
        return self in (
            # The env metadata struct is emitted separately from the others
            T.env_md,

            # Entitiy info and the root node's entity type is not emitted per
            # se, because it is a generic instantiation from
            # Langkit_Support.Lexical_Env.
            CompiledTypeMetaclass.root_grammar_class.entity_info(),
            CompiledTypeMetaclass.root_grammar_class.entity,
        )

    def c_inc_ref(self, capi):
        """
        Name of the C API function to inc-ref structure value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.name + names.Name('Inc_Ref'))

    def c_dec_ref(self, capi):
        """
        Name of the C API function to dec-ref structure value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.name + names.Name('Dec_Ref'))


class ASTNodeType(BaseStructType):
    """
    Type for an AST node.
    """

    def __init__(self, name, location, doc, base, fields,
                 env_spec=None, element_type=None, annotations=None,
                 is_generic_list_type=False, is_abstract=False,
                 is_synthetic=False, has_abstract_list=False,
                 is_enum_node=False, is_bool_node=False):
        """
        :param names.Name name: Name for this node.

        :param langkit.diagnostics.Location|None location: Location for the
            declaration of this node, if any.

        :param str|None doc: User documentation for this node.

        :param ASTNodeType|None base: ASTNodeType subclass corresponding to the
            base class for this node. None when creating the root node.

        :param list[(str, AbstractNodeData)] fields: List of (name, field) for
            this node's fields. Inherited fields must not appear in this list.

        :param langkit.envs.EnvSpec|None env_spec: Environment specification
            for this node, if any.

        :param ASTNodeType|None element_type: For root list types, this must be
            the ASTNodeType subclass that this list contains. Must be left to
            None in all other cases.

        :param bool is_generic_list_type: Whether this subclass will
            materialize the generic list type.

        :param bool is_abstract: Whether this node is abstract. Note that this
            can be changed later. This is forced to True for the generic list
            type and for root list types whose element type has the
            `has_abstract_list` attribute set to True.

        :param bool is_synthetic: Whether this node is synthetic. Note that
            this can be changed later.

        :param bool has_abstract_list: Whether the root list type for this node
            must be abstract. Node that this can be changed later, until the
            list type is actually created.

        :param bool is_enum_node: Whether this node comes from the expansion of
            langkit.dsl.EnumNode.

        :param bool is_bool_node: Whether this not is a qualifier coming from
            the expansion of langkit.dsl.EnumNode.
        """
        is_root = base is None
        is_root_list = base is not None and base.is_generic_list_type
        is_list = base is not None and (is_root_list or base.is_list_type)

        if is_root_list:
            assert element_type.is_ast_node

            # TODO: at this point, we need to make sure thas
            # element_type.has_abstract_list in the future.
            is_abstract = is_abstract or element_type.has_abstract_list

        else:
            assert element_type is None
            if is_list:
                element_type = base._element_type

        super(ASTNodeType, self).__init__(
            name, location, doc,
            is_ptr=True, null_allowed=True, is_ada_record=False,
            is_list_type=is_list, should_emit_array_type=not is_root,
            exposed=True, is_refcounted=False, nullexpr=null_constant(),
            py_nullexpr='None', element_type=element_type
        )
        self._base = base

        # Register this new subclass where appropriate in CompiledTypeMetaclass
        if is_root:
            CompiledTypeMetaclass.root_grammar_class = self
        CompiledTypeMetaclass.astnode_types.append(self)

        # Now we have an official root node type, we can create its builtin
        # fields.
        if is_root:
            fields = self.builtin_properties() + fields
        self._init_fields(fields)

        self.is_root_node = is_root
        self.is_generic_list_type = is_generic_list_type
        self.is_root_list_type = is_root_list

        from langkit.dsl import Annotations
        annotations = annotations or Annotations()
        self.annotations = annotations
        self.annotations.process_annotations(self, is_root)

        if env_spec:
            env_spec.ast_node = self
        self.is_env_spec_inherited = env_spec is None

        self.env_spec = env_spec
        """
        EnvSpec instance corresponding to this node.
        :type: langkit.compiled_types.EnvSpec|None
        """

        # List types are resolved by construction: we create list types to
        # contain specific ASTNodeType subclasses. All other types are not
        # resolved, only the grammar will resolve them..
        self.is_type_resolved = is_list

        # By default, ASTNodeType subtypes aren't abstract. The "abstract"
        # decorator may change this attribute later. Likewise for synthetic
        # nodes and nodes whose root list type is abstract.
        self.abstract = is_abstract or is_root or is_generic_list_type
        self.synthetic = is_synthetic

        self.has_abstract_list = has_abstract_list
        """
        Whether the automatically generated list type for this ASTNodeType (the
        "root list type") is abstract.
        :type: bool
        """

        # Prepare the list of subclasses for this node type and, if applicable,
        # register it as a subclass of its base.
        self.subclasses = []
        """
        List of subclasses. Overriden in the root grammar class and its
        children.

        :type: list[ASTNodeType]
        """

        if not is_root:
            base.subclasses.append(self)

        # If this is the root grammar type, create the generic list type name
        self.generic_list_type = None
        """
        Root grammar class subclass. It is abstract, generated automatically
        when the root grammar class is known. All root list types subclass it.
        :type: ASTNodeType|None
        """

        if base is None:
            generic_list_type_name = (
                names.Name.from_camel(annotations.generic_list_type)
                if annotations.generic_list_type else
                (self.name + names.Name('Base_List'))
            )

            self.generic_list_type = ASTNodeType(
                name=generic_list_type_name, location=None, doc=None,
                base=self, fields=[], is_generic_list_type=True
            )

        self.parser = None
        """
        Canonical parser for this ASTNodeType. Determined during the
        NodesToParsers pass.

        :type: Parser
        """

        self.is_enum_node = is_enum_node
        self.is_bool_node = is_bool_node

    def repr_name(self):
        """
        Return a name that will be used when serializing this AST node.
        :rtype: str
        """
        # This name is used by pretty printers-like code: we need the
        # "original" node name here, not keyword-escaped ones.
        result = self.annotations.repr_name or self.name.camel
        return result

    def is_builtin(self):
        """
        Some AST nodes are considered "built-in", which means that either no
        code needs to be emitted for them, either special code will be emitted
        on a special path, and we can omit them from regular code generation.

        :rtype: bool
        """
        return self in (
            # The root grammar class and the generic list types are emitted
            # separately from the others.
            CompiledTypeMetaclass.root_grammar_class,
            CompiledTypeMetaclass.root_grammar_class.generic_list_type,
        )

    def set_types(self, types):
        """
        Associate `types` (a list of CompiledType) to fields in `self` . It is
        valid to perform this association multiple times as long as types are
        consistent.

        :type types: list[CompiledType]
        """
        fields = self.get_parse_fields()

        check_source_language(
            len(fields) == len(types), "{} has {} fields ({} types given). You"
            " probably have inconsistent grammar rules and type "
            "declarations".format(self.name.camel, len(fields), len(types))
        )

        # TODO: instead of expecting types to be subtypes, we might want to
        # perform type unification (take the nearest common ancestor for all
        # field types). But then again, maybe not, it might be too confusing.
        for field, f_type in zip(fields, types):
            if field.type:
                check_source_language(
                    f_type.matches(field.type),
                    "Field {} already had type {}, got {}".format(
                        field.qualname, field.type.name, f_type.name
                    )
                )

        # Only assign types if self was not yet typed. In the case where it
        # was already typed, we checked above that the new types were
        # consistent with the already present ones.
        if not self.is_type_resolved:
            self.is_type_resolved = True

            for inferred_type, field in zip(types, fields):

                # At this stage, if the field has a type, it means that the
                # user assigned it one originally. In this case we will use the
                # inferred type for checking only (raising an assertion if it
                # does not correspond).
                if field.type:
                    with field.diagnostic_context:
                        check_source_language(
                            # Using matches here allows the user to annotate a
                            # field with a more general type than the one
                            # inferred.
                            inferred_type.matches(field.type),
                            'Expected type {} but type inferenced yielded type'
                            ' {}'.format(
                                field.type.name.camel,
                                inferred_type.name.camel
                            )
                        )
                else:
                    field.type = inferred_type

    def get_inheritance_chain(self):
        """
        Return the chain of ASTNodeType instances following the `base` link as
        a list.  Root-most classes come first.

        :rtype: list[ASTNodeType]
        """
        node = self
        result = []
        while node is not None:
            result.append(node)
            node = node.base()
        return reversed(result)

    @staticmethod
    def common_ancestor(*nodes):
        """
        Return the bottom-most common parent AST node for all `nodes` AST
        nodes.

        :param list[ASTNodeType] nodes: List of AST node types for which we
            are looking for the common ancestor.
        :rtype: ASTNodeType
        """
        return list(takewhile(
            lambda a: len(set(a)) == 1,
            zip(*map(ASTNodeType.get_inheritance_chain, nodes))
        ))[-1][0]

    def base(self):
        """
        Helper to return the base type of this AST node type.
        :rtype: ASTNodeType|None
        """
        return self._base

    @memoized
    def concrete_subclasses(self):
        """
        Return the list of all (direct or indirect) subclass types for `self`
        that are not abstract, sorted by hierarchical name.

        :rtype: list[ASTNodeType]
        """
        sorted_direct_subclasses = sorted(
            self.subclasses, key=lambda subcls: subcls.hierarchical_name()
        )
        return sum(
            ((subcls.concrete_subclasses() if subcls.abstract else [subcls])
             for subcls in sorted_direct_subclasses),
            []
        )

    def get_parse_fields(self, predicate=None, include_inherited=True):
        """
        Return the list of all the parse fields `self` has, including its
        parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[Field]
        """
        return self.get_abstract_fields(predicate, include_inherited,
                                        field_class=Field)

    def get_properties(self, predicate=None, include_inherited=True):
        """
        Return the list of all the fields `self` has.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[langkit.expressions.base.PropertyDef]
        """
        return self.get_abstract_fields(
            lambda f: f.is_property and (predicate is None or predicate(f)),
            include_inherited
        )

    def get_memoized_properties(self, include_inherited=False):
        """
        Return the list of all memoized properties `self` has.

        :param bool include_inherited: If true, include inheritted properties
            in the returned list. Return only properties that were part of the
            declaration of this node otherwise.

        :rtype: list[langkit.expressions.base.PropertyDef]
        """
        return self.get_properties(lambda p: p.memoized, include_inherited)

    def fields_with_accessors(self):
        """
        Return a list of fields for which we must generate accessors in APIs.

        This list excludes inherited/overriden fields so that they are not
        generated multiple times. This list also excludes private fields unless
        the context requires them to be public in the generated library.
        """
        return [f
                for f in self.get_abstract_fields(
                    include_inherited=False,
                    predicate=library_public_field
                )
                if not f.is_overriding]

    def c_type(self, c_api_settings):
        return c_node_type(c_api_settings)

    def hierarchical_name(self):
        """
        Return a name that contains all the base classes for this node type.

        For instance, if C derives from B which itself derives from A, this
        returns "A.B.C".

        :rtype: str
        """
        return '.'.join(node.name.base_name
                        for node in self.get_inheritance_chain())

    def ada_kind_name(self):
        """
        Return the name of the Ada enumerator to represent this kind of node.
        :rtype: str
        """
        return (get_context().lang_name + self.name).camel_with_underscores

    def value_type_name(self):
        """
        Return the name of the Ada type for the record that implements this AST
        node. The main type name designates the class-wide access to this
        record.

        :rtype: str
        """
        return (self.name + names.Name('Type')).camel_with_underscores

    # We want structural equality on lists whose elements have the same types.
    # Memoization is one way to make sure that, for each CompiledType instance
    # X: X.list is X.list.
    @property
    @memoized
    def list(self):
        """
        Return an ASTNodeType subclass that represent a list of `self`.

        :rtype: CompiledType
        """
        result = ASTNodeType(
            name=self.name + names.Name('List'),
            location=None, doc=None,
            base=CompiledTypeMetaclass.root_grammar_class.generic_list_type,
            fields=[], element_type=self
        )

        ctx = get_context(True)
        if ctx:
            ctx.list_types.add(result._element_type)
        else:
            CompiledTypeMetaclass.pending_list_types.append(result)

        return result

    def entity_info(self):
        """
        Return the entity info type, which is a record that contains semantic
        information which, when added to an AST node, makes an entity.
        """
        # This is manual memoization. It is necessary because memoization does
        # not play well with class method when we want the memoization to be
        # common to the whole class hierarchy.
        if not CompiledTypeMetaclass.entity_info:
            CompiledTypeMetaclass.entity_info = StructType(
                names.Name('Entity_Info'), None, None,
                [
                    ('md', BuiltinField(
                        # Use a deferred type so that the language spec. can
                        # reference entity types even before it declared the
                        # metadata class.
                        T.defer_env_md,
                        doc='The metadata associated to the AST node'
                    )),
                    ('rebindings', BuiltinField(env_rebindings_type,
                                                access_needs_incref=True,
                                                doc=""))
                ],
            )
        return CompiledTypeMetaclass.entity_info

    @property
    @memoized
    def entity(self):
        """
        Return the entity type, which is a node type with assorted semantic
        information.
        """

        name = names.Name('Entity')
        if not self.is_root_node:
            name += self.name

        result = StructType(
            name, None, None,
            [('el', BuiltinField(self, doc='The stored AST node')),
             ('info', BuiltinField(self.entity_info(),
                                   access_needs_incref=True,
                                   doc='Entity info for this node'))],
        )
        result.is_entity_type = True
        result.el_type = self

        if self.is_root_node:
            # LexicalEnv.get, which is bound in the AST.C generate package,
            # returns arrays of root node entities, so the corresponding
            # array type must be declared manually there.
            result.should_emit_array_type = False

        return result

    def check_resolved(self):
        """
        Emit a non-fatal error if this ASTNodeType subclass is not type
        resolved.
        """
        # Consider that AST nodes with type annotations for all their fields
        # are type resolved: they don't need to be referenced by the grammar.
        self.is_type_resolved = (
            self.is_type_resolved
            or all(f._type is not None for f in self.get_parse_fields())
        )

        check_source_language(
            self.is_type_resolved,
            'Unresolved ASTNode subclass. Use it in the grammar or provide a'
            ' type annotation for all its fields'
        )

    def check_homonym_fields(self):
        """
        Emit non-fatal errors if some fields in this subclass have conflicting
        homonym fields in a superclass.
        """
        inherited_fields = (self.base().get_abstract_fields_dict()
                            if self.base() else {})
        for f_n, f_v in self._fields.items():
            with f_v.diagnostic_context:
                homonym_fld = inherited_fields.get(f_n)
                if homonym_fld:
                    check_source_language(
                        f_v.is_property and homonym_fld.is_property,
                        '"{}" must be renamed as it conflicts with'
                        ' {}'.format(
                            f_n, homonym_fld.qualname
                        )
                    )

    def builtin_properties(self):
        """
        Return properties available for all AST nodes.

        Note that CompiledTypeMetaclass.root_grammar_class must be defined
        first.

        :rtype: list[(str, AbstractNodeData)]
        """
        from langkit.expressions import PropertyDef

        root_type = CompiledTypeMetaclass.root_grammar_class
        assert root_type

        # Note that we must not provide implementation for them here (no
        # expression) since the implementation comes from the hard-coded root
        # AST node type definition.
        return [
            # The following fields return lexical env values, which are
            # ref-counted. However these specific envs are owned by the
            # analysis unit, so they are not ref-counted.

            ('node_env', PropertyDef(
                expr=None, prefix=None, type=lexical_env_type,
                public=False, external=True, uses_entity_info=True,
                force_dispatching=True, warn_on_unused=False,
                doc='For nodes that introduce a new environment, return the'
                    ' parent lexical environment. Return the "inherited"'
                    ' environment otherwise.'
            )),
            ('children_env', PropertyDef(
                expr=None, prefix=None, type=lexical_env_type,
                public=False, external=True, uses_entity_info=True,
                warn_on_unused=False,
                doc='For nodes that introduce a new environment, return it.'
                    ' Return the "inherited" environment otherwise.'
            )),

            ('parent', BuiltinField(
                type=root_type,
                doc='Return the lexical parent for this node. Return null for'
                    ' the root AST node or for AST nodes for which no one has'
                    ' a reference to the parent.'
            )),

            # The following builtin fields are implemented as a property, so
            # there is no need for an additional inc-ref.
            ('parents', BuiltinField(
                type=root_type.array,
                doc='Return an array that contains the lexical parents (this'
                    ' node included). Nearer parents are first in the list.',
                access_needs_incref=False,
            )),
            ('children', BuiltinField(
                type=root_type.array,
                doc='Return an array that contains the direct lexical'
                    ' children.',
                access_needs_incref=False,
            )),
            ('token_start', PropertyDef(
                expr=None, prefix=None, type=token_type,
                public=True, external=True, uses_entity_info=False,
                doc='Return the first token used to parse this node.'
            )),
            ('token_end', PropertyDef(
                expr=None, prefix=None, type=token_type,
                public=True, external=True, uses_entity_info=False,
                doc='Return the last token used to parse this node.'
            )),
            ('child_index', BuiltinField(
                type=long_type,
                doc="Return the 0-based index for Node in its parent's"
                    " children."
            )),
            ('previous_sibling', BuiltinField(
                type=root_type,
                doc="Return the node's previous sibling, if there is one."
            )),
            ('next_sibling', BuiltinField(
                type=root_type,
                doc="Return the node's next sibling, if there is one."
            )),
        ]


# We tag the ASTNodeType class as abstract here, because of the circular
# dependency between the @abstract decorator and the ASTNodeType class, which
# is caused by the assert statement that is inside the decorator.
ASTNodeType.abstract = True


class ArrayType(CompiledType):
    """
    Base class for array types.
    """

    def __init__(self, name, element_type):
        # By default, array types are not exposed. A compilation pass will tag
        # only the ones that are exposed through the public API.
        super(ArrayType, self).__init__(name=name, is_ptr=True, exposed=False,
                                        is_refcounted=True,
                                        nullexpr=null_constant(),
                                        element_type=element_type)

        # Register this type where it needs to be registered
        ctx = get_context(True)
        if ctx:
            ctx.array_types.add(self)
        else:
            CompiledTypeMetaclass.pending_array_types.append(self)

    @property
    def name(self):
        return self.element_type.name + names.Name('Array_Access')

    @property
    def api_name(self):
        """
        Name of the type for general values in our bindings.
        """
        return self.element_type.name + names.Name('Array')

    @property
    def pointed(self):
        """
        Name of the type for values that are pointed to by general values.

        :rtype: names.Name
        """
        return self.element_type.name + names.Name('Array_Record')

    @property
    def pkg_vector(self):
        """
        Name of the Langkit_Support.Vectors package instantiation corresponding
        to this element_type.

        :rtype: names.Name
        """
        return self.element_type.name + names.Name('Vectors')

    def c_type(self, c_api_settings):
        return CAPIType(c_api_settings, self.api_name)

    def index_type(self):
        """
        Name of the index type for this array type.

        :rtype: str
        """
        pkg_vector_name = self.element_type.name + names.Name('Vectors')
        return '{}.Index_Type'.format(pkg_vector_name.camel_with_underscores)

    def vector(self):
        """
        Name of the type for vector values.

        :rtype: names.Name
        """
        return names.Name(self.pkg_vector.camel_with_underscores + '.Vector')

    def array(self):
        """
        Name of the type for array values.

        :rtype: names.Name
        """
        return self.api_name

    def c_inc_ref(self, capi):
        """
        Name of the C API function to inc-ref an array value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.api_name + names.Name('Inc_Ref'))

    def c_dec_ref(self, capi):
        """
        Name of the C API function to dec-ref an array value.

        :param langkit.c_api.CAPISettings capi: Settings for the C API.
        :rtype: str
        """
        return capi.get_name(self.api_name + names.Name('Dec_Ref'))


class _EnumType(CompiledType):
    """
    Enumeration type. This is a compiled type that hold a single value in a set
    of possible ones.
    """

    def __init__(self, name, location, doc, alternatives, suffix):
        """
        :param name: See CompiledType.__init__.
        :param location: See CompiledType.__init__.
        :param doc: See CompiledType.__init__.

        :param list[str] alternatives: The list of alternatives for this
            EnumType subclass.

        :param str suffix: Suffix to use for the alternatives when they are
            invalid identifiers in some target language.
        """
        super(_EnumType, self).__init__(
            name, location, doc,
            is_ptr=False, nullexpr='Uninitialized'
        )
        self.alternatives = alternatives
        self.suffix = suffix

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

    def base_name(self):
        """
        Return a names.Name instance holding the unescaped name for this type.

        :rtype: names.Name
        """
        return self._name

    @property
    def name(self):
        return self._name + names.Name('Type')

    def c_type(self, c_api_settings):
        return CAPIType(c_api_settings, self.base_name())

    def get_enumerator(self, alt):
        """
        Return a names.Name instance for alt's enumerator name.

        This is used in Ada code generation.

        :param str alt: The alternative for which we want the enumerator name.
        :rtype: names.Name
        """
        result = names.Name(alt)
        return (result + names.Name.from_lower(self.suffix)
                if is_keyword(result) else result)

    def alternatives_for(self, language_settings):
        """
        Return the sequence of names to use for alternatives in the language
        corresponding to language_settings.

        :param AbstractAPISettings language_settings: The language for which we
            want the enum names.
        """
        type_name = self.base_name()
        return [
            language_settings.get_enum_alternative(
                type_name, names.Name.from_lower(alt),
                names.Name.from_lower(self.suffix)
            )
            for alt in self.alternatives
        ]


def create_enum_node_types(cls):
    """
    Create the ASTNodeType instances to implement a EnumNode.

    :param langkit.dsl.EnumNode cls: EnumNode subclass that describes this
        type.
    """
    from langkit.expressions import Property, AbstractProperty

    is_bool_node = bool(cls._qualifier)

    fields = list(cls._fields)
    if is_bool_node:
        prop = AbstractProperty(type=bool_type, public=True)
        prop.location = cls._location
        fields.append(('as_bool', prop))

    # Generate the abstract base node type
    base_enum_node = ASTNodeType(
        name=cls._name, location=cls._location, doc=cls._doc, base=T.root_node,
        fields=fields, is_abstract=True, is_enum_node=True,
        is_bool_node=is_bool_node,
    )
    base_enum_node.alternatives = cls._alternatives
    base_enum_node.is_type_resolved = True
    base_enum_node._alternatives = []
    cls._type = base_enum_node

    for alt in cls._alternatives:
        alt_name = cls._name + alt.name

        # Generate the derived class corresponding to this alternative
        fields = []
        if is_bool_node:
            prop = Property(alt.name.lower == 'present')
            prop.location = cls._location
            fields.append(('as_bool', prop))

        alt_type = ASTNodeType(
            name=alt_name, location=None, doc=None,
            base=base_enum_node,
            fields=fields
        )
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
                    name in ('array', 'list', 'entity', 'new')
                    or not isinstance(prefix, BaseStructType)
                ):
                    return getattr(prefix, name)

                try:
                    return prefix._fields[name]
                except KeyError:
                    check_source_language(
                        False,
                        '{prefix} has no {attr} attribute'.format(
                            prefix=(prefix.name.camel
                                    if isinstance(prefix, CompiledType) else
                                    prefix),
                            attr=repr(name)
                        )
                    )
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
        assert CompiledTypeMetaclass.env_metadata is not None
        return CompiledTypeMetaclass.env_metadata

    @property
    def defer_env_md(self):
        return self.Defer(lambda: self.env_md)

    @property
    def entity_info(self):
        """
        Shortcut to get the entity information type.
        :rtype: StructType
        """
        return CompiledTypeMetaclass.root_grammar_class.entity_info()

    @property
    def entity(self):
        """
        This property returns the root type used to describe an AST node with
        semantic information attached.
        """
        return self.root_node.entity

    @property
    @memoized
    def env_assoc(self):
        """
        EnvAssoc type, used to add associations of key and value to the lexical
        environments, via the add_to_env primitive.
        """
        return StructType(
            names.Name('Env_Assoc'), None, None,
            [('key', UserField(type=symbol_type)),
             ('val', UserField(type=self.defer_root_node))]
        )


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

    if typeref is None or isinstance(typeref, CompiledType):
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
