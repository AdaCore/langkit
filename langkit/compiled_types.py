from __future__ import absolute_import

from collections import OrderedDict
from copy import copy
import inspect
from itertools import count

from langkit import diagnostics, names
from langkit.c_api import CAPIType
from langkit.common import get_type, null_constant, is_keyword
from langkit.diagnostics import extract_library_location, check_source_language
from langkit.template_utils import common_renderer
from langkit.utils import (
    memoized, type_check, col, Colors, common_ancestor, issubtype
)


def get_context():
    """
    Return the current compilation context, see
    langkit.compile_context.get_context.

    TODO: this function exists only to workaround circular dependency issues.
    We should get rid of them.

    :rtype: CompileCtx
    """
    from langkit.compile_context import get_context
    return get_context()


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


def decl_type(ada_type):
    """
    :type ada_type: CompiledType
    :rtype: str
    """
    return str(ada_type.storage_type_name())


def c_node_type(capi):
    return CAPIType(capi, 'base_node')


def library_public_field(field):
    """
    Return whether we must generate accessors in APIs for this field.

    :param AbstractNodeData field: Field to test.
    :rtype: bool
    """
    return not field.is_private or get_context().library_fields_all_public


def make_renderer(base_renderer=None):
    """
    Create a template renderer with common helpers.

    :param Renderer base_renderer: The renderer to base the resulting
        renderer on.
    """
    if base_renderer is None:
        base_renderer = common_renderer

    template_args = {
        'is_enum':          type_check(EnumType),
        'is_long':          type_check(LongType),
        'is_bool':          type_check(BoolType),
        'is_ast_node':      type_check(ASTNode),
        'is_sloc_range':    type_check(SourceLocationRangeType),
        'is_token_type':    type_check(Token),
        'is_symbol_type':   type_check(Symbol),
        'is_array_type':    type_check(ArrayType),
        'is_lexical_env':   type_check(LexicalEnvType),
        'is_struct_type':   type_check(Struct),
        'decl_type':        decl_type,
    }
    if get_context():
        capi = get_context().c_api_settings
        env_element = get_context().env_element

        # Name of the root AST node access type
        type_name = get_context().root_grammar_class.name()

        # Name of the root AST node record type
        value_type = type_name + names.Name("Type")

        # Name of the root AST node kind type
        kind_name = value_type + names.Name("Kind")

        template_args.update({
            'root_node_type_name':   type_name,
            'root_node_value_type':  value_type,
            'root_node_kind_name':   kind_name,
            'ctx':                   get_context(),
            'ada_api':               get_context().ada_api_settings,
            'capi':                  capi,
            'bool_type':             BoolType.c_type(capi).name,
            'analysis_context_type': CAPIType(capi, 'analysis_context').name,
            'analysis_unit_type':    CAPIType(capi, 'analysis_unit').name,
            'node_kind_type':        CAPIType(capi, 'node_kind_enum').name,
            'node_type':             c_node_type(capi).name,
            'lexical_env_type':      CAPIType(capi, 'lexical_env').name,
            'env_element_type':      (env_element.c_type(capi).name
                                      if env_element else None),
            'token_type':            CAPIType(capi, 'token').name,
            'sloc_type':             CAPIType(capi, 'source_location').name,
            'sloc_range_type':       SourceLocationRangeType.c_type(capi).name,
            'text_type':             CAPIType(capi, 'text').name,
            'diagnostic_type':       CAPIType(capi, 'diagnostic').name,
            'exception_type':        CAPIType(capi, 'exception').name,
            'library_public_field':  library_public_field,
        })
    return base_renderer.update(template_args)


def render(*args, **kwargs):
    return make_renderer().render(*args, **kwargs)


class CompiledType(object):
    """
    Base class used to describe types in the generated code.

    It is intended to be subclassed in order to create now compiled types.
    However, subclasses are not intended to be instantiated.
    """

    is_ptr = True
    """
    Whether this type is handled through pointers only in the generated code
    """

    is_storage_value = False
    """
    Whether this type is stored by value. If this is True, we expect:
    1. is_ptr to be true.
    2. storage_type_name to be overloaded to provide the name of the value
       type.

    TODO: We should check that those invariants are respected this somewhere.
    """

    is_list_type = False
    """
    Whether this type represents an instantiation of ASTList (i.e. a list of
    AST nodes).
    """

    def __init__(self):
        assert False, (
            'CompiledType subclasses are not meant to be instantiated'
        )

    @classmethod
    def is_collection(cls):
        return False

    @classmethod
    def add_to_context(cls):
        """
        If needed, put bits into the global context to implement this compiled
        type.

        Note that once this method got called on some "cls", further calls on
        the same class must do nothing. Thus it is safe to call this method
        multiple times for the same type.

        Must be overriden in subclasses if needed.
        """
        pass

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
        raise NotImplementedError()

    @classmethod
    def storage_type_name(cls):
        """
        Return the name of the type that is used to store instances of this
        type in Structs/ASTNodes. This only needs to be overloaded when the
        "is_storage_value" attribute is set to True, in which case the storage
        type and the type passed around in properties and in the public API are
        not the same.
        """
        return cls.name()

    @classmethod
    def nullexpr(cls):
        """
        Return a string to be used in code generation for "null" expressions.

        Must be overriden in subclasses.
        """
        raise NotImplementedError()

    @classmethod
    def storage_nullexpr(cls):
        """
        Return the nullexpr that is used for fields of this type in
        Structs/ASTNodes. Only needed if "is_storage_value" is True. See
        storage_type_name for more details.
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
        raise NotImplementedError()

    @classmethod
    def unify(cls, other):
        """
        Assuming "cls" and "other" are types that match, return the most
        general type to cover both. An AssertionError is raised if they don't
        match.

        :param CompiledType cls: Type parameter.
        :param CompiledType other: Type parameter.
        :rtype: CompiledType
        """
        assert cls.matches(other)
        if issubclass(other, ASTNode):
            return common_ancestor(cls, other)
        else:
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

        if issubclass(formal, ASTNode):
            return issubclass(cls, formal)
        else:
            return cls == formal

    # We want structural equality on lists whose elements have the same types.
    # Memoization is one way to make sure that, for each CompiledType subclass
    # X: X.list_type() is X.list_type().
    @classmethod
    @memoized
    def list_type(cls):
        """
        Return an ASTNode subclass that represent a list of "cls".

        :rtype: CompiledType
        """
        element_type = cls

        def add_to_context(cls):
            if cls in get_context().types:
                return
            get_context().types.add(cls)
            get_context().list_types.add(cls.element_type())

            # Make sure the type this list contains is already declared
            cls.element_type().add_to_context()

        return type(
            '{}ListType'.format(element_type.name().camel),
            (StructMetaClass.root_grammar_class, ), {
                'is_ptr': True,

                'name': classmethod(lambda cls:
                                    names.Name('List') +
                                    cls.element_type().name()),
                'add_to_context': classmethod(add_to_context),
                'nullexpr': classmethod(lambda cls: null_constant()),

                'is_list_type': True,
                'is_collection': classmethod(lambda cls: True),
                'element_type': classmethod(lambda cls: element_type),
            }
        )

    # Likewise for array types
    @classmethod
    @memoized
    def array_type(cls):
        """
        Create an array type for "cls".

        :rtype: ArrayType
        """
        element_type = cls
        return type(
            '{}ArrayType'.format(element_type.name().camel), (ArrayType, ), {
                'element_type': classmethod(lambda cls: element_type),
            }
        )


class BasicType(CompiledType):
    """
    Base class used to describe simple types that do not need declaration code
    generation.
    """
    _name = None
    _nullexpr = None
    _storage_nullexpr = None
    _external = False

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


class LexicalEnvType(BasicType):
    _name = "AST_Envs.Lexical_Env"
    _nullexpr = "null"
    is_ptr = True

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'lexical_env')


class LogicVarType(BasicType):
    """
    This type represents a logical variable. A logical variable is a reference,
    that can take a specific value when it is bound to a specific Equation, and
    that equation is solved for a certain problem set.

    It is a special type in langkit at this point, in that its value is
    initially undefined, and is mutated after the solve of an equation to a
    specific value. It is thus the only type in langkit for the moment for
    which variables of the type can be mutated after creation.

    TODO: For the moment a logic variable is necessarily a holder around an
    ASTNode. At some point we might want to make it generic, like list and
    array types.
    """
    _name = "Logic_Var"
    _nullexpr = "null"
    _storage_nullexpr = "Null_Var_Record"
    is_ptr = False
    is_storage_value = True

    @classmethod
    def storage_type_name(cls):
        return names.Name("Logic_Var_Record")

    @classmethod
    def c_type(cls, c_api_settings):
        raise Exception("Cannot expose logic variables to C at the moment")


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
    _nullexpr = "Null_Rel"
    is_ptr = False

    @classmethod
    def c_type(cls, c_api_settings):
        raise Exception("Cannot expose logic variables to C at the moment")


class BoolType(BasicType):
    is_ptr = False
    _name = get_type(bool)
    _nullexpr = "false"

    @classmethod
    def c_type(cls, c_api_settings):
        # "bool" is not a built-in type in C: we define our own type based on
        # uint8_t.
        return CAPIType(c_api_settings, 'bool')


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
    _name = "Token_Index"
    _nullexpr = "No_Token"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'token')


class Symbol(BasicType):
    is_ptr = False
    _name = "Symbol_Type"
    _nullexpr = "null"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'text')


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

    def __init__(self, name=None, private=False):
        """
        :param names.Name|None name: Name for this field. Most of the time,
            this is initially unknown at field creation, so it is filled only
            at Struct creation time.

        :param bool|None private: Whether this AbstractNodeData instance is
            supposed to be private or not.

            In the context of properties only, None is also allowed: in this
            case, inherit vibility from parents. If there is no property to
            override and None is passed, make the property public. This is
            computed in the "compute" pass.
        """
        self._index = next(self._counter)
        self._is_private = private

        self.location = extract_library_location()

        self._name = name
        """
        Name for this property. If None at instanciation time, it must be
        initialized when the embedding Struct subclass is created.
        :type: names.Name
        """

        self.ast_node = None
        """
        ASTNode subclass that declared this field. Initialized when creating
        ASTNode subclasses (so left None for other Struct subclasses).
        :type: ASTNode
        """

        self.arguments = []
        """
        Code generation-wise, all node data can be considered as functions
        which take at least a mandatory Self argument and return the
        corresponding data.

        This is a list that describes all other arguments. For each argument,
        this contains a tuple for:

          * the name of the argument;
          * its type;
          * its default value as a string, or None if there is no default
            value.

        Note that only Property instances accept other arguments.

        :type: list[(names.Name, CompiledType, None|str)]
        """

    @property
    def is_private(self):
        """
        Whether this field is private.
        :rtype: bool
        """
        assert self._is_private is not None
        return self._is_private

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
        raise NotImplementedError()

    @type.setter
    def type(self, type):
        raise NotImplementedError()

    @property
    def name(self):
        """
        :rtype: names.Name
        """
        assert self._name
        radix = (names.Name(self._name.base_name[1:])
                 if self.is_internal else
                 self._name)
        return self.prefix + radix if self.prefix else radix

    @property
    def qualname(self):
        """
        Return the qualified name for this field, i.e. the name of the owning
        type plus the name of the field itself. This is useful for diagnostic
        messages.
        :rtype: str
        """
        return '{}.{}'.format(
            self.ast_node.name().camel if self.ast_node else '<unresolved>',
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
        raise NotImplementedError()

    @property
    def accessor_basename(self):
        """
        Return the base name for the accessor we generate for this field.

        Note that this is available only for fields attached to AST nodes.

        :rtype: names.Name
        """
        assert self.ast_node
        return self.ast_node.name() + self.name

    @property
    def explicit_arguments(self):
        """
        Return the subset of "self.arguments" that are explicit arguments, that
        is to say the subset that users actually handle in expressions.
        """
        return self.arguments


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

    def __init__(self, repr=True, doc=None, type=None):
        """
        Create an AST node field.

        :param bool repr: If true, the field will be displayed when
            pretty-printing the embedding AST node.
        :param str|None doc: User documentation for this field.
        """

        if not self.concrete:
            raise NotImplementedError()

        super(AbstractField, self).__init__(private=False)

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
    subclasses of ASTNode.
    """
    concrete = True


class UserField(AbstractField):
    """
    Fields that are not meant to store parsing results. Can be used on any
    Node type, will be ignored by the parsing code.
    """

    def __init__(self, type, repr=False, doc=None, is_private=False):
        """
        See inherited doc. In this version we just ensure that a type is
        passed because it is mandatory for data fields. We also set repr to
        False because most of the time you don't want User fields to show up in
        the pretty printer.

        :type type: CompiledType
        :type doc: str
        """
        super(UserField, self).__init__(repr, doc, type)
        self._is_private = is_private

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


class NodeMacro(object):
    """
    This class is used to extend Struct subclasses in a programmatic way.
    You can put fields and attributes on a NodeMacro subclass that you would
    put on a Struct subclass, and then extend any Struct subclass by adding
    the NodeMacro subclass in the list of _macros of the Struct. For example::

        class Foo(NodeMacro):
            a = Field(..)
            b = Property(Self.c)

        class Bar(RootASTNode):
            _macros = [Foo]
            c = Field(..)

        class Baz(RootASTNode):
            _macros = [Foo]
            c = Property(..)

    What is highlighted by this example is that type checking is done after
    macro expansion, and since langkit has type inference, macros are
    polymorphic. Here Bar.c and Baz.c can have different types, and their b
    property will also have a different type.
    """
    pass


def create_macro(attrib_dict):
    """
    Helper to create macro types from a dict of attributes.

    :param dict attrib_dict: The attributes to put on the macro class.
    :rtype: NodeMacro
    """
    return type('macro', (NodeMacro, ), attrib_dict)


class StructMetaClass(type):
    """
    Internal metaclass for AST nodes, used to ease fields handling during code
    generation.
    """

    astnode_types = []
    """
    List of ASTNode types. This list is updated every time a new astnode type
    is created.

    :type: list[ASTNode]
    """

    struct_types = []
    """
    List of all plain struct types.

    :type: list[Struct]
    """

    root_grammar_class = None
    """
    The class used as a root for the whole ASTNode hierarchy for the
    currently compiled grammar. Every ASTNode must derive directly or
    indirectly from that class.

    :type: ASTNode
    """

    env_metadata = None
    """
    The class designing the type used as metadata for the lexical environments
    values. Must be a pure struct type.

    :type: Struct
    """

    @staticmethod
    def fields_dict(fields):
        """
        From a list of (name, AbstractNodeData) tuples, return an ordered
        dict of the same data, sorted by field index. Abstracts the logic of
        field reordering.

        :type fields: [(str, AbstractNodeData)]
        :rtype: OrderedDict[str, AbstractNodeData]
        """
        return OrderedDict(sorted(fields, key=lambda (_, f): f._index))

    def __new__(mcs, name, bases, dct):

        is_root_grammar_class = False

        assert len(bases) == 1, (
            "Multiple inheritance for AST nodes is not supported"
        )
        base, = bases

        # We want to check that every type deriving from ASTNode actually
        # derives from an user defined subclass of ASTNode.

        # First, skip Struct and ASTNode built-ins, and skip subclasses of
        # Struct.
        if name not in ["Struct", "ASTNode"] and base is not Struct:
            # If we have no root grammar class yet and reach this point,
            # the type necessarily derives from ASTNode. It's the root
            # grammar class.
            if mcs.root_grammar_class is None and base is ASTNode:
                is_root_grammar_class = True
            # Else, check that it does indeed derives from the root grammar
            # class.
            else:
                assert issubclass(base, mcs.root_grammar_class), (
                    "Every ASTNode subclass must derive directly or indirectly"
                    " from the root grammar class"
                )

                assert base != ASTNode, (
                    "You can have only one class deriving from ASTNode, which"
                    " will be the root class of your grammar"
                )

        # Here, we'll register fields and properties for the root AST node
        # class. Note that we must not provide implementation for them here (no
        # expression) since the implementation comes from the hard-coded root
        # AST node type definition.
        if is_root_grammar_class:
            dct["node_env"] = BuiltinField(
                type=LexicalEnvType, is_private=True,
                doc='For nodes that introduce a new environment, return the'
                    ' parent lexical environment. Return the "inherited"'
                    ' environment otherwise.'
            )
            dct["children_env"] = BuiltinField(
                type=LexicalEnvType, is_private=True,
                doc='For nodes that introduce a new environment, return it.'
                    ' Return the "inherited" environment otherwise.'
            )

            dct["parent"] = BuiltinField(
                type=None,
                doc="Return the lexical parent for this node. Return null for"
                    " the root AST node or for AST nodes for which no one has"
                    " a reference to the parent."
            )
            dct["parents"] = BuiltinField(
                type=None,
                doc="Return an array that contains the lexical parents (this"
                    " node included). Nearer parents are first in the list."
            )

        # Get the list of macro classes, and compute the ordered dicts of
        # fields for each of them.
        macro_classes = dct.get("_macros", [])
        macro_fields = [
            mcs.fields_dict([
                (f_n, copy(f_v)) for f_n, f_v in klass.__dict__.items()
                if isinstance(f_v, AbstractNodeData)
            ]) for klass in macro_classes
        ]

        # Gather the fields and properties in a dictionary. Recover the order
        # of field declarations.  See the Field class definition for more
        # details.
        fields = mcs.fields_dict(
            [(f_n, f_v) for f_n, f_v in dct.items()
             if isinstance(f_v, AbstractNodeData)]
        )

        # Append the list of fields that we've got from macros. Append them
        # in order, so that the order of declaration in NodeMacro classes is
        # respected, as well as the order of macro classes in the _macro field.
        for fields_dict in macro_fields:
            fields.update(fields_dict)

        for f_n, f_v in fields.iteritems():
            with diagnostics.context(
                'in {}.{}'.format(name, f_n),
                extract_library_location()
            ):
                check_source_language(
                    not f_n.startswith('_'),
                    'Underscode-prefixed field names are not allowed'
                )

        # Compute lexical environment specification. Since it can be
        # specified in macros, we want to make sure that there's only one.
        env_spec = dct.get('env_spec', None)
        for klass in macro_classes:
            es = klass.__dict__.get('env_spec', None)
            assert not (es and env_spec), (
                "Too many lexical environments specifications defined for "
                "node {}".format(name)
            )
            env_spec = env_spec or es
        dct['is_env_spec_inherited'] = env_spec is None
        dct['env_spec'] = env_spec

        # Env specs may need to create properties: add these as fields for this
        # node.
        if env_spec:
            for p in env_spec.create_properties():
                fields[p._name.lower] = p

        for field_name, field in fields.items():
            # Remove fields/props as class members: we want them to be
            # stored in their own dicts.
            if field_name in dct:
                dct.pop(field_name)
            # Store the name of the field in the field
            field.name = names.Name.from_lower(field_name)

        dct['_fields'] = fields

        # By default, ASTNode subtypes aren't abstract
        dct['abstract'] = False

        dct['subclasses'] = []

        dct['is_type_resolved'] = False
        cls = type.__new__(mcs, name, bases, dct)

        # Associate each field and property to this ASTNode subclass. Likewise
        # for the environment specification.
        for field in fields.values():
            field.ast_node = cls
        if env_spec:
            env_spec.ast_node = cls

        assert cls.is_ast_node() or not cls.get_properties(), (
            "Properties are not yet supported on plain structs"
        )

        if cls.is_ast_node():
            mcs.astnode_types.append(cls)
        else:
            mcs.struct_types.append(cls)

        if is_root_grammar_class:
            mcs.root_grammar_class = cls
            fields["parent"].type = cls
            fields["parents"].type = cls.array_type()
        elif cls.is_ast_node():
            base.subclasses.append(cls)

        return cls


def abstract(cls):
    """
    Decorator to tag an ASTNode subclass as abstract.

    :param ASTNode cls: Type parameter. The ASTNode subclass to decorate.
    """
    assert issubclass(cls, ASTNode)
    cls.abstract = True
    return cls


def root_grammar_class(cls):
    """
    Decorator to tag an ASTNode subclass as the root grammar node.

    :param ASTNode cls: Type parameter. The ASTNode subclass to decorate.
    """
    assert cls.base() == ASTNode
    assert StructMetaClass.root_grammar_class == cls, (
        "You can have only one descendent of ASTNode, and it must be the "
        "root grammar class"
    )
    EnvElement.get_abstract_fields_dict()['el'].type = cls
    return cls


def env_metadata(cls):
    """
    Decorator to tag a Struct subclass as the type used for lexical
    environments metadata.

    The assumption that is done for the moment is that the struct subclass
    given as argument must only have boolean fields. In the context of
    libadalang, the idea is that env metadata is to be used to express things
    such as "is this element accessed through an implicit dereference?" or
    "does this element correspond to an Ada 2005 dot notation call".

    This allows metadata to be combinable, that is you must be able to take two
    metadata objects and combine them. Obvious for booleans, not so much
    generally.

    The fact that metadata is combinable allows us, for example, to take an
    env containing subpprograms accessed via dot notation, and annotate the
    whole env with the property "implicit dereference", if the receiver is
    an access. Env elements containing a subprogram will thus have both
    properties set.

    :param Struct cls: Type parameter. The Struct subclass to decorate.
    """

    StructMetaClass.env_metadata = cls
    assert issubclass(cls, Struct), (
        "The type chosen to be environment metadata must be a struct type"
    )

    assert not issubclass(cls, ASTNode), (
        "The type chosen to be environment metadata must not be an ASTNode "
        "type"
    )
    for field in cls.get_fields():
        assert isinstance(field, UserField), (
            "Fields of the Struct type chosen to be environment metadata "
            "must be instances of UserField."
        )
        assert field.type == BoolType, (
            "Fields of the Struct type chosen to be environment metadata "
            "must have type boolean"
        )

    # Set the type of the EnvElement metadata field to cls
    EnvElement.get_abstract_fields_dict()['MD'].type = cls

    return cls


class TypeDeclaration(object):
    """Simple holder for generated type declarations."""

    def __init__(self, type, public_part, private_part):
        """

        :param type(ASTNode) type: The type that this TypeDeclaration holds
            onto.
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


class Struct(CompiledType):
    """
    Base class for all user struct-like composite types, such as POD structs
    and AST nodes.

    User subclasses deriving from Struct will define by-value POD types that
    cannot be subclassed themselves.
    """

    env_spec = None
    ":type: langkit.compiled_types.EnvSpec"

    _fields = OrderedDict()
    """
    The fields for this ASTNode, instantiated by the metaclass
    :type: dict[str, Field]
    """

    __metaclass__ = StructMetaClass
    is_ptr = False

    @classmethod
    def is_builtin(cls):
        """
        Some Structs are considered "built-in", which means that either no
        code needs to be emitted for them, either special code will be
        emitted on a special path, and we can omit them from regular code
        generation.

        :rtype: bool
        """
        return cls in (
            # The root grammar class is emitted separately from the others
            StructMetaClass.root_grammar_class,

            # The env metadata struct is emitted separately from the others
            StructMetaClass.env_metadata,

            # EnvElement is not emitted per se, because it is a generic
            # instantiation from Langkit_Support.Lexical_Env.
            EnvElement
        )

    @classmethod
    def is_ast_node(cls):
        """
        Helper for issubclass(cls, ASTNode), to determine if a subclass is a
        struct or an astnode descendent.

        :rtype: bool
        """
        return False

    @classmethod
    def set_types(cls, types):
        """
        Associate `types` (a list of CompiledType) to fields in `cls` . It is
        valid to perform this association multiple times as long as types are
        consistent.

        :type types: list[CompiledType]
        """
        fields = cls.get_parse_fields(include_inherited=False)

        assert len(fields) == len(types), (
            "{} has {} fields ({} types given). You probably have"
            " inconsistent grammar rules and type declarations".format(
                cls, len(fields), len(types)
            )
        )

        # TODO: instead of expecting types to be subtypes, we might want to
        # perform type unification (take the nearest common ancestor for all
        # field types). But then again, maybe not, it might be too confusing.
        if cls.is_type_resolved:
            for field, f_type in zip(fields, types):
                check_source_language(
                    issubclass(f_type, field.type),
                    "Field {} already had type {}, got {}".format(
                        field.name, field.type.name(), f_type.name()
                    )
                )

        # Only assign types if cls was not yet typed. In the case where it
        # was already typed, we checked above that the new types were
        # consistent with the already present ones.
        if not cls.is_type_resolved:
            cls.is_type_resolved = True

            for field_type, field in zip(types, fields):

                # At this stage, if the field has a type, it means that the
                # user assigned it one originally. In this case we will use the
                # inferred type for checking only (raising an assertion if it
                # does not correspond).
                if field.type:
                    f = inspect.getfile(cls)
                    l = inspect.getsourcelines(cls)[1]
                    assert field.type == field_type, (
                        col("Inferred type for field does not correspond to "
                            "type provided by the user.\n", Colors.FAIL) +
                        col("class {astnode_name}, file {file} line {line}\n",
                            Colors.WARNING) +
                        "Field {field_name}, "
                        "Provided type : {ptype}, Inferred type: {itype}"
                    ).format(astnode_name=cls.name(), file=f, line=l,
                             ptype=field.type.name().camel,
                             itype=field_type.name().camel,
                             field_name=field._name.camel)
                else:
                    field.type = field_type

    @classmethod
    def get_inheritance_chain(cls):
        """
        Return a list for all classes from ASTNode to `cls` in the inheritance
        chain.

        :rtype: list[ASTNode]
        """
        return reversed([base_class for base_class in cls.mro()
                         if issubclass(base_class, ASTNode)])

    @classmethod
    def get_properties(cls, predicate=None, include_inherited=True):
        """
        Return the list of all the fields `cls` has, including its parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[langkit.expressions.base.PropertyDef]
        """
        return cls.get_abstract_fields(
            lambda f: (predicate is None or predicate(f)) and f.is_property,
            include_inherited
        )

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

        :rtype: list[AbstractField]
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

        :rtype: dict[str, AbstractField]
        """

        def get_fields(klass):
            return OrderedDict(
                filter(lambda (k, v): isinstance(v, field_class),
                       klass._fields.items())
            )

        if include_inherited and cls.is_ast_node():
            fields = OrderedDict()
            for base_class in cls.get_inheritance_chain():
                fields.update(get_fields(base_class))
            return fields
        else:
            return get_fields(cls)

    @classmethod
    def is_typed(cls):
        """
        Helper to determine whether the Struct is typed or not, eg. whether
        every field has a definite type.

        :rtype: bool
        """
        return all(
            f.type for f in cls.get_fields(include_inherited=False)
        )

    @classmethod
    def add_to_context(cls):

        assert cls.is_typed, (
            "Trying to generate code for a type before typing is complete"
        )

        if cls not in get_context().types and cls != ASTNode:
            base_class = cls.__bases__[0]
            if issubclass(base_class, ASTNode):
                base_class.add_to_context()

            get_context().types.add(cls)

            for f in cls.get_fields(include_inherited=False):
                if f.type:
                    f.type.add_to_context()

    @classmethod
    def name(cls):
        """
        Return the name that will be used in code generation for this AST node
        type.
        """
        name = names.Name.from_camel(cls.__name__)
        return (name + names.Name('Node') if is_keyword(name) else name)

    @classmethod
    def repr_name(cls):
        """Return a name that will be used when serializing this AST node."""
        # This name is used by pretty printers-like code: we need the
        # "original" node name here, not keyword-escaped ones.
        return getattr(cls, "_repr_name", cls.__name__)

    @classmethod
    def nullexpr(cls):
        """
        Return a value that can be considered as "null" for this AST node type.
        It indicates the absence of AST node.
        """
        if cls.is_ptr:
            return null_constant()
        else:
            return names.Name('No') + cls.name()

    @classmethod
    def doc(cls):
        # Yield documentation only for user types: types defined in Langkit
        # have documentation that targets Langkit users.
        return cls.__doc__ if cls != ASTNode else None

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, cls.name())


class ASTNode(Struct):
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

    subclasses = []
    """
    List of subclasses. Overriden in the root grammar class and its children.
    :type: list[ASTNode]
    """

    @classmethod
    def base(cls):
        """
        Helper to return the base class of this ASTNode subclass.
        :rtype: ASTNode
        """
        assert cls != ASTNode, (
            "Base is not meant to be called on ASTNode itself, only on"
            " subclasses of it"
        )
        return cls.__base__

    @classmethod
    def fields_with_accessors(cls):
        """
        Return a list of fields for which we must generate accessors in APIs.

        This list excludes inherited fields so that they are not generated
        multiple times. This list also excludes private fields unless the
        context requires them to be public in the generated library.
        """
        return cls.get_abstract_fields(
            include_inherited=False,
            predicate=library_public_field
        )

    @classmethod
    def is_ast_node(cls):
        return True

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

# We tag the ASTNode class as abstract here, because of the circular dependency
# between the @abstract decorator and the ASTNode class, which is caused by the
# assert statement that is inside the decorator.
ASTNode.abstract = True


class ArrayType(CompiledType):
    """
    Base class for array types.
    """

    is_ptr = True

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
        null_constant()

    @classmethod
    def add_to_context(cls):
        if cls in get_context().types:
            return
        get_context().types.add(cls)
        get_context().array_types.add(cls)

        # Make sure the type this list contains is already declared
        cls.element_type().add_to_context()

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
    def vector(cls):
        """
        Name of the type for vector values.

        :rtype: names.Name
        """
        return names.Name(cls.pkg_vector().camel_with_underscores + '.Vector')


class EnumType(CompiledType):
    """
    Base class for compiled types that hold a single value in a set of possible
    ones.

    Subclasses must override the `alternatives` member to hold a list of
    distinct strings that represent the set of possibilities.  They represent
    the compiled type.

    Instances represent either the enum type itself in the generated code or a
    particular enum value.
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

    # noinspection PyMissingConstructor
    def __init__(self, alt):
        """
        Create a value that represent one of the enum alternatives.

        :param str alt: The alternative to use for this instance.
        """
        # CompiledType are not usually supposed to be instantiated.  EnumType
        # is an exception to this rule, so do not call CompiledType.__init__.
        assert alt in self.alternatives
        self.alt = alt

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
    def add_to_context(cls):
        if cls not in get_context().types:
            get_context().types.add(cls)
            get_context().enum_types.add(cls)

    @classmethod
    def nullexpr(cls):
        return "Uninitialized"

    @classmethod
    def doc(cls):
        return cls.__doc__

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

    @property
    def enumerator(self):
        """
        Return `get_enumerator` for this alternative.
        :rtype: names.Name
        """
        return self.get_enumerator(self.alt)

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


class EnvElement(Struct):
    """
    Denotes the type returned by doing a get operation on a lexical
    environment. This is a wrapper containing the ast node stored as a
    value, as well as the metadata associated to this node in the source
    lexical environment.
    """
    # The type of el will be filled when the root_grammar_class is used
    el = BuiltinField(None, doc="The stored AST node")

    # The type of MD is initialized to LongType, because by default,
    # the type for metadata is an integer in Ada.
    MD = BuiltinField(LongType, doc="The metadata associated to the AST node")


class TypeRepo(object):
    """
    Repository of types. Used to be able to do early references to not yet
    declared types, in this fashion::

        T = TypeRepo()

        class A(ASTNode):
            p = AbstractProperty(type=T.B)

        class B(ASTNode):
            pass

    Only struct and ast node types are reachable through the type repository.
    """

    @memoized
    def type_dict(self):
        """
        Returns a dictionnary of names to types.

        :rtype: dict[str, CompiledType]
        """
        return {
            t.__name__: t
            for t in
            StructMetaClass.struct_types + StructMetaClass.astnode_types
        }

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

        def array_type(self):
            """
            Proxy to the CompiledType.array_type classmethod.

            :rtype: ArrayType
            """
            return TypeRepo.Defer(lambda: self.get().array_type())

        def list_type(self):
            """
            Proxy to the CompiledType.list_type classmethod.

            :rtype: CompiledType
            """
            return TypeRepo.Defer(lambda: self.get().list_type())

    def __getattr__(self, type_name):
        """
        Build and return a Defer type that references the above type.

        :param str type_name: The name of the rule.
        """
        return TypeRepo.Defer(lambda: self.type_dict()[type_name])


def resolve_type(type_or_defer):
    """
    Given an object that can be either a TypeRepo.Defer instance or a
    CompiledType, returns a CompiledType.

    :param CompiledType|TypeRepo.Defer type_or_defer: the type to resolve.
    :rtype: CompiledType
    """
    if type_or_defer and not issubtype(type_or_defer, CompiledType):
        check_source_language(
            isinstance(type_or_defer, TypeRepo.Defer),
            "Type provided is neither a CompiledType, "
            "neither a defered type reference: {}".format(type_or_defer)
        )
        return type_or_defer.get()
    return type_or_defer
