from __future__ import absolute_import, division, print_function

from langkit.compiled_types import (
    _EnumType, ASTNodeType, AbstractNodeData, Field as _Field, StructType,
    UserField as _UserField, T
)
from langkit.diagnostics import (
    Context, check_source_language, extract_library_location
)
from langkit.expressions import PropertyDef
import langkit.names as names
from langkit.utils import classproperty, inherited_property, issubtype


class DSLType(object):
    """
    Base class for the representation of types in the DSL.
    """
    def new(cls, *args, **kwargs):
        """
        Shortcut to the New expression, allowing type.new(..) syntax.

        :rtype: AbstractExpression
        """
        return T.Defer(lambda: cls._type.new(*args, **kwargs))

    @classproperty
    def array(cls):
        """
        Return the array type whose element type is `cls`.
        """
        return T.Defer(lambda: cls._type.array)

    @classmethod
    def _diagnostic_context(cls):
        ctx_message = 'in {}'.format(cls._name.camel)
        return Context(ctx_message, cls._location)

    @staticmethod
    def _import_base_type_info(name, location, dct):
        """
        Metaclass helper.  Register in `dct` the name, location and doc
        information for the class to be built.
        """
        dct['_name'] = names.Name.from_camel(name)
        dct['_location'] = location
        dct['_doc'] = dct.get('__doc__')

    _type = None
    """
    Link to the StructType/ASTNodeType subclass corresponding to this subclass.
    This is of course initialized only when we create the said
    StructType/ASTNodeType subclass.

    :type: langkit.compiled_types.CompiledType
    """

    _name = None
    """
    Name for this type.
    :type: names.Name
    """

    _location = None
    """
    Location in the language spec. for the declaration of this type, or None if
    it is created by Langkit.

    :type: langkit.diagnostics.Location|None
    """

    _doc = None
    """
    Docstring associated to this type.

    :type: str|None
    """


class BaseStruct(DSLType):
    """
    Base class for Struct and ASTNode.
    """

    _fields = None
    """
    List of couples: (field name, field) for this type. For Struct subclasses,
    this is actually a list of UserField. For ASTNode subclasses, this excludes
    inherited fields.

    :type: list[(str, langkit.compiled_types.AbstractNodeData)]
    """

    @classmethod
    def collect_fields(cls, owning_type, location, dct, field_cls):
        """
        Metaclass helper. Excluding __special__ entries, make sure all entries
        in `dct` are instances of `field_cls` and return them as an annotated
        list: (name, field).

        This ensure that all fields are associated to a legal name, and records
        this name in the field instances.

        :param str owning_type: Name of the type for the type that will own the
            fields.  Used for diagnostic message formatting purposes.
        :param langkit.diagnostic.Location: Location for the declaration of the
            owning type.
        :param dict[str, T] dct: Input class dictionnary.
        :param AbstractNodeData|list(AbstractNodeData) field_cls:
            AbstractNodeData subclass, or list of subclasses.
        :rtype: list[(str, AbstractNodeData)]
        """
        result = []
        for f_n, f_v in dct.items():
            # Ignore __special__ fields
            if f_n.startswith('__') and f_n.endswith('__'):
                continue

            with Context('in {}.{}'.format(owning_type, f_n), location):
                expected_types = (field_cls
                                  if isinstance(field_cls, tuple) else
                                  (field_cls, ))
                check_source_language(
                    isinstance(f_v, field_cls),
                    'Field {f_name} is a {f_type}, but only instances of'
                    ' {exp_type} subclasses are allowed in {metatype}'
                    ' subclasses'.format(
                        f_name=f_n, f_type=type(f_v),
                        exp_type='/'.join(t.__name__ for t in expected_types),
                        metatype=cls.__name__,
                    )
                )
                check_source_language(
                    not f_n.startswith('_'),
                    'Underscore-prefixed field names are not allowed'
                )
                check_source_language(
                    f_n.lower() == f_n,
                    'Field names must be lower-case'
                )

            f_v.name = names.Name.from_lower(f_n)
            result.append((f_n, f_v))

        # Sort fields by creation time order so that users get fields in the
        # same order as it was declared in the DSL.
        result.sort(key=lambda (_, f): f._index)
        return result


def _check_decorator_use(decorator, expected_cls, cls):
    """
    Helper for class decorators below. Raise a diagnostic error if `cls`,
    which is the input parameter of `decorator`, is not a subclass of
    `expected_cls`.
    """
    location = extract_library_location()
    with Context('In call to the {} decorator'.format(decorator.__name__),
                 location):
        check_source_language(
            issubtype(cls, expected_cls),
            'The {} decorator must be called on a {} subclass'
            ' (here, got: {})'.format(decorator.__name__,
                                      expected_cls.__name__,
                                      cls)
        )


class _StructMetaclass(type):
    """
    Internal metaclass for struct types, used to collect all Struct subclasses
    that language specifications declare.
    """

    struct_types = []
    """
    List of all Struct subclasses, excluding Struct itself.

    :type: list[Struct]
    """

    env_metadata = None
    """
    Struct subclass to be used as metadata in lexical environments. See the
    `env_metadata` decorator.

    :type: Struct
    """

    @classmethod
    def reset(cls):
        cls.struct_types = []
        cls.env_metadata = None

    def __new__(mcs, name, bases, dct):
        is_base = bases == (BaseStruct, )

        if not is_base:
            mcs.process_subclass(name, bases, dct)

        cls = type.__new__(mcs, name, bases, dct)

        if not is_base:
            mcs.struct_types.append(cls)

            struct_type = StructType(cls._name, cls._location, cls._doc,
                                     cls._fields)
            struct_type.dsl_decl = cls
            cls._type = struct_type

        return cls

    @classmethod
    def process_subclass(mcs, name, bases, dct):
        location = extract_library_location()

        with Context('in {}'.format(name), location):
            check_source_language(
                bases == (Struct, ),
                'Struct subclasses must derive from Struct only',
            )

        fields = Struct.collect_fields(name, location, dct, _UserField)
        DSLType._import_base_type_info(name, location, dct)
        dct['_fields'] = fields


class Struct(BaseStruct):
    """
    Base class for all POD struct types.

    To define a new struct type, subclass Struct and put in its definition
    several Field or UserField instances.
    """

    __metaclass__ = _StructMetaclass

    _is_env_metadata = False
    """
    Whether this struct type is tagged to be used as metadata in lexical
    environments.

    :type: bool
    """


def env_metadata(cls):
    """
    Decorator to tag a Struct subclass as the type used for lexical
    environments metadata.

    The assumption that is done for the moment is that the struct subclass
    given as argument must only have boolean fields. In the context of
    Libadalang, the idea is that env metadata is to be used to express things
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
    _check_decorator_use(env_metadata, Struct, cls)

    with cls._diagnostic_context():
        check_source_language(
            _StructMetaclass.env_metadata is None,
            'Only one Struct subclass can be the env metadata'
        )
    _StructMetaclass.env_metadata = cls
    cls._is_env_metadata = True
    return cls


inherited_annotation = inherited_property(lambda s: s.get_parent_annotations())


class Annotations(object):
    def __init__(self, repr_name=None, generic_list_type=None,
                 warn_on_node=None):
        """
        Constructor for a node's annotations.

        :param str|None repr_name: The name to be used in repr for this node
            type.

        :param str|None generic_list_type: The name of the generic list type.
        """
        self.repr_name = repr_name
        self.generic_list_type = generic_list_type
        self._warn_on_node = warn_on_node

    @inherited_annotation
    def warn_on_node(self):
        return self._warn_on_node

    def process_annotations(self, node, is_root):
        self.node = node
        check_source_language(
            self.repr_name is None or isinstance(self.repr_name, str),
            'If provided, _repr_name must be a string (here: {})'.format(
                self.repr_name
            )
        )

        if self.generic_list_type is not None:
            check_source_language(
                is_root, 'Only the root AST node can hold the name of the'
                ' generic list type'
            )
            check_source_language(
                is_root, 'Name of the generic list type must be a string, but'
                ' got {}'.format(repr(self.generic_list_type))
            )

        if is_root and self._warn_on_node is None:
            self._warn_on_node = False

    def get_parent_annotations(self):
        """
        Get annotations for the base node.

        :rtype: Annotations
        """
        bn = self.node.base()
        return bn.annotations if bn else None


class _ASTNodeMetaclass(type):
    """
    Internal metaclass for AST node types, used to collect all ASTNode
    subclasses that language specifications declare.
    """

    astnode_types = []
    """
    List of all ASTNode subclasses, excluding ASTNode itself.

    :type: list[ASTNode]
    """

    root_type = None
    """
    The direct ASTNode subclass that is used as a root for the whole AST node
    hierarchy.

    :type: ASTNode
    """

    @classmethod
    def reset(cls):
        cls.astnode_types = []
        cls.root_type = None

    def __new__(mcs, name, bases, dct):
        # Does this subclass come from this module? If so, it's not to be
        # considered as a DSLType per se.
        is_base = ((len(bases) == 1 and bases[0] in (object, BaseStruct))
                   or dct.pop('_ASTNodeList__is_astnode_list_cls', False))

        # Is this the root AST node type?
        is_root = not is_base and mcs.root_type is None

        if not is_base:
            mcs.process_subclass(name, bases, dct, is_root)

        cls = type.__new__(mcs, name, bases, dct)

        if is_root:
            mcs.root_type = cls

        if not is_base:
            mcs.astnode_types.append(cls)

            # Create the corresponding ASTNodeType subclass
            if cls._base is _ASTNodeList:
                # Only root list types are supposed to directly subclass
                # _ASTNodeList.
                element_type = cls._element_type._type
                assert element_type
                astnode_type = element_type.list

            else:
                astnode_type = ASTNodeType(
                    cls._name, cls._location, cls._doc,
                    base=None if is_root else cls._base._type,
                    fields=cls._fields,
                    env_spec=cls._env_spec,
                    annotations=cls._annotations,
                )

            astnode_type.dsl_decl = cls
            cls._type = astnode_type

        return cls

    @classmethod
    def process_subclass(mcs, name, bases, dct, is_root):
        from langkit.envs import EnvSpec

        location = extract_library_location()
        base = bases[0]
        is_list_type = issubclass(base, _ASTNodeList)
        is_root_list_type = base is _ASTNodeList

        node_ctx = Context('in {}'.format(name), location)

        with node_ctx:
            check_source_language(len(bases) == 1,
                                  'ASTNode subclasses must have exactly one'
                                  ' base class')
            if mcs.root_type is not None:
                check_source_language(
                    base is not ASTNode,
                    'Only one class can derive from ASTNode (previous was:'
                    ' {})'.format(mcs.root_type.__name__)
                )

            env_spec = dct.pop('env_spec', None)
            check_source_language(
                env_spec is None or isinstance(env_spec, EnvSpec),
                'Invalid environment specification: {}'.format(env_spec)
            )

            annotations = dct.pop('annotations', None)

        # If this is a list type, determine the corresponding element type
        if is_root_list_type:
            element_type = dct.pop('_element_type')
        elif is_list_type:
            element_type = base._element_type
        else:
            element_type = None

        fields = ASTNode.collect_fields(name, location, dct, AbstractNodeData)

        # AST list types are not allowed to have syntax fields
        if is_list_type:
            syntax_fields = [f_n for f_n, f_v in fields
                             if not f_v.is_property]
            with node_ctx:
                check_source_language(
                    not syntax_fields,
                    'ASTNode list types are not allowed to have fields'
                    ' (here: {})'.format(', '.join(sorted(syntax_fields)))
                )

        DSLType._import_base_type_info(name, location, dct)
        dct['_fields'] = fields
        dct['_base'] = base
        dct['_env_spec'] = env_spec

        # Make sure subclasses don't inherit the "list_type" cache from their
        # base classes.
        dct['_list_type'] = None
        dct['_element_type'] = element_type
        dct['_annotations'] = annotations


class ASTNode(BaseStruct):
    """
    Base class for all AST node types.

    Exactly one class must derive from ASTNode directly, then all other
    subclasses must derive from this first subclass.
    """

    __metaclass__ = _ASTNodeMetaclass

    _base = None
    """
    Base class from which this AST node type derives, or None for the root
    type.

    :type: ASTNode|None
    """

    _env_spec = None
    """
    EnvSpec instance associated to this subclass, if any.
    :type: langkit.envs.EnvSpec|None
    """

    _has_abstract_list = None
    """
    Whether the list type corresponding to this AST node must be abstract.
    :type: bool
    """

    _list_type = None
    """
    Cache for the `list_type` method.
    :type: _ASTNodeList
    """

    @classproperty
    def list(cls):
        """
        Return an ASTNode subclass that represents a list of "cls".

        :rtype: _ASTNodeList
        """
        with cls._diagnostic_context():
            check_source_language(
                cls is not ASTNode,
                'Lists of base ASTNode are not allowed'
            )
        if cls._list_type is None:
            cls._list_type = type((cls._name + names.Name('List')).camel,
                                  (_ASTNodeList, ),
                                  {'_element_type': cls})
        return cls._list_type

    @classproperty
    def entity(cls):
        return T.Defer(lambda: cls._type.entity)

    def __new__(cls, *args):
        """
        This constructor can be used in the grammar to create a parser for this
        AST node.
        """
        from langkit.parsers import Row

        def get():
            assert cls._type
            return cls._type
        return Row(*args) ^ T.Defer(get)


class _ASTNodeList(ASTNode):
    """
    Internal base class for all AST node list types. Use ASTNode.list_type to
    build such classes.
    """
    __is_astnode_list_cls = True

    _element_type = None
    """
    ASTNode subclass that contains.
    :type: ASTNode
    """


def abstract(cls):
    """
    Decorator to tag an ASTNode subclass as abstract.

    :param ASTNode cls: Type parameter. The ASTNode subclass to decorate.
    """
    _check_decorator_use(abstract, ASTNode, cls)
    cls._type.abstract = True
    return cls


def synthetic(cls):
    """
    Decorator to tag an ASTNode subclass as synthetic.

    :param ASTNode cls: Type parameter. The ASTNode subclass to decorate.
    """
    _check_decorator_use(synthetic, ASTNode, cls)
    cls._type.synthetic = True
    return cls


def has_abstract_list(cls):
    """
    Decorator to make the automatically generated list type for "cls" (the
    "root list type") abstract.

    :param ASTNode cls: Type parameter. The AST node type to decorate.
    """
    _check_decorator_use(has_abstract_list, ASTNode, cls)
    cls._type.has_abstract_list = True
    return cls


class _EnumNodeMetaclass(type):

    enum_types = []
    """
    List of all EnumNode subclasses, excluding EnumNode itself.

    :type: list[EnumNode]
    """

    @classmethod
    def reset(cls):
        cls.enum_types = []

    def __new__(mcs, name, bases, dct):
        # Don't do anything for EnumNode itself: it's just a placeholder
        if bases == (BaseStruct, ):
            return type.__new__(mcs, name, bases, dct)

        location = extract_library_location()
        with Context('in {}'.format(name), location):

            qualifier = dct.pop('qualifier', False)
            if qualifier:
                alternatives = ['present', 'absent']
            else:
                alternatives = dct.pop('alternatives', None)
                check_source_language(
                    alternatives is not None,
                    'Missing "alternatives" field'
                )
                check_source_language(
                    isinstance(alternatives, list)
                    and all(isinstance(alt, str) for alt in alternatives),
                    'The "alternatives" field must contain a list of strings'
                )

        alts = [EnumNode.Alternative(names.Name.from_lower(alt))
                for alt in alternatives]
        fields = EnumNode.collect_fields(name, location, dct,
                                         (_UserField, PropertyDef))

        DSLType._import_base_type_info(name, location, dct)
        dct['_fields'] = fields
        dct['_alternatives'] = alts
        dct['_qualifier'] = qualifier

        # Make Alternative instances available as EnumNode class attributes for
        # a convenient way to create parsers for them.
        for alt in alts:
            attr_name = (names.Name('alt') + alt.name).lower
            dct[attr_name] = alt

        cls = type.__new__(mcs, name, bases, dct)

        mcs.enum_types.append(cls)
        for alt in alts:
            alt._enum_node_cls = cls

        return cls


class EnumNode(BaseStruct):
    """
    Using this base class, users can create a hierarchy of nodes deriving from
    the root node that are similar to an enum type. By declaring an EnumNode
    derived type in the following way::

        class Foo(T.EnumNode):
            alternatives = ['bar', 'baz']

    The user will get:

    * An abstract node type the base type, deriving from T.root_node, denoted
      by Foo.
    * A concrete but empty node type for every alternative of the enum type,
      that can be denoted by Foo.alt_{alt_name}.

    Instead of providing explicit alternatives, the user can just define the
    EnumNode as a qualifier node::

        class Foo(T.EnumNode):
            qualifier = True

    In which case, alternatives will automatically be "present" and "absent",
    and an as_bool method will be automatically generated.
    """

    __metaclass__ = _EnumNodeMetaclass

    _alternatives = None
    """
    List of alternatives: one for each enum value.
    :type: list[EnumNode.Alternative]
    """

    _qualifier = None
    """
    If provided and set to True, the alternatives used are "present" and
    "absent".

    :type: bool
    """

    class Alternative(object):
        def __init__(self, name):
            """
            :param names.Name name: Alternative name.
            """
            self.name = name

            self._enum_node_cls = None
            self._type = None

        @property
        def enum_node_cls(self):
            """
            EnumNode subclass that this alternative belongs to.
            """
            assert self._enum_node_cls
            return self._enum_node_cls

        @property
        def type(self):
            """
            ASTNodeType subclass corresponding to this alternative.
            """
            assert self._type
            return self._type

        @property
        def type_ref(self):
            """
            TypeRepo.Defer instance for the ASTNodeType subclass corresponding
            to this alternative.
            """
            def get():
                assert self._type
                return self._type
            return T.Defer(get)

        def __call__(self, *args):
            """
            Parser constructor for this alternative.
            :rtype: langkit.parsers.Parser
            """
            return self.enum_node_cls._create_parser(self.type_ref, *args)

    def __new__(cls, *args):
        """
        This constructor can be used in the grammar to create a parser for this
        EnumNode. This is valid only when qualifier is set to True.
        """
        assert cls._qualifier
        return cls._create_parser(None, *args)

    @classmethod
    def _create_parser(cls, typeref, *args):
        """
        Construct a parser for this EnumNode subclass.
        """
        from langkit.parsers import Row, Opt

        if cls._qualifier:
            # If the node is a boolean node, then we want to parse the
            # sub-parsers as an optional parser that will be booleanized.
            return Opt(*args).as_bool(cls)

        else:
            # Otherwise, we want to parse the sub-parsers as a row + transform
            return Row(*args) ^ typeref


class _EnumMetaclass(type):

    enum_types = set()
    """
    Set of all EnumType subclasses.

    :type: set[langkit.compiled_types.CompiledType]
    """

    _base_cls = None

    def __new__(mcs, name, bases, dct):
        DSLType._import_base_type_info(name, extract_library_location(), dct)

        cls = type.__new__(mcs, name, bases, dct)

        # If this is an EnumType subclass, register it and create the
        # corresponding CompiledType subclass.
        if mcs._base_cls:
            mcs.enum_types.add(cls)
            cls._type = _EnumType(
                names.Name.from_camel(name), cls._location, cls._doc,
                cls.alternatives, cls.suffix,
            )

        else:
            mcs._base_cls = cls

        return cls


class EnumType(DSLType):
    """
    Base class for compiled types that hold a single value in a set of possible
    ones.

    Subclasses must override the `alternatives` member to hold a list of
    distinct strings that represent the set of possibilities.  They represent
    the compiled type.

    Instances represent a particular enum value.
    """

    __metaclass__ = _EnumMetaclass

    alternatives = []
    """
    The list of alternatives for this EnumType subclass.

    :type: list[str]
    """

    suffix = ''
    """
    Suffix to use for the alternatives when they are invalid identifiers in
    some target language.

    :type: str
    """

    def __init__(self, alt):
        """
        Create a value that represent one of the enum alternatives.

        :param str alt: The alternative to use for this instance.
        """
        super(EnumType, self).__init__()
        assert alt in self.alternatives
        self.alt = alt

    @property
    def _enum_value(self):
        """
        Return the _EnumType.Alternative instance corresponding to this
        instance.

        :rtype: _EnumType.Alternative
        """
        assert self._type
        return _EnumType.Alternative(self._type, self.alt)


def Field(repr=True, doc=None, type=None):
    """
    Create a field that is meant to store parsing results. Only AST nodes can
    hold such fields.

    :param bool repr: Whether the field will be displayed when pretty-printing
        the embedding AST node.

    :param str|None doc: User documentation for this field.

    :param DSLType|CompiledType|None type: DSLType or CompiledType subclass for
        values this field holds. If left to None, the type will be inferred
        from the grammar.
    """
    return _Field(repr, doc, type)


def UserField(type, repr=False, doc=None, public=True):
    """
    Create a field that is not meant to store parsing results. Both AST nodes
    and Struct can hold such types.

    :param DSLType|CompiledType type: DSLType or CompiledType subclass for
        values this field holds.

    :param bool repr: Whether the field will be displayed when pretty-printing
        the embedding AST node.

    :param str|None doc: User documentation for this field.

    :param bool is_public: Whether this field is public in the generated APIs.
    """
    return _UserField(type, repr, doc, public)


class _BuiltinType(DSLType):
    """
    Base class for all built-in compiled types.
    """
    import langkit.compiled_types as _ct
    _ = _ct


class AnalysisUnitKind(_BuiltinType):
    """
    Type for the analysis unit kind enumeration.
    """
    _type = _BuiltinType._ct.analysis_unit_kind


class AnalysisUnitType(_BuiltinType):
    """
    Type for analysis unit values.
    """
    _type = _BuiltinType._ct.analysis_unit_type


class BoolType(_BuiltinType):
    """
    Type for boolean values.
    """
    _type = _BuiltinType._ct.bool_type


class EquationType(_BuiltinType):
    """
    An EquationType instance is an equation where logic variables (of type
    LogicVarType) are involved in logical expressions.

    An equation can be solved, and the variables instances will then be bound
    to specific values.

    Equations instance will typically be produced by expressions involving
    logic variables.
    """
    _type = _BuiltinType._ct.equation_type


class EnvRebindingsType(_BuiltinType):
    """
    Type for environment rebinding values.
    """
    _type = _BuiltinType._ct.env_rebindings_type


class LexicalEnvType(_BuiltinType):
    """
    Type for lexical environments.
    """
    _type = _BuiltinType._ct.lexical_env_type


class LogicVarType(_BuiltinType):
    """
    Type for logic variables, to be used in equations (see EquationType).
    """
    _type = _BuiltinType._ct.logic_var_type


class LongType(_BuiltinType):
    """
    Simple integer type.
    """
    _type = _BuiltinType._ct.long_type


class Symbol(_BuiltinType):
    """
    Type for symbol values (canonicalized names).
    """
    _type = _BuiltinType._ct.symbol_type


class Token(_BuiltinType):
    """
    Type for token values, as found in an analysis unit's token data handler.
    """
    _type = _BuiltinType._ct.token_type
