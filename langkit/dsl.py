from __future__ import annotations

import itertools
from typing import Any, ContextManager, Dict, List, Tuple, Type, Union

from langkit.compiled_types import (
    ASTNodeType, AbstractNodeData, CompiledTypeRepo, EnumType, Field as _Field,
    StructType, T, UserField as _UserField, resolve_type
)
from langkit.diagnostics import (
    Location, check_source_language, diagnostic_context,
    extract_library_location
)
from langkit.expressions import PropertyDef
import langkit.names as names
from langkit.utils import classproperty, inherited_property, issubtype


class DSLType:
    """
    Base class for the representation of types in the DSL.
    """

    @classmethod
    def new(cls, *args, **kwargs):
        """
        Shortcut to the New expression, allowing type.new(..) syntax.

        :rtype: AbstractExpression
        """
        from langkit.expressions.structs import New
        return New(cls, *args, **kwargs)

    @classproperty
    def array(cls):
        """
        Return the array type whose element type is `cls`.
        """
        return T.Defer(lambda: cls._resolve().array,
                       '{}.array'.format(cls._name.camel))

    @classmethod
    def _diagnostic_context(cls) -> ContextManager[None]:
        return diagnostic_context(cls._location)

    @staticmethod
    def _import_base_type_info(name, location, dct):
        """
        Metaclass helper.  Register in `dct` the name, location and doc
        information for the class to be built.
        """
        dct['_name'] = names.Name.from_camel(name)
        dct['_location'] = location
        dct['_doc'] = dct.get('__doc__', '')

    @classmethod
    def _resolve(cls):
        """
        Resolve to the CompiledType instance corresponding to this DSL type
        subclass.

        :rtype: langkit.compiled_types.CompiledType
        """
        # Provide a sane default for classes that are created by users, but we
        # will override this for builtin types.
        assert cls._type
        return cls._type

    _type = None
    """
    Link to the StructType/ASTNodeType subclass corresponding to this subclass.
    This is of course initialized only when we create the said
    StructType/ASTNodeType subclass.

    :type: langkit.compiled_types.CompiledType
    """

    _name: names.Name
    """
    Name for this type.
    """

    _location = None
    """
    Location in the language spec. for the declaration of this type, or None if
    it is created by Langkit.

    :type: langkit.diagnostics.Location|None
    """

    _doc = ''
    """
    Docstring associated to this type.

    :type: str
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
    def collect_fields(
        cls,
        owning_type: str,
        location: Location,
        dct: Dict[str, Any],
        field_cls: Union[Type[AbstractNodeData],
                         Tuple[Type[AbstractNodeData], ...]],
        only_null_fields: bool
    ) -> List[Tuple[str, AbstractNodeData]]:
        """
        Metaclass helper. Excluding __special__ entries, make sure all entries
        in `dct` are instances of `field_cls` and return them as an annotated
        list: (name, field).

        This ensures that all fields are associated to a legal name, and
        records this name in the field instances.

        :param owning_type: Name of the type for the type that will own the
            fields.  Used for diagnostic message formatting purposes.
        :param location: Location for the declaration of the owning type.
        :param dct: Input class dictionnary.
        :param field_cls: AbstractNodeData subclass, or list of subclasses.
        :param only_null_fields: Whether syntax fields, if accepted, must be
            null.
        """
        result = []
        for f_n, f_v in dct.items():
            # Ignore __special__ fields
            if f_n.startswith('__') and f_n.endswith('__'):
                continue

            with diagnostic_context(location):
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
                if only_null_fields and isinstance(f_v, _Field):
                    check_source_language(f_v.null,
                                          'Only null fields allowed here')
            result.append((f_n, f_v))

        # Sort fields by creation time order so that users get fields in the
        # same order as it was declared in the DSL.
        result.sort(key=lambda kv: kv[1]._serial)
        return result


def _check_decorator_use(decorator, expected_cls, cls):
    """
    Helper for class decorators below. Raise a diagnostic error if `cls`,
    which is the input parameter of `decorator`, is not a subclass of
    `expected_cls`.
    """
    location = extract_library_location()
    with diagnostic_context(location):
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

    struct_types: List[Struct] = []
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

        with diagnostic_context(location):
            check_source_language(
                bases == (Struct, ),
                'Struct subclasses must derive from Struct only',
            )

        fields = Struct.collect_fields(
            name, location, dct, _UserField, only_null_fields=False
        )
        DSLType._import_base_type_info(name, location, dct)
        dct['_fields'] = fields


class Struct(BaseStruct, metaclass=_StructMetaclass):
    """
    Base class for all POD struct types.

    To define a new struct type, subclass Struct and put in its definition
    several Field or UserField instances.
    """

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


class Annotations:
    def __init__(self, repr_name=None, generic_list_type=None,
                 warn_on_node=None, rebindable=False,
                 custom_short_image=False, snaps=False):
        """
        Constructor for a node's annotations.

        :param str|None repr_name: The name to be used in repr for this node
            type.
        :param str|None generic_list_type: The name of the generic list type.
        :param bool|None warn_on_node: Inherited attribute (False by default).
            Per-node switch to disable some warnings.
        :param bool rebindable: Whether lexical environments that belong to
            this kind of node can be rebound.
        :param bool custom_short_image: Whether this AST node must use a
            custom Short_Text_Image implementation. If true, extensions must
            add the declaration and the definition of a function called
            `[NODE_NAME]_Short_Text_Image` that takes the node in argument and
            that returns a `Text_Type` value.
        :param bool snaps: Whether this node's SLOCs are supposed to snap or
            not. Snapping designates the behavior where the start SLOC will be
            anchored to the previous token's end SLOC rather than the node's
            first token start SLOC, and conversely for the end SLOC.
        """
        self.repr_name = repr_name
        self.generic_list_type = generic_list_type
        self._warn_on_node = warn_on_node
        self._rebindable = rebindable
        self.custom_short_image = custom_short_image
        self._snaps = snaps

    @inherited_annotation
    def warn_on_node(self):
        return self._warn_on_node

    @inherited_annotation
    def rebindable(self):
        return self._rebindable

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
        bn = self.node.base
        return bn.annotations if bn else None

    @inherited_annotation
    def snaps(self):
        return self._snaps


class _ASTNodeMetaclass(type):
    """
    Internal metaclass for AST node types, used to collect all ASTNode
    subclasses that language specifications declare.
    """

    astnode_types: List[ASTNode] = []
    """
    List of all ASTNode subclasses, excluding ASTNode itself.
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

        # To determine if this class is the root node type, we use the fact
        # that we know ASTNode will be the first class after BaseStruct to be
        # created by this metaclass.
        is_root = not is_base and mcs.root_type is None

        if not is_base:
            cls = mcs.process_subclass(name, bases, dct, is_root)
        else:
            cls = type.__new__(mcs, name, bases, dct)

        if is_root:
            mcs.root_type = cls

        return cls

    @classmethod
    def process_subclass(mcs, name, bases, dct, is_root):
        from langkit.envs import EnvSpec

        location = extract_library_location()
        base = bases[0]
        is_list_type = issubclass(base, _ASTNodeList)
        is_root_list_type = base is _ASTNodeList

        def node_ctx():
            return diagnostic_context(location)

        with node_ctx():
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
        if is_list_type:
            element_type = (dct.pop('_element_type')
                            if is_root_list_type else base._element_type)
            allowed_field_types = PropertyDef
        else:
            element_type = None
            allowed_field_types = AbstractNodeData
        only_null_fields = False

        # Determine if this is a token node
        with node_ctx():
            is_token_node = dct.pop('token_node', None)
            check_source_language(
                is_token_node is None or isinstance(is_token_node, bool),
                'The "token_node" field, when present, must contain a boolean'
            )

            # If the "token_node" annotation is left to None, inherit it
            # (default is False).
            if is_token_node is None:
                is_token_node = bool(base._is_token_node)

            if is_token_node:
                allowed_field_types = (_UserField, PropertyDef)
            else:
                # Make sure that all derivations of a token node are token
                # nodes themselves.
                check_source_language(
                    not base._is_token_node,
                    '"token_node" annotation inconsistent with inherited AST'
                    ' node'
                )

        # Determine if this is an error node
        with node_ctx():
            is_error_node = dct.pop('error_node', None)
            check_source_language(
                is_error_node is None or isinstance(is_error_node, bool),
                'The "error_node" field, when present, must contain a boolean'
            )

            # If the "error_node" annotation is left to None, inherit it
            # (default is False).
            if is_error_node is None:
                is_error_node = bool(base._is_error_node)

            if is_error_node:
                check_source_language(not is_token_node,
                                      'Error nodes cannot also be token nodes')
                check_source_language(not is_list_type,
                                      'Error nodes cannot also be lists')
                allowed_field_types = (_Field, _UserField, PropertyDef)
                only_null_fields = True

            else:
                # Make sure that all derivations of an error node are error
                # nodes themselves.
                check_source_language(
                    not base._is_error_node,
                    '"error_node" annotation inconsistent with inherited node'
                )

        # Handle enum nodes
        with node_ctx():
            # Forbid inheriting from an enum node
            check_source_language(
                not base._is_enum_node,
                'Inheriting from an enum node is forbidden'
            )

            # Determine if this is an enum node
            is_enum_node = dct.pop('enum_node', False)
            check_source_language(
                isinstance(is_enum_node, bool),
                'The "enum_node" field, when present, must contain a boolean'
            )

            if is_enum_node:
                check_source_language(not is_error_node,
                                      'Error nodes cannot also be enum nodes')

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
                        isinstance(alternatives, list) and all(
                            isinstance(alt, str) for alt in alternatives
                        ),
                        'The "alternatives" field must contain a list of'
                        ' strings'
                    )

                alts = [_EnumNodeAlternative(names.Name.from_lower(alt))
                        for alt in alternatives]

                allowed_field_types = (_UserField, PropertyDef)

        fields = ASTNode.collect_fields(
            name, location, dct, allowed_field_types, only_null_fields
        )

        DSLType._import_base_type_info(name, location, dct)

        if is_enum_node:
            mcs.import_enum_node_attributes(dct, qualifier, alts, fields)

        dct['_fields'] = fields
        dct['_base'] = base
        dct['_env_spec'] = env_spec
        dct['_is_token_node'] = is_token_node
        dct['_is_error_node'] = is_error_node
        dct['_is_enum_node'] = is_enum_node

        # Make sure subclasses don't inherit the "list_type" cache from their
        # base classes.
        dct['_list_type'] = None
        dct['_element_type'] = element_type
        dct['_annotations'] = annotations

        cls = type.__new__(mcs, name, bases, dct)

        mcs.astnode_types.append(cls)

        # Create the corresponding ASTNodeType subclass
        if cls._base is _ASTNodeList:
            # Only root list types are supposed to directly subclass
            # _ASTNodeList.
            element_type = cls._element_type._resolve()
            assert element_type
            astnode_type = element_type.list
        else:
            astnode_type = ASTNodeType(
                cls._name, cls._location, cls._doc,
                base=None if is_root else cls._base._resolve(),
                fields=cls._fields,
                env_spec=cls._env_spec,
                annotations=cls._annotations,

                # Only enum nodes are abstract at this point
                is_abstract=cls._is_enum_node,

                is_enum_node=cls._is_enum_node,
                is_bool_node=cls._is_enum_node and cls._qualifier,
                is_token_node=cls._is_token_node,
                is_error_node=cls._is_error_node,
            )

        astnode_type.dsl_decl = cls
        cls._type = astnode_type

        if is_enum_node:
            mcs.create_enum_node_alternatives(cls, astnode_type)

        return cls

    @classmethod
    def import_enum_node_attributes(mcs, dct, qualifier, alts, fields):
        from langkit.expressions import AbstractProperty
        from langkit.parsers import Opt, _Row, _Transform

        def create_parser_bool_node(cls, *args):
            # If the node is a boolean node, then we want to parse the
            # sub-parsers as an optional parser that will be booleanized.
            return Opt(*args).as_bool(cls)

        def create_parser_enum_node(cls, alt_typeref, *args):
            # Otherwise, we want to parse the sub-parsers as a row + transform
            return _Transform(_Row(*args), alt_typeref)

        def constructor(cls, *args):
            """
            This constructor can be used in the grammar to create a parser for
            this enum node. This is valid only when qualifier is set to True.
            """
            assert qualifier
            return create_parser_bool_node(cls, *args)

        dct['__new__'] = constructor
        dct['_create_parser'] = classmethod(
            create_parser_bool_node if qualifier
            else create_parser_enum_node
        )

        dct['_alternatives'] = alts
        dct['_qualifier'] = qualifier

        # Make _EnumNodeAlternative instances available as attributes of the
        # enum node class for a convenient way to create parsers for them.
        for alt in alts:
            attr_name = (names.Name('Alt') + alt.name).lower
            dct[attr_name] = alt

        if qualifier:
            # Add the synthetic "as_bool" abstract property
            present_alt = alts[0]
            prop = AbstractProperty(
                type=T.Bool, public=True,
                doc='Return whether this is an instance of {}'.format(
                    (dct['_name'] + present_alt.name).camel
                )
            )
            prop.location = dct['_location']
            fields.append(('as_bool', prop))

    @classmethod
    def create_enum_node_alternatives(mcs, enum_cls, enum_type):
        from langkit.expressions import Property

        enum_type.alternatives = enum_cls._alternatives
        enum_type.is_type_resolved = True
        enum_type._alternatives = []
        enum_type._alternatives_map = {}

        for alt in enum_cls._alternatives:
            alt_name = enum_cls._name + alt.name

            # Generate the derived class corresponding to this alternative
            fields = []
            if enum_cls._qualifier:
                prop = Property(alt.name.lower == 'present')
                prop.location = enum_cls._location
                fields.append(('as_bool', prop))

            alt_type = ASTNodeType(
                name=alt_name, location=None, doc='',
                base=enum_type,
                fields=fields,
                dsl_name='{}.{}'.format(enum_type.dsl_name, alt.name.camel)
            )
            alt._type = alt_type
            alt._enum_node_cls = enum_cls

            # Make the alternative derived class accessible from the root
            # node for the enum.
            enum_type._alternatives.append(alt_type)
            enum_type._alternatives_map[alt.name.camel] = alt_type


class ASTNode(BaseStruct, metaclass=_ASTNodeMetaclass):
    """
    Base class for all AST node types.

    Exactly one class must derive from ASTNode directly, then all other
    subclasses must derive from this first subclass.
    """

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

    _is_token_node = None
    """
    Whether this node only materializes a token.
    :type: bool
    """

    _is_error_node = None
    """
    Whether this node only materializes syntax errors.
    :type: bool
    """

    _is_enum_node = None
    """
    Whether this node defines an enum node.
    :type: bool
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
        return T.Defer(lambda: cls._resolve().entity,
                       '{}.entity'.format(cls._name.camel))

    def __new__(cls, *args):
        """
        This constructor can be used in the grammar to create a parser for this
        AST node.
        """
        from langkit.parsers import _Row, _Transform

        return _Transform(_Row(*args), T.Defer(cls._resolve, cls._name.camel))


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
    with cls._type.diagnostic_context:
        check_source_language(
            not cls._type.is_error_node,
            "Error nodes cannot be abstract"
        )
    cls._type.abstract = True
    return cls


def synthetic(cls):
    """
    Decorator to tag an ASTNode subclass as synthetic.

    :param ASTNode cls: Type parameter. The ASTNode subclass to decorate.
    """
    _check_decorator_use(synthetic, ASTNode, cls)
    with cls._type.diagnostic_context:
        check_source_language(
            not cls._type.is_error_node,
            "Error nodes cannot be synthetic"
        )
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


class _EnumNodeAlternative:
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
        The enum node class that this alternative belongs to.
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
        return T.Defer(get, 'Alternative({})'.format(self.name.camel))

    def __call__(self, *args):
        """
        Parser constructor for this alternative.
        :rtype: langkit.parsers.Parser
        """
        return self.enum_node_cls._create_parser(self.type_ref, *args)


def Field(repr=True, doc='', type=None):
    """
    Create a field that is meant to store parsing results. Only AST nodes can
    hold such fields.

    :param bool repr: Whether the field will be displayed when pretty-printing
        the embedding AST node.

    :param str doc: User documentation for this field.

    :param DSLType|CompiledType|None type: DSLType or CompiledType subclass for
        values this field holds. If left to None, the type will be inferred
        from the grammar.
    """
    return _Field(repr, doc, type)


def AbstractField(type, doc=''):
    """
    Create an abstract field.

    Concrete node subclasses must override all inherited abstract fields, and
    only concrete fields can override abstract fields. Abstract fields are
    useful to make syntax fields available in abstract nodes when these don't
    have the same field index in derived nodes.

    :param DSLType|CompiledType type: DSLType or CompiledType subclass for
        values this field holds.

    :param str doc: User documentation for this field.
    """
    return _Field(type=type, doc=doc, abstract=True)


def NullField():
    """
    Create a null field.

    Null fields are valid only when they override an abstract field. They are a
    way to say that this field is absent on all concrete nodes that inherit
    this null field.
    """
    return _Field(null=True)


def UserField(type, repr=False, doc='', public=True, default_value=None):
    """
    Create a field that is not meant to store parsing results. Both AST nodes
    and Struct can hold such types.

    :param DSLType|CompiledType type: DSLType or CompiledType subclass for
        values this field holds.

    :param bool repr: Whether the field will be displayed when pretty-printing
        the embedding AST node.

    :param str doc: User documentation for this field.

    :param bool is_public: Whether this field is public in the generated APIs.

    :param None|AbstractExpression default_value: Default value for this field,
        when omitted from New expressions.
    """
    return _UserField(type, repr, doc, public, default_value)


class _EnumMetaclass(type):

    base_enum_type = None
    """
    This is set to Enum once this class it built.
    """

    enum_types: List[Enum] = []
    """
    List of all Enum subclasses.
    """

    @classmethod
    def reset(cls):
        cls.enum_types = []

    def __new__(mcs, name, bases, dct):
        # Don't do anything special for Enum itself
        if not mcs.base_enum_type:
            result = type.__new__(mcs, name, bases, dct)
            mcs.base_enum_type = result
            return result

        location = extract_library_location()
        with diagnostic_context(location):
            check_source_language(
                bases == (Enum, ),
                'Enumeration types must derive from and only from Enum'
            )

            # Get the list of values, initializing their name
            values = []
            default_val_name = None
            for key, value in dct.items():
                # Ignore __special__ fields
                if key.startswith('__') and key.endswith('__'):
                    continue

                check_source_language(
                    isinstance(value, EnumValue),
                    'Enum subclass can only contain EnumValue instances'
                    ' (here, {} is {})'.format(key, value)
                )
                check_source_language(
                    value._type is None,
                    'EnumValue instances cannot be used in multiple Enum'
                    ' subclasses (here: {})'.format(key)
                )
                value._name = names.Name.from_lower(key)
                values.append(value)

                # If this is the default value for this enum type, store it
                if value.is_default_val:
                    check_source_language(
                        default_val_name is None,
                        'Only one default value is allowed'
                    )
                    default_val_name = value._name

            values.sort(key=lambda v: v._id)
            dct['_values'] = values

        DSLType._import_base_type_info(name, location, dct)

        # Create the subclass and associate values to it
        cls = type.__new__(mcs, name, bases, dct)
        for value in cls._values:
            value._type = cls

        # Now create the CompiledType instance, register it where needed
        enum_type = EnumType(cls._name, cls._location, cls._doc,
                             [v._name for v in cls._values],
                             default_val_name=default_val_name)
        enum_type.dsl_decl = cls
        cls._type = enum_type

        # Associate the enumeration values in the DSL/Langkit internals
        for dsl_val, internal_val in zip(cls._values, enum_type.values):
            dsl_val._type = cls
            dsl_val._value = internal_val
            internal_val.dsl_decl = dsl_val

        return cls


class Enum(DSLType, metaclass=_EnumMetaclass):
    """
    Subclass this in order to create enumeration types.

    For instance::

        class MyEnum(Enum):
            a = EnumValue()
            b = EnumValue()
    """

    _values = None
    """
    List of enumeration values associated to this type.

    :type: list[EnumValue]
    """


class EnumValue:
    """
    Enumeration value, to be used when subclassing Enum.

    An enum type is nullable when it has an EnumValue that is the default value
    for this enum.
    """

    _next_id = iter(itertools.count(0))

    def __init__(self, is_default=False):
        self._id = next(self._next_id)
        """
        Program-wide unique identifier used to sort enumeration values by
        instantiation order.

        :type: int
        """

        self._name = None
        """
        Name for this value.

        :type: names.Name
        """

        self._value = None
        """
        :type: langkit.compiled_type.EnumValue
        """

        self._type = None
        """
        Enum subclass that owns this value.
        """

        self.is_default_val = is_default
        """
        Whether this value is the default value for this enum.
        """


class _BuiltinType(DSLType):
    """
    Base class for all built-in compiled types.
    """

    @classmethod
    def _resolve(cls):
        return CompiledTypeRepo.type_dict[cls._name.camel]


class _BuiltinValue:
    """
    Base class for all built-in values.
    """

    def __init__(self, resolver):
        """
        :param resolver: Callback that returns the
            ``langkit.expressions.AbstractExpression`` instance corresponding
            to this value.
        :type resolver: () -> langkit.expressions.AbstractExpression
        """
        self._resolver = resolver

    def _resolve(self):
        return self._resolver()

    @classmethod
    def _enum_value_resolver(cls, enum_type_name, value_name):
        return cls(
            lambda:
            resolve_type(getattr(T, enum_type_name)).resolve_value(value_name))


class LookupKind(_BuiltinType):
    """
    Type for the analysis unit kind enumeration.
    """
    _name = names.Name('Lookup_Kind')

    recursive = _BuiltinValue._enum_value_resolver('LookupKind', 'recursive')
    flat = _BuiltinValue._enum_value_resolver('LookupKind', 'flat')
    minimal = _BuiltinValue._enum_value_resolver('LookupKind', 'minimal')


class AnalysisUnitKind(_BuiltinType):
    """
    Type for the analysis unit kind enumeration.
    """
    _name = names.Name('Analysis_Unit_Kind')

    unit_specification = _BuiltinValue._enum_value_resolver(
        'AnalysisUnitKind', 'unit_specification')
    unit_body = _BuiltinValue._enum_value_resolver(
        'AnalysisUnitKind', 'unit_body')


class AnalysisUnit(_BuiltinType):
    """
    Type for analysis unit values.
    """
    _name = names.Name('Analysis_Unit')


class Bool(_BuiltinType):
    """
    Type for boolean values.
    """
    _name = names.Name('Bool')


class Equation(_BuiltinType):
    """
    An Equation instance is an equation where logic variables (of type
    LogicVar) are involved in logical expressions.

    An equation can be solved, and the variables instances will then be bound
    to specific values.

    Equations instance will typically be produced by expressions involving
    logic variables.
    """
    _name = names.Name('Equation')


class EnvRebindings(_BuiltinType):
    """
    Type for environment rebinding values.
    """
    _name = names.Name('Env_Rebindings')


class LexicalEnv(_BuiltinType):
    """
    Type for lexical environments.
    """
    _name = names.Name('Lexical_Env')


class LogicVar(_BuiltinType):
    """
    Type for logic variables, to be used in equations (see Equation).
    """
    _name = names.Name('Logic_Var')


class Address(_BuiltinType):
    """
    Opaque address type. Use to store data for extensions.
    """
    _name = names.Name('Address')


class Int(_BuiltinType):
    """
    Simple integer type.
    """
    _name = names.Name('Int')


class Symbol(_BuiltinType):
    """
    Type for symbol values (canonicalized names).
    """
    _name = names.Name('Symbol')


class Token(_BuiltinType):
    """
    Type for token values, as found in an analysis unit's token data handler.
    """
    _name = names.Name('Token')


class BigInt(_BuiltinType):
    """
    Type for integers of arbitrary precision.
    """
    _name = names.Name('Big_Int')
