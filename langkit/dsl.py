from __future__ import absolute_import, division, print_function

from langkit.compiled_types import AbstractField, UserField, T
from langkit.diagnostics import (
    Context, check_source_language, extract_library_location
)
from langkit.expressions import PropertyDef
import langkit.names as names
from langkit.utils import issubtype


class DSLType(object):
    """
    Base class for the representation of types in the DSL.
    """

    @classmethod
    def array_type(cls):
        """
        Return the array type whose element type is `cls`.
        """
        return T.Defer(lambda: cls._type.array_type())

    @classmethod
    def _diagnostic_context(cls):
        ctx_message = 'in {}'.format(cls._name.camel)
        return Context(ctx_message, cls._location)

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


class BaseStruct(DSLType):
    """
    Base class for Struct and ASTNode.
    """

    _fields = None
    """
    List of fields for this type. For Struct subclasses, this is actually a
    list of AbstractField. For ASTNode subclasses, this excludes inherited
    fields.

    :type: list[langkit.compiled_types.AbstractNodeData]
    """


def check_decorator_use(decorator, expected_cls, cls):
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


def filter_out_special_fields(dct):
    """
    Helper for metaclasses. Return dct without the special fields (__foo__).

    :param dict[str, T] dct: Class attributes dictionnary.
    :rtype: dict[str, T]
    """
    return {k: v for k, v in dct.items()
            if not k.startswith('__') or not k.endswith('__')}


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

        return cls

    @classmethod
    def process_subclass(mcs, name, bases, dct):
        location = extract_library_location()

        with Context('in {}'.format(name), location):
            check_source_language(
                bases == (Struct, ),
                'Struct subclasses must derive from Struct only',
            )

        # Make sure all fields are AbstractField instances; assign them
        # their name.
        fields = []
        for f_n, f_v in filter_out_special_fields(dct).items():
            fields.append((f_n, f_v))
            with Context('in {}.{}'.format(name, f_n), location):
                check_source_language(
                    isinstance(f_v, AbstractField),
                    'Field {} is a {}, but only Field/UserField instances'
                    ' are allowed in Struct subclasses'.format(f_n,
                                                               type(f_v))
                )
                check_source_language(
                    not f_n.startswith('_'),
                    'Underscore-prefixed field names are not allowed'
                )
                f_v.name = names.Name.from_lower(f_n)

        dct['_name'] = names.Name.from_camel(name)
        dct['_location'] = location
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
    check_decorator_use(env_metadata, Struct, cls)

    with cls._diagnostic_context():
        check_source_language(
            _StructMetaclass.env_metadata is None,
            'Only one Struct subclass can be the env metadata'
        )
    _StructMetaclass.env_metadata = cls
    cls._is_env_metadata = True
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
        if bases == (DSLType, ):
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

        fields = []
        for f_n, f_v in filter_out_special_fields(dct).items():
            fields.append((f_n, f_v))
            with Context('in {}.{}'.format(name, f_n), location):
                check_source_language(
                    isinstance(f_v, (UserField, PropertyDef)),
                    'Field {} is a {}, but only UserField/Property instances'
                    ' are allowed in EnumNode subclasses'.format(f_n,
                                                                 type(f_v))
                )
                check_source_language(
                    not f_n.startswith('_'),
                    'Underscore-prefixed field names are not allowed'
                )
                f_v.name = names.Name.from_lower(f_n)

        dct = {
            '_name': names.Name.from_camel(name),
            '_location': location,
            '_alternatives': alts,
            '_qualifier': qualifier,
            '_fields': fields,
        }

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


class EnumNode(DSLType):
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

    _fields = None
    """
    List of fields for this type.
    :type: list[langkit.compiled_types.AbstractNodeData]
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
