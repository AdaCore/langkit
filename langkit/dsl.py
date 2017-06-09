from __future__ import absolute_import, division, print_function

from langkit.compiled_types import AbstractField
from langkit.diagnostics import (
    Context, check_source_language, extract_library_location
)
import langkit.names as names
from langkit.utils import issubtype


class BaseStruct(object):
    """
    Base class for Struct and ASTNode.
    """

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
        for f_n, f_v in dct.items():
            if f_n.startswith('__') and f_n.endswith('__'):
                continue
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
