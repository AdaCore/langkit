from __future__ import absolute_import

from collections import OrderedDict
import inspect
from itertools import count

from langkit import names
from langkit.c_api import CAPIType
from langkit.common import get_type, null_constant, is_keyword
from langkit.template_utils import TemplateEnvironment, common_renderer
from langkit.utils import memoized, type_check, col, Colors, common_ancestor


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
    return str(ada_type.name())


def c_node_type(capi):
    return CAPIType(capi, 'base_node')


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
        'is_array_type':    type_check(ArrayType),
        'decl_type':        decl_type,
    }
    if get_context():
        capi = get_context().c_api_settings

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
            'analysis_context_type': CAPIType(capi, 'analysis_context').name,
            'analysis_unit_type':    CAPIType(capi, 'analysis_unit').name,
            'node_kind_type':        CAPIType(capi, 'node_kind_enum').name,
            'node_type':             c_node_type(capi).name,
            'token_type':            CAPIType(capi, 'token').name,
            'sloc_type':             CAPIType(capi, 'source_location').name,
            'sloc_range_type':       SourceLocationRangeType.c_type(capi).name,
            'text_type':             CAPIType(capi, 'text').name,
            'diagnostic_type':       CAPIType(capi, 'diagnostic').name,
            'exception_type':        CAPIType(capi, 'exception').name,
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

    # Whether this type is handled through pointers only in the generated code
    is_ptr = True

    is_list_type = False
    """
    Whether this type represents an instantiation of ASTList (i.e. a list of
    AST nodes).
    :type: bool
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
        this type.

        Must be overriden in subclasses.
        """
        raise NotImplementedError()

    @classmethod
    def nullexpr(cls):
        """
        Return a string to be used in code generation for "null" expressions.

        Must be overriden in subclasses.
        """
        raise NotImplementedError()

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
            '{}ListType'.format(element_type.name()),
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

        :rtype: CompiledType
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
    _external = False

    @classmethod
    def name(cls):
        return names.Name(cls._name)

    @classmethod
    def nullexpr(cls):
        return cls._nullexpr

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, cls.name(), external=cls._external)


class LexicalEnvType(BasicType):
    _name = "AST_Envs.Lexical_Env"
    _nullexpr = "null"
    is_ptr = True

    @classmethod
    def c_type(cls, c_api_settings):
        raise Exception("Cannot expose lexical environments to C at the "
                        "moment")


class BoolType(BasicType):
    is_ptr = False
    _name = get_type(bool)
    _nullexpr = "false"

    @classmethod
    def c_type(cls, c_api_settings):
        # "bool" is not a built-in in C: do not force users to pull
        # stdbool.h...
        return CAPIType(c_api_settings, 'int', external=True)


class LongType(BasicType):
    is_ptr = False
    _name = get_type(long)
    _nullexpr = "0"
    _external = True


class SourceLocationRangeType(BasicType):
    is_ptr = False
    _name = "SourceLocationRange"
    _nullexpr = "SourceLocationRange()"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'source_location_range')


class Token(BasicType):
    is_ptr = False
    _name = "Token"
    _nullexpr = "No_Token"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'token')


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

    prefix = None
    """
    This can be overriden in subclasses of AbstractNodeData to add a prefix to
    the name of AbstractNodeData instances.

    :type: names.Name|None
    """

    def __init__(self, private=False):
        """
        :param bool private: Whether this AbstractNodeData instance is
            supposed to be private or not. For the moment since the Ada API is
            not well defined, private properties are not exposed in C and
            Python, but are public in Ada.
        """
        self._index = next(self._counter)
        self.is_private = private
        self._name = names.Name("")

        self.ast_node = None
        """
        ASTNode subclass that declared this field. Initialized when creating
        ASTNode subclasses (so left None for other Struct subclasses).
        :type: ASTNode
        """

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
        return (self.prefix + self._name if self.prefix else self._name)

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
        return names.Name(
            '{}_{}'.format(self.ast_node.name().base_name, self.name.base_name)
        )


class AbstractField(AbstractNodeData):
    """
    Placeholder descriptors used to associate data to AST nodes (see below).
    """

    concrete = False
    """
    Field used to prevent instantiation of the class. Concrete descendants
    of AbstractField must put that field to True in their definition.
    """

    prefix = names.Name("F")

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
        return '<ASTNode {} Field({})>'.format(self._index, self._name)

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
        self.is_private = is_private

    concrete = True


class BuiltinField(UserField):
    """
    A built-in field is just like a UserField, except that its name has no
    prefix. It is disregarded by the parsing machinery too.
    """

    prefix = names.Name("")

    def __init__(self, *args, **kwargs):
        super(BuiltinField, self).__init__(*args, **kwargs)


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
        # class.
        if is_root_grammar_class:
            dct["parent_env"] = BuiltinField(type=LexicalEnvType,
                                             is_private=True)
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

        fields_list = [(f_n, f_v) for f_n, f_v in dct.items()
                       if isinstance(f_v, AbstractNodeData)]

        # Gather the fields and properties in a dictionary. Recover the order
        # of field declarations.  See the Field class definition for more
        # details.
        fields = OrderedDict(sorted(fields_list, key=lambda (_, f): f._index))

        for field_name, field in fields.items():
            # Remove fields/props as class members: we want them to be
            # stored in their own dicts.
            dct.pop(field_name)
            # Store the name of the field in the field
            field.name = names.Name.from_lower(field_name)

        dct['_fields'] = fields

        # By default, ASTNode subtypes aren't abstract
        dct['abstract'] = False

        dct['is_type_resolved'] = False
        cls = type.__new__(mcs, name, bases, dct)

        # Associate each field and property to this ASTNode subclass
        for field in fields.values():
            field.ast_node = cls

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

        def is_subtype(base_type, subtype):
            return issubclass(subtype, base_type)

        def are_subtypes(fields, new_types):
            return all(
                is_subtype(f.type, n)
                for f, n in zip(fields, new_types)
            )

        # TODO: instead of expecting types to be *exactly* the same, perform
        # type unification (take the nearest common ancestor for all field
        # types).
        assert (not cls.is_type_resolved or
                are_subtypes(fields, types)), (
            "Already associated types for some fields are not consistent with"
            " current ones:\n- {}\n- {}".format(
                [f.type for f in fields], types
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
    def compute_properties(cls):
        """
        Render generated code for properties.

        Since we handle properties on AST nodes only, there is nothing to do in
        the general case: this is overriden in ASTNode.
        """
        pass

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

        :rtype: list[langkit.expressions.Property]
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
        :type predicate: None|(Field) -> bool

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

            cls.compute_properties()

            if cls.env_spec:
                cls.env_spec.compute(cls)

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

    @classmethod
    def compute_properties(cls):
        for p in cls.get_properties(include_inherited=False):
            p.render(cls)

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
        multiple times.
        """
        return cls.get_abstract_fields(
            include_inherited=False,
            predicate=lambda f: not f.is_private
        )

    @classmethod
    def is_ast_node(cls):
        return True

    @classmethod
    def c_type(cls, c_api_settings):
        return c_node_type(c_api_settings)

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
        return CAPIType(c_api_settings, cls.api_name().lower)

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
        return CAPIType(c_api_settings, cls.base_name().lower)

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
