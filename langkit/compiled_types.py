from collections import OrderedDict
from itertools import count

from c_api import CAPIType
from common import get_type, null_constant, is_keyword
from compile_context import get_context
from expressions import Property, AbstractNodeField
import names
from python_api import PythonAPIType
from template_utils import TemplateEnvironment, common_renderer
from utils import memoized, type_check


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
        'decl_type':        decl_type,
    }
    if get_context():
        capi = get_context().c_api_settings
        template_args.update({
            'ctx':                   get_context(),
            'ada_api':               get_context().ada_api_settings,
            'capi':                  capi,
            'analysis_context_type': CAPIType(capi, 'analysis_context').name,
            'analysis_unit_type':    CAPIType(capi, 'analysis_unit').name,
            'node_kind_type':        CAPIType(capi, 'node_kind_enum').name,
            'node_type':             CAPIType(capi, 'node').name,
            'token_type':            CAPIType(capi, 'token').name,
            'sloc_type':             CAPIType(capi, 'source_location').name,
            'sloc_range_type':       SourceLocationRangeType.c_type(capi).name,
            'text_type':             CAPIType(capi, 'text').name,
            'diagnostic_type':       CAPIType(capi, 'diagnostic').name,
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

    def __init__(self):
        assert False, (
            'CompiledType subclasses are not meant to be instantiated'
        )

    @classmethod
    def add_to_context(cls):
        """
        If needed, put bits into the global context to implement this compiled
        type.

        Must be overriden in subclasses if needed.
        """
        pass

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
    def py_type(cls, python_api_settings):
        """Return a PythonAPIType instance for this type.

        Must be overriden in subclasses.

        :param PythonApiSettings python_api_settings: The settings for the C
            API.
        """
        raise NotImplementedError()


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


class BoolType(BasicType):
    is_ptr = False
    _name = get_type(bool)
    _nullexpr = "false"

    @classmethod
    def c_type(cls, c_api_settings):
        # "bool" is not a built-in in C: do not force users to pull
        # stdbool.h...
        return CAPIType(c_api_settings, 'int', external=True)

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'c_int', True)


class LongType(BasicType):
    is_ptr = False
    _name = get_type(long)
    _nullexpr = "0"
    _external = True

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'c_long', True)


class SourceLocationRangeType(BasicType):
    is_ptr = False
    _name = "SourceLocationRange"
    _nullexpr = "SourceLocationRange()"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'source_location_range')

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'SlocRange', False)


class Token(BasicType):
    is_ptr = False
    _name = "Token"
    _nullexpr = "No_Token"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'token')

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'token', False)


class Field(AbstractNodeField):
    """
    Placeholder descriptors used to associate data to AST nodes (see below).
    """

    # Hack: the field declarations order in AST nodes matters.  The simple and
    # very handy syntax we use here for such declarations doesn't preserve this
    # order in Python2, however.  Waiting for the move to Python3, we use a
    # hack here: the following counter will help us to recover the declaration
    # order (assuming it is the same as the Field instantiation order).
    _counter = iter(count(0))

    def __init__(self, repr=True, doc=None, type=None):
        """Create an AST node field.

        :param bool repr: If true, the field will be displayed when
            pretty-printing the embedding AST node.
        :param str|None doc: User documentation for this field.
        """
        self.repr = repr
        self._name = None
        self._index = next(self._counter)
        self._doc = doc

        self.ast_node = None
        """
        ASTNode subclass that declared this field. Initialized when creating
        ASTNode subclasses.
        :type: ASTNode
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

    @property
    def name(self):
        """
        :rtype: names.Name
        """
        assert self._name
        return names.Name("F") + self._name

    @name.setter
    def name(self, name):
        assert isinstance(name, names.Name)
        self._name = name

    def __repr__(self):
        return '<ASTNode {} Field({})>'.format(self._index, self._name)

    def doc(self):
        return self._doc


class AstNodeMetaclass(type):
    """
    Internal metaclass for AST nodes, used to ease fields handling during code
    generation.
    """
    def __new__(mcs, name, bases, dct):
        assert len(bases) == 1, (
            "Multiple inheritance for AST nodes is not supported")

        # Gather the fields in a dictionary
        fields = OrderedDict(sorted(
            ((f_n, f_v)
             for f_n, f_v in dct.items() if isinstance(f_v, Field)),
            # Recover the order of field declarations.  See the Field
            # class definition for more details.
            key=lambda (_, f): f._index
        ))

        # Gather the properties in a dictionary
        properties = OrderedDict(sorted(
            [(f_n, f_v) for f_n, f_v in dct.items()
             if isinstance(f_v, Property)],
            key=lambda (n, v): n
        ))

        for field_name, field in fields.items() + properties.items():
            # Remove fields/props as class members: we want them to be
            # stored in their own dicts.
            dct.pop(field_name)
            # Store the name of the field in the field
            field.name = names.Name.from_lower(field_name)

        dct['_fields'] = fields
        dct['_properties'] = properties

        # By default, ASTNode subtypes aren't abstract
        dct['abstract'] = False

        dct['is_type_resolved'] = False
        cls = type.__new__(mcs, name, bases, dct)

        # Associate each field and property to this ASTNode subclass
        for field in fields.values():
            field.ast_node = cls
        for prop in properties.values():
            prop.ast_node = cls

        return cls


def abstract(cls):
    """
    Decorator to tag an ASTNode subclass as abstract.

    :param ASTNode cls: Type parameter. The ASTNode subclass to decorate.
    """
    assert issubclass(cls, ASTNode)
    cls.abstract = True
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


class ASTNode(CompiledType):
    """
    Base class for all user AST nodes.

    Subclasses can define new AST node types, but they also can be abstract
    themselves (to form a true tree of AST node types).  Each subclass can
    define a list of fields (see the above Field class), so that each concrete
    class' fields are the sum of all its subclass' fields plus its own.

    This base class defines utilities to emit native code for the AST node
    types: type declaration and type usage (to declare AST node variables).
    """

    _fields = OrderedDict()
    """
    The fields for this ASTNode, instantiated by the metaclass
    :type: dict[str, Field]
    """

    _properties = OrderedDict()
    """
    The properties for this ASTNode, instantiated by the metaclass
    :type: dict[str, Property]
    """

    __metaclass__ = AstNodeMetaclass

    @classmethod
    def compute_properties(cls):
        for p in cls._properties.values():
            p.render(cls)

    @classmethod
    def create_type_definition(cls):
        """
        Emit a type definition for this AST node type in
        `context.types_definitions`, emit:
          - a class with all its fields and its methods;
          - a forward declaration for this AST node type's "nil" singleton.

        Also emit the implementation for the corresponding methods/singletons
        in `context.body`.
        """
        base_class = cls.__bases__[0]

        t_env = TemplateEnvironment(
            cls=cls,
            base_name=base_class.name()
        )
        tdef_incomp = TypeDeclaration.render(
            'astnode_type_def_incomplete_ada', t_env, cls)
        tdef = TypeDeclaration.render('astnode_type_def_ada', t_env, cls)
        get_context().incomplete_types_declarations.append(tdef_incomp)
        get_context().types_declarations.append(tdef)

        get_context().primitives_bodies.append(
            render('astnode_type_impl_ada', t_env))

    @classmethod
    def get_inheritance_chain(cls):
        """
        Return a list for all classes from ASTNode to `cls` in the inheritance
        chain.
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

        :rtype: list[Property]
        """
        return cls._get_abstract_fields(predicate, include_inherited,
                                        field_name='_properties')

    @classmethod
    def get_fields(cls, predicate=None, include_inherited=True):
        """
        Return the list of all the fields `cls` has, including its parents'.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[Field]
        """
        return cls._get_abstract_fields(predicate, include_inherited)

    @classmethod
    def _get_abstract_fields(cls, predicate=None, include_inherited=True,
                             field_name='_fields'):
        if include_inherited:
            fields = []
            for base_class in cls.get_inheritance_chain():
                fields.extend(getattr(base_class, field_name).values())
        else:
            fields = getattr(cls, field_name).values()
        return filter(predicate or (lambda f: True), fields)

    @classmethod
    def get_abstract_fields(cls, predicate=None, include_inherited=True):
        """
        Get all AbstractField instances for the class.

        :param predicate: Predicate to filter fields if needed.
        :type predicate: None|(Field) -> bool

        :param bool include_inherited: If true, include inheritted fields in
            the returned list. Return only fields that were part of the
            declaration of this node otherwise.

        :rtype: list[AbstractField]
        """
        ret = cls.get_fields(predicate, include_inherited)
        ":type: list[AbstractField]"

        props = cls.get_properties(predicate, include_inherited)
        ret.extend(props)

        return ret

    @classmethod
    def add_to_context(cls):
        """
        Emit code in the global context for this AST node type.  Do nothing if
        called more than once on a single class or if called on ASTNode itself.
        """
        if cls not in get_context().types and cls != ASTNode:
            base_class = cls.__bases__[0]
            if issubclass(base_class, ASTNode):
                base_class.add_to_context()

            get_context().types.add(cls)
            cls.compute_properties()
            cls.create_type_definition()

            # Generate abstract field accessors (C public API) for this node
            # kind.
            primitives = []
            for field in cls.get_abstract_fields(include_inherited=False):
                accessor_basename = names.Name(
                    '{}_{}'.format(cls.name().base_name,
                                   field.name.base_name)
                )
                accessor_fullname = get_context().c_api_settings.get_name(
                    accessor_basename
                )

                t_env = TemplateEnvironment(
                    astnode=cls,
                    field=field,
                    accessor_name=accessor_fullname,
                )
                accessor_decl = render(
                    'c_api/astnode_field_access_decl_ada', t_env)
                accessor_impl = render(
                    'c_api/astnode_field_access_impl_ada', t_env)
                accessor_c_decl = render(
                    'c_api/astnode_field_access_decl_c', t_env)

                primitives.append(AbstractFieldAccessor(
                    accessor_basename,
                    declaration=accessor_decl,
                    implementation=accessor_impl,
                    c_declaration=accessor_c_decl,
                    field=field,
                ))
            get_context().c_astnode_primitives[cls] = primitives

            # For the Python API, generate subclasses for each AST node kind
            # (for both abstract and concrete classes). Each will ship accessor
            # for the fields they define.
            if get_context().python_api_settings:
                get_context().py_astnode_subclasses[cls] = render(
                    'python_api/ast_subclass_py',
                    pyapi=get_context().python_api_settings,
                    cls=cls,
                    parent_cls=list(cls.get_inheritance_chain())[-2],
                    primitives=primitives,
                )

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
            return "nil_{0}".format(cls.name())

    @classmethod
    def doc(cls):
        # Yield documentation only for user types: types defined in Langkit
        # have documentation that targets Langkit users.
        return cls.__doc__ if cls != ASTNode else None

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'node')

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'node', False)


# We tag the ASTNode class as abstract here, because of the circular dependency
# between the @abstract decorator and the ASTNode class, which is caused by the
# assert statement that is inside the decorator.
ASTNode.abstract = True


# We want structural equality on lists whose elements have the same types.
# Memoization is one way to make sure that, for each CompiledType subclass X::
#    list_type(X) == list_type(X)
@memoized
def list_type(element_type):
    """
    Return an ASTNode subclass that represent a list of `element_type`.

    :param ASTNode element_type: Type parameter. The type of the elements of
        the resulting list.
    """

    # noinspection PyUnusedLocal
    @classmethod
    def name(cls):
        return names.Name('List') + element_type.name()

    @classmethod
    def add_to_context(cls):
        if cls in get_context().types:
            return
        get_context().types.add(cls)
        get_context().list_types.add(element_type)

        # Make sure the type this list contains is already declared
        element_type.add_to_context()

        t_env = TemplateEnvironment(element_type=element_type)
        get_context().list_types_declarations.append(TypeDeclaration.render(
            'astlist_def_ada', t_env, cls
        ))

    return type(
        '{}ListType'.format(element_type.name()), (ASTNode, ), {
            'is_ptr':         True,
            'add_to_context': add_to_context,
            'name':           name,
            'nullexpr':       classmethod(lambda cls: null_constant()),
        }
    )


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
            render = make_renderer().render
            get_context().types.add(cls)
            get_context().enum_declarations.append(TypeDeclaration.render(
                'enum_type_decl_ada', None, cls, cls=cls
            ))
            get_context().c_astnode_field_types[cls] = render(
                'c_api/enum_type_decl_c', cls=cls,
            )
            get_context().c_astnode_field_types_ada[cls] = render(
                'c_api/enum_type_spec_ada', cls=cls,
            )
            if get_context().python_api_settings:
                get_context().py_astnode_field_types[cls] = render(
                    'python_api/enum_type_decl_py', cls=cls,
                    pyapi=get_context().python_api_settings
                )

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
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'c_uint', True)

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
