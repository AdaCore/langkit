from __future__ import annotations

import inspect
from typing import (
    Callable, Generic, Iterable, Protocol, TYPE_CHECKING, Type, TypeVar
)


_DerivableType = TypeVar("_DerivableType", bound="_DerivableTypeProtocol")


if TYPE_CHECKING:
    from typing_extensions import ParamSpec

    from langkit.compile_context import CompileCtx
    from langkit.compiled_types import ASTNodeType
    from langkit.diagnostics import Location

    import liblktlang as L

    _P = ParamSpec("_P")
    _T = TypeVar("_T")
    _RT = TypeVar("_RT")

    class _DerivableTypeProtocol(Protocol[_DerivableType]):
        @property
        def base(self) -> _DerivableType | None: ...

        subclasses: list[_DerivableType]

        def get_inheritance_chain(self) -> list[_DerivableType]: ...

        def unify(
            self,
            other: _DerivableType,
            error_location: Location | L.LktNode,
            error_msg: str | None = None,
        ) -> _DerivableType: ...


def type_check_instance(cls: Type) -> Callable[[Type], bool]:
    """
    Return a predicate that will return true if its parameter is a subclass
    of `cls`.

    :param cls: Class to check against.
    """
    return lambda t: isinstance(t, cls)


def assert_type(obj: object, typ: Type[_T]) -> _T:
    """
    Ensure that obj is a subclass or instance of type, depending on whether obj
    is a type or an instance. Return obj. This allows a few things compared to
    regular asserts:

    1. The type of the return is correctly inferred by Pycharm.
    2. You can use a "fluent interface" style for style checks, using::

        b = assert_type(c, int) + 15

    rather than::

        assert isinstance(c, int)
        b = c + 15

    3. Provides a relatively informative error message by default.

    :param Any obj: The object to check.
    :param T typ: Type parameter. The expected type of obj.
    :rtype: T
    """
    if issubclass(type(obj), type):
        assert isinstance(obj, type)
        assert issubclass(obj, typ), (
            "Wrong base type for type {}, expected {}".format(obj, typ)
        )
    else:
        assert isinstance(obj, typ), (
            "Wrong type for object {}, expected {}, found {}".format(
                obj, typ, type(obj)
            )
        )
    return obj


class TypeSet(Generic[_DerivableType]):
    """
    This class is an helper for when you need to check wether an abstract
    operation was fulfilled for a whole hierarchy of classes, given that if
    the operation is fulfilled for all subclasses, then we consider that it is
    done for the parent class.

    This allows to automate the logic of, for example, checking if abstract
    properties are overridden properly, or if a match expression handles all
    cases.
    """

    def __init__(self, types: Iterable[_DerivableType] = set()):
        """
        :param types: Initial set of matched types.
        """
        # Working set of ASTNodeType instances for the types that are covered
        # by matchers. Updated as we go through the list of matchers.
        self.matched_types: set[_DerivableType] = set()
        for t in types:
            self.include(t)

    def __eq__(self, type_set: object) -> bool:
        return (
            isinstance(type_set, TypeSet)
            and self.matched_types == type_set.matched_types
        )

    def __ne__(self, type_set: object) -> bool:
        return not (self == type_set)

    def __contains__(self, t: _DerivableType) -> bool:
        """
        Check whether ``t`` is included in this type set.
        """
        return t in self.matched_types

    def update(self, type_set: TypeSet[_DerivableType]) -> None:
        """
        Extend self to contain all types in ``type_set``.

        :param type_set: Types to include.
        """
        assert isinstance(type_set, TypeSet)
        for t in type_set.matched_types:
            self.include(t)

    def include(self, t: _DerivableType) -> bool:
        """
        Include a class and all of its subclasses.

        If `t` is the last subclass for some base class, this adds the parent
        subclass. This makes sense as once all concrete subclasses of the
        abstract `A` type are handled, it is true that `A` is handled.

        Return whether `t` was already present in `self`.

        :param t: AST node to include.
        """
        if t in self.matched_types:
            return True

        # Include "t" and all its subclasses
        self.matched_types.add(t)
        for child in t.subclasses:
            self.include(child)

        # Include its parents if all their children are matched. Stop right
        # before the first parent that is concrete.
        parents = list(t.get_inheritance_chain())[:-1]
        for parent in reversed(parents):
            if not parent.abstract or parent in self.matched_types:
                break

            subclasses = set(parent.concrete_subclasses)
            if not subclasses.issubset(self.matched_types):
                break

            # If we reach this point, all parent's concrete subclasses are
            # matched, so we can state that parent itself is always matched.
            self.matched_types.add(parent)

            # Also include subclasses: we may add abstract subclasses which
            # have no concrete subclasses themselves. Typically: the generic
            # list type (which is abstract) while there is no list in the
            # grammar.
            for s in parent.subclasses:
                self.include(s)

        return False

    def exclude(self, t: _DerivableType) -> None:
        """
        Exclude a class and all of its subclasses.

        :param t: Node to exclude.
        """
        if t not in self.matched_types:
            return

        def remove_children(t: _DerivableType) -> None:
            self.matched_types.discard(t)
            for child in t.subclasses:
                remove_children(child)

        # Exclude "t" and all its subclasses
        remove_children(t)

        # Make sure abstract parents are excluded too
        for parent in t.get_inheritance_chain():
            if not parent.abstract:
                break
            self.matched_types.discard(parent)

    def unmatched_types(self, t: _DerivableType) -> set[_DerivableType]:
        """
        Return the set of `t` subclasses that are not matched by any matcher.

        Omit subclasses when none of them are matched: only the parent is
        returned in this case, so that we don't flood users with whole
        hierarchies of classes.
        """
        if t in self.matched_types:
            return set()

        subclasses = set(t.subclasses)
        if subclasses & self.matched_types:
            result = set()
            for cls in subclasses:
                result.update(self.unmatched_types(cls))
        else:
            result = {t}
        return result

    @property
    def minimal_matched_types(self) -> set[_DerivableType]:
        """
        Return the minimal set of subclasses that are matched.

        The result is "minimal" in the sense that if B derives from A and both
        are matched, the result includes only A.
        """
        return {t for t in self.matched_types
                if t.base is None or t.base not in self.matched_types}

    @property
    def minimal_common_type(self) -> _DerivableType:
        """
        Return the most specific common ancestor type for matched types.
        """
        from langkit.diagnostics import Location

        types = list(self.matched_types)
        assert types

        result = types.pop()
        while types:
            result = result.unify(types.pop(), Location.nowhere)
        return result


def issubtype(type: object, parent_type: Type) -> bool:
    """
    Helper wrapper around issubclass, because issubclass will raise an
    exception if compared object is not a class, which is extremely tedious for
    us in langkit.

    :param type: The object or type to check against parent_type.
    :param type parent_type: The supposed parent_type of type.
    """
    return inspect.isclass(type) and issubclass(type, parent_type)


def not_implemented_error(
    self_or_cls: object,
    method: object,
) -> NotImplementedError:  # no-code-coverage
    """
    Return a NotImplementedError instance to explain that `self or_cls` must
    override method `method`.

    Use this instead of ``abc.abstractmethod``/``abc.abstractproperty`` to
    implement methods/properties that are voluntarily left unimplemented, but
    for which the owning type must be considered as concrete.

    :param or_cls: Class that does not implement `method`, or the instance of
        such a class.
    :param method: Method object for the method that should be overriden.
    """
    cls: type = (
        self_or_cls if inspect.isclass(self_or_cls) else type(self_or_cls)
    )
    func = method.fget if isinstance(method, property) else method
    return NotImplementedError('{} must override method {}'.format(
        getattr(cls, "__name__"),
        getattr(func, "__name__")
    ))


def astnode_kind_set(context: CompileCtx, nodes: Iterable[ASTNodeType]) -> str:
    """
    Turn a set of AST nodes into a the corresponding set of AST node kinds.

    The result is suitable to use in Ada's `X in Y | Z` construct.

    :param set[langkit.compiled_types.ASTNodeType] nodes: Set of AST
        nodes to process. Abstract AST nodes are expanded into the set of
        their concrete subclasses.
    :rtype: str
    """
    # Compute the set of concrete AST nodes that is covered by `nodes`
    concrete_nodes = set()
    for n in nodes:
        if n.abstract:
            concrete_nodes.update(n.concrete_subclasses)
        else:
            concrete_nodes.add(n)

    kinds = sorted(context.node_kind_constants[astnode]
                   for astnode in concrete_nodes)

    # Try to collapse sequences of contiguous kinds into ranges
    first_kind = kinds.pop(0)
    groups = [(first_kind, first_kind)]
    for k in kinds:
        first, last = groups[-1]
        if k == last + 1:
            groups[-1] = (first, k)
        else:
            groups.append((k, k))

    # Turn numeric constants into enumeration names
    groups_names = [
        (context.kind_constant_to_node[f].ada_kind_name,
         context.kind_constant_to_node[l].ada_kind_name)
        for f, l in groups
    ]

    return ' | '.join(
        (f if f == l else '{} .. {}'.format(f, l))
        for f, l in groups_names
    )


def collapse_concrete_nodes(
    input_type: ASTNodeType,
    astnodes: Iterable[ASTNodeType],
) -> tuple[list[set[ASTNodeType]], set[ASTNodeType]]:
    """
    Compute the set of concrete subclasses for `input_types` that match each
    AST node in `astnodes`. For each item in `astnodes`, we exclude the set of
    subclasses already returned for the previous item.

    This is useful to implement Match-like constructs: for each matcher, this
    computes the set of concrete AST nodes that can be matched (hence the
    exclusion).

    This returns a tuple: first element is a list of the same size as
    `astnodes` and contains the corresponding sets of concrete AST nodes. The
    second element is the set of unmatched concrete AST nodes.
    """
    remaining_nodes = set(input_type.concrete_subclasses)
    result = []

    for node in astnodes:
        candidates = set(node.concrete_subclasses)
        result.append(candidates & remaining_nodes)
        remaining_nodes -= result[-1]

    return (result, remaining_nodes)


class Uninitialized:
    """
    Dummy class whose instances mean "this variable/field is not initialized".
    """
    pass
