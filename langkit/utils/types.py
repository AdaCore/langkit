from __future__ import absolute_import, division, print_function

import inspect


def type_check_instance(klass):
    """
    Return a predicate that will return true if its parameter is a subclass
    of `klass`.
    :param type klass: Class to check against.
    :rtype: (T) -> bool
    """
    return lambda t: isinstance(t, klass)


def dispatch_on_type(typ_or_inst, type_to_action_assocs, exception=None):
    """
    Dispatch on `typ_or_inst`, execute the corresponding action depending on
    the type. Every "type" (MatcherType below) in type_to_action_assocs will be
    tested in turn; the correpsonding action will be executed if the type:

      * is `typ_or_inst`;
      * is a class and `typ_or_inst` is an instance of it;
      * is a class and `typ_or_inst` is a subclass of it.

    :param InputType typ_or_inst: The type/instance to dispatch upon.

    :param type_to_action_assocs: An association of types to actions that
        returns something.
    :type type_to_action_assocs: list[(MatcherType, (InputType) -> RType)]

    :param Exception exception: The exception to raise in case the type is
        not in the dispatch table.

    :rtype: RType
    """
    exception = exception or Exception(
        "Error in dispatch_on_type: {} not handled".format(typ_or_inst)
    )
    for target_type, action in type_to_action_assocs:
        if (
            target_type is typ_or_inst
            or (inspect.isclass(target_type)
                and (issubclass(type(typ_or_inst), target_type)
                     or issubtype(typ_or_inst, target_type)))
        ):
            return action(typ_or_inst)
    raise exception


def assert_type(obj, typ):
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


class TypeSet(object):
    """
    This class is an helper for when you need to check wether an abstract
    operation was fulfilled for a whole hierarchy of classes, given that if
    the operation is fulfilled for all subclasses, then we consider that it is
    done for the parent class.

    This allows to automate the logic of, for example, checking if abstract
    properties are overridden properly, or if a match expression handles all
    cases.
    """

    def __init__(self):
        # Working set of ASTNode subclasses for the types that are covered by
        # matchers. Updated as we go through the list of matchers.
        self.matched_types = set()

    def include(self, t):
        """
        Include a class and all of its subclasses.

        If `t` is the last subclass for some base class, this adds the parent
        subclass. This makes sense as once all concrete subclasses of the
        abstract `A` type are handled, it is true that `A` is handled.

        Return whether `t` was already present in `self`.

        :param ASTNodeType|None t: AST node to include.
        :rtype: bool
        """
        if t in self.matched_types:
            return True

        # Include "t" and all its subclasses
        self.matched_types.add(t)
        for child in t.subclasses:
            self.include(child)

        # Include its parents if all their children are matched
        parents = list(t.get_inheritance_chain())[:-1]
        for parent in reversed(parents):
            if parent in self.matched_types:
                break
            subclasses = set(parent.subclasses)
            if not subclasses.issubset(self.matched_types):
                break
            # If we reach this point, all parent's subclasses are matched,
            # so we can state that parent itself is always matched.
            self.matched_types.add(parent)

        return False

    def unmatched_types(self, t):
        """
        Return the set of `t` subclasses that are not matched by any matcher.

        Omit subclasses when none of them are matched: only the parent is
        returned in this case, so that we don't flood users with whole
        hierarchies of classes.

        :type t: ASTNodeType
        :rtype: set[ASTNodeType]
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


def issubtype(type, parent_type):
    """
    Helper wrapper around issubclass, because issubclass will raise an
    exception if compared object is not a class, which is extremely tedious for
    us in langkit.

    :param type: The object or type to check against parent_type.
    :param type parent_type: The supposed parent_type of type.
    """
    return inspect.isclass(type) and issubclass(type, parent_type)


def not_implemented_error(self_or_cls, method):  # no-code-coverage
    """
    Return a NotImplementedError instance to explain that `self or_cls` must
    override method `method`.

    :param self or_cls: Class that does not implement `method`, or the instance
        of such a class.
    :param method: Method object for the method that should be overriden.
    :rtype: NotImplementedError
    """
    cls = self_or_cls if inspect.isclass(self_or_cls) else type(self_or_cls)
    return NotImplementedError('{} must override method {}'.format(
        cls.__name__, method.__name__
    ))


def astnode_kind_set(context, nodes):
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
    groups = [
        (context.kind_constant_to_node[f].ada_kind_name,
         context.kind_constant_to_node[l].ada_kind_name)
        for f, l in groups
    ]

    return ' | '.join(
        (f if f == l else '{} .. {}'.format(f, l))
        for f, l in groups
    )


def collapse_concrete_nodes(input_type, astnodes):
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

    :rtype: (list[set[ASTNodeType]], set[ASTNodeType])
    """
    remaining_nodes = set(input_type.concrete_subclasses)
    result = []

    for node in astnodes:
        candidates = set(node.concrete_subclasses)
        result.append(candidates & remaining_nodes)
        remaining_nodes -= result[-1]

    return (result, remaining_nodes)
