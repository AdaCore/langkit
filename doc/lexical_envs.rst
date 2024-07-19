********************
Lexical environments
********************

Basics
======

Lexical environments make it possible to create maps from symbols to nodes:
they are the most crucial data structure used to implement semantic analysis
with Langkit. There are various ways to link lexical environments together,
described below, so that a symbol lookup through a single lexical environment
can actually perform a search through others envs.

For instance, in order to implement semantic analysis for a C-like language:

.. code-block:: c

   { /* Scope 1 */
       int a;
       int b;

       { /* Scope 2 */
          int c;
       }
   }

Which produces a parse tree such as:

.. code-block:: text

   <Scope> /* Scope 1 */
     <VarDecl>
       <Id "int">
       <Id "a">
     <VarDecl>
       <Id "int">
       <Id "b">
     <Scope> /* Scope 2 */
       <VarDecl>
         <Id "int">
         <Id "c">

Creating one lexical environment per scope allows symbol lookups that fit the C
semantics will yield the following topology:

.. code-block:: text

   Lexical env for Scope 1 (no parent):
     a: <int a>
     b: <int b>

   Lexical env for Scope 2 (parent is lexical env for Scope 1):
     c: <int c>

From there, looking for symbol ``a`` in Scope 2 will return the ``int a``
declaration, thanks to the recursive lookup from Scope 2 to Scope 1, following
the former's parent link.


Env specs
=========

An env spec is a sequence of actions ("env actions") whose goal is to create
the mesh of lexical environments that is the most appropriate to model the
analyzed language's scoping rules, so that properties can easily query them to
implement semantic analysis.

Note that unlike the properties DSL, which uses a pure functional paradigm
(there are exceptions, but by default properties have no side effect), env spec
are essentially imperative: actions are executed in sequence, they modify the
state of the nodes that are traversed, and thus change the behavior of future
property calls.

Each env action performs an elementary action. For instance:

* Set the "current" lexical environment (more about what this means in the next
  section).

* Create a lexical environment.

* Add a symbol/node association in the current environment.


Populate Lexical Env (PLE)
==========================

At runtime, env actions are executed during a pass called "Populate Lexical
Env" (PLE): env actions are executed for all nodes in one analysis unit in a
depth-first-search fashion.

For each node, the PLE pass selects the list of env actions to execute
depending on the node type. Also, env actions execution operates on a "current
environment", which env actions can mutate or replace. Given one node, PLE runs
through the following steps:

* Inherit the "current environment" from the PLE run on the parent node.
* Set the node's "node_env/children_env" to this current env (more about these
  properties in the next section).
* Execute the "pre" env actions.
* If the current env has changed, update the node's "node_env/children_env".
* Run PLE on the node children.
* execute the "post" env actions.

Regarding env actions selection: by default env specs are inherited. For
instance, if node A has an env spec, node B derives from A and does not have an
env spec, A's env spec applies to B. And if node C also derives from A but
defines an env spec, A's env spec does not apply to C.

Users can manually run PLE, but if they don't, it is automatically executed
when needed, and is executed at most once per analysis unit (until the unit is
reparsed).


Nodes create lexical environments
=================================

Lexical environments are created during the PLE pass: each node can create up
to one environment. As a consequence, nodes have either one or two associated
environments, available through two properties:

* ``node_env`` always return the environment in which the node lives, i.e. its
  scope.

  For instance in a C-like language, we can consider that we create two nodes
  for the following chunk of code ``{ int a; }``: (1) the node for the
  brackets, and (2) the declaration note for ``int a``.

  The ``node_env`` of (1) is whatever env one of its parent nodes has created.
  (2) lives in the scope of (1), so ``node_env`` for (1) would be the
  environment that (2) has created.

* ``children_env`` returns the same as ``node_env`` if the node does not create
  an environment, or the environment it has created otherwise.

  To continue on the previous example: the ``children_env`` of (1) is the
  environment this node has created (this is (2)'s ``node_env`` However the
  ``children_env`` of (2) is (1)'s ``children_env`` as (2) does not create
  environments, ``node_env`` and ``children_env`` are the same for it.


Basic example
=============

The following concrete example will illustrate the above explanations. Let's
write env specs and semantic analysis properties for a very simple programming
language, where one can define functions that take arguments and return a
computation from these arguments (a mix of additions and function calls from
the arguments).

.. code-block:: python

    class FooNode(ASTNode):
        pass

    @abstract
    class Expr(FooNode):
        """Expression to compute a number."""
        pass

    class RefId(Expr):
        """Identifier that refers to a definition."""
        token_node = True

        @langkit_property()
        def referenced_decl():
            # Starting from the environment associated to this RefId node (must
            # be the children_env for the parent FuncDecl), look for the first
            # node registered under the same symbol as Self.
            return Self.node_env.get_first(Self.symbol)

    class FuncCall(Expr):
        """Call to a function.

        Example: foo(bar, baz)

        "foo" is the callee, "bar" and "baz" are the arguments.
        """
        callee = Field(type=T.RefId)
        args = Field(type=T.Expr.list)

    class Addition(Expr):
        """Addition of two numbers.

        Example: a + b.

        "a" is left, "b" is right.
        """
        left = Field(type=T.Expr)
        right = Field(type=T.Expr)

    class ArgDecl(FooNode):
        """Function argument declaration."""
        token_node = True

        env_spec = EnvSpec(
            # In the current environment, register the symbol/node association
            # for the symbol corresponding to Self and associate it to Self.
            add_to_env_kv(key=Self.symbol, val=Self),
        )

    class FuncDeclName(FooNode):
        """Name in a function declaration."""
        token_node = True

    class FuncDecl(FooNode):
        """Function declaration.

        Functions take numbers as arguments and return a number computed from
        the arguments.

        Example:
            foo(a, b, c) = (a + bar(b, c))

        "foo" is the name of the function, "a", "b" and "c" are its arguments,
        and "a + bar(b, c)" is its expression.
        """
        name = Field(type=T.FuncDeclName)
        args = Field(type=T.ArgDecl.list)
        expr = Field(type=T.Expr)

        env_spec = EnvSpec(
            # In the environment coming from the parent of this function (i.e.
            # the scope in which this function is defined), add a symbol/node
            # entry corresponding to this function declaration.
            #
            # When resolving a function call (using the RefId.referenced_decl
            # property on a FuncCall.callee field), this entry will be
            # returned.
            add_to_env_kv(key=Self.name.symbol, val=Self),

            # Add a new environment so that argument declarations and the
            # function expression live in a dedicated scope. This allows
            # argument declarations for one function not to be visible from
            # another function.
            #
            # What was the current environment when starting PLE on this node
            # (E1) becomes the parent of this new environment (E2): this is
            # what allows a symbol search through E2 (when resolving an
            # identifier in the expression) to automatically explore E1:
            # semantic resolution in the expression function has visibility on
            # both arguments in E2 and function declarations in E1.
            add_env(),
        )


Env actions
===========

This section provides a high level overview of the env actions available.
Please refer the corresponding function docstrings below for detailed
information about each action.

``add_env``
    Create a new lexical environment. There can be only one such action per env
    spec.

``reference``
    Add to a lexical environment E references to other envs. This is a way to
    extend the set of environment covered by a lookup on E: the lookup will
    process E, and then will process the envs referenced by E. Note that when
    adding multiple references, the lookups done in the resolver of each
    reference will only be allowed to go through env references added *before*
    itself in the env. Hence the order in which env references are added
    matters.

``add_to_env``/``add_to_env_kv``
    Add symbol/node associations to a lexical environment.

``handle_children``
    Allow to run env actions after the PLE on children has completed. Put this
    env action in the middle of the env spec: the actions before it are run as
    "pre" actions, and the actions after it are run as "post" actions. There
    can be at most one ``handle_children`` action in a given env spec.

``set_initial_env``/``set_initial_env_by_name``
    Set the current environment. Except for ``do`` hooks, this action must come
    first in the env spec.

``do``
    Evaluate a property DSL expression. This is useful only for properties
    which have side effects.

    .. TODO::

        Explain somewhere both how this can be useful, and the limitations that
        come with properties with side effects (unsafe territory).


Unit boundaries and named environments
======================================

This env spec frameworks forbid lexical environments that cross the boundary of
analysis units:

* ``add_to_env``/``add_to_env_kv`` are not allowed to insert a foreign node
  (i.e. a node that comes from another analysis unit that the one being PLE'd).

* They are also not allowed to insert nodes to foreign environments (envs
  created during the PLE of another analysis unit).

* ``reference`` is not allowed to use foreign nodes to create the reference to
  other envs.

* ``set_initial_env`` is not allowed to set a foreign env as the current one.

If one of these rules is violated, a ``Property_Error`` is raised to abort PLE.

Env specs must use another mechanism to create such cross-unit links: named
environments. When creating an environment, one can pass a list of names
(symbols) to ``add_env`` to associate this new environment to each name. It is
then possible to refer to this environment from the PLE of other units thanks
so these names:

* If the ``AddToEnvDestEnv`` struct passed to ``add_to_env``/``add_to_env_kv``
  contains an env name, that name is used to fetch the corresponding named
  lexical env.

* Likewise for ``set_initial_env_by_name``.

Using names instead of direct env/node values across unit boundaries is
necessary for safety and idempotence: the result of the PLE pass on a given
analysis unit must be the same regardless of the other analysis units loaded
and regardless of the future reparsing of other analysis units.
