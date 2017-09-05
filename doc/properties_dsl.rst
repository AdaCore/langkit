**************
Properties DSL
**************

In language specifications, the definition AST nodes is itself composed of
several layers.  Each AST node is defined as a Python subclass of ``ASTNode``.
These subclasses contain various members:

* Syntax fields (``... = Field(...)``) which store other AST nodes; these are
  created at parsing time.

* Environment specifications (``env = EnvSpec(...)``) which define how to
  construct lexical environments. This describes what is done when one calls
  ``Populate_Lexical_Env``: the tree is visited and the env specs of all nodes
  are executed.

* Properties (``... = Property(...)`` or ``@langkit_property``). These are like
  C++ methods, or Ada tagged type primitives, in a dedicated programming
  language: the properties DSL.  This DSL provides features to build and solve
  logic equations: create logic variables tied to nodes, add possible values,
  add constraints on variables, etc.

This DSL is useful to provide high-level services, such as fetching the
definition corresponding to an identifier in source files.  In order to do this
language specifications first define how lexical environments are built, and
then use the properties DSL to query them, build equations and then solve
equations.

Note that the properties DSL is mostly functional. This fact gives us some
invariants on which to rely in order to handle memoization of results/data
invalidation, and so on.


Declaring properties
====================

There are two equivalent syntaxes to create properties. One is to use the
attribute syntax with the ``langkit.expressions.Property`` constructor::

    class MyNode(RootNode):
        my_prop = Property(Self.parent)

The other one is to use the method syntax with the
``langkit.expressions.langkit_property`` decorator::

    class MyNode(RootNode):
        @langkit_property()
        def my_prop():
            return Self.parent

In both cases, ``Self.parent`` is the body of the property: this what is
evaluated when the property is called and what gives the result of the
property. See the reference below to learn about what constructs are available
to write properties bodies.

Both ``Property`` and ``langkit_property`` accept various keyword arguments to
refine the property definition. For instance, the ``public`` boolean argument
(false by default) makes it possible to make the property available to the
users of the generated library.

Reguarding user documentation, you can either pass a ``doc`` string argument to
``Property`` or provide a docstring to the function under ``langkit_property``:
it will be passed to code generation.


DSL fundamentals
================

.. todo::

   Fill this section: talk about types, entities, attributes accesses (with and
   without arguments).


Dynamic variables
=================

.. todo::

   Fill this section.


Memoization
===========

.. todo::

   Fill this section.


Abstract properties
===================

.. todo::

   Fill this section.


External properties
===================

.. todo::

   Fill this section.


Equations
=========

.. todo::

   Fill this section.


Attribute expressions reference
===============================

.. auto-properties-dsl:: attr

Expression constructors reference
=================================

.. auto-properties-dsl:: cls
