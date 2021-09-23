****************
LKT coding style
****************

This is the current - evolving - coding style for the LKT syntax. For the
moment this is just organized as a flat list of rules.

Indentation & braces
====================

Indentation is expected to be 4 spaces. The general rule is that everything
inside of braces should be indented, opening brace should be on the line of the
preceding logically linked token, and closing brace on its own line.

.. code-block::

    fun a(b: Int, c: Int): Int = {
        b + c
    }

Casing
======

We generally follow the Python PEP8 casing convention. In short:

* Type names should be capitalized, using CamelCase when there are several
  parts in the name.
* Variable names should be lowercase, using underscores when there are several
  parts.

Parens/brackets/colon
=====================

* No space expected before parens for calls and arrays accesses
* No space expected before brackets for generic declarations and
  instantiations.
* No space expected before colons for type annotations, but there should always
  be a space after the colon.

.. code-block::

    fun a(): Int = 12

    generic[T]
    fun bar(): T

    val c: Int = a()
    val d: Int = b[Int]()

    val e: (Int) -> Int

Maximum line length
===================

As in mostly everything at AdaCore, and everything in Langkit, 80 columns hard
limit on line length.

Generic declarations
====================

Generic declarations should have the ``generic`` keyword on its own line, along
with generic parameters.

.. code-block::

    generic[T, U]
    function map(in_array: Array[T], map_fn: (T) -> U): Array[U]

Annotations
===========

Some flexibility in terms of formatting is granted for annotations. When the
declaration is short, as in:

.. code-block::

    @parse_field a: B

The annotation can be put on the same line.

When it's needed for line length reasons to break the code, or when it's felt
like it helps readability, a line break can be inserted after annotations.

.. code-block::

    @export @memoized
    fun do_complicated_stuff(param1: Type1, param2: Type2, param3: Type3 = i)
