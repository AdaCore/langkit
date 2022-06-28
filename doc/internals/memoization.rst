Properties memoization
======================


Property memoization is a mechanism to improve the run time performance of
properties: a memoized property caches its result for a given set of arguments
to avoid later recomputations. By default, no property is memoized, and the
language spec can request the memoization of each property explicitly.


Implementation
--------------

When called, a memoized property will look up in the memoization table if it
was already called with the same arguments: if it was, the previous result
(saved in the memoization table: a regular result or a ``PropertyError``
exception) is returned directly, and the property code is not evaluated.

Property memoization is implemented inside the body of a property. This means
that it is not possible to memoize external properties (its implementation is
outside the reach of code generation), and that memoizing one property does not
automatically memoize all the properties that override it. As a consequence, it
is not possible to memoize an abstract property (which has no implementation).

The goal of property memoization is only to enhance run time performance: it is
not supposed to change the result of any property (it would not be sound).

The memoization table is implemented with a hash table. For this reason, it is
not possible to memoize a property if at least one of its arguments is not
hashable (see ``CompiledType.hashable``).


Soundness checks
----------------

In order to automatically reject as many memoized properties that could break
the soundness principle, the memoization of a property P that creates side
effects is forbidden, and memoization of properties that can have P in their
call trees. Two kinds of side effects are automatically detected:

* When a property calls a logic equation's "solve" property: solves mutates
  logic variables, so it is unsafe for a memoized property to access it.
  Without this protection, the following could happen:

  .. code-block:: python

     @langkit_property(memoized=True)
     def p1():
         return Self.equation.solve

     @langkit_property()
     def p2():
         return If(
             Self.p1,
             Self.logic_var.get_value,
             No(FooNode)
         )

  The first time ``p2`` is called, it calls ``p1``, which is evaluated; it
  returns True, which is saved for later calls, and most importantly, ``.solve``
  mutates ``Self.logic_var`` to contain the result of the equation resolution.

  The second time ``p2` is called, ``p1`` is not evaluated, thanks to a
  memoization hit, so ``.solve`` is not called, and thus ``Self.logic_var`` is
  left with whatever value it contains. So ``p2`` may return an obsolete value
  in this case.

* When a property tries to access the value of a logic variable. A variation of
  the example above shows why:

  .. code-block:: python

     @langkit_property(memoized=True)
     def p1():
         return Self.logic_var.get_value

     @langkit_property()
     def p2():
         v1 = Var(If(
             Self.equation_1.solve,
             Self.p1,
             No(FooNode)
         ))
         v2 = Var(If(
             Self.equation_1.solve,
             Self.p1,
             No(FooNode)
         ))
         return MyStruct.new(v1=v1, v2=v2)

  The first time ``p1`` is called, it evaluates ``get_value`` and returns the
  value that was assigned to the logic var during the resolution of
  ``equation_1``. However the second time it is called, it returns the same
  value instead of getting the value assigned to ``logic_var`` during the
  resolution of ``equation_2``.

External properties can do all sorts of side effects on their own and Langkit
cannot automatically get to know about them. In order to benefit from the
memoization soundness protection system, the declaration of external properties
can indicate that they are unsafe to memoize (see ``langkit_property``'s
``call_non_memoizable_because`` argument).

While this protection mechanism is useful to prevent the accidental
introduction of memoization bugs, it can be also too strict is some cases.
Consider for instance the following example:

.. code-block:: python

    @langkit_property(memoized=True)
    def p():
        return If(
            Self.equtaion.solve,
            Self.logic_var.get_value,
            No(FooNode)
        )

Memoization is safe in this case, as all calls to ``.get_value`` are
necessarily preceeded by corresponding calls to ``.solve``, i.e. it is not
possible for ``.get_value`` to access stale data. In order to force Langkit to
allow the memoization of this property, use ``langkit_property``'s
``call_memoizable`` argument.


Interaction with PLE
--------------------

Since the very purpose of the Populate Lexical Env pass is to create side
effects (each step can modify lexical environment), trying to cache property
return values during this pass is unsound. For this reason, property
memoization is disabled during PLE.

For the rare cases where memoization is both necessary for reasonable run time
performance and is known not to trigger caching bugs (due to PLE side effects),
it is possible to force memoization during PLE: use ``langkit_property``'s
``memoize_in_populate`` argument.
