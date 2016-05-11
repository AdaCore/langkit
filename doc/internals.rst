*********
Internals
*********

This section describes various Langkit implementation details. It is intended
for Langkit developpers.

Memory management in properties
===============================

General principles
------------------

Compiled types in Langkit can be either automatically allocated ("by-value"
types such as booleans, integers or structures) or dynamically allocated (AST
nodes, arrays, lexical environments, etc.). For dynamically allocated ones, we
need to define when the values must be freed.

On one hand, AST nodes and the lexical environments created in the
``Populate_Lexical_Env`` pass are owned by their analysis unit, and thus are
deallocated at the same time as their owning unit. So from the point of view of
properties, they are actually statically allocated: they live before the
property evaluation and still live afterwards.

Arrays and all other lexical environments, on the other hand, are a completely
different matter. Properties can create them and sometimes they are "dead" (not
needed anymore) before a property evaluation completes. In order to trigger the
deallocation of these at the proper moment, Langkit uses a reference counting
mechanism. Let's call "ref-counted values" all the values that integrate with
this mechanism.

Ref-counted values can have any number of "owners". When a ref-counted values
is created, its number of owners (aka "ref-count") is 1: the creator of the
value is the only one that owns it. There are multiple operations available to
work with these values: some will create more owners (incrementing the
ref-count: inc-ref), some will remove ownership (decrementing the ref-count:
dec-ref). When the ref-count drops to 0, the value has no owner anymore,
meaning that no-one can access this value anymore, so the value can be
deallocated.

This mechanism guarantees that properties does not leak memory if one condition
is met: there must not be any ownership loop in values. This is not possible in
properties as values are immutable.

Reference counting interface
----------------------------

We distinguish two classes of values for all types ``T`` that interact with the
reference counting mechanism:

1. Non-null values, which have a regular ref-count.

2. The null value, which represents the absence of allocated object for ``T``.
   It is special since it is unique and it has no ref-count as there is no
   associated object.

For all such ``T`` types, implementation must provide two primitives:

* ``procedure Inc_Ref (Self : T);``

  This just increases the ref-count for ``Self``. Note that this primitive can
  assume that ``Self`` is a non-null value.

* ``procedure Dec_Ref (Self : in out T);``

  If ``Self`` is the null value, this primitive is a no-op. Otherwise, it
  decreases the ref-count for ``Self`` and sets it to the null value. If its
  ref-count reached zero, it is deallocated. Note that in the latter case, if
  ``Self`` owns other ref-counted values, dec-ref must occur for them as well.

Ownership rules
---------------

At this point, the only missing bit to specify how ref-counting works is: to
what rules does ownership obey?

First, passing arguments to properties does not create new ownership. In other
words, if property A is called with a ref-counted value B, then A needs to
inc-ref B in order to get an ownership on it. In this case we say that A
"borrows" B: it has a reference to it but it does not own it. Second, returning
values in properties does create ownership: when property A returns, the caller
has an ownership on its result.

Properties operations (like ``CollectionGet.Expr``) all define whether their
result is borrowed or a new ownership: one has to read the definition of the
corresponding ``ResolvedExpression`` in the source code to find out.  However
in the case an operation creates a new ownership, it has to store its result in
a temporary variable. Langkit will automatically dec-ref this variable when it
goes out of scope (returning or raising a ``Property_Error``).  Another way to
put it is: operations that create an ownership for their result must give this
ownership to the property context. This context will know when to dec-ref this
value thanks to the *scope mechanism*.

Scopes
------

Property local variables are organized in a traditional scope hierarchy. The
top-level scope is the one that lives as long as the property is evaluated.
Children scopes are the one introduced to materialize different lifetimes, for
instance loop scopes, which go out of scope after each iteration.

All local variables must belong to exactly one scope. A block def-refs all
variables it contains when the execution flow leaves it.
