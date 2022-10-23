Design document: Incremental reparse in Langkit
===============================================

Why
---

Here I will explain why it is a desirable feature, as well as why it
might not be worth the effort. In my opinion only implementing it would
allow us to know, so it's a risky endeavor.

Note that those perks and their limits are shared by all implementations
of incremental parsers, not only by the proposed implementation on
Packrat parsers.

Better performance
~~~~~~~~~~~~~~~~~~

We can expect better performance on reparses in interactive contexts.
This is important in an IDE.

Limits
^^^^^^

We still have linear complexity, because we need to update every node
and token after the edit. The hope is that the constant factor is worth
it, and it seems to be substantiated by some research on the subject.

BUT you will never have the comfort of knowing that a reparse's
complexity is linear on length of the edit.

Additional error recovery
~~~~~~~~~~~~~~~~~~~~~~~~~

In a lot of cases, we can keep a lot of the original tree in case of
syntax error, just getting rid of overlapping nodes in case of an edit
that deletes content. In cases of insertions, nothing needs to be done
in case of syntax error (I know you can do better than that, but that's
still a good start)

This is clearly an appealing and elegant approach in IDEs where:

1. The code changes all the time.
2. Most used operation is insertion.

Limits
^^^^^^

You still need regular error recovery features. It is not a replacement
for regular error recovery. For example, in the following case:

.. code-block:: ada

    package A is
       package B is
          |  -- Cursor here
    end A;

You want block indentation to know that there has been an opened block
that is not yet closed. Incremental parsing will just give you a syntax
error, and indent at the level of the A package, which is not what is
desired.

API: Users can rely on nodes persisting
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This allows user to design features that keep track of nodes or/and
attach custom data to them. This gives ONE notion of identity to rely
on.

This can be potentially useful to develop some features (example:
spec/body synchronization)

Limits
^^^^^^

-  You encourage memory leaks: When the users have no way to keep a
   persistence reference to a node, they won't. This currently allows
   Langkit to free mostly any part of its memory at any time, knowing
   that it can recompute results when needed (True for syntactic and
   semantic analysis).

This makes it so we have a clear path to how to solve "soft OOM"
problems in Langkit via a GC mechanism:

1. Free lexical envs/properties caches
2. Free lexical envs completely
3. Free whole trees

If people start relying on persistent references to nodes, we lose part
of that freedom.

-  You push a potentially wrong notion of node identity: In the
   following example

.. code-block:: ada

    package A is
        procedure Foo (A : Integer);
    end A;

    package body A is
        procedure Foo (A : Integer) is
        begin
        end Foo;

        procedure Bar (A : Integer) is
        begin
        end Bar;
    end A;

If the user renames ``Foo`` to ``Bar`` in the spec (line 2), it
fundamentally changes the semantic identity of the node: It is now a
different subprogram, with a different body. If you kept a reference to
this node, expecting it for example to keep the same body or spec, you
might be wrong.

Other example: if the user moves Foo after Bar in the body, by cutting
and pasting, then a persistent reference to the node will be null.

-  You potentially make everything slower: Suddenly we need nodes to be
   some kind of weak references, because since they *can* persist, you
   don't want a crash in the case the node did not actually persist, so
   you cannot free the node without warning the user about it. You have
   several solutions to that:

   1. Never free a node. This is bad and probably unaffordable for
      memory reasons in a persistent application.
   2. Invalidate every reference to the node. This requires keeping, in
      the node, a back reference to every alive reference, so that we
      can notify when it is freed. This is extremely costly both in
      terms of memory and CPU, and would strongly point to at least not
      make persistent references the default kind of node the user
      manipulates.
   3. ??? There might be some more efficient solution but I cannot find
      it.

How
---

`This
paper <https://ohmlang.github.io/pubs/sle2017/incremental-packrat-parsing.pdf>`__
describes how to make a packrat parser incremental.

It would seem like a proper basis for our incrementalization of
Langkit's parsers. The algorithms are simple and easy to understand.

Why not replace with an LALR parser ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Because we already have the packrat engine, and a pretty clear plan
   on how to make it incremental. Reimplementing a parsing engine from
   scratch is much more effort.
-  Because the approach doesn't seem better, and would require full
   reimplementation of the engine.
-  Because LALR error reporting is reportedly hard/bad. Even though
   Libadalang/Langkit is not currently great at syntax error reporting,
   we know how to make it better.

Why replace with an LALR parser ?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-  Because the incremental approach might be better hardened.
-  Because they might be generally faster and more memory efficient than
   packrat parsers.

Challenges
----------

Langkit doesn't keep full memo tables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

And in order to implement this algorithm, we probably would need to. But
keeping full memo tables for all files in an analysis context seems like
a bad idea memory wise. An idea would be to switch to full memo tables
when needed. When ?

.. attention:: Since we will have a variety of clients, and some might not be
    able to signal when they'll edit a file, not relying on the third idea
    below seems like a good idea.

    Generally, the whole problem seems like something that should use a fixed
    size cache (size between 5 and 20 seems about right given the number of
    files people generally edit) and a general caching policy such as
    LIFO/LRU/LFU.

1. First idea: when editing. Problem: It's too late, you need to do a
   full reparse to get back the memo tables, and one of the goals of
   incremental parsing is to avoid full reparses on interactive
   operations.
2. Second idea: When first editing, but instead of reparse, deduce the
   tables from the tree. This should be possible if we have a
   ``one rule <-> one node`` correspondence, which we should already
   have because of pretty printers.
3. Third idea: In the background when the user stays on a file. Combined
   with 2. (deduce tables from the tree) it should make it possible to
   not use too much memory except on the currently edited file.


The paper does not completely solve "node persistence"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the paper, when you make an edit, you will invalidate all affected
nodes in the packrat tables. The problem is that this does not only
include directly affected nodes - nodes that are found overlapping with
the edit - but also any parent of those, for obvious reasons.

Ideally, we would like to keep those nodes, but only modify their
contents.

Idea: put parent nodes on trial
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here is an algorithm I thought about to allow that:

1. When applying the edit, all overlapping nodes are invalidated.
2. Then, subsequent nodes in the tables, as well as their slocs
   (tokens), need to be updated.
3. All parent nodes are then "put on trial": They are not removed from
   the memo tables, but specially marked. When the parser for the rule
   enters parsing for the item at the specified memo table entry, he
   will reparse the item, but not reallocate it, allowing to keep it
   persistent in memory.

After applying this algorithm, supposedly running the parser again
should be sufficient.

Relocating tokens and memo tables entries
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Memo table entries
^^^^^^^^^^^^^^^^^^

I don't see an other solution than to iterate every memo table after the
last token after the edit, and relocate the nodes in them to the proper
index. This is an ``O(NM)`` operation, where N is the size of the tables
and M the number of grammar rules (so M can be considered a constant
factor).

    Idea: If we store packrat tables as a vector of arrays, where arrays
    are indexed by grammar rules, we can make this ``O(N)``

In my opinion, the cost of that operation can, and should, be evaluated
before we go any further.

Tokens
''''''

Token stream should be iterated and tokens locations properly
re-offseted. Since sloc ranges for nodes are computed via tokens, that
should be enough.

Making tokenizer incremental
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

That should not be too difficult, in Ada at least. Making the lexer
engine generally incremental might be harder.
