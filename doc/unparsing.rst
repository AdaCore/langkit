Unparsing
=========

Adding the ``@with_unparsers`` annotation to a grammar declaration in Lkt
enables the automatic generation of unparsers for the corresponding language.
Unparsers allow generated libraries to turn parse trees back to parsable text,
which in turn makes several features available:

* Using the ``Langkit_Support.Generic_API.Unparsing`` Ada API to pretty-print
  source code.
* Using the ``Langkit_Support.Generic_API.Rewriting`` Ada API to modify parse
  trees and get back parsable text that reflects the changes.

An unparser is conceptually just a function which takes a node and returns the
corresponding parsable text. In order for Langkit to be able to derive these
functions automatically, it makes some assumptions about the lexer (not
actually checked by Langkit) and enforces restrictions on the grammar.


Assumptions
-----------

The unparsing machinery in Langkit makes the following assumption, which is
true for most "usual" programming languages: arbitrary numbers of ASCII spaces
(``0x20``) and newlines (``0x0a``) and can be inserted between two adjacent
tokens without affecting how these tokens are analyzed. Depending on settings,
the pretty-printing engine (powered by the Prettier library) may also use
horizontal tabulations (ASCII ``0x09``) for indentation, and emit ``CR LF``
sequences for line breaks.

Another assumption is that when two tokens need to be separated by at least one
space (see :ref:`token_families_unparsing_spacing`), a sequence of either a
line break or a mix of line breaks and whitespaces would fit too.

Note that indentation-sensitive languages, like Python, are not supported
currently.


Lexer annotations
-----------------

In addition to the ``@with_unparsers`` annotation, language specifications must
also include annotations at the lexer level to specify how sequences of tokens
must be unparsed.


``@with_unparsing_newline``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tokens that span until the end of the line must have the
``@with_unparsing_newline`` annotation. The classical example is comment
trivias:

.. code-block:: text

   @with_unparsing_newline
   @trivia()
   Comment <- p"#(.?)+"

Thanks to this, unparsing the following token sequence:

.. code-block:: text

   Comment("# Very important variable next")
   Identifier("a")
   Colon(":")
   Identifier("Int")

Will yield the expected line break between the two:

.. code-block:: text

   # Very important variable next
   a:Int

Instead of the erroneous:

.. code-block:: text

   # Very important variable nexta:Int


.. _token_families_unparsing_spacing:


Token families and ``@unparsing_spacing``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The unparsing engine needs to know when two tokens must be separated by at
least one whitespace (spacing required). This is typically necessary between a
keyword and an identifier:

.. code-block:: text

   # Sequence of tokens to unparse
   ForKeyword("for")
   Identifier("i")
   InKeyword("in")
   Identifier("my_vector")
   Colon(":")

   # Without spacing information: incorrect
   foriinmy_vector:

   # With spacing required between keywords and identifiers
   for i in my_vector:

To specify spacing requirements, lexers must first declare tokens in token
families:

.. code-block:: text

   family alphanumericals {
       ForKeyword <- "for"
       InKeyword <- "in"
       Identifier <- p"[a-zA-Z_][a-zA-Z_0-9]*"
   }

   family punctuation {
       Colon <- ":"
   }

Then, the ``@unparsing_spacing`` annotation specifies that there must be at
least one space between any two tokens that belong to the ``alpharumericals``
family:

.. code-block:: text

   @unparsing_spacing(with=alphanumericals)
   family alphanumericals {


``@comment``
~~~~~~~~~~~~

For the pretty-printing engine to correctly identify the comments in the set of
trivias to preserve during reformatting, comment trivias must be identified as
such. This is the job for the ``comment=`` argument for the ``@trivia()``
annotation:

.. code-block:: text

   @with_unparsing_newline
   @trivia(comment=true)
   Comment <- p"#(.?)+"


Grammar restrictions
--------------------

The main invariant to be maintained is that there must be exactly one possible
sequence of tokens that can create a given parse tree. For instance, the two
following grammar rules cannot coexist:

.. code-block:: text

   assign_1 <- AssignStmt("set" id "=" expr)
   assign_2 <- AssignStmt(id ":=" expr)

Because that would mean that there are two different syntaxes for the
``AssignStmt`` node, and so it would not be possible to have a single unparsing
function for that node. For the same reason, the following rule will be
rejected:

.. code-block:: text

   assign <- AssignStmt("set" id list+("=") expr)

Langkit still allows tokens that surround a given node field to be present or
not depending on whether the field is present. For instance:

.. code-block:: text

   var_decl <- VarDecl("var" id ?("=" expr))

Here, Langkit will know that the unparser for the ``VarDecl`` node must unparse
the ``=`` token if and only if the node's second field is present. For this to
be valid, Langkit will also check that the ``expr`` grammar rule never returns
a null node in a unit free of parsing errors.

Tokens must always "belong" to a specific node or to a specific field. So for
instance, the following parsing rules are rejected:

.. code-block:: text

   # The "var" keyword must belong to the AssignStmt node
   # Invalid:
   var_decl <- pick("var" AssignStmt(id "=" expr))
   # Valid:
   var_decl <- AssignStmt("var" id "=" expr)

   # The ";" token must be produced inside the "stmt" parsing rule; the list
   # rule cannot hold all of them.
   # Invalid:
   stmt <- Stmt(...)
   decls <- list*(stmt ";")
   # Valid
   stmt <- Stmt(... ";")
   decls <- list*(stmt)

   # The "return" token must be produced inside the "ReturnStmt" parser: the
   # field in the "Stmt" node cannot conditionally hold it.
   # Invalid:
   stmt <- Stmt(
       or(
           | pick("return" Return(expr))
           | Call(call_expr)
       )
   )
   # Valid
   stmt <- Stmt(
       or(
           | Return("return" expr)
           | Call(call_expr)
       )
   )

Each token node must be parsed by a simple token rule, and must be associated
with exactly one kind of token, the following rules are invalid:

.. code-block:: text

   class RefId: RootNode implements TokenNode {
   }

   # RefId can be associated either with Identifier tokens, or with Null
   # tokens, but cannot be associated with both.
   ref_id <- RefId(@Identifier)
   ref_id <- RefId(@Null)

   ref_id <- RefId(or(@Identifier, @Null))
