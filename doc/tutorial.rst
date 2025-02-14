********
Tutorial
********


Introduction
============

If you are completely new to Langkit, this tutorial is for you! It will run
through the implementation of an analysis library for a simple language and
will go further until actually using the generated library as a Python module
to implement an interpreter for this language. This should provide you a decent
background about how to deal with Langkit at every step of the pipeline.

Little disclaimer, though: this tutorial is intended for people with zero
experience with Langkit but a reasonable knowledge of how compilers work (what
a lexer is, what a parser is, what semantic analysis means, etc.). Being
comfortable with the Python programming language will be useful as well.

We will focus on a very simple language for the purpose of this tutorial:
Kaleidoscope, which is defined and used in a `LLVM tutorial
<http://llvm.org/docs/tutorial/index.html>`_.


Setup
=====

First, please make sure that the ``langkit`` Python package is available in
your Python environment (i.e. that Python scripts can import it). Also, please
install:

* a GNAT toolchain: the generated library uses the Ada programming language, so
  you need to be able to build Ada source code;

* `GNATcoll <http://docs.adacore.com/gnatcoll-docs/>`_, an Ada library
  providing various utilities;

* Mako, a template system for Python which should already be installed if you
  used ``setup.py/easy_install/pip/...`` to install Langkit.


Getting started
===============

Alright, so having to copy-paste files in order to start something is quite
boring: let's use a script that will do this for us! Move to a working
directory and run:

.. code-block:: text

    $ scripts/create-project.py Kaleidoscope

This will create a ``kaleidoscope`` directory, a dummy language specification
(lexer and parser) as well as a ``manage.py`` script that will help you to
generate and build your analysis library. Let's step into it:

.. code-block:: text

    $ cd kaleidoscope

And check that this skeleton already builds:

.. code-block:: text

    $ lkm make

This should generate and then build the analysis library in the ``build`` local
directory. Check in particular:

* ``build/src`` and ``build/lib``, which contain the Ada sources, C header
  files and static/shared libraries for the generated library;

* ``build/obj-mains``, which contains a ``libkaleidoscopelang_parse``
  binary, useful to easily run the lexer/parser from the command line;
  note that it is statically linked with the generated library to ease
  debugging and testing (you don't have to add ``build/lib`` directory
  to your ``LD_LIBRARY_PATH``);

* ``build/python``, which contains the Python binding for the generated
  library.

In order to be able to use the library directly in its build directory, you
need to update your environment. The following command does that:

.. code-block:: text

    $ eval $(lkm setenv)

.. note::

   For real life use, the generated library is supposed to be installed. If it
   is installed in a standard location (for instance ``/usr`` on Unix systems),
   this will make this environment update unnecessary.

If everything went fine so far, you should be able to run the
``libkaleidoscopelang_parse`` test binary:

.. code-block:: text

    $ libkaleidoscopelang_parse
    Parsing failed:
    <input>:1:1: Expected 'example', got Termination
    <null node>

Great! This binary just tries to parse its command-line argument and displays
the resulting parse tree. The dummy language specification describes a language
that allows exactly one "example" keyword:

.. code-block:: text

    $ libkaleidoscopelang_parse example
    ExampleNode[1:1-1:8]

Here, we have an ``ExampleNode`` which spans from line 1, column 1 to line 1,
column 8.  This language is pretty useless but now we checked that the setup
was working, let's implement Kaleidoscope!


Lexing
======

We are about to start with the most elementary piece of code that will handle
our language: the lexer!  Also known as a scanner, a lexer will take a stream
of text (i.e.  your source files) and split it into *tokens* (or *lexemes*),
which are kind of "words" for programming languages. Langkit hides the gory
details and lets you just write a concise description for the lexer in Lkt.
Fire up your favorite code editor and open ``kaleidoscope/tokens.lkt``.

This file contains a ``lexer`` block that defines the set of token kinds that
the lexer will produce and what to do with them, as well as lexing rules to
produce these patterns:

.. code-block:: text

   lexer kaleidoscope_lexer {
       Example <- "example"
   }

So let's first talk about token kinds. The tokens most lexers yield have a kind
that determines what kind of word they represent: is it an identifier? an
integer literal? a keyword? In addition, Langkit also creates tokens for chunks
of source code that are generally just discarded in compiler architectures,
like comments or whitespaces. Even though such tokens are of no use to compile
source code, they are useful for other kinds of language toolings, such as
reformatters; yet the parser must discard them. In Langkit, these special
tokens are called "trivias".

Here is a larger lexer that will be useful to implement Kaleidoscope:

.. code-block:: text

    lexer kaleidoscope_lexer {
        Example <- "example"

        # Trivias
        @trivia() Whitespace <- p"[ \\t\\r\\n]+"
        @trivia() Comment <- p"#.*"

        # Keywords
        Def <- "def"
        Extern <- "extern"

        # Other alphanumeric tokens
        Identifier <- p"[a-zA-Z][a-zA-Z0-9_]*"
        Number <- p"([0-9]+)|([0-9]+\\.[0-9]*)|([0-9]*\\.[0-9]+)"

        # Punctuation
        LPar <- "("
        RPar <- ")"
        Comma <- ","
        Colon <- ":"
        Semicolon <- ";"

        # Operators
        Plus <- "+"
        Minus <- "-"
        Mult <- "*"
        Div <- "/"
    }

Ok, so here we have three kind of tokens:

* Trivias (whitespaces and comments), annotated with ``@trivias()``, that the
  lexer will create and which the parser will ignore.

* Identifiers, which we'll use for function names and variable names.

* All other tokens (keywords such as ``def`` or ``extern``, decimal literals
  ``Number``, etc.).

Each token is associated with a lexing rule. Some make the lexer match an exact
string:

.. code-block:: text

   # The lexer will create a Def token when it finds exactly "def" in the
   # source code.
   Def <- "def"

Other rules make the lexer match a *pattern* (note the ``p`` prefix before the
string literal):

.. code-block:: text

   # The lexer will create an Identifier token when it finds one ASCII letter
   # (lowercase or uppercase) followed by zero or many letters, numbers or
   # underscores.
   Identifier <- p"[a-zA-Z][a-zA-Z0-9_]*"

This formalism is very analog to what you can find in other lexer generators
such as ``flex``: the association of an action (token to create) with a source
code matcher (literal string or regular expression pattern).

Note that the order of lexing rules matters: the source excerpt ``def`` matches
both the lexing rule for the ``Def`` token and the one for the ``Identifier``
token. However, since the ``Def`` rule appears before the one for
``Identifier``, ``Def`` has precedence over ``Identifier`` in case both match.
Thanks to this, the lexer considers that ``def`` is always a keyword, never an
identifier.

In both the token kinds definition and the rules specification above, we kept
handling of the ``Example`` token in order to keep the parser happy (it still
references it). You will be able to get rid of it once we take care of the
parser.

Alright, let's see how this affects our library. Before our work, only
``example`` was accepted as an input, everything else was rejected by the
lexer:

.. code-block:: text

    $ libkaleidoscopelang_parse def
    Parsing failed:
    <input>:1:1: Invalid token, ignored
    <input>:1:2: Invalid token, ignored
    <input>:1:3: Invalid token, ignored
    <input>:1:4: Expected 'example', got Termination
    <null node>

Now, you should get this:

.. code-block:: text

    $ lkm make
    $ libkaleidoscopelang_parse def
    Parsing failed:
    <input>:1:1: Expected 'example', got 'def'
    <null node>

The parser is still failing but that's not a surprise since we only took care
of the lexer so far. What is interesting is that we see thanks to the ``Def``
rule, the lexer correctly turned the ``def`` input text into a ``Def`` token.
Let's check with numbers:

.. code-block:: text

    $ ./build/obj-mains/libkaleidoscopelang_parse 0
    Parsing failed:
    <input>:1:1: Expected 'example', got Number
    <null node>

Looking good! Lexing seems to work, so let's get the parser working.


Nodes and parsing
=================

The job of parsers is to turn a stream of tokens into a parse tree (or syntax
tree), which is a representation of the source code making analysis easier. Our
next task will be to actually define how the parse tree looks like so that the
parser will know what to create.

Take your code editor, open ``kaleidoscope/nodes.lkt`` and replace the
``ExampleNode`` class definition with the following ones:

.. code-block:: text

    |" Function declaration.
    class Function: KaleidoscopeNode {
        @parse_field
        proto: Prototype

        @parse_field
        body: Expr
    }

    |" External function declaration.
    class ExternDecl: KaleidoscopeNode {
        @parse_field
        proto: Prototype
    }

    |" Function prototype: name and arguments.
    class Prototype: KaleidoscopeNode {
        @parse_field
        name: Identifier

        @parse_field
        args: ASTList[Identifier]
    }

    |" Top-level expression
    class TopLevelExpr: KaleidoscopeNode {
        @parse_field
        expr: Expr
    }

    |" Base class for expression nodes.
    @abstract
    class Expr: KaleidoscopeNode {
    }

    |" Integer literal.
    class Number: Expr implements TokenNode {
    }

    |" Identifier (used both as references and defining identifiers).
    class Identifier: Expr implements TokenNode {
    }

    |" Sub-expression wrapped in parens.
    class ParenExpr: Expr {
        @parse_field
        expr: Expr
    }

    |" Operator for a binary expression.
    enum class Operator: KaleidoscopeNode {
        case Plus, Minus, Mult, Div
    }

    |" Binary expression (left-hand side operand, operator and right-hand side
    |" operand).
    class BinaryExpr: Expr {
        @parse_field
        lhs: Expr

        @parse_field
        op: Operator

        @parse_field
        rhs: Expr
    }

    |" Function call expression.
    class CallExpr: Expr {
        @parse_field
        callee: Identifier

        @parse_field
        args: ASTList[Expr]
    }

Each class definition is a way to declare how a particular parse node will
look.  Think of it as a kind of structure: here the ``Function`` node has two
fields: ``proto`` (itself a ``Prototype`` node) and ``body`` (itself an
``Expr`` node).

Some nodes can have multiple forms: for instance, an expression can be a number
or a binary operation (addition, subtraction, etc.) and in each case we need to
store different information in them: in the former we just need the number
value whereas in binary operations we need both operands (``lhs`` and ``rhs``
in the ``BinaryExpr`` class definition above) and the kind of operation (``op``
above). The strategy that compiler writers sometimes adopt is to use
inheritance (as in `OOP
<https://en.wikipedia.org/wiki/Object-oriented_programming>`_) in order to
describe such nodes: there is an abstract ``Expr`` class while the ``Number``
and ``BinaryExpr`` are concrete classes deriving from it.

This is exactly the approach that Langkit uses: all nodes derive from the
``KaleidoscopeNode`` class (which is the root node type), and you can create
abstract classes (using the ``abstract`` annotation) to create a hierarchy of
node types.

Careful readers may also have spotted something else: the ``Operator``
enumeration node type. We use an enumeration node type in order to store in the
most simple way what kind of operation a ``BinaryExpr`` represents. Enumeration
nodes are declared in an ``enum class`` block, and contain no parsing field,
but declare sub-node types with the ``case C1, C2, ...`` syntax.

Some class declarations (``Number`` and ``Identifier``) also include the
``implements TokenNode`` syntax. This specifies that these nodes don't hold any
field but instead are used to materialize in the source a single token. When
compiling the grammar, Langkit will make sure that parsers creating these kind
of nodes do consume only one token.

Fine, we have our data structures so now we shall use them! In order to create
a parser, Langkit requires you to describe a grammar, hence the ``grammar
kaleidoscope_grammar`` block already present in ``parser.lkt``. Basically, the
only thing you have to do with a grammar is to add *parsing rules* to it: a
rule is a kind of sub-parser, in that it describes how to turn a stream of
token into a subtree.  Rules can reference each other recursively: an
expression can be a binary operator, but a binary operator is itself composed
of expressions! And in order to let the parser know how to start parsing you
have to specify an entry point rule: this is the ``@main_rule`` annotation in
the grammar (currently associated to the rule appropriately called
``'main_rule'``).

Langkit generates recursive descent parsers using `parser combinators
<https://en.wikipedia.org/wiki/Parser_combinator>`_ in a ``grammar`` block
declaration, similar to the ``lexer`` block definition for the lexer. Parsing
rules look like the following:

* ``@Identifier`` matches exactly one ``Identifier`` token.
* ``"def"`` matches exactly one ``def`` token; it is equivalent to ``@Def``;
* ``Def("def", @Identifier)`` matches a ``def`` token followed by an
  identifier token, creating a ``Def`` node for them.
* ``or("def" | "extern")`` matches either a ``def`` keyword, either a ``extern``
  one (no more, no less).

Let's move forward with a real world example: Kaleidoscope! Each chunk of code
below appears inside the ``grammar`` block for the kaleidoscope language:

.. code-block:: text

    @with_lexer(kaleidoscope_lexer)
    grammar kaleidoscope_grammar {
        # ... parsing rules ...
    }

Let's first redefine the ``main_rule`` parsing rule:

.. code-block:: text

    @main_rule main_rule <- list+(
        or(extern_decl | function | top_level_expr)
    )

``external_decl`` references the parsing rule called ``external_decl``.  It
does not exist yet, but Langkit allows such forward references anyway so that
rules can reference themselves in a recursive fashion.

``list+(...)`` expresses a list parser, which matches multiple times its
subparser (``...```). Like in regular expressions, ``+`` specifies that the
list parser requires at least one element, while ``list*(...)`` would allow the
list parser to match zero element.

So what this rule matches is a list in which elements can be either external
declarations, function definitions or expressions.

.. code-block:: text

    extern_decl <- ExternDecl("extern" prototype ";")

This one is interesting: inside the parens, we matches the ``extern`` keyword
followed by what the ``prototype`` rule matches, followed by a semicolon. Then,
thanks to the ``ExternDecl`` call, we take the content we matched and create an
``ExternDecl`` node to hold the result.

... but how is that possible? We saw above that ``ExternDecl`` has only one
field, whereas the call matched three items. The trick is that mere tokens are
discarded.  Once the ``Extern`` token is discarded, the only thing left is what
``prototype`` matched, and so there is exactly one result to put in
``ExternDecl``'s only field: ``proto``.

.. code-block:: text

    function <- Function("def" prototype expr ";")

We have here a pattern that is very similar to ``extern_decl``, except that the
node constructor has two non-discarded results: ``prototype`` and ``expr``.
This is fortunate, as the ``Function`` node requires two fields.

.. code-block:: text

    prototype <- Prototype(identifier "(" list*(identifier, ",") ")")

The only new bit in this rule is the ``list`` parser second argument: in the
``main_rule`` it had only one: a sub-parser to specify how to match individual
list elements. Here, we also have an argument to specify that a comma token
must be present between each list item. Having ``*`` instead of ``+`` also
tells the list parser that it is valid for the parsed list to be empty.

So our argument list has commas to separate arguments and we may have functions
that take no argument.

.. code-block:: text

    top_level_expr <- TopLevelExpr(expr ";")
    expr <- or(
        | ParenExpr("(" expr ")")
        | BinaryExpr(
            expr
            or(
                | Operator.Plus("+")
                | Operator.Minus("-")
            )
            prod_expr
        )
        | prod_expr
    )

Let's dive into the richest grammatical element of Kaleidoscope: expressions!
An expression can be either:

* A sub-expression nested in parenthesis, to give users more control over how
  associativity works.

* Two sub-expressions with an operator in the middle, building a binary
  expression. This shows how we can turn tokens into enumerators:

  .. code-block:: text

      Operator.Plus("+")

  This matches a ``+`` token (``Plus`` in our lexer definition) and yields the
  ``Plus`` node enumerator from the ``Operator`` enumeration node type.

* The ``prod_expr`` kind of expression: see below.

.. code-block:: text

    prod_expr <- or(
        | BinaryExpr(
            prod_expr
            or(
                | Operator.Mult("*")
                | Operator.Div("/")
            )
            call_or_single
        )
        | call_or_single
    )

This parsing rule is very similar to ``expr``: except for the parents
sub-rule, the difference lies in which operators are allowed there: ``expr``
allowed only arithmetic sums (plus and minus) whereas this one allows only
products (multiplication and division). ``expr`` references itself everywhere
except for the right-hand-side of binary operations and the "forward"
sub-parser: it references the ``prod_expr`` rule instead. On the other hand,
``prod_expr`` references itself everywhere with the same exceptions.  This
layering pattern is used to deal with associativity in the parser: going into
details of parsing methods is not the purpose of this tutorial but fortunately
there are many articles that explain `how this works
<https://www.google.fr/search?q=recursive+descent+parser+associativity>`_. Just
remember that: yes, Langkit handles left recursion.

.. code-block:: python

    call_or_single <- or(
        | CallExpr(identifier "(" list*(expr, ",") ")")
        | identifier
        | number
    )

Well, this time there is nothing new. Moving on to the two last rules...

.. code-block:: text

    identifier <- Identifier(@Identifier)
    number <- Number(@Number)

Until now, the parsing rules we wrote only used string literals to match
tokens, so parsing rule were written mentionning these literals directly
(``"("``, ``"def"``, ...), for readability. While this works for tokens such as
keywords, operators or punctuation, we cannot match a token kind with no
specific text associated this way, like identifiers and numbers: to achieve
this, these parsing rules use the ``@Identifier`` and ``@Number`` notation.

Our grammar is complete, for a very simple version of the Kaleidoscope
language! If you have dealt with Yacc-like grammars before, I'm sure you'll
find this quite concise, especially considering that it covers both parsing and
parse tree instantiation.

Let's now check with basic examples if the parser works as expected. First, we
have to launch another build and then run ``libkaleidoscopelang_parse`` on some
code:

.. code-block:: text

    $ lkm make
    $ libkaleidoscopelang_parse 'extern foo(a); def bar(a, b) a * foo(a + 1);'
    KaleidoscopeNodeList[1:1-1:45]
    |  ExternDecl[1:1-1:14]
    |  |proto:
    |  |  Prototype[1:8-1:14]
    |  |  |name:
    |  |  |  Identifier[1:8-1:11]: foo
    |  |  |args:
    |  |  |  IdentifierList[1:12-1:13]
    |  |  |  |  Identifier[1:12-1:13]: a
    |  FunctionNode[1:16-1:44]
    |  |proto:
    |  |  Prototype[1:20-1:29]
    |  |  |name:
    |  |  |  Identifier[1:20-1:23]: bar
    |  |  |args:
    |  |  |  IdentifierList[1:24-1:28]
    |  |  |  |  Identifier[1:24-1:25]: a
    |  |  |  |  Identifier[1:27-1:28]: b
    |  |body:
    |  |  BinaryExpr[1:30-1:44]
    |  |  |lhs:
    |  |  |  Identifier[1:30-1:31]: a
    |  |  |op:
    |  |  |  OperatorMult[1:32-1:33]
    |  |  |rhs:
    |  |  |  CallExpr[1:34-1:44]
    |  |  |  |callee:
    |  |  |  |  Identifier[1:34-1:37]: foo
    |  |  |  |args:
    |  |  |  |  ExprList[1:38-1:43]
    |  |  |  |  |  BinaryExpr[1:38-1:43]
    |  |  |  |  |  |lhs:
    |  |  |  |  |  |  Identifier[1:38-1:39]: a
    |  |  |  |  |  |op:
    |  |  |  |  |  |  OperatorPlus[1:40-1:41]
    |  |  |  |  |  |rhs:
    |  |  |  |  |  |  Number[1:42-1:43]: 1


Yay! What a pretty parse tree! Here's also a very useful tip for grammar
development: it's possible to run ``libkaleidoscopelang_parse`` on rules that
are not the main ones. For instance, imagine we want to test only the ``expr``
parsing rule: you just have to use the ``-r`` argument to specify that we want
the parser to start with it:

.. code-block:: text

    $ libkaleidoscopelang_parse -r prototype 'foo(a, b)'
    Prototype[1:1-1:10]
    |f_name:
    |  Identifier[1:1-1:4]: foo
    |f_args:
    |  IdentifierList[1:5-1:9]
    |  |  Identifier[1:5-1:6]: a
    |  |  Identifier[1:8-1:9]: b

So we have our analysis library. We can already use it to parse code, get a
parse tree and do something useful with it.


Using the generated library's Python API
========================================

The previous steps of this tutorial led us to generate an analysis library for
the Kaleidoscope language. That's cool, but what would be even cooler would be
to use this library. So what about writing an interpreter for Kaleidoscope
code?

Interpreter
-----------

The generated library is implemented using the Ada programming language, so its
"native" API is an Ada API. However Langkit by default also generates language
bindings for it: the C API (inconvenient to use, rather internal) and a Python
API. Let's use the Python API for now as it's more concise and handier.
Besides, using the Python API makes it really easy to experiment since you have
an interactive interpreter.

Alright, so the first thing to do with the Python API is to import the
``libkaleidoscopelang`` module and instantiate an analysis context from it:

.. code-block:: python

    import libkaleidoscopelang as lkl
    ctx = lkl.AnalysisContext()

Then, we can parse code in order to yield ``AnalysisUnit`` objects, which
contain the parse tree. There are two ways to parse code: parse from a file or
parse from a in-memory buffer (i.e. a string value):

.. code-block:: python

    # Parse code from the 'foo.kal' file.
    unit_1 = ctx.get_from_file('foo.kal')

    # Parse code from a buffer as if it came from the 'foo.kal' file.
    unit_2 = ctx.get_from_buffer('bar.kal', 'def bar(a, b) a + b;')

    print(unit_1)
    # <AnalysisUnit 'foo.kal'>

    print(unit_2)
    # <AnalysisUnit 'bar.kal'>

The parse tree is reachable thanks to the ``root`` attribute in analysis units:
you can then browse the parse tree programmatically:

.. code-block:: python

    # Get the root node
    print(unit_2.root)
    # <KaleidoscopeNodeList bar.kal:1:1-1:21>

    unit_2.root.dump()
    # KaleidoscopeNodeList bar.kal:1:1-1:21
    # |item_0:
    # |  FunctionNode bar.kal:1:1-1:21
    # |  |f_proto:
    # |  |  Prototype bar.kal:1:5-1:14
    # |  |  |f_name:
    # |  |  |  Identifier bar.kal:1:5-1:8: bar
    # ...

    print(unit_2.root[0])
    # <FunctionNode bar.kal:1:1-1:21>

    print(list(unit_2.root[0].iter_fields()))
    # [
    #     ('f_proto', <Prototype bar.kal:1:5-1:14>),
    #     ('f_body', <BinaryExpr bar.kal:1:15-1:20>),
    # ]

    print(list(unit_2.root[0].f_body))
    # [
    #     <Identifier bar.kal:1:15-1:16>,
    #     <OperatorPlus bar.kal:1:17-1:18>,
    #     <Identifier bar.kal:1:19-1:20>,
    # ]

Note how names for node fields got a ``f_`` prefix: this is used to distinguish
node fields from generic attributes and methods, such as ``iter_fields`` or
``sloc_range``. Similarly, the ``Function`` type was renamed as
``FunctionNode`` so that the name does not clash with the ``function`` keyword
in Ada in the generated library.

You are kindly invited to either skim through the generated Python module or
use the ``help(...)`` built-in in order to discover how you can explore trees.

Alright, let's start the interpreter, now! First, let's declare an
``Interpreter`` class and an ``ExecutionError`` exception:

.. code-block:: python

    class ExecutionError(Exception):
        def __init__(self, sloc_range: lkl.SlocRange, message: str):
            self.sloc_range = sloc_range
            self.message = message


    class Interpreter:
        def __init__(self) -> None:
            # The following dict keeps track of function declarations found so
            # far.
            self.functions: dict[str, lkl.FunctionNode] = {}

        def execute(self, root: lkl.KaleidoscopeNodeList) -> None:
            pass # TODO

        def evaluate(
            self,
            expr: lkl.Expr,
            env: dict[str, float] | None = None,
        ) -> float:
            pass # TODO

Our interpreter will raise an ``ExecutionError`` each time the Kaleidoscope
program does something wrong. In order to execute a script, one has to
instantiate the ``Interpreter`` class and to invoke its ``execute`` method
passing it the parse tree. Then, evaluating any expression is easy: just invoke
the ``evaluate`` method passing it an ``Expr`` instance.

Our top-level code looks like this:

.. code-block:: python

    def print_error(
        filename: str,
        sloc_range: lkl.SlocRange,
        message: str,
    ) -> None:
        line = sloc_range.start.line
        column = sloc_range.start.column
        print(f"In {filename}, line {line}:", file=sys.stderr)
        with open(filename) as f:
            # Get the corresponding line in the source file and display it
            for _ in range(sloc_range.start.line - 1):
                f.readline()
            print(f"  {f.readline().rstrip()}", file=sys.stderr)
            print(f"  {' ' * (column - 1)}^", file=sys.stderr)
        print(f"Error: {message}", file=sys.stderr)


    def execute(filename: str) -> None:
        ctx = lkl.AnalysisContext()
        unit = ctx.get_from_file(filename)
        if unit.diagnostics:
            for diag in unit.diagnostics:
                print_error(filename, diag.sloc_range, diag.message)
                sys.exit(1)
        root = unit.root
        assert isinstance(root, lkl.KaleidoscopeNodeList)
        try:
            Interpreter().execute(root)
        except ExecutionError as exc:
            print_error(filename, exc.sloc_range, exc.message)
            sys.exit(1)

Call ``execute`` with a filename and it will:

1. parse the corresponding script;
2. print any lexing/parsing error (and exit if there are errors);
3. interpret it (and print messages from execution errors).

The ``print_error`` function is a fancy helper to nicely show the user where
the error occurred. Now that the framework is ready, let's implement the
important bits in ``Interpreter``:

.. code-block:: python

    # Method for the Interpreter class
    def execute(self, root: lkl.KaleidoscopeNodeList) -> None:
        for node in root:
            if isinstance(node, lkl.FunctionNode):
                self.functions[node.f_proto.f_name.text] = node

            elif isinstance(node, lkl.ExternDecl):
                raise ExecutionError(
                    node.sloc_range,
                    "External declarations are not supported"
                )

            elif isinstance(node, lkl.TopLevelExpr):
                print(self.evaluate(node.f_expr))

            else:
                # There should be no other kind of node at top-level
                assert False

Nothing really surprising here: we browse all top-level grammatical elements
and take different decisions based on their kind: we register functions,
evaluate expressions and complain when coming across anything else (i.e.
external declarations: given our grammar, it should not be possible to get
another kind of node).

Also note how we access text from nodes: ``node.f_proto.f_name`` is a
``libkaleidoscope.Identifier`` node instance, and its text is available through
the ``text`` attribute.

Now comes the last bit: expression evaluation.

.. code-block:: python

    # Method for the Interpreter class
    def evaluate(
        self,
        expr: lkl.Expr,
        env: dict[str, float] | None = None,
    ) -> float:
        local_env = env or {}
        if env is None:
            env = {}

        if isinstance(expr, lkl.Number):
            return float(expr.text)

        elif isinstance(expr, lkl.Identifier):
            try:
                return local_env[expr.text]
            except KeyError:
                raise ExecutionError(
                    expr.sloc_range, f"Unknown identifier: {expr.text}"
                )

This first chunk introduces how we deal with "environments" (i.e. how we
associate values to identifiers). ``evaluate`` takes an optional parameter
which is used to provide an environment to evaluate the expression. If the
expression is allowed to reference the ``a`` variable, which contains ``1.0``,
then ``env`` will be ``{"a": 1.0}``.

Let's continue: first add the following declaration to the ``Interpreter``
class:

.. code-block:: python

    # Mapping: enumerators for the Operator type -> callables to perform the
    # operations themselves.
    BINOPS = {
        lkl.OperatorPlus: lambda x, y: x + y,
        lkl.OperatorMinus: lambda x, y: x - y,
        lkl.OperatorMult: lambda x, y: x * y,
        lkl.OperatorDiv: lambda x, y: x / y,
    }

Now, we can easily evaluate binary operations. Get back to the ``evaluate``
method definition and complete it with:

.. code-block:: python

        elif isinstance(expr, lkl.BinaryExpr):
            lhs = self.evaluate(expr.f_lhs, local_env)
            rhs = self.evaluate(expr.f_rhs, local_env)
            return self.BINOPS[type(expr.f_op)](lhs, rhs)

And finally, the very last bit: function calls!

.. code-block:: python

        elif isinstance(expr, lkl.CallExpr):
            name = expr.f_callee.text
            try:
                func = self.functions[name]
            except KeyError:
                raise ExecutionError(
                    expr.f_callee.sloc_range, f"No such function: '{name}'"
                )
            formals = func.f_proto.f_args
            actuals = expr.f_args

            # Check that the call is consistent with the function prototype
            if len(formals) != len(actuals):
                raise ExecutionError(
                    expr.sloc_range,
                    f"'{name}' expects {len(formals)} arguments, but got"
                    f" {len(actuals)} ones",
                )

            # Evaluate arguments and then evaluate the call itself
            new_env = {f.text: self.evaluate(a, local_env)
                       for f, a in zip(formals, actuals)}
            result = self.evaluate(func.f_body, new_env)
            return result

        else:
            # There should be no other kind of node in expressions
            assert False

Here we are! Let's try this interpreter on some "real-world" Kaleidoscope code:

.. code-block:: text

    def add(a, b)
      a + b;

    def sub(a, b)
      a - b;

    1;
    add(1, 2);
    add(1, sub(2, 3));

    meh();

Save this to a ``foo.kal`` file, for instance, and run the interpreter:

.. code-block:: text

    $ python kalrun.py foo.kal
    1.0
    3.0
    0.0
    In foo.kal, line 11:
      meh();
      ^
    Error: No such function: "meh"

Congratulations, you wrote an interpreter with Langkit! Enhancing the lexer,
the parser and the interpreter to handle fancy language constructs such as
conditionals, more data types or variables is left as an exercise for the
readers! ;-)

See also ``kalint.py`` file if you need any hint on how to correctly
assemble all the piece of code given above.

Pretty-printing
---------------

.. todo::

    Once the constraints for unparsing are properly documented, write an
    unparsing configuration and use it to reformat Kaleidoscope code.

IDE support
-----------

.. todo::

    Extend Kaleidoscope to generate a language server for it.
