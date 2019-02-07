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

* `Quex <http://sourceforge.net/projects/quex/files/HISTORY/0.64/>`_ (a lexer
  generator), version 0.64.8;

* Mako, a template system for Python which should already be installed if you
  used ``setup.py/easy_install/pip/...`` to install Langkit.


Getting started
===============

Alright, so having to copy-paste files in order to start something is quite
boring: let's use a script that will do this for us! Move to a working
directory and run:

.. code-block:: text

    $ create-project.py Kaleidoscope

This will create a ``kaleidoscope`` directory, a dummy language specification
(lexer and parser) as well as a ``manage.py`` script that will help you to
generate and build your analysis library. Let's step into it:

.. code-block:: text

    $ cd kaleidoscope

And check that this skeleton already builds:

.. code-block:: text

    $ ./manage.py make

This should generate and then build the analysis library in the ``build`` local
directory. Check in particular:

* ``build/include`` and ``build/lib``, which contain the Ada sources, C header
  files and static/shared libraries for the generated library;

* ``build/bin``, which contains a ``parse`` binary, useful to easily run the
  lexer/parser from the command line; note that it is statically linked with
  the generated library to ease debugging and testing (you don't have to add
  ``build/lib`` directory to your ``LD_LIBRARY_PATH``);

* ``build/python``, which contains the Python binding for the generated
  library.

If everything went fine, you should be able to run the ``parse`` test binary:

.. code-block:: text

    $ build/bin/parse
    Parsing failed:
    <input>:1:1: Expected 'example', got Termination
    <null node>

Great! This binary just tries to parse its command-line argument and displays
the resulting AST. The dummy language specification describes a language that
allows exactly one "example" keyword:

.. code-block:: text

    $ build/bin/parse example
    ExampleNode[1:1-1:8]

Here, we have an ``ExampleNode`` which spans from line 1, column 1 to line 1,
column 8.  This language is pretty useless but now we checked that the setup
was working, let's implement Kaleidoscope!


Lexing
======

We are about to start with the most elementary piece of code that will handle
our language: the lexer!  Also known as a scanner, a lexer will take a stream
of text (i.e.  your source files) and split it into *tokens* (or *lexemes*),
which are kind of "words" for programming languages. Langkit relies on Quex to
generate an efficient lexer but hides the gory details and lets you just
write a Python description for the lexer. Fire up your favorite code editor and
open ``language/lexer.py``.

This module contains three blocks:

* an import statement, which pulls all the objects we need to build our lexer
  from Langkit;

* a ``Token`` class definition, used to define both the set of token kinds that
  the lexer will produce and what to do with them (more on that below);

* the instantiation of a lexer in ``kaleidoscope_lexer`` and adding two lexing
  rules for it (more on that farther below).

So let's first talk about token kinds. The tokens most lexers yield have a kind
that determines what kind of word they represent: is it an identifier? an
integer literal? a keyword? The parser then relies on this token kind to decide
what to do with it. But we also use the token kind in order to decide whether
we keep the text associated to it and if we do, how to store it.

For instance we generally keep identifiers in symbol tables so that we can
compare them efficiently (no string comparison, just a pointer equality, for
example) and allocate memory for the text only once: identical identifiers will
reference the same memory chunk. On the other hand, string literals are almost
always unique and thus are not good candidates for symbol tables.

In Langkit, we declare the list of token kinds subclassing the ``LexerToken``
class.

.. code-block:: python

    class Token(LexerToken):
        Example    = WithText()

        # Keywords
        Def        = WithText()
        Extern     = WithText()

        # Other alphanumeric tokens
        Identifier = WithSymbol()
        Number     = WithText()

        # Punctuation
        LPar       = WithText()
        RPar       = WithText()
        Comma      = WithText()
        Colon      = WithText()

        # Operators
        Plus       = WithText()
        Minus      = WithText()
        Mult       = WithText()
        Div        = WithText()

Ok, so here we have four kind of tokens:

* Identifiers, which we'll use for function names and variable names so we want
  to put the corresponding text in a symbol table. We use ``WithSymbol``
  instances to achieve this.

* All other tokens (keywords such as ``def`` or ``extern``, decimal literals
  ``Number``, etc.) for which we will just keep the associated text, we use
  ``WithText`` instances. This will allow us later able to extract the
  corresponding integer value for decimal literals for instance.

Do not forget to add ``WithSymbol`` to the import statement so that you can use
them in your lexer specification.

Good, so now let's create the lexer itself.  The first thing to do is to
instantiate the ``Lexer`` class and provide it the set of available tokens:

.. code-block:: python

    kaleidoscope_lexer = Lexer(Token)

Then, the only thing left to do is to add lexing rules to match text and
actually yield Tokens. This is done using our lexer's ``add_rules`` method:

.. code-block:: python

    kaleidoscope_lexer.add_rules(
        (Pattern(r"[ \t\r\n]+"),                        Ignore()),
        (Pattern(r"#.*"),                               Ignore()),

        (Literal("def"),                                Token.Def),
        (Literal("extern"),                             Token.Extern),
        (Pattern(r"[a-zA-Z][a-zA-Z0-9]*"),              Token.Identifier),
        (Pattern(r"([0-9]+)|([0-9]+\.[0-9]*)|([0-9]*\.[0-9]+)"), Token.Number),

        (Literal("("),                                  Token.LPar),
        (Literal(")"),                                  Token.RPar),
        (Literal(","),                                  Token.Comma),
        (Literal(";"),                                  Token.Colon),

        (Literal("+"),                                  Token.Plus),
        (Literal("-"),                                  Token.Minus),
        (Literal("*"),                                  Token.Mult),
        (Literal("/"),                                  Token.Div),
    )

This kind of construct is very analog to what you can find in other lexer
generators such as ``flex``: on the left you have what text to match and on the
right you have what should be done with it:

* The first ``Pattern`` matches any blank character and discards them, thanks
  to the ``Ignore`` action.

* The second one discards comments (everything starting with ``#`` until the
  end of the line).

* The two ``Literal`` matchers hit on the corresponding keywords and associate
  the corresponding token kinds.

* The two last ``Pattern`` will respectively match identifiers and numbers, and
  emit the corresponding token kinds.

Only exact input strings trigger ``Literal`` matchers while the input is
matched against a regular expression with ``Pattern`` matchers. Note that the
order of rules is meaningful: here, the input is matched first against keywords
and then only if there is no match, identifers and number patterns are matched.
If ``Literal`` rules appeared at the end, ``def`` would always be emitted
as an identifier.

In both the token kinds definition and the rules specification above, we kept
handling for the ``example`` token in order to keep the parser happy (it still
references it). You will be able to get rid of it once we take care of the
parser.

Alright, let's see how this affects our library. As for token kind definitions,
don't forget to import ``Pattern`` and ``Ignore`` from ``langkit.lexer`` and
then re-build the library.

Before our work, only ``example`` was accepted as an input, everything else was
rejected by the lexer:

.. code-block:: text

    $ build/bin/parse def
    Parsing failed:
    <input>:1:1: Invalid token, ignored
    <input>:1:2: Invalid token, ignored
    <input>:1:3: Invalid token, ignored
    <input>:1:4: Expected 'example', got Termination
    <null node>

Now, you should get this:

.. code-block:: text

    Parsing failed:
    <input>:1:1: Expected 'example', got 'def'
    <null node>

The parser is still failing but that's not a surprise since we only took care
of the lexer so far. What is interesting is that we see thanks to ``"Def"``
that the lexer correctly turned the ``def`` input text into a ``Def`` token.
Let's check with numbers:

.. code-block:: text

    $ build/bin/parse 0
    Parsing failed:
    <input>:1:1: Expected 'example', got Number
    <null node>

Looking good! Lexing seems to work, so let's get the parser working.


AST and Parsing
===============

The job of parsers is to turn a stream of tokens into an AST (Abstract Syntax
Tree), which is a representation of the source code making analysis easier. Our
next task will be to actually define how our AST will look like so that the
parser will know what to create.

Take your code editor, open ``language/parser.py`` and replace the
``ExampleNode`` class definition with the following ones:

.. code-block:: python

    class Function(KaleidoscopeNode):
        proto = Field()
        body  = Field()

    class ExternDecl(KaleidoscopeNode):
        proto = Field()

    class Prototype(KaleidoscopeNode):
        name = Field()
        args = Field()

    @abstract
    class Expr(KaleidoscopeNode):
        pass

    class Number(Expr):
        token_node = True

    class Identifier(Expr):
        token_node = True

    class Operator(KaleidoscopeNode):
        enum_node = True
        alternatives = ['plus', 'minus', 'mult', 'div']

    class BinaryExpr(Expr):
        lhs = Field()
        op = Field()
        rhs = Field()

    class CallExpr(Expr):
        callee = Field()
        args = Field()

As usual, new code comes with its new dependencies: also complete the
``langkit.dsl`` import statement with ``abstract`` and ``Field``.

Each class definition is a way to declare how a particular AST node will look.
Think of it as a kind of structure: here the ``Function`` AST node has two
fields: ``proto`` and ``body``. Note that unlike most AST declarations out
there, we did not associate types to the fields: this is expected as we will
see later.

Some AST nodes can have multiple forms: for instance, an expression can be
a number or a binary operation (addition, subtraction, etc.) and in each case
we need to store different information in them: in the former we just need the
number value whereas in binary operations we need both members of the additions
(``lhs`` and ``rhs`` in the ``BinaryExpr`` class definition above) and the kind
of operation (``op`` above). The strategy compiler writers sometimes adopt is
to use inheritance (as in `OOP
<https://en.wikipedia.org/wiki/Object-oriented_programming>`_) in order to
describe such AST nodes: there is an abstract ``Expr`` class while the
``Number`` and ``BinaryExpr`` are concrete classes deriving from it.

This is exactly the approach that Langkit uses: all "root" AST nodes derive
from the ``KaleidoscopeNode`` class, and you can create abstract classes (using
the ``abstract`` class decorator) to create a hierarchy of node types.

Careful readers may also have spotted something else: the ``Operator``
enumeration node type. We use an enumeration node type in order to store in the
most simple way what kind of operation a ``BinaryExpr`` represents. As you can
see, creating an enumeration node type is very easy: simply set the special
``enum_node`` annotation to ``True`` in the node class body and set the
``alternatives`` field to a sequence of strings that will serve as names
for the enumeration node values (also called *enumerators*).

There is also the special ``token_node = True`` annotation, which both the
``Number`` and ``Identifier`` classes have. This annotation specifies that
these nodes don't hold any field but instead are used to materialize in the
tree a single token. When compiling the grammar, Langkit will make sure that
parsers creating these kind of nodes do consume only one token.

Fine, we have our data structures so now we shall use them! In order to create
a parser, Langkit requires you to describe a grammar, hence the ``Grammar``
instantiation already present in ``parser.py``. Basically, the only thing you
have to do with a grammar is to add *rules* to it: a rule is a kind of
sub-parser, in that it describes how to turn a stream of token into an AST.
Rules can reference each other recursively: an expression can be a binary
operator, but a binary operator is itself composed of expressions! And in order
to let the parser know how to start parsing you have to specify an entry rule:
this is the ``main_rule_name`` field of the grammar (currently set to
``'main_rule'``).

Langkit generates recursive descent parsers using `parser combinators
<https://en.wikipedia.org/wiki/Parser_combinator>`_. Here are a few fictive
examples:

* ``'def'`` matches exactly one ``def`` token;
* ``Def('def', Token.Identifier)`` matches a ``def`` token followed by an
  identifier token, creating a ``Def`` node.
* ``Or('def', 'extern')`` matches either a ``def`` keyword, either a ``extern``
  one (no more, no less).

The basic idea is that you use the callables Langkit provides (``List``, ``Or``,
etc. from the ``langkit.parsers`` module) in order to compose in a quite
natural way what rules can match. Let's move forward with a real world example:
Kaleidoscope! Each chunk of code below appears as a keyword argument of the
``add_rules`` method invocation (you can remove the previous ``main_rule``
one). But first, let's add a shortcut for our grammar instance:

.. code-block:: python

    G = kaleidoscope_grammar

We also need to import the ``Token`` class from our lexer module:

.. code-block:: python

    from language.lexer import Token

Now, redefine the ``main_rule`` parsing rule:

.. code-block:: python

    main_rule=List(Pick(Or(G.extern_decl, G.function, G.expr), ';')),

``G.external_decl`` references the parsing rule called ``external_decl``.  It
does not exist yet, but Langkit allows such forward references anyway so that
rules can reference themselves in a recursive fashion.

So what this rule matches is a list in which elements can be either external
declarations, function definitions or expressions, each one followed by a
colon.

.. code-block:: python

    extern_decl=ExternDecl('extern', G.prototype),

This one is interesting: inside the parens, we matches the ``extern`` keyword
followed by what the ``prototype`` rule matches. Then, thanks to the
``ExternDecl`` call, we take the content we matched and create an
``ExternDecl`` AST node to hold the result.

... but how is that possible? We saw above that ``ExternDecl`` has only one
field, whereas the call matched two items. The trick is that by default, mere
tokens are discarded.  Once it's discarded, the only thing left is what
``prototype`` matched, and so there is exactly one result to put in
``ExternDecl``.

.. code-block:: python

    function=Function('def', G.prototype, G.expr),

We have here a pattern that is very similar to ``extern_decl``, except that the
AST node constructor has two non-discarded results: ``prototype`` and ``expr``.
This is fortunate, as the ``Function`` node requires two fields.

.. code-block:: python

    prototype=Prototype(G.identifier, '(',
                        List(G.identifier, sep=',', empty_valid=True),
                        ')'),

The only new bit in this rule is how the ``List`` combinator is used: last
time, it had only one parameter: a sub-parser to specify how to match
individual list elements. Here, we also have a ``sep`` argument to specify that
a comma token must be present between each list item and the ``empty_valid``
argument tells ``List`` that it is valid for the parsed list to be empty (it's
not allowed by default).

So our argument list has commas to separate arguments and we may have functions
that take no argument.

.. code-block:: python

    expr=Or(
        Pick('(', G.expr, ')'),
        BinaryExpr(G.expr,
            Or(Operator.alt_plus('+'),
               Operator.alt_minus('-')),
            G.prod_expr
        ),
        G.prod_expr,
    ),

Let's dive into the richest grammatical element of Kaleidoscope: expressions!
An expression can be either:

* A sub-expression nested in parenthesis, to give users more control over how
  associativity works. Note that we used here the ``Pick`` parser to parse
  parens while only returning the AST node that ``G.expr`` yields.

* Two sub-expressions with an operator in the middle, building a binary
  expression. This shows how we can turn tokens into enumerators:

  .. code-block:: python

      Operator.alt_plus('+')

  This matches a ``+`` token (``Plus`` in our lexer definition) and yields the
  ``plus`` node enumerator from the ``Operator`` enumeration node type.

* The ``prod_expr`` kind of expression: see below.

.. code-block:: python

    prod_expr=Or(
        BinaryExpr(G.prod_expr,
            Or(Operator.alt_mult('*'),
               Operator.alt_div('/')),
            G.call_or_single
        ),
        G.call_or_single,
    ),

This parsing rule is very similar to ``expr``: except for the parents
sub-rule, the difference lies in which operators are allowed there: ``expr``
allowed only sums (plus and minus) whereas this one allows only products
(multiplication and division). ``expr`` references itself everywhere except for
the right-hand-side of binary operations and the "forward" sub-parser: it
references the ``prod_expr`` rule instead. On the other hand, ``prod_expr``
references itself everywhere with the same exceptions.  This layering pattern
is used to deal with associativity in the parser: going into details of parsing
methods is not the purpose of this tutorial but fortunately there are many
articles that explain `how this works
<https://www.google.fr/search?q=recursive+descent+parser+associativity>`_ (just
remember that: yes, Langkit handles left recursivity!).

.. code-block:: python

    call_or_single=Or(
        CallExpr(G.identifier, '(',
                 List(G.expr, sep=',', empty_valid=True),
                 ')'),
        G.identifier,
        G.number,
    ),

Well, this time there is nothing new. Moving on to the two last rules...

.. code-block:: python

    identifier=Identifier(Token.Identifier),
    number=Number(Token.Number),

Until now, the parsing rules we wrote only used string literals to match
tokens. While this works for things like keywords, operators or punctuation, we
cannot match a token kind with no specific text associated this way. So these
rules use instead directly reference the tokens defined in your
``language.lexer.Token`` class (don't forget to import it!).

Until now, we completely put aside types in the AST: fields were declared
without associated types. However, in order to generate the library, someone
*has* to take care of assigning definite type to them. Langkit uses for that a
`type inference <https://en.wikipedia.org/wiki/Type_inference>`_ algorithm
that deduces types automatically from how AST nodes are used in the grammar.
For instance, doing the following (fictive example):

.. code-block:: python

    SomeNode(SomeEnumeration.alt_someval('sometok'))

Then the typer will know that the type of the SomeNode's only field is the
``SomeEnumeration`` type.

Our grammar is complete, for a very simple version of the Kaleidoscope
language! If you have dealt with Yacc-like grammars before, I'm sure you'll
find this quite concise, especially considering that it covers both parsing and
AST building.

Let's check with basic examples if the parser works as expected. First, we have
to launch another build and then run ``parse`` on some code:

.. code-block:: text

    $ ./manage.py make
    [... snipped...]

    $ build/bin/parse 'extern foo(a); def bar(a, b) a * foo(a + 1);'
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


Yay! What a pretty AST! Here's also a very useful tip for grammar development:
it's possible to run ``parse`` on rules that are not the main ones. For
instance, imagine we want to test only the ``expr`` parsing rule: you just
have to use the ``-r`` argument to specify that we want the parser to start
with it:

.. code-block:: text

    $ build/bin/parse -r expr '1 + 2'
    BinaryExpr[1:1-1:6]
    |lhs:
    |  Number[1:1-1:2]: 1
    |op:
    |  OperatorPlus[1:3-1:4]
    |rhs:
    |  Number[1:5-1:6]: 2

So we have our analysis library: there's nothing more we can do right now to
enhance it, but on the other hand we can already use it to parse code and get
AST's.


Using the generated library's Python API
========================================

The previous steps of this tutorial led us to generate an analysis library for
the Kaleidoscope language. That's cool, but what would be even cooler would be
to use this library. So what about writing an interpreter for Kaleidoscope
code?

Kaleidoscope interpreter
------------------------

At the moment, the generated library uses the Ada programming language and its
API isn't stable yet. However, it also exposes a C API and a Python one on the
top of it. Let's use the Python API for now as it's more concise, handier and
likely more stable. Besides, using the Python API makes it really easy to
experiment since you have an interactive interpreter. So, considering you
successfully built the library with the Kaleidoscope parser and lexer, make
sure the ``build/lib/langkit_support.relocatable`` and the
``build/lib/libkaleidoscopelang.relocatable`` directories is in your
``LD_LIBRARY_PATH`` (on most Unix, ``DYLD_FALLBACK_LIBRARY_PATH`` on Darwin,
adapt for Windows) and that the ``build/python/libkaleidoscopelang.py`` is
reachable from Python (add ``build/python`` in your ``PYTHONPATH`` environment
variable).

Alright, so the first thing to do with the Python API is to import the
``libkaleidoscopelang`` module and instantiate an analysis context from it:

.. code-block:: python

    import libkaleidoscopelang as lkl
    ctx = lkl.AnalysisContext()

Then, we can parse code in order to yield ``AnalysisUnit`` objects, which
contain the AST. There are two ways to parse code: parse from a file or parse
from a buffer (i.e. a string value):

.. code-block:: python

    # Parse code from the 'foo.kal' file.
    unit_1 = ctx.get_from_file('foo.kal')

    # Parse code from a buffer as if it came from the 'foo.kal' file.
    unit_2 = ctx.get_from_buffer('foo.kal', 'def foo(a, b) a + b;')

.. todo::

    When diagnostics bindings in Python will become more convenient (useful
    __repr__ and __str__), talk about them.

The AST is reachable thanks to the ``root`` attribute in analysis units: you
can then browse the AST nodes programmatically:

.. code-block:: python

    # Get the root AST node.
    print unit_2.root
    # <KaleidoscopeNodeList 1:1-1:21>

    unit_2.root.dump()
    # KaleidoscopeNodeList 1:1-1:21
    # |item_0:
    # |  FunctionNode 1:1-1:20
    # |  |proto:
    # |  |  Prototype 1:5-1:14
    # |  |  |name:
    # |  |  |  Identifier 1:5-1:8: foo
    # ...

    print unit_2.root[0]
    # <FunctionNode 1:1-1:20>

    print list(unit_2.root[0].iter_fields())
    # [(u'f_proto', <Prototype 1:5-1:14>),
    #  (u'f_body', <BinaryExpr 1:15-1:20>)]

    print list(unit_2.root[0].f_body)
    # [<Identifier 1:15-1:16>,
    #  <OperatorPlus 1:17-1:18>,
    #  <Identifier 1:19-1:20>]

Note how names for AST node fields got a ``f_`` prefix: this is used to
distinguish AST node fields from generic AST node attributes and methods, such
as ``iter_fields`` or ``sloc_range``. Similarly, the ``Function`` AST type was
renamed as ``FunctionNode`` so that the name does not clash with the
``function`` keyword in Ada in the generated library.

You are kindly invited to either skim through the generated Python module or
use the ``help(...)`` built-in in order to discover how you can explore trees.

Alright, let's start the interpreter, now! First, let's declare an
``Interpreter`` class and an ``ExecutionError`` exception:

.. code-block:: python

    class ExecutionError(Exception):
        def __init__(self, sloc_range, message):
            self.sloc_range = sloc_range
            self.message = message


    class Interpreter(object):
        def __init__(self):
            # Mapping: function name -> FunctionNode instance
            self.functions = {}

        def execute(self, ast):
            pass # TODO

        def evaluate(self, node, env=None):
            pass # TODO

Our interpreter will raise an ``ExecutionError`` each time the Kaleidoscope
program does something wrong. In order to execute a script, one has to
instantiate the ``Interpreter`` class and to invoke its ``execute`` method
passing it the parsed AST. Then, evaluating any expression is easy: just invoke
the ``evaluate`` method passing it an ``Expr`` instance.

Our top-level code looks like this:

.. code-block:: python

    def print_error(filename, sloc_range, message):
        line = sloc_range.start.line
        column = sloc_range.start.column
        print >> sys.stderr, 'In {}, line {}:'.format(filename, line)
        with open(filename) as f:
            # Get the corresponding line in the source file and display it
            for _ in range(sloc_range.start.line - 1):
                f.readline()
            print >> sys.stderr, '  {}'.format(f.readline().rstrip())
            print >> sys.stderr, '  {}^'.format(' ' * (column - 1))
        print >> sys.stderr, 'Error: {}'.format(message)


    def execute(filename):
        ctx = lkl.AnalysisContext()
        unit = ctx.get_from_file(filename)
        if unit.diagnostics:
            for diag in unit.diagnostics:
                print_error(filename, diag.sloc_range, diag.message)
            sys.exit(1)
        try:
            Interpreter().execute(unit.root)
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
    def execute(self, ast):
        assert isinstance(ast, lkl.KaleidoscopeNodeList)
        for node in ast:
            if isinstance(node, lkl.FunctionNode):
                self.functions[node.f_proto.f_name.text] = node

            elif isinstance(node, lkl.ExternDecl):
                raise ExecutionError(
                    node.sloc_range,
                    'External declarations are not supported'
                )

            elif isinstance(node, lkl.Expr):
                print self.evaluate(node)

            else:
                # There should be no other kind of node at top-level
                assert False

Nothing really surprising here: we browse all top-level grammatical elements
and take different decisions based on their kind: we register functions,
evaluate expressions and complain when coming across anything else (i.e.
external declarations: given our grammar, it should not be possible to get
another kind of node).

Also note how we access text from tokens: ``node.f_proto.f_name.f_name`` is a
``libkaleidoscope.Token`` instance, and its text is available through the
``text`` attribute. Our AST does not contain any, but if you had tokens without
text (remember, it's the lexer declaration that decides whether we keep text or
not for each specific token), the ``text`` attribute would return ``None``
instead.

Now comes the last bit: expression evaluation.

.. code-block:: python

    # Method for the Interpreter class
    def evaluate(self, node, env=None):
        if env is None:
            env = {}

        if isinstance(node, lkl.Number):
            return float(node.text)

        elif isinstance(node, lkl.Identifier):
            try:
                return env[node.text]
            except KeyError:
                raise ExecutionError(
                    node.sloc_range,
                    'Unknown identifier: {}'.format(node.text)
                )

This first chunk introduces how we deal with "environments" (i.e. how we
associate values to identifiers). ``evaluate`` takes an optional parameter
which is used to provide an environment to evaluate the expression. If the
expression is allowed to reference the ``a`` variable, which contains ``1.0``,
then ``env`` will be ``{'a': 1.0}``.

Let's continue: first add the following declaration to the ``Interpreter``
class:

.. code-block:: python

    # Mapping: enumerators for the Operator type -> callables to perform the
    # operations themselves.
    BINOPS = {lkl.OperatorPlus:  lambda x, y: x + y,
              lkl.OperatorMinus: lambda x, y: x - y,
              lkl.OperatorMult:  lambda x, y: x * y,
              lkl.OperatorDiv:   lambda x, y: x / y}

Now, we can easily evaluate binary operations. Get back to the ``evaluate``
method definition and complete it with:

.. code-block:: python

        elif isinstance(node, lkl.BinaryExpr):
            lhs = self.evaluate(node.f_lhs, env)
            rhs = self.evaluate(node.f_rhs, env)
            return self.BINOPS[type(node.f_op)](lhs, rhs)

Yep: in the Python API, enumerators appear as strings. It's the better tradeoff
we found so far to write concise code while avoiding name clashes: this works
well even if multiple enumeration types have homonym enumerators.

And finally, the very last bit: function calls!

.. code-block:: python

        elif isinstance(node, lkl.CallExpr):
            name = node.f_callee.text
            try:
                func = self.functions[name]
            except KeyError:
                raise ExecutionError(
                    node.f_callee.sloc_range,
                    'No such function: "{}"'.format(name)
                )
            formals = func.f_proto.f_args
            actuals = node.f_args

            # Check that the call is consistent with the function prototype
            if len(formals) != len(actuals):
                raise ExecutionError(
                    node.sloc_range,
                    '"{}" expects {} arguments, but got {} ones'.format(
                        node.f_callee.f_name.text,
                        len(formals), len(actuals)
                    )
                )

            # Evaluate arguments and then evaluate the call itself
            new_env = {f.text: self.evaluate(a, env)
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
      meh()
      ^
    Error: No such function: "meh"

Congratulations, you wrote an interpreter with Langkit! Enhancing the lexer,
the parser and the interpreter to handle fancy language constructs such as
conditionals, more data types or variables is left as an exercise for the
readers! ;-)

.. todo::

    When the sub-parsers are exposed in the C and Python APIs, write the last
    part to evaluate random expressions (not just standalone scripts).

Kaleidoscope IDE support
------------------------

.. todo::

    When we can use trivia as well as semantic requests from the Python API,
    write some example on, for instance, support for Kaleidoscope in GPS
    (highlighting, blocks, cross-references).
