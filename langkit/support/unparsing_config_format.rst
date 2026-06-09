.. _unparsing_configuration_file_format:

Unparsing configuration file format
===================================

The unparsing configuration is a JSON file that provides "document templates",
i.e. patterns to generate Prettier documents. Langkit's unparsing engine uses
these templates to turn a given syntax tree to a Prettier document, and then
delegates the final transformation of this document to text to Prettier itself.

Knowledge about Prettier documents (its Intermediate Representation) is
required in order to write unparsing configuration: please refer to `Prettier's
own documentation
<https://github.com/prettier/prettier/blob/main/commands.md>`_.


Top level structure
-------------------

Unparsing configuration files have the following format:

.. code-block:: json

   {
     "node_configs": {
        "Node1": {},
        "Node2": {}
     },
     "max_empty_lines": 1,
     "token_configs": {}
   }

``node_configs``

   This entry is mandatory, and provides a mapping from node type names to
   :ref:`unparsing_cfg_node_config`.

   Standard node derivation rules apply to configurations: if node B derives
   from node A, and if node B does not specify a configuration for its field F,
   then the configuration of field F for node A applies (same for the list
   separator).

``max_empty_lines``

   Optional. If provided, it must be a natural number that indicates the
   maximum number of consecutive empty lines to preserve during the source code
   reformatting. If omitted, all empty lines are preserved.

``token_configs``

   Optional entry that determines how individual tokens are formatted. See
   :ref:`unparsing_cfg_token_config`.


.. _unparsing_cfg_node_config:

Node configurations
-------------------

Node configurations control how a given node type must be formatted. It is
encoded as a JSON object which can have the following entries:

``node`` (optional)

   If present, it contains a document template to wrap the basic unparsing of
   the node. (see :ref:`unparsing_cfg_doc_templates`).

``fields`` (optional)

   If present, must be a mapping from field names to document templates to
   format the field when it is present in the syntax tree.

``sep`` (optional, only for list nodes)

   Document template to unparse the list separator.

``leading_sep`` and ``trailing_sep`` (optional, only for list nodes that accept
respectively leading and trailing separators)

   Document templates to unparse leading/trailing separators.

``flush_before_children`` (optional, only for list nodes)

   Boolean (true by default), that controls whether line breaks recovered from
   the source to reformat are flushed before each list element.

``independent_lines`` (optional, only for list nodes)

   Boolean (false by default), that controls whether each list item is
   formatted on its own line.  When true, the formatting of rewritten trees can
   stop reformating at the boundary of such nodes.

``table`` (optional,  onlyfor list nodes)

   If present, unparsing such lists yield a list of table documents, each list
   child being unparsing to a list row. If present, it must contain an object
   that accepts the following entries:

   * ``sep_before``: Whether list separators must be inserted at the end of the
     previous row (``"sep_before": true``) or at the beginning of the next row
     (``"sep_before": false``). This is optional, and defaults to ``true``.

   * ``split``: Determine which kind of trivia found between two list children
     trigger a table split (i.e. the presence of such trivias end the current
     table, and trigger the creation of a new table for the next children).
     This field is optional (by default: nothing splits table), and when
     present, must be an array of strings, with the following possible values:
     ``"empty_line"``, ``"line_comment"``.

   * ``must_break``: Whether each row for this table must go on its own line.
     If false (the default), rows go on each line only when a break occurs in
     the table.

   * ``join``: Determine whether a list child must be put on the previous table
     row, i.e. whether to join what would instead be two rows.

     If present, this must be an object with a mandatory "predicate" entry,
     that must be a reference to a predicate property  i.e. a property that
     each list child has and that returns whether to join rows, as a boolean.

     The optional ``template`` entry must be a template that describes how to
     join two rows: the first row is substituted to the ``recurse_left``
     template and the second row is substituted to the ``recurse_right``
     template. For example:

     .. code-block:: json

        "join": {
          "predicate": "p_my_predicate",
          "template": [
            "recurse_left",
            {"kind": "tableSeparator", "text": ""},
            {"kind": "group", "document": ["line", "recurse_right"]}
          ]
        }


.. _unparsing_cfg_token_config:

Token configurations
--------------------

Token configurations control how individual tokens used for parsing (keywords,
punctuation, ...) must be formatted. It is encoded as a JSON object which can
have the following entries:

``default`` (optional)

   When provided, it must be one of the following strings:

   * ``lower``: format tokens as lower case.
   * ``upper``: format tokens as upper case.
   * ``original``: keep the formatting found in the original sources.

   If omitted, ``lower`` is used. Note that for case-sensitive languages,
   ``lower`` and ``upper`` are equivalent: just preserve the casing used in the
   original sources.

``formattings`` (optional)

   A JSON object that maps default token formattings to the ones to use when
   unparsing with this configuration.  These formattings override the
   formattings implied by the ``default`` setting. For associations to
   ``null``, use the formatting found in the original source.

   For example:

   .. code-block:: json

      "token_configs": {
        "default": "upper",
        "formattings": {
          "|": "!",
          "abstract": "Abstract",
          "overriding": null
        }
      }

   This configuration instructs the unparser to, by default, turn all keywords
   into uppercase, but unparse ``|`` tokens into ``!`` (valid if the lexer
   considers that the two are equivalent), unparse ``abstract`` as ``Abstract``
   and preserve the original formatting for ``overriding`` keywords.


.. _unparsing_cfg_doc_templates:

Document templates
------------------

A document template is essentially a set of instructions to produce the expected
Prettier document for a given syntax tree node. Templates are created by the
composition of the following building blocks.

``recurse``

   This is the default template to unparse nodes, node fields or list
   separators. Instantiating it just yields the default unparsing for that
   node/field/separator:

   .. code-block:: json

      "recurse"

``breakParent``

   Yield the corresponding Prettier document:

   .. code-block:: json

      "breakParent"

``line``, ``hardline``, ``hardlineWithoutBreakParent``, ``softline``,
``literalline``

   Yield the corresponding Prettier document:

   .. code-block:: json

      "line"
      "hardline"
      "hardlineWithoutBreakParent"
      "softline"
      "literalline"

``flushLineBreaks``

   Placeholder to emit potential line breaks that come from the source code to
   reformat:

   .. code-block:: json

      "flushLineBreaks"

``trim``

   Yield the corresponding Prettier document.

   .. code-block:: json

      "trim"

``whitespace``

   Yield a ``text`` document with the specified amount of spaces:

   .. code-block:: json

      {"kind": "whitespace", "length": 2}

   Or as a shortcut for a length of 1:

   .. code-block:: json

      "whitespace"

``align``

   Yield an ``align`` Prettier document:

   .. code-block:: json

      {
        "kind": "align",
        "width": 3,
        "contents": "recurse"
      }
      {
        "kind": "align",
        "width": "foo",
        "contents": "recurse"
      }

   See also :ref:`unparsing_cfg_bubble_up`.

``continuationLineIndent``

   Yield an ``continuationLineIndent`` Prettier document:

   .. code-block:: json

      {
        "kind": "continuationLineIndent",
        "contents": "recurse"
      }

   See also :ref:`unparsing_cfg_bubble_up`.

``dedent``

   Yield a ``dedent`` Prettier document:

   .. code-block:: json

      {"kind": "dedent", "contents": "recurse"}

   See also :ref:`unparsing_cfg_bubble_up`.

``dedentToRoot``

   Yield a ``dedentToRoot`` Prettier document:

   .. code-block:: json

      {"kind": "dedentToRoot", "contents": "recurse"}

   See also :ref:`unparsing_cfg_bubble_up`.

``fill``

   Yield a ``fill`` Prettier document:

   .. code-block:: json

      {"kind": "fill", "document": "recurse"}

   See also :ref:`unparsing_cfg_bubble_up`.

``group``

   Yield a ``group`` Prettier document:

   .. code-block:: json

      {"kind": "group", "document": "recurse"}
      {
        "kind": "group",
        "document": "recurse",
        "shouldBreak": true
      }

   An optional ``id`` field makes it define a symbol to reference in the same
   template:

   .. code-block:: json

      {
        "kind": "group",
        "document": "recurse",
        "id": "mySymbol"
      }

   See also :ref:`unparsing_cfg_bubble_up`.

``ifBreak``

   Yield a ``ifBreak`` Prettier document:

   .. code-block:: json

      {"kind": "ifBreak", "breakContents": "recurse"}
      {
        "kind": "ifBreak",
        "breakContents": "recurse",
        "flatContents": "recurse"
      }
      {
        "kind": "ifBreak",
        "breakContents": "recurse",
        "flatContents": "recurse",
        "groupId": "mySymbol"
      }

``match``

   Yield one of several alternative templates depending on a controlling node
   (what the ``node`` expression returns).

   Each alternative template is guarded by a pattern (see
   :ref:`unparsing_cfg_patterns`): the expression yields the alternative
   template associated to the first pattern that matches the controlling node.

   Note that at least one pattern (in practice: the last one) must match all
   values (i.e. be the default pattern):

   .. code-block:: json

      {
        "kind": "match",
        "node": "this_node",
        "matchers": [
          {"pattern": null, "document": "recurse"},
          {"pattern": "*", "document": "recurse"}
        ]
      }

   If evaluating the ``node`` expression triggers an error, the patterns are
   matched against a null node.

``indent``

   Yield a ``indent`` Prettier document:

   .. code-block:: json

      {"kind": "indent", "contents": "recurse"}

   See also :ref:`unparsing_cfg_bubble_up`.

* The "innerRoot" template yields a "innerRoot" Prettier document::

    {"kind": "innerRoot", "contents": "recurse"}

``markAsRoot``

   Yield a ``markAsRoot`` Prettier document:

   .. code-block:: json

      {"kind": "markAsRoot", "contents": "recurse"}

   See also :ref:`unparsing_cfg_bubble_up`.

``recurse_field``

   Valid only in ``node`` templates for concrete nodes that are neither
   abstract, token nor list nodes. When used, the whole template cannot contain
   any ``recurse``/``recurse_flatten`` template, and the template, once
   linearized, must reflect how the node is unparsed.

   For example, let's consider that the ``VarDecl`` node is created parsing the
   following chunks:

   .. code-block:: text

      "var" [f_name] ":" [f_type] ";"

   Then its ``node`` template must contain two ``recurse_field`` templates for
   the two fields, in the same order, and with the same tokens in between.  For
   instance:

   .. code-block:: json

      [
        {"kind": "text", "text": "var"},
        {"kind": "recurse_field", "field": "f_name"},
        {
          "kind": "group",
          "document": [
            {"kind": "text", "text": ":"},
            {"kind": "recurse_field", "field": "f_type"}
          ]
        },
        {"kind": "text", "text": ";"},
      ]

``recurse_flatten``

   Acts like ``recurse`` but refines its result so that the document nested in
   ``align``, ``fill``, ``group``, ``indent`` templates and in 1-item document
   lists is returned instead (recursively).

``tableSeparator``

   Yield a ``tableSeparator`` Prettier document:

   .. code-block:: json

      {"kind": "tableSeparator", "text": "some_text_to_unparse"}

``text``

   Yield a ``text`` Prettier document:

   .. code-block:: json

      {"kind": "text", "text": "some_text_to_unparse"}

   Using this template is valid in specific contexts only:

   * For ``node`` templates, when used with ``recurse_field`` templates: see the
     documentation for ``recurse_field``;

   * For ``fields`` templates: in this case, the linearized template must
     reflect how the field is unparsed. See the documentation for
     ``recurse_field`` to have more information about linearization.

``list``

    JSON lists yield the corresponding ``list`` Prettier documents:

    .. code-block:: json

       ["whitespace", "recurse"]


.. _unparsing_cfg_expressions:

Expressions
-----------

Some document templates like ``match`` or ``if`` contain expressions to control
what documents to yield exactly: they can be evaluated to booleans, strings,
nodes, ... but they never yield documents by themselves.

The following expression building blocks are available:

``bin_op``

    Perform a binary operation on two operands. For instance:

    .. code-block:: json

       {
         "kind": "bin_op",
         "op": "="
         "lhs": {"kind": "node_symbol", "prefix": "this_field"},
         "rhs": {"kind": "symbol_value", "value": "foo"}
       }

    The two operands (``lhs`` and ``rhs`` entries) are sub-expressions, andn
    the operator (``op`` entry) can be one of the following strings:

    * ``=``, to test equality. Both operands must have compatible types (two
      arbitrary nodes that do not need to be the same type, or the same type
      exactly), and the result is a boolean.

    * ``and``/``or``, the short-circuiting boolean operators.

``cast``

    Take a node and convert it to another type. If the conversion fails, this
    yields a null node:

    .. code-block:: json

       {
         "kind": "cast",
         "node": "this_node",
         "type": "NodeType"
       }

``eval_member``

    Evaluate the member (field or property) of a given value (struct or node),
    with potential arguments, and return the field value or property result.
    Property arguments can be passed as positional arguments (in the ``args``
    list) or as keyword arguments (in the ``kwargs`` object), or both (keyword
    arguments are associated after the positional arguments):

    .. code-block:: json

       {
         "kind": "eval_member",
         "member": "some_field",
         "prefix": "this_node"
       }
       {
         "kind": "eval_member",
         "member": "some_property",
         "prefix": "this_node",
         "args": [
            "this_node"
            {"kind": "is_empty": "node": "this_node"}
         ],
         "kwargs": {
           "arg3": "this_node",
           "arg4": {"kind": "node_text", "node": "this_node"}
         }
       }

``is_a``

    Return whether the ``node`` operand matches the given ``pattern`` (see
    :ref:`unparsing_cfg_patterns`):

    .. code-block:: json

       {
         "kind": "is_a",
         "node": "this_node",
         "pattern": {"kind": "node", "type": "Node2"}
       }

``is_empty``

    Return whether its operand is considered as empty for unparsing purposes
    (i.e. an empty list node with no nearby comment).

    .. code-block:: json

       {"kind": "is_empty", "node": "this_node"}

``node_symbol``

    Take a token node and returns the corresponding symbol:

    .. code-block:: json

       {"kind": "node_symbol", "node": "this_node"}

    Note that it returns the empty symbol for null nodes.

``node_text``

    Take a node and returns its text:

    .. code-block:: json

       {"kind": "node_text", "node": "this_node"}

    Note that it returns the empty string for null nodes.

``not``

    Take a boolean and returns its opposite:

    .. code-block:: json

       {
         "kind": "not",
         "operand": {"kind": "is_empty", "node": "this_node"}
       }

``string``

    This expression materializes a string literal:

    .. code-block:: json

       {"kind": "string", "value": "some string value"}

``symbol``

    This expression materializes a symbol literal:

    .. code-block:: json

       {"kind": "symbol", "value": "some string value"}

``this_node``

    Return the node that instantiates the current template:

    .. code-block:: json

       "this_node"

``this_field`` (valid only inside fields configurations)

    Return the child node of ``this_node`` used to instantiate the current
    template.

    .. code-block:: json

       "this_field"

In case of an error happening during expression evaluation, the evaluation is
reported in the ``LANGKIT.UNPARSING.EXPANSION_ERRORS`` trace (GNATCOLL.Traces),
and the encompassing condition evaluates to false.


.. _unparsing_cfg_patterns:

Patterns
--------

Patterns are used to test values against given "shapes". A pattern can be
one of the following:


``*``

    This pattern matches all possible values:

    .. code-block:: json

       "*"

``null``

    Matches null nodes.

    .. code-block:: json

       null

``false``/``true``

    Match either the ``false`` or the ``true`` boolean.

    .. code-block:: json

       false
       true

``symbol_literal``

    Match symbol values against a given constant:

    .. code-block:: json

       {"kind": "symbol_literal", "value": "foo"}

``node``

    Match nodes of a given type (or a derivation of the given type):

    .. code-block:: json

       {"kind": "node", "type": "SomeNodeType"}

    This patterns also allows performing additional tests on its
    fields/properties:

    .. code-block:: json

       {
         "kind": "node",
         "type": "SomeNodeType",
         "members": [
           {
             "member": "f_some_field",
             "pattern": {"kind": "node", "type": "SomeOtherType"}
           },
           {
             "member": "p_some_property",
             "args": ["this_node"],
             "kwargs": {"arg2": "this_field"},
             "pattern": false
           }
         ]
       }

    In the previous example, the pattern will match only nodes ``n`` that
    satisfy *all* of the following conditions:

    * The node is of the ``SomeNodeType`` (or any of its descendant).
    * The ``f_some_field`` member is a node that is of the ``SomeOtherType``
      (likewise).
    * The ``n.p_some_property(this_node, arg2=this_field)`` member evaluates
      without errors and its result is false.

``not``

    Match anything that a subpattern does *not* match. The following pattern
    matches all symbol values *except* the ``foo`` symbol.

    .. code-block:: json

       {"kind": "not", "pattern": {"kind": "symbol_literal", "value": "foo"}}

``or``

    Matches if *any* of the sub-patterns match. The following pattern matches
    either the ``foo`` symbol or the ``bar`` one.

    .. code-block:: json

       {
         "kind": "or",
         "patterns": [
           {"kind": "symbol_literal", "value": "foo"},
           {"kind": "symbol_literal", "value": "bar"}
         ]
       }


.. _unparsing_cfg_bubble_up:

Bubble up (handling of trivias)
-------------------------------

The unparsing engine inserts trivias to preserve (comments and line breaks)
in the same context as the token that preceeds them, with one exception:
trivias that preceed or come after list nodes are inserted in that list node.

After template expansion, the Prettier document produced for a given syntax
tree is post-processed: trivias that appear before the first token of a node
(leading trivias) or after its last token (trailing trivias) may be moved up in
the document tree ("bubbled up").

By default, all leading trivias are bubbled up, and trailing trivias are
bubbled up only for ``fill` and ``group`` documents. In order to satisfy some
coding styles, these defaults can be overriden on a case by case basis in
templates. To achieve this, the following templates accept the optional
``bubbleUpLeadingTrivias`` and ``bubbleUpTrailingTrivias`` boolean entries to
override the default behavior for trivias bubbling up.

* ``align``
* ``continuationLineIndent``
* ``dedent``
* ``dedentToRoot``
* ``fill``
* ``group``
* ``indent``
* ``innerRoot``
* ``markAsRoot``

For instance:

.. code-block:: json

   {
     "kind": "group",
     "contents": "recurse",
     "bubbleUpLeadingTrivias": false
   }
   {
     "kind": "fill",
     "contents": "recurse",
     "bubbleUpTrailingTrivias": true
   }
   {
     "kind": "dedent",
     "contents": "recurse",
     "bubbleUpLeadingTrivias": false,
     "bubbleUpTrailingTrivias": true
   }
