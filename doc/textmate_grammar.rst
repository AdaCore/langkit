***************************
TextMate Grammar Generation
***************************

Langkit can generate TextMate grammars from language specifications, providing
a (quite free) syntactic coloration in editors that support this format.

To enable the TextMate grammar generation, you have to provide a configuration
file for it in the ``langkit.yaml`` file, following this example:

.. code-block:: yaml

    lkt_spec:
      entry_point: foo.lkt
    library:
      language_name: Foo
    textmate_config_file: my_textmate_config.json

The provided file is resolved relatively to the ``extensions`` directory.


Configuration files
===================

TextMate configuration files are JSON files that provide information about
association between TextMate scopes and tokens defined in the language
specification.


Top level
---------

The top level of configuration files must be a JSON object mapping
**TextMate scopes** to their corresponding **matcher**:

.. code-block:: json

    {
        "scope.first": "<matcher_1>",
        "scope.second": "<matcher_2>"
    }

You can find a documentation about predefined TextMate scope following at
https://macromates.com/manual/en/language_grammars#naming_conventions.

Matching order is defined by this top level object, meaning that scope
declaration order is significant. Example:

.. code-block:: json

    {
        "scope_1": "(hello)|(world)",
        "scope_2": "hello"
    }

In this example the ``scope_2`` is never matched because ``scope_1`` is
covering it and is declared before.


Matchers
--------

Matchers are JSON values that are used to match specific part of the document
and associate them to a scope. There are several kinds of matcher that may be
combined:

Regex matcher
^^^^^^^^^^^^^

A JSON string that starts with ``/`` that matches the regular expression
after it.

*Example: This matches the "[abc]" regex token.*

.. code-block:: json

    "/[abc]"


Token matcher
^^^^^^^^^^^^^

A JSON string starting with ``@`` followed by the name of the token to match.
This name must be exactly the same as the one defined in the language
specification.

*Example: This matches all parts of the document that match the regex defining
the "MyToken" token.*

.. code-block:: json

    "@MyToken"

.. important::

    A common pattern in TextMate grammars is to surround matched words with
    word boundaries ( ``\b`` ). You can express it on token matchers with this
    shorthand syntax:

    .. code-block:: json

        "\\b@MyToken\\b"


List matcher
^^^^^^^^^^^^

A JSON list that matches one of the matchers inside of it.

*Example: This matches either matcher_1 or matcher_2.*

.. code-block:: json

    ["<matcher_1>", "<matcher_2>"]


Combination matcher
^^^^^^^^^^^^^^^^^^^

A JSON object defining a combination of other matchers. It must define a
``"combination"`` field that contains either ``"and"`` or ``"or"``. This field
describes the way submatchers are going to be combined together. It must also
define a ``"matchers"`` field that contains the array of matchers to combine.

*Example: This matches all parts of the document that are a sequence of
matcher_1 THEN matcher_2.*

.. code-block:: json

    {
        "combination": "and",
        "matchers": ["<matcher_1>", "<matcher_2>"]
    }

*Example: This matches all parts of the document that are either matcher_1 OR
matcher_2.*

.. code-block:: json

    {
        "combination": "or",
        "matchers": ["<matcher_1>", "<matcher_2>"]
    }

You may nest multiple combination matchers to express a more complex matching
logic:

.. code-block:: json

    {
        "combination": "and",
        "matchers": [
            "<matcher_1>",
            {
                "combination": "or",
                "matchers": ["<matcher_2>", "<matcher_3>"]
            }
        ]
    }
