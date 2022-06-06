"""
Check that ``langkit.documentation`` documentation formatters output docstrings
as expected.
"""

from langkit import documentation as doc

from docutils.parsers.rst.roles import register_local_role


register_local_role("passthrough", doc.PassthroughNode.role_fn)

docstring = """
This is a docstring. This is a ``literal``. This is *emphasis*. This is
**strong emphasis**. This is a passthrough plugin role: :passthrough:`5.4`

* This is a bullet point with several lines. Viral schlitz humblebrag, shabby
  chic pitchfork selfies subway tile live-edge sartorial austin man bun.

* This is a second bullet point with
  several lines

.. attention:: This is a Sphinx admonition that spans several lines.
   Cold-pressed scenester lo-fi, cronut asymmetrical vegan kogi heirloom
   hashtag.

.. code-block:: ada

    procedure This_Is_Ada is
    begin
        Put_Line ("Woot");
    end This_Is_Ada;

This is a last paragraph which spans several lines. I'm baby truffaut salvia
VHS shaman, prism gastropub fingerstache forage::

    a = "This is a literal block"
    print(a)

    for c in a:
        print(c)

1. This is a numbered list woot woot
2. Second item

:param pouet: This is a last paragraph which spans several lines. I'm baby
    truffaut salvia VHS shaman, prism gastropub fingerstache forage. Tousled
    food truck typewriter.

:param foo: Foo foo foo foo
    foo

.. attention:: Let's test a nested block, by having an admonition that contains
   a bullet list:

   * This is a bullet point with several lines. Viral schlitz humblebrag,
     shabby chic pitchfork selfies subway tile live-edge sartorial austin man
     bun.

   * This is a second bullet point with
     several lines

   .. code-block:: python

       print("And now a code extract")
"""

print("Original docstring")
print("==================")
print(docstring)
print()

for lang_name, format_fn in (
    ("C", doc.format_c),
    ("Ada", doc.format_ada),
    ("Python", doc.format_python),
    ("Ocaml", doc.format_ocaml),
    ("Text", doc.format_text)
):
    header = f"Output for {lang_name}"
    print(header)
    print("=" * len(header))
    print()
    print(f"        {format_fn(docstring, 8)}")
    print()
