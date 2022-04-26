"""
Check that ``langkit.documentation`` documentation formatters output docstrings
as expected.
"""

from langkit import documentation as doc


docstring = """
This is a docstring

* This is a bullet point with several lines. Viral schlitz humblebrag, shabby
  chic pitchfork selfies subway tile live-edge sartorial austin man bun.

* This is a second bullet point with
  several lines

.. attention:: This is a Sphinx admonition that spans several lines.
   Cold-pressed scenester lo-fi, cronut asymmetrical vegan kogi heirloom
   hashtag. 8-bit gluten-free bushwick try-hard taiyaki readymade chartreuse
   disrupt iceland DIY.

This is a last paragraph which spans several lines. I'm baby truffaut salvia
VHS shaman, prism gastropub fingerstache forage. Tousled food truck typewriter,
brunch bushwick dreamcatcher stumptown jianbing. Raw denim cronut kombucha.
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
