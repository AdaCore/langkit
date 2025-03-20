from __future__ import annotations

import pathlib

from langkit.parsers import (
    Defer,
    List,
    Opt,
    Or,
    Parser,
    _Extract,
    _Row,
    _Token,
    _Transform,
)


def emit_railroad_diagram(parser: Parser) -> None:
    """
    Pass to emit railroad diagrams using the railroad_diagrams library.

    Railroads will be emitted in $BUILD/railroad-diagrams.
    """
    from railroad import (
        Choice,
        Diagram,
        DiagramItem,
        End,
        OneOrMore,
        Optional,
        Sequence,
        Skip,
        Start,
        ZeroOrMore,
    )

    def recurse(p: Parser) -> DiagramItem | str | None:

        # Transform parsers are just ignored
        if isinstance(p, _Transform):
            return recurse(p.children[0])

        elif isinstance(p, Opt):
            # Opt parsers are straightforwardly wrapped into an Optional
            return Optional(recurse(p.parser))

        elif isinstance(p, _Extract):
            # Extract is ignored
            return recurse(p.parser)

        # For list parsers, we create a sequence with the right separator
        # and sub-parser.
        elif isinstance(p, List):

            sep = recurse(p.sep) if p.sep else None
            child = recurse(p.parser)
            if p.empty_valid:
                return ZeroOrMore(child, repeat=sep)
            else:
                return OneOrMore(child, repeat=sep)

        # For defers, we just return the rule name
        elif isinstance(p, Defer):
            return p.rule_name

        # For tokens, we return either the quoted original string, or the DSL
        # name.
        elif isinstance(p, _Token):
            if p._original_string:
                return repr(p._original_string)
            else:
                return p.val.dsl_name

        children = []

        for c in p.children:
            res = recurse(c)
            if res is not None:
                children.append(res)

        if isinstance(p, Or):
            if len(children) == 0:
                return None

            children = sorted(children, key=lambda c: isinstance(c, Skip))
            return Choice(0, *children)

        elif isinstance(p, _Row):
            if len(children) == 0:
                return Skip()

            return Sequence(*children)

        else:
            return None

    d = Diagram(
        # Explicit start point with the parser's name as label
        Start("simple", label=parser.name.lower()),
        *[c for c in [recurse(parser)] if c is not None],
        End("simple"),
    )

    # Output the diagram to svg in $BUILD/railroad-diagrams/$RULENAME.svg
    emitter = parser.context.emitter
    assert emitter is not None
    out_dir = pathlib.Path(emitter.lib_root, "railroad-diagrams")
    out_dir.mkdir(parents=True, exist_ok=True)

    with (out_dir / f"{parser.name.lower()}.svg").open("w") as f:
        d.writeSvg(f.write)
