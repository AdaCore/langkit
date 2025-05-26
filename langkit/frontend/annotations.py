from __future__ import annotations

import dataclasses
from typing import Any, ClassVar, Type, TypeVar

from langkit.compile_context import CompileCtx
from langkit.diagnostics import check_source_language, error
from langkit.frontend.scopes import Scope
from langkit.frontend.static import parse_static_str

import liblktlang as L


# List of annotations that we don't compute here but that we can safely ignore
ANNOTATIONS_WHITELIST = ["builtin"]


class AnnotationSpec:
    """
    Synthetic description of how a declaration annotation works.
    """

    def __init__(
        self,
        name: str,
        unique: bool,
        require_args: bool,
        default_value: Any = None,
    ):
        """
        :param name: Name of the annotation (``foo`` for the ``@foo``
            annotation).
        :param unique: Whether this annotation can appear at most once for a
            given declaration.
        :param require_args: Whether this annotation requires arguments.
        :param default_value: For unique annotations, value to use in case the
            annotation is absent.
        """
        self.name = name
        self.unique = unique
        self.require_args = require_args
        self.default_value = default_value if unique else []

    def interpret(
        self,
        ctx: CompileCtx,
        annotation: L.DeclAnnotation,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        """
        Subclasses must override this in order to interpret an annotation.

        This method must validate and interpret ``args`` and ``kwargs``, and
        return a value suitable for annotations processing.

        :param annotation: Annotation declaration that is interpreted.
        :param args: Positional arguments for the annotation.
        :param kwargs: Keyword arguments for the annotation.
        :param scope: Scope to use when resolving entities mentionned in the
            annotation's arguments.
        """
        raise NotImplementedError

    def parse_single_annotation(
        self,
        ctx: CompileCtx,
        result: dict[str, Any],
        annotation: L.DeclAnnotation,
        scope: Scope,
    ) -> None:
        """
        Parse an annotation node according to this spec. Add the result to
        ``result``.
        """
        check_source_language(
            self.name not in result or not self.unique,
            "This annotation cannot appear multiple times",
            location=annotation,
        )

        # Check that parameters presence comply to the spec
        if not annotation.f_args:
            check_source_language(
                not self.require_args,
                "Arguments required for this annotation",
                location=annotation.f_name,
            )
            value = self.interpret(ctx, annotation, [], {}, scope)
        else:
            check_source_language(
                self.require_args,
                "This annotation accepts no argument",
                location=annotation.f_name,
            )

            # Collect positional and named arguments
            args = []
            kwargs = {}
            for arg in annotation.f_args.f_args:
                if arg.f_name:
                    name = arg.f_name.text
                    check_source_language(
                        name not in kwargs,
                        "Named argument repeated",
                        location=arg.f_name,
                    )
                    kwargs[name] = arg.f_value

                else:
                    check_source_language(
                        not kwargs,
                        "Positional arguments must appear before named ones",
                        location=arg,
                    )
                    args.append(arg.f_value)

            # Evaluate this annotation
            value = self.interpret(ctx, annotation, args, kwargs, scope)

        # Store annotation evaluation into the result
        if self.unique:
            result[self.name] = value
        else:
            result.setdefault(self.name, [])
            result[self.name].append(value)


class FlagAnnotationSpec(AnnotationSpec):
    """
    Convenience subclass for flags.
    """

    def __init__(self, name: str):
        super().__init__(
            name, unique=True, require_args=False, default_value=False
        )

    def interpret(
        self,
        ctx: CompileCtx,
        annotation: L.DeclAnnotation,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        return True


class StringLiteralAnnotationSpec(AnnotationSpec):
    """
    Convenience subclass for annotations that take a string literal.
    """

    def __init__(self, name: str):
        super().__init__(
            name, unique=True, require_args=True, default_value=None
        )

    def interpret(
        self,
        ctx: CompileCtx,
        annotation: L.DeclAnnotation,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        if len(args) != 1 or kwargs:
            error(
                "exactly one position argument expected: a static string",
                location=annotation.f_name,
            )
        return parse_static_str(ctx, args[0])


@dataclasses.dataclass
class ParsedAnnotations:
    """
    Namespace object to hold annotation parsed values.
    """

    syn_annotations: dict[str, L.DeclAnnotation]
    """
    Mapping of annotation names to the corresponding syntactic annotation node
    (for the annotations that are present), or to the unit root node (for the
    annotations that are absent, useful to get diagnostic emitting code
    straightforward).
    """

    annotations: ClassVar[list[AnnotationSpec]]


def check_no_annotations(full_decl: L.FullDecl | L.DeclAnnotationList) -> None:
    """
    Check that the declaration has no annotation.
    """
    annotations = (
        full_decl
        if isinstance(full_decl, L.DeclAnnotationList)
        else full_decl.f_decl_annotations
    )
    check_source_language(
        len(annotations) == 0, "No annotation allowed", location=annotations
    )


AnyPA = TypeVar("AnyPA", bound=ParsedAnnotations)


def parse_annotations(
    ctx: CompileCtx,
    annotation_class: Type[AnyPA],
    full_decl: L.FullDecl | L.DeclAnnotationList,
    scope: Scope,
) -> AnyPA:
    """
    Parse annotations according to the specs in
    ``annotation_class.annotations``. Return a ParsedAnnotations that contains
    the interpreted annotation values for each present annotation.

    :param annotation_class: ParsedAnnotations subclass for the result, holding
        the annotation specs to guide parsing.
    :param full_decl: Declaration whose annotations are to be parsed, or the
        annotations themselves.
    :param scope: Scope to use when resolving entities mentionned in the
        annotation's arguments.
    """
    syn_annotations = {
        s.name: full_decl.unit.root for s in annotation_class.annotations
    }

    # Build a mapping for all specs
    specs_map: dict[str, AnnotationSpec] = {}
    for s in annotation_class.annotations:
        assert s.name not in specs_map
        specs_map[s.name] = s

    # Process annotations
    annotations = (
        full_decl
        if isinstance(full_decl, L.DeclAnnotationList)
        else full_decl.f_decl_annotations
    )
    values: dict[str, Any] = {}
    for a in annotations:
        name = a.f_name.text
        spec = specs_map.get(name)
        if spec is not None:
            spec.parse_single_annotation(ctx, values, a, scope)
        elif name not in ANNOTATIONS_WHITELIST:
            error(f"Invalid annotation: {name}", location=a)
        syn_annotations[name] = a

    # Use the default value for absent annotations
    for s in annotation_class.annotations:
        values.setdefault(s.name, s.default_value)

    # Create the namespace object to hold results
    return annotation_class(
        syn_annotations=syn_annotations, **values  # type: ignore
    )
