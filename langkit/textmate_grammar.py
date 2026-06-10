from __future__ import annotations

from abc import abstractmethod
from dataclasses import dataclass
import enum
import json
import os
import re
from typing import Any, NoReturn, TYPE_CHECKING

from langkit.diagnostics import Location, error
from langkit.language_api import AbstractAPISettings
from langkit.lexer import Name, TokenAction


if TYPE_CHECKING:
    from langkit.compile_context import CompileCtx


class Matcher:
    """
    Base class for all matchers declared in a TextMate configuration file.
    """

    @abstractmethod
    def to_regex(self) -> str: ...


class RegexMatcher(Matcher):
    """
    Matcher based on a regular expression.
    """

    regex: str

    def __init__(self, regex: str):
        self.regex = regex

    def to_regex(self) -> str:
        return self.regex


class CombinationKind(enum.StrEnum):
    """
    All valid kind for combination matchers.
    """

    AND = "and"
    OR = "or"


class CombinationMatcher(Matcher):
    """
    Matcher that is the result of other matchers combination.
    """

    kind: CombinationKind
    """
    The way this matcher is gonna combine its submatchers.
    """

    submatchers: list[Matcher]
    """
    Submatchers composing the matcher.
    """

    def __init__(self, kind: CombinationKind, submatchers: list[Matcher]):
        self.kind = kind
        self.submatchers = submatchers

    def to_regex(self) -> str:
        sub_regex = [f"({m.to_regex()})" for m in self.submatchers]
        return ("|" if self.kind == "or" else "").join(sub_regex)


@dataclass
class TextMateRule:
    """
    This class represents a TextMate rule. It contains information required to
    emit a TextMate rule JSON object. A rule is used to match a part of the
    document and include it in a scope, this scope is later used to perform
    syntactic coloration on the document.
    """

    identifier: str
    """
    Identifier of the TextMate rule.
    """

    scope: str
    """
    Name of the scope to apply to document part(s) matched by this rule.
    """

    pattern: str
    """
    Regular expression for the rule to match.
    """

    def render(self) -> str:
        """
        Render this TextMate rule as a JSON object.
        """
        # Create a dictionary with all fields
        obj = {
            "name": self.scope,
            "match": self.pattern,
        }

        # Then dump it as JSON
        return json.dumps(obj)


class TextMateGrammarSettings(AbstractAPISettings):
    """
    Container for the TextMate grammar generation settings.
    """

    named_pattern_matcher = re.compile(r"\{[a-zA-Z][a-zA-Z0-9_]*\}")
    """
    Regular expression required to match on named pattern inside token regular
    expressions.
    """

    def __init__(self, ctx: CompileCtx) -> None:
        self.context = ctx
        self.all_rules: list[TextMateRule] | None = None

    def load_textmate_config(self, ctx: CompileCtx) -> None:
        """
        Load the TextMate configuration provided to ``ctx``, perform all
        verifications about it and store the result in this TextMate grammar
        settings instance.
        """
        # Requirements to load the textmate configuration
        assert self.context == ctx
        assert ctx.lexer is not None

        # Now parse the configuration file
        try:
            with open(self.config_file, "r") as f:
                config = json.load(f)
        except OSError as e:
            error(
                f"Cannot open the TextMate config file: {e}",
                Location.nowhere,
            )
        except json.JSONDecodeError as e:
            error(
                e.msg,
                Location(
                    self.config_file,
                    e.lineno,
                    e.colno,
                ),
            )

        # Then, load the configuration as TextMate rules
        rule_map: dict[str, TextMateRule] = {}
        for scope, matcher_source in config.items():
            matcher = self.to_matcher(matcher_source)
            if matcher is not None:
                rule = TextMateRule(
                    identifier=scope,
                    scope=scope,
                    pattern=matcher.to_regex(),
                )
                rule_map[rule.identifier] = rule

        # Finally, place the loading result in the compilation context
        self.all_rules = list(rule_map.values())

    @property
    def config_file(self) -> str:
        """
        Full path to the TextMate grammar configuration file.
        """
        assert self.context.config.textmate_config_file is not None
        return os.path.join(
            self.context.extensions_dir,
            self.context.config.textmate_config_file,
        )

    def error_in_config(self, msg: str) -> NoReturn:
        """
        Emit an error located in the TextMate configuration file.
        """
        error(msg, Location(self.config_file))

    def to_matcher(self, val: Any) -> Matcher:
        """
        Create a matcher from the provided value, emitting an error if this
        is not possible.
        """
        match val:
            case str() as s:
                if s.startswith("@") or s.startswith("\\b@"):
                    start, bound = (
                        (3, "\\b") if s.startswith("\\b@") else (1, "")
                    )
                    end = len(s) - len(bound)
                    return RegexMatcher(
                        f"{bound}{self.token_regex(s[start:end])}{bound}"
                    )
                elif s.startswith("/"):
                    return RegexMatcher(s[1:])
                else:
                    self.error_in_config(
                        f'Cannot create a matcher from the string "{s}" (it'
                        ' should starts either with "@" or "/")'
                    )

            case list() as l:
                return CombinationMatcher(
                    CombinationKind.OR,
                    [self.to_matcher(m) for m in l],
                )

            case dict() as d:
                # Fetch and check the combination kind
                kind_str = self.get_field(d, "combination", str)
                try:
                    kind = CombinationKind(kind_str)
                except ValueError:
                    self.error_in_config(
                        f"Invalid combination kind: {kind_str}"
                    )

                # Then create the combination matcher
                return CombinationMatcher(
                    kind,
                    [
                        self.to_matcher(s)
                        for s in self.get_field(d, "matchers", list)
                    ],
                )

        self.error_in_config(f"Invalid matcher source: {val}")

    def token_regex(self, token_name: str) -> str:
        """
        Get the regular expression that defines the token described by the
        provided ``token_name``. Emit an error if the provided name doesn't
        lead to an existing token.
        """
        try:
            name = Name.from_camel(token_name)
        except ValueError as e:
            self.error_in_config(str(e))

        # Ensure the token exists
        if name not in self.context.lexer.tokens_set:
            self.error_in_config(f'Unknown token name "{token_name}"')

        # Get the rule corresponding to the provided name
        rule = [
            r
            for r in self.context.lexer.rules
            if isinstance(r.action, TokenAction) and r.action.name == name
        ][0]

        # Perform named pattern replacement in the rule matcher
        return self.process_named_patterns(rule.matcher.regexp)

    def process_named_patterns(self, source_regex: str) -> str:
        """
        Replace all named patterns inside ``source_regex`` by their actual
        value.
        """
        return self.named_pattern_matcher.sub(
            lambda s: [
                self.process_named_patterns(p[1])
                for p in self.context.lexer.patterns
                if p[0] == s.group()[1:-1]
            ][0],
            source_regex,
        )

    def get_field(
        self,
        obj: dict[str, Any],
        field: str,
        expected_type: type,
        default: Any | None = None,
    ) -> Any:
        """
        Try getting ``field`` in the provided ``obj`` value. If its is not
        present and the provided ``default`` is not None, then return the
        default value, otherwise emit an error.

        Finally, this function checks that the fetched value is an instance of
        ``expected_type``, emit an error if not.
        """
        try:
            res = obj[field]
        except KeyError:
            self.error_in_config(f'Cannot find the "{field}" field in {obj}')
        if isinstance(res, expected_type):
            return res
        else:
            self.error_in_config(
                f'"{field}" is a {type(res).__name__}, expecting '
                + expected_type.__name__
            )
