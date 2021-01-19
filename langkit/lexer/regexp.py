from __future__ import annotations

from collections import defaultdict
from contextlib import contextmanager
import itertools
import re
from typing import (Any, Callable, Dict, Iterable, Iterator, List, Optional,
                    Set, TYPE_CHECKING, Tuple, TypeVar)


from langkit.diagnostics import check_source_language
from langkit.lexer.char_set import CharSet
from langkit.lexer.unicode_data import unicode_categories_char_sets


if TYPE_CHECKING:
    from langkit.lexer import RuleAssoc


rule_name_re = re.compile('[a-zA-Z][a-zA-Z0-9_]*')
del unicode_categories_char_sets


T = TypeVar("T")
U = TypeVar("U")


def _to_dot(starting_state: T,
            get_transitions: Callable[[T], List[Tuple[U, T]]],
            get_state_label: Callable[[T], Optional[str]]) -> str:
    """
    Helper to emit a dot(1) file representing a states graph.

    Return the content of a dot(1) file as a string.

    :param starting_state: Initial state for the graph.

    :param get_transitions: Function that returns for a given state the list of
        transitions from it. Each transition is a couple: (transition label,
        next state).

    :param get_state_label: Function that returns an optional label for a
        state.
    """
    id_generator = iter(itertools.count(0))
    ids: Dict[T, int] = {}

    nodes: List[str] = []
    edges: List[str] = []

    def add_node(n: T) -> int:
        try:
            return ids[n]
        except KeyError:
            pass

        id = next(id_generator)
        ids[n] = id

        for label, next_state in get_transitions(n):
            next_id = add_node(next_state)
            edges.append('{} -> {} [label="{}"];'.format(
                id, next_id, label))

        node_label = get_state_label(n)
        nodes.append('{} [label="{}"];'.format(
            id, '{} {}'.format(id, node_label) if node_label else id
        ))
        return id

    add_node(starting_state)
    return '\n'.join(['digraph g {'] + nodes + edges + ['}'])


class SequenceReader:
    def __init__(self, sequence: str):
        self.sequence = sequence
        self.index = 0

    def next_is(self, *items: str) -> bool:
        return not self.eof and self.peek() in items

    def peek(self) -> str:
        assert not self.eof
        return self.sequence[self.index]

    def read(self) -> str:
        assert not self.eof
        result = self.peek()
        self.index += 1
        return result

    def go_back(self) -> None:
        assert self.index >= 0
        self.index -= 1

    @property
    def eof(self) -> bool:
        return self.index == len(self.sequence)


class RegexpCollection:

    class Parser:
        """Base class for regexp components."""

        def to_nfa(self,
                   regexps: RegexpCollection) -> Tuple[NFAState, NFAState]:
            """
            Turn this parser into a NFA.

            :param regexps: Collection of named patterns that `self` can
                reference.
            """
            raise NotImplementedError()

    class Sequence(Parser):
        """Consume input that sub-parsers can consume sequentially."""

        def __init__(self, subparsers: List[RegexpCollection.Parser]):
            self.subparsers = subparsers

        def to_nfa(self,
                   regexps: RegexpCollection) -> Tuple[NFAState, NFAState]:
            # If this sequences matches only the empty input, just return a
            # single state.
            if not self.subparsers:
                single = NFAState()
                return (single, single)

            # Otherwise, create NFAs for subparsers and return them as a chain
            sub_nfas = [s.to_nfa(regexps) for s in self.subparsers]
            for i in range(len(sub_nfas) - 1):
                sub_nfas[i][1].add_transition(None, sub_nfas[i + 1][0])
            return (sub_nfas[0][0], sub_nfas[-1][1])

        def __repr__(self) -> str:
            return 'Seq({})'.format(', '.join(
                repr(s) for s in self.subparsers))

    class Repeat(Parser):
        """Accept input running the sub-parser multiple times."""

        def __init__(self, subparser: RegexpCollection.Parser):
            self.subparser = subparser

        def to_nfa(self,
                   regexps: RegexpCollection) -> Tuple[NFAState, NFAState]:
            # Get the subparser NFA and connect its ends to match repetitions
            nfa = self.subparser.to_nfa(regexps)
            nfa[1].add_transition(None, nfa[0])

            # The new ending NFA state is the first one from the subparser so
            # that the NFA accepts empty inputs.
            return (nfa[0], nfa[0])

        def __repr__(self) -> str:
            return 'Repeat({})'.format(self.subparser)

    class Or(Parser):
        """Accept input that at least one sub-parser accepts."""

        def __init__(self, subparsers: List[RegexpCollection.Parser]):
            self.subparsers = subparsers

        def to_nfa(self,
                   regexps: RegexpCollection) -> Tuple[NFAState, NFAState]:
            starting = NFAState()
            ending = NFAState()

            # Branch on empty input from our starting to the starting of all
            # subparsers, and branch on empty input from the ending of all
            # subparsers to our ending.
            for s in self.subparsers:
                sub_s, sub_e = s.to_nfa(regexps)
                starting.add_transition(None, sub_s)
                sub_e.add_transition(None, ending)

            return (starting, ending)

        def __repr__(self) -> str:
            return 'Or({})'.format(', '.join(
                repr(s) for s in self.subparsers))

    class Opt(Parser):
        """Accept input that the sub-parser accepts, or the empty input."""

        def __init__(self, subparser: RegexpCollection.Parser):
            self.subparser = subparser

        def to_nfa(self,
                   regexps: RegexpCollection) -> Tuple[NFAState, NFAState]:
            starting, ending = self.subparser.to_nfa(regexps)
            starting.add_transition(None, ending)
            return (starting, ending)

        def __repr__(self) -> str:
            return 'Opt({})'.format(self.subparser)

    class Range(Parser):
        """Accept any character in the given set."""

        def __init__(self, char_set: CharSet):
            self.char_set = char_set

        def to_nfa(self,
                   regexps: RegexpCollection) -> Tuple[NFAState, NFAState]:
            starting = NFAState()
            ending = NFAState()
            starting.add_transition(self.char_set, ending)
            return (starting, ending)

        def __repr__(self) -> str:
            return repr(self.char_set)

    class Defer(Parser):
        """Defer parsing to a named regexp."""

        def __init__(self, name: str):
            self.name = name

        def to_nfa(self,
                   regexps: RegexpCollection) -> Tuple[NFAState, NFAState]:
            with regexps._visit_rule(self.name):
                try:
                    parser = regexps.patterns[self.name]
                except KeyError:
                    check_source_language(
                        False, 'unknown pattern: {}'.format(self.name))
                return parser.to_nfa(regexps)

        def __repr__(self) -> str:
            return '@{}'.format(self.name)

    escape_chars = {
        '(': '(', ')': ')',
        '[': '[', ']': ']',
        '|': '|', '+': '+', '*': '*', '\\': '\\',
        'a': '\a', 'b': '\b', 'B': '\\', 'e': '\033', 'f': '\f',
        'n': '\n', 'r': '\r', 't': '\t',
        '0': '\0',
    }

    def __init__(self) -> None:
        self.patterns: Dict[str, RegexpCollection.Parser] = {}
        self._visiting_patterns: Set[str] = set()

    def _parse(self, regexp: str) -> RegexpCollection.Parser:
        """
        Parse `regexp` as regular expression string.
        """
        stream = SequenceReader(regexp)
        root = self._parse_or(stream, toplevel=True)
        assert stream.eof

        return root

    def add_pattern(self, name: str, regexp: str) -> None:
        """
        Add a named pattern to this collection.

        :param name: Name for the pattern.
        :param regexp: Regular expression string for the pattern.
        """
        # Register the parser for regexp
        assert name not in self.patterns
        self.patterns[name] = self._parse(regexp)

    def nfa_for(self, regexp: str) -> Tuple[NFAState, NFAState]:
        """
        Parse the given regular expression string and return a NFA for it.

        :param regexp: Regular expression to parse.
        """
        return self._parse(regexp).to_nfa(self)

    @contextmanager
    def _visit_rule(self, rule_name: str) -> Iterator[None]:
        """
        Context manager to report cycles in named pattern references.

        When trying to resolve a reference to a named pattern, use this context
        manager: it will emit an error if the caller, or one of its caller
        (recursively) is already trying to reference this named pattern (can
        occur only if there is a cycle, which is illegal).

        :param rule_name: Name of the referenced pattern.
        """
        check_source_language(
            rule_name not in self._visiting_patterns,
            'infinite recursion in {}'.format(rule_name))
        self._visiting_patterns.add(rule_name)
        yield
        self._visiting_patterns.remove(rule_name)

    def _read_escape(self, stream: SequenceReader) -> int:
        """
        Read an escaped character. Return the ordinal for the character that is
        meant.

        :param stream: Input regexp stream.
        """
        assert stream.read() == '\\'
        check_source_language(not stream.eof, 'bogus escape')
        char = stream.read()

        # If this encodes a Unicode code point, read the characters and turn
        # them into a Unicode character.
        codepoint_chars_count = {'u': 4, 'U': 8}.get(char, None)
        if codepoint_chars_count is not None:
            codepoint = 0
            for i in range(codepoint_chars_count):
                check_source_language(not stream.eof, 'bogus Unicode escape')
                char = stream.read().lower()
                if '0' <= char <= '9':
                    digit = ord(char) - ord('0')
                elif 'a' <= char <= 'f':
                    digit = ord(char) - ord('a') + 0xa
                else:
                    check_source_language(
                        False, 'invalid Unicode escape sequence')
                codepoint = codepoint * 16 + digit
            return codepoint

        return ord(self.escape_chars.get(char, char))

    def _parse_or(self,
                  stream: SequenceReader,
                  toplevel: bool = False) -> RegexpCollection.Parser:
        """
        Read a sequence of alternatives. Stop at EOF or at the first unmatched
        parenthesis.

        :param stream: Input regexp stream.
        :param toplevel: If true, reject unmatched parentheses. Otherwise
            accept them.
        """
        subparsers = []
        while True:
            subparsers.append(self._parse_sequence(stream))
            if stream.eof:
                break
            elif stream.next_is(')'):
                check_source_language(not toplevel, 'unbalanced parentheses')
                break
            else:
                assert stream.read() == '|'
        return self.Or(subparsers)

    def _parse_sequence(self,
                        stream: SequenceReader) -> RegexpCollection.Parser:
        """
        Parse a sequence of regexps. Stop at the first unmatched parenthesis or
        at the first top-level pipe character.

        :param stream: Input regexp stream.
        """
        subparsers = []
        while True:
            if stream.eof or stream.next_is('|', ')'):
                break

            elif stream.next_is('('):
                # Nested group: recursively parse alternatives
                stream.read()
                subparsers.append(self._parse_or(stream))
                check_source_language(stream.next_is(')'),
                                      'unbalanced parenthesis')
                stream.read()

            elif stream.next_is('['):
                # Parse a range of characters
                subparsers.append(self._parse_range(stream))

            elif stream.next_is('{'):
                # Parse a reference to a named pattern
                stream.read()
                name = ''
                while not stream.eof and not stream.next_is('}'):
                    name += stream.read()
                check_source_language(stream.next_is('}'),
                                      'unbalanced bracket')
                stream.read()
                check_source_language(rule_name_re.match(name) is not None,
                                      'invalid rule name: {}'.format(name))
                subparsers.append(self.Defer(name))

            elif stream.next_is('*', '+', '?'):
                # Repeat the previous sequence item
                check_source_language(bool(subparsers), 'nothing to repeat')
                check_source_language(
                    not isinstance(subparsers[-1], self.Repeat),
                    'multiple repeat')
                wrapper = {
                    '*': lambda p: self.Repeat(p),
                    '+': lambda p: self.Sequence([p, self.Repeat(p)]),
                    '?': lambda p: self.Opt(p)
                }[stream.read()]
                subparsers[-1] = wrapper(subparsers[-1])

            elif stream.next_is('.'):
                # Generally, "." designates any character *except* newlines. Do
                # the same here.
                stream.read()
                subparsers.append(self.Range(CharSet('\n').negation))

            elif stream.next_is('^', '$'):
                check_source_language(
                    False, 'matching beginning or ending is unsupported')

            elif stream.next_is('\\'):
                # Parse an escape sequence. In can be a Unicode character, a
                # Unicode property or a simple escape sequence.
                stream.read()

                # \p and \P refer to character sets from Unicode general
                # categories.
                if stream.next_is('p', 'P'):
                    action = stream.read()

                    # Read the category name, which must appear between curly
                    # brackets.
                    category = ''
                    check_source_language(
                        stream.next_is('{'),
                        'incomplete Unicode category matcher')
                    stream.read()
                    while not stream.eof and not stream.next_is('}'):
                        category += stream.read()
                    check_source_language(
                        stream.next_is('}'),
                        'incomplete Unicode category matcher')
                    stream.read()

                    try:
                        char_set = CharSet.for_category(category)
                    except KeyError:
                        check_source_language(
                            False,
                            'invalid Unicode category: {}'.format(category))
                    if action == 'P':
                        char_set = char_set.negation
                    subparsers.append(self.Range(char_set))

                else:
                    stream.go_back()
                    subparsers.append(
                        self.Range(
                            CharSet.from_int(self._read_escape(stream))
                        )
                    )

            else:
                subparsers.append(self.Range(CharSet(stream.read())))

        return self.Sequence(subparsers)

    def _parse_range(self, stream: SequenceReader) -> RegexpCollection.Parser:
        """
        Parse a regular expression for a character range.

        :param file stream: Input regexp stream.
        :rtype: RegexpCollection.Parser
        """
        assert stream.read() == '['
        ranges: List[Tuple[int, int]] = []

        # First, determine if this range must be negated
        negate = False
        if stream.next_is('^'):
            negate = True
            stream.read()

        # Now, read ranges...
        #
        # TODO: handle '-' and ']' in first position.
        in_range = False
        while not stream.eof and not stream.next_is(']'):
            if stream.next_is('-'):
                check_source_language(bool(ranges and not in_range),
                                      'dangling dash')
                in_range = True
                stream.read()
            else:
                char = (self._read_escape(stream)
                        if stream.next_is('\\') else ord(stream.read()))
                if in_range:
                    low, high = ranges.pop()
                    assert low == high
                    ranges.append((low, char))
                else:
                    ranges.append((char, char))
                in_range = False

        check_source_language(not in_range, 'dangling dash')
        check_source_language(stream.next_is(']'),
                              'unbalanced square bracket')
        assert stream.read() == ']'

        char_set = CharSet.from_int_ranges(*ranges)
        if negate:
            char_set = char_set.negation
        return self.Range(char_set)


class NFAState:
    """
    Single state in a non-deterministic finite state machine.
    """

    # Generator of unique IDs (integers) for NFAState instances
    _id_generator = itertools.count(0)

    def __init__(self) -> None:
        self._id = next(self._id_generator)

        self.label: Any = None
        """
        Annotation for this node. If non-None, this annotation is propagated to
        the corresponding DFA states.
        """

        self.transitions: List[Tuple[Optional[CharSet], NFAState]] = []
        """
        List of associations between character sets and other states.

        None means that no character is needed to do the transition.
        Because this is a NFA, the various character sets can overlap.

        :type: list[(None|CharSet, NFAState)]
        """

    def __lt__(self, other: NFAState) -> bool:
        assert isinstance(other, NFAState)
        return self._id < other._id

    def add_transition(self,
                       chars: Optional[CharSet],
                       next_state: NFAState) -> None:
        """
        Add a transition from this state to another one.

        :param chars: Specification of the input that allows to transition from
            this state to the next one. A CharSet instance indicates that one
            character in this set is required. None indicates that the empty
            input allows to transition.
        :param next_state: Destination state for this new transition.
        """
        assert chars is None or isinstance(chars, CharSet)
        assert isinstance(next_state, NFAState)
        self.transitions.append((chars, next_state))

    @staticmethod
    def follow_spontaneous_transitions(
        states: Iterable[NFAState]
    ) -> Set[NFAState]:
        """
        Return the set of states that can be reached from the given set of
        states following spontaneous transitions.

        :param states: List of starting states.
        """
        result: Set[NFAState] = set()

        def process(state: NFAState) -> None:
            if state in result:
                return
            result.add(state)
            for chars, next_state in state.transitions:
                if chars is None:
                    process(next_state)

        for state in states:
            process(state)
        return result

    @staticmethod
    def reachable_nonspontaneous_transitions(
        states: Tuple[NFAState, ...]
    ) -> List[Tuple[CharSet, NFAState]]:
        """
        Return the list of non-spontaneous transitions that can be reached from
        the given set of states.

        Computing this is valid if following any spontaneous transition from
        one given state leads to a state that is also in ``states``, i.e. if
        ``follow_spontaneous_transitions(states) == states``.

        :param states: List of starting states.
        """
        result: List[Tuple[CharSet, NFAState]] = []
        for state in states:
            for chars, next_state in state.transitions:
                if chars is None:
                    assert next_state in states
                else:
                    result.append((chars, next_state))
        return result

    @staticmethod
    def hashable_state_set(states: Set[NFAState]) -> Tuple[NFAState, ...]:
        """
        Return a value that can be used as a dict key/set element to represent
        a set of states.
        """
        return tuple(sorted(states))

    @staticmethod
    def deterministic_transitions(
        states: Tuple[NFAState, ...]
    ) -> Dict[Tuple[NFAState, ...], CharSet]:
        """
        Return the set of deterministic (non-spontaneous and disjoint)
        transitions that leave the "states" sub-graph.

        This is a major helper in the convertion of NFAs into corresponding
        DFAs.

        The result is a mapping from sets of NFA states (destination of
        deterministic transitions) to disjoint character sets (label for
        transitions).

        :param states: Set of states from which we compute transitions.
        """
        # First, collect for each reachable state the set of characters that
        # allow to reach that state.
        transitions: Dict[NFAState, CharSet] = {}
        for chars, next_state in NFAState.reachable_nonspontaneous_transitions(
            states
        ):
            assert chars is not None
            try:
                other_chars = transitions[next_state]
            except KeyError:
                transitions[next_state] = chars
            else:
                transitions[next_state] = other_chars | chars

        # Linearize the transition labels: flatten all character sets to have a
        # stream of "start range"/"end range" of transitions considering all
        # input characters.
        #
        # For instance, for the following transitions: {
        #    S1: [a:z],
        #    S2: [a:h, s],
        #    S3: [f:l]
        # }
        # we will get the following stream of events: {
        #    'a': Event(adding=S1;S2),
        #    'f': Event(adding=S3),
        #    'h': Event(removing=S2),
        #    'l': Event(removing=S3),
        #    's': Event(adding=S2,removing=S2),
        #    'z': Event(removing=S1),
        # }.
        class Event:
            def __init__(self) -> None:
                self.adding: Set[NFAState] = set()
                self.removing: Set[NFAState] = set()

        event_map: Dict[int, Event] = defaultdict(Event)
        for next_state, chars in transitions.items():
            for r in chars.ranges:
                event_map[r[0]].adding.add(next_state)
                event_map[r[1]].removing.add(next_state)
        events = sorted(event_map.items())

        # The final step is to compute the set of transitions for which
        # character sets are disjoint: just follow the stream of events.

        result: Dict[Tuple[NFAState, ...], CharSet] = defaultdict(CharSet)

        def add_transition(low: Optional[int],
                           high: int,
                           states: Iterable[NFAState]) -> None:
            if states:
                assert isinstance(low, int)
            else:
                return
            next_states = NFAState.follow_spontaneous_transitions(states)
            result[NFAState.hashable_state_set(next_states)].add_int_range(
                low, high
            )

        # Set of states "active" for the current position in the events stream
        active_states: Set[NFAState] = set()

        # Character for the last event we processed
        last_char: Optional[int] = None

        for char, event in events:
            assert event.adding or event.removing

            if event.adding:
                add_transition(last_char, char - 1, active_states)
                active_states = active_states.union(event.adding)
                last_char = char

            if event.removing:
                add_transition(last_char, char, active_states)
                active_states = active_states.difference(event.removing)
                last_char = char + 1

        return result

    def to_dfa(self) -> DFAState:
        """
        Return the conversion of this NFA into a DFA.

        :rtype: DFAState
        """
        result: Optional[DFAState] = None

        # Mapping from sets of NFAState nodes (see NFAState.hashable_state_set)
        # to the corresponding DFAState nodes.
        dfa_states: Dict[Tuple[NFAState, ...], DFAState] = {}

        # List of tuple[NFAState, CharSet and tuple[NFAState]] for the
        # transitions that constitute the DFA.
        transitions: List[Tuple[Tuple[NFAState, ...],
                                CharSet,
                                Tuple[NFAState, ...]]] = []

        queue = {
            self.hashable_state_set(
                self.follow_spontaneous_transitions([self])
            )
        }
        while queue:
            states = queue.pop()
            if states in dfa_states:
                # TODO: unreachable?
                raise RuntimeError()

            new_node = DFAState(labels={s.label for s in states
                                        if s.label is not None})
            dfa_states[states] = new_node
            if result is None:
                result = new_node

            for next_states, char_set in self.deterministic_transitions(
                states
            ).items():
                if next_states not in dfa_states:
                    queue.add(next_states)
                transitions.append((states, char_set, next_states))

        for states, char_set, next_states in transitions:
            dfa_states[states].add_transition(char_set,
                                              dfa_states[next_states])

        assert result
        return result

    def to_dot(self) -> str:
        """
        Return a dot script representing this NFA.
        """
        return _to_dot(self, lambda s: s.transitions, lambda s: s.label)


class DFAState:
    """
    Single state in a deterministic state machine.
    """

    def __init__(self, labels: Optional[Set[Any]] = None):
        self.labels = labels or set()
        """
        Set of labels inheritted from the set of NFA states that this DFA state
        represents.
        """

        self.transitions: List[Tuple[CharSet, DFAState]] = []
        """
        List of associations between character sets and other states.

        Because this is a DFA, character sets cannot overlap.
        """

    def add_transition(self, chars: CharSet, next_state: DFAState) -> None:
        """
        Add a transition from this state to another one.

        :param chars: Specification of the input that allows to transition from
            this state to the next one. A CharSet instance indicates that one
            character in this set is required.
        :param next_state: Destination state for this new transition.
        """
        assert isinstance(chars, CharSet)
        assert isinstance(next_state, DFAState)

        # Check that ``chars`` does overlap with character sets for other
        # transitions.
        for other_chars, _ in self.transitions:
            assert not chars.overlaps_with(other_chars), (
                'Overlapping input char sets: {} and {}'
                .format(chars, other_chars)
            )

        self.transitions.append((chars, next_state))

    def to_dot(self) -> str:
        """
        Return a dot script representing this DFA.

        :rtype: str
        """
        return _to_dot(self,
                       lambda s: s.transitions,
                       lambda s: '\n'.join(str(l) for l in sorted(s.labels)))


class DFACodeGenHolder:
    """
    Holder for convenient data structures to generate code for the DFA.
    """

    class State:
        def __init__(self,
                     dfa_state: DFAState,
                     label: str,
                     transitions: List[Tuple[CharSet, DFAState]],
                     action: Optional[RuleAssoc]):
            self.dfa_state = dfa_state
            """
            DFA state this represents.
            """

            self.label = label
            """
            Code label for this state.
            """

            self.action = action
            """
            Action to execute when reaching this state.
            """

            # Non-ASCII transitions (i.e. wide Unicode handling) generally
            # creates huge case clauses in the generated code, which makes
            # compilation times and memory consumption unacceptable. This also
            # slows down the generated code.
            #
            # Because most lexer inputs will be pure ASCII anyway, we try here
            # to separate two cases:
            #
            # 1. pure ASCII transitions (or simple Unicode ones), to be lowered
            #    to case statements in Ada;
            # 2. complex Unicode transitions, to be lowered into sequences of
            #    if statements and table-based character lookups.
            #
            # (1) keep the lexer fast for common inputs and (2) reduces the
            # complexity of the CFG, which is good for compiler resources.

            self._transitions = transitions

            self.case_transitions: List[Tuple[CharSet, str]] = []
            """
            List of transitions to be lowered to a case statement. This maps
            maching characters to code labels for the next states.
            """

            self.table_transitions: List[Tuple[CharSet, str]] = []
            """
            List of transitions to be lowered to table-based character lookups.
            This maps the character sets for the lookup table for character
            lookup to code labels for the next states corresponding to these
            characters.
            """

            self.named_table_transitions: List[Tuple[str, str]] = []
            """
            List of table-based transitions. This contains the same data as
            ``table_transitions``, but with table names (for generated code
            entities) rather than abstract character sets.
            """

        def compute_transitions(self,
                                state_labels: Dict[DFAState, str]) -> None:
            """
            Compute self.case_transitions and self.table_transitions.

            :param state_labels: Labels for all DFA states.
            """
            for char_set, next_state in sorted(self._transitions):
                label = state_labels[next_state]
                ascii, non_ascii = char_set.split_ascii_subsets

                # If the non-ASCII range is too complex, lower it to
                # table-based character lookups. Otherwise, but everything in
                # case lowering.
                if len(non_ascii.ranges) > 3:
                    if not ascii.is_empty:
                        self.case_transitions.append((ascii, label))
                    self.table_transitions.append((non_ascii, label))
                else:
                    self.case_transitions.append((char_set, label))

        @property
        def has_transitions(self) -> bool:
            return bool(self.case_transitions or self.table_transitions)

    def __init__(self,
                 dfa: DFAState,
                 get_action: Callable[[Set[Any]], Optional[RuleAssoc]]):
        self.states: List[DFACodeGenHolder.State] = []
        """
        :type: list[DFACodeGenHolder.State]
        """

        # Compute the list of states corresponding to the code blocks to emit.
        # We store them in a list (self.states) to have deterministic code
        # emission, but we also maintain a set (visited_states) for fast
        # membership test.
        visited_states = set()

        queue = [dfa]
        while queue:
            dfa_state = queue.pop(0)
            if dfa_state in visited_states:
                continue
            visited_states.add(dfa_state)

            # Compute transition and queue unvisited nodes
            transitions = sorted(dfa_state.transitions)
            for char_set, next_state in transitions:
                if next_state not in queue:
                    queue.append(next_state)

            self.states.append(
                self.State(
                    dfa_state,
                    f'State_{len(self.states)}',
                    transitions,
                    get_action(dfa_state.labels)
                )
            )

        # Generate labels for all the states we saw
        self.state_labels = {state.dfa_state: state.label
                             for state in self.states}
        """
        :type: dict[DFAState, str]
        """

        for state in self.states:
            state.compute_transitions(self.state_labels)

        # Generate arrays for table-based character lookups.  At the same time,
        # replace character sets with these names in table-based transitions.

        self.charset_to_tablename: Dict[CharSet, str] = {}

        for state in self.states:
            for char_set, label in state.table_transitions:
                try:
                    table_name = self.charset_to_tablename[char_set]
                except KeyError:
                    table_name = 'Ranges_{}'.format(
                        len(self.charset_to_tablename))
                    self.charset_to_tablename[char_set] = table_name
                state.named_table_transitions.append((table_name, label))

    def ada_table_decls(self, prefix: str) -> str:
        """
        Helper to generate the Ada declarations for character lookup tables.
        """
        tables = sorted(
            (table_name, char_set)
            for char_set, table_name in self.charset_to_tablename.items()
        )

        lines: List[str] = []
        for table_name, char_set in tables:
            ranges: List[str] = []
            for l, h in char_set.ranges:
                if ranges:
                    ranges[-1] += ','
                ranges.append("(Character_Type'Val ({}),"
                              " Character_Type'Val ({}))".format(l, h))
            lines.append('{} : constant Character_Range_Array := ('
                         .format(table_name))
            lines.extend(ranges)
            lines.append(');')
        return '\n'.join(prefix + line for line in lines)
