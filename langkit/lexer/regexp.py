from __future__ import absolute_import, division, print_function

from collections import defaultdict
from contextlib import contextmanager
import itertools
import re


from langkit.lexer.char_set import CharSet
from langkit.lexer.unicode_data import unicode_categories_char_sets


rule_name_re = re.compile('[a-zA-Z][a-zA-Z0-9_]*')
del unicode_categories_char_sets


def _to_dot(starting_state, get_transitions, get_state_label):
    """
    Helper to emit a dot(1) file representing a states graph.

    Return the content of a dot(1) file as a string.

    :param T starting_state: Initial state for the graph.

    :param get_transitions: Function that returns for a given state the list of
        transitions from it. Each transition is a couple: (transition label,
        next state).
    :type get_transitions: (T) -> list[(U, T)]

    :param get_state_label: Function that returns an optional label for a
        state.
    :type: get_state_label: (T) -> str|None

    :rtype: str
    """
    id_generator = iter(itertools.count(0))
    ids = {}

    nodes = []
    edges = []

    def add_node(n):
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


class SequenceReader(object):
    def __init__(self, sequence):
        self.sequence = sequence
        self.index = 0

    def next_is(self, *items):
        return not self.eof and self.peek() in items

    def peek(self):
        assert not self.eof
        return self.sequence[self.index]

    def read(self):
        assert not self.eof
        result = self.peek()
        self.index += 1
        return result

    def go_back(self):
        assert self.index >= 0
        self.index -= 1

    @property
    def eof(self):
        return self.index == len(self.sequence)


class RegexpCollection(object):

    class Parser(object):
        """Base class for regexp components."""

        def to_nfa(self, regexps):
            raise NotImplementedError()

    class Sequence(Parser):
        """Consume input that sub-parsers can consume sequentially."""
        def __init__(self, subparsers):
            self.subparsers = subparsers

        def to_nfa(self, regexps):
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

        def __repr__(self):
            return 'Seq({})'.format(', '.join(
                repr(s) for s in self.subparsers))

    class Repeat(Parser):
        """Accept input running the sub-parser multiple times."""
        def __init__(self, subparser):
            self.subparser = subparser

        def to_nfa(self, regexps):
            # Get the subparser NFA and connect its ends to match repetitions
            nfa = self.subparser.to_nfa(regexps)
            nfa[1].add_transition(None, nfa[0])

            # The new ending NFA state is the first one from the subparser so
            # that the NFA accepts empty inputs.
            return (nfa[0], nfa[0])

        def __repr__(self):
            return 'Repeat({})'.format(self.subparser)

    class Or(Parser):
        """Accept input that at least one sub-parser accepts."""
        def __init__(self, subparsers):
            self.subparsers = subparsers

        def to_nfa(self, regexps):
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

        def __repr__(self):
            return 'Or({})'.format(', '.join(
                repr(s) for s in self.subparsers))

    class Opt(Parser):
        """Accept input that the sub-parser accepts, or the empty input."""
        def __init__(self, subparser):
            self.subparser = subparser

        def to_nfa(self, regexps):
            starting, ending = self.subparser.to_nfa(regexps)
            starting.add_transition(None, ending)
            return (starting, ending)

        def __repr__(self):
            return 'Opt({})'.format(self.subparser)

    class Range(Parser):
        """Accept any character in the given set."""
        def __init__(self, char_set):
            self.char_set = char_set

        def to_nfa(self, regexps):
            starting = NFAState()
            ending = NFAState()
            starting.add_transition(self.char_set, ending)
            return (starting, ending)

        def __repr__(self):
            return repr(self.char_set)

    class Defer(Parser):
        """Defer parsing to a named regexp."""
        def __init__(self, name):
            self.name = name

        def to_nfa(self, regexps):
            with regexps._visit_rule(self.name):
                parser = regexps.patterns[self.name]
                return parser.to_nfa(regexps)

        def __repr__(self):
            return '@{}'.format(self.name)

    escape_chars = {
        '(': '(', ')': ')',
        '[': '[', ']': ']',
        '|': '|', '+': '+', '*': '*', '\\': '\\',
        'a': '\a', 'b': '\b', 'B': '\\', 'e': '\033', 'f': '\f',
        'n': '\n', 'r': '\r', 't': '\t',
        '0': '\0',
    }

    def __init__(self):
        self.patterns = {}
        self._visiting_patterns = set()

    def _parse(self, regexp):
        stream = SequenceReader(regexp)
        root = self._parse_or(stream)
        assert stream.eof

        return root

    def add_patterns(self, **kwargs):
        for name, regexp in kwargs.iteritems():
            # Register the parser for regexp
            assert name not in self.patterns
            self.patterns[name] = self._parse(regexp)

    def nfa_for(self, regexp):
        return self._parse(regexp).to_nfa(self)

    @contextmanager
    def _visit_rule(self, rule_name):
        if rule_name in self._visiting_patterns:
            raise ValueError('infinite recursion in {}'.format(rule_name))
        self._visiting_patterns.add(rule_name)
        yield
        self._visiting_patterns.remove(rule_name)

    @classmethod
    def _read_escape(cls, stream):
        assert stream.read() == '\\'
        if stream.eof:
            raise ValueError('bogus escape')
        char = stream.read()
        return cls.escape_chars.get(char, char)

    @classmethod
    def _parse_or(cls, stream):
        subparsers = []
        while True:
            subparsers.append(cls._parse_sequence(stream))
            if stream.eof or stream.next_is(')'):
                break
            else:
                assert stream.read() == '|'
        return cls.Or(subparsers)

    @classmethod
    def _parse_sequence(cls, stream):
        subparsers = []
        while True:
            if stream.eof:
                break

            elif stream.next_is('('):
                stream.read()
                subparsers.append(cls._parse_or(stream))
                if not stream.next_is(')'):
                    raise ValueError('unbalanced parenthesis')
                stream.read()

            elif stream.next_is('['):
                subparsers.append(cls._parse_range(stream))

            elif stream.next_is('{'):
                stream.read()
                name = ''
                while not stream.eof and not stream.next_is('}'):
                    name += stream.read()
                if not stream.next_is('}'):
                    raise ValueError('unbalanced bracket')
                stream.read()
                if not rule_name_re.match(name):
                    raise ValueError('invalid rule name: {}'.format(name))
                subparsers.append(cls.Defer(name))

            elif stream.next_is('*', '+', '?'):
                if not subparsers:
                    raise ValueError('nothing to repeat')
                elif isinstance(subparsers[-1], cls.Repeat):
                    raise ValueError('multiple repeat')
                wrapper = {
                    '*': lambda p: cls.Repeat(p),
                    '+': lambda p: cls.Sequence([p, cls.Repeat(p)]),
                    '?': lambda p: cls.Opt(p)
                }[stream.read()]
                subparsers[-1] = wrapper(subparsers[-1])

            elif stream.next_is('.'):
                # Generally, "." designates any character *except* newlines. Do
                # the same here.
                stream.read()
                subparsers.append(cls.Range(CharSet('\n').negation))

            elif stream.next_is('|', ')'):
                break

            elif stream.next_is('^', '$'):
                raise ValueError('matching beginning or ending is unsupported')

            elif stream.next_is('\\'):
                stream.read()

                # \p and \P refer to character sets from Unicode general
                # categories.
                if stream.next_is('p', 'P'):
                    action = stream.read()

                    # Read the category name, which must appear between curly
                    # brackets.
                    category = ''
                    if not stream.next_is('{'):
                        raise ValueError('incomplete Unicode category matcher')
                    stream.read()
                    while not stream.eof and not stream.next_is('}'):
                        category += stream.read()
                    if not stream.next_is('}'):
                        raise ValueError('incomplete Unicode category matcher')
                    stream.read()

                    try:
                        char_set = CharSet.for_category(category)
                    except KeyError:
                        raise ValueError('invalid Unicode category: {}'.format(
                            category
                        ))
                    if action == 'P':
                        char_set = char_set.negation
                    subparsers.append(cls.Range(char_set))

                else:
                    stream.go_back()
                    subparsers.append(
                        cls.Range(CharSet(cls._read_escape(stream))))

            else:
                subparsers.append(cls.Range(CharSet(stream.read())))

        return cls.Sequence(subparsers)

    @classmethod
    def _parse_range(cls, stream):
        assert stream.read() == '['
        ranges = []

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
                if in_range:
                    raise ValueError('dangling dash')
                in_range = True
                stream.read()
            else:
                char = (cls._read_escape(stream)
                        if stream.next_is('\\') else stream.read())
                if in_range:
                    low = ranges.pop()
                    ranges.append((low, char))
                else:
                    ranges.append(char)
                in_range = False

        if in_range:
            raise ValueError('dangling dash')
        elif not stream.next_is(']'):
            raise ValueError('unbalanced square bracket')
        assert stream.read() == ']'

        char_set = CharSet(*ranges)
        if negate:
            char_set = char_set.negation
        return cls.Range(char_set)


class NFAState(object):
    """
    Single state in a non-deterministic finite state machine.
    """

    def __init__(self):
        self.label = None
        """
        Annotation for this node. If non-None, this annotation is propagated to
        the corresponding DFA states.
        """

        self.transitions = []
        """
        List of associations between character sets and other states.

        None means that no character is needed to do the transition.
        Because this is a NFA, the various character sets can overlap.

        :type: list[(None|CharSet, NFAState)]
        """

    def add_transition(self, chars, next_state):
        """
        Add a transition from this state to another one.

        :param None|CharSet chars: Specification of the input that allows to
            transition from this state to the next one. A CharSet instance
            indicates that one character in this set is required. None
            indicates that the empty input allows to transition.
        :param NFAState next_state: Destination state for this new transition.
        """
        assert chars is None or isinstance(chars, CharSet)
        assert isinstance(next_state, NFAState)
        self.transitions.append((chars, next_state))

    @staticmethod
    def follow_spontaneous_transitions(states):
        """
        Return the set of states that can be reached from the given set of
        states following spontaneous transitions.

        :param set[NFAState] states: List of starting states.
        :rtype: set[NFAState]
        """
        result = set()

        def process(state):
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
    def reachable_nonspontaneous_transitions(states):
        """
        Return the list of non-spontaneous transitions that can be reached from
        the given set of states.

        Computing this is valid if following any spontaneous transition from
        one given state leads to a state that is also in ``states``, i.e. if
        ``follow_spontaneous_transitions(states) == states``.

        :param set[NFAState] states: List of starting states.
        :rtype: list[(CharSet, set[NFAState])]
        """
        result = []
        for state in states:
            for chars, next_state in state.transitions:
                if chars is None:
                    assert next_state in states
                else:
                    result.append((chars, next_state))
        return result

    @staticmethod
    def hashable_state_set(states):
        """
        Return a value that can be used as a dict key/set element to represent
        a set of states.

        :rtype: set[states]
        """
        return tuple(sorted(states))

    @staticmethod
    def deterministic_transitions(states):
        """
        Return the set of deterministic (non-spontaneous and disjoint)
        transitions that leave the "states" sub-graph.

        This is a major helper in the convertion of NFAs into corresponding
        DFAs.

        The result is a mapping from sets of NFA states (destination of
        deterministic transitions) to disjoint character sets (label for
        transitions).

        :param list[NFAState] states: Set of states from which we compute
            transitions.
        :rtype: dict[tuple[NFAState], CharSet]
        """
        # First, collect for each reachable state the set of characters that
        # allow to reach that state.
        transitions = {}
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
        class Event(object):
            def __init__(self):
                self.adding = set()
                self.removing = set()

        events = defaultdict(Event)
        for next_state, chars in transitions.iteritems():
            for r in chars.ranges:
                events[r[0]].adding.add(next_state)
                events[r[1]].removing.add(next_state)
        events = sorted(events.iteritems())

        # The final step is to compute the set of transitions for which
        # character sets are disjoint: just follow the stream of events.

        result = defaultdict(CharSet)

        def add_transition(low, high, states):
            if not states:
                return
            states = NFAState.follow_spontaneous_transitions(states)
            result[NFAState.hashable_state_set(states)].add_range(
                unichr(low), unichr(high))

        # Set of states "active" for the current position in the events stream
        states = set()

        # Character for the last event we processed
        last_char = None

        for char, event in events:
            assert event.adding or event.removing

            if event.adding:
                add_transition(last_char, char - 1, states)
                states = states.union(event.adding)
                last_char = char

            if event.removing:
                add_transition(last_char, char, states)
                states = states.difference(event.removing)
                last_char = char + 1

        return result

    def to_dfa(self):
        """
        Return the conversion of this NFA into a DFA.

        :rtype: DFAState
        """
        result = None

        # Mapping from sets of NFAState nodes (see NFAState.hashable_state_set)
        # to the corresponding DFAState nodes.
        dfa_states = {}

        # List of tuple[NFAState], CharSet and tuple[NFAState] for the
        # transitions that constitute the DFA.
        transitions = []

        queue = {
            self.hashable_state_set(
                self.follow_spontaneous_transitions([self])
            )
        }
        while queue:
            states = queue.pop()
            if states in dfa_states:
                return

            new_node = DFAState(labels={s.label for s in states
                                        if s.label is not None})
            dfa_states[states] = new_node
            if result is None:
                result = new_node

            for next_states, char_set in self.deterministic_transitions(
                states
            ).iteritems():
                if next_states not in dfa_states:
                    queue.add(next_states)
                transitions.append((states, char_set, next_states))

        for states, char_set, next_states in transitions:
            dfa_states[states].add_transition(char_set,
                                              dfa_states[next_states])

        assert result
        return result

    def to_dot(self):
        """
        Return a dot script representing this NFA.

        :rtype: str
        """
        return _to_dot(self, lambda s: s.transitions, lambda s: s.label)


class DFAState(object):
    """
    Single state in a deterministic state machine.
    """

    def __init__(self, labels=None):
        self.labels = labels or set()
        """
        Set of labels inheritted from the set of NFA states that this DFA state
        represents.

        :type: set[T]
        """

        self.transitions = []
        """
        List of associations between character sets and other states.

        Because this is a DFA, character sets cannot overlap.

        :type: list[(CharSet, NFAState)]
        """

    def add_transition(self, chars, next_state):
        """
        Add a transition from this state to another one.

        :param CharSet chars: Specification of the input that allows to
            transition from this state to the next one. A CharSet instance
            indicates that one character in this set is required.
        :param DFAState next_state: Destination state for this new transition.
        """
        assert isinstance(chars, CharSet)
        assert isinstance(next_state, DFAState)

        # Check that ``chars`` does overlap with character sets for other
        # transitions.
        for other_chars, _ in self.transitions:
            if chars.overlaps_with(other_chars):
                raise ValueError('Overlapping input char sets: {} and {}'
                                 .format(chars, other_chars))

        self.transitions.append((chars, next_state))

    def to_dot(self):
        """
        Return a dot script representing this DFA.

        :rtype: str
        """
        return _to_dot(self,
                       lambda s: s.transitions,
                       lambda s: '\n'.join(str(l) for l in sorted(s.labels)))


class DFACodeGenHolder(object):
    """
    Holder for convenient data structures to generate code for the DFA.
    """

    class State(object):
        def __init__(self, dfa_state, label, transitions, action):
            self.dfa_state = dfa_state
            """
            DFA state this represents.

            :type: DFAState
            """

            self.label = label
            """
            Code label for this state.

            :type: str
            """

            self.action = action
            """
            Action to execute when reaching this state.

            :type: langkit.lexer.RuleAction
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

            self.case_transitions = []
            """
            List of transitions to be lowered to a case statement. This maps
            maching characters to code labels for the next states.

            :type: list[(CharSet, str)]
            """

            self.table_transitions = []
            """
            List of transitions to be lowered to table-based character lookups.
            This maps the name of the lookup table for character lookup to code
            labels for the next states.

            Note that right after the "compute_transitions" pass, the mapping
            is from character set. These are replaced by table names later on.

            :type: list[(str, str)]
            """

        def compute_transitions(self, state_labels):
            """
            Compute self.case_transitions and self.table_transitions.

            :param dict[DFAState, str] state_labels: Labels for all DFA states.
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
        def has_transitions(self):
            return self.case_transitions or self.table_transitions

    def __init__(self, dfa, get_action):
        self.states = []
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
            state = queue.pop(0)
            if state in visited_states:
                continue
            visited_states.add(state)

            # Compute transition and queue unvisited nodes
            transitions = sorted(state.transitions)
            for char_set, next_state in transitions:
                if next_state not in queue:
                    queue.append(next_state)

            self.states.append(self.State(
                state, 'State_{}'.format(len(self.states)), transitions,
                get_action(state.labels)))

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

        self.charset_to_tablename = {}
        """
        :type: dict[CharSet, str]
        """

        for state in self.states:
            new_transitions = []
            for char_set, next_state in state.table_transitions:
                try:
                    table_name = self.charset_to_tablename[char_set]
                except KeyError:
                    table_name = 'Ranges_{}'.format(
                        len(self.charset_to_tablename))
                    self.charset_to_tablename[char_set] = table_name
                new_transitions.append((table_name, next_state))
            state.table_transitions = new_transitions

    def ada_table_decls(self, prefix):
        """
        Helper to generate the Ada declarations for character lookup tables.
        """
        tables = sorted(
            (table_name, char_set)
            for char_set, table_name in self.charset_to_tablename.iteritems()
        )

        lines = []
        for table_name, char_set in tables:
            ranges = []
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
