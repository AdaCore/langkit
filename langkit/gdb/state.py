from __future__ import absolute_import, division, print_function

from collections import OrderedDict


def analysis_line_no(context, frame):
    """
    If the given frame is in the $-analysis.adb file, return its currently
    executed line number. Return None otherwise.

    :type frame: gdb.Frame
    :rtype: int|None
    """
    current_file = frame.function().symtab.fullname()
    return (frame.find_sal().line
            if current_file == context.debug_info.filename else None)


class State(object):
    """
    Holder for the execution state of a property.
    """

    def __init__(self, line_no, prop):
        self.property = prop
        """
        :type: langkit.gdb.debug_info.Property
        The property currently running.
        """

        self.scopes = []
        """
        :type: list[ScopeState]

        The stack of scope states describing the current execution state. The
        first item is the scope for the property itself. The following items
        are the nested scopes currently activated. The last item is the most
        nested scope.
        """

        self.started_expressions = []
        """
        :type: list[ExpressionEvaluation]
        Stack of expressions that are being evaluated.
        """

        self.line_no = line_no
        """
        :type: int
        The line number in the generated source code where execution was when
        this state was decoded.
        """

    @property
    def property_scope(self):
        """
        Return the ScopeState associated to the running property.

        :rtype: ScopeState
        """
        return self.scopes[0]

    @property
    def innermost_scope(self):
        """
        Return the ScopeState associated to the innermost activated scope.

        :rtype: ScopeState
        """
        return self.scopes[-1]

    def lookup_current_expr(self):
        """
        Return the innermost currently evaluating expression and its scope
        state. Return (None, None) if there is no evaluating expression.

        :rtype: (None, None)|(langkit.gdb.state.ScopeState,
                              langkit.gdb.state.ExpressionEvaluation)
        """
        for scope_state in reversed(self.scopes):
            for e in reversed(scope_state.expressions.values()):
                if e.is_started:
                    return scope_state, e
        return (None, None)

    @classmethod
    def decode(cls, context, frame):
        """
        Decode the execution state from the given GDB frame. Return None if no
        property is running in this frame.

        :type frame: gdb.Frame
        :rtype: None|State
        """
        from langkit.gdb.debug_info import Event, Scope

        line_no = analysis_line_no(context, frame)

        # First, look for the currently running property
        prop = context.debug_info.lookup_property(line_no) if line_no else None
        if prop is None:
            return None

        # Create the result, add the property root scope
        result = cls(line_no, prop)
        root_scope_state = ScopeState(result, None, prop)
        result.scopes.append(root_scope_state)

        def build_scope_state(scope_state):
            for event in scope_state.scope.events:

                if isinstance(event, Event):
                    if event.line_no > line_no:
                        break
                    event.apply_on_state(scope_state)

                elif isinstance(event, Scope):
                    if event.line_range.first_line > line_no:
                        break
                    elif line_no in event.line_range:
                        sub_scope_state = ScopeState(result, scope_state,
                                                     event)
                        result.scopes.append(sub_scope_state)
                        build_scope_state(sub_scope_state)
                        break

        build_scope_state(root_scope_state)
        return result


class ScopeState(object):
    """
    Holder for the execution state of a specific scope in a property.
    """

    def __init__(self, state, parent, scope):
        self.state = state
        """
        :type: State
        """

        self.parent = parent
        """
        :type: None|ScopeState
        """

        self.scope = scope
        """
        :type: langkit.gdb.debug_info.Scope

        The scope of interest.
        """

        self.bindings = []
        """
        :type: list[Binding]

        Bindings that are live in this state.
        """

        self.expressions = OrderedDict()
        """
        :type: dict[str, ExpressionEvaluation]

        Expressions that are currently being evaluated or that are evaluated in
        this state, indexed by unique ids.
        """


class Binding(object):
    """
    Describe the mapping between a DSL-level variable and an Ada one in the
    generated code.
    """

    def __init__(self, dsl_name, gen_name):
        self.dsl_name = dsl_name
        """
        :type: str
        Name of the variable in the DSL.
        """

        self.gen_name = gen_name
        """
        :type: str
        Name of the variable in the Ada generated code.
        """


class ExpressionEvaluation(object):
    """
    Describe the state of evaluation of an expression.
    """

    STATE_START = 'start'
    """
    State of an expression whose evaluation has been started, but hasn't been
    completed yet.
    """

    STATE_DONE = 'done'
    """
    State of an expression whose evaluation has been completed. Its result is
    available for use in the result variable, if there is one.
    """

    def __init__(self, start_event):
        self.start_event = start_event

        self.parent_expr = None
        self.sub_exprs = []

        self.state = self.STATE_START
        self.done_at_line = None

    @property
    def expr_id(self):
        return self.start_event.expr_id

    @property
    def expr_repr(self):
        return self.start_event.expr_repr

    @property
    def result_var(self):
        return self.start_event.result_var

    @property
    def dsl_sloc(self):
        return self.start_event.dsl_sloc

    @property
    def done_event(self):
        return self.start_event.done_event

    def set_done(self, line_no):
        self.state = self.STATE_DONE
        self.done_at_line = line_no

    @property
    def is_started(self):
        return self.state == self.STATE_START

    @property
    def is_done(self):
        return self.state == self.STATE_DONE

    def append_sub_expr(self, expr):
        """
        Append `expr` to the list of sub-expressions for `self`. Also set
        `self` as the parent of `expr`.
        """
        assert expr.parent_expr is None
        self.sub_exprs.append(expr)
        expr.parent_expr = self

    def __repr__(self):
        return '<ExpressionEvaluation {}, {}>'.format(self.expr_id,
                                                      self.dsl_sloc)
