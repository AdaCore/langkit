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

    def __init__(self, prop):
        self.property = prop
        """
        :type: langkit.gdb.debug_info.Property
        The property currently running.
        """

        self.scopes = []
        """
        :type: list[ScopeState]
        The list of scope state to describe the current execution state. The
        first item is the scope for the property itself. The following items
        are the nested scopes currently activated. The last item is the most
        nested scope.
        """

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
        result = cls(prop)
        root_scope_state = ScopeState(None, prop)
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
                        sub_scope_state = ScopeState(scope_state, event)
                        result.scopes.append(sub_scope_state)
                        build_scope_state(sub_scope_state)
                        break

        build_scope_state(root_scope_state)
        return result


class ScopeState(object):
    """
    Holder for the execution state of a specific scope in a property.
    """

    def __init__(self, parent, scope):
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
        this state.
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

    def __init__(self, expr_id, expr_repr, result_var=None, expr_loc=None):
        self.expr_id = expr_id
        self.expr_repr = expr_repr
        self.result_var = result_var
        self.expr_loc = expr_loc

        self.state = self.STATE_START

    def set_done(self):
        self.state = self.STATE_DONE

    @property
    def is_started(self):
        return self.state == self.STATE_START

    @property
    def is_done(self):
        return self.state == self.STATE_DONE
