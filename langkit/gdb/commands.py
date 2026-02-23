from __future__ import annotations

from collections import namedtuple
from functools import partial
from io import StringIO
from typing import Callable

import gdb

from langkit.debug_info import ExprStart, LktLocation, Property, Scope
from langkit.gdb.context import Context
from langkit.gdb.control_flow import go_next, go_out, go_step_inside
from langkit.gdb.state import Binding
from langkit.gdb.utils import expr_repr, name_repr, prop_repr
from langkit.utils import no_colors
from langkit.utils.text import indent


class BaseCommand(gdb.Command):
    """
    Factorize common code for our commands.
    """

    def __init__(
        self,
        context: Context,
        basename: str,
        command_class: int,
        completer_class: int = gdb.COMPLETE_NONE,
    ):
        name = context.prefix + basename
        if completer_class is None:
            super().__init__(name=name, command_class=command_class)
        else:
            super().__init__(
                name=name,
                command_class=command_class,
                completer_class=completer_class,
            )
        self.context = context


class StateCommand(BaseCommand):
    """Display the state of the currently running property.

    This command may be followed by a "/X" flag, where X is one or several of:

        * f: display the full image of values (no ellipsis);
        * s: to print the name of the Ada variables that hold DSL values.

    There is one optional argument: a variable name. If specified, this command
    only displays information for this variable.
    """

    def __init__(self, context: Context):
        super().__init__(context, "state", gdb.COMMAND_DATA)

    def invoke(self, arg: str, from_tty: bool) -> None:
        args = arg.split()
        flags = set()
        var_name = None

        # Get flags, if any
        if args and args[0].startswith("/"):
            flags = set(args.pop(0)[1:])
            invalid_flags = flags.difference("sf")
            if invalid_flags:
                print("Invalid flags: {}".format(", ".join(invalid_flags)))
                return

        # Get the variable name, if any
        if args:
            var_name = args.pop(0)

        if args:
            print("Invalid extra arguments: {}".format(" ".join(args)))

        StatePrinter(
            self.context,
            with_ellipsis="f" not in flags,
            with_locs="s" in flags,
            var_name=var_name,
        ).run()


class StatePrinter:
    """
    Helper class to embed code to display the state of the currently running
    property.
    """

    ellipsis_limit = 80

    def __init__(
        self,
        context: Context,
        with_ellipsis: bool = True,
        with_locs: bool = False,
        var_name: str | None = None,
    ):
        self.context = context

        self.frame = gdb.selected_frame()
        self.state = self.context.decode_state(self.frame)

        self.with_ellipsis = with_ellipsis
        self.with_locs = with_locs
        self.var_name = var_name
        self.sio = StringIO()

    def _render(self) -> None:
        """
        Internal render method for the state printer.
        """

        # We rebind print to print to our StringIO instance for the scope of
        # this method.
        prn = partial(print, file=self.sio)

        def print_binding(print_fn: Callable[[str], None], b: Binding) -> None:
            print_fn(
                "{}{} = {}".format(
                    name_repr(b),
                    self.loc_image(b.gen_name),
                    self.value_image(b.gen_name),
                )
            )

        if self.state is None:
            prn("Selected frame is not in a property.")
            return

        # If we are asked to display only one variable, look for it, print it,
        # and stop there.
        if self.var_name:
            for scope_state in self.state.scopes:
                for b in scope_state.bindings:
                    if b.lkt_name == self.var_name:
                        print_binding(prn, b)
                        return
            prn("No binding called {}".format(self.var_name))
            return

        prn("Running {}".format(prop_repr(self.state.property)))
        if self.state.property.lkt_sloc:
            prn("from {}".format(self.state.property.lkt_sloc))

        if self.state.in_memoization_lookup:
            prn("About to return a memoized result...")

        for scope_state in self.state.scopes:

            def print_binding_cb(strn: str) -> None:
                prn(indent(strn, 2))

            for b in scope_state.bindings:
                print_binding(print_binding_cb, b)

            done_exprs, last_started = scope_state.sorted_expressions()

            for e in done_exprs:
                prn(
                    "  {}{} -> {}".format(
                        expr_repr(e),
                        self.loc_image(e.result_var),
                        self.value_image(e.result_var, subsequent_indent="  "),
                    )
                )

            if last_started:
                prn("")
                prn("Currently evaluating {}".format(expr_repr(last_started)))
                if last_started.lkt_sloc:
                    prn("from {}".format(last_started.lkt_sloc))

    def run(self) -> None:
        """
        Output the state to stdout.
        """
        self._render()
        print(self.sio.getvalue())

    def render(self) -> str:
        """
        Return the state as a string.
        """
        with no_colors():
            self._render()
        return self.sio.getvalue()

    def loc_image(self, var_name: str) -> str:
        """
        If `self.with_locs`, return the name of the Ada variable that holds the
        DSL value.
        """
        return " ({})".format(var_name) if self.with_locs else ""

    def value_image(self, var_name: str, subsequent_indent: str = "") -> str:
        """
        Return the image of the value contained in the `var_name` variable.

        :param subsequent_indent: Prefix for each line in the result except the
            first one.
        """
        # Switching to lower-case is required since GDB ignores case
        # insentivity for Ada from the Python API.
        value = str(self.frame.read_var(var_name.lower()))
        if self.with_ellipsis and len(value) > self.ellipsis_limit:
            value = "{}...".format(value[: self.ellipsis_limit])

        # Add the requested indentation for all lines except the first one
        lines = value.splitlines()
        lines[1:] = [subsequent_indent + line for line in lines[1:]]
        return "\n".join(lines)


class BreakCommand(BaseCommand):
    """Put a breakpoint on a property. One of the following forms is allowed:

        * A case-insensitive property qualified name; for instance::
              break MyNode.p_property

        * A DSL source location; for instance, in spec.py, line 128::
              break spec.py:128

    In both cases, one can pass an expression to make the breakpoint
    conditional. For instance::

        break MyNode.p_property if $match("<Node XXX>", self)
    """

    def __init__(self, context: Context):
        super().__init__(context, "break", gdb.COMMAND_BREAKPOINTS)

    def complete(self, text: str, word: str) -> list[str]:
        """
        Try to complete `word`.

        Assuming `word` is the start of a property qualified name, return all
        possible completions. This is case insensitive, for user convenience.
        """
        prefix = word.lower()
        result = [
            prop.name
            for prop in self.context.debug_info.properties
            if prop.name.lower().startswith(prefix)
        ]

        # If the users didn't ask for a special property, don't suggest special
        # properties, as they are usually just noise for them.
        if not prefix.startswith("["):
            result = [n for n in result if not n.startswith("[")]

        return result

    def invoke(self, arg: str, from_tty: bool) -> None:
        argv = arg.strip().split(None, 2)

        spec = None
        cond = None

        if len(argv) == 0:
            print("Breakpoint specification missing")
            return

        elif len(argv) == 1:
            (spec,) = argv

        elif len(argv) == 3:
            spec, if_kwd, cond = argv
            if if_kwd != "if":
                print('Invalid arguments (second arg should be "if")')
                return

        else:
            print("Invalid number of arguments")
            return

        bp = (
            self.break_on_lkt_sloc(spec)
            if ":" in spec
            else self.break_on_property(spec)
        )
        if bp and cond:
            try:
                bp.condition = cond
            except gdb.error as exc:
                print(exc)
                return

    def break_on_property(self, qualname: str) -> gdb.Breakpoint | None:
        """
        Try to put a breakpoint on a property whose qualified name is
        `qualname`. Display a message for the user if that is not possible.
        """
        lower_prop = qualname.lower()

        for prop in self.context.debug_info.properties:
            if prop.name.lower() == lower_prop:
                break
        else:
            print("No such property: {}".format(qualname))
            return None

        if prop.body_start is None:
            print("Cannot break on {}: it has no code".format(prop.name))
            return None

        # Break on the first line of the property's first inner scope so that
        # we skip the prologue (all variable declarations).
        return gdb.Breakpoint(prop.body_start.gdb_spec)

    def break_on_lkt_sloc(self, lkt_sloc: str) -> gdb.Breakpoint | None:
        """
        Try to put a breakpoint on code that maps to the given DSL source
        location. Display a message for the user if that is not possible.
        """
        sloc_spec = LktLocation.parse(lkt_sloc)
        if sloc_spec is None:
            print("Nothing to match")
            return None

        Match = namedtuple("Match", "prop lkt_sloc ada_loc")
        matches = []

        def process_scope(prop: Property, scope: Scope) -> None:
            assert isinstance(sloc_spec, LktLocation)
            for e in scope.events:
                if isinstance(e, Scope):
                    process_scope(prop, e)
                elif (
                    isinstance(e, ExprStart)
                    and e.lkt_sloc
                    and e.lkt_sloc.matches(sloc_spec)
                ):
                    matches.append(Match(prop, e.lkt_sloc, e.loc))

        for prop in self.context.debug_info.properties:
            process_scope(prop, prop)

        if not matches:
            print("No match for {}".format(sloc_spec))
            return None

        elif len(matches) == 1:
            (m,) = matches

        else:
            print("Multiple matches for {}:".format(sloc_spec))

            def idx_fmt(i: int) -> str:
                return "[{}] ".format(i)

            idx_width = len(idx_fmt(len(matches)))
            for i, m in enumerate(matches, 1):
                print(
                    "{}In {}, {}".format(
                        idx_fmt(i).rjust(idx_width), m.prop.name, m.lkt_sloc
                    )
                )
                print("{}at {}".format(" " * idx_width, m.ada_loc))

            print("Please chose one of the above locations [default=1]:")
            try:
                choice_str = input("> ")
            except EOFError:
                print("Aborting: no breakpoint created")
                return None

            if not choice_str:
                choice = 1
            else:
                try:
                    choice = int(choice_str)
                except ValueError:
                    print("Invalid index choice: {}".format(choice_str))
                    return None

                if choice < 1 or choice > len(matches):
                    print(
                        "Choice must be in range {}-{}".format(1, len(matches))
                    )
                    return None

            m = matches[choice]

        return gdb.Breakpoint(m.ada_loc.gdb_spec)


class NextCommand(BaseCommand):
    """Continue execution until reaching another expression."""

    def __init__(self, context: Context):
        super().__init__(context, "next", gdb.COMMAND_RUNNING)

    def invoke(self, arg: str, from_tty: bool) -> None:
        if arg:
            print("This command takes no argument")
        else:
            go_next(self.context)


class OutCommand(BaseCommand):
    """Continue execution until the end of the evaluation of the current
    sub-expression.
    """

    def __init__(self, context: Context):
        super().__init__(context, "out", gdb.COMMAND_RUNNING)

    def invoke(self, arg: str, from_tty: bool) -> None:
        if arg:
            print("This command takes no argument")
        else:
            go_out(self.context)


class StepInsideCommand(BaseCommand):
    """If execution is about to call a property, step inside it. Traverse
    dispatch properties in order to land directly in the dispatched property.
    """

    def __init__(self, context: Context):
        super().__init__(context, "si", gdb.COMMAND_RUNNING)

    def invoke(self, arg: str, from_tty: bool) -> None:
        if arg:
            print("This command takes no argument")
        else:
            go_step_inside(self.context)
