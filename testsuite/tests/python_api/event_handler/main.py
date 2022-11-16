from contextlib import contextmanager
import sys

import libfoolang


print("main.py: Starting...")
print("")


class ExceptionManager:
    enabled = False

    @classmethod
    @contextmanager
    def enable(cls):
        sys.stdout.flush()
        cls.enabled = True
        yield
        cls.enabled = False

    @classmethod
    def maybe_raise(cls):
        if cls.enabled:
            raise ValueError("artificial exception")


class EventHandler(libfoolang.EventHandler):
    def __init__(self, label):
        self.label = label

    def unit_requested_callback(self, context, name, from_unit, found,
                                is_not_found_error):
        print(f"{self.label}: unit_requested_callback")
        ExceptionManager.maybe_raise()
        print(f"  name: {name}")
        print(f"  from_unit: {from_unit}")
        print(f"  found: {found}")
        print(f"  is_not_found_error: {is_not_found_error}")
        print("")

    def unit_parsed_callback(self, context, unit, reparsed):
        print(f"{self.label}: unit_parsed_callback")
        ExceptionManager.maybe_raise()
        print(f"  unit: {unit}")
        print(f"  reparsed: {reparsed}")
        print("")


# Create an analysis context with our event handler
print("== create context ==")
print("")
ctx = libfoolang.AnalysisContext(event_handler=EventHandler("MyEH"))

# Trigger the "unit parsed" event twice: once for the initial parsing, and once
# for a reparsing.
print("== unit parsed ==")
print("")
u = ctx.get_from_buffer(filename="main.txt", buffer="example\n")
u = ctx.get_from_buffer(filename="main.txt", buffer="example\n")

with ExceptionManager.enable():
    u = ctx.get_from_buffer(filename="main.txt", buffer="example\n")
    print("")

# Trigger the "unit requested" event with various parameters
print("== unit requested ==")
print("")
n = u.root
n.p_trigger_unit_requested("foo_1", found=True, error=False)
n.p_trigger_unit_requested("foo_2", found=False, error=False)
n.p_trigger_unit_requested("foo_3", found=False, error=True)
with ExceptionManager.enable():
    n.p_trigger_unit_requested("foo_4", found=False, error=False)
    print("")

print("main.py: Done.")
