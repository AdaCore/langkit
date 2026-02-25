"""
Check that the "lkm generate" command removes stale generated source files.
"""

import os

import langkit.scripts.create_project as create_project
import langkit.scripts.lkm as lkm


# Create the test project and build it a first time (to create the cache).
# Disable all outputs to avoid cluttering the test output.
create_project.main(["Foo"])
with open("langkit.yaml", "a") as f:
    print("plugin_passes: [my_plugin.create_pass]", file=f)

# Instruct our plugin to write both an Ada spec and a body in the generated
# sources the first time, and only the Ada spec the second time: we expect the
# second generation to remove the body.
for write_a_body in ("yes", ""):
    print(f"With WRITE_A_BODY={write_a_body}")
    os.environ["WRITE_A_BODY"] = write_a_body

    # Add an artificial change to a source file so that the following performs
    # code generation. Do not use "-f" as this would prevent us from checking
    # that incremental generation works as expected.
    with open("foo/nodes.lkt", "a") as f:
        print("# Dummy addition", file=f)

    lkm.main(["generate", "-vnone"])

    for filename in sorted(os.listdir("build/src")):
        if (
            # Check the presence of the plugin-generated sources
            filename.startswith("libfoolang-extra_src.")
            # Also check the presence of the lexer state machine, as its body
            # has an additional caching mechanism inside the code generator.
            or filename.startswith("libfoolang-lexer_state_machine.")
        ):
            print(f"  {filename}")
    print()

print("Done")
