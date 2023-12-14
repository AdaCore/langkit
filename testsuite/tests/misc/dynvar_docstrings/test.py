"""
Check that docstrings for dynamic variables are correctly propagated to
properties that take these dynamic variables in the generated library.
"""

print("Reading property docs in generated code...")
with open("build/src/libfoolang-analysis.ads") as f:

    # Tracks whether we are processing the declaration of a property function.
    # Used to ignore all comments but the ones of these functions.
    in_property = False

    for line in f:
        line = line.strip()
        if line.startswith("function P_P"):
            in_property = True
            print("")
            print(f"== Doc for {line.split()[1]} ==")
            print("")

        elif in_property:
            if line.startswith("--"):
                print(line)
            elif not line:
                in_property = False

print("Done")
