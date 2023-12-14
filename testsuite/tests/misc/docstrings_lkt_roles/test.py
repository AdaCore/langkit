"""
Check that the typeref langkit doc directive is correctly emitted in generated
APIs.
"""

for path in [
    "build/src/libfoolang-analysis.ads",
    "build/src/libfoolang.h",
    "build/ocaml/libfoolang.mli",
    "build/python/libfoolang/__init__.py",
]:
    with open(path) as f:
        for line in f.readlines():
            if 'Log this docstring' in line:
                print(f"{path}:")
                print(f"  {line.strip()}")

print('Done')
