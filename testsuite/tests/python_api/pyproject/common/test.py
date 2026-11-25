import os.path


def main(ctx):
    print("# pyproject.toml[project] begin")
    with open("build/python/pyproject.toml") as f:
        in_project = False
        for line in f:
            if in_project:
                if line.startswith("["):
                    in_project = False
            elif line.strip() == "[project]":
                in_project = True

            if in_project and line.strip():
                print(line.rstrip())
    print("# pyproject.toml[project] end")

    dirname = "build/python"
    for filename in sorted(os.listdir("build/python")):
        if filename not in {"libfoolang", "pyproject.toml"}:
            print(f"# {filename} begin")
            with open(os.path.join(dirname, filename)) as f:
                for line in f:
                    print(line.strip())
            print(f"# {filename} end")
