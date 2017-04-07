python import sys; sys.path.append(${repr(langkit_path)})
python import langkit.gdb; langkit.gdb.setup(\
    lib_name=${repr(lib_name)}, \
    astnode_names=${repr(astnode_names)}, \
    prefix=${repr(prefix)} \
)
