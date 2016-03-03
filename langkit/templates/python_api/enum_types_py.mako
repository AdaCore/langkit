## vim: filetype=makopython

<%def name="decl(cls)">

# Enumerators for ${cls.c_type(capi).name}
${py_doc(cls)}
% for py_alt, alt in zip(cls.alternatives_for(pyapi), cls.alternatives):
${py_alt} = '${alt}'
% endfor
${cls.c_type(capi).name}_to_str = [
    UNINITIALIZED,
% for alt in cls.alternatives:
    '${alt}',
% endfor
]
str_to_${cls.c_type(capi).name} = {
    UNINITIALIZED: 0,
% for i, alt in enumerate(cls.alternatives, 1):
    '${alt}': ${i},
% endfor
}

</%def>
