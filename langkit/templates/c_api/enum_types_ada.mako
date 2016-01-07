## vim: filetype=makoada

<%def name="spec(cls)">

type ${cls.c_type(capi).name} is new int;
type ${cls.c_type(capi).name}_Ptr is access ${cls.c_type(capi).name};
${ada_doc(cls, lang='c')}

</%def>
