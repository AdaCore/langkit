## vim: filetype=makoada

function ${_self.gen_fn_name} (Parser : in out Parser_Type;
                               Pos    : Token_Index)
                               return ${decl_type(_self.get_type())};
