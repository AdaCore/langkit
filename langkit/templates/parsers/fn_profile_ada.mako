## vim: filetype=makoada

function ${_self.gen_fn_name} (Parser : in out Parser_Type;
                               Pos    : Token_Index)
                               return ${_self.get_type().storage_type_name()};
