## vim: filetype=makoada

function ${parser.gen_fn_name}
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return ${parser.get_type().storage_type_name};
