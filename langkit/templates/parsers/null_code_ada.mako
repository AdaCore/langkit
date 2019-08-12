## vim: filetype=makoada

% if parser.type.is_list_type:
   ${parser.res_var} := ${parser.type.parser_allocator} (Parser.Mem_Pool);
   Initialize
     (Self              => ${T.root_node.internal_conversion(parser.type,
                                                             parser.res_var)},
      Kind              => ${parser.type.ada_kind_name},
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Index'Max (${parser.start_pos}, 1),
      Token_End_Index   => No_Token_Index);
   Initialize_List
     (Self   => ${ctx.generic_list_type.internal_conversion(parser.type,
                                                            parser.res_var)},
      Parser => Parser,
      Count  => 0);

% else:
   ${parser.res_var} := ${parser.type.storage_nullexpr};
% endif
