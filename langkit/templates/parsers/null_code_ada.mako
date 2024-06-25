## vim: filetype=makoada

% if parser.type.is_list_type:
   ${parser.res_var} := ${parser.type.parser_allocator} (Parser.Mem_Pool);
   Initialize
     (Self              => ${parser.res_var},
      Kind              => ${parser.type.ada_kind_name},
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Index'Max (${parser.start_pos}, 1),
      Token_End_Index   => No_Token_Index);
   Initialize_List
     (Self   => ${parser.res_var},
      Parser => Parser,
      Count  => 0);

%elif parser.type.is_bool_node:
   <% _, alt_false = parser.type._alternatives %>
   ${parser.res_var} := ${alt_false.parser_allocator} (Parser.Mem_Pool);
   Initialize
     (Self              => ${parser.res_var},
      Kind              => ${alt_false.ada_kind_name},
      Unit              => Parser.Unit,
      Token_Start_Index => ${parser.start_pos},
      Token_End_Index   => No_Token_Index);

% else:
   ${parser.res_var} := ${parser.type.storage_nullexpr};
% endif
