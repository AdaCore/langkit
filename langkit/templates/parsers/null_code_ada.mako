## vim: filetype=makoada

% if parser.get_type().is_list_type:
   ${parser.res_var} :=
    (${parser.get_type().storage_type_name}_Alloc.Alloc (Parser.Mem_Pool));
   ${parser.res_var}.Unit := Parser.Unit;
   ${parser.res_var}.Count := 0;
   ${parser.res_var}.Nodes := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, 0);

   ${parser.res_var}.Token_Start_Index
     := Token_Index'Max (1, ${parser.start_pos} - 1);
   ${parser.res_var}.Token_End_Index := No_Token_Index;

% else:
   ${parser.res_var} := ${parser.get_type().storage_nullexpr};
% endif
