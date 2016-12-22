## vim: filetype=makoada

% if _self.get_type().is_list_type:
   ${res} :=
    (${_self.get_type().storage_type_name()}_Alloc.Alloc (Parser.Mem_Pool));
   ${res}.Unit := Analysis_Unit_Interface (Parser.Unit);

   ${res}.Token_Start := Token_Index'Max (1, ${pos_name} - 1);
   ${res}.Token_End := No_Token_Index;

% else:
   ${res} := ${_self.get_type().storage_nullexpr()};
% endif
