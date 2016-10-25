## vim: filetype=makoada

% if _self.get_type().is_list_type:
   ${res} :=
    (${_self.get_type().storage_type_name()}_Alloc.Alloc (Parser.Mem_Pool));
   ${res}.Unit := Parser.Unit;
   ${res}.Token_Start := ${pos_name} - 1;
   ${res}.Token_End := ${pos_name};
% else:
   ${res} := ${_self.get_type().storage_nullexpr()};
% endif
