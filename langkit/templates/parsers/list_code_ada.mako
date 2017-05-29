## vim: filetype=makoada

--  Start list_code

## If we accept empty lists, then we never want to return No_Token_Index as a
## position.
% if parser.empty_valid:
    ${parser.pos_var} := ${parser.start_pos};
% else:
    ${parser.pos_var} := No_Token_Index;
% endif

<% list_type = parser.get_type() %>

${parser.cpos} := ${parser.start_pos};
${parser.tmplist} := Get_Parse_List (Parser);

loop
   ## Parse one list element
   ${parser.parser.generate_code()}

   ## Stop as soon as we cannot parse list elements anymore
   exit when ${parser.parser.pos_var} = No_Token_Index;

   ${parser.pos_var} := ${parser.parser.pos_var};
   ${parser.cpos} := ${parser.pos_var};

   ## Append the parsed result to the list
   ${parser.tmplist}.Nodes.Append
     (${ctx.root_grammar_class.name()} (${parser.parser.res_var}));

   ## Parse the separator, if there is one. The separator is always discarded.
   % if parser.sep:
      ${parser.sep.generate_code()}
      if ${parser.sep.pos_var} /= No_Token_Index then
          ${parser.cpos} := ${parser.sep.pos_var};
      else
         ## If we didn't successfully parse a separator, exit
         exit;
      end if;
   % endif

end loop;

## Create the result of this parser: an AST list node
${parser.res_var} := ${list_type.name()}_Alloc.Alloc (Parser.Mem_Pool);
${parser.res_var}.Unit := Parser.Unit;

if ${parser.tmplist}.Nodes.Length > 0 then
   ## Copy the elements from our temporray parse list to the result
   declare
      use Node_Bump_Ptr_Vectors;
      In_Vec  : ${T.root_node.array_type().pkg_vector()}.Vector renames
         ${parser.tmplist}.Nodes;
      Out_Vec : Vector renames ${parser.res_var}.Vec;
   begin
      Out_Vec := Node_Bump_Ptr_Vectors.Create (Parser.Mem_Pool);
      for I in In_Vec.First_Index .. In_Vec.Last_Index loop
         Append (Out_Vec, In_Vec.Get (I));
      end loop;
   end;

   ${parser.res_var}.Token_Start_Index := ${parser.start_pos};
   ${parser.res_var}.Token_End_Index :=
     (if ${parser.cpos} = ${parser.start_pos}
      then ${parser.start_pos} else ${parser.cpos} - 1);

else
   ${parser.res_var}.Token_Start_Index :=
      Token_Index'Max (${parser.start_pos}, 1);
   ${parser.res_var}.Token_End_Index := No_Token_Index;
end if;

Release_Parse_List (Parser, ${parser.tmplist});


--  End list_code
