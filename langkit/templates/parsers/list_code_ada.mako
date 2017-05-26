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

${parser.res_var} := ${list_type.name()}_Alloc.Alloc (Parser.Mem_Pool);

${parser.res_var}.Token_Start_Index := Token_Index'Max (${parser.start_pos}, 1);
${parser.res_var}.Token_End_Index := No_Token_Index;

${parser.cpos} := ${parser.start_pos};

loop
   ## Parse one list element
   ${parser.parser.generate_code()}

   ## Stop as soon as we cannot parse list elements anymore
   exit when ${parser.parser.pos_var} = No_Token_Index;

   ${parser.pos_var} := ${parser.parser.pos_var};
   ${parser.cpos} := ${parser.pos_var};

   if Node_Bump_Ptr_Vectors.Length (${parser.res_var}.Vec) = 0 then
      ${parser.res_var}.Vec := Node_Bump_Ptr_Vectors.Create (Parser.Mem_Pool);
   end if;

   ## Append the parsed result to the list
   Node_Bump_Ptr_Vectors.Append
     (${parser.res_var}.Vec,
      ${ctx.root_grammar_class.name()} (${parser.parser.res_var}));

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

${parser.res_var}.Unit := Parser.Unit;
if Node_Bump_Ptr_Vectors.Length (${parser.res_var}.Vec) > 0 then
   ${parser.res_var}.Token_Start_Index := ${parser.start_pos};
   ${parser.res_var}.Token_End_Index :=
     (if ${parser.cpos} = ${parser.start_pos}
      then ${parser.start_pos} else ${parser.cpos} - 1);
end if;


--  End list_code
