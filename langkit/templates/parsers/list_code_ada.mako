## vim: filetype=makoada

--  Start list_code

## If we accept empty lists, then we never want to return No_Token_Index as a
## position.
% if parser.empty_valid:
    ${parser.pos_var} := ${parser.start_pos};
% else:
    ${parser.pos_var} := No_Token_Index;
% endif

<% list_type = parser.type %>

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
     (${ctx.root_grammar_class.name} (${parser.parser.res_var}));

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

## Create the result of this parser: an AST list node, and copy the elements
## from our temporary parse list to the result.
declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := ${parser.tmplist}.Nodes.Length;
begin
   ${parser.res_var} := ${list_type.name}_Alloc.Alloc (Parser.Mem_Pool);

   ## Depending on whether we have an empty list, initialize token start/end
   ## information.
   if Count > 0 then
      Token_Start := ${parser.start_pos};
      Token_End := (if ${parser.cpos} = ${parser.start_pos}
                    then ${parser.start_pos}
                    else ${parser.cpos} - 1);

   else
      Token_Start := Token_Index'Max (${parser.start_pos}, 1);
      Token_End := No_Token_Index;
   end if;

   Initialize
     (Self              => ${parser.res_var},
      Kind              => ${list_type.ada_kind_name},
      Unit              => Parser.Unit,
      Token_Start_Index => Token_Start,
      Token_End_Index   => Token_End);
   Initialize_List
     (Self   => ${parser.res_var},
      Parser => Parser,
      Count  => Count);

   declare
      Vec : ${T.root_node.array.pkg_vector}.Vector renames
         ${parser.tmplist}.Nodes;
      Arr : Alloc_AST_List_Array.Element_Array_Access renames
         ${parser.res_var}.Nodes;
   begin
      Arr := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, Vec.Length);
      for I in Vec.First_Index .. Vec.Last_Index loop
         Arr (I) := Vec.Get (I);
      end loop;
   end;
end;

Release_Parse_List (Parser, ${parser.tmplist});

--  End list_code
