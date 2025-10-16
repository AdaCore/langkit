## vim: filetype=makoada

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

## If we accept a leading separator, try to scan it now. Just keep rolling if
## it is not there.
% if parser.allow_leading:
   ${parser.sep.generate_code()}
   if ${parser.sep.pos_var} /= No_Token_Index then
       ${parser.cpos} := ${parser.sep.pos_var};
       ${parser.has_leading} := True;
   else
       ${parser.has_leading} := False;
   end if;
% endif

${parser.render_set_mark()}
loop
   ## Parse one list element
   ${parser.parser.generate_code()}

   ## Stop as soon as we cannot parse list elements anymore
   exit when ${parser.parser.pos_var} = No_Token_Index;

   ${parser.pos_var} := ${parser.parser.pos_var};
   ${parser.cpos} := ${parser.pos_var};
   ${parser.render_set_mark()}

   ## Append the parsed result to the list
   ${parser.tmplist}.Nodes.Append (${parser.parser.res_var});

   ## Parse the separator, if there is one. The separator is always discarded.
   % if parser.sep:
      ${parser.sep.generate_code()}

      ## If we didn't successfully parse a separator, exit
      exit when ${parser.sep.pos_var} = No_Token_Index;

      ## The next attempt at parsing a list element will start where the
      ## separator parser stopped.
      ${parser.cpos} := ${parser.sep.pos_var};

      ## If we accept a trailing separator, mark the separator we just got
      ## as being part of this list node.
      % if parser.allow_trailing:
         ${parser.pos_var} := ${parser.cpos};
      % endif
   % endif

   ## Reset necessary "no backtrack" variables before the next iteration
   % for v in parser.nobt_reset_group:
      ${v} := False;
   % endfor
end loop;
## Rollback diagnostics to the point just before our last (failed) attempt at
## parsing a list item. This way, when returning the list node, this parser
## will have contributed to diagnostics only for the parsing of the actual list
## elements.
${parser.render_rollback()}

## If we had a leading separator, then we expect at least one element
% if parser.allow_leading:
   if ${parser.has_leading} and then ${parser.tmplist}.Nodes.Is_Empty then
      ${parser.pos_var} := No_Token_Index;
   end if;
% endif

## Create the result of this parser: an AST list node, and copy the elements
## from our temporary parse list to the result.
declare
   Token_Start, Token_End : Token_Index;
   Count                  : constant Natural := ${parser.tmplist}.Nodes.Length;
begin
   ${parser.res_var} := ${list_type.parser_allocator} (Parser.Mem_Pool);

   ## Depending on whether we have an empty list, initialize token start/end
   ## information.
   if Count > 0 then
      Token_Start := ${parser.start_pos};
      Token_End := (if ${parser.cpos} = ${parser.start_pos}
                    then ${parser.start_pos}
                    else ${parser.pos_var} - 1);

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
