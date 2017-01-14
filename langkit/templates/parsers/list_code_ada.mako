## vim: filetype=makoada

--  Start list_code

## If we accept empty lists, then we never want to return No_Token_Index as a
## position.
## TODO: This is weird because:
## 1. With those semantics, list(empty_valid=true) is equivalent to Opt(list),
##    so it might be better to compile it as such.
## 2. An empty list result should probably result in an empty list, not in a
##    null result.
% if _self.empty_valid:
    ${pos} := ${pos_name};
% else:
    ${pos} := No_Token_Index;
% endif

<%
   list_type = _self.get_type()
   el_type   = list_type.element_type().name()
%>

${res} := ${list_type.name()}_Alloc.Alloc (Parser.Mem_Pool);

${res}.Token_Start := Token_Index'Max (${pos_name}, 1);
${res}.Token_End := No_Token_Index;

${cpos} := ${pos_name};

loop
   ## Parse one list element
   ${parser_context.code}

   ## Stop as soon as we cannot parse list elements anymore
   exit when ${parser_context.pos_var_name} = No_Token_Index;

   ${pos} := ${parser_context.pos_var_name};
   ${cpos} := ${parser_context.pos_var_name};

   if Node_Bump_Ptr_Vectors.Length (${res}.Vec) = 0 then
      ${res}.Vec := Node_Bump_Ptr_Vectors.Create (Parser.Mem_Pool);
   end if;

   ## Append the parsed result to the list
   Node_Bump_Ptr_Vectors.Append
     (${res}.Vec,
      ${ctx.root_grammar_class.name()} (${parser_context.res_var_name}));

   ## If we are parsing nodes, then set the parent of parsed node to the
   ## list, and increment its ref count.
   % if is_ast_node (_self.parser.get_type()):
      if ${parser_context.res_var_name} /= null then
         ${parser_context.res_var_name}.Parent :=
           ${root_node_type_name} (${res});
      end if;
   % endif

   ## Parse the separator, if there is one. The separator is always discarded.
   % if _self.sep:
      ${sep_context.code}
      if ${sep_context.pos_var_name} /= No_Token_Index then
          ${cpos} := ${sep_context.pos_var_name};
      else
         ## If we didn't successfully parse a separator, exit
         exit;
      end if;
   % endif

end loop;

## If we managed to parse a list, compute and set the sloc range for this AST
## node.
if ${res} /= null then
   ${res}.Unit := Parser.Unit;
   if Node_Bump_Ptr_Vectors.Length (${res}.Vec) > 0 then
      ${res}.Token_Start := ${pos_name};
      ${res}.Token_End := (if ${cpos} = ${pos_name}
                           then ${pos_name}
                           else ${cpos} - 1);
   end if;
end if;


--  End list_code
