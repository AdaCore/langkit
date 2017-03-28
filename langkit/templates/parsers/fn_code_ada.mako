## vim: filetype=makoada

<%
ret_type = parser.get_type().storage_type_name()
memo = "Parser.Private_Part.{}_Memo".format(parser.gen_fn_name)
%>

function ${parser.gen_fn_name} (Parser : in out Parser_Type;
                                Pos    : Token_Index)
                                return ${ret_type}
is
   % for name, typ in var_context:
      ${name} : ${typ.storage_type_name()}
         ${":= " + typ.storage_nullexpr() if typ.storage_nullexpr() else ""};
   % endfor

   % if parser.is_left_recursive():
      Mem_Pos : Token_Index := Pos;
      Mem_Res : ${ret_type} := ${parser.get_type().storage_nullexpr()};
   % endif

   M : ${ret_type}_Memos.Memo_Entry :=
     Get (${memo}, Pos);

begin

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      ${parser_context.res_var_name} := M.Instance;
      return ${parser_context.res_var_name};
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return ${parser_context.res_var_name};
   end if;

   % if parser.is_left_recursive():
       Set (${memo},
            False,
            ${parser_context.res_var_name},
            Pos,
            Mem_Pos);

       <<Try_Again>>
   % endif

   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   ${parser_context.code}

   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

   % if parser.is_left_recursive():
      if ${parser_context.pos_var_name} > Mem_Pos then
         Mem_Pos := ${parser_context.pos_var_name};
         Mem_Res := ${parser_context.res_var_name};
         Set (${memo},
              ${parser_context.pos_var_name} /= No_Token_Index,
              ${parser_context.res_var_name},
              Pos,
              ${parser_context.pos_var_name});
         goto Try_Again;

      elsif Mem_Pos > Pos then
         ${parser_context.res_var_name} := Mem_Res;
         ${parser_context.pos_var_name} := Mem_Pos;
         goto No_Memo;
      end if;
   % endif

   Set (${memo},
        ${parser_context.pos_var_name} /= No_Token_Index,
        ${parser_context.res_var_name},
        Pos,
        ${parser_context.pos_var_name});

   % if parser.is_left_recursive():
       <<No_Memo>>
   % endif

   Parser.Current_Pos := ${parser_context.pos_var_name};

   return ${parser_context.res_var_name};
end ${parser.gen_fn_name};
