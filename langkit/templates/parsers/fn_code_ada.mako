## vim: filetype=makoada

<%
ret_type = parser.type.storage_type_name
memo = 'PP.{}_Memo'.format(parser.gen_fn_name)
%>

function ${parser.gen_fn_name}
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return ${ret_type}
is
   % for name, typ in var_context:
      <%
         default_expr_suffix = ""
         if isinstance(typ, str):
            type_expr = typ
         else:
            type_expr = typ.storage_type_name
            if typ.storage_nullexpr:
               default_expr_suffix = f" := {typ.storage_nullexpr}"
      %>
      ${name} : ${type_expr}${default_expr_suffix};
   % endfor

   Mark_On_Entry : constant Diagnostic_Mark := Parser.Last_Diag;

   % if parser.is_left_recursive():
      Mem_Pos  : Token_Index := Pos;
      Mem_Res  : ${ret_type} := ${parser.type.storage_nullexpr};
      Mem_Mark : Diagnostic_Mark := Mark_On_Entry;
   % endif

   PP : constant Parser_Private_Part := +Parser.Private_Part;
   M  : Memos.Memo_Entry := Memos.Get (${memo}, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      Append_Group (Parser.Pool, Parser.Last_Diag, M.Mark);
      ${parser.res_var} := M.Instance;
      return ${parser.res_var};
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Set_Last_Fail (Parser, M);
      return ${parser.res_var};
   end if;

   Parser.Last_Diag := No_Diagnostic;

   % if parser.is_left_recursive():
       Set_Failure
         (${memo},
          Pos,
          Parser.Last_Fail,
          Parser.Last_Diag);

       <<Try_Again>>
       Parser.Last_Diag := No_Diagnostic;

      ## Reinit variables that need it
      % for var in var_context:
      % if var.reinit:
      ${var.name} := ${var.type.storage_nullexpr};
      % endif
      % endfor

   % endif

   ---------------------------
   -- MAIN COMBINATORS CODE --
   ---------------------------

   ${code}

   -------------------------------
   -- END MAIN COMBINATORS CODE --
   -------------------------------

   % if parser.is_left_recursive():
      if ${parser.pos_var} > Mem_Pos then
         Mem_Pos := ${parser.pos_var};
         Mem_Res := ${parser.res_var};
         Mem_Mark := Parser.Last_Diag;
         if ${parser.pos_var} = No_Token_Index then
            Set_Failure (${memo}, Pos, Parser.Last_Fail, Mem_Mark);
         else
            Memos.Set_Success
              (${memo},
               Pos,
               ${parser.res_var},
               Mem_Mark,
               ${parser.pos_var});
         end if;
         goto Try_Again;

      elsif Mem_Pos > Pos then
         ${parser.res_var} := Mem_Res;
         ${parser.pos_var} := Mem_Pos;
         Parser.Last_Diag := Mem_Mark;
         goto No_Memo;
      end if;
   % endif

   if ${parser.pos_var} = No_Token_Index then
      Set_Failure
        (${memo}, Pos, Parser.Last_Fail, Parser.Last_Diag);
   else
      Memos.Set_Success
        (${memo}, Pos, ${parser.res_var}, Parser.Last_Diag, ${parser.pos_var});
   end if;

   % if parser.is_left_recursive():
       <<No_Memo>>
   % endif

   declare
      Top : constant Diagnostic_Mark := Parser.Last_Diag;
   begin
      Parser.Last_Diag := Mark_On_Entry;
      Append_Group (Parser.Pool, Parser.Last_Diag, Top);
   end;
   Parser.Current_Pos := ${parser.pos_var};

   return ${parser.res_var};
end ${parser.gen_fn_name};
