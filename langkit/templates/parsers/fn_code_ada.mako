## vim: filetype=makoada

<%
ret_type = parser.type.storage_type_name
memo = 'PP.{}_Memo'.format(parser.gen_fn_name)
%>

function ${parser.gen_fn_name}
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return ${ret_type}
is
   use ${ret_type}_Memos;

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

   % if parser.is_left_recursive():
      Mem_Pos : Token_Index := Pos;
      Mem_Res : ${ret_type} := ${parser.type.storage_nullexpr};
   % endif

   PP : constant Parser_Private_Part := +Parser.Private_Part;
   M  : Memo_Entry := Get (${memo}, Pos);

begin
   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      ${parser.res_var} := M.Instance;
      return ${parser.res_var};
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      return ${parser.res_var};
   end if;

   % if parser.is_left_recursive():
       Set (${memo}, False, ${parser.res_var}, Pos, Mem_Pos);

       <<Try_Again>>

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
         Set
           (${memo},
            ${parser.pos_var} /= No_Token_Index,
            ${parser.res_var},
            Pos,
            ${parser.pos_var});
         goto Try_Again;

      elsif Mem_Pos > Pos then
         ${parser.res_var} := Mem_Res;
         ${parser.pos_var} := Mem_Pos;
         goto No_Memo;
      end if;
   % endif

   Set
     (${memo},
      ${parser.pos_var} /= No_Token_Index,
      ${parser.res_var},
      Pos,
      ${parser.pos_var});

   % if parser.is_left_recursive():
       <<No_Memo>>
   % endif

   Parser.Current_Pos := ${parser.pos_var};

   return ${parser.res_var};
end ${parser.gen_fn_name};
