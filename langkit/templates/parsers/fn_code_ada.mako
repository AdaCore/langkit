## vim: filetype=makoada

<%
ret_type = parser.type.storage_type_name
memo = 'Parser.Private_Part.{}_Memo'.format(parser.gen_fn_name)
%>

function ${parser.gen_fn_name}
  (Parser : in out Parser_Type;
   Pos    : Token_Index) return ${ret_type}
is
   use ${ret_type}_Memos;

   Call_Depth : aliased Natural;

   % for name, typ in var_context:
      ${name} :
         % if isinstance(typ, str):
            ${typ};
         % else:
            ${typ.storage_type_name}
            % if typ.storage_nullexpr:
               := ${typ.storage_nullexpr};
            % endif
         % endif
   % endfor

   % if parser.is_left_recursive():
      Mem_Pos : Token_Index := Pos;
      Mem_Res : ${ret_type} := ${parser.type.storage_nullexpr};
   % endif

   M : Memo_Entry := Get (${memo}, Pos);

begin
   Enter_Call (Parser, Call_Depth'Access);

   if M.State = Success then
      Parser.Current_Pos := M.Final_Pos;
      ${parser.res_var} := M.Instance;
      Exit_Call (Parser, Call_Depth);
      return ${parser.res_var};
   elsif M.State = Failure then
      Parser.Current_Pos := No_Token_Index;
      Exit_Call (Parser, Call_Depth);
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

   Exit_Call (Parser, Call_Depth);
   return ${parser.res_var};

exception
   when others =>
      Exit_Call (Parser, Call_Depth);
      raise;
end ${parser.gen_fn_name};
