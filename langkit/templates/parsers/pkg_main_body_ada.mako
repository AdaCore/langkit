## vim: filetype=makoada

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Packrat;
with Langkit_Support.Text;    use Langkit_Support.Text;

with ${get_context().ada_api_settings.lib_name}.Lexer;
use ${get_context().ada_api_settings.lib_name}.Lexer;

package body ${_self.ada_api_settings.lib_name}.AST.Types.Parsers is

   --  Prepare packrat instantiations: one per enum type and onefor each kind
   --  of node (including lists).

   % for enum_type in _self.enum_types:
      package ${enum_type.name()}_Memos is new Langkit_Support.Packrat
        (${enum_type.name()});
      use ${enum_type.name()}_Memos;
   % endfor

   % for cls in _self.astnode_types:
      package ${cls.name()}_Memos is new Langkit_Support.Packrat
        (${cls.name()});
      use ${cls.name()}_Memos;

      % if cls in _self.list_types:
         package List_${cls.name()}_Memos is new Langkit_Support.Packrat
           (List_${cls.name()});
         use List_${cls.name()}_Memos;
      % endif
   % endfor

   % for parser in _self.generated_parsers:
   ${parser.spec}
   % endfor

   procedure Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True);
   --  Helper for the user parsing function, to be called after a low-level
   --  parsing function. Check_Complete has the same semantics as in Parse. If
   --  the parsing failed (Parser.Current_Pos = -1), append corresponding
   --  diagnostics to Parser.Diagnostics, do nothing instead.

   ----------------------
   -- Create_From_File --
   ----------------------

   function Create_From_File
     (Filename, Charset : String;
      Unit              : Analysis_Unit_Interface;
      With_Trivia       : Boolean := False)
      return Parser_type
   is
      TDH : Token_Data_Handler renames Unit.Token_Data.all;
   begin
      Lex_From_Filename (Filename, Charset, TDH, With_Trivia);
      return (Unit => Unit, TDH => Unit.Token_Data, others => <>);
   end Create_From_File;

   ------------------------
   -- Create_From_Buffer --
   ------------------------

   function Create_From_Buffer
     (Buffer, Charset : String;
      Unit            : Analysis_Unit_Interface;
      With_Trivia     : Boolean := False)
      return Parser_type
   is
      TDH : Token_Data_Handler renames Unit.Token_Data.all;
   begin
      Lex_From_Buffer (Buffer, Charset, TDH, With_Trivia);
      return (Unit => Unit, TDH => Unit.Token_Data, others => <>);
   end Create_From_Buffer;

   ---------------------------
   -- Process_Parsing_Error --
   ---------------------------

   procedure Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True)
   is

      procedure Add_Last_Fail_Diagnostic is
         Last_Token : Token_Raw_Data_Type renames
            Get_Token (Parser.TDH.all, Parser.Last_Fail.Pos);
         D : constant Diagnostic :=
           (Sloc_Range => Last_Token.Sloc_Range,
            Message    => To_Unbounded_Wide_Wide_String (To_Text
              ("Expected """
               & Token_Kind_Name (Parser.Last_Fail.Expected_Token_Id)
               & """, got """
               & Token_Kind_Name (Parser.Last_Fail.Found_Token_Id)
               & """")));
      begin
         Parser.Diagnostics.Append (D);
      end Add_Last_Fail_Diagnostic;

   begin

      if Parser.Current_Pos = -1 then
         Add_Last_Fail_Diagnostic;
      elsif Check_Complete
        and then Parser.Current_Pos /= Last_Token (Parser.TDH.all)
      then
         --  If the fail pos is the current position of the parser, it means
         --  that the longest parse is the correct result, and that we have
         --  some garbage afterwards.
         if Parser.Current_Pos = Parser.Last_Fail.Pos then
            declare
               First_Garbage_Token : Token_Raw_Data_Type renames
                  Get_Token (Parser.TDH.all, Parser.Current_Pos);
               Token_Id            : constant Token_Kind :=
                  Token_Kind'val (First_Garbage_Token.Id);
               D                   : constant Diagnostic :=
                 (Sloc_Range => First_Garbage_Token.Sloc_Range,
                  Message    => To_Unbounded_Wide_Wide_String (To_Text
                    ("End of input expected, got """
                     & Token_Kind_Name (Token_Id)
                     & """")));
            begin
               Parser.Diagnostics.Append (D);
            end;
         --  Else, the last fail pos is further down the line, and we want to
         --  have the diagnostic of what exactly failed.
         else
            pragma Assert (Parser.Current_Pos < Parser.Last_Fail.Pos);
            Add_Last_Fail_Diagnostic;
         end if;
      end if;
   end Process_Parsing_Error;

   -----------
   -- Parse --
   -----------

   function Parse
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True;
      Rule           : Grammar_Rule)
      return ${root_node_type_name}
   is
      Result : ${root_node_type_name};
   begin
      case Rule is
      % for name in _self.user_rule_names:
         when ${Name.from_lower(name)}_Rule =>
            Result := ${root_node_type_name}
              (${_self.rules_to_fn_names[name].gen_fn_name} (Parser, 0));
      % endfor
      end case;
      Process_Parsing_Error (Parser, Check_Complete);
      Clean_All_Memos;
      return Result;
   end Parse;

   % for parser in _self.generated_parsers:
   ${parser.body}
   % endfor

   ---------------------
   -- Clean_All_Memos --
   ---------------------

   procedure Clean_All_Memos is
   begin
      % for fn in _self.fns:
         Clear (${fn}_Memo);
      % endfor
   end Clean_All_Memos;

end ${_self.ada_api_settings.lib_name}.AST.Types.Parsers;
