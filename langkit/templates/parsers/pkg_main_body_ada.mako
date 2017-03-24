## vim: filetype=makoada

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Packrat;
with Langkit_Support.Text;    use Langkit_Support.Text;

package body ${ada_lib_name}.Analysis.Parsers is

   --  Prepare packrat instantiations: one per enum type and onefor each kind
   --  of node (including lists). Likewise for bump ptr. allocators, except
   --  we need them only for non-abstract AST nodes.
   --
   --  In the Tagged_Alloc instanciations, there are unchecked conversions to
   --  wrap System.Address values from a low-level allocator. All read/writes
   --  for the pointed values are made through values of the same access types
   --  (i.e. AST node access). Thus, strict aliasing issues should not arise
   --  for these.
   --
   --  See <https://gcc.gnu.org/onlinedocs/gnat_ugn/
   --       Optimization-and-Strict-Aliasing.html>.

   pragma Warnings (Off, "is not referenced");
   pragma Warnings (Off, "possible aliasing problem for type");
   % for enum_type in ctx.enum_types:
      package ${enum_type.name()}_Memos is new Langkit_Support.Packrat
        (${enum_type.name()}, Token_Index);
      use ${enum_type.name()}_Memos;
   % endfor

   % for cls in ctx.astnode_types:
      package ${cls.name()}_Memos is new Langkit_Support.Packrat
        (${cls.name()}, Token_Index);
      use ${cls.name()}_Memos;

      % if not cls.abstract:
         package ${cls.name()}_Alloc is
            new Tagged_Alloc (${cls.value_type_name()});
      % endif
   % endfor
   pragma Warnings (On, "is not referenced");
   pragma Warnings (On, "possible aliasing problem for type");

   % for parser in ctx.generated_parsers:
   ${parser.spec}
   % endfor

   procedure Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True);
   --  Helper for the user parsing function, to be called after a low-level
   --  parsing function. Check_Complete has the same semantics as in Parse. If
   --  the parsing failed (Parser.Current_Pos = No_Token_Index), append
   --  corresponding diagnostics to Parser.Diagnostics, do nothing instead.

   ----------------------
   -- Create_From_File --
   ----------------------

   function Create_From_File
     (Filename, Charset : String;
      Read_BOM          : Boolean;
      Unit              : Analysis_Unit;
      With_Trivia       : Boolean := False)
      return Parser_type
   is
      TDH : Token_Data_Handler_Access renames Token_Data (Unit);
   begin
      Lex_From_Filename (Filename, Charset, Read_BOM, TDH.all, With_Trivia);
      return (Unit            => Unit,
              TDH             => TDH,
              % if ctx.symbol_literals:
              Symbol_Literals =>
                 Unit.Context.Symbol_Literals'Unrestricted_Access,
              % endif
              others          => <>);
   end Create_From_File;

   ------------------------
   -- Create_From_Buffer --
   ------------------------

   function Create_From_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      Unit            : Analysis_Unit;
      With_Trivia     : Boolean := False)
      return Parser_type
   is
      TDH : Token_Data_Handler_Access renames Token_Data (Unit);
   begin
      Lex_From_Buffer (Buffer, Charset, Read_BOM, TDH.all, With_Trivia);
      return (Unit            => Unit,
              TDH             => TDH,
              % if ctx.symbol_literals:
              Symbol_Literals =>
                 Unit.Context.Symbol_Literals'Unrestricted_Access,
              % endif
              others          => <>);
   end Create_From_Buffer;

   ---------------------------
   -- Process_Parsing_Error --
   ---------------------------

   procedure Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True)
   is

      procedure Add_Last_Fail_Diagnostic is
         Last_Token : Lexer.Token_Data_Type renames
            Get_Token (Parser.TDH.all, Parser.Last_Fail.Pos);
         D : constant Diagnostic :=
           (if Parser.Last_Fail.Kind = Token_Fail then
             (Sloc_Range => Last_Token.Sloc_Range,
              Message    => To_Unbounded_Wide_Wide_String (To_Text
                ("Expected "
                 & Token_Error_Image (Parser.Last_Fail.Expected_Token_Id)
                 & ", got "
                 & Token_Error_Image (Parser.Last_Fail.Found_Token_Id))))
            else
              (Sloc_Range => Last_Token.Sloc_Range,
               Message => To_Unbounded_Wide_Wide_String
                 (To_Text (Parser.Last_Fail.Custom_Message.all))));
      begin
         Parser.Diagnostics.Append (D);
      end Add_Last_Fail_Diagnostic;

   begin

      if Parser.Current_Pos = No_Token_Index then
         Add_Last_Fail_Diagnostic;
      elsif Check_Complete
        and then Parser.Current_Pos /= Last_Token (Parser.TDH.all)
      then
         --  If the fail pos is the current position of the parser or after,
         --  it means that the longest parse is the correct result, and that we
         --  have some garbage afterwards.
         if Parser.Current_Pos >= Parser.Last_Fail.Pos then
            declare
               First_Garbage_Token : Lexer.Token_Data_Type renames
                  Get_Token (Parser.TDH.all, Parser.Current_Pos);
               D                   : constant Diagnostic :=
                 (Sloc_Range => First_Garbage_Token.Sloc_Range,
                  Message    => To_Unbounded_Wide_Wide_String (To_Text
                    ("End of input expected, got """
                     & Token_Kind_Name (First_Garbage_Token.Kind)
                     & """")));
            begin
               Parser.Diagnostics.Append (D);
            end;

         --  Else, the last fail pos is further down the line, and we want to
         --  have the diagnostic of what exactly failed.
         else
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
      % for name in ctx.user_rule_names:
         when ${Name.from_lower(name)}_Rule =>
            Result := ${root_node_type_name}
              (${ctx.grammar.rules[name].gen_fn_name}
                 (Parser, First_Token_Index));
      % endfor
      end case;
      Process_Parsing_Error (Parser, Check_Complete);
      Clean_All_Memos;
      Set_Parents (Result, null);
      return Result;
   end Parse;

   % for parser in ctx.generated_parsers:
   ${parser.body}
   % endfor

   ---------------------
   -- Clean_All_Memos --
   ---------------------

   procedure Clean_All_Memos is
   begin
      % for fn in ctx.fns:
         Clear (${fn}_Memo);
      % endfor
   end Clean_All_Memos;

end ${ada_lib_name}.Analysis.Parsers;
