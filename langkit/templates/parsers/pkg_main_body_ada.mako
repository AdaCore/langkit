## vim: filetype=makoada

with Ada.Unchecked_Deallocation;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Packrat;
with Langkit_Support.Text;    use Langkit_Support.Text;

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;

<% sorted_fns = sorted(ctx.fns, key=lambda f: f.gen_fn_name) %>

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
   % for cls in ctx.astnode_types:
      package ${cls.name}_Memos is new Langkit_Support.Packrat
        (${cls.name}, Token_Index);

      % if not cls.abstract:
         package ${cls.name}_Alloc is
            new Tagged_Alloc (${cls.value_type_name()});
      % endif
   % endfor
   pragma Warnings (On, "is not referenced");
   pragma Warnings (On, "possible aliasing problem for type");

   type Free_Parse_List_Record;
   type Free_Parse_List is access all Free_Parse_List_Record;
   --  Cache of temporary lists of AST nodes used in List parsers

   type Free_Parse_List_Record is record
      Nodes : ${ctx.root_grammar_class.array.pkg_vector}.Vector;
      Next  : Free_Parse_List;
   end record;

   type Parser_Private_Part_Type is record
      Parse_Lists : Free_Parse_List;

      % for parser in sorted_fns:
      <% ret_type = parser.get_type().storage_type_name %>
      ${parser.gen_fn_name}_Memo : ${ret_type}_Memos.Memo_Type;
      % endfor
   end record;

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

   function Get_Parse_List (Parser : Parser_Type) return Free_Parse_List;
   --  Get a free parse list, or allocate one if there is no free parse list in
   --  Parser. When done with the result, the caller must invoke
   --  Release_Parse_List.

   procedure Release_Parse_List
     (Parser : Parser_Type; List : in out Free_Parse_List);
   --  Release a parse list, putting it in Parsers' free list. Set List to
   --  null.

   ---------------------------
   -- Init_Parser_From_File --
   ---------------------------

   procedure Init_Parser_From_File
     (Filename, Charset : String;
      Read_BOM          : Boolean;
      Unit              : Analysis_Unit;
      TDH               : Token_Data_Handler_Access;
      Symbol_Literals   : Symbol_Literal_Array_Access;
      With_Trivia       : Boolean;
      Parser            : in out Parser_Type) is
   begin
      Reset (Parser);
      Lex_From_Filename (Filename, Charset, Read_BOM, TDH.all,
                         Parser.Diagnostics, With_Trivia);
      Parser.Unit := Unit;
      Parser.TDH := TDH;
      Parser.Symbol_Literals := Symbol_Literals;
   end Init_Parser_From_File;

   -----------------------------
   -- Init_Parser_From_Buffer --
   -----------------------------

   procedure Init_Parser_From_Buffer
     (Buffer, Charset : String;
      Read_BOM        : Boolean;
      Unit            : Analysis_Unit;
      TDH             : Token_Data_Handler_Access;
      Symbol_Literals : Symbol_Literal_Array_Access;
      With_Trivia     : Boolean;
      Parser          : in out Parser_Type) is
   begin
      Reset (Parser);
      Lex_From_Buffer (Buffer, Charset, Read_BOM, TDH.all,
                       Parser.Diagnostics, With_Trivia);
      Parser.Unit := Unit;
      Parser.TDH := TDH;
      Parser.Symbol_Literals := Symbol_Literals;
   end Init_Parser_From_Buffer;

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
             Create (Last_Token.Sloc_Range, To_Text
               ("Expected "
                & Token_Error_Image (Parser.Last_Fail.Expected_Token_Id)
                & ", got "
                & Token_Error_Image (Parser.Last_Fail.Found_Token_Id)))
            else
              Create (Last_Token.Sloc_Range,
                      To_Text (Parser.Last_Fail.Custom_Message.all)));
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
            begin
               Append (Parser.Diagnostics, First_Garbage_Token.Sloc_Range,
                       To_Text ("End of input expected, got """
                                & Token_Kind_Name (First_Garbage_Token.Kind)
                                & """"));
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
      Rule           : Grammar_Rule) return Parsed_Node
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
      Set_Parents (Result, null);
      return Parsed_Node (Result);
   end Parse;

   % for parser in ctx.generated_parsers:
   ${parser.body}
   % endfor

   -----------
   -- Reset --
   -----------

   procedure Reset (Parser : in out Parser_Type) is
      New_Parser : Parser_Type;
      --  We create this new parser instance to leverage creation of default
      --  values, so as to not repeat them.
   begin
      --  We just keep the private part, to not have to reallocate it
      New_Parser.Private_Part := Parser.Private_Part;

      --  And then reset everything else
      Parser := New_Parser;

      --  Reset the memo tables in the private part
      % for fn in sorted_fns:
         ${fn.get_type().storage_type_name}_Memos.Clear
           (Parser.Private_Part.${fn.gen_fn_name}_Memo);
      % endfor
   end Reset;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Parser : in out Parser_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Parser_Private_Part_Type, Parser_Private_Part);
      procedure Free is new Ada.Unchecked_Deallocation
        (Free_Parse_List_Record, Free_Parse_List);

      Cur : Free_Parse_List renames Parser.Private_Part.Parse_Lists;
   begin
      while Cur /= null loop
         declare
            Next : constant Free_Parse_List := Cur.Next;
         begin
            Cur.Nodes.Destroy;
            Free (Cur);
            Cur := Next;
         end;
      end loop;
      Free (Parser.Private_Part);
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Parser : in out Parser_Type) is
   begin
      Parser.Private_Part := new Parser_Private_Part_Type'(others => <>);
   end Initialize;

   --------------------
   -- Get_Parse_List --
   --------------------

   function Get_Parse_List (Parser : Parser_Type) return Free_Parse_List is
      Lists  : Free_Parse_List renames Parser.Private_Part.Parse_Lists;
      Result : Free_Parse_List;
   begin
      if Lists = null then
         Result := new Free_Parse_List_Record;

      else
         Result := Lists;
         Lists := Lists.Next;
      end if;

      return Result;
   end Get_Parse_List;

   ------------------------
   -- Release_Parse_List --
   ------------------------

   procedure Release_Parse_List
     (Parser : Parser_Type; List : in out Free_Parse_List)
   is
      Lists  : Free_Parse_List renames Parser.Private_Part.Parse_Lists;
   begin
      List.Nodes.Clear;
      List.Next := Lists;
      Lists := List;
      List := null;
   end Release_Parse_List;

end ${ada_lib_name}.Analysis.Parsers;
