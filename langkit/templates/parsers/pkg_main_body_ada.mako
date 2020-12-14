## vim: filetype=makoada

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Packrat;

pragma Warnings (Off, "referenced");
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
pragma Warnings (On, "referenced");

with Langkit_Support.Text;        use Langkit_Support.Text;

with ${ada_lib_name}.Common;             use ${ada_lib_name}.Common;
use ${ada_lib_name}.Common.Precomputed_Symbols;

with ${ada_lib_name}.Implementation;     use ${ada_lib_name}.Implementation;

pragma Warnings (Off, "referenced");
with ${ada_lib_name}.Private_Converters; use ${ada_lib_name}.Private_Converters;
pragma Warnings (On, "referenced");

<% sorted_fns = sorted(ctx.fns, key=lambda f: f.gen_fn_name) %>

package body ${ada_lib_name}.Parsers is
   pragma Warnings (Off, "use clause");
   use all type Langkit_Support.Symbols.Symbol_Type;
   pragma Warnings (On, "use clause");

   --  Prepare packrat instantiations: one per enum type and onefor each kind
   --  of node (including lists). Likewise for bump ptr. allocators, except
   --  we need them only for non-abstract AST nodes.
   --
   --  In the Alloc instanciations, there are unchecked conversions to wrap
   --  System.Address values from a low-level allocator. All read/writes for
   --  the pointed values are made through values of the same access types
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
         <%
            subtype = 'Subtype_For_{}'.format(cls.kwless_raw_name)
            access = 'Access_To_{}'.format(subtype)
         %>
         subtype ${subtype} is
            ${T.root_node.value_type_name} (${cls.ada_kind_name});
         type ${access} is access all ${subtype};
         package ${cls.name}_Alloc is new Alloc
           (${subtype}, ${access});

         function ${cls.parser_allocator}
           (Pool : Bump_Ptr_Pool) return ${cls.name};

         function ${cls.parser_allocator}
           (Pool : Bump_Ptr_Pool) return ${cls.name}
         is
            Result      : constant ${access} := ${cls.name}_Alloc.Alloc (Pool);
            Result_Kind : ${T.node_kind}
               with Import, Address => Result.Kind'Address;
            --  Result.Kind is a discriminant, so we can't modify it directly.
            --  We need to initialize it manually, though, as we don't use a
            --  standard Ada allocator for nodes. Use an overlay to workaround
            --  Ada's restrictions.
         begin
            Result_Kind := ${cls.ada_kind_name};
            return ${cls.name} (Result);
         end ${cls.parser_allocator};

      % endif
   % endfor
   pragma Warnings (On, "is not referenced");
   pragma Warnings (On, "possible aliasing problem for type");

   procedure Initialize_List
     (Self   : ${ctx.generic_list_type.name};
      Parser : Parser_Type;
      Count  : Natural);
   --  Helper for parsers, to initialize the list of children in a freshly
   --  allocated list node.

   type Dontskip_Parser_Function is access function
     (Parser : in out Parser_Type;
      Pos    : Token_Index)
      return ${ctx.root_grammar_class.storage_type_name};

   package Dont_Skip_Fn_Vectors
   is new Ada.Containers.Vectors (Natural, Dontskip_Parser_Function);

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
      <% ret_type = parser.type.storage_type_name %>
      ${parser.gen_fn_name}_Memo : ${ret_type}_Memos.Memo_Type;
      % endfor

      Dont_Skip : Dont_Skip_Fn_Vectors.Vector;
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

   procedure Add_Last_Fail_Diagnostic (Parser : in out Parser_Type);
   --  Add a diagnostic for the last fail position of the parser

   function Get_Parse_List (Parser : Parser_Type) return Free_Parse_List;
   --  Get a free parse list, or allocate one if there is no free parse list in
   --  Parser. When done with the result, the caller must invoke
   --  Release_Parse_List.

   procedure Release_Parse_List
     (Parser : Parser_Type; List : in out Free_Parse_List);
   --  Release a parse list, putting it in Parsers' free list. Set List to
   --  null.

   ---------------------
   -- Initialize_List --
   ---------------------

   procedure Initialize_List
     (Self   : ${ctx.generic_list_type.name};
      Parser : Parser_Type;
      Count  : Natural) is
   begin
      Self.Count := Count;
      Self.Nodes := Alloc_AST_List_Array.Alloc (Parser.Mem_Pool, 0);
   end Initialize_List;

   -----------------
   -- Init_Parser --
   -----------------

   procedure Init_Parser
     (Input       : Internal_Lexer_Input;
      With_Trivia : Boolean;
      Unit        : access Implementation.Analysis_Unit_Type;
      TDH         : Token_Data_Handler_Access;
      Parser      : in out Parser_Type) is
   begin
      Reset (Parser);
      Extract_Tokens
        (Input, With_Trivia, TDH.all, Parser.Diagnostics);
      Parser.Unit := Unit;
      Parser.TDH := TDH;
   end Init_Parser;

   ------------------------------
   -- Add_Last_Fail_Diagnostic --
   ------------------------------

   procedure Add_Last_Fail_Diagnostic (Parser : in out Parser_Type)
   is
      Last_Token : Stored_Token_Data renames
         Get_Token (Parser.TDH.all, Parser.Last_Fail.Pos);
      D : constant Diagnostic :=
        (if Parser.Last_Fail.Kind = Token_Fail then
          Create (Sloc_Range (Parser.TDH.all, Last_Token), To_Text
            ("Expected "
             & Token_Error_Image (Parser.Last_Fail.Expected_Token_Id)
             & ", got "
             & Token_Error_Image (Parser.Last_Fail.Found_Token_Id)))
         else
           Create (Sloc_Range (Parser.TDH.all, Last_Token),
                   To_Text (Parser.Last_Fail.Custom_Message.all)));
   begin
      Parser.Diagnostics.Append (D);
   end Add_Last_Fail_Diagnostic;

   ---------------------------
   -- Process_Parsing_Error --
   ---------------------------

   procedure Process_Parsing_Error
     (Parser         : in out Parser_Type;
      Check_Complete : Boolean := True) is
   begin

      if Parser.Current_Pos = No_Token_Index then
         Add_Last_Fail_Diagnostic (Parser);
      elsif Check_Complete
        and then Parser.Current_Pos /= Last_Token (Parser.TDH.all)
      then
         --  If the fail pos is the current position of the parser or after,
         --  it means that the longest parse is the correct result, and that we
         --  have some garbage afterwards.
         if Parser.Current_Pos >= Parser.Last_Fail.Pos then
            declare
               First_Garbage_Token : Stored_Token_Data renames
                  Get_Token (Parser.TDH.all, Parser.Current_Pos);
            begin
               Append
                 (Parser.Diagnostics,
                  Sloc_Range (Parser.TDH.all, First_Garbage_Token),
                  To_Text
                    ("End of input expected, got """
                     & Token_Kind_Name
                         (To_Token_Kind (First_Garbage_Token.Kind))
                     & """"));
            end;

         --  Else, the last fail pos is further down the line, and we want to
         --  have the diagnostic of what exactly failed.
         else
            Add_Last_Fail_Diagnostic (Parser);
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
      Result : ${T.root_node.name};
   begin
      case Rule is
      % for name in ctx.grammar.user_defined_rules:
         when ${ctx.grammar_rule_api_name(name)} =>
            Result := ${ctx.grammar.rules[name].gen_fn_name}
              (Parser, First_Token_Index);
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
         ${fn.type.storage_type_name}_Memos.Clear
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

end ${ada_lib_name}.Parsers;
