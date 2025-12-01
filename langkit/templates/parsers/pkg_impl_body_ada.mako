## vim: filetype=makoada

with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System;

with Langkit_Support.Bump_Ptr;    use Langkit_Support.Bump_Ptr;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Packrat;     use Langkit_Support.Packrat;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;

pragma Warnings (Off, "referenced");
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
pragma Warnings (On, "referenced");

with Langkit_Support.Text;        use Langkit_Support.Text;

with ${ada_lib_name}.Common;         use ${ada_lib_name}.Common;
with ${ada_lib_name}.Implementation; use ${ada_lib_name}.Implementation;
use ${ada_lib_name}.Implementation.Precomputed_Symbols;
with ${ada_lib_name}.Lexer_Implementation;
use ${ada_lib_name}.Lexer_Implementation;

pragma Warnings (Off, "referenced");
with ${ada_lib_name}.Private_Converters; use ${ada_lib_name}.Private_Converters;
pragma Warnings (On, "referenced");

% if ctx.grammar.uses_external_properties:
with ${ada_lib_name}.Implementation.Extensions;
% endif

<% sorted_fns = sorted(ctx.fns, key=lambda f: f.gen_fn_name) %>

package body ${ada_lib_name}.Parsers_Impl is
   pragma Warnings (Off, "use clause");
   use all type Langkit_Support.Symbols.Symbol_Type;
   pragma Warnings (On, "use clause");

   --  Prepare packrat instantiations: one per enum type and onefor each kind
   --  of node (including lists). Likewise for bump ptr. allocators, except
   --  we need them only for non-abstract AST nodes.

   pragma Warnings (Off, "is not referenced");
   package Memos is new Langkit_Support.Packrat.Tables
     (${T.root_node.name}, null, Token_Kind);

   % for cls in ctx.node_types:
      % if not cls.abstract:
         <%
            subtype = 'Subtype_For_{}'.format(cls.kwless_raw_name)
            access = 'Access_To_{}'.format(subtype)
         %>
         subtype ${subtype} is
            ${T.root_node.value_type_name} (${cls.ada_kind_name});
         type ${access} is access all ${subtype};
         pragma No_Strict_Aliasing (${access});
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

   procedure Set_Failure
     (Memo     : in out Memos.Memo_Type;
      Pos      : Token_Index;
      Info     : Fail_Info;
      Mark     : Diagnostic_Mark);
   --  Convenience wrapper for ``Memos.Set_Failure``

   procedure Set_Last_Fail
     (Parser : in out Parser_Type; M : Memos.Memo_Entry);
   --  Set ``Parser.Last_Fail`` from a failing memo entry

   type Dontskip_Parser_Function is access function
     (Parser : in out Parser_Type;
      Pos    : Token_Index) return ${ctx.root_node_type.storage_type_name};

   package Dont_Skip_Fn_Vectors
   is new Ada.Containers.Vectors (Natural, Dontskip_Parser_Function);

   type Free_Parse_List_Record;
   type Free_Parse_List is access all Free_Parse_List_Record;
   --  Cache of temporary lists of AST nodes used in List parsers

   type Free_Parse_List_Record is record
      Nodes : ${ctx.root_node_type.array.pkg_vector}.Vector;
      Next  : Free_Parse_List;
   end record;

   type Parser_Private_Part_Type is record
      Parse_Lists : Free_Parse_List;

      % for parser in sorted_fns:
      <% ret_type = parser.type.storage_type_name %>
      ${parser.gen_fn_name}_Memo : Memos.Memo_Type;
      % endfor

      Dont_Skip : Dont_Skip_Fn_Vectors.Vector;
   end record;
   type Parser_Private_Part is access all Parser_Private_Part_Type;
   pragma No_Strict_Aliasing (Parser_Private_Part);
   function "+" is new Ada.Unchecked_Conversion
     (System.Address, Parser_Private_Part);
   function "+" is new Ada.Unchecked_Conversion
     (Parser_Private_Part, System.Address);

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

   pragma Warnings (Off, "is not referenced");

   -------------------------
   -- Diagnostics helpers --
   -------------------------

   procedure Append
     (Parser : in out Parser_Type; Location : Token_Index; Message : String);

   procedure Add_Last_Fail_Diagnostic (Parser : in out Parser_Type);
   --  Add a diagnostic for the last fail position of the parser

   procedure Import_Diagnostics (Parser : in out Parser_Type);
   --  Import the chain of diagnostics from ``Parser.Last_Diag`` to
   --  ``Parser.Diagnostics``.

   ------------------
   -- List helpers --
   ------------------

   function Get_Parse_List (Parser : Parser_Type) return Free_Parse_List;
   --  Get a free parse list, or allocate one if there is no free parse list in
   --  Parser. When done with the result, the caller must invoke
   --  Release_Parse_List.

   procedure Release_Parse_List
     (Parser : Parser_Type; List : in out Free_Parse_List);
   --  Release a parse list, putting it in Parsers' free list. Set List to
   --  null.

   procedure Initialize_List
     (Self   : ${ctx.generic_list_type.name};
      Parser : Parser_Type;
      Count  : Natural);
   --  Helper for parsers, to initialize the list of children in a freshly
   --  allocated list node.

   pragma Warnings (On, "is not referenced");

   -----------------
   -- Set_Failure --
   -----------------

   procedure Set_Failure
     (Memo     : in out Memos.Memo_Type;
      Pos      : Token_Index;
      Info     : Fail_Info;
      Mark     : Diagnostic_Mark)
   is
      Expected_Token_Id : Token_Kind := Token_Kind'First;
      Found_Token_Id    : Token_Kind := Token_Kind'First;
   begin
      if Info.Data.Kind = Token_Fail then
         Expected_Token_Id := Info.Data.Expected_Token_Id;
         Found_Token_Id := Info.Data.Found_Token_Id;
      end if;

      Memos.Set_Failure
        (Memo,
         Pos,
         Mark,
         Info.Data.Kind,
         Info.Pos,
         Expected_Token_Id,
         Found_Token_Id);
   end Set_Failure;

   -------------------
   -- Set_Last_Fail --
   -------------------

   procedure Set_Last_Fail (Parser : in out Parser_Type; M : Memos.Memo_Entry)
   is
   begin
      Parser.Last_Fail.Pos := M.Final_Pos;
      case M.Fail_Kind is
         when Token_Fail =>
            Parser.Last_Fail.Data :=
              (Token_Fail, M.Expected_Token_Id, M.Found_Token_Id);
         when Predicate_Fail =>
            Parser.Last_Fail.Data := (Kind => Predicate_Fail);
      end case;
   end Set_Last_Fail;

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
     (Input         : Lexer_Input;
      With_Trivia   : Boolean;
      Unit          : access Implementation.Analysis_Unit_Type;
      TDH           : Token_Data_Handler_Access;
      Parser        : in out Parser_Type;
      Old_TDH       : access constant Token_Data_Handler;
      Same_Contents : out Boolean)
   is
      --  Never try to use file readers for internal units: these are generally
      --  not actual source files, and file readers, which are external users
      --  of the generated library, have no reason to be aware of them.

      FR : constant Internal_File_Reader_Access :=
        (if Unit.Is_Internal
         then null
         else Unit.Context.File_Reader);
   begin
      Reset (Parser);
      Extract_Tokens
        (Input,
         With_Trivia,
         FR,
         TDH.all,
         Parser.Diagnostics,
         Old_TDH,
         Same_Contents);
      if Same_Contents then
         return;
      end if;
      Parser.Unit := Unit;
      Parser.TDH := TDH;
   end Init_Parser;

   ------------
   -- Append --
   ------------

   procedure Append
     (Parser : in out Parser_Type; Location : Token_Index; Message : String) is
   begin
      Append
        (Self     => Parser.Pool,
         Mark     => Parser.Last_Diag,
         Location => Sloc_Range
                       (Parser.TDH.all, Get_Token (Parser.TDH.all, Location)),
         Message  => To_Unbounded_Text (To_Text (Message)));
   end Append;

   ------------------------------
   -- Add_Last_Fail_Diagnostic --
   ------------------------------

   procedure Add_Last_Fail_Diagnostic (Parser : in out Parser_Type) is
   begin
      Append
        (Parser,
         Parser.Last_Fail.Pos,
         Message =>
           (if Parser.Last_Fail.Data.Kind = Token_Fail
            then "Expected "
                 & Token_Error_Image (Parser.Last_Fail.Data.Expected_Token_Id)
                 & ", got "
                 & Token_Error_Image (Parser.Last_Fail.Data.Found_Token_Id)
            else "Syntax error"));
   end Add_Last_Fail_Diagnostic;

   ------------------------
   -- Import_Diagnostics --
   ------------------------

   procedure Import_Diagnostics (Parser : in out Parser_Type) is
      procedure Append_One (D : Langkit_Support.Diagnostics.Diagnostic);

      ----------------
      -- Append_One --
      ----------------

      procedure Append_One (D : Langkit_Support.Diagnostics.Diagnostic) is
      begin
         Parser.Diagnostics.Append (D);
      end Append_One;
   begin
      Iterate (Parser.Pool, Parser.Last_Diag, Append_One'Access);
   end Import_Diagnostics;

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
                 (Parser,
                  Parser.Current_Pos,
                  "End of input expected, got """
                  & Token_Kind_Name (To_Token_Kind (First_Garbage_Token.Kind))
                  & """");
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
      Import_Diagnostics (Parser);
      return Parsed_Node (Result);
   exception
      when Exc : ${ctx.property_exception_matcher} =>
         Import_Diagnostics (Parser);
         Append
           (Parser.Diagnostics,
            No_Source_Location_Range,
            To_Text ("Error during parsing: "
                     & Ada.Exceptions.Exception_Message (Exc)));
         return Parsed_Node (${T.root_node.nullexpr});
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

      PP : Parser_Private_Part;
   begin
      --  We just keep the private part, to not have to reallocate it
      New_Parser.Private_Part := Parser.Private_Part;
      New_Parser.Last_Diag := No_Diagnostic;

      --  And then reset everything else
      Parser := New_Parser;

      --  Reset the memo tables in the private part
      PP := +Parser.Private_Part;
      % for fn in sorted_fns:
         Memos.Clear (PP.${fn.gen_fn_name}_Memo);
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

      PP  : Parser_Private_Part := +Parser.Private_Part;
      Cur : Free_Parse_List renames PP.Parse_Lists;
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
      Free (PP);
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Parser : in out Parser_Type) is
      PP : constant Parser_Private_Part :=
        new Parser_Private_Part_Type'(others => <>);
   begin
      Parser.Private_Part := +PP;
   end Initialize;

   --------------------
   -- Get_Parse_List --
   --------------------

   function Get_Parse_List (Parser : Parser_Type) return Free_Parse_List is
      PP     : constant Parser_Private_Part := +Parser.Private_Part;
      Lists  : Free_Parse_List renames PP.Parse_Lists;
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
      PP    : constant Parser_Private_Part := +Parser.Private_Part;
      Lists : Free_Parse_List renames PP.Parse_Lists;
   begin
      List.Nodes.Clear;
      List.Next := Lists;
      Lists := List;
      List := null;
   end Release_Parse_List;

   ----------
   -- Dump --
   ----------

   procedure Dump (Parser : Parser_Type) is

      ${ada_enum_type_decl(
         "Any_Parser", [p.gen_fn_name for p in sorted_fns], 6
      )}

      type Memo_Entry_Key is record
         Offset : Token_Index;
         Parser : Any_Parser;
      end record;

      function "<" (Left, Right : Memo_Entry_Key) return Boolean
      is (Left.Offset < Right.Offset
          or else (Left.Offset = Right.Offset
                   and then Left.Parser < Right.Parser));

      type Memo_Entry is record
         State     : Memo_State;
         Instance  : ${T.root_node.name};
         Mark      : Diagnostic_Mark;
         Final_Pos : Token_Index;
      end record;

      package Memo_Entry_Maps is new Ada.Containers.Ordered_Maps
        (Key_Type => Memo_Entry_Key, Element_Type => Memo_Entry);

      TDH          : Token_Data_Handler renames Parser.TDH.all;
      PP           : constant Parser_Private_Part := +Parser.Private_Part;
      Memo_Entries : Memo_Entry_Maps.Map;
   begin
      Put_Line ("Current_Pos:" & Parser.Current_Pos'Image);
      if Parser.Last_Fail.Pos /= No_Token_Index then
         Put_Line ("Last_Fail:");
         Put_Line ("  Pos:" & Parser.Last_Fail.Pos'Image);
         Put_Line ("  Kind: " & Parser.Last_Fail.Data.Kind'Image);
         case Parser.Last_Fail.Data.Kind is
         when Token_Fail =>
            Put_Line
              ("  Expected_Token_Id: "
               & Parser.Last_Fail.Data.Expected_Token_Id'Image);
            Put_Line
              ("  Found_Token_Id: "
               & Parser.Last_Fail.Data.Found_Token_Id'Image);
         when Predicate_Fail =>
            null;
         end case;
      end if;
      if Parser.Last_Diag /= No_Diagnostic then
         Put_Line ("Diagnostics:");
         declare
            procedure Process (D : Diagnostic);

            -------------
            -- Process --
            -------------

            procedure Process (D : Diagnostic) is
            begin
               Put_Line ("  " & To_Pretty_String (D));
            end Process;
         begin
            Iterate (Parser.Pool, Parser.Last_Diag, Process'Access);
         end;
      end if;

      if PP = null then
         return;
      end if;

      --  Collect all entries from packrat tables into a single ordered map

      % for parser in sorted_fns:
         declare
            procedure Process (E : Memos.Memo_Entry);

            -------------
            -- Process --
            -------------

            procedure Process (E : Memos.Memo_Entry) is
               K : constant Memo_Entry_Key :=
                 (E.Offset, ${parser.gen_fn_name});
               V : constant Memo_Entry :=
                 (E.State, E.Instance, E.Mark, E.Final_Pos);
            begin
               Memo_Entries.Insert (K, V);
            end Process;
         begin
            Memos.Iterate (PP.${parser.gen_fn_name}_Memo, Process'Access);
         end;
      % endfor

      --  Dump all entries in token stream order

      declare
         Last_Token : Token_Index := No_Token_Index;
      begin
         for Cur in Memo_Entries.Iterate loop
            declare
               K : constant Memo_Entry_Key := Memo_Entry_Maps.Key (Cur);
               V : constant Memo_Entry := Memo_Entry_Maps.Element (Cur);
            begin
               if K.Offset > Last_Token then
                  declare
                     T    : constant Stored_Token_Data :=
                       Get_Token (TDH, K.Offset);
                     Text : constant Text_Type :=
                       Langkit_Support.Token_Data_Handlers.Text (TDH, T);
                  begin
                     Put ("[" & K.Offset'Image & "] ");
                     Put (Image (Get_Sloc (TDH, T.Source_First)) & ": ");
                     Put (Image (Text, With_Quotes => True));
                     New_Line;
                     Last_Token := K.Offset;
                  end;
               end if;
               Put ("  [" & V.State'Image & "] ");
               Put (K.Parser'Image);
               if V.State = Success then
                  Put (": " & Trace_Image (V.Instance));
                  Put (" [to" & V.Final_Pos'Image & "]");
               end if;
               New_Line;
               declare
                  procedure Process (D : Diagnostic);

                  -------------
                  -- Process --
                  -------------

                  procedure Process (D : Diagnostic) is
                  begin
                     Put_Line ("    " & To_Pretty_String (D));
                  end Process;
               begin
                  Iterate (Parser.Pool, V.Mark, Process'Access);
               end;
            end;
         end loop;
      end;
   end Dump;

end ${ada_lib_name}.Parsers_Impl;
