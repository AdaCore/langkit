## vim: filetype=makoada

<%namespace name="array_types" file="array_types_ada.mako" />
<%namespace name="entities"    file="entities_ada.mako" />
<%namespace name="exts"        file="extensions.mako" />
<%namespace name="list_types"  file="list_types_ada.mako" />

<% root_node_array = T.root_node.array %>

with Ada.Containers;             use Ada.Containers;
with Ada.Containers.Hashed_Maps;
% if not ctx.separate_properties:
with Ada.Containers.Vectors;
% endif
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;

% if not ctx.separate_properties:
   with Langkit_Support.Array_Utils;
% endif

with Langkit_Support.Images; use Langkit_Support.Images;
with Langkit_Support.Slocs;  use Langkit_Support.Slocs;
with Langkit_Support.Text;   use Langkit_Support.Text;

pragma Warnings (Off, "referenced");
with Langkit_Support.Adalog.Abstract_Relation;
use Langkit_Support.Adalog.Abstract_Relation;
with Langkit_Support.Adalog.Debug;
use Langkit_Support.Adalog.Debug;
with Langkit_Support.Adalog.Operations;
use Langkit_Support.Adalog.Operations;
with Langkit_Support.Adalog.Predicates;
use Langkit_Support.Adalog.Predicates;
with Langkit_Support.Adalog.Pure_Relations;
use Langkit_Support.Adalog.Pure_Relations;
pragma Warnings (On, "referenced");

with ${ada_lib_name}.Analysis.Implementation;
use ${ada_lib_name}.Analysis.Implementation;
with ${ada_lib_name}.Analysis.Parsers; use ${ada_lib_name}.Analysis.Parsers;
with ${ada_lib_name}.Lexer;

% if ctx.separate_properties:
   with ${ada_lib_name}.Analysis.Properties;
   use ${ada_lib_name}.Analysis.Properties;
% endif

${(exts.with_clauses(with_clauses + [
   ((ctx.default_unit_provider.unit_fqn, False)
    if ctx.default_unit_provider else None),
   ((ctx.symbol_canonicalizer.unit_fqn, False)
    if ctx.symbol_canonicalizer else None),
]))}

package body ${ada_lib_name}.Analysis is

   use AST_Envs;

   procedure Destroy (Unit : in out Analysis_Unit);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Context_Type, Analysis_Context);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Type, Analysis_Unit);

   function Create_Unit
     (Context             : Analysis_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Analysis_Unit
      with Pre => not Has_Unit (Context, +Normalized_Filename.Full_Name);
   --  Create a new analysis unit and register it in Context

   function Get_Unit
     (Context           : Analysis_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Init_Parser       :
        access procedure (Unit     : Analysis_Unit;
                          Read_BOM : Boolean;
                          Parser   : in out Parser_Type);
      Rule              : Grammar_Rule) return Analysis_Unit;
   --  Helper for Get_From_File and Get_From_Buffer: do all the common work
   --  using Init_Parser to either parse from a file or from a buffer. Return
   --  the resulting analysis unit.

   function Create_Symbol_Literals
     (Symbols : Symbol_Table) return Symbol_Literal_Array;
   --  Create pre-computed symbol literals in Symbols and return them

   function Wrap
     (Index : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler_Access)
      return Token_Type;

   ------------
   -- Create --
   ------------

   function Create
     (Charset     : String := Default_Charset;
      With_Trivia : Boolean := False
      % if ctx.default_unit_provider:
         ; Unit_Provider : Unit_Provider_Access_Cst := null
      % endif
     ) return Analysis_Context
   is
      % if ctx.default_unit_provider:
         P : constant Unit_Provider_Access_Cst :=
           (if Unit_Provider = null
            then ${ctx.default_unit_provider.fqn}
            else Unit_Provider);
      % endif
      Actual_Charset : constant String :=
        (if Charset = "" then Default_Charset else Charset);
      Symbols        : constant Symbol_Table := Create;
      Context        : Analysis_Context;
   begin
      Context := new Analysis_Context_Type'
        (Ref_Count     => 1,
         Units         => <>,
         Removed_Units => <>,
         Filenames     => <>,
         Symbols       => Symbols,
         Charset       => To_Unbounded_String (Actual_Charset),
         With_Trivia   => With_Trivia,
         Root_Scope    => AST_Envs.Create
                            (Parent => AST_Envs.No_Env_Getter,
                             Node   => null,
                             Owner  => No_Analysis_Unit),

         % if ctx.default_unit_provider:
         Unit_Provider => P,
         % endif

         Symbol_Literals => Create_Symbol_Literals (Symbols),

         Parser => <>,

         Discard_Errors_In_Populate_Lexical_Env => <>,
         Logic_Resolution_Timeout => <>,
         In_Populate_Lexical_Env => False,
         Populate_Lexical_Env_Queue => <>,
         Cache_Version => <>,

         Rewriting_Handle => <>,
         Templates_Unit => <>);

      Initialize (Context.Parser);
      ${exts.include_extension(ctx.ext('analysis', 'context', 'create'))}
      return Context;
   end Create;

   % for sym, name in ctx.sorted_symbol_literals:
      Text_${name} : aliased constant Text_Type := ${string_repr(sym)};
   % endfor

   Symbol_Literals_Text : array (Symbol_Literal_Type) of Text_Cst_Access :=
   (
      % if ctx.symbol_literals:
         ${(', '.join("{name} => Text_{name}'Access".format(name=name)
                      for sym, name in ctx.sorted_symbol_literals))}
      % else:
         1 .. 0 => <>
      % endif
   );

   ----------------------------
   -- Create_Symbol_Literals --
   ----------------------------

   function Create_Symbol_Literals
     (Symbols : Symbol_Table) return Symbol_Literal_Array
   is
      Result : Symbol_Literal_Array;
   begin
      % if ctx.symbol_literals:
         for Literal in Symbol_Literal_Type'Range loop
            declare
               Raw_Text : Text_Type renames
                  Symbol_Literals_Text (Literal).all;
               Symbol   : constant Symbolization_Result :=
                  % if ctx.symbol_canonicalizer:
                     ${ctx.symbol_canonicalizer.fqn} (Raw_Text)
                  % else:
                     Create_Symbol (Raw_Text)
                  % endif
               ;
            begin
               if Symbol.Success then
                  Result (Literal) := Find (Symbols, Symbol.Symbol);
               else
                  raise Program_Error with
                    "Cannot canonicalize symbol literal: " & Image (Raw_Text);
               end if;
            end;
         end loop;
         return Result;
      % else:
         return (1 .. 0 => <>);
      % endif
   end Create_Symbol_Literals;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Context : Analysis_Context) is
   begin
      Context.Ref_Count := Context.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Context : in out Analysis_Context) is
   begin
      Context.Ref_Count := Context.Ref_Count - 1;
      if Context.Ref_Count = 0 then
         Destroy (Context);
      end if;
   end Dec_Ref;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context; Discard : Boolean) is
   begin
      Context.Discard_Errors_In_Populate_Lexical_Env := Discard;
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context; Timeout : Natural) is
   begin
      Context.Logic_Resolution_Timeout := Timeout;
   end Set_Logic_Resolution_Timeout;

   --------------------------
   -- Has_Rewriting_Handle --
   --------------------------

   function Has_Rewriting_Handle (Context : Analysis_Context) return Boolean is
   begin
      return Context.Rewriting_Handle /= No_Rewriting_Handle_Pointer;
   end Has_Rewriting_Handle;

   -----------------
   -- Create_Unit --
   -----------------

   function Create_Unit
     (Context             : Analysis_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Analysis_Unit
   is
      use Units_Maps;

      Cur  : Cursor := Context.Removed_Units.Find
        (Normalized_Filename);
      Unit : Analysis_Unit;
   begin
      if Cur = No_Element then
         Unit := Create_Special_Unit
           (Context, Normalized_Filename, Charset, Rule);
      else
         Unit := Element (Cur);
         Context.Removed_Units.Delete (Cur);
      end if;
      Context.Units.Insert (Normalized_Filename, Unit);
      return Unit;
   end Create_Unit;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Context           : Analysis_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Init_Parser       :
        access procedure (Unit     : Analysis_Unit;
                          Read_BOM : Boolean;
                          Parser   : in out Parser_Type);
      Rule              : Grammar_Rule) return Analysis_Unit
   is
      use Units_Maps;

      Normalized_Filename : constant GNATCOLL.VFS.Virtual_File :=
         Normalized_Unit_Filename (Context, Filename);

      Cur     : constant Cursor :=
         Context.Units.Find (Normalized_Filename);
      Created : constant Boolean := Cur = No_Element;
      Unit    : Analysis_Unit;

      Read_BOM : constant Boolean := Charset'Length = 0;
      --  Unless the caller requested a specific charset for this unit, allow
      --  the lexer to automatically discover the source file encoding before
      --  defaulting to the context-specific one. We do this trying to match a
      --  byte order mark.

      Actual_Charset : Unbounded_String;

   begin
      --  Determine which encoding to use.  The parameter comes first, then the
      --  unit-specific default, then the context-specific one.

      if Charset'Length /= 0 then
         Actual_Charset := To_Unbounded_String (Charset);
      elsif not Created then
         Actual_Charset := Element (Cur).Charset;
      else
         Actual_Charset := Context.Charset;
      end if;

      --  Create the Analysis_Unit if needed

      if Created then
         Unit := Create_Unit
           (Context, Normalized_Filename, To_String (Actual_Charset), Rule);
      else
         Unit := Element (Cur);
      end if;
      Unit.Charset := Actual_Charset;

      --  (Re)parse it if needed

      if Created or else Reparse then
         declare
            Reparsed : Reparsed_Unit;
         begin
            Do_Parsing (Unit, Read_BOM, Init_Parser, Reparsed);
            Update_After_Reparse (Unit, Reparsed);
         end;
      end if;

      return Unit;
   end Get_Unit;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context       : Analysis_Context;
      Unit_Filename : String) return Boolean is
   begin
      return Context.Units.Contains
        (Normalized_Unit_Filename (Context, Unit_Filename));
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Analysis_Context;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule :=
         ${Name.from_lower(ctx.main_rule_name)}_Rule) return Analysis_Unit
   is
      procedure Init_Parser
        (Unit     : Analysis_Unit;
         Read_BOM : Boolean;
         Parser   : in out Parser_Type) is
      begin
         Init_Parser_From_File
           (Filename, To_String (Unit.Charset), Read_BOM, Unit,
            Token_Data (Unit),
            Parsers.Symbol_Literal_Array_Access
              (Symbol_Literals (Unit.Context)),
            Context.With_Trivia, Parser);
      end Init_Parser;
   begin
      return Get_Unit
        (Context, Filename, Charset, Reparse, Init_Parser'Access, Rule);
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context     : Analysis_Context;
      Filename    : String;
      Charset     : String := "";
      Buffer      : String;
      Rule        : Grammar_Rule :=
         ${Name.from_lower(ctx.main_rule_name)}_Rule) return Analysis_Unit
   is
      procedure Init_Parser
        (Unit     : Analysis_Unit;
         Read_BOM : Boolean;
         Parser   : in out Parser_Type) is
      begin
         Init_Parser_From_Buffer
           (Buffer, To_String (Unit.Charset), Read_BOM, Unit,
            Token_Data (Unit),
            Parsers.Symbol_Literal_Array_Access
              (Symbol_Literals (Unit.Context)),
            Context.With_Trivia, Parser);
      end Init_Parser;
   begin
      return Get_Unit (Context, Filename, Charset, True, Init_Parser'Access,
                       Rule);
   end Get_From_Buffer;

   % if ctx.default_unit_provider:

   -----------------------
   -- Get_From_Provider --
   -----------------------

   function Get_From_Provider
     (Context     : Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit is
   begin
      return Context.Unit_Provider.Get_Unit
        (Context, Name, Kind, Charset, Reparse);

   exception
      when Property_Error =>
         raise Invalid_Unit_Name_Error with
            "Invalid unit name: " & Image (Name, With_Quotes => True)
            & " (" & Unit_Kind'Image (Kind) & ")";
   end Get_From_Provider;

   % endif

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context     : Analysis_Context;
      Filename    : String;
      Error       : String;
      Charset     : String := "";
      Rule        : Grammar_Rule :=
         ${Name.from_lower(ctx.main_rule_name)}_Rule)
      return Analysis_Unit
   is
      use Units_Maps;

      Normalized_Filename : constant GNATCOLL.VFS.Virtual_File :=
         Normalized_Unit_Filename (Context, Filename);
      Cur                 : constant Cursor :=
         Context.Units.Find (Normalized_Filename);
   begin
      if Cur = No_Element then
         declare
            Unit : constant Analysis_Unit := Create_Unit
              (Context, Normalized_Filename, Charset, Rule);
            Msg  : constant Text_Type := To_Text (Error);
         begin
            Append (Unit.Diagnostics, No_Source_Location_Range, Msg);
            return Unit;
         end;
      else
         return Element (Cur);
      end if;
   end Get_With_Error;

   ------------
   -- Remove --
   ------------

   procedure Remove (Context : Analysis_Context; File_Name : String) is
      use Units_Maps;

      Cur      : Cursor := Context.Units.Find
        (Normalized_Unit_Filename (Context, File_Name));
      Unit     : Analysis_Unit;
      Reparsed : Reparsed_Unit;
   begin
      if Cur = No_Element then
         raise Constraint_Error with "No such analysis unit";
      end if;

      Unit := Element (Cur);
      Traces.Trace (Main_Trace, "Removing unit: " & Basename (Unit));

      --  Do as if we just reparsed Unit with minimal data, to get rid of all
      --  its parsing data. This will schedule a lexical enviroment cleanup.
      Initialize (Reparsed.TDH, Context.Symbols);
      Update_After_Reparse (Unit, Reparsed);

      --  Move the unit to the set of removed units so the unit handle still
      --  points to valid memory. We will re-use it if we reparse this unit.
      Context.Units.Delete (Cur);
      Context.Removed_Units.Insert (Unit.File_Name, Unit);
   end Remove;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Analysis_Context) is
   begin
      for Unit of Context.Units loop
         Unit.Context := null;
         Dec_Ref (Unit);
      end loop;
      for Unit of Context.Removed_Units loop
         Unit.Context := null;
         Dec_Ref (Unit);
      end loop;
      Dec_Ref (Context.Templates_Unit);
      AST_Envs.Destroy (Context.Root_Scope);
      Destroy (Context.Symbols);
      Destroy (Context.Parser);
      Free (Context);
   end Destroy;

   -------------
   -- Context --
   -------------

   function Context (Unit : Analysis_Unit) return Analysis_Context is
   begin
      return Unit.Context;
   end Context;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Unit : Analysis_Unit) is
   begin
      Unit.Ref_Count := Unit.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Unit : in out Analysis_Unit) is
   begin
      if Unit = No_Analysis_Unit then
         return;
      end if;
      Unit.Ref_Count := Unit.Ref_Count - 1;
      if Unit.Ref_Count = 0 then
         Destroy (Unit);
      end if;
   end Dec_Ref;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit    : Analysis_Unit;
      Charset : String := "")
   is
      Dummy : constant Analysis_Unit := Get_From_File
        (Unit.Context, +Unit.File_Name.Full_Name, Charset, Reparse => True);
   begin
      null;
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit    : Analysis_Unit;
      Charset : String := "";
      Buffer  : String)
   is
      Dummy : constant Analysis_Unit := Get_From_Buffer
        (Unit.Context, +Unit.File_Name.Full_Name, Charset, Buffer);
   begin
      null;
   end Reparse;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Unit : in out Analysis_Unit) is
   begin
      if Unit = No_Analysis_Unit then
         return;
      end if;

      Unit.Exiled_Entries.Destroy;
      Unit.Foreign_Nodes.Destroy;
      Analysis_Unit_Sets.Destroy (Unit.Referenced_Units);

      % if ctx.has_memoization:
         Destroy (Unit.Memoization_Map);
      % endif

      Destroy_Rebindings (Unit.Rebindings'Access);
      Unit.Rebindings.Destroy;

      if Unit.AST_Root /= null then
         Destroy (Unit.AST_Root);
      end if;

      Free (Unit.TDH);
      Free (Unit.AST_Mem_Pool);
      Destroy_Unit_Destroyables (Unit);
      Destroyable_Vectors.Destroy (Unit.Destroyables);
      Free (Unit);
   end Destroy;

   -----------
   -- Print --
   -----------

   procedure Print
     (Unit       : Analysis_Unit;
      Show_Slocs : Boolean := True) is
   begin
      if Unit.AST_Root = null then
         Put_Line ("<empty analysis unit>");
      else
         Unit.AST_Root.Print (Show_Slocs);
      end if;
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Analysis_Unit) is

      procedure Process (Trivia : Token_Index) is
         Data : constant Lexer.Token_Data_Type :=
            Unit.TDH.Trivias.Get (Natural (Trivia)).T;
      begin
         Put_Line (Image (Text (Unit.TDH, Data)));
      end Process;

      Last_Token : constant Token_Index :=
         Token_Index (Token_Vectors.Last_Index (Unit.TDH.Tokens) - 1);
      --  Index for the last token in Unit excluding the Termination token
      --  (hence the -1).
   begin
      for Tok of Get_Leading_Trivias (Unit.TDH) loop
         Process (Tok);
      end loop;

      PP_Trivia (Unit.AST_Root);

      for Tok of Get_Trivias (Unit.TDH, Last_Token) loop
         Process (Tok);
      end loop;
   end PP_Trivia;

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Kind : ${root_node_kind_name}) return Boolean is
   begin
      return Is_Token_Node_Kind (Kind);
   end Is_Token_Node;

   ------------------
   -- Is_List_Node --
   ------------------

   function Is_List_Node (Kind : ${root_node_kind_name}) return Boolean is
   begin
      return ${('Kind in {}'.format(ctx.generic_list_type.ada_kind_range_name)
                if ctx.generic_list_type.concrete_subclasses else
                'False')};
   end Is_List_Node;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Unit : Analysis_Unit) is
      Context : constant Analysis_Context := Unit.Context;

      Has_Errors : Boolean := False;
      --  Whether at least one Property_Error occurred during this PLE pass

      Saved_In_Populate_Lexical_Env : constant Boolean :=
         Unit.Context.In_Populate_Lexical_Env;
   begin
      --  If there are several analysis units for which lexical envs must be
      --  re-created, go through them now. Check the In_Populate_Lexical_Env
      --  flag to avoid infinite recursion.
      if not Context.Populate_Lexical_Env_Queue.Is_Empty
         and then not Context.In_Populate_Lexical_Env
      then
         Flush_Populate_Lexical_Env_Queue (Context);
      end if;

      --  TODO??? Handle env invalidation when reparsing a unit and when a
      --  previous call raised a Property_Error.
      if Unit.Is_Env_Populated then
         return;
      end if;
      Unit.Is_Env_Populated := True;

      if Unit.AST_Root = null then
         return;
      end if;

      Traces.Trace (Main_Trace, "Populating lexical envs for unit: "
                                & Basename (Unit));

      Context.In_Populate_Lexical_Env := True;

      % if ctx.subunit_root:
         if Unit.AST_Root /= null then
            --  If the tree root is a list of sub-units, populate envs for each
            --  one of them.
            if Unit.AST_Root.Kind = ${ctx.subunit_root.list.ada_kind_name} then
               for I in 1 .. Unit.AST_Root.Abstract_Children_Count loop
                  Has_Errors := Populate_Lexical_Env (Unit.AST_Root.Child (I))
                                or else Has_Errors;
               end loop;

            --  Otherwise, populate envs only if the root is a sub-unit itself
            elsif Unit.AST_Root.Kind = ${ctx.subunit_root.ada_kind_name} then
               Has_Errors := Populate_Lexical_Env (Unit.AST_Root);
            end if;
         end if;
      % else:
         Has_Errors := Populate_Lexical_Env (Unit.AST_Root);
      % endif

      Context.In_Populate_Lexical_Env :=
         Saved_In_Populate_Lexical_Env;

      if Has_Errors and then not Context.Discard_Errors_In_Populate_Lexical_Env
      then
         raise Property_Error with
            "errors occurred in Populate_Lexical_Env";
      end if;
   end Populate_Lexical_Env;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Analysis_Unit) return Boolean is
   begin
      return not Unit.Diagnostics.Is_Empty;
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Analysis_Unit) return Diagnostics_Array is
      Result : Diagnostics_Array (1 .. Natural (Unit.Diagnostics.Length));
      I      : Natural := 1;
   begin
      for D of Unit.Diagnostics loop
         Result (I) := D;
         I := I + 1;
      end loop;
      return Result;
   end Diagnostics;

   ---------------------------
   -- Format_GNU_Diagnostic --
   ---------------------------

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit; D : Diagnostic) return String
   is
      Filename : constant String := Basename (Unit);
      Sloc     : constant Source_Location := Start_Sloc (D.Sloc_Range);
      Msg      : constant String :=
         Image
           (Ada.Strings.Wide_Wide_Unbounded.To_Wide_Wide_String (D.Message));
   begin
      return (Filename
              & (if Sloc = No_Source_Location then "" else ":" & Image (Sloc))
              & ": " & Msg);
   end Format_GNU_Diagnostic;

   % if ctx.default_unit_provider:
   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Analysis_Context)
      return Unit_Provider_Access_Cst is (Context.Unit_Provider);
   % endif

   ----------
   -- Root --
   ----------

   function Root (Unit : Analysis_Unit) return ${root_entity.api_name} is
     ((Unit.AST_Root, No_Public_Entity_Info));

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Analysis_Unit) return Token_Type is
     (Wrap (First_Token_Or_Trivia (Unit.TDH), Unit.TDH'Access));

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Analysis_Unit) return Token_Type is
     (Wrap (Last_Token_Or_Trivia (Unit.TDH), Unit.TDH'Access));

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Analysis_Unit) return Natural is
     (Unit.TDH.Tokens.Length);

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Analysis_Unit) return Natural is
     (Unit.TDH.Trivias.Length);

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Analysis_Unit; Sloc : Source_Location) return Token_Type
   is
      use Token_Data_Handlers;
      Result : constant Token_Or_Trivia_Index := Lookup_Token (Unit.TDH, Sloc);
   begin
      return Wrap (Result, Unit.TDH'Access);
   end Lookup_Token;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context (Unit : Analysis_Unit) return Analysis_Context is
     (Unit.Context);

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Analysis_Unit) return String is
     (+Unit.File_Name.Full_Name);

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Analysis_Unit) return String is
   begin
      return To_String (Unit.Charset);
   end Get_Charset;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Token_Type) return Boolean is
      pragma Assert (Left.TDH = Right.TDH);
   begin
      if Left.Index.Token < Right.Index.Token then
         return True;

      elsif Left.Index.Token = Right.Index.Token then
         return Left.Index.Trivia < Right.Index.Trivia;

      else
         return False;
      end if;
   end "<";

   ----------
   -- Wrap --
   ----------

   function Wrap
     (Index : Token_Or_Trivia_Index;
      TDH   : Token_Data_Handler_Access)
      return Token_Type is
   begin
      return (if Index = No_Token_Or_Trivia_Index
              then No_Token
              else (TDH, Index));
   end;

   ----------
   -- Next --
   ----------

   function Next (Token : Token_Type) return Token_Type is
   begin
      return (if Token.TDH = null
              then No_Token
              else Wrap (Next (Token.Index, Token.TDH.all), Token.TDH));
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Token : Token_Type) return Token_Type is
   begin
      return (if Token.TDH = null
              then No_Token
              else Wrap (Previous (Token.Index, Token.TDH.all), Token.TDH));
   end Previous;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Token : Token_Type) return Symbol_Type is
      subtype Token_Data_Reference is
         Token_Data_Handlers.Token_Vectors.Element_Access;

      Token_Data : constant Token_Data_Reference :=
        (if Token.Index.Trivia = No_Token_Index
         then Token_Data_Reference
           (Token.TDH.Tokens.Get_Access (Natural (Token.Index.Token)))
         else Token_Data_Reference'
           (Token.TDH.Trivias.Get_Access
              (Natural (Token.Index.Trivia) - 1).T'Access));
   begin
      return Force_Symbol (Token.TDH.all, Token_Data.all);
   end Get_Symbol;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Self : Token_Iterator) return Token_Type
   is (Token_Start (Self.Node));

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Type) return Token_Type
   is (Next (Tok));

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Type) return Boolean
   is (Tok.Index.Token <= Self.Last);

   -------------
   -- Element --
   -------------

   function Element (Self : Token_Iterator; Tok : Token_Type) return Token_Type
   is (Tok);

   --------------
   -- Raw_Data --
   --------------

   function Raw_Data (T : Token_Type) return Lexer.Token_Data_Type is
     (if T.Index.Trivia = No_Token_Index
      then Token_Vectors.Get (T.TDH.Tokens, Natural (T.Index.Token))
      else Trivia_Vectors.Get (T.TDH.Trivias, Natural (T.Index.Trivia)).T);

   ----------
   -- Data --
   ----------

   function Data (Token : Token_Type) return Token_Data_Type is
   begin
      return Convert (Token.TDH.all, Token, Raw_Data (Token));
   end Data;

   ----------
   -- Text --
   ----------

   function Text (Token : Token_Type) return Text_Type is
      RD : constant Lexer.Token_Data_Type := Raw_Data (Token);
   begin
      return Token.TDH.Source_Buffer (RD.Source_First .. RD.Source_Last);
   end Text;

   ----------
   -- Text --
   ----------

   function Text (First, Last : Token_Type) return Text_Type is
      FD : constant Token_Data_Type := Data (First);
      LD : constant Token_Data_Type := Data (Last);
   begin
      if First.TDH /= Last.TDH then
         raise Constraint_Error;
      end if;
      return FD.Source_Buffer.all (FD.Source_First .. LD.Source_Last);
   end Text;

   ----------
   -- Text --
   ----------

   function Text (Token : Token_Type) return String
   is (Image (Text (Token)));

   ----------
   -- Kind --
   ----------

   function Kind (Token_Data : Token_Data_Type) return Token_Kind is
   begin
      return Token_Data.Kind;
   end Kind;

   ---------------
   -- Is_Trivia --
   ---------------

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean is
   begin
      return Token_Data.Is_Trivia;
   end Is_Trivia;

   -----------
   -- Index --
   -----------

   function Index (Token : Token_Type) return Token_Index is
   begin
      return (if Token.Index.Trivia = No_Token_Index
              then Token.Index.Token
              else Token.Index.Trivia);
   end Index;

   -----------
   -- Index --
   -----------

   function Index (Token_Data : Token_Data_Type) return Token_Index is
   begin
      return Token_Data.Index;
   end Index;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range
   is
   begin
      return Token_Data.Sloc_Range;
   end Sloc_Range;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (L, R : Token_Type) return Boolean is
      DL : constant Token_Data_Type := Data (L);
      DR : constant Token_Data_Type := Data (R);
      TL : constant Text_Type := Text (L);
      TR : constant Text_Type := Text (R);
   begin
      return DL.Kind = DR.Kind and then TL = TR;
   end Is_Equivalent;

   -----------
   -- Image --
   -----------

   function Image (Token : Token_Type) return String is
      D : constant Token_Data_Type := Data (Token);
   begin
      return ("<Token Kind=" & Token_Kind_Name (D.Kind) &
              " Text=" & Image (Text (Token), With_Quotes => True) & ">");
   end Image;

   --  Those maps are used to give unique ids to lexical envs while pretty
   --  printing them.

   package Address_To_Id_Maps is new Ada.Containers.Hashed_Maps
     (Lexical_Env, Integer, Hash, "=");

   type Dump_Lexical_Env_State is record
      Env_Ids : Address_To_Id_Maps.Map;
      --  Mapping: Lexical_Env -> Integer, used to remember which unique Ids we
      --  assigned to the lexical environments we found.

      Next_Id : Positive := 1;
      --  Id to assign to the next unknown lexical environment

      Root_Env : Lexical_Env;
      --  Lexical environment we consider a root (this is the Root_Scope from
      --  the current analysis context), or null if unknown.
   end record;
   --  Holder for the state of lexical environment dumpers

   function Get_Env_Id
     (E : Lexical_Env; State : in out Dump_Lexical_Env_State) return String;
   --  If E is known, return its unique Id from State. Otherwise, assign it a
   --  new unique Id and return it.

   ----------------
   -- Get_Env_Id --
   ----------------

   function Get_Env_Id
     (E : Lexical_Env; State : in out Dump_Lexical_Env_State) return String
   is
      C        : Address_To_Id_Maps.Cursor;
      Inserted : Boolean;
   begin
      if E = Null_Lexical_Env then
         return "$null";

      elsif E = State.Root_Env then
         --  Insert root env with a special Id so that we only print it once
         State.Env_Ids.Insert (E, -1, C, Inserted);
         return "$root";
      end if;

      State.Env_Ids.Insert (E, State.Next_Id, C, Inserted);
      if Inserted then
         State.Next_Id := State.Next_Id + 1;
      end if;

      return '@' & Stripped_Image (Address_To_Id_Maps.Element (C));
   end Get_Env_Id;

   ------------------------
   -- Trigger_Envs_Debug --
   ------------------------

   procedure Trigger_Envs_Debug (Is_Active : Boolean) is
   begin
      GNATCOLL.Traces.Set_Active (AST_Envs.Me, Is_Active);
   end Trigger_Envs_Debug;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Analysis_Unit) is
      Node     : constant ${root_node_type_name} := Unit.AST_Root;
      Root_Env : constant Lexical_Env := Unit.Context.Root_Scope;
      State    : Dump_Lexical_Env_State := (Root_Env => Root_Env, others => <>);

      --------------------------
      -- Explore_Parent_Chain --
      --------------------------

      procedure Explore_Parent_Chain (Env : Lexical_Env) is
      begin
         if Env /= Null_Lexical_Env then
            Dump_One_Lexical_Env
              (Env, Get_Env_Id (Env, State),
               Get_Env_Id (AST_Envs.Get_Env (Env.Env.Parent), State));
            Explore_Parent_Chain (AST_Envs.Get_Env (Env.Env.Parent));
         end if;
      end Explore_Parent_Chain;

      --------------
      -- Internal --
      --------------

      procedure Internal (Current : ${root_node_type_name}) is
         Explore_Parent : Boolean := False;
         Env, Parent    : Lexical_Env;
      begin
         if Current = null then
            return;
         end if;

         --  We only dump environments that we haven't dumped before. This way
         --  we'll only dump environments at the site of their creation, and
         --  not in any subsequent link. We use the Env_Ids map to check which
         --  envs we have already seen or not.
         if not State.Env_Ids.Contains (Current.Self_Env) then
            Env := Current.Self_Env;
            Parent := AST_Envs.Get_Env (Env.Env.Parent);
            Explore_Parent := not State.Env_Ids.Contains (Parent);

            Dump_One_Lexical_Env
              (Env, Get_Env_Id (Env, State), Get_Env_Id (Parent, State));

            if Explore_Parent then
               Explore_Parent_Chain (Parent);
            end if;
         end if;

         for Child of ${root_node_array.api_name}'(Children (Current)) loop
            Internal (Child);
         end loop;
      end Internal;
      --  This procedure implements the main recursive logic of dumping the
      --  environments.
   begin
      Internal (${root_node_type_name} (Node));
   end Dump_Lexical_Env;

   -----------
   -- Image --
   -----------

   function Image (Value : Boolean) return String
   is (if Value then "True" else "False");

   % if not ctx.separate_properties:
      ${entities.bodies()}
   % endif

end ${ada_lib_name}.Analysis;
