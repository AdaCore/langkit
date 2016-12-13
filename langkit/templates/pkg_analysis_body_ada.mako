## vim: filetype=makoada

with Ada.Containers;                  use Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Text;    use Langkit_Support.Text;

with ${get_context().ada_api_settings.lib_name}.Analysis.Internal;
use ${get_context().ada_api_settings.lib_name}.Analysis.Internal;
with ${get_context().ada_api_settings.lib_name}.Lexer;
with ${get_context().ada_api_settings.lib_name}.AST.Types.Parsers;
use ${get_context().ada_api_settings.lib_name}.AST.Types.Parsers;

package body ${_self.ada_api_settings.lib_name}.Analysis is

   procedure Destroy (Unit : Analysis_Unit);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Context_Type, Analysis_Context);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Type, Analysis_Unit);

   procedure Update_Charset (Unit : Analysis_Unit; Charset : String);
   --  If Charset is an empty string, do nothing. Otherwise, update
   --  Unit.Charset field to Charset.

   procedure Do_Parsing
     (Unit       : Analysis_Unit;
      Read_BOM   : Boolean;
      Get_Parser : access function (Unit     : Analysis_Unit;
                                    Read_BOM : Boolean)
                                    return Parser_Type);
   --  Helper for Get_Unit and the public Reparse procedures: parse an analysis
   --  unit using Get_Parser and replace Unit's AST_Root and the diagnostics
   --  with the parsers's output.

   function Get_Unit
     (Context           : Analysis_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Get_Parser        : access function (Unit     : Analysis_Unit;
                                           Read_BOM : Boolean)
                                           return Parser_Type;
      With_Trivia       : Boolean;
      Rule              : Grammar_Rule)
      return Analysis_Unit;
   --  Helper for Get_From_File and Get_From_Buffer: do all the common work
   --  using Get_Parser to either parse from a file or from a buffer. Return
   --  the resulting analysis unit.

   --------------------
   -- Update_Charset --
   --------------------

   procedure Update_Charset (Unit : Analysis_Unit; Charset : String)
   is
   begin
      if Charset'Length /= 0 then
         Unit.Charset := To_Unbounded_String (Charset);
      end if;
   end Update_Charset;

   ------------
   -- Create --
   ------------

   function Create
     (Charset : String := ${string_repr(_self.default_charset)}
      % if _self.default_unit_file_provider:
         ; Unit_File_Provider : Unit_File_Provider_Access_Cst :=
             ${_self.default_unit_file_provider.fqn}
      % endif
     ) return Analysis_Context
   is
   begin
      return new Analysis_Context_Type'
        (Ref_Count  => 1,
         Units_Map  => <>,
         Symbols    => Create,
         Charset    => To_Unbounded_String (Charset),
         Root_Scope => AST_Envs.Create
                         (Parent        => AST_Envs.No_Env_Getter,
                          Node          => null,
                          Is_Refcounted => False)

         % if _self.default_unit_file_provider:
         , Unit_File_Provider => Unit_File_Provider
         % endif
        );
   end Create;

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

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Context           : Analysis_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Get_Parser        : access function (Unit     : Analysis_Unit;
                                           Read_BOM : Boolean)
                                           return Parser_Type;
      With_Trivia       : Boolean;
      Rule              : Grammar_Rule)
      return Analysis_Unit
   is
      use Units_Maps;

      Fname   : constant Unbounded_String := To_Unbounded_String (Filename);
      Cur     : constant Cursor := Context.Units_Map.Find (Fname);
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
         Unit := new Analysis_Unit_Type'
           (Context           => Context,
            Ref_Count         => 1,
            AST_Root          => null,
            File_Name         => Fname,
            Charset           => <>,
            TDH               => <>,
            Diagnostics       => <>,
            With_Trivia       => With_Trivia,
            Is_Env_Populated  => False,
            Has_Filled_Caches => False,
            Rule              => Rule,
            AST_Mem_Pool      => No_Pool,
            Destroyables      => Destroyable_Vectors.Empty_Vector,
            Referenced_Units  => <>,
            Lex_Env_Data      => <>);
         Initialize (Unit.TDH, Context.Symbols);
         Context.Units_Map.Insert (Fname, Unit);
      else
         Unit := Element (Cur);
      end if;
      Unit.Charset := Actual_Charset;

      --  (Re)parse it if needed

      if Created
         or else Reparse
         or else (With_Trivia and then not Unit.With_Trivia)
      then
         Do_Parsing (Unit, Read_BOM, Get_Parser);
      end if;

      return Unit;
   end Get_Unit;

   ----------------
   -- Do_Parsing --
   ----------------

   procedure Do_Parsing
     (Unit       : Analysis_Unit;
      Read_BOM   : Boolean;
      Get_Parser : access function (Unit     : Analysis_Unit;
                                    Read_BOM : Boolean)
                                    return Parser_Type)
   is

      procedure Add_Diagnostic (Message : String);
      --  Helper to add a sloc-less diagnostic to Unit

      --------------------
      -- Add_Diagnostic --
      --------------------

      procedure Add_Diagnostic (Message : String) is
      begin
         Unit.Diagnostics.Append
           ((Sloc_Range => No_Source_Location_Range,
             Message    => To_Unbounded_Wide_Wide_String (To_Text (Message))));
      end Add_Diagnostic;

      Parser : Parser_Type;

   begin
      --  If we have an AST_Mem_Pool already, we are reparsing. We want to
      --  destroy it to free all the allocated memory.
      if Unit.AST_Root /= null then
         Unit.AST_Root.Destroy;
      end if;
      if Unit.AST_Mem_Pool /= No_Pool then
         Free (Unit.AST_Mem_Pool);
      end if;
      Unit.AST_Root := null;
      Unit.Has_Filled_Caches := False;
      Unit.Diagnostics.Clear;

      --  As (re-)loading an unit can change how any AST node property in the
      --  whole analysis context behaves, we have to invalidate caches. This
      --  is likely overkill, but kill all caches here as it's easy to do.
      Reset_Property_Caches (Unit.Context);

      --  Now create the parser. This is where lexing occurs, so this is where
      --  we get most "setup" issues: missing input file, bad charset, etc.
      --  If we have such an error, catch it, turn it into diagnostics and
      --  abort parsing.

      declare
         use Ada.Exceptions;
      begin
         Parser := Get_Parser (Unit, Read_BOM);
      exception
         when Exc : Name_Error =>
            --  This happens when we cannot open the source file for lexing:
            --  return an unit anyway with diagnostics indicating what happens.

            Add_Diagnostic
              (Exception_Message (Exc));
            return;

         when Lexer.Unknown_Charset =>
            Add_Diagnostic
              ("Unknown charset """ & To_String (Unit.Charset) & """");
            return;

         when Lexer.Invalid_Input =>
            --  TODO??? Tell where (as a source location) we failed to decode
            --  the input.
            Add_Diagnostic
              ("Could not decode source as """ & To_String (Unit.Charset)
               & """");
            return;
      end;

      --  We have correctly setup a parser! Now let's parse and return what we
      --  get.

      Unit.AST_Mem_Pool := Create;
      Parser.Mem_Pool := Unit.AST_Mem_Pool;

      Unit.AST_Root := Parse (Parser, Rule => Unit.Rule);
      Unit.Diagnostics := Parser.Diagnostics;
      Clean_All_Memos;
   end Do_Parsing;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context     : Analysis_Context;
      Filename    : String;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False;
      Rule        : Grammar_Rule :=
         ${Name.from_lower(_self.main_rule_name)}_Rule)
      return Analysis_Unit
   is
      function Get_Parser
        (Unit     : Analysis_Unit;
         Read_BOM : Boolean)
         return Parser_Type
      is (Create_From_File (Filename, To_String (Unit.Charset), Read_BOM,
                            Analysis_Unit_Interface (Unit), With_Trivia));
   begin
      return Get_Unit
        (Context, Filename, Charset, Reparse, Get_Parser'Access, With_Trivia,
         Rule);
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context     : Analysis_Context;
      Filename    : String;
      Charset     : String := "";
      Buffer      : String;
      With_Trivia : Boolean := False;
      Rule        : Grammar_Rule :=
         ${Name.from_lower(_self.main_rule_name)}_Rule)
      return Analysis_Unit
   is
      function Get_Parser
        (Unit     : Analysis_Unit;
         Read_BOM : Boolean)
         return Parser_Type
      is (Create_From_Buffer (Buffer, To_String (Unit.Charset), Read_BOM,
                              Analysis_Unit_Interface (Unit), With_Trivia));
   begin
      return Get_Unit (Context, Filename, Charset, True, Get_Parser'Access,
                       With_Trivia, Rule);
   end Get_From_Buffer;

   % if _self.default_unit_file_provider:

   -----------------------
   -- Get_From_Provider --
   -----------------------

   function Get_From_Provider
     (Context     : Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False;
      With_Trivia : Boolean := False;
      Rule        : Grammar_Rule :=
         ${Name.from_lower(_self.main_rule_name)}_Rule)
      return Analysis_Unit
   is
   begin
      declare
         Filename : constant String :=
            Context.Unit_File_Provider.Get_File (Name, Kind);
      begin
         return Get_From_File
           (Context, Filename, Charset, Reparse, With_Trivia, Rule);
      end;

   exception
      when Property_Error =>
         raise Invalid_Unit_Name_Error with
            "Invalid unit name: " & Image (Name, With_Quotes => True)
            & " (" & Unit_Kind'Image (Kind) & ")";
   end Get_From_Provider;

   % endif

   ------------
   -- Remove --
   ------------

   procedure Remove (Context   : Analysis_Context;
                     File_Name : String)
   is
      use Units_Maps;

      Cur : Cursor := Context.Units_Map.Find (To_Unbounded_String (File_Name));
   begin
      if Cur = No_Element then
         raise Constraint_Error with "No such analysis unit";
      end if;

      --  We remove the corresponding analysis unit from this context but
      --  users could keep references on it, so make sure it can live
      --  independently.

      declare
         Unit : constant Analysis_Unit := Element (Cur);
      begin
         Unit.Context := null;
         Dec_Ref (Unit);
      end;

      Context.Units_Map.Delete (Cur);
   end Remove;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Analysis_Context) is
   begin
      for Unit of Context.Units_Map loop
         Unit.Context := null;
         Dec_Ref (Unit);
      end loop;
      AST_Envs.Destroy (Context.Root_Scope);
      Destroy (Context.Symbols);
      Free (Context);
   end Destroy;

   ---------------------------
   -- Reset_Property_Caches --
   ---------------------------

   procedure Reset_Property_Caches (Context : Analysis_Context) is
   begin
      for Unit of Context.Units_Map loop
         Unit.Reset_Property_Caches;
      end loop;
   end Reset_Property_Caches;

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

   procedure Dec_Ref (Unit : Analysis_Unit) is
   begin
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
      function Get_Parser
        (Unit     : Analysis_Unit;
         Read_BOM : Boolean)
         return Parser_Type
      is (Create_From_File (To_String (Unit.File_Name),
                            To_String (Unit.Charset),
                            Read_BOM,
                            Analysis_Unit_Interface (Unit)));
   begin
      Update_Charset (Unit, Charset);
      Do_parsing (Unit, Charset'Length = 0, Get_Parser'Access);
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit    : Analysis_Unit;
      Charset : String := "";
      Buffer  : String)
   is
      function Get_Parser
        (Unit     : Analysis_Unit;
         Read_BOM : Boolean)
         return Parser_Type
      is (Create_From_Buffer (Buffer, To_String (Unit.Charset), Read_BOM,
                              Analysis_Unit_Interface (Unit)));
   begin
      Update_Charset (Unit, Charset);
      Do_parsing (Unit, Charset'Length = 0, Get_Parser'Access);
      Unit.Charset := To_Unbounded_String (Charset);
   end Reparse;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Unit : Analysis_Unit) is
      Unit_Var : Analysis_Unit := Unit;
   begin
      Destroy (Unit.Lex_Env_Data);
      Analysis_Unit_Sets.Destroy (Unit.Referenced_Units);

      if Unit.AST_Root /= null then
         Destroy (Unit.AST_Root);
      end if;

      Free (Unit.TDH);
      Free (Unit.AST_Mem_Pool);
      for D of Unit.Destroyables loop
         D.Destroy (D.Object);
      end loop;
      Destroyable_Vectors.Destroy (Unit.Destroyables);
      Free (Unit_Var);

   end Destroy;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Analysis_Unit) is
   begin
      if Unit.AST_Root = null then
         Put_Line ("<empty analysis unit>");
      else
         Unit.AST_Root.Print;
      end if;
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Analysis_Unit) is
      Last_Token : constant Token_Index :=
         Token_Index (Token_Vectors.Last_Index (Unit.TDH.Tokens) - 1);
      --  Index for the last token in Unit excluding the Termination token
      --  (hence the -1).
   begin
      for Tok of Get_Leading_Trivias (Unit.TDH) loop
         Put_Line (Image (Tok.Text.all));
      end loop;

      PP_Trivia (Unit.AST_Root);

      for Tok of Get_Trivias (Unit.TDH, Last_Token) loop
         Put_Line (Image (Tok.Text.all));
      end loop;
   end PP_Trivia;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Unit : Analysis_Unit) is
   begin
      --  TODO??? Handle env invalidation when reparsing an unit and when a
      --  previous call raised a Property_Error.
      if Unit.Is_Env_Populated then
         return;
      end if;
      Unit.Is_Env_Populated := True;

      Populate_Lexical_Env (Unit.AST_Root, Unit.Context.Root_Scope);
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

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Analysis_Unit) is
   begin
      Dump_Lexical_Env (Unit.AST_Root, Unit.Context.Root_Scope);
   end Dump_Lexical_Env;

   --------------------------
   -- Register_Destroyable --
   --------------------------

   overriding
   procedure Register_Destroyable_Helper
     (Unit    : access Analysis_Unit_Type;
      Object  : System.Address;
      Destroy : Destroy_Procedure)
   is
   begin
      Destroyable_Vectors.Append (Unit.Destroyables, (Object, Destroy));
   end Register_Destroyable_Helper;

   -----------------------
   -- Set_Filled_Caches --
   -----------------------

   overriding procedure Set_Filled_Caches (Unit : access Analysis_Unit_Type)
   is
   begin
      Unit.Has_Filled_Caches := True;
   end Set_Filled_Caches;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Node : access ${root_node_value_type}'Class)
      return Analysis_Unit
   is
   begin
      return Convert (Get_Unit (Node));
   end Get_Unit;

   --------------------
   -- Reference_Unit --
   --------------------

   procedure Reference_Unit (From, Referenced : Analysis_Unit) is
      Dummy : Boolean;
   begin
      Dummy := Analysis_Unit_Sets.Add (From.Referenced_Units, Referenced);
   end Reference_Unit;

   -------------------
   -- Is_Referenced --
   -------------------

   overriding function Is_Referenced
     (Unit, Referenced : access Analysis_Unit_Type) return Boolean
   is
   begin
      if Unit = null or else Referenced = null then
         return False;
      elsif Unit = Referenced then
         return True;
      else
         return Analysis_Unit_Sets.Has (Unit.Referenced_Units, Referenced);
      end if;
   end;

   ----------------------
   -- Get_Lex_Env_Data --
   ----------------------

   function Get_Lex_Env_Data
     (Unit : Analysis_Unit) return Lex_Env_Data
   is
   begin
      return Unit.Lex_Env_Data'Unrestricted_Access;
   end Get_Lex_Env_Data;

   ---------------------------
   -- Reset_Property_Caches --
   ---------------------------

   procedure Reset_Property_Caches (Unit : access Analysis_Unit_Type) is

      -----------
      -- Visit --
      -----------

      function Visit
        (Node : access ${root_node_value_type}'Class)
         return Visit_Status
      is
      begin
         Node.Reset_Property_Caches;
         return Into;
      end Visit;

   begin
      if Unit.Has_Filled_Caches and Unit.AST_Root /= null then
         Unit.AST_Root.Traverse (Visit'Access);
      end if;
      Unit.Has_Filled_Caches := False;
   end Reset_Property_Caches;

end ${_self.ada_api_settings.lib_name}.Analysis;
