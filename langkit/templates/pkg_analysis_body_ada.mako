## vim: filetype=makoada

with Ada.Containers;                  use Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Text;    use Langkit_Support.Text;

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
      Get_Parser : access function (Unit : Analysis_Unit) return Parser_Type);
   --  Helper for Get_Unit and the public Reparse procedures: parse an analysis
   --  unit using Get_Parser and replace Unit's AST_Root and the diagnostics
   --  with the parsers's output.

   function Get_Unit
     (Context           : Analysis_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Get_Parser        : access function (Unit : Analysis_Unit)
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
     (Charset : String := ${string_repr(_self.default_charset)})
      return Analysis_Context
   is
   begin
      return new Analysis_Context_Type'
        (Units_Map  => <>,
         Symbols    => Create,
         Charset    => To_Unbounded_String (Charset),
         Root_Scope => AST_Envs.Create (null, null));
   end Create;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Context           : Analysis_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Get_Parser        : access function (Unit : Analysis_Unit)
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
           (Context      => Context,
            Ref_Count    => 1,
            AST_Root     => null,
            File_Name    => Fname,
            Charset      => <>,
            TDH          => <>,
            Diagnostics  => <>,
            With_Trivia  => With_Trivia,
            Rule         => Rule,
            AST_Mem_Pool => No_Pool,
            Destroyables => Destroyable_Vectors.Empty_Vector);
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
         Do_Parsing (Unit, Get_Parser);
      end if;

      return Unit;
   end Get_Unit;

   ----------------
   -- Do_Parsing --
   ----------------

   procedure Do_Parsing
     (Unit       : Analysis_Unit;
      Get_Parser : access function (Unit : Analysis_Unit) return Parser_Type)
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
      if Unit.AST_Mem_Pool /= No_Pool then
         Free (Unit.AST_Mem_Pool);
      end if;
      Unit.AST_Root := null;
      Unit.Diagnostics.Clear;

      --  Now create the parser. This is where lexing occurs, so this is where
      --  we get most "setup" issues: missing input file, bad charset, etc.
      --  If we have such an error, catch it, turn it into diagnostics and
      --  abort parsing.

      declare
         use Ada.Exceptions;
      begin
         Parser := Get_Parser (Unit);
      exception
         when Exc : Name_Error =>
            --  This happens when we cannot open the source file for lexing:
            --  return an unit anyway with diagnostics indicating what happens.

            Add_Diagnostic
              ("Cannot open source file: " & Exception_Message (Exc));
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
      function Get_Parser (Unit : Analysis_Unit) return Parser_Type
      is (Create_From_File (Filename, To_String (Unit.Charset),
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
      function Get_Parser (Unit : Analysis_Unit) return Parser_Type
      is (Create_From_Buffer (Buffer, To_String (Unit.Charset),
                              Analysis_Unit_Interface (Unit), With_Trivia));
   begin
      return Get_Unit (Context, Filename, Charset, True, Get_Parser'Access,
                       With_Trivia, Rule);
   end Get_From_Buffer;

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
      function Get_Parser (Unit : Analysis_Unit) return Parser_Type
      is (Create_From_File (To_String (Unit.File_Name),
                            To_String (Unit.Charset),
                            Analysis_Unit_Interface (Unit)));
   begin
      Update_Charset (Unit, Charset);
      Do_parsing (Unit, Get_Parser'Access);
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit    : Analysis_Unit;
      Charset : String := "";
      Buffer  : String)
   is
      function Get_Parser (Unit : Analysis_Unit) return Parser_Type
      is (Create_From_Buffer (Buffer, To_String (Unit.Charset),
                              Analysis_Unit_Interface (Unit)));
   begin
      Update_Charset (Unit, Charset);
      Do_parsing (Unit, Get_Parser'Access);
      Unit.Charset := To_Unbounded_String (Charset);
   end Reparse;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Unit : Analysis_Unit) is
      Unit_Var : Analysis_Unit := Unit;
   begin
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
      Result : Diagnostics_Array (0 .. Natural (Unit.Diagnostics.Length) - 1);
      I      : Natural := 0;
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

end ${_self.ada_api_settings.lib_name}.Analysis;
