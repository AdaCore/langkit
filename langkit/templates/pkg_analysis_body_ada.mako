## vim: filetype=makoada

<%namespace name="entities"   file="entities_ada.mako" />
<%namespace name="exts"       file="extensions.mako" />
<%namespace name="list_types" file="list_types_ada.mako" />

<% root_node_array = T.root_node.array %>

with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

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

with ${ada_lib_name}.Analysis.Converters;
use ${ada_lib_name}.Analysis.Converters;
with ${ada_lib_name}.Lexer;

${(exts.with_clauses(with_clauses + [
   ((ctx.default_unit_provider.unit_fqn, False)
    if ctx.default_unit_provider else None),
]))}

package body ${ada_lib_name}.Analysis is

   use ${ada_lib_name}.Implementation;
   use AST_Envs;

   ---------------------------
   -- Unit_Provider_Wrapper --
   ---------------------------

   type Unit_Provider_Wrapper is new Internal_Unit_Provider with record
      Internal : Unit_Provider_Access_Cst;
   end record;

   overriding function Get_Unit_Filename
     (Provider : Unit_Provider_Wrapper;
      Name     : Text_Type;
      Kind     : Unit_Kind) return String
   is (Provider.Internal.Get_Unit_Filename (Name, Kind));

   overriding function Get_Unit
     (Provider    : Unit_Provider_Wrapper;
      Context     : Internal_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Internal_Unit
   is (Internal_Unit (Provider.Internal.Get_Unit (Analysis_Context (Context),
                                                  Name, Kind, Charset,
                                                  Reparse)));

   ------------
   -- Create --
   ------------

   function Create
     (Charset     : String := Default_Charset;
      With_Trivia : Boolean := True
      % if ctx.default_unit_provider:
         ; Unit_Provider : Unit_Provider_Access_Cst := null
      % endif
     ) return Analysis_Context
   is
      % if ctx.default_unit_provider:
         P : constant Internal_Unit_Provider_Access :=
           new Unit_Provider_Wrapper'
             (Internal => (if Unit_Provider = null
                           then ${ctx.default_unit_provider.fqn}
                           else Unit_Provider));
      % endif
      Result         : constant Internal_Context := Create
        (Charset,
         With_Trivia
         % if ctx.default_unit_provider:
            , Internal_Unit_Provider_Access_Cst (P)
         % endif
         );
   begin
      return Analysis_Context (Result);
   end Create;

   ---------------------
   -- Has_With_Trivia --
   ---------------------

   function Has_With_Trivia (Context : Analysis_Context) return Boolean is
   begin
      return Has_With_Trivia (Internal_Context (Context));
   end Has_With_Trivia;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Context : Analysis_Context) is
   begin
      Inc_Ref (Internal_Context (Context));
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Context : in out Analysis_Context) is
      C : Internal_Context := Internal_Context (Context);
   begin
      Dec_Ref (C);
      Context := Analysis_Context (C);
   end Dec_Ref;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context; Discard : Boolean) is
   begin
      Discard_Errors_In_Populate_Lexical_Env
        (Internal_Context (Context), Discard);
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context; Timeout : Natural) is
   begin
      Set_Logic_Resolution_Timeout (Internal_Context (Context), Timeout);
   end Set_Logic_Resolution_Timeout;

   --------------------------
   -- Has_Rewriting_Handle --
   --------------------------

   function Has_Rewriting_Handle (Context : Analysis_Context) return Boolean is
   begin
      return Has_Rewriting_Handle (Internal_Context (Context));
   end Has_Rewriting_Handle;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context       : Analysis_Context;
      Unit_Filename : String) return Boolean is
   begin
      return Has_Unit (Internal_Context (Context), Unit_Filename);
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Analysis_Context;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      return Analysis_Unit
        (Get_From_File (Internal_Context (Context), Filename, Charset,
                        Reparse, Rule));
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context;
      Filename : String;
      Charset  : String := "";
      Buffer   : String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      return Analysis_Unit
        (Get_From_Buffer (Internal_Context (Context), Filename, Charset,
                          Buffer, Rule));
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Analysis_Context;
      Filename : String;
      Error    : String;
      Charset  : String := "";
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Result : constant Internal_Unit :=
         Implementation.Get_With_Error (Internal_Context (Context), Filename,
                                        Error, Charset, Rule);
   begin
      return Analysis_Unit (Result);
   end Get_With_Error;

   % if ctx.default_unit_provider:

   -----------------------
   -- Get_From_Provider --
   -----------------------

   function Get_From_Provider
     (Context : Analysis_Context;
      Name    : Text_Type;
      Kind    : Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit is
   begin
      return Analysis_Unit
        (Get_From_Provider (Internal_Context (Context), Name, Kind,
                            Charset, Reparse));
   end Get_From_Provider;

   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Analysis_Context) return Unit_Provider_Access_Cst
   is
      Provider : constant Internal_Unit_Provider_Access_Cst :=
         Unit_Provider (Internal_Context (Context));
   begin
      --  By design, Unit_Provider_Wrapper is supposed to be the only
      --  implementation of the Internal_Unit_Provider interface.
      if Provider.all not in Unit_Provider_Wrapper'Class then
         raise Program_Error;
      end if;

      return Unit_Provider_Wrapper (Provider.all).Internal;
   end Unit_Provider;

   % endif

   ------------
   -- Remove --
   ------------

   procedure Remove (Context : Analysis_Context; Filename : String) is
   begin
      Remove (Internal_Context (Context), Filename);
   end Remove;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Analysis_Context) is
      C : Internal_Context := Internal_Context (Context);
   begin
      Destroy (C);
      Context := Analysis_Context (C);
   end Destroy;

   -------------
   -- Context --
   -------------

   function Context (Unit : Analysis_Unit) return Analysis_Context is
   begin
      return Analysis_Context (Context (Internal_Unit (Unit)));
   end Context;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Unit : Analysis_Unit) is
   begin
      Inc_Ref (Internal_Unit (Unit));
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Unit : in out Analysis_Unit) is
      U : Internal_Unit := Internal_Unit (Unit);
   begin
      Dec_Ref (U);
      Unit := Analysis_Unit (U);
   end Dec_Ref;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Analysis_Unit; Charset : String := "") is
   begin
      Reparse (Internal_Unit (Unit), Charset);
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit : Analysis_Unit; Charset : String := ""; Buffer  : String) is
   begin
      Reparse (Internal_Unit (Unit), Charset, Buffer);
   end Reparse;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Unit : Analysis_Unit) is
   begin
      Populate_Lexical_Env (Internal_Unit (Unit));
   end Populate_Lexical_Env;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Analysis_Unit) return String is
   begin
      return Get_Filename (Internal_Unit (Unit));
   end Get_Filename;

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Analysis_Unit) return String is
   begin
      return Get_Charset (Internal_Unit (Unit));
   end Get_Charset;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Analysis_Unit) return Boolean is
   begin
      return Has_Diagnostics (Internal_Unit (Unit));
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Analysis_Unit) return Diagnostics_Array is
   begin
      return Diagnostics (Internal_Unit (Unit));
   end Diagnostics;

   ---------------------------
   -- Format_GNU_Diagnostic --
   ---------------------------

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit; D : Diagnostic) return String is
   begin
      return Format_GNU_Diagnostic (Internal_Unit (Unit), D);
   end Format_GNU_Diagnostic;

   ----------
   -- Root --
   ----------

   function Root (Unit : Analysis_Unit) return ${root_entity.api_name} is
   begin
      return Create_Entity (Root (Internal_Unit (Unit)));
   end Root;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Analysis_Unit) return Token_Type is
   begin
      return First_Token (Internal_Unit (Unit));
   end First_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Analysis_Unit) return Token_Type is
   begin
      return Last_Token (Internal_Unit (Unit));
   end Last_Token;

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Analysis_Unit) return Natural is
   begin
      return Token_Count (Internal_Unit (Unit));
   end Token_Count;

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Analysis_Unit) return Natural is
   begin
      return Trivia_Count (Internal_Unit (Unit));
   end Trivia_Count;

   ----------
   -- Text --
   ----------

   function Text (Unit : Analysis_Unit) return Text_Type is
   begin
      return Text (Internal_Unit (Unit));
   end Text;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Analysis_Unit; Sloc : Source_Location) return Token_Type is
   begin
      return Lookup_Token (Internal_Unit (Unit), Sloc);
   end Lookup_Token;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Analysis_Unit) is
   begin
      Dump_Lexical_Env (Internal_Unit (Unit));
   end Dump_Lexical_Env;

   ------------------------
   -- Trigger_Envs_Debug --
   ------------------------

   procedure Trigger_Envs_Debug (Is_Active : Boolean) is
   begin
      GNATCOLL.Traces.Set_Active (AST_Envs.Me, Is_Active);
   end Trigger_Envs_Debug;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Analysis_Unit; Show_Slocs : Boolean := True) is
   begin
      Print (Internal_Unit (Unit), Show_Slocs);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Analysis_Unit) is
   begin
      PP_Trivia (Internal_Unit (Unit));
   end PP_Trivia;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context (Unit : Analysis_Unit) return Analysis_Context is
     (Context (Unit));
   --  TODO??? Remove this, Context already does the job

   -----------
   -- Image --
   -----------

   function Image (Value : Boolean) return String
   is (if Value then "True" else "False");

   ${entities.bodies()}

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

end ${ada_lib_name}.Analysis;
