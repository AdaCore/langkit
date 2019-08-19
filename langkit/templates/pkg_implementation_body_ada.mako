## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="exts"          file="extensions.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />
<%namespace name="memoization"   file="memoization_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />

<% root_node_array = T.root_node.array %>

with Ada.Containers;                  use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with GNATCOLL.Traces;

with Langkit_Support.Hashes;  use Langkit_Support.Hashes;
with Langkit_Support.Images;  use Langkit_Support.Images;
with Langkit_Support.Relative_Get;
with Langkit_Support.Slocs;   use Langkit_Support.Slocs;
with Langkit_Support.Text;    use Langkit_Support.Text;

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

with ${ada_lib_name}.Analysis;   use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Converters; use ${ada_lib_name}.Converters;
with ${ada_lib_name}.Introspection_Implementation;

${(exts.with_clauses(with_clauses + [
   ((ctx.env_hook_subprogram.unit_fqn, False, False)
    if ctx.env_hook_subprogram else None),
   ((ctx.symbol_canonicalizer.unit_fqn, False, False)
    if ctx.symbol_canonicalizer else None)
]))}

## Generate a dispatching case statement for the root node class. It will keep
## all the node classes which pass the predicate 'pred'. The caller needs to
## pass two def blocks, 'action', which takes the node kind as parameter, and
## emits the action for each matched node kind, and 'default', taking no
## parameter. and emitting the default action.
<%def name="case_dispatch(pred)">
   <%
   node_types = list(reversed(filter(pred, ctx.astnode_types)))
   concrete_types, _ = ctx.collapse_concrete_nodes(
       ctx.root_grammar_class, node_types
   )
   concrete_mappings = zip(node_types, concrete_types)
   %>

   case Self.Kind is
      % for node, subclasses in concrete_mappings:
         % if subclasses:
            when ${ctx.astnode_kind_set(subclasses)} =>
            ${caller.action(node)}
         % endif
      % endfor
      when others => ${caller.default()}
   end case;
</%def>

package body ${ada_lib_name}.Implementation is

   package Context_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Internal_Context);

   type Contexts_Destructor is limited
      new Ada.Finalization.Limited_Controlled with null record;
   overriding procedure Finalize (CD : in out Contexts_Destructor);
   --  Helper to destroy all contexts when terminating the process

   protected Context_Pool is

      procedure Acquire (Context : out Internal_Context)
         with Post => Context /= null;
      --  If a context is free for reuse, increment its serial number and
      --  return it. Otherwise, allocate a new one. In any case, this does not
      --  initialize it, except for the Serial_Number and Released fields.

      procedure Release (Context : in out Internal_Context)
         with Pre  => Context /= null,
              Post => Context = null;
      --  Tag Context as free for reuse and set it to null

      procedure Free;
      --  Free all contexts in this pool. Intended to be called only when the
      --  process is terminating, to avoid reported memory leaks.

   private

      Available : Context_Vectors.Vector;
      --  List of allocated contexts that can be re-used right now

      CD : Contexts_Destructor with Unreferenced;
      --  Singleton whose only purpose is to free all contexts in Available
      --  when finalized.

   end Context_Pool;

   generic
      type T (<>) is limited private;
      type T_Access is access all T;
      with procedure Destroy (Object : in out T_Access);
   procedure Register_Destroyable_Gen
     (Unit : Internal_Unit; Object : T_Access);
   --  Generic procedure to register an object so that it is automatically
   --  destroyed when Unit is destroyed.

   procedure Register_Destroyable_Helper
     (Unit    : Internal_Unit;
      Object  : System.Address;
      Destroy : Destroy_Procedure);
   --  Common underlying implementation for Register_Destroyable_Gen

   function Solve_Wrapper
     (R            : Relation;
      Context_Node : ${T.root_node.name}) return Boolean;
   --  Wrapper for Langkit_Support.Adalog.Solve; will handle setting the debug
   --  strings in the equation if in debug mode.

   procedure Destroy (Env : in out Lexical_Env_Access);

   function Snaps_At_Start (Self : ${T.root_node.name}) return Boolean;
   function Snaps_At_End (Self : ${T.root_node.name}) return Boolean;

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

   -----------
   -- Image --
   -----------

   function Image (Self : Symbol_Type) return ${T.String.name} is
      T      : constant Text_Type := Symbols.Image (Self);
      Result : constant ${T.String.name} :=
         ${T.String.constructor_name} (T'Length);
   begin
      Result.Items := T;
      return Result;
   end Image;

   ------------------
   -- Context_Pool --
   ------------------

   protected body Context_Pool is

      -------------
      -- Acquire --
      -------------

      procedure Acquire (Context : out Internal_Context) is
      begin
         if Available.Is_Empty then
            Context := new Analysis_Context_Type;
            Context.Serial_Number := 1;
         else
            Context := Available.Last_Element;
            Context.Serial_Number := Context.Serial_Number + 1;
            Available.Delete_Last;
         end if;
         Context.Released := False;
      end Acquire;

      -------------
      -- Release --
      -------------

      procedure Release (Context : in out Internal_Context) is
      begin
         Available.Append (Context);
         Context.Released := True;
         Context := null;
      end Release;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         for C of Available loop
            Free (C);
         end loop;
      end Free;

   end Context_Pool;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (CD : in out Contexts_Destructor) is
      pragma Unreferenced (CD);
   begin
      Context_Pool.Free;
   end Finalize;

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

   function To_Lookup_Kind_Type (K : Lookup_Kind) return Lookup_Kind_Type
   is
     (Lookup_Kind_Type'Val (Lookup_Kind'Pos (K)));

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset       : String;
      Unit_Provider : Internal_Unit_Provider_Access;
      With_Trivia   : Boolean;
      Tab_Stop      : Positive) return Internal_Context
   is
      Actual_Charset : constant String :=
        (if Charset = "" then Default_Charset else Charset);
      Symbols        : constant Symbol_Table := Create_Symbol_Table;
      Context        : Internal_Context;
   begin
      Context_Pool.Acquire (Context);
      Context.Ref_Count := 1;
      Context.Symbols := Symbols;
      Context.Charset := To_Unbounded_String (Actual_Charset);
      Context.Tab_Stop := Tab_Stop;
      Context.With_Trivia := With_Trivia;
      Context.Root_Scope := AST_Envs.Create_Lexical_Env
        (Parent => AST_Envs.No_Env_Getter,
         Node   => null,
         Owner  => No_Analysis_Unit);

      Context.Unit_Provider := Unit_Provider;

      Initialize (Context.Parser);

      Context.Discard_Errors_In_Populate_Lexical_Env := True;
      Context.Logic_Resolution_Timeout := 100_000;
      Context.In_Populate_Lexical_Env := False;
      Context.Cache_Version := 0;
      Context.Reparse_Cache_Version := 0;

      Context.Rewriting_Handle := No_Rewriting_Handle_Pointer;
      Context.Templates_Unit := No_Analysis_Unit;

      ${exts.include_extension(ctx.ext('analysis', 'context', 'create'))}

      return Context;
   end Create_Context;

   -----------------
   -- Create_Unit --
   -----------------

   function Create_Unit
     (Context             : Internal_Context;
      Normalized_Filename : Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit
   is
      use Units_Maps;

      Unit : Internal_Unit;
   begin
      Unit := Create_Special_Unit
        (Context, Normalized_Filename, Charset, Rule);
      Context.Units.Insert (Normalized_Filename, Unit);
      return Unit;
   end Create_Unit;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Input             : Internal_Lexer_Input;
      Rule              : Grammar_Rule) return Internal_Unit
   is
      use Units_Maps;

      Normalized_Filename : constant GNATCOLL.VFS.Virtual_File :=
         Normalized_Unit_Filename (Context, Filename);

      Cur     : constant Cursor :=
         Context.Units.Find (Normalized_Filename);
      Created : constant Boolean := Cur = No_Element;
      Unit    : Internal_Unit;

      Actual_Charset : Unbounded_String;
      Refined_Input  : Internal_Lexer_Input := Input;

   begin
      --  Determine which encoding to use. The parameter comes first, then the
      --  unit-specific default, then the context-specific one.

      if Charset'Length /= 0 then
         Actual_Charset := To_Unbounded_String (Charset);
      elsif not Created then
         Actual_Charset := Element (Cur).Charset;
      else
         Actual_Charset := Context.Charset;
      end if;

      if Refined_Input.Kind = File then
         Refined_Input.Filename := Normalized_Filename;
      end if;

      if Refined_Input.Kind in File | Bytes_Buffer then
         Refined_Input.Charset := Actual_Charset;

         --  Unless the caller requested a specific charset for this unit,
         --  allow the lexer to automatically discover the source file encoding
         --  before defaulting to the context-specific one. We do this trying
         --  to match a byte order mark.

         Refined_Input.Read_BOM := Charset'Length = 0;
      end if;

      --  Create the Internal_Unit if needed

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
            Do_Parsing (Unit, Refined_Input, Reparsed);
            Update_After_Reparse (Unit, Reparsed);
         end;
      end if;

      return Unit;
   end Get_Unit;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context       : Internal_Context;
      Unit_Filename : String) return Boolean is
   begin
      return Context.Units.Contains
        (Normalized_Unit_Filename (Context, Unit_Filename));
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Reparse  : Boolean;
      Rule     : Grammar_Rule) return Internal_Unit
   is
      Input : constant Internal_Lexer_Input :=
        (Kind     => File,
         Charset  => <>,
         Read_BOM => False,
         Filename => <>);
   begin
      return Get_Unit (Context, Filename, Charset, Reparse, Input, Rule);
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Buffer   : String;
      Rule     : Grammar_Rule) return Internal_Unit
   is
      Input : constant Internal_Lexer_Input :=
        (Kind        => Bytes_Buffer,
         Charset     => <>,
         Read_BOM    => False,
         Bytes       => Buffer'Address,
         Bytes_Count => Buffer'Length);
   begin
      return Get_Unit (Context, Filename, Charset, True, Input, Rule);
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Internal_Context;
      Filename : String;
      Error    : String;
      Charset  : String;
      Rule     : Grammar_Rule) return Internal_Unit
   is
      use Units_Maps;

      Normalized_Filename : constant Virtual_File :=
         Normalized_Unit_Filename (Context, Filename);
      Cur                 : constant Cursor :=
         Context.Units.Find (Normalized_Filename);
   begin
      if Cur = No_Element then
         declare
            Unit : constant Internal_Unit := Create_Unit
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

   % if ctx.default_unit_provider:

   -----------------------
   -- Get_From_Provider --
   -----------------------

   function Get_From_Provider
     (Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String;
      Reparse : Boolean) return Internal_Unit is
   begin
      return Context.Unit_Provider.Get_Unit
        (Context, Name, Kind, Charset, Reparse);

   exception
      when Property_Error =>
         raise Invalid_Unit_Name_Error with
            "Invalid unit name: " & Image (Name, With_Quotes => True)
            & " (" & Analysis_Unit_Kind'Image (Kind) & ")";
   end Get_From_Provider;

   % endif

   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Internal_Context) return Internal_Unit_Provider_Access
   is (Context.Unit_Provider);

   ----------
   -- Hash --
   ----------

   function Hash (Context : Internal_Context) return Hash_Type is
      function H is new Hash_Access (Analysis_Context_Type, Internal_Context);
   begin
      return H (Context);
   end Hash;

   ---------------------
   -- Has_With_Trivia --
   ---------------------

   function Has_With_Trivia (Context : Internal_Context) return Boolean is
   begin
      return Context.With_Trivia;
   end Has_With_Trivia;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Internal_Context; Discard : Boolean) is
   begin
      Context.Discard_Errors_In_Populate_Lexical_Env := Discard;
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Internal_Context; Timeout : Natural) is
   begin
      Context.Logic_Resolution_Timeout := Timeout;
   end Set_Logic_Resolution_Timeout;

   --------------------------
   -- Has_Rewriting_Handle --
   --------------------------

   function Has_Rewriting_Handle (Context : Internal_Context) return Boolean is
   begin
      return Context.Rewriting_Handle /= No_Rewriting_Handle_Pointer;
   end Has_Rewriting_Handle;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Context : Internal_Context) is
   begin
      if Context /= null then
         Context.Ref_Count := Context.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Context : in out Internal_Context) is
   begin
      if Context /= null then
         Context.Ref_Count := Context.Ref_Count - 1;
         if Context.Ref_Count = 0 then
            Destroy (Context);
         end if;
      end if;
   end Dec_Ref;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : in out Internal_Context) is
   begin
      --  If we are asked to free this context, it means that no one else have
      --  references to its analysis units, so it's safe to destroy these.
      for Unit of Context.Units loop
         Destroy (Unit);
      end loop;
      Context.Units := Units_Maps.Empty_Map;
      Context.Filenames := Virtual_File_Maps.Empty_Map;

      Destroy (Context.Templates_Unit);
      AST_Envs.Destroy (Context.Root_Scope);
      Destroy (Context.Symbols);
      Destroy (Context.Parser);
      Destroy (Context.Unit_Provider);
      Context_Pool.Release (Context);
   end Destroy;

   -------------
   -- Context --
   -------------

   function Context (Unit : Internal_Unit) return Internal_Context is
   begin
      return Unit.Context;
   end Context;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Internal_Unit) return Hash_Type is
      function H is new Hash_Access (Analysis_Unit_Type, Internal_Unit);
   begin
      return H (Unit);
   end Hash;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Internal_Unit; Charset : String) is
      Dummy : constant Internal_Unit := Get_From_File
        (Unit.Context, +Unit.Filename.Full_Name, Charset,
         Reparse => True,
         Rule    => Unit.Rule);
   begin
      null;
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Internal_Unit; Charset : String; Buffer : String)
   is
      Dummy : constant Internal_Unit := Get_From_Buffer
        (Unit.Context, +Unit.Filename.Full_Name, Charset, Buffer, Unit.Rule);
   begin
      null;
   end Reparse;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Unit : Internal_Unit) is
      Context : constant Internal_Context := Unit.Context;

      Has_Errors : Boolean := False;
      --  Whether at least one Property_Error occurred during this PLE pass

      Saved_In_Populate_Lexical_Env : constant Boolean :=
         Unit.Context.In_Populate_Lexical_Env;

      procedure Reset_Envs_Caches (Unit : Internal_Unit) is
         procedure Internal (Node : ${T.root_node.name}) is
         begin
            if Node = null then
               return;
            end if;
            Reset_Caches (Node.Self_Env);
            for I in 1 .. Children_Count (Node) loop
               Internal (Child (Node, I));
            end loop;
         end Internal;
      begin
         Internal (Unit.AST_Root);
      end Reset_Envs_Caches;

   begin
      --  TODO??? Handle env invalidation when reparsing a unit and when a
      --  previous call raised a Property_Error.
      if Unit.Is_Env_Populated then
         return;
      end if;
      Unit.Is_Env_Populated := True;

      if Unit.AST_Root = null then
         return;
      end if;

      GNATCOLL.Traces.Trace (Main_Trace, "Populating lexical envs for unit: "
                                         & Basename (Unit));
      GNATCOLL.Traces.Increase_Indent (Main_Trace);

      Context.In_Populate_Lexical_Env := True;

      % if ctx.ple_unit_root:
         if Unit.AST_Root /= null then
            --  If the tree root is a list of PLE units, populate envs for each
            --  one of them.
            if Unit.AST_Root.Kind = ${ctx.ple_unit_root.list.ada_kind_name}
            then
               for I in 1 .. Children_Count (Unit.AST_Root) loop
                  Has_Errors := Populate_Lexical_Env (Child (Unit.AST_Root, I))
                                or else Has_Errors;
               end loop;

            --  Otherwise, populate envs only if the root is a PLE unit itself
            elsif Unit.AST_Root.Kind = ${ctx.ple_unit_root.ada_kind_name} then
               Has_Errors := Populate_Lexical_Env (Unit.AST_Root);
            end if;
         end if;
      % else:
         Has_Errors := Populate_Lexical_Env (Unit.AST_Root);
      % endif

      Context.In_Populate_Lexical_Env :=
         Saved_In_Populate_Lexical_Env;

      GNATCOLL.Traces.Decrease_Indent (Main_Trace);

      Reset_Envs_Caches (Unit);

      if Has_Errors and then not Context.Discard_Errors_In_Populate_Lexical_Env
      then
         raise Property_Error with
            "errors occurred in Populate_Lexical_Env";
      end if;
   end Populate_Lexical_Env;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Internal_Unit) return String is
     (+Unit.Filename.Full_Name);

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Internal_Unit) return String is
   begin
      return To_String (Unit.Charset);
   end Get_Charset;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Internal_Unit) return Boolean is
   begin
      return not Unit.Diagnostics.Is_Empty;
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Internal_Unit) return Diagnostics_Array is
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
     (Unit : Internal_Unit; D : Diagnostic) return String
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

   ----------
   -- Root --
   ----------

   function Root (Unit : Internal_Unit) return ${T.root_node.name} is
     (Unit.AST_Root);

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Internal_Unit) return Token_Reference is
     (Wrap_Token_Reference (Unit.TDH'Access,
                            First_Token_Or_Trivia (Unit.TDH)));

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Internal_Unit) return Token_Reference is
     (Wrap_Token_Reference (Unit.TDH'Access, Last_Token_Or_Trivia (Unit.TDH)));

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Internal_Unit) return Natural is
     (Unit.TDH.Tokens.Length);

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Internal_Unit) return Natural is
     (Unit.TDH.Trivias.Length);

   ----------
   -- Text --
   ----------

   function Text (Unit : Internal_Unit) return Text_Type is
   begin
      return Text (First_Token (Unit), Last_Token (Unit));
   end Text;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Token_Reference
   is
      Result : constant Token_Or_Trivia_Index := Lookup_Token (Unit.TDH, Sloc);
   begin
      return Wrap_Token_Reference (Unit.TDH'Access, Result);
   end Lookup_Token;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Internal_Unit) is
      Node     : constant ${T.root_node.name} := Unit.AST_Root;
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
               Get_Env_Id (Get_Env (Env.Env.Parent, No_Entity_Info), State));
            Explore_Parent_Chain (Get_Env (Env.Env.Parent, No_Entity_Info));
         end if;
      end Explore_Parent_Chain;

      --------------
      -- Internal --
      --------------

      procedure Internal (Current : ${T.root_node.name}) is
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
            Parent := Get_Env (Env.Env.Parent, No_Entity_Info);
            Explore_Parent := not State.Env_Ids.Contains (Parent);

            Dump_One_Lexical_Env
              (Env, Get_Env_Id (Env, State), Get_Env_Id (Parent, State));

            if Explore_Parent then
               Explore_Parent_Chain (Parent);
            end if;
         end if;

         for Child of ${root_node_array.array_type_name}'(Children (Current))
         loop
            Internal (Child);
         end loop;
      end Internal;
      --  This procedure implements the main recursive logic of dumping the
      --  environments.
   begin
      Internal (${T.root_node.name} (Node));
   end Dump_Lexical_Env;

   -----------
   -- Print --
   -----------

   procedure Print (Unit : Internal_Unit; Show_Slocs : Boolean) is
   begin
      if Unit.AST_Root = null then
         Put_Line ("<empty analysis unit>");
      else
         Print (Unit.AST_Root, Show_Slocs);
      end if;
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Internal_Unit) is

      procedure Process (Trivia : Token_Index) is
         Data : constant Stored_Token_Data :=
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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Unit : in out Internal_Unit) is
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

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : ${T.root_node.name}) return Boolean is
   begin
      return Is_Token_Node (Node.Kind);
   end Is_Token_Node;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic (Node : ${T.root_node.name}) return Boolean is
   begin
      return Node.Kind in Synthetic_Nodes;
   end Is_Synthetic;

   ------------------------------
   -- Register_Destroyable_Gen --
   ------------------------------

   procedure Register_Destroyable_Gen
     (Unit : Internal_Unit; Object : T_Access)
   is
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Destroy_Procedure);
      procedure Destroy_Procedure (Object : in out T_Access) renames Destroy;
   begin
      Register_Destroyable_Helper
        (Unit,
         Object.all'Address,
         Convert (Destroy_Procedure'Address));
   end Register_Destroyable_Gen;

   procedure Register_Destroyable is new Register_Destroyable_Gen
     (AST_Envs.Lexical_Env_Type, AST_Envs.Lexical_Env_Access, Destroy);

   ${array_types.body(root_node_array)}

   % for array_type in ctx.array_types:
   % if array_type.element_type.should_emit_array_type:
   ${array_types.body(array_type)}
   % endif
   % endfor

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Internal_Unit) return Boolean is
   begin
      return Left.Filename < Right.Filename;
   end "<";

   % if ctx.has_memoization:
      ${memoization.body()}
   % endif

   % if ctx.has_env_assoc:
      procedure Add_To_Env
        (Self        : ${T.root_node.name};
         Mapping     : ${T.env_assoc.name};
         Initial_Env : Lexical_Env;
         Resolver    : Entity_Resolver);
      --  Helper for Populate_Lexical_Env: add the key/element Mapping in the
      --  Env lexical environment using the given metadata (MD).
   % endif

   % if ctx.has_ref_env:
      procedure Ref_Env
        (Self                : ${T.root_node.name};
         Dest_Env            : Lexical_Env;
         Ref_Env_Nodes       : in out ${T.root_node.array.name};
         Resolver            : Lexical_Env_Resolver;
         Kind                : Ref_Kind;
         Cats                : Ref_Categories;
         Shed_Rebindings     : Boolean);
      --  Add referenced environments to Self.Self_Env. Calling this takes an
      --  ownership share for Ref_Env_Nodes.
   % endif

   -------------------
   -- Solve_Wrapper --
   -------------------

   function Solve_Wrapper
     (R            : Relation;
      Context_Node : ${T.root_node.name}) return Boolean is
   begin
      if Context_Node /= null and then Langkit_Support.Adalog.Debug.Debug then
         Assign_Names_To_Logic_Vars (Context_Node);
      end if;

      begin
         return Solve (R, Context_Node.Unit.Context.Logic_Resolution_Timeout);
      exception
         when Langkit_Support.Adalog.Early_Binding_Error =>
            raise Property_Error with "invalid equation for logic resolution";
         when Langkit_Support.Adalog.Timeout_Error =>
            raise Property_Error with "logic resolution timed out";
      end;
   end Solve_Wrapper;

   % if ctx.has_env_assoc:
      ----------------
      -- Add_To_Env --
      ----------------

      procedure Add_To_Env
        (Self        : ${T.root_node.name};
         Mapping     : ${T.env_assoc.name};
         Initial_Env : Lexical_Env;
         Resolver    : Entity_Resolver)
      is
         Root_Scope : Lexical_Env renames Self.Unit.Context.Root_Scope;
         MD         : ${T.env_md.name} renames Mapping.Metadata;

         Dest_Env : Lexical_Env :=
           (if Mapping.Dest_Env = Empty_Env
            then Initial_Env
            else Mapping.Dest_Env);
      begin
         if Mapping = No_Env_Assoc then
            return;
         end if;

         if Mapping.Val.Unit /= Self.Unit then
            raise Property_Error with "Cannot add_to_env an AST node that"
                                      & " comes from another analysis unit";
         end if;

         <% astnode_fields = [f for f in T.env_md.get_fields()
                              if f.type.is_ast_node] %>
         % if astnode_fields:
            --  Make sure metadata does not contain any foreign node
            if ${(
               ' or else '.join(
                   ['({n} /= null and then {n}.Unit /= Self.Unit)'.format(
                       n='MD.{}'.format(f.name)
                   ) for f in astnode_fields]
               )
            )}
            then
               raise Property_Error
                  with "Cannot add metadata that contains foreign nodes";
            end if;
         % endif

         --  Add the element to the environment
         Add (Self     => Dest_Env,
              Key      => Mapping.Key,
              Value    => Mapping.Val,
              MD       => MD,
              Resolver => Resolver);

         --  If we're adding the element to an environment that belongs to a
         --  different unit, then:
         if Dest_Env /= Empty_Env
            and then (Dest_Env = Root_Scope
                      or else Dest_Env.Env.Node.Unit /= Self.Unit)
         then
            --  Add the environment, the key, and the value to the list of
            --  entries contained in other units, so we can remove them when
            --  reparsing Val's unit.
            Mapping.Val.Unit.Exiled_Entries.Append
              ((Dest_Env, Mapping.Key, Mapping.Val));

            if Dest_Env /= Root_Scope then
               --  Add Val to the list of foreign nodes that Dest_Env's unit
               --  contains, so that when that unit is reparsed, we can call
               --  Add_To_Env again on those nodes.
               Dest_Env.Env.Node.Unit.Foreign_Nodes.Append
                 ((Mapping.Val, Self.Unit));
            end if;
         end if;
      end Add_To_Env;
   % endif

   % if ctx.has_ref_env:
      -------------
      -- Ref_Env --
      -------------

      procedure Ref_Env
        (Self                : ${T.root_node.name};
         Dest_Env            : Lexical_Env;
         Ref_Env_Nodes       : in out ${T.root_node.array.name};
         Resolver            : Lexical_Env_Resolver;
         Kind                : Ref_Kind;
         Cats                : Ref_Categories;
         Shed_Rebindings     : Boolean)
      is
      begin
         for N of Ref_Env_Nodes.Items loop
            if N /= null then
               if N.Unit /= Self.Unit then
                  raise Property_Error with
                     "attempt to add a referenced environment to a foreign"
                     & " unit";
               end if;
               Reference (Dest_Env, N, Resolver, Kind, Cats, Shed_Rebindings);
            end if;
         end loop;
         Dec_Ref (Ref_Env_Nodes);
      end Ref_Env;
   % endif

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Env : in out Lexical_Env_Access) is
      Mutable_Env : Lexical_Env := (Env, 0, Env.Kind, No_Analysis_Unit, 0);
   begin
      Destroy (Mutable_Env);
      Env := null;
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self              : ${T.root_node.name};
      Kind              : ${T.node_kind};
      Unit              : Internal_Unit;
      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      Parent            : ${T.root_node.name} := null;
      Self_Env          : Lexical_Env := AST_Envs.Empty_Env) is
   begin
      Self.Parent := Parent;
      Self.Kind := Kind;
      Self.Unit := Unit;

      Self.Token_Start_Index := Token_Start_Index;
      Self.Token_End_Index := Token_End_Index;

      Self.Self_Env := Self_Env;
      Self.Last_Attempted_Child := -1;

      ${astnode_types.init_user_fields(T.root_node, 'Self')}
   end Initialize;

   ---------------------
   -- Pre_Env_Actions --
   ---------------------

   function Pre_Env_Actions
     (Self                : ${T.root_node.name};
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env
   is
   begin

      <%self:case_dispatch pred="${lambda n: n.env_spec}">
      <%def name="action(node)">
         return ${node.name}_Pre_Env_Actions
           (${node.internal_conversion(T.root, 'Self')},
            Bound_Env, Root_Env, Add_To_Env_Only);
      </%def>
      <%def name="default()"> return Null_Lexical_Env; </%def>
      </%self:case_dispatch>

   end Pre_Env_Actions;

   ----------------------
   -- Post_Env_Actions --
   ----------------------

   procedure Post_Env_Actions
     (Self                : ${T.root_node.name};
      Bound_Env, Root_Env : AST_Envs.Lexical_Env) is
   begin
      <%self:case_dispatch pred="${lambda n: n.env_spec}">
      <%def name="action(n)">
         % if n.env_spec.post_actions:
         ${n.name}_Post_Env_Actions
           (${n.internal_conversion(T.root, 'Self')},
            Bound_Env, Root_Env);
         % else:
         null;
         % endif
      </%def>
      <%def name="default()"> null; </%def>
      </%self:case_dispatch>
   end Post_Env_Actions;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Node : ${T.root_node.name}) return Symbol_Type is
   begin
      return Get_Symbol (Token (Node, Node.Token_Start_Index));
   end Get_Symbol;

   ----------
   -- Text --
   ----------

   function Text
     (Node : ${T.root_node.name}) return Text_Type
   is
      Start_T : constant Token_Reference :=
         Token (Node, Node.Token_Start_Index);
      End_T   : constant Token_Reference := Token (Node, Node.Token_End_Index);
   begin
      --  No text is associated to synthetic and ghost nodes

      if Is_Synthetic (Node) then
         return "";
      end if;

      if Is_Ghost (Node) then
         return "";
      end if;

      return Text (Start_T, End_T);
   end Text;

   ---------------------
   -- Is_Visible_From --
   ---------------------

   function Is_Visible_From
     (Referenced_Env, Base_Env : AST_Envs.Lexical_Env) return Boolean is
      Referenced_Node : constant ${T.root_node.name} :=
         Referenced_Env.Env.Node;
      Base_Node       : constant ${T.root_node.name} := Base_Env.Env.Node;
   begin
      if Referenced_Node = null then
         raise Property_Error with
            "referenced environment does not belong to any analysis unit";
      elsif Base_Node = null then
         raise Property_Error with
            "base environment does not belong to any analysis unit";
      end if;
      return Is_Referenced_From (Referenced_Node.Unit, Base_Node.Unit);
   end Is_Visible_From;

   ----------
   -- Unit --
   ----------

   function Unit (Node : ${T.root_node.name}) return Internal_Unit is
   begin
      return Node.Unit;
   end Unit;

   ${array_types.body(T.LexicalEnv.array)}
   ${array_types.body(T.root_node.entity.array)}

   function Lookup_Internal
     (Node : ${T.root_node.name};
      Sloc : Source_Location) return ${T.root_node.name};
   procedure Lookup_Relative
     (Node       : ${T.root_node.name};
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out ${T.root_node.name});
   --  Implementation helpers for the looking up process

   -----------------
   -- Set_Parents --
   -----------------

   procedure Set_Parents
     (Node, Parent : ${T.root_node.name})
   is
   begin
      if Node = null then
         return;
      end if;

      Node.Parent := ${T.root_node.name} (Parent);

      for I in 1 .. Children_Count (Node) loop
         Set_Parents (Child (Node, I), Node);
      end loop;
   end Set_Parents;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Node : ${T.root_node.name}) is
   begin
      if Node = null then
         return;
      end if;

      Reset_Logic_Vars (Node);
      for I in 1 .. Children_Count (Node) loop
         Destroy (Child (Node, I));
      end loop;
   end Destroy;

   -----------
   -- Child --
   -----------

   function Child (Node  : ${T.root_node.name};
                   Index : Positive) return ${T.root_node.name}
   is
      Result          : ${T.root_node.name};
      Index_In_Bounds : Boolean;
   begin
      Get_Child (Node, Index, Index_In_Bounds, Result);
      return (if Index_In_Bounds then Result else null);
   end Child;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : ${T.root_node.name};
      Visit : access function (Node : ${T.root_node.name})
              return Visit_Status)
     return Visit_Status
   is
      Status : Visit_Status := Into;

   begin
      if Node /= null then
         Status := Visit (Node);

         --  Skip processing the child nodes if the returned status is Over
         --  or Stop. In the former case the previous call to Visit has taken
         --  care of processing the needed childs, and in the latter case we
         --  must immediately stop processing the tree.

         if Status = Into then
            for I in 1 .. Children_Count (Node) loop
               declare
                  Cur_Child : constant ${T.root_node.name} :=
                     Child (Node, I);

               begin
                  if Cur_Child /= null then
                     Status := Traverse (Cur_Child, Visit);
                     exit when Status /= Into;
                  end if;
               end;
            end loop;
         end if;
      end if;

      if Status = Stop then
         return Stop;

      --  At this stage the Over status has no sense and we just continue
      --  processing the tree.

      else
         return Into;
      end if;
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : ${T.root_node.name};
      Visit : access function (Node : ${T.root_node.name})
                               return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   ------------------------
   -- Traverse_With_Data --
   ------------------------

   function Traverse_With_Data
     (Node  : ${T.root_node.name};
      Visit : access function (Node : ${T.root_node.name};
                               Data : in out Data_Type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status
   is
      function Helper (Node : ${T.root_node.name}) return Visit_Status;

      ------------
      -- Helper --
      ------------

      function Helper (Node : ${T.root_node.name}) return Visit_Status is
      begin
         return Visit (Node, Data);
      end Helper;

      Saved_Data : Data_Type;
      Result     : Visit_Status;

   begin
      if Reset_After_Traversal then
         Saved_Data := Data;
      end if;
      Result := Traverse (Node, Helper'Access);
      if Reset_After_Traversal then
         Data := Saved_Data;
      end if;
      return Result;
   end Traverse_With_Data;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : ${T.root_node.name}) return Source_Location_Range
   is
      type Token_Anchor is (T_Start, T_End);
      type Token_Pos is record
         Pos    : Token_Index;
         Anchor : Token_Anchor;
      end record;

      TDH                    : Token_Data_Handler renames Node.Unit.TDH;
      Token_Start, Token_End : Token_Pos;

      function Get (Index : Token_Index) return Stored_Token_Data is
        (Get_Token (TDH, Index));

      function Sloc (T : Token_Pos) return Source_Location is
        (if T.Anchor = T_Start
         then Start_Sloc (Get (T.Pos).Sloc_Range)
         else End_Sloc (Get (T.Pos).Sloc_Range));

   begin
      if Is_Synthetic (Node) then
         return Sloc_Range (Node.Parent);
      end if;

      if Is_Ghost (Node) then
         Token_Start := (if Node.Token_Start_Index = 1
                         then (1, T_Start)
                         else (Node.Token_Start_Index - 1, T_End));
         Token_End := Token_Start;
      else
         Token_Start := (Node.Token_Start_Index, T_Start);
         Token_End := (Node.Token_End_Index, T_End);
      end if;

      if Snaps_At_Start (Node)
         and then not Is_Ghost (Node)
         and then Token_Start.Pos /= 1
      then
         Token_Start := (Token_Start.Pos - 1, T_End);
      end if;

      if Snaps_At_End (Node) and then Token_End.Pos /= Last_Token (TDH) then
         Token_End := (Token_End.Pos + 1, T_Start);
      end if;

      return Make_Range (Sloc (Token_Start), Sloc (Token_End));
   end Sloc_Range;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : ${T.root_node.name};
      Sloc : Source_Location) return ${T.root_node.name}
   is
      Position : Relative_Position;
      Result   : ${T.root_node.name};
   begin
      if Sloc = No_Source_Location then
         return null;
      end if;

      Lookup_Relative
        (${T.root_node.name} (Node), Sloc, Position, Result);
      return Result;
   end Lookup;

   ---------------------
   -- Lookup_Internal --
   ---------------------

   function Lookup_Internal
     (Node : ${T.root_node.name};
      Sloc : Source_Location) return ${T.root_node.name}
   is
      --  For this implementation helper (i.e. internal primitive), we can
      --  assume that all lookups fall into this node's sloc range.
      pragma Assert (Compare (Sloc_Range (Node), Sloc) = Inside);

      Children : constant ${root_node_array.array_type_name} :=
         Implementation.Children (Node);
      Pos      : Relative_Position;
      Result   : ${T.root_node.name};
   begin
      --  Look for a child node that contains Sloc (i.e. return the most
      --  precise result).

      for Child of Children loop
         --  Note that we assume here that child nodes are ordered so that the
         --  first one has a sloc range that is before the sloc range of the
         --  second child node, etc.

         if Child /= null then
            Lookup_Relative (Child, Sloc, Pos, Result);
            case Pos is
               when Before =>
                   --  If this is the first node, Sloc is before it, so we can
                   --  stop here.  Otherwise, Sloc is between the previous
                   --  child node and the next one...  so we can stop here,
                   --  too.
                   return Node;

               when Inside =>
                   return Result;

               when After =>
                   --  Sloc is after the current child node, so see with the
                   --  next one.
                   null;
            end case;
         end if;
      end loop;

      --  If we reach this point, we found no children that covers Sloc, but
      --  Node still covers it (see the assertion).
      return Node;
   end Lookup_Internal;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : ${T.root_node.name};
      Sloc : Source_Location) return Relative_Position is
   begin
      return Compare (Sloc_Range (Node), Sloc);
   end Compare;

   ---------------------
   -- Lookup_Relative --
   ---------------------

   procedure Lookup_Relative
     (Node       : ${T.root_node.name};
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out ${T.root_node.name})
   is
      Result : constant Relative_Position :=
        Compare (Node, Sloc);
   begin
      Position := Result;
      Node_Found := (if Result = Inside
                     then Lookup_Internal (Node, Sloc)
                     else null);
   end Lookup_Relative;

   -------------
   -- Compare --
   -------------

   function Compare
     (Left, Right : ${T.root_node.name};
      Relation    : Comparison_Relation) return Boolean
   is
      LS, RS : Source_Location;
   begin
      if Left = null or else Right = null or else Left.Unit /= Right.Unit then
         raise Property_Error with "invalid node comparison";
      end if;

      LS := Start_Sloc (Sloc_Range (Left));
      RS := Start_Sloc (Sloc_Range (Right));
      return (case Relation is
              when Langkit_Support.Types.Less_Than        => LS < RS,
              when Langkit_Support.Types.Less_Or_Equal    => LS <= RS,
              when Langkit_Support.Types.Greater_Than     => LS > RS,
              when Langkit_Support.Types.Greater_Or_Equal => LS >= RS);
   end Compare;

   --------------
   -- Children --
   --------------

   function Children
     (Node : ${T.root_node.name}) return ${root_node_array.array_type_name}
   is
      First : constant Integer := ${root_node_array.index_type()}'First;
      Last  : constant Integer := First + Children_Count (Node) - 1;
   begin
      return A : ${root_node_array.array_type_name} (First .. Last)
      do
         for I in First .. Last loop
            A (I) := Child (Node, I);
         end loop;
      end return;
   end Children;

   function Children
     (Node : ${T.root_node.name}) return ${root_node_array.name}
   is
      C : ${root_node_array.array_type_name} := Children (Node);
   begin
      return Ret : ${root_node_array.name} :=
         ${root_node_array.constructor_name} (C'Length)
      do
         Ret.Items := C;
      end return;
   end Children;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node        : ${T.root_node.name};
      Line_Prefix : String := "")
   is
      Children_Prefix : constant String := Line_Prefix & "|  ";
   begin
      Put_Line (Line_Prefix & Kind_Name (Node));
      for C of Children_With_Trivia (Node) loop
         case C.Kind is
            when Trivia =>
               Put_Line (Children_Prefix & Debug_Text (C.Trivia));
            when Child =>
               PP_Trivia (C.Node, Children_Prefix);
         end case;
      end loop;
   end PP_Trivia;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   function Populate_Lexical_Env (Node : ${T.root_node.name}) return Boolean
   is

      Context  : constant Internal_Context := Node.Unit.Context;
      Root_Env : constant Lexical_Env := Context.Root_Scope;

      function Populate_Internal
        (Node      : ${T.root_node.name};
         Bound_Env : Lexical_Env) return Boolean;
      --  Do the lexical env population on Node and recurse on its children

      -----------------------
      -- Populate_Internal --
      -----------------------

      function Populate_Internal
        (Node      : ${T.root_node.name};
         Bound_Env : Lexical_Env) return Boolean
      is
         Result      : Boolean := False;
         Initial_Env : Lexical_Env;
      begin
         if Node = null then
            return Result;
         end if;

         --  By default (i.e. unless env actions add a new env),
         --  the environment we store in Node is the current one.
         Node.Self_Env := Bound_Env;

         begin
            Initial_Env := Pre_Env_Actions (Node, Bound_Env, Root_Env);

            if Initial_Env /= Null_Lexical_Env then
               Node.Self_Env := Initial_Env;
            end if;

            --  Call recursively on children
            for C of ${root_node_array.array_type_name}'(Children (Node)) loop
               Result := Populate_Internal (C, Node.Self_Env) or else Result;
            end loop;

            Post_Env_Actions (Node, Initial_Env, Root_Env);
         exception
            when Property_Error =>
               return True;
         end;

         return Result;
      end Populate_Internal;

   begin
      % if ctx.ple_unit_root:
         --  This is intended to be called on PLE unit roots only
         if Node.Kind /= ${ctx.ple_unit_root.ada_kind_name} then
            raise Program_Error;
         end if;
      % endif

      --  This function is meant to be called during an existing PLE pass. If
      --  if is called outside of this context, run the PLE pass on Node's
      --  analysis unit. Likewise, if PLE has not run on the unit that owns
      --  this PLE unit yet, do a full run, which will in the end trigger the
      --  PLE on this PLE unit.
      --
      --  We do this so that as soon as PLE is required on a PLE unit: the
      --  whole unit end up with its lexical environments populated.
      if not Context.In_Populate_Lexical_Env then
         begin
            Populate_Lexical_Env (Node.Unit);
            return False;
         exception
            when Property_Error =>
               return True;
         end;
      end if;

      % if ctx.ple_unit_root:
         --  Do nothing if its lexical envs have already been populated for
         --  this node.
         declare
            <%
               is_env_populated_field = (
                  ctx.ple_unit_root.get_abstract_node_data_dict()
                  [ctx.ple_unit_root.is_env_populated_indexing_name]
               ).name
            %>
            PLE_Unit_Root : constant ${ctx.ple_unit_root.name} :=
               ${ctx.ple_unit_root.internal_conversion(T.root_node, 'Node')};
         begin
            if PLE_Unit_Root.${is_env_populated_field} then
               return False;
            end if;
            PLE_Unit_Root.${is_env_populated_field} := True;
         end;

      % else:
         --  This is intended to be called on the root node only
         if Node.Parent /= null then
            raise Program_Error;
         end if;
      % endif

      return Populate_Internal (Node, Root_Env);
   end Populate_Lexical_Env;

   ------------------------------
   -- AST_Envs_Node_Text_Image --
   ------------------------------

   function AST_Envs_Node_Text_Image
     (Node  : ${T.root_node.name};
      Short : Boolean := True) return Text_Type is
   begin
      if Short then
         return To_Text (Basename (Node.Unit))
           & ":" & To_Text (Image (Start_Sloc (Sloc_Range (Node))));
      else
         return Short_Text_Image (Node);
      end if;
   end AST_Envs_Node_Text_Image;

   -------------------
   -- Is_Rebindable --
   -------------------

   function Is_Rebindable (Node : ${T.root_node.name}) return Boolean is
   begin
      <% rebindable_nodes = [n for n in ctx.astnode_types
                             if n.annotations.rebindable] %>
      % if not rebindable_nodes:
         return True;
      % else:
         return Node.Kind in ${ctx.astnode_kind_set(rebindable_nodes)};
      % endif
   end Is_Rebindable;

   ------------------------
   -- Register_Rebinding --
   ------------------------

   procedure Register_Rebinding
     (Node : ${T.root_node.name}; Rebinding : System.Address)
   is
      pragma Warnings (Off, "possible aliasing problem for type");
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Env_Rebindings);
      pragma Warnings (Off, "possible aliasing problem for type");
   begin
      Node.Unit.Rebindings.Append (Convert (Rebinding));
   end Register_Rebinding;

   --------------------
   -- Element_Parent --
   --------------------

   function Element_Parent
     (Node : ${T.root_node.name}) return ${T.root_node.name}
   is (Node.Parent);

   ----------
   -- Hash --
   ----------

   function Hash (Node : ${T.root_node.name}) return Hash_Type
   is
      function H is new Hash_Access
        (${T.root_node.value_type_name()}, ${T.root_node.name});
   begin
      return H (Node);
   end Hash;

   % if T.Bool.requires_hash_function:
      function Hash (B : Boolean) return Hash_Type is (Boolean'Pos (B));
   % endif

   % if T.Int.requires_hash_function:
      function Hash (I : Integer) return Hash_Type is (Hash_Type'Mod (I));
   % endif

   % if T.entity_info.requires_hash_function:
      function Hash (Info : ${T.entity_info.name}) return Hash_Type is
        (Combine (Hash (Info.MD), Hash (Info.Rebindings)));
   % endif

   ${struct_types.body_hash(T.entity)}

   --------------------------
   -- Big integers wrapper --
   --------------------------

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer
     (Image : String; Base : Integer := 10) return Big_Integer_Type
   is
      use GNATCOLL.GMP;
      use GNATCOLL.GMP.Integers;
   begin
      return new Big_Integer_Record'(Value     => Make (Image, Int (Base)),
                                     Ref_Count => 1);
   end Create_Big_Integer;

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer
     (Big_Int : GNATCOLL.GMP.Integers.Big_Integer) return Big_Integer_Type
   is
      Result : constant Big_Integer_Type :=
         new Big_Integer_Record'(Value     => <>,
                                 Ref_Count => 1);
   begin
      Result.Value.Set (Big_Int);
      return Result;
   end Create_Big_Integer;

   ------------------------
   -- Create_Big_Integer --
   ------------------------

   function Create_Big_Integer (Int : Integer) return Big_Integer_Type is
      Result : constant Big_Integer_Type :=
         new Big_Integer_Record'(Value     => <>,
                                 Ref_Count => 1);
   begin
      Result.Value.Set (GNATCOLL.GMP.Long (Int));
      return Result;
   end Create_Big_Integer;

   -------------------------------
   -- Create_Public_Big_Integer --
   -------------------------------

   function Create_Public_Big_Integer
     (Big_Int : Big_Integer_Type) return GNATCOLL.GMP.Integers.Big_Integer is
   begin
      return Result : GNATCOLL.GMP.Integers.Big_Integer do
         Result.Set (Big_Int.Value);
      end return;
   end Create_Public_Big_Integer;

   % if ctx.properties_logging:
   -----------------
   -- Trace_Image --
   -----------------

   function Trace_Image (I : Big_Integer_Type) return String is
   begin
      return GNATCOLL.GMP.Integers.Image (I.Value);
   end Trace_Image;
   % endif

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Big_Int : Big_Integer_Type) return Integer is
      Image : constant String := Big_Int.Value.Image;
   begin
      return Integer'Value (Image);
   exception
      when Constraint_Error =>
         raise Property_Error with "out of range big integer";
   end To_Integer;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Big_Int : Big_Integer_Type) is
   begin
      if Big_Int.Ref_Count /= -1 then
         Big_Int.Ref_Count := Big_Int.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Big_Int : in out Big_Integer_Type) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Big_Integer_Record, Big_Integer_Type);
   begin
      if Big_Int = null or else Big_Int.Ref_Count = -1 then
         return;
      end if;

      Big_Int.Ref_Count := Big_Int.Ref_Count - 1;
      if Big_Int.Ref_Count = 0 then
         Destroy (Big_Int);
      end if;
   end Dec_Ref;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value = Right.Value;
   end Equivalent;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value < Right.Value;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value <= Right.Value;
   end "<=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value > Right.Value;
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Big_Integer_Type) return Boolean is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Left.Value >= Right.Value;
   end ">=";

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Big_Integer_Type) return Big_Integer_Type is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Create_Big_Integer (Left.Value + Right.Value);
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (Left, Right : Big_Integer_Type) return Big_Integer_Type is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Create_Big_Integer (Left.Value - Right.Value);
   end "-";

   ------------------
   -- Unit_Version --
   ------------------

   function Unit_Version (Unit : Internal_Unit) return Version_Number is
   begin
      return Unit.Unit_Version;
   end Unit_Version;

   ---------------------
   -- Context_Version --
   ---------------------

   function Context_Version (Unit : Internal_Unit) return Integer is
   begin
      return Unit.Context.Reparse_Cache_Version;
   end Context_Version;
   ----------------------
   -- Short_Text_Image --
   ----------------------

   function Short_Text_Image (Self : ${T.root_node.name}) return Text_Type
   is
   begin
      if Self = null then
         return "None";
      end if;

      <%self:case_dispatch
         pred="${lambda n: n.annotations.custom_short_image}">
      <%def name="action(node)">
         return ${node.kwless_raw_name}_Short_Image
           (${node.internal_conversion(T.root_node, 'Self')});
      </%def>
      <%def name="default()">
         return "<" & To_Text (Kind_Name (Self))
                & " "
                & To_Text
                  (Ada.Directories.Simple_Name 
                     (Get_Filename (Unit (Self))))
                & ":" & To_Text (Image (Sloc_Range (Self))) & ">";
      </%def>
      </%self:case_dispatch>
   end Short_Text_Image;

   --------------------
   -- Snaps_At_Start --
   --------------------

   function Snaps_At_Start (Self : ${T.root_node.name}) return Boolean is
   begin
      <%self:case_dispatch pred="${lambda n: n.snaps_at_start}">
      <%def name="action(node)">
         return True;
      </%def>
      <%def name="default()">
         return False;
      </%def>
      </%self:case_dispatch>
   end Snaps_At_Start;

   ------------------
   -- Snaps_At_End --
   ------------------

   function Snaps_At_End (Self : ${T.root_node.name}) return Boolean is
   begin
      <%self:case_dispatch pred="${lambda n: n.snaps_at_end}">
      <%def name="action(node)">
         return True;
      </%def>
      <%def name="default()">
         return Is_Incomplete (Self);
      </%def>
      </%self:case_dispatch>
   end Snaps_At_End;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node         : ${T.root_node.name};
      Include_Self : Boolean := True)
      return ${root_node_array.name}
   is
      Count : Natural := 0;
      Start : ${T.root_node.name} :=
        (if Include_Self then Node else Node.Parent);
      Cur   : ${T.root_node.name} := Start;
   begin
      while Cur /= null loop
         Count := Count + 1;
         Cur := Cur.Parent;
      end loop;

      declare
         Result : constant ${root_node_array.name} :=
            ${root_node_array.constructor_name} (Count);
      begin
         Cur := Start;
         for I in Result.Items'Range loop
            Result.Items (I) := Cur;
            Cur := Cur.Parent;
         end loop;
         return Result;
      end;
   end Parents;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index (Node : ${T.root_node.name}) return Natural
   is (1);

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index (Node : ${T.root_node.name}) return Natural
   is (Children_Count (Node));

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node            : ${T.root_node.name};
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${T.root_node.name})
   is
      K : constant ${T.node_kind} := Node.Kind;
   begin
      <%
        root_type = ctx.root_grammar_class.name

        def get_actions(astnode, node_expr):
            specific_fields = astnode.get_parse_fields(
                lambda f: not f.abstract and not f.null,
                include_inherited=False
            )

            result = []

            # Emit only one processing code for all list types: no need to
            # repeat it multiple times.
            if astnode.is_generic_list_type:
                result.append("""
                    if Index > {node}.Count then
                        Index_In_Bounds := False;
                    else
                        Result := {node}.Nodes (Index);
                    end if;
                    return;
                """.format(node=node_expr))
            elif astnode.is_list:
                pass

            # Otherwise, emit code to handle regular fields, when there are
            # some.
            elif specific_fields:
                # Compute the index of the first AST node field we handle here
                all_fields = astnode.get_parse_fields(
                    lambda f: not f.abstract and not f.null,
                    include_inherited=True
                )
                first_field_index = len(all_fields) - len(specific_fields) + 1

                result.append('case Index is')
                for i, f in enumerate(specific_fields, first_field_index):
                    converted_node = T.root_node.internal_conversion(
                        f.type,
                        '{}.{}'.format(node_expr, f.name))
                    result.append("""
                        when {} =>
                            Result := {};
                            return;
                    """.format(i, converted_node))
                result.append("""
                        when others => null;
                    end case;
                """)
            return '\n'.join(result)
      %>

      Index_In_Bounds := True;
      Result := null;
      ${ctx.generate_actions_for_hierarchy('Node', 'K', get_actions)}

      --  Execution should reach this point iff nothing matched this index, so
      --  we must be out of bounds.
      Index_In_Bounds := False;
   end Get_Child;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : ${T.root_node.name};
      Show_Slocs  : Boolean;
      Line_Prefix : String := "")
   is
      K               : constant ${T.node_kind} := Node.Kind;
      Attr_Prefix     : constant String := Line_Prefix & "|";
      Children_Prefix : constant String := Line_Prefix & "|  ";

   begin
      Put (Line_Prefix & Kind_Name (Node));
      if Show_Slocs then
         Put ("[" & Image (Sloc_Range (Node)) & "]");
      end if;

      if Is_Incomplete (Node) then
         Put (" <<INCOMPLETE>>");
      end if;

      if Is_Token_Node (Node.Kind) then
         Put_Line (": " & Image (Text (Node)));

      % if ctx.generic_list_type.concrete_subclasses:
      elsif Node.Kind not in ${ctx.generic_list_type.ada_kind_range_name} then
         New_Line;
      % endif

      end if;

      % if ctx.generic_list_type.concrete_subclasses:
         --  List nodes are displayed in a special way (they have no field)
         if K in ${ctx.generic_list_type.ada_kind_range_name} then
            declare
               List : constant ${ctx.generic_list_type.name} :=
                  ${ctx.generic_list_type.internal_conversion(T.root_node,
                                                              'Node')};
            begin
               if List.Count = 0 then
                  Put_Line (": <empty list>");
                  return;
               end if;

               New_Line;
               for Child of List.Nodes (1 .. List.Count) loop
                  if Child /= null then
                     Print (Child, Show_Slocs, Line_Prefix & "|  ");
                  end if;
               end loop;
            end;
            return;
         end if;
      % endif

      % if ctx.sorted_parse_fields:
         --  This is for regular nodes: display each field
         declare
            use ${ada_lib_name}.Introspection_Implementation;
            Field_List : constant Field_Reference_Array := Fields (K);
         begin
            for I in Field_List'Range loop
               declare
                  Child : constant ${T.root_node.name} :=
                     Implementation.Child (Node, I);
               begin
                  Put (Attr_Prefix & Field_Name (Field_List (I)) & ":");
                  if Child /= null then
                     New_Line;
                     Print (Child, Show_Slocs, Children_Prefix);
                  else
                     Put_Line (" <null>");
                  end if;
               end;
            end loop;
         end;
      % endif
   end Print;

   ------------
   -- Parent --
   ------------

   function Parent (Node : ${T.root_node.name}) return ${T.root_node.name} is
   begin
      return Node.Parent;
   end Parent;

   ------------------
   -- Stored_Token --
   ------------------

   function Stored_Token
     (Node  : ${T.root_node.name};
      Token : Token_Reference) return Token_Index
   is
      Index : constant Token_Or_Trivia_Index := Get_Token_Index (Token);
   begin
      if Node.Unit.TDH'Access /= Get_Token_TDH (Token) then
         raise Property_Error with
           ("Cannot associate a token and a node from different analysis"
            & " units");
      elsif Index.Trivia /= No_Token_Index then
         raise Property_Error with
           ("A node cannot hold trivia");
      end if;

      return Index.Token;
   end Stored_Token;

   --------------------------
   -- Children_With_Trivia --
   --------------------------

   function Children_With_Trivia
     (Node : ${T.root_node.name}) return Bare_Children_Array
   is
      package Children_Vectors is new Ada.Containers.Vectors
        (Positive, Bare_Child_Record);
      use Children_Vectors;

      Ret_Vec : Vector;
      TDH     : Token_Data_Handler renames Node.Unit.TDH;

      procedure Append_Trivias (First, Last : Token_Index);
      --  Append all the trivias of tokens between indices First and Last to
      --  the returned vector.

      function Filter_Children
        (Parent : ${T.root_node.name})
         return ${root_node_array.array_type_name};
      --  Return an array for all children in Parent that are not null and that
      --  aren't ghost nodes.

      --------------------
      -- Append_Trivias --
      --------------------

      procedure Append_Trivias (First, Last : Token_Index) is
      begin
         for I in First .. Last loop
            for D of Get_Trivias (TDH, I) loop
               Ret_Vec.Append
                 ((Kind   => Trivia,
                   Trivia => Wrap_Token_Reference (TDH'Access, (I, D))));
            end loop;
         end loop;
      end Append_Trivias;

      ---------------------
      -- Filter_Children --
      ---------------------

      function Filter_Children
        (Parent : ${T.root_node.name})
         return ${root_node_array.array_type_name}
      is
         Children : constant ${root_node_array.array_type_name} :=
            Implementation.Children (Parent);
         Result   : ${root_node_array.array_type_name} (Children'Range);
         Next     : Integer := Result'First;
      begin
         for I in Children'Range loop
            --  Get rid of null nodes and of nodes with no real existence in
            --  the source code.
            if Children (I) /= null and then not Is_Ghost (Children (I)) then
               Result (Next) := Children (I);
               Next := Next + 1;
            end if;
         end loop;
         return Result (Result'First .. Next - 1);
      end Filter_Children;

      First_Child : constant Positive := 1;
      N_Children  : constant ${root_node_array.array_type_name} :=
         Filter_Children (Node);
   begin
      if N_Children'Length > 0
        and then (Node.Token_Start_Index
                    /= N_Children (First_Child).Token_Start_Index)
      then
         Append_Trivias (Node.Token_Start_Index,
                         N_Children (First_Child).Token_Start_Index - 1);
      end if;

      for I in N_Children'Range loop
         Ret_Vec.Append (Bare_Child_Record'(Child, N_Children (I)));
         Append_Trivias (N_Children (I).Token_End_Index,
                         (if I = N_Children'Last
                          then Node.Token_End_Index - 1
                          else N_Children (I + 1).Token_Start_Index - 1));
      end loop;

      declare
         A : Bare_Children_Array (1 .. Natural (Ret_Vec.Length));
      begin
         for I in A'Range loop
            A (I) := Ret_Vec.Element (I);
         end loop;
         return A;
      end;
   end Children_With_Trivia;

   --------------
   -- Is_Ghost --
   --------------

   function Is_Ghost (Node : ${T.root_node.name}) return Boolean
   is (Node.Token_End_Index = No_Token_Index);

   -------------------
   -- Is_Incomplete --
   -------------------

   function Is_Incomplete (Node : ${T.root_node.name}) return Boolean
   is
      LGC : ${T.root_node.name};
   begin
     if Is_List_Node (Node.Kind) then
        LGC := (if Last_Child_Index (Node) /= 0
                then Child (Node, Last_Child_Index (Node))
                else null);
        return LGC /= null and then Is_Incomplete (LGC);
      else
         return Node.Last_Attempted_Child > -1;
      end if;
   end;

   -----------------
   -- Token_Start --
   -----------------

   function Token_Start (Node : ${T.root_node.name}) return Token_Reference
   is (Token (Node, Node.Token_Start_Index));

   ---------------
   -- Token_End --
   ---------------

   function Token_End (Node : ${T.root_node.name}) return Token_Reference
   is
     (if Node.Token_End_Index = No_Token_Index
      then Token_Start (Node)
      else Token (Node, Node.Token_End_Index));

   -----------
   -- Token --
   -----------

   function Token
     (Node  : ${T.root_node.name};
      Index : Token_Index) return Token_Reference
   is
     (Wrap_Token_Reference (Token_Data (Node.Unit), (Index, No_Token_Index)));

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : ${T.root_node.name}) return Boolean
   is (Node = null);

   -----------------
   -- Child_Index --
   -----------------

   function Child_Index (Node : ${T.root_node.name}) return Integer
   is
      N : ${T.root_node.name} := null;
   begin
      if Node.Parent = null then
         raise Property_Error with
            "Trying to get the child index of a root node";
      end if;

      for I in First_Child_Index (Node.Parent)
            .. Last_Child_Index (Node.Parent)
      loop
         N := Child (Node.Parent, I);
         if N = Node then
            return I - 1;
         end if;
      end loop;

      --  If we reach this point, then Node isn't a Child of Node.Parent. This
      --  is not supposed to happen.
      raise Program_Error;
   end Child_Index;

   -------------------
   -- Fetch_Sibling --
   -------------------

   function Fetch_Sibling
     (Node   : ${T.root_node.name};
      E_Info : ${T.entity_info.name};
      Offset : Integer) return ${root_entity.name}
   is
      Node_Index : constant Positive := Child_Index (Node) + 1;
      --  Child_Index is 0-based, but the Child primitive expects a 1-based
      --  index.

      Sibling_Index : constant Integer := Node_Index + Offset;

      Sibling : constant ${T.root_node.name} :=
        (if Sibling_Index >= 1
         then Child (Node.Parent, Sibling_Index)
         else null);
      --  Child returns null for out-of-bound indexes
   begin
      --  Don't forget to clear entity info if the result is null
      return (if Sibling = null then No_Entity else (Sibling, E_Info));
   end Fetch_Sibling;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling
     (Node   : ${T.root_node.name};
      E_Info : ${T.entity_info.name} := ${T.entity_info.nullexpr})
      return ${root_entity.name} is
   begin
      return Fetch_Sibling (Node, E_Info, -1);
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling
     (Node   : ${T.root_node.name};
      E_Info : ${T.entity_info.name} := ${T.entity_info.nullexpr})
      return ${root_entity.name} is
   begin
      return Fetch_Sibling (Node, E_Info, 1);
   end Next_Sibling;

   ## Env metadata's body

   ${struct_types.body(T.env_md)}

   -------------
   -- Combine --
   -------------

   function Combine
     (L, R : ${T.env_md.name}) return ${T.env_md.name}
   is
      % if not T.env_md.get_fields():
      pragma Unreferenced (L, R);
      % endif
      Ret : ${T.env_md.name} := ${T.env_md.nullexpr};
   begin
      % for field in T.env_md.get_fields():
         % if field.type.is_bool_type:
         Ret.${field.name} := L.${field.name} or R.${field.name};
         % else:
         if L.${field.name} = ${field.type.nullexpr} then
            Ret.${field.name} := R.${field.name};
         elsif R.${field.name} = ${field.type.nullexpr} then
            Ret.${field.name} := L.${field.name};
         else
            raise Property_Error with "Wrong combine for metadata field";
         end if;
         % endif

      % endfor
      return Ret;
   end Combine;

   ---------
   -- Get --
   ---------

   function Get
     (A     : AST_Envs.Entity_Array;
      Index : Integer) return ${root_entity.name}
   is
      function Length (A : AST_Envs.Entity_Array) return Natural
      is (A'Length);

      function Get
        (A     : AST_Envs.Entity_Array;
         Index : Integer) return ${root_entity.name}
      is (A (Index + 1)); --  A is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Entity,
         Sequence_Type => AST_Envs.Entity_Array,
         Length        => Length,
         Get           => Get);
      Result : ${root_entity.name};
   begin
      if Relative_Get (A, Index, Result) then
         return Result;
      else
         raise Property_Error with "out-of-bounds array access";
      end if;
   end Get;

   -----------
   -- Group --
   -----------

   function Group
     (Envs   : ${T.LexicalEnv.array.name};
      Env_Md : ${T.env_md.name} := No_Metadata) return ${T.LexicalEnv.name}
   is (Group (AST_Envs.Lexical_Env_Array (Envs.Items), Env_Md));

   % for astnode in ctx.astnode_types:
       ${astnode_types.body_decl(astnode)}
   % endfor

   ------------------
   -- Children_Env --
   ------------------

   function Children_Env
     (Node   : ${T.root_node.name};
      E_Info : ${T.entity_info.name} := ${T.entity_info.nullexpr})
      return Lexical_Env
   is (Rebind_Env (Node.Self_Env, E_Info));

   --------------
   -- Node_Env --
   --------------

   function Node_Env
     (Node   : ${T.root_node.name};
      E_Info : ${T.entity_info.name} := ${T.entity_info.nullexpr})
      return Lexical_Env
   is
      function Get_Base_Env return Lexical_Env;
      --  Return the environment that we need to rebind before returning

      ------------------
      -- Get_Base_Env --
      ------------------

      function Get_Base_Env return Lexical_Env is
         function Get_Parent_Env return Lexical_Env;

         --------------------
         -- Get_Parent_Env --
         --------------------

         function Get_Parent_Env return Lexical_Env is
            Parent : constant Lexical_Env :=
               Get_Env (Node.Self_Env.Env.Parent, No_Entity_Info);
         begin
            --  If Node is the root scope or the empty environment, Parent can
            --  be a wrapper around the null node. Turn this into the
            --  Empty_Env, as null envs are erroneous values in properties.
            return (if Parent.Env = null
                    then Empty_Env
                    else Parent);
         end Get_Parent_Env;

      begin
         <% node_types = [n.ada_kind_name for n in ctx.astnode_types
                          if not n.abstract
                          and n.effective_env_spec
                          and n.effective_env_spec.adds_env] %>
         return
         % if node_types:
           (if Node.Kind in ${" | ".join(node_types)}
            then Get_Parent_Env
            else Node.Self_Env);
         % else:
           Node.Self_Env;
         % endif
      end Get_Base_Env;

      Base_Env : Lexical_Env := Get_Base_Env;
      Result   : constant Lexical_Env := Rebind_Env (Base_Env, E_Info);
   begin
      Dec_Ref (Base_Env);
      return Result;
   end Node_Env;

   ------------
   -- Parent --
   ------------

   function Parent
     (Node   : ${T.root_node.name};
      E_Info : ${T.entity_info.name} := ${T.entity_info.nullexpr})
      return ${root_entity.name} is
   begin
      --  TODO: shed entity information as appropriate
      return (Node.Parent, E_Info);
   end Parent;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node   : ${T.root_node.name};
      E_Info : ${T.entity_info.name} := ${T.entity_info.nullexpr})
      return ${root_entity.array.name}
   is
      Bare_Parents : ${root_node_array.name} := Parents (Node);
      Result       : ${root_entity.array.name} :=
         ${root_entity.array.constructor_name} (Bare_Parents.N);
   begin
      --  TODO: shed entity information as appropriate
      for I in Bare_Parents.Items'Range loop
         Result.Items (I) := (Bare_Parents.Items (I), E_Info);
      end loop;
      Dec_Ref (Bare_Parents);
      return Result;
   end Parents;

   --------------
   -- Children --
   --------------

   function Children
     (Node   : ${T.root_node.name};
      E_Info : ${T.entity_info.name} := ${T.entity_info.nullexpr})
      return ${root_entity.array.name}
   is
      Bare_Children : ${root_node_array.name} := Children (Node);
      Result        : ${root_entity.array.name} :=
         ${root_entity.array.constructor_name} (Bare_Children.N);
   begin
      --  TODO: shed entity information as appropriate
      for I in Bare_Children.Items'Range loop
         Result.Items (I) := (Bare_Children.Items (I), E_Info);
      end loop;
      Dec_Ref (Bare_Children);
      return Result;
   end Children;

   ## Generate the bodies of the root grammar class properties
   % for prop in T.root_node.get_properties(include_inherited=False):
   ${prop.prop_def}
   % endfor

   ## Generate the extensions for root node type

   <% ext = ctx.ext('nodes', T.root_node.raw_name, 'bodies') %>
   ${exts.include_extension(ext)}

   ## Generate bodies of untyped wrappers
   % for prop in T.root_node.get_properties( \
      include_inherited=False, \
      predicate=lambda f: f.requires_untyped_wrapper \
   ):
   ${prop.untyped_wrapper_def}
   % endfor

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : ${T.root_node.name}) is

      procedure Assign
        (Node  : ${T.root_node.name};
         LV    : in out Logic_Var_Record;
         Field : String);
      --  Assign a name to the LV logic variable. Node must be the node that
      --  owns LV, and Field must be the name of the field in Node that holds
      --  LV.

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Node  : ${T.root_node.name};
         LV    : in out Logic_Var_Record;
         Field : String) is
      begin
         LV.Dbg_Name := new String'
           (Image (Short_Text_Image (Node)) & "." & Field);
      end Assign;

      K : constant ${T.node_kind} := Node.Kind;

   begin
      <%
          def get_actions(astnode, node_expr):
              return '\n'.join(
                  'Assign ({root_node}, {field_node}.{field}, "{field}");'
                  .format(
                     root_node=T.root_node.internal_conversion(
                        astnode, node_expr),
                     field_node=field.struct.internal_conversion(
                        astnode, node_expr),
                     field=field.name
                  )
                  for field in astnode.get_user_fields(
                      include_inherited=False)
                  if field.type.is_logic_var_type
              )
      %>
      ${ctx.generate_actions_for_hierarchy('Node', 'K', get_actions)}
      for Child of ${root_node_array.array_type_name}'(Children (Node)) loop
         if Child /= null then
            Assign_Names_To_Logic_Vars (Child);
         end if;
      end loop;
   end Assign_Names_To_Logic_Vars;

   ----------------
   -- Text_Image --
   ----------------

   function Text_Image (Ent : ${T.entity.name}) return Text_Type is
   begin
      if Ent.Node /= null then
         declare
            Node_Image : constant Text_Type := Short_Text_Image (Ent.Node);
         begin
            return
            (if Ent.Info.Rebindings /= null
             then "<| "
             & Node_Image (Node_Image'First + 1 .. Node_Image'Last - 1) & " "
             & AST_Envs.Text_Image (Ent.Info.Rebindings) & " |>"
             else Node_Image);
         end;
      else
         return "None";
      end if;
   end Text_Image;

   -----------
   -- Image --
   -----------

   function Image (Ent : ${T.entity.name}) return String is
      Result : constant Text_Type := Text_Image (Ent);
   begin
      return Image (Result);
   end Image;

   ---------------
   -- Can_Reach --
   ---------------

   function Can_Reach (El, From : ${T.root_node.name}) return Boolean is
   begin
      --  Since this function is only used to implement sequential semantics in
      --  envs, we consider that elements coming from different units are
      --  always visible for each other, and let the user implement language
      --  specific visibility rules in the DSL.
      if El = null or else From = null or else El.Unit /= From.Unit then
         return True;
      end if;

       return Compare
         (Start_Sloc (Sloc_Range (El)),
          Start_Sloc (Sloc_Range (From))) = After;
   end Can_Reach;

   function ${root_entity.constructor_name}
     (Node : ${T.root_node.name};
      Info : ${T.entity_info.name}) return ${root_entity.name} is
   begin
      return (Node => Node, Info => Info);
   end;

   -----------------
   -- Hash_Entity --
   -----------------

   function Hash_Entity (Self : ${root_entity.name}) return Hash_Type is
   begin
      return Combine (Hash (Self.Node), Hash (Self.Info.Rebindings));
   end Hash_Entity;

   --------------------
   -- Compare_Entity --
   --------------------

   function Compare_Entity (Left, Right : ${root_entity.name}) return Boolean
   is
   begin
      return (Left.Node = Right.Node
              and then Left.Info.Rebindings = Right.Info.Rebindings);
   end Compare_Entity;

   procedure Destroy_Synthetic_Node (Node : in out ${T.root_node.name});
   --  Helper for the Register_Destroyable above

   ------------
   -- Length --
   ------------

   function Length (Node : ${ctx.generic_list_type.name}) return Natural
   is (Children_Count
         (${T.root_node.internal_conversion(ctx.generic_list_type, 'Node')}));

   % if ctx.properties_logging:

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (B : Boolean) return String is
      begin
         return (if B then "True" else "False");
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (I : Integer) return String is
      begin
         return Integer'Image (I);
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (S : Symbol_Type) return String is
      begin
         return (if S = null
                 then "None"
                 else Image (S.all, With_Quotes => True));
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Env : Lexical_Env) return String is
      begin
         if Env.Kind = Primary then
            return "<LexicalEnv for " & Trace_Image (Env.Env.Node) & ">";
         else
            return "<LexicalEnv synthetic>";
         end if;
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (E : ${root_entity.name}) return String is
      begin
         if E.Node = null then
            return "None";
         else
            return ("<|" & Trace_Image (E.Node, Decoration => False)
                    & " " & Trace_Image (E.Info) & "|>");
         end if;
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Info : ${T.entity_info.name}) return String is
      begin
         return ("(MD => " & Trace_Image (Info.MD)
                 & ", Rebindings => " & Trace_Image (Info.Rebindings));
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (R : Env_Rebindings) return String is
      begin
         return Image (Text_Image (R));
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Unit : Internal_Unit) return String is
      begin
         return "Internal_Unit (""" & Basename (Unit) & """)";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Eq : Logic_Equation) return String is
      begin
         return "<LogicEquation>";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Var : Logic_Var) return String is
      begin
         return "<LogicVariable>";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (K : Analysis_Unit_Kind) return String is
      begin
         return Analysis_Unit_Kind'Image (K);
      end Trace_Image;

   % endif

   % for struct_type in no_builtins(ctx.struct_types):
   ${struct_types.body(struct_type)}
   % endfor

   ${astnode_types.logic_helpers()}

   % for astnode in no_builtins(ctx.astnode_types):
     % if not astnode.is_list_type:
       ${astnode_types.body(astnode)}
     % endif
   % endfor

   % for astnode in ctx.astnode_types:
      % if astnode.is_root_list_type:
         ${list_types.body(astnode.element_type)}
      % elif astnode.is_list_type:
         ${astnode_types.body(astnode)}
      % endif
   % endfor

   ----------------------------
   -- Destroy_Synthetic_Node --
   ----------------------------

   procedure Destroy_Synthetic_Node (Node : in out ${T.root_node.name}) is
      procedure Free is new Ada.Unchecked_Deallocation
        (${T.root_node.value_type_name()}, ${T.root_node.name});
   begin
      --  Don't call Node.Destroy, as Node's children may be gone already: they
      --  have their own destructor and there is no specified order for the
      --  call of these destructors.
      Reset_Logic_Vars (Node);

      Free (Node);
   end Destroy_Synthetic_Node;

   -----------
   -- Image --
   -----------

   function Image (Value : Boolean) return String
   is (if Value then "True" else "False");

   % if ctx.properties_logging:
      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image
        (Node       : ${T.root_node.name};
         Decoration : Boolean := True) return String is
      begin
         if Node = null then
            return "None";
         else
            declare
               Result : constant String :=
                 (Kind_Name (Node) & " "
                  & Basename (Node.Unit) & ":"
                  & Image (Sloc_Range (Node)));
            begin
               return (if Decoration then "<" & Result & ">" else Result);
            end;
         end if;
      end Trace_Image;
   % endif

   Kind_Names : array (${T.node_kind}) of Unbounded_String :=
     (${", \n".join(cls.ada_kind_name
                    + " => To_Unbounded_String (\""
                    + cls.repr_name() + "\")"
                    for cls in ctx.astnode_types if not cls.abstract)});

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : ${T.root_node.name}) return String is
   begin
      return To_String (Kind_Names (Node.Kind));
   end Kind_Name;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Node : ${T.root_node.name}) return Natural is
      C : Integer := Kind_To_Node_Children_Count (Node.Kind);
   begin
      if C = -1 then
         return ${ctx.generic_list_type.internal_conversion(
                     T.root_node, 'Node')}.Count;
      else
         return C;
      end if;
   end Children_Count;

   ----------------------
   -- Reset_Logic_Vars --
   ----------------------

   procedure Reset_Logic_Vars (Node : ${T.root_node.name}) is

      procedure Reset (LV : in out Logic_Var_Record);
      --  Reset the LV logic variable, clearing the value it stores

      -----------
      -- Reset --
      -----------

      procedure Reset (LV : in out Logic_Var_Record) is
      begin
         --  TODO??? Fix Adalog so that Destroy resets the
         --  value it stores.
         LV.Value := No_Entity;
         Eq_Node.Refs.Reset (LV);
         Eq_Node.Refs.Destroy (LV);
      end Reset;

      K : constant ${T.node_kind} := Node.Kind;

   begin
      <%
          def get_actions(astnode, node_expr):
              return '\n'.join(
                  'Reset ({}.{});'.format(node_expr, field.name)
                  for field in astnode.get_user_fields(
                      include_inherited=False)
                  if field.type.is_logic_var_type
              )
      %>
      ${ctx.generate_actions_for_hierarchy('Node', 'K', get_actions)}
   end Reset_Logic_Vars;

   ----------------
   -- Token_Data --
   ----------------

   function Token_Data (Unit : Internal_Unit) return Token_Data_Handler_Access
   is (Unit.TDH'Access);

   -------------------
   -- Lookup_Symbol --
   -------------------

   function Lookup_Symbol
     (Context : Internal_Context; Symbol : Text_Type) return Symbol_Type
   is
      Canon_Symbol : constant Symbolization_Result :=
         % if ctx.symbol_canonicalizer:
            ${ctx.symbol_canonicalizer.fqn} (Symbol)
         % else:
            Create_Symbol (Symbol)
         % endif
      ;
   begin
      if Canon_Symbol.Success then
         return Find (Context.Symbols, Canon_Symbol.Symbol);
      else
         raise Invalid_Symbol_Error with Image (Symbol, With_Quotes => True);
      end if;
   end Lookup_Symbol;

   -------------------------
   -- Create_Special_Unit --
   -------------------------

   function Create_Special_Unit
     (Context             : Internal_Context;
      Normalized_Filename : Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit
   is
      Unit : Internal_Unit := new Analysis_Unit_Type'
        (Context           => Context,
         AST_Root          => null,
         Filename          => Normalized_Filename,
         Charset           => To_Unbounded_String (Charset),
         TDH               => <>,
         Diagnostics       => <>,
         Is_Env_Populated  => False,
         Rule              => Rule,
         AST_Mem_Pool      => No_Pool,
         Destroyables      => Destroyable_Vectors.Empty_Vector,
         Referenced_Units  => <>,
         Exiled_Entries    => Exiled_Entry_Vectors.Empty_Vector,
         Foreign_Nodes     =>
            Foreign_Node_Entry_Vectors.Empty_Vector,
         Rebindings        => Env_Rebindings_Vectors.Empty_Vector,
         Cache_Version     => <>,
         Unit_Version      => <>
         % if ctx.has_memoization:
         , Memoization_Map => <>
         % endif
      );
   begin
      Initialize (Unit.TDH, Context.Symbols);
      return Unit;
   end Create_Special_Unit;

   --------------------
   -- Templates_Unit --
   --------------------

   function Templates_Unit (Context : Internal_Context) return Internal_Unit is
   begin
      if Context.Templates_Unit = No_Analysis_Unit then
         Context.Templates_Unit := Create_Special_Unit
           (Context             => Context,
            Normalized_Filename => No_File,
            Charset             => Default_Charset,
            Rule                => ${ctx.main_rule_api_name});
      end if;
      return Context.Templates_Unit;
   end Templates_Unit;

   --------------
   -- Set_Rule --
   --------------

   procedure Set_Rule (Unit : Internal_Unit; Rule : Grammar_Rule) is
   begin
      Unit.Rule := Rule;
   end Set_Rule;

   ------------------------------
   -- Normalized_Unit_Filename --
   ------------------------------

   function Normalized_Unit_Filename
     (Context : Internal_Context; Filename : String) return Virtual_File
   is
      use Virtual_File_Maps;
      Key : constant Unbounded_String := To_Unbounded_String (Filename);
      Cur : Cursor := Context.Filenames.Find (Key);
   begin
      if Cur = No_Element then
         declare
            F : constant Virtual_File := Create
              (Create_From_Base (+Filename).Full_Name,
               Normalize => True);
         begin
            Context.Filenames.Insert (Key, F);
            return F;
         end;
      else
         return Element (Cur);
      end if;
   end Normalized_Unit_Filename;

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable_Helper
     (Unit    : Internal_Unit;
      Object  : System.Address;
      Destroy : Destroy_Procedure)
   is
   begin
      Destroyable_Vectors.Append (Unit.Destroyables, (Object, Destroy));
   end Register_Destroyable_Helper;

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable
     (Unit : Internal_Unit; Node : ${T.root_node.name})
   is
      procedure Helper is new Register_Destroyable_Gen
        (${T.root_node.value_type_name()},
         ${T.root_node.name},
         Destroy_Synthetic_Node);
   begin
      Helper (Unit, Node);
   end Register_Destroyable;

   -----------------------
   -- Invalidate_Caches --
   -----------------------

   procedure Invalidate_Caches
     (Context : Internal_Context; Invalidate_Envs : Boolean) is
   begin
      --  Increase Context's version number. If we are about to overflow, reset
      --  all version numbers from analysis units.
      if Context.Cache_Version = Natural'Last then
         Context.Cache_Version := 1;
         for Unit of Context.Units loop
            Unit.Cache_Version := 0;
         end loop;
      else
         Context.Cache_Version := Context.Cache_Version + 1;
      end if;

      if Invalidate_Envs then
         Context.Reparse_Cache_Version := Context.Cache_Version;
      end if;
   end Invalidate_Caches;

   ------------------
   --  Reset_Envs  --
   ------------------

   procedure Reset_Envs (Unit : Internal_Unit) is

      procedure Deactivate_Refd_Envs (Node : ${T.root_node.name});
      procedure Recompute_Refd_Envs (Node : ${T.root_node.name});

      --------------------------
      -- Deactivate_Refd_Envs --
      --------------------------

      procedure Deactivate_Refd_Envs (Node : ${T.root_node.name}) is
      begin
         if Node = null then
            return;
         end if;

         Deactivate_Referenced_Envs (Node.Self_Env);
         for I in 1 .. Children_Count (Node) loop
            Deactivate_Refd_Envs (Child (Node, I));
         end loop;
      end Deactivate_Refd_Envs;

      -------------------------
      -- Recompute_Refd_Envs --
      -------------------------

      procedure Recompute_Refd_Envs (Node : ${T.root_node.name}) is
      begin
         if Node = null then
            return;
         end if;
         Recompute_Referenced_Envs (Node.Self_Env);
         for I in 1 .. Children_Count (Node) loop
            Recompute_Refd_Envs (Child (Node, I));
         end loop;
      end Recompute_Refd_Envs;

   begin
      --  First pass will deactivate every referenced envs that Unit possesses
      Deactivate_Refd_Envs (Unit.AST_Root);

      --  Second pass will recompute the env they are pointing to
      Recompute_Refd_Envs (Unit.AST_Root);
   end Reset_Envs;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Reparsed : in out Reparsed_Unit) is
   begin
      Free (Reparsed.TDH);
      Reparsed.Diagnostics := Diagnostics_Vectors.Empty_Vector;
      Free (Reparsed.AST_Mem_Pool);
      Reparsed.AST_Root := null;
   end Destroy;

   --------------
   -- Basename --
   --------------

   function Basename (Filename : String) return String is
   begin
      return +Create (+Filename).Base_Name;
   end Basename;

   --------------
   -- Basename --
   --------------

   function Basename (Unit : Internal_Unit) return String is
   begin
      return +Unit.Filename.Base_Name;
   end Basename;

   ------------------
   -- Reset_Caches --
   ------------------

   procedure Reset_Caches (Unit : Internal_Unit) is
      Cache_Version : constant Natural := Unit.Cache_Version;
   begin
      if Cache_Version < Unit.Context.Reparse_Cache_Version then
         Unit.Cache_Version := Unit.Context.Reparse_Cache_Version;
         Reset_Envs (Unit);
      end if;

      if Cache_Version < Unit.Context.Cache_Version then
         Unit.Cache_Version := Unit.Context.Cache_Version;
         % if ctx.has_memoization:
            Destroy (Unit.Memoization_Map);
         % endif
      end if;
   end Reset_Caches;

   % if ctx.has_memoization:

      ----------------------------
      -- Lookup_Memoization_Map --
      ----------------------------

      function Lookup_Memoization_Map
        (Unit   : Internal_Unit;
         Key    : in out Mmz_Key;
         Cursor : out Memoization_Maps.Cursor) return Boolean
      is
         Inserted : Boolean;
         Value    : constant Mmz_Value := (Kind => Mmz_Evaluating);
      begin
         --  Make sure that we don't lookup stale caches
         Reset_Caches (Unit);

         Unit.Memoization_Map.Insert (Key, Value, Cursor, Inserted);

         if not Inserted then
            Destroy (Key.Items);
            Key := Memoization_Maps.Key (Cursor);
         end if;

         return Inserted;
      end Lookup_Memoization_Map;
   % endif

   --------------------
   -- Reference_Unit --
   --------------------

   procedure Reference_Unit (From, Referenced : Internal_Unit) is
      Dummy : Boolean;
   begin
      Dummy := Analysis_Unit_Sets.Add (From.Referenced_Units, Referenced);
   end Reference_Unit;

   ------------------------
   -- Is_Referenced_From --
   ------------------------

   function Is_Referenced_From
     (Self, Unit : Internal_Unit) return Boolean is
   begin
      if Unit = null or else Self = null then
         return False;
      elsif Unit = Self then
         return True;
      else
         return Analysis_Unit_Sets.Has (Unit.Referenced_Units, Self);
      end if;
   end Is_Referenced_From;

   ----------------
   -- Do_Parsing --
   ----------------

   procedure Do_Parsing
     (Unit   : Internal_Unit;
      Input  : Internal_Lexer_Input;
      Result : out Reparsed_Unit)
   is
      Context  : constant Internal_Context := Unit.Context;
      Unit_TDH : constant Token_Data_Handler_Access := Token_Data (Unit);

      Saved_TDH : Token_Data_Handler;
      --  Holder to save tokens data in Unit.
      --
      --  By design, parsing is required to bind the nodes it creates to an
      --  analysis unit. However, this procedure is supposed to preserve the
      --  Unit itself and return its parsing result in Result.
      --
      --  In order to implement this, we first move "old" token data in this
      --  variable, then we do parsing. Only then, we can move "new" token data
      --  from the unit to Result, and restore the "old" token data to Unit.
      --  This last step is what Rotate_TDH (see below) is above.

      procedure Rotate_TDH;
      --  Move token data from Unit to Result and restore data in Saved_TDH to
      --  Unit.

      procedure Add_Diagnostic (Message : String);
      --  Helper to add a sloc-less diagnostic to Unit

      ----------------
      -- Rotate_TDH --
      ----------------

      procedure Rotate_TDH is
      begin
         Move (Result.TDH, Unit_TDH.all);
         Move (Unit_TDH.all, Saved_TDH);
      end Rotate_TDH;

      --------------------
      -- Add_Diagnostic --
      --------------------

      procedure Add_Diagnostic (Message : String) is
      begin
         Append (Result.Diagnostics, No_Source_Location_Range,
                 To_Text (Message));
      end Add_Diagnostic;

   begin
      GNATCOLL.Traces.Trace (Main_Trace, "Parsing unit " & Basename (Unit));

      Result.AST_Root := null;

      Move (Saved_TDH, Unit_TDH.all);
      Initialize (Unit_TDH.all, Saved_TDH.Symbols);

      --  This is where lexing occurs, so this is where we get most "setup"
      --  issues: missing input file, bad charset, etc. If we have such an
      --  error, catch it, turn it into diagnostics and abort parsing.
      --
      --  As it is quite common, first check if the file is readable: if not,
      --  don't bother opening it and directly emit a diagnostic. This avoid
      --  pointless exceptions which harm debugging.

      if Input.Kind = File 
         and then 
         (Input.Filename.Is_Directory or else (not Input.Filename.Is_Readable))
      then
         declare
            Name : constant String := Basename (Unit);
         begin
            GNATCOLL.Traces.Trace
              (Main_Trace, "WARNING: File is not readable: " & Name);
            Add_Diagnostic ("Cannot read " & Name);
            Rotate_TDH;
            return;
         end;
      end if;

      declare
         use Ada.Exceptions;
      begin
         Init_Parser
           (Input, Context.Tab_Stop, Context.With_Trivia, Unit, Unit_TDH,
            Unit.Context.Parser);
      exception
         when Exc : Name_Error =>
            --  This happens when we cannot open the source file for lexing:
            --  return a unit anyway with diagnostics indicating what happens.

            GNATCOLL.Traces.Trace
              (Main_Trace,
               "WARNING: Could not open file " & Basename (Unit));

            Add_Diagnostic (Exception_Message (Exc));
            Rotate_TDH;
            return;

         when Unknown_Charset =>
            Add_Diagnostic
              ("Unknown charset """ & To_String (Unit.Charset) & """");
            Rotate_TDH;
            return;

         when Invalid_Input =>
            --  TODO??? Tell where (as a source location) we failed to decode
            --  the input.
            Add_Diagnostic
              ("Could not decode source as """ & To_String (Unit.Charset)
               & """");
            Rotate_TDH;
            return;
      end;

      --  We have correctly setup a parser! Now let's parse and return what we
      --  get.

      Result.AST_Mem_Pool := Create;
      Unit.Context.Parser.Mem_Pool := Result.AST_Mem_Pool;

      Result.AST_Root := ${T.root_node.name}
        (Parse (Unit.Context.Parser, Rule => Unit.Rule));
      Result.Diagnostics.Append (Unit.Context.Parser.Diagnostics);
      Rotate_TDH;
   end Do_Parsing;

   --------------------------
   -- Update_After_Reparse --
   --------------------------

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit) is
   begin
      --  Replace Unit's diagnostics by Reparsed's
      Unit.Diagnostics := Reparsed.Diagnostics;
      Reparsed.Diagnostics.Clear;

      --  As (re-)loading a unit can change how any AST node property in the
      --  whole analysis context behaves, we have to invalidate caches. This
      --  is likely overkill, but kill all caches here as it's easy to do.
      --
      --  As an optimization, invalidate referenced envs cache only if this is
      --  not the first time we parse Unit.
      Invalidate_Caches
        (Unit.Context, Invalidate_Envs => Unit.AST_Root /= null);

      --  Likewise for token data
      Free (Unit.TDH);
      Move (Unit.TDH, Reparsed.TDH);

      --  Reparsing will invalidate all lexical environments related to this
      --  unit, so destroy all related rebindings as well. This browses AST
      --  nodes, so we have to do this before destroying the old AST nodes
      --  pool.
      Destroy_Rebindings (Unit.Rebindings'Access);

      --  Destroy the old AST node and replace it by the new one
      if Unit.AST_Root /= null then
         Destroy (Unit.AST_Root);
      end if;
      Unit.AST_Root := Reparsed.AST_Root;

      --  Likewise for memory pools
      Free (Unit.AST_Mem_Pool);
      Unit.AST_Mem_Pool := Reparsed.AST_Mem_Pool;
      Reparsed.AST_Mem_Pool := No_Pool;

      --  Increment unit version number to invalidate caches and stale node
      --  reference.
      Unit.Unit_Version := Unit.Unit_Version + 1;

      --  If Unit had its lexical environments populated, re-populate them
      if not Unit.Is_Env_Populated then
         return;
      end if;

      declare
         Unit_Name     : constant String := +Unit.Filename.Base_Name;
         Context       : constant Internal_Context := Unit.Context;
         Foreign_Nodes : ${T.root_node.name}_Vectors.Vector :=
            ${T.root_node.name}_Vectors.Empty_Vector;

         Saved_In_Populate_Lexical_Env : constant Boolean :=
            Context.In_Populate_Lexical_Env;
      begin
         GNATCOLL.Traces.Trace
           (Main_Trace, "Updating lexical envs for " & Unit_Name
                        & " after reparse");
         GNATCOLL.Traces.Increase_Indent (Main_Trace);

         Context.In_Populate_Lexical_Env := True;

         --  Remove the `symbol -> AST node` associations in foreign lexical
         --  environments.
         Remove_Exiled_Entries (Unit);

         --  Collect all nodes that are foreign in this Unit's lexical envs.
         --  Exclude them from the corresponding lists of exiled entries.
         Extract_Foreign_Nodes (Unit, Foreign_Nodes);

         --  Reset the flag so that the call to Populate_Lexical_Env below does
         --  its work.
         Unit.Is_Env_Populated := False;

         --  Now that Unit has been reparsed, we can destroy all its
         --  destroyables, which refer to the old tree (i.e. dangling
         --  pointers).
         Destroy_Unit_Destroyables (Unit);

         for FN of Foreign_Nodes loop
            declare
               Node_Image : constant String := Image (Short_Text_Image (FN));
               Unit_Name  : constant String := +FN.Unit.Filename.Base_Name;
            begin
               GNATCOLL.Traces.Trace
                 (Main_Trace, "Rerooting: " & Node_Image
                              & " (from " & Unit_Name & ")");
            end;
            Reroot_Foreign_Node (FN);
         end loop;
         Foreign_Nodes.Destroy;

         Populate_Lexical_Env (Unit);
         Context.In_Populate_Lexical_Env := Saved_In_Populate_Lexical_Env;
         GNATCOLL.Traces.Decrease_Indent (Main_Trace);
      end;
   end Update_After_Reparse;

   -------------------------------
   -- Destroy_Unit_Destroyables --
   -------------------------------

   procedure Destroy_Unit_Destroyables (Unit : Internal_Unit) is
   begin
      for D of Unit.Destroyables loop
         D.Destroy (D.Object);
      end loop;
      Destroyable_Vectors.Clear (Unit.Destroyables);
   end Destroy_Unit_Destroyables;

   ---------------------------
   -- Remove_Exiled_Entries --
   ---------------------------

   procedure Remove_Exiled_Entries (Unit : Internal_Unit) is
   begin
      for EE of Unit.Exiled_Entries loop
         AST_Envs.Remove (EE.Env, EE.Key, EE.Node);

         --  Also strip foreign nodes information from "outer" units so that it
         --  does not contain stale information (i.e. dangling pointers to
         --  nodes that belong to the units in the queue).
         if EE.Env.Owner /= No_Analysis_Unit then
            declare
               Foreign_Nodes : Foreign_Node_Entry_Vectors.Vector renames
                  EE.Env.Env.Node.Unit.Foreign_Nodes;
               Current       : Positive := Foreign_Nodes.First_Index;
            begin
               while Current <= Foreign_Nodes.Last_Index loop
                  if Foreign_Nodes.Get (Current).Node = EE.Node then
                     Foreign_Nodes.Pop (Current);
                  else
                     Current := Current + 1;
                  end if;
               end loop;
            end;
         end if;
      end loop;

      Unit.Exiled_Entries.Clear;
   end Remove_Exiled_Entries;

   ---------------------------
   -- Extract_Foreign_Nodes --
   ---------------------------

   procedure Extract_Foreign_Nodes
     (Unit          : Internal_Unit;
      Foreign_Nodes : in out ${T.root_node.name}_Vectors.Vector) is
   begin
      for FN of Unit.Foreign_Nodes loop
         Foreign_Nodes.Append (FN.Node);

         declare
            Exiled_Entries : Exiled_Entry_Vectors.Vector renames
               FN.Unit.Exiled_Entries;
            Current        : Positive := Exiled_Entries.First_Index;
         begin
            while Current <= Exiled_Entries.Last_Index loop
               if Exiled_Entries.Get (Current).Node = FN.Node then
                  Exiled_Entries.Pop (Current);
               else
                  Current := Current + 1;
               end if;
            end loop;
         end;
      end loop;
      Unit.Foreign_Nodes.Clear;
   end Extract_Foreign_Nodes;

   --------------------------
   -- Reroot_Foreign_Nodes --
   --------------------------

   procedure Reroot_Foreign_Node (Node : ${T.root_node.name}) is
      Unit : constant Internal_Unit := Node.Unit;
   begin

      --  First, filter the exiled entries in foreign units so that they don't
      --  contain references to this unit's lexical environments.  We need to
      --  do that before running the partial Populate_Lexical_Env pass so that
      --  we don't remove exiled entries that this pass will produce.
      declare
         Exiled_Entries : Exiled_Entry_Vectors.Vector renames
            Unit.Exiled_Entries;
         Current        : Positive := Exiled_Entries.First_Index;
      begin
         while Current <= Exiled_Entries.Last_Index loop
            if Exiled_Entries.Get (Current).Node = Node then
               Exiled_Entries.Pop (Current);
            else
               Current := Current + 1;
            end if;
         end loop;
      end;

      --  Re-do a partial Populate_Lexical_Env pass for each foreign node that
      --  this unit contains so that they are relocated in our new lexical
      --  environments.
      declare
         Root_Scope : Lexical_Env renames Unit.Context.Root_Scope;
         Env        : constant Lexical_Env :=
            Pre_Env_Actions (Node, Node.Self_Env, Root_Scope, True);
      begin
         Post_Env_Actions (Node, Env, Root_Scope);
      end;
   end Reroot_Foreign_Node;

   ----------
   -- Text --
   ----------

   function Text (Node : ${T.root_node.name}) return ${T.String.name} is
      T      : constant Text_Type := Text (Node);
      Result : constant ${T.String.name} :=
         ${T.String.constructor_name} (T'Length);
   begin
      Result.Items := T;
      return Result;
   end Text;

   ------------------------
   -- Destroy_Rebindings --
   ------------------------

   procedure Destroy_Rebindings
     (Rebindings : access Env_Rebindings_Vectors.Vector)
   is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Env_Rebindings_Type, Env_Rebindings);

      procedure Recurse (R : Env_Rebindings);
      --  Destroy R's children and then destroy R. It is up to the caller to
      --  remove R from its parent's Children vector.

      procedure Unregister
        (R          : Env_Rebindings;
         Rebindings : in out Env_Rebindings_Vectors.Vector);
      --  Remove R from Rebindings

      -------------
      -- Recurse --
      -------------

      procedure Recurse (R : Env_Rebindings) is
      begin
         for C of R.Children loop
            Recurse (C);
         end loop;
         R.Children.Destroy;

         Unregister (R, R.Old_Env.Env.Node.Unit.Rebindings);
         Unregister (R, R.New_Env.Env.Node.Unit.Rebindings);

         declare
            Var_R : Env_Rebindings := R;
         begin
            Destroy (Var_R);
         end;
      end Recurse;

      ----------------
      -- Unregister --
      ----------------

      procedure Unregister
        (R          : Env_Rebindings;
         Rebindings : in out Env_Rebindings_Vectors.Vector) is
      begin
         for I in 1 .. Rebindings.Length loop
            if Rebindings.Get (I) = R then
               Rebindings.Pop (I);
               return;
            end if;
         end loop;

         --  We are always supposed to find R in Rebindings, so this should be
         --  unreachable.
         raise Program_Error;
      end Unregister;

   begin
      while Rebindings.Length > 0 loop
         declare
            R : constant Env_Rebindings := Rebindings.Get (1);
         begin
            --  Here, we basically undo what has been done in AST_Envs.Append

            --  If this rebinding has no parent, then during its creation we
            --  registered it in its Old_Env. Otherwise, it is registered
            --  in its Parent's Children list.
            if R.Parent = null then
               R.Old_Env.Env.Rebindings_Pool.Delete (R.New_Env);
            else
               Unregister (R, R.Parent.Children);
            end if;

            --  In all cases it's registered in Old_Env's and New_Env's units
            Recurse (R);
         end;
      end loop;
   end Destroy_Rebindings;

   --------------------------
   -- Get_Rewriting_Handle --
   --------------------------

   function Get_Rewriting_Handle
     (Context : Internal_Context) return Rewriting_Handle_Pointer is
   begin
      return Context.Rewriting_Handle;
   end Get_Rewriting_Handle;

   --------------------------
   -- Set_Rewriting_Handle --
   --------------------------

   procedure Set_Rewriting_Handle
     (Context : Internal_Context; Handle : Rewriting_Handle_Pointer) is
   begin
      Context.Rewriting_Handle := Handle;
   end Set_Rewriting_Handle;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Node_Safety_Net) is
   begin
      if Self.Context = null then
         return;
      end if;

      --  Check that Self's context has not been release (see the
      --  Context_Pool). Then check that the unit version is the same.
      if Self.Context.Released
         or else Self.Context.Serial_Number /= Self.Context_Serial
         or else Self.Unit.Unit_Version /= Self.Unit_Version
      then
         raise Stale_Reference_Error;
      end if;
   end Check_Safety_Net;

begin
   No_Big_Integer.Value.Set (0);
end ${ada_lib_name}.Implementation;
