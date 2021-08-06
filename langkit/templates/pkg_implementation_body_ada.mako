## vim: filetype=makoada

<%namespace name="array_types"    file="array_types_ada.mako" />
<%namespace name="iterator_types" file="iterator_types_ada.mako" />
<%namespace name="astnode_types"  file="astnode_types_ada.mako" />
<%namespace name="exts"           file="extensions.mako" />
<%namespace name="memoization"    file="memoization_ada.mako" />
<%namespace name="struct_types"   file="struct_types_ada.mako" />

<% root_node_array = T.root_node.array %>

with Ada.Containers;                  use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

pragma Warnings (Off, "internal");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "internal");

with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

with GNATCOLL.Traces;

with GNAT.Traceback.Symbolic;

with Langkit_Support.Hashes;  use Langkit_Support.Hashes;
with Langkit_Support.Images;  use Langkit_Support.Images;
with Langkit_Support.Relative_Get;

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

with ${ada_lib_name}.Private_Converters;
use ${ada_lib_name}.Private_Converters;
with ${ada_lib_name}.Introspection_Implementation;

pragma Warnings (Off, "referenced");
${exts.with_clauses(with_clauses + [
   ((ctx.symbol_canonicalizer.unit_fqn, False, False)
    if ctx.symbol_canonicalizer
       and not ctx.symbol_canonicalizer.unit_fqn.startswith("Langkit_Support.")
    else None),
   ((ctx.default_unit_provider.unit_fqn, False, False)
    if ctx.default_unit_provider else None)
])}
pragma Warnings (On, "referenced");

## Generate a dispatching case statement for the root node class. It will keep
## all the node classes which pass the predicate 'pred'. The caller needs to
## pass two def blocks, 'action', which takes the node kind as parameter, and
## emits the action for each matched node kind, and 'default', taking no
## parameter. and emitting the default action.
<%def name="case_dispatch(pred)">
   <%
   node_types = list(reversed([n for n in ctx.astnode_types if pred(n)]))
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

   use ${ada_lib_name}.Common.Precomputed_Symbols;

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
      --  initialize it, except for the Serial_Number field.

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

   procedure Register_Destroyable_Helper
     (Unit    : Internal_Unit;
      Object  : System.Address;
      Destroy : Destroy_Procedure);
   --  Common underlying implementation for Register_Destroyable_Gen

   pragma Warnings (Off, "referenced");
   function Construct_Entity_Array
     (V : AST_Envs.Entity_Vectors.Vector) return ${T.entity.array.name};
   pragma Warnings (On, "referenced");

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

   ----------------------------
   -- Construct_Entity_Array --
   ----------------------------

   function Construct_Entity_Array
     (V : AST_Envs.Entity_Vectors.Vector) return ${T.entity.array.name}
   is
      Ret : ${T.entity.array.name} :=
        ${T.entity.array.constructor_name} (V.Length);
   begin
      for J in V.First_Index .. V.Last_Index loop
         Ret.Items (J) := V.Get (J);
      end loop;

      declare
         Tmp : AST_Envs.Entity_Vectors.Vector := V;
      begin
         Tmp.Destroy;
      end;

      return Ret;
   end Construct_Entity_Array;

   ----------------
   -- Enter_Call --
   ----------------

   procedure Enter_Call
     (Context : Internal_Context; Call_Depth : access Natural)
   is
      Max             : Natural renames Context.Max_Call_Depth;
      Current         : Natural renames Context.Current_Call_Depth;
      High_Water_Mark : Natural renames Context.Call_Depth_High_Water_Mark;
   begin
      Current := Current + 1;
      High_Water_Mark := Natural'Max (High_Water_Mark, Current);
      Call_Depth.all := Current;
      if Current > Max then
         raise Property_Error with "stack overflow";
      end if;
   end Enter_Call;

   ---------------
   -- Exit_Call --
   ---------------

   procedure Exit_Call (Context : Internal_Context; Call_Depth : Natural) is
      Current : Natural renames Context.Current_Call_Depth;
   begin
      if Call_Depth /= Current then
         raise Unexpected_Call_Depth with
            "Langkit code generation bug for call depth handling detected";
      end if;
      Current := Current - 1;
   end Exit_Call;

   -----------
   -- Image --
   -----------

   function Image (Self : Symbol_Type) return ${T.String.name} is
      T      : constant Text_Type := Image (Self);
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
            Available.Delete_Last;
         end if;
      end Acquire;

      -------------
      -- Release --
      -------------

      procedure Release (Context : in out Internal_Context) is
      begin
         Available.Append (Context);
         Context.Serial_Number := Context.Serial_Number + 1;
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

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (File_Reader : in out Internal_File_Reader_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_File_Reader'Class, Internal_File_Reader_Access);
   begin
      if File_Reader /= null and then File_Reader.all.Dec_Ref then
         Destroy (File_Reader);
      end if;
   end Dec_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Internal_Event_Handler_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_Event_Handler'Class, Internal_Event_Handler_Access);
   begin
      if Self /= null and then Self.all.Dec_Ref then
         Destroy (Self);
      end if;
   end Dec_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Provider : in out Internal_Unit_Provider_Access) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (Internal_Unit_Provider'Class, Internal_Unit_Provider_Access);
   begin
      if Provider /= null and then Provider.all.Dec_Ref then
         Destroy (Provider);
      end if;
   end Dec_Ref;

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

   pragma Warnings (Off, "referenced");
   function To_Lookup_Kind_Type (K : Lookup_Kind) return Lookup_Kind_Type
   is
     (Lookup_Kind_Type'Val (Lookup_Kind'Pos (K)));
   pragma Warnings (On, "referenced");

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset        : String;
      File_Reader    : Internal_File_Reader_Access;
      Unit_Provider  : Internal_Unit_Provider_Access;
      Event_Handler  : Internal_Event_Handler_Access;
      With_Trivia    : Boolean;
      Tab_Stop       : Positive;
      Max_Call_Depth : Natural := ${ctx.default_max_call_depth})
      return Internal_Context
   is
      Actual_Charset : constant String :=
        (if Charset = "" then Default_Charset else Charset);
      Symbols        : constant Precomputed_Symbol_Table
        := Create_Symbol_Table;
      Context        : Internal_Context;
   begin
      Context_Pool.Acquire (Context);
      Context.Ref_Count := 1;
      Context.Symbols := Symbol_Table (Symbols);
      Context.Charset := To_Unbounded_String (Actual_Charset);
      Context.Tab_Stop := Tab_Stop;
      Context.With_Trivia := With_Trivia;
      Context.Root_Scope := Create_Static_Lexical_Env
        (Parent => AST_Envs.No_Env_Getter,
         Node   => null);

      --  Create a new ownership share for Event_Handler so that it lives at
      --  least as long as this analysis context.
      Context.Event_Handler := Event_Handler;
      if Context.Event_Handler /= null then
         Context.Event_Handler.Inc_Ref;
      end if;

      --  Create a new ownership share for File_Reader so that it lives at
      --  least as long as this analysis context.
      Context.File_Reader := File_Reader;
      if Context.File_Reader /= null then
         Context.File_Reader.Inc_Ref;
      end if;

      --  Create a new ownership share for Unit_Provider so that it lives at
      --  least as long as this analysis context.
      Context.Unit_Provider := Unit_Provider;
      if Context.Unit_Provider /= null then
         Context.Unit_Provider.Inc_Ref;
      end if;

      % if ctx.default_unit_provider:
         if Context.Unit_Provider = null then
            Context.Unit_Provider := ${ctx.default_unit_provider.fqn};
         end if;
      % endif

      Initialize (Context.Parser);

      Context.Discard_Errors_In_Populate_Lexical_Env := True;
      Context.Logic_Resolution_Timeout :=
        Langkit_Support.Adalog.Default_Timeout_Ticks_Number;
      Context.In_Populate_Lexical_Env := False;
      Context.Cache_Version := 0;
      Context.Reparse_Cache_Version := 0;

      Context.Rewriting_Handle := No_Rewriting_Handle_Pointer;
      Context.Templates_Unit := No_Analysis_Unit;

      Context.Max_Call_Depth := Max_Call_Depth;

      Context.Available_Rebindings := Env_Rebindings_Vectors.Empty_Vector;

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
      Rule              : Grammar_Rule;
      Is_Internal       : Boolean := False) return Internal_Unit
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
      --  Determine which encoding to use. Use the Charset parameter (if
      --  provided), otherwise use the context-wide default.

      Actual_Charset := (if Charset'Length /= 0
                         then To_Unbounded_String (Charset)
                         else Context.Charset);

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

      Unit :=
        (if Created
         then Create_Unit (Context, Normalized_Filename,
                           To_String (Actual_Charset), Rule)
         else Element (Cur));

      --  If an internal unit is requested, set the corresponding flag.
      --  Otherwise, make sure that the unit we return isn't internal.

      if Is_Internal then
         Unit.Is_Internal := True;
      end if;

      --  (Re)parse it if needed

      if Created or else Reparse then

         --  It is illegal to reparse an internal unit for public API users.
         --  Since public APIs do not allow to pass True to Is_Internal, we can
         --  check here that only the implementation can ask to reparse an
         --  internal unit.

         if Unit.Is_Internal and then not Is_Internal then
            raise Precondition_Failure with "cannot reparse an internal unit";
         end if;

         declare
            Reparsed : Reparsed_Unit;
         begin
            Do_Parsing (Unit, Refined_Input, Reparsed);
            Update_After_Reparse (Unit, Reparsed);
         end;

         --  Now that we have removed reparsed the unit, update its current
         --  charset.

         Unit.Charset := Actual_Charset;
      end if;

      if Context.Event_Handler /= null then
         Context.Event_Handler.Unit_Parsed_Callback (Context, Unit, Reparse);
      end if;

      return Unit;
   end Get_Unit;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean is
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
      if Reparse and then Has_Rewriting_Handle (Context) then
         raise Precondition_Failure with
            "cannot reparse during tree rewriting";
      end if;

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
      if Has_Rewriting_Handle (Context) then
         raise Precondition_Failure with
            "cannot parse from buffer during tree rewriting";

      elsif Context.File_Reader /= null then
         raise Precondition_Failure with
            "cannot parse from buffer with a file reader";
      end if;

      return Get_Unit (Context, Filename, Charset, True, Input, Rule);
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Internal_Context;
      Filename : String;
      Error    : Text_Type;
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
         begin
            Append (Unit.Diagnostics, No_Source_Location_Range, Error);
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
      if Reparse and then Has_Rewriting_Handle (Context) then
         raise Precondition_Failure with
            "cannot reparse during tree rewriting";
      end if;

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
      --  Destroy all named environment data structures
      for Desc of Context.Named_Envs loop
         for V of Desc.Foreign_Nodes loop
            V.Destroy;
         end loop;
         Destroy (Desc);
      end loop;
      Context.Named_Envs.Clear;

      --  If we are asked to free this context, it means that no one else have
      --  references to its analysis units, so it's safe to destroy these.
      for Unit of Context.Units loop
         Destroy (Unit);
      end loop;
      Context.Units := Units_Maps.Empty_Map;
      Context.Filenames := Virtual_File_Maps.Empty_Map;

      declare
         procedure Destroy is new Ada.Unchecked_Deallocation
           (Env_Rebindings_Type, Env_Rebindings);

         AR : Env_Rebindings_Vectors.Vector renames
            Context.Available_Rebindings;
         R  : Env_Rebindings;
      begin
         for I in AR.First_Index .. AR.Last_Index loop
            R := AR.Get (I);
            Destroy (R);
         end loop;
         AR.Destroy;
      end;

      Destroy (Context.Templates_Unit);
      AST_Envs.Destroy (Context.Root_Scope);
      Destroy (Context.Symbols);
      Destroy (Context.Parser);
      Dec_Ref (Context.File_Reader);
      Dec_Ref (Context.Unit_Provider);
      Dec_Ref (Context.Event_Handler);
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
      Has_Errors := Populate_Lexical_Env (Unit.AST_Root);
      Context.In_Populate_Lexical_Env := Saved_In_Populate_Lexical_Env;

      GNATCOLL.Traces.Decrease_Indent (Main_Trace);
      GNATCOLL.Traces.Trace
        (Main_Trace,
         "Finished populating lexical envs for unit: " & Basename (Unit));

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

      ----------------
      -- Get_Parent --
      ----------------

      function Get_Parent (Env : Lexical_Env) return Lexical_Env is
         E : constant Lexical_Env_Access := Unwrap (Env);
      begin
         return Get_Env (E.Parent, No_Entity_Info);
      end Get_Parent;

      --------------------------
      -- Explore_Parent_Chain --
      --------------------------

      procedure Explore_Parent_Chain (Env : Lexical_Env) is
         P : Lexical_Env;
      begin
         if Env /= Null_Lexical_Env then
            P := Get_Parent (Env);
            Dump_One_Lexical_Env
              (Env, Get_Env_Id (Env, State), Get_Env_Id (P, State));
            Explore_Parent_Chain (P);
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
            Parent := Get_Parent (Env);
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

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type
   is
   begin
      return Get_Line (Unit.TDH, Line_Number);
   end Get_Line;

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
      Unit.Exiled_Entries_In_NED.Destroy;
      Unit.Exiled_Envs.Destroy;
      Unit.Named_Envs.Destroy;
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
      ${exts.include_extension(ctx.ext('analysis', 'unit', 'destroy'))}
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

   % for array_type in ctx.array_types:
      ${array_types.body(array_type)}
   % endfor

   % for iterator_type in ctx.iterator_types:
      % if iterator_type.is_used:
         ${iterator_types.body(iterator_type)}
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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Env : in out Lexical_Env_Access) is
      Mutable_Env : Lexical_Env :=
        (Wrap (Env), 0, Env.Kind, No_Generic_Unit, 0);
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
      pragma Unreferenced (Kind);
      Self.Parent := Parent;
      Self.Unit := Unit;

      Self.Token_Start_Index := Token_Start_Index;
      Self.Token_End_Index := Token_End_Index;

      Self.Self_Env := Self_Env;
      Self.Last_Attempted_Child := -1;

      ${astnode_types.init_user_fields(T.root_node, 'Self')}
   end Initialize;

   --------------------
   -- Use_Direct_Env --
   --------------------

   procedure Use_Direct_Env (State : in out PLE_Node_State; Env : Lexical_Env)
   is
   begin
      State.Current_Env := Env;
      State.Current_NED := null;
   end Use_Direct_Env;

   -------------------
   -- Use_Named_Env --
   -------------------

   procedure Use_Named_Env
     (State   : in out PLE_Node_State;
      Context : Internal_Context;
      Name    : Symbol_Type) is
   begin
      State.Current_NED := Get_Named_Env_Descriptor (Context, Name);
      State.Current_Env := State.Current_NED.Env_With_Precedence;
   end Use_Named_Env;

   ---------------------
   -- Set_Initial_Env --
   ---------------------

   procedure Set_Initial_Env
     (Self     : ${T.root_node.name};
      State    : in out PLE_Node_State;
      Name     : Symbol_Type;
      Resolver : Lexical_Env_Resolver) is
   begin
      --  An empty name is the way for the expression to say to fallback on the
      --  direct initial environment computation.
      if Name /= null then
         Use_Named_Env (State, Self.Unit.Context, Name);

      else
         Use_Direct_Env
           (State,
            Resolver ((Node => Self, Info => ${T.entity_info.nullexpr})));
      end if;
   end Set_Initial_Env;

   ----------------
   -- Add_To_Env --
   ----------------

   procedure Add_To_Env
     (Self              : ${T.root_node.name};
      State             : PLE_Node_State;
      Key               : Symbol_Type;
      Value             : ${T.root_node.name};
      MD                : ${T.env_md.name};
      Resolver          : Entity_Resolver;
      Dest_Env_Name     : ${T.Symbol.name};
      Dest_Env_Fallback : Lexical_Env;
      DSL_Location      : String)
   is
      Context    : constant Internal_Context := Self.Unit.Context;
      Root_Scope : Lexical_Env renames Context.Root_Scope;
      --  Shortcuts

      Dest_Env : Lexical_Env;
      Dest_NED : Named_Env_Descriptor_Access;
      --  Description for the destination environment
   begin
      --  Sanitize the content to add to the destination environment: Key,
      --  Value and MD (Resolver is always correct).

      if Key = null or else Value = null then
         return;
      end if;

      if Value.Unit /= Self.Unit then
         raise Property_Error with "Cannot add_to_env an AST node that comes"
                                   & " from another analysis unit";
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

      --  Then determine the destination environment

      if Dest_Env_Name /= null then
         --  There is an environment name: just lookup the corresponding
         --  NED/env.
         Dest_NED := Get_Named_Env_Descriptor (Context, Dest_Env_Name);
         Dest_Env := Dest_NED.Env_With_Precedence;

      elsif Dest_Env_Fallback /= Empty_Env then
         --  There is an explicit destination environment
         Dest_NED := null;
         Dest_Env := Dest_Env_Fallback;

      else
         --  Just use the current environment
         Dest_NED := State.Current_NED;
         Dest_Env := State.Current_Env;
      end if;

      --  Sanitize it

      if Dest_Env.Kind /= Static_Primary then
         raise Property_Error with
            "Cannot add elements to a lexical env that is not static-primary";

      elsif
         --  Since lexical envs need to sort the foreign nodes they contain,
         --  and that the total order on nodes is not defined for synthetic
         --  nodes, it is not possible to add a synthetic node to a foreign
         --  lexical environment.
         --
         --  This reasoning applies to environments that belong to foreign
         --  units, but also to the root environment.
         Is_Foreign (Dest_Env, Self) and then Is_Synthetic (Value)
      then
         raise Property_Error with
            "Cannot add a synthetic node to a lexical env from another"
            & " analysis unit";

      elsif
         --  If requested, reject foreign destination environments.
         --
         --  Note that this checks only explicit destination environments
         --  (Dest_Env_Fallback): Set_Initial_Env already sanitized initial
         --  environments (State.Current_Env). Also note that Dest_Env_Fallback
         --  is Empty_Env (i.e. the fallback env expression is not evaluated)
         --  if we had a non-null env name (no need to fallback if we use a
         --  named environment).
         --
         --  This is an attempt at identifying uses of the unsound relocation
         --  mechanism (as opposed to named environments), so this applies to
         --  all foreign environments (root scope included).
         DSL_Location'Length > 0
         and then Is_Foreign_Strict (Dest_Env_Fallback, Self)
      then
         raise Property_Error with
            "unsound foreign environment in AddToEnv (" & DSL_Location & ")";
      end if;

      --  Now that everything is sanitized, we can proceed with the actual
      --  key/value pair addition. Note that this does nothing if Dest_Env
      --  ended up empty.
      Add (Dest_Env, Key, Value, MD, Resolver);

      --  If we're adding the element to an environment by env name, we must
      --  register this association in two places: in the target named env
      --  entry, and in Value's unit.
      if Dest_NED /= null then
         declare
            use NED_Assoc_Maps;

            FN    : Map renames Dest_NED.Foreign_Nodes;
            Dummy : Boolean;
            Cur   : Cursor;
         begin
            FN.Insert (Key      => Key,
                       New_Item => Internal_Map_Node_Vectors.Empty_Vector,
                       Position => Cur,
                       Inserted => Dummy);
            declare
               V : Internal_Map_Node_Vectors.Vector renames
                  FN.Reference (Cur);
            begin
               V.Append ((Value, MD, Resolver));
            end;
         end;
         Value.Unit.Exiled_Entries_In_NED.Append ((Dest_NED, Key, Value));

      --  Otherwise, if we're adding the element to an environment that belongs
      --  to a different unit, or to the root scope, then:
      elsif Dest_Env = Root_Scope or else Is_Foreign_Strict (Dest_Env, Self)
      then
         --  Add the environment, the key, and the value to the list of entries
         --  contained in other units, so we can remove them when reparsing
         --  Val's unit.
         Value.Unit.Exiled_Entries.Append ((Dest_Env, Key, Value));

         if Dest_Env /= Root_Scope then
            --  Add Val to the list of foreign nodes that Dest_Env's unit
            --  contains, so that when that unit is reparsed, we can call
            --  Add_To_Env again on those nodes.
            Convert_Unit (Dest_Env.Owner).Foreign_Nodes.Append
              ((Value, Self.Unit));
         end if;
      end if;
   end Add_To_Env;

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
      Shed_Rebindings     : Boolean) is
   begin
      for N of Ref_Env_Nodes.Items loop
         if N /= null then
            if N.Unit /= Self.Unit then
               raise Property_Error with
                  "attempt to add a referenced environment to a foreign unit";
            end if;
            Reference (Dest_Env, N, Resolver, Kind, Cats, Shed_Rebindings);
         end if;
      end loop;
      Dec_Ref (Ref_Env_Nodes);
   end Ref_Env;

   -------------
   -- Add_Env --
   -------------

   procedure Add_Env
     (Self              : ${T.root_node.name};
      State             : in out PLE_Node_State;
      No_Parent         : Boolean;
      Transitive_Parent : Boolean;
      Resolver          : Lexical_Env_Resolver;
      Names             : in out ${T.Symbol.array.name})
   is
      Parent_From_Name : constant Boolean := State.Current_NED /= null;
      --  Does the parent environment comes from a named environment lookup?

      Parent_Foreign : constant Boolean :=
         Is_Foreign_Strict (State.Current_Env, Self);

      --  Determine how to get the parent of this new environment:
      --
      --  (1) no parent if requested;
      --  (2) the current environment as the static parent if it comes from a
      --      named env lookup or if it is not foreign (or is the empty/root
      --      environment);
      --  (3) a dynamic parent in all other cases (the current environment is
      --      foreign and not fetched as a named environment.
      Parent_Getter : constant Env_Getter :=
        (if No_Parent
         then AST_Envs.No_Env_Getter

         elsif Parent_From_Name or else not Parent_Foreign
         then AST_Envs.Simple_Env_Getter (State.Current_Env)

         else AST_Envs.Dyn_Env_Getter (Resolver, Self));
   begin
      --  Create the environment itself
      Self.Self_Env := Create_Static_Lexical_Env
        (Parent            => Parent_Getter,
         Node              => Self,
         Transitive_Parent => Transitive_Parent);

      --  If the parent of this new environment comes from a named environment
      --  lookup, register this new environment so that its parent is updated
      --  when the precence for this named environment changes.
      if Parent_From_Name then
         declare
            NED : constant Named_Env_Descriptor_Access := State.Current_NED;
         begin
            Self.Unit.Exiled_Envs.Append ((NED, Self.Self_Env));
            NED.Foreign_Envs.Insert (Self, Self.Self_Env);
         end;
      end if;

      --  From now on, the current environment is Self.Self_Env, with a direct
      --  access to it. It does not go through the env naming scheme, since
      --  only this node and its children (i.e. non-foreign nodes) will access
      --  it as a "current" environment during PLE.
      Use_Direct_Env (State, Self.Self_Env);

      --  Register the environment we just created on all the requested names
      if Names /= null then
         declare
            Context   : constant Internal_Context := Self.Unit.Context;
            Env       : constant Lexical_Env := Self.Self_Env;
            NENU      : NED_Maps.Map renames
               State.Unit_State.Named_Envs_Needing_Update;
         begin
            for N of Names.Items loop
               Register_Named_Env (Context, N, Env, NENU);
            end loop;
            Dec_Ref (Names);
         end;
      end if;
   end Add_Env;

   ---------------------
   -- Pre_Env_Actions --
   ---------------------

   procedure Pre_Env_Actions
     (Self            : ${T.root_node.name};
      State           : in out PLE_Node_State;
      Add_To_Env_Only : Boolean := False) is
   begin

      <%self:case_dispatch pred="${lambda n: n.env_spec}">
      <%def name="action(n)">
         % if n.env_spec.pre_actions:
            ${n.raw_name}_Pre_Env_Actions (Self, State, Add_To_Env_Only);
         % else:
            null;
         % endif
      </%def>
      <%def name="default()"> null; </%def>
      </%self:case_dispatch>

   end Pre_Env_Actions;

   ----------------------
   -- Post_Env_Actions --
   ----------------------

   pragma Warnings (Off, "referenced");
   procedure Post_Env_Actions
     (Self : ${T.root_node.name}; State : in out PLE_Node_State)
   is
      pragma Warnings (On, "referenced");
   begin
      <%self:case_dispatch pred="${lambda n: n.env_spec}">
      <%def name="action(n)">
         % if n.env_spec.post_actions:
            ${n.raw_name}_Post_Env_Actions (Self, State);
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
      if Node = null then
         raise Property_Error with "cannot get the symbol of a null node";
      end if;
      return Get_Symbol (Token (Node, Node.Token_Start_Index));
   end Get_Symbol;

   ----------
   -- Text --
   ----------

   function Text
     (Node : ${T.root_node.name}) return Text_Type
   is
   begin
      if Node = null then
         raise Property_Error with "cannot get the text of a null node";
      end if;

      declare
         Start_T : constant Token_Reference :=
            Token (Node, Node.Token_Start_Index);
         End_T   : constant Token_Reference :=
            Token (Node, Node.Token_End_Index);
      begin
         --  No text is associated to synthetic and ghost nodes

         if Is_Synthetic (Node) then
            return "";
         end if;

         if Is_Ghost (Node) then
            return "";
         end if;

         return Text (Start_T, End_T);
      end;
   end Text;

   ---------------------
   -- Is_Visible_From --
   ---------------------

   function Is_Visible_From
     (Referenced_Env, Base_Env : Lexical_Env) return Boolean
   is
      Referenced_Unit : constant Internal_Unit :=
         Convert_Unit (Referenced_Env.Owner);
      Base_Unit       : constant Internal_Unit :=
         Convert_Unit (Base_Env.Owner);
   begin
      if Referenced_Unit = null then
         raise Property_Error with
            "referenced environment does not belong to any analysis unit";
      elsif Base_Unit = null then
         raise Property_Error with
            "base environment does not belong to any analysis unit";
      end if;
      return Is_Referenced_From (Referenced_Unit, Base_Unit);
   end Is_Visible_From;

   ----------
   -- Unit --
   ----------

   function Unit (Node : ${T.root_node.name}) return Internal_Unit is
   begin
      return Node.Unit;
   end Unit;

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

      Free_User_Fields (Node);
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
      return Result;
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
         then Sloc_Start (TDH, Get (T.Pos))
         else Sloc_End (TDH, Get (T.Pos)));

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

   ---------
   -- Get --
   ---------

   function Get
     (Node    : ${ctx.generic_list_type.name};
      Index   : Integer;
      Or_Null : Boolean := False) return ${T.root_node.name}
   is
      function Length (Node : ${ctx.generic_list_type.name}) return Natural
      is (Node.Count);
      --  Wrapper around the Length primitive to get the compiler happy for the
      --  the package instantiation below.

      function Absolute_Get
        (L     : ${ctx.generic_list_type.name};
         Index : Integer) return ${T.root_node.name}
      is (L.Nodes.all (Index + 1));
      --  L.Nodes is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => ${T.root_node.name},
         Sequence_Type => ${ctx.generic_list_type.name},
         Length        => Length,
         Get           => Absolute_Get);

      Result : ${T.root_node.name};
   begin
      if Node = null and then Or_Null then
         return null;
      elsif Relative_Get (Node, Index, Result) then
         return Result;
      elsif Or_Null then
         return null;
      else
         raise Property_Error with "out-of-bounds AST list access";
      end if;
   end Get;

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
      for C of Children_And_Trivia (Node) loop
         case C.Kind is
            when Trivia =>
               Put_Line (Children_Prefix & Image (Text (C.Trivia)));
            when Child =>
               PP_Trivia (C.Node, Children_Prefix);
         end case;
      end loop;
   end PP_Trivia;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   function Populate_Lexical_Env (Node : ${T.root_node.name}) return Boolean is

      Context    : constant Internal_Context := Node.Unit.Context;
      Unit_State : aliased PLE_Unit_State := (Named_Envs_Needing_Update => <>);
      Root_State : constant PLE_Node_State :=
        (Unit_State  => Unit_State'Unchecked_Access,
         Current_Env => Context.Root_Scope,
         Current_NED => null);

      function Populate_Internal
        (Node         : ${T.root_node.name};
         Parent_State : PLE_Node_State) return Boolean;
      --  Do the lexical env population on Node and recurse on its children

      procedure Register_Foreign_Env
        (Node : ${T.root_node.name}; State : PLE_Node_State);
      --  Given a node and its PLE state, register Node.Self_Env as being
      --  initialized through the named environment mechanism, if that's indeed
      --  the case. Do nothing otherwise.

      -----------------------
      -- Populate_Internal --
      -----------------------

      function Populate_Internal
        (Node         : ${T.root_node.name};
         Parent_State : PLE_Node_State) return Boolean
      is
         Result : Boolean := False;
         State  : PLE_Node_State := Parent_State;
      begin
         if Node = null then
            return Result;
         end if;

         --  By default (i.e. unless env actions add a new env), the
         --  environment we store in Node is the current one.
         Node.Self_Env := State.Current_Env;

         --  Run pre/post actions, and run PLE on children in between. Make
         --  sure we register the potential foreign Node.Self_Env environment
         --  at the end, even when an exception interrupts PLE to keep the
         --  state consistent.
         begin
            Pre_Env_Actions (Node, State);
            if State.Current_Env /= Null_Lexical_Env then
               Node.Self_Env := State.Current_Env;
               Register_Foreign_Env (Node, State);
            end if;

            --  Call recursively on children
            for I in First_Child_Index (Node) .. Last_Child_Index (Node) loop
               Result := Populate_Internal
                 (Child (Node, I), State) or else Result;
            end loop;

            Post_Env_Actions (Node, State);
         exception
            when Exc : Property_Error =>
               if PLE_Errors_Trace.Is_Active then
                   GNATCOLL.Traces.Trace
                     (PLE_Errors_Trace,
                      "Exception raised during PLE "
                      & Ada.Exceptions.Exception_Name (Exc) & " : "
                      & Ada.Exceptions.Exception_Message (Exc));
                   GNATCOLL.Traces.Trace
                     (PLE_Errors_Trace,
                      GNAT.Traceback.Symbolic.Symbolic_Traceback (Exc));
               end if;
               Register_Foreign_Env (Node, State);
               return True;
         end;

         return Result;
      end Populate_Internal;

      --------------------------
      -- Register_Foreign_Env --
      --------------------------

      procedure Register_Foreign_Env
        (Node : ${T.root_node.name}; State : PLE_Node_State) is
      begin
         if State.Current_NED /= null then
            State.Current_NED.Nodes_With_Foreign_Env.Insert (Node);
            Node.Unit.Nodes_With_Foreign_Env.Insert (Node, State.Current_NED);
         end if;
      end Register_Foreign_Env;

   begin
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

      --  This is intended to be called on the root node only
      if Node.Parent /= null then
         raise Program_Error;
      end if;

      return Result : constant Boolean :=
         Populate_Internal (Node, Root_State)
      do
         Update_Named_Envs (Unit_State.Named_Envs_Needing_Update);
      end return;
   end Populate_Lexical_Env;

   ------------------------------
   -- AST_Envs_Node_Text_Image --
   ------------------------------

   function AST_Envs_Node_Text_Image
     (Node  : ${T.root_node.name};
      Short : Boolean := True) return Text_Type is
   begin
      if Short then
         if Node = null then
            return "null";
         end if;
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
         pragma Unreferenced (Node);
         return True;
      % else:
         return Node.Kind in ${ctx.astnode_kind_set(rebindable_nodes)};
      % endif
   end Is_Rebindable;

   -----------------------
   -- Acquire_Rebinding --
   -----------------------

   function Acquire_Rebinding
     (Node             : ${T.root_node.name};
      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings
   is
      Result    : Env_Rebindings;
      Available : Env_Rebindings_Vectors.Vector renames
         Node.Unit.Context.Available_Rebindings;
   begin
      --  Use an existing and available Env_Rebindings_Type record for Node's
      --  Context, otherwise allocate a new rebinding.
      Result := (if Available.Is_Empty
                 then new Env_Rebindings_Type'(Version => 0, others => <>)
                 else Available.Pop);

      Result.Parent := Parent;
      Result.Old_Env := Old_Env;
      Result.New_Env := New_Env;
      Result.Children := Env_Rebindings_Vectors.Empty_Vector;
      return Result;
   end Acquire_Rebinding;

   -----------------------
   -- Release_Rebinding --
   -----------------------

   procedure Release_Rebinding (Self : in out Env_Rebindings) is
      Available : Env_Rebindings_Vectors.Vector renames
         Unwrap (Self.Old_Env).Node.Unit.Context.Available_Rebindings;
   begin
      --  Bumping the version number, to invalidate existing references to
      --  Self.
      Self.Version := Self.Version + 1;

      Self.Children.Destroy;
      Available.Append (Self);
      Self := null;
   end Release_Rebinding;

   ------------------------
   -- Register_Rebinding --
   ------------------------

   procedure Register_Rebinding
     (Node : ${T.root_node.name}; Rebinding : Env_Rebindings) is
   begin
      Node.Unit.Rebindings.Append (Rebinding);
   end Register_Rebinding;

   --------------------
   -- Element_Parent --
   --------------------

   function Element_Parent
     (Node : ${T.root_node.name}) return ${T.root_node.name}
   is (Node.Parent);

   ---------------
   -- Node_Unit --
   ---------------

   function Node_Unit (Node : ${T.root_node.name}) return Generic_Unit_Ptr is
   begin
      return Convert_Unit (Node.Unit);
   end Node_Unit;

   ----------
   -- Hash --
   ----------

   function Hash (Node : ${T.root_node.name}) return Hash_Type
   is
      function H is new Hash_Access
        (${T.root_node.value_type_name}, ${T.root_node.name});
   begin
      return H (Node);
   end Hash;

   % if T.Bool.requires_hash_function:
      function Hash (B : Boolean) return Hash_Type is (Boolean'Pos (B));
   % endif

   % if T.Int.requires_hash_function:
      function Hash (I : Integer) return Hash_Type is (Hash_Type'Mod (I));
   % endif

   % if T.Character.requires_hash_function:
      function Hash (I : Character_Type) return Hash_Type
      is (Hash_Type'Mod (Character_Type'Pos(I)));
   % endif

   ------------------------
   -- Named environments --
   ------------------------

   ---------
   -- Add --
   ---------

   procedure Add
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : AST_Envs.Internal_Map_Node)
   is
      use NED_Assoc_Maps;

      Pos   : Cursor;
      Dummy : Boolean;
   begin
      --  Make sure there is a vector entry for Key
      Self.Insert (Key, Internal_Map_Node_Vectors.Empty_Vector, Pos, Dummy);

      --  Append Node to that vector
      declare
         V : Internal_Map_Node_Vectors.Vector renames Self.Reference (Pos);
      begin
         V.Append (Node);
      end;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : ${T.root_node.name})
   is
      use NED_Assoc_Maps;

      V : Internal_Map_Node_Vectors.Vector renames Self.Reference (Key);
   begin
      --  Remove the (assumed unique) entry in V whose node is Node. The order
      --  of items in V is not significant, so we can use Pop for efficient
      --  removal. Do the traversal in reverse order for correctness.
      for I in reverse 1 .. V.Length loop
         if V.Get_Access (I).Node = Node then
            V.Pop (I);
            exit;
         end if;
      end loop;
   end Remove;

   ------------------------------
   -- Get_Named_Env_Descriptor --
   ------------------------------

   function Get_Named_Env_Descriptor
     (Context : Internal_Context;
      Name    : Symbol_Type) return Named_Env_Descriptor_Access
   is
      use NED_Maps;

      --  Look for an existing entry for Name
      Pos : constant Cursor := Context.Named_Envs.Find (Name);
   begin
      if Has_Element (Pos) then
         return Element (Pos);
      end if;

      --  There is no such entry: create one
      return Result : constant Named_Env_Descriptor_Access :=
         new Named_Env_Descriptor'
           (Name                   => Name,
            Envs                   => <>,
            Env_With_Precedence    => Empty_Env,
            Foreign_Nodes          => <>,
            Foreign_Envs           => <>,
            Nodes_With_Foreign_Env => <>)
      do
         Context.Named_Envs.Insert (Name, Result);
      end return;
   end Get_Named_Env_Descriptor;

   ------------------------
   -- Register_Named_Env --
   ------------------------

   procedure Register_Named_Env
     (Context                   : Internal_Context;
      Name                      : Symbol_Type;
      Env                       : Lexical_Env;
      Named_Envs_Needing_Update : in out NED_Maps.Map)
   is
      NED_Access : constant Named_Env_Descriptor_Access :=
         Get_Named_Env_Descriptor (Context, Name);
      NED        : Named_Env_Descriptor renames NED_Access.all;
      Node       : constant ${T.root_node.name} := Env_Node (Env);
   begin
      NED.Envs.Insert (Node, Env);
      Node.Unit.Named_Envs.Append ((Name, Env));

      --  If that insertion must change the env that has precedence, signal
      --  that NED requires an update.

      if NED.Envs.First_Element /= NED.Env_With_Precedence then
         Named_Envs_Needing_Update.Include (Name, NED_Access);
      end if;
   end Register_Named_Env;

   ----------------------
   -- Update_Named_Env --
   ----------------------

   procedure Update_Named_Envs (Named_Envs : NED_Maps.Map) is
   begin
      for Cur in Named_Envs.Iterate loop
         declare
            NE      : Named_Env_Descriptor renames NED_Maps.Element (Cur).all;
            New_Env : constant Lexical_Env :=
              (if NE.Envs.Is_Empty
               then Empty_Env
               else NE.Envs.First_Element);
         begin
            --  If there was an environment with precedence, remove its foreign
            --  nodes.
            if NE.Env_With_Precedence /= Empty_Env then
               for Cur in NE.Foreign_Nodes.Iterate loop
                  declare
                     Key   : constant ${T.Symbol.name} :=
                        NED_Assoc_Maps.Key (Cur);
                     Nodes : Internal_Map_Node_Vectors.Vector renames
                        NE.Foreign_Nodes.Reference (Cur);
                  begin
                     for N of Nodes loop
                        Remove (NE.Env_With_Precedence, Key, N.Node);
                     end loop;
                  end;
               end loop;
            end if;

            --  Now, set the new environment that has precedence
            NE.Env_With_Precedence := New_Env;

            --  Add the foreign nodes to the new environment with precedence,
            --  if any.
            for Cur in NE.Foreign_Nodes.Iterate loop
               declare
                  Key   : constant ${T.Symbol.name} :=
                     NED_Assoc_Maps.Key (Cur);
                  Nodes : Internal_Map_Node_Vectors.Vector renames
                     NE.Foreign_Nodes.Reference (Cur);
               begin
                  for N of Nodes loop
                     Add (New_Env, Key, N.Node, N.MD, N.Resolver);
                  end loop;
               end;
            end loop;

            --  Set the parent environment of all foreign environments
            for Cur in NE.Foreign_Envs.Iterate loop
               declare
                  Env : Lexical_Env_Record renames
                     Unwrap (Sorted_Env_Maps.Element (Cur)).all;
               begin
                  Env.Parent := Simple_Env_Getter (New_Env);
               end;
            end loop;

            --  Update nodes whose environment was the old env with precedence
            for N of NE.Nodes_With_Foreign_Env loop
               N.Self_Env := New_Env;
            end loop;
         end;
      end loop;
   end Update_Named_Envs;

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

   function Unit_Version (Unit : Generic_Unit_Ptr) return Version_Number is
   begin
      return Convert_Unit (Unit).Unit_Version;
   end Unit_Version;

   -------------------------
   -- Get_Context_Version --
   -------------------------

   function Get_Context_Version
     (Node : ${T.root_node.name}) return Version_Number is
   begin
      return Node.Unit.Context.Cache_Version;
   end Get_Context_Version;

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
         return ${node.kwless_raw_name}_Short_Image (Self);
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
     (Node      : ${T.root_node.name};
      With_Self : Boolean := True)
      return ${root_node_array.name}
   is
      Count : Natural := 0;
      Start : ${T.root_node.name} :=
        (if With_Self then Node else Node.Parent);
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
                    result.append("""
                        when {} =>
                            Result := {}.{};
                            return;
                    """.format(i, node_expr, f.name))
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
            if Node.Count = 0 then
               Put_Line (": <empty list>");
               return;
            end if;

            New_Line;
            for Child of Node.Nodes (1 .. Node.Count) loop
               if Child /= null then
                  Print (Child, Show_Slocs, Line_Prefix & "|  ");
               end if;
            end loop;
            return;
         end if;
      % endif

      % if ctx.sorted_parse_fields:
         --  This is for regular nodes: display each field
         declare
            use ${ada_lib_name}.Introspection_Implementation;
            Field_List : constant Syntax_Field_Reference_Array :=
               Syntax_Fields (K);
         begin
            for I in Field_List'Range loop
               declare
                  Child : constant ${T.root_node.name} :=
                     Implementation.Child (Node, I);
               begin
                  Put
                    (Attr_Prefix
                     & Image (Syntax_Field_Name (Field_List (I)))
                     & ":");
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

   -------------------------
   -- Children_And_Trivia --
   -------------------------

   function Children_And_Trivia
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
      --  Return an array for all children in Parent that are not null

      --------------------
      -- Append_Trivias --
      --------------------

      procedure Append_Trivias (First, Last : Token_Index) is
      begin
         for I in First .. Last loop
            for D of Get_Trivias (TDH, I) loop
               Ret_Vec.Append
                 (Bare_Child_Record'
                    (Kind   => Trivia,
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
            if Children (I) /= null then
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

      --  Append each node to Ret_Vec, and append trivia that follow after each
      --  non-ghost nodes.
      for I in N_Children'Range loop
         Ret_Vec.Append (Bare_Child_Record'(Child, N_Children (I)));
         if not Is_Ghost (N_Children (I)) then
            Append_Trivias (N_Children (I).Token_End_Index,
                            (if I = N_Children'Last
                             then Node.Token_End_Index - 1
                             else N_Children (I + 1).Token_Start_Index - 1));
         end if;
      end loop;

      declare
         A : Bare_Children_Array (1 .. Natural (Ret_Vec.Length));
      begin
         for I in A'Range loop
            A (I) := Ret_Vec.Element (I);
         end loop;
         return A;
      end;
   end Children_And_Trivia;

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

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : ${T.root_node.name}) return Boolean is
   begin
      --  Reject invalid inputs
      if Left /= null and Is_Synthetic (Left) then
         raise Property_Error with "left node is synthetic";
      elsif Right /= null and Is_Synthetic (Right) then
         raise Property_Error with "right node is synthetic";
      end if;

      --  Null nodes come first
      if Left = null then
         return Right /= null;
      elsif Right = null then
         return False;
      end if;

      --  So we have two non-null nodes. Sort by unit filename
      if Left.Unit < Right.Unit then
         return True;
      elsif Left.Unit /= Right.Unit then
         return False;
      end if;

      --  Both nodes come from the same unit: compare their token indexes
      if Left.Token_Start_Index < Right.Token_Start_Index then
         return True;
      elsif Left.Token_Start_Index > Right.Token_Start_Index then
         return False;
      else
         return Left.Token_End_Index < Right.Token_End_Index;
      end if;
   end "<";

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : ${T.root_node.name}) return Boolean
   is (Node = null);

   ----------
   -- Kind --
   ----------

   function Kind (Node : ${T.root_node.name}) return ${T.node_kind}
   is (Node.Kind);

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
      Offset : Integer) return ${T.root_node.name} is
   begin
      --  Root nodes have no sibling: handle them now to avoid invalid requests
      --  in the code below.
      if Node.Parent = null then
         return null;
      end if;

      declare
         Node_Index : constant Positive := Child_Index (Node) + 1;
         --  Child_Index is 0-based, but the Child primitive expects a 1-based
         --  index.

         Sibling_Index : constant Integer := Node_Index + Offset;
      begin
         --  Child returns null for out-of-bound indexes

         return (if Sibling_Index >= 1
                 then Child (Node.Parent, Sibling_Index)
                 else null);
      end;
   end Fetch_Sibling;

   -------------------
   -- Fetch_Sibling --
   -------------------

   function Fetch_Sibling
     (Node   : ${T.root_node.name};
      E_Info : ${T.entity_info.name};
      Offset : Integer) return ${root_entity.name}
   is
      Sibling : constant ${T.root_node.name} := Fetch_Sibling (Node, Offset);
   begin
      --  Don't forget to clear entity info if the result is nul

      return (if Sibling = null
              then No_Entity
              else (Sibling, E_Info));
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

   -------------------------------
   -- Create_Static_Lexical_Env --
   -------------------------------

   function Create_Static_Lexical_Env
     (Parent            : Env_Getter;
      Node              : ${T.root_node.name};
      Transitive_Parent : Boolean := False) return Lexical_Env
   is
      Unit : constant Internal_Unit :=
        (if Node = null then null else Node.Unit);
   begin
      return Result : Lexical_Env := Create_Lexical_Env
        (Parent, Node, Transitive_Parent, Convert_Unit (Unit))
      do
         if Unit /= null then
            Register_Destroyable (Unit, Unwrap (Result.Env));
         end if;
      end return;
   end Create_Static_Lexical_Env;

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
   is (Group (Lexical_Env_Array (Envs.Items), Env_Md));

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
         pragma Warnings (Off, "referenced");
         function Get_Parent_Env return Lexical_Env;
         pragma Warnings (On, "referenced");

         --------------------
         -- Get_Parent_Env --
         --------------------

         function Get_Parent_Env return Lexical_Env is
            Parent : constant Lexical_Env := AST_Envs.Parent (Node.Self_Env);
         begin
            --  If Node is the root scope or the empty environment, Parent can
            --  be a wrapper around the null node. Turn this into the
            --  Empty_Env, as null envs are erroneous values in properties.
            return (if Unwrap (Parent) = null
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
     (Node      : ${T.root_node.name};
      With_Self : Boolean := True;
      E_Info    : ${T.entity_info.name} := ${T.entity_info.nullexpr})
      return ${root_entity.array.name}
   is
      Bare_Parents : ${root_node_array.name} := Parents (Node, With_Self);
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

   ---------------------
   -- New_Unit_String --
   ---------------------

   function New_Unit_String
     (Unit : Internal_Unit; Str : String) return String_Access
   is
      procedure Register_Destroyable_String is new Register_Destroyable_Gen
        (String, String_Access, Free);
   begin
      return Ret : String_Access := new String'(Str) do
         Register_Destroyable_String (Unit, Ret);
      end return;
   end New_Unit_String;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : ${T.root_node.name}) is

      pragma Warnings (Off, "referenced");

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
         LV.Dbg_Name :=
           New_Unit_String
             (Node.Unit, Image (Short_Text_Image (Node)) & "." & Field);
      end Assign;

      K : constant ${T.node_kind} := Node.Kind;

      pragma Warnings (On, "referenced");

   begin
      <%
          def get_actions(astnode, node_expr):
              return '\n'.join(
                  'Assign ({node}, {node}.{field}, "{field}");'
                  .format(node=node_expr, field=field.name)
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

   ---------------------
   -- Full_Sloc_Image --
   ---------------------

   function Full_Sloc_Image (Node : ${T.root_node.name}) return ${T.String.name}
   is
      Res      : constant Text_Type :=
        To_Text
          (Ada.Directories.Simple_Name
             (Get_Filename (Unit (Node))))
           & ":" & To_Text (Image (Start_Sloc (Sloc_Range (Node)))) & ": ";
      Result : constant ${T.String.name} :=
         ${T.String.constructor_name} (Res'Length);
   begin
      Result.Items := Res;
      return Result;
   end Full_Sloc_Image;

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

   --------------------------------
   -- Create_Dynamic_Lexical_Env --
   --------------------------------

   function Create_Dynamic_Lexical_Env
     (Self              : ${T.root_node.name};
      Assocs_Getter     : Inner_Env_Assocs_Resolver;
      Assoc_Resolver    : Entity_Resolver;
      Transitive_Parent : Boolean) return Lexical_Env
   is
      Unit : constant Internal_Unit := Self.Unit;
   begin
      --  This restriction is necessary to avoid relocation issues when
      --  Self.Self_Env is terminated.
      if Is_Foreign_Strict (Self.Self_Env, Self) then
         raise Property_Error with
           ("cannot create a dynamic lexical env when Self.Self_Env is"
            & " foreign");
      end if;

      return Result : constant Lexical_Env := Create_Dynamic_Lexical_Env
        (Parent            => No_Env_Getter,
         Node              => Self,
         Transitive_Parent => Transitive_Parent,
         Owner             => Convert_Unit (Unit),
         Assocs_Getter     => Assocs_Getter,
         Assoc_Resolver    => Assoc_Resolver)
      do
         --  Since dynamic lexical environments can only be created in lazy
         --  field initializers, it is fine to tie Result's lifetime to the
         --  its owning unit's lifetime.
         Register_Destroyable (Unit, Unwrap (Result));
      end return;
   end Create_Dynamic_Lexical_Env;

   procedure Destroy_Synthetic_Node (Node : in out ${T.root_node.name});
   --  Helper for the Register_Destroyable above

   ------------
   -- Length --
   ------------

   function Length (Node : ${ctx.generic_list_type.name}) return Natural
   is (if Node = null then 0 else Children_Count (Node));

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
         case Env.Kind is
         when Static_Primary =>
            return "<LexicalEnv static-primary for "
                   & Trace_Image (Env_Node (Env)) & ">";
         when others =>
            return "<LexicalEnv synthetic>";
         end case;
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
         pragma Unreferenced (Eq);
      begin
         return "<LogicEquation>";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Var : Logic_Var) return String is
         pragma Unreferenced (Var);
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

   % for struct_type in ctx.struct_types:
   ${struct_types.body(struct_type)}
   % endfor

   ${astnode_types.logic_helpers()}

   % for astnode in ctx.astnode_types:
      ${astnode_types.body(astnode)}
   % endfor

   ----------------------------
   -- Destroy_Synthetic_Node --
   ----------------------------

   procedure Destroy_Synthetic_Node (Node : in out ${T.root_node.name}) is
      procedure Free is new Ada.Unchecked_Deallocation
        (${T.root_node.value_type_name}, ${T.root_node.name});
   begin
      --  Don't call Node.Destroy, as Node's children may be gone already: they
      --  have their own destructor and there is no specified order for the
      --  call of these destructors.
      Free_User_Fields (Node);
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
         return Node.Count;
      else
         return C;
      end if;
   end Children_Count;

   ----------------------
   -- Free_User_Fields --
   ----------------------

   procedure Free_User_Fields (Node : ${T.root_node.name}) is

      procedure Reset_Logic_Var (LV : in out Logic_Var_Record);
      --  Reset the LV logic variable, clearing the value it stores

      ---------------------
      -- Reset_Logic_Var --
      ---------------------

      procedure Reset_Logic_Var (LV : in out Logic_Var_Record) is
      begin
         LV.Value := No_Entity;
         Eq_Node.Refs.Reset (LV);
         Eq_Node.Refs.Destroy (LV);
      end Reset_Logic_Var;

      K : constant ${T.node_kind} := Node.Kind;

   begin
      <%
          def get_actions(astnode, node_expr):
              result = []
              for field in astnode.get_user_fields(include_inherited=False):
                  if field.type.is_refcounted:
                      free_subp = 'Dec_Ref'
                  elif field.type.is_logic_var_type:
                      free_subp = 'Reset_Logic_Var'
                  else:
                      free_subp = None

                  if free_subp:
                      result.append(f'{free_subp} ({node_expr}.{field.name});')

              return '\n'.join(result)
      %>
      ${ctx.generate_actions_for_hierarchy('Node', 'K', get_actions)}
   end Free_User_Fields;

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
         return Get_Symbol
           (Context.Symbols, Find (Context.Symbols, Canon_Symbol.Symbol));
      else
         raise Invalid_Symbol_Error with Image (Canon_Symbol.Error_Message);
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
        (Context                      => Context,
         Is_Internal                  => False,
         AST_Root                     => null,
         Filename                     => Normalized_Filename,
         Charset                      => To_Unbounded_String (Charset),
         TDH                          => <>,
         Diagnostics                  => <>,
         Is_Env_Populated             => False,
         Rule                         => Rule,
         AST_Mem_Pool                 => No_Pool,
         Destroyables                 => Destroyable_Vectors.Empty_Vector,
         Referenced_Units             => <>,
         Exiled_Entries               => Exiled_Entry_Vectors.Empty_Vector,
         Foreign_Nodes                =>
            Foreign_Node_Entry_Vectors.Empty_Vector,
         Exiled_Entries_In_NED        =>
            Exiled_Entry_In_NED_Vectors.Empty_Vector,
         Exiled_Envs                  => Exiled_Env_Vectors.Empty_Vector,
         Named_Envs                   => Named_Env_Vectors.Empty_Vector,
         Nodes_With_Foreign_Env       => <>,
         Rebindings                   => Env_Rebindings_Vectors.Empty_Vector,
         Cache_Version                => <>,
         Unit_Version                 => <>,
         % if ctx.has_memoization:
         Memoization_Map            => <>,
         % endif
         others => <>
      );
   begin
      Initialize (Unit.TDH, Context.Symbols,
                  Context.Tab_Stop);
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
        (${T.root_node.value_type_name},
         ${T.root_node.name},
         Destroy_Synthetic_Node);
   begin
      Helper (Unit, Node);
   end Register_Destroyable;

   --------------------------
   -- Register_Destroyable --
   --------------------------

   procedure Register_Destroyable
     (Unit : Internal_Unit; Env : AST_Envs.Lexical_Env_Access)
   is
      procedure Helper is new Register_Destroyable_Gen
        (AST_Envs.Lexical_Env_Record, AST_Envs.Lexical_Env_Access, Destroy);
   begin
      Helper (Unit, Env);
   end Register_Destroyable;

   -----------------------
   -- Invalidate_Caches --
   -----------------------

   procedure Invalidate_Caches
     (Context : Internal_Context; Invalidate_Envs : Boolean) is
   begin
      --  Increase Context's version number. If we are about to overflow, reset
      --  all version numbers from analysis units.
      if Context.Cache_Version = Version_Number'Last then
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
      Cache_Version : constant Version_Number := Unit.Cache_Version;
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

      ----------------
      -- Rotate_TDH --
      ----------------

      procedure Rotate_TDH is
      begin
         Move (Result.TDH, Unit_TDH.all);
         Move (Unit_TDH.all, Saved_TDH);
      end Rotate_TDH;

   begin
      GNATCOLL.Traces.Trace (Main_Trace, "Parsing unit " & Basename (Unit));

      Result.AST_Root := null;

      Move (Saved_TDH, Unit_TDH.all);
      Initialize (Unit_TDH.all, Saved_TDH.Symbols,
                  Unit.Context.Tab_Stop);

      --  This is where lexing occurs, so this is where we get most "setup"
      --  issues: missing input file, bad charset, etc. If we have such an
      --  error, catch it, turn it into diagnostics and abort parsing.
      --
      --  As it is quite common, first check if the file is readable: if not,
      --  don't bother opening it and directly emit a diagnostic. This avoid
      --  pointless exceptions which harm debugging. Note that this
      --  optimization is valid only when there is no file reader, which can
      --  work even when there is no real source file.

      if Context.File_Reader = null
         and then Input.Kind = File
         and then (Input.Filename.Is_Directory
                   or else (not Input.Filename.Is_Readable))
      then
         declare
            Name : constant String := Basename (Unit);
         begin
            GNATCOLL.Traces.Trace
              (Main_Trace, "WARNING: File is not readable: " & Name);
            Append
              (Result.Diagnostics,
               No_Source_Location_Range,
               "Cannot read " & To_Text (Name));
            Rotate_TDH;
            return;
         end;
      end if;

      --  Initialize the parser, which fetches the source buffer and extract
      --  all tokens.

      Init_Parser
        (Input, Context.With_Trivia, Unit, Unit_TDH, Unit.Context.Parser);

      --  If we could run the lexer, run the parser and get the root node

      if Unit_TDH.Source_Buffer /= null then
         Result.AST_Mem_Pool := Create;
         Unit.Context.Parser.Mem_Pool := Result.AST_Mem_Pool;
         Result.AST_Root := ${T.root_node.name}
           (Parse (Unit.Context.Parser, Rule => Unit.Rule));
      end if;

      --  Forward token data and diagnostics to the returned unit

      Rotate_TDH;

      --  TODO we use a for loop rt. than the new ``Append_Vector`` here for
      --  compatibility with old compilers, but someday we'll be able to get
      --  rid of it.
      for Diag of Unit.Context.Parser.Diagnostics loop
         Result.Diagnostics.Append (Diag);
      end loop;
   end Do_Parsing;

   --------------------------
   -- Update_After_Reparse --
   --------------------------

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit) is
   begin
      --  Remove the `symbol -> AST node` associations for Unit's nodes in
      --  foreign lexical environments.
      Remove_Exiled_Entries (Unit);

      --  Remove the named envs that Unit created
      declare
         Named_Envs_Needing_Update : NED_Maps.Map;
      begin
         Remove_Named_Envs (Unit, Named_Envs_Needing_Update);
         Update_Named_Envs (Named_Envs_Needing_Update);
      end;

      --  At this point, envs and nodes that don't belong to this unit no
      --  longer reference this unit's envs and nodes. It is thus now safe to
      --  deallocate this unit's obsolete data.

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
         if EE.Env.Owner /= No_Generic_Unit then
            declare
               Foreign_Nodes : Foreign_Node_Entry_Vectors.Vector renames
                  Convert_Unit (EE.Env.Owner).Foreign_Nodes;
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

   -----------------------
   -- Remove_Named_Envs --
   -----------------------

   procedure Remove_Named_Envs
     (Unit                      : Internal_Unit;
      Named_Envs_Needing_Update : in out NED_Maps.Map) is
   begin
      --  Remove nodes in this unit from the Named_Env_Descriptor.Foreign_Nodes
      --  components in which they are registered and from the foreign
      --  environments themselves.
      for EE of Unit.Exiled_Entries_In_NED loop
         Remove (EE.Named_Env.Foreign_Nodes, EE.Key, EE.Node);
         Remove (EE.Named_Env.Env_With_Precedence, EE.Key, EE.Node);
      end loop;
      Unit.Exiled_Entries_In_NED.Clear;

      --  Remove nodes in this unit from the
      --  Named_Env_Descriptor.Nodes_With_Foreign_Env components in which they
      --  are registered.
      for Cur in Unit.Nodes_With_Foreign_Env.Iterate loop
         declare
            use Node_To_Named_Env_Maps;
            Node : constant ${T.root_node.name} := Key (Cur);
            NE   : constant Named_Env_Descriptor_Access := Element (Cur);
         begin
            NE.Nodes_With_Foreign_Env.Delete (Node);
         end;
      end loop;
      Unit.Nodes_With_Foreign_Env.Clear;

      --  Remove ends in this unit from the Named_Env_Descriptor.Foreign_Envs
      --  components in which they are registered.
      for EE of Unit.Exiled_Envs loop
         EE.Named_Env.Foreign_Envs.Delete (Env_Node (EE.Env));
      end loop;
      Unit.Exiled_Envs.Clear;

      --  Remove named envs that this unit created
      for NE of Unit.Named_Envs loop
         declare
            NED_Access : constant Named_Env_Descriptor_Access :=
               Unit.Context.Named_Envs.Element (NE.Name);
            NED        : Named_Env_Descriptor renames NED_Access.all;
         begin
            NED.Envs.Delete (Env_Node (NE.Env));

            --  If this named environment had precedence, we must schedule an
            --  update for this name environment entry.
            if NE.Env = NED.Env_With_Precedence then
               Named_Envs_Needing_Update.Include (NE.Name, NED_Access);
               NED.Env_With_Precedence := Empty_Env;
            end if;
         end;
      end loop;
      Unit.Named_Envs.Clear;
   end Remove_Named_Envs;

   ---------------------------
   -- Extract_Foreign_Nodes --
   ---------------------------

   procedure Extract_Foreign_Nodes
     (Unit          : Internal_Unit;
      Foreign_Nodes : in out ${T.root_node.name}_Vectors.Vector) is
   begin
      --  Go through all foreign nodes registered in Unit's lexical
      --  environments.
      for FN of Unit.Foreign_Nodes loop
         --  Collect them
         Foreign_Nodes.Append (FN.Node);

         --  For each foreign node, remove the corresponding exiled entry in
         --  that foreign unit (each foreign node in unit A has a corresponding
         --  exiled entry in unit B).
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
         Unit_State : aliased PLE_Unit_State :=
           (Named_Envs_Needing_Update => <>);
         State      : PLE_Node_State :=
           (Unit_State  => Unit_State'Unchecked_Access,
            Current_Env => Node.Self_Env,
            Current_NED => null);
      begin
         Pre_Env_Actions (Node, State, Add_To_Env_Only => True);
         Post_Env_Actions (Node, State);
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
      procedure Recurse (R : in out Env_Rebindings);
      --  Destroy R's children and then destroy R. It is up to the caller to
      --  remove R from its parent's Children vector.

      procedure Unregister
        (R          : Env_Rebindings;
         Rebindings : in out Env_Rebindings_Vectors.Vector);
      --  Remove R from Rebindings

      -------------
      -- Recurse --
      -------------

      procedure Recurse (R : in out Env_Rebindings) is
      begin
         for C of R.Children loop
            declare
               C_Var : Env_Rebindings := C;
            begin
               Recurse (C_Var);
            end;
         end loop;
         R.Children.Destroy;

         Unregister (R, Convert_Unit (R.Old_Env.Owner).Rebindings);
         Unregister (R, Convert_Unit (R.New_Env.Owner).Rebindings);

         Release_Rebinding (R);
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
            R : Env_Rebindings := Rebindings.Get (1);
         begin
            --  Here, we basically undo what has been done in AST_Envs.Append

            --  If this rebinding has no parent, then during its creation we
            --  registered it in its Old_Env. Otherwise, it is registered
            --  in its Parent's Children list.
            if R.Parent = null then
               Unwrap (R.Old_Env).Rebindings_Pool.Delete (R.New_Env);
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

   -----------------------
   -- Create_Safety_Net --
   -----------------------

   function Create_Safety_Net
     (Context : Internal_Context) return Iterator_Safety_Net
   is
   begin
      return (Context         => Context,
              Context_Serial  => Context.Serial_Number,
              Context_Version => Context.Cache_Version);
   end Create_Safety_Net;

   ----------------------
   -- Check_Safety_Net --
   ----------------------

   procedure Check_Safety_Net (Self : Iterator_Safety_Net) is
   begin
      if Self.Context = null then
         return;
      end if;

      --  Check that the context is still the same (not released nor reused)
      if Self.Context.Serial_Number /= Self.Context_Serial
         or else Self.Context.Cache_Version /= Self.Context_Version
      then
         raise Stale_Reference_Error;
      end if;
   end Check_Safety_Net;

   ----------------------
   -- String_To_Symbol --
   ----------------------

   function String_To_Symbol
     (Context : Internal_Context; S : ${T.String.name}) return Symbol_Type is
   begin
      return (if S.N > 0
              then Lookup_Symbol (Context, S.Items)
              else null);
   exception
      when Exc : Invalid_Symbol_Error =>
         raise Property_Error with Ada.Exceptions.Exception_Message (Exc);
   end String_To_Symbol;

begin
   No_Big_Integer.Value.Set (0);
end ${ada_lib_name}.Implementation;
