## vim: filetype=makoada

<%namespace name="array_types"    file="array_types_ada.mako" />
<%namespace name="iterator_types" file="iterator_types_ada.mako" />
<%namespace name="set_types"      file="set_types_ada.mako" />
<%namespace name="astnode_types"  file="astnode_types_ada.mako" />
<%namespace name="exts"           file="extensions.mako" />
<%namespace name="memoization"    file="memoization_ada.mako" />
<%namespace name="struct_types"   file="struct_types_ada.mako" />

<%
   root_node_array = T.root_node.array
   cache_collection = cfg.library.cache_collection
%>

with Ada.Containers;                  use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

pragma Warnings (Off, "internal");
with Ada.Strings.Wide_Wide_Unbounded.Aux;
use Ada.Strings.Wide_Wide_Unbounded.Aux;
pragma Warnings (On, "internal");

with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;
with System.Memory;

% if T.String.requires_hash_function:
with GNAT.String_Hash;
% endif
with GNAT.Task_Lock;
with GNAT.Traceback.Symbolic;

with GNATCOLL.Traces;

with Langkit_Support.Adalog.Debug;
with Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Hashes; use Langkit_Support.Hashes;
with Langkit_Support.Images; use Langkit_Support.Images;
with Langkit_Support.Names;  use Langkit_Support.Names;
with Langkit_Support.Relative_Get;

with ${ada_lib_name}.Private_Converters;
use ${ada_lib_name}.Private_Converters;

pragma Warnings (Off, "referenced");
${exts.with_clauses(with_clauses)}
pragma Warnings (On, "referenced");

## Generate a dispatching case statement for the root node class. It will keep
## all the node classes which pass the predicate 'pred'. The caller needs to
## pass two def blocks, 'action', which takes the node kind as parameter, and
## emits the action for each matched node kind, and 'default', taking no
## parameter. and emitting the default action.
<%def name="case_dispatch(pred)">
   <%
   node_types = list(reversed([n for n in ctx.node_types if pred(n)]))
   concrete_types, _ = ctx.collapse_concrete_nodes(
       ctx.root_node_type, node_types
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

   pragma Extensions_Allowed (On);

   use Precomputed_Symbols;

   pragma Warnings (Off, "has no effect");
   use Solver;
   pragma Warnings (On, "has no effect");

   package Context_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Internal_Context);

   type Contexts_Destructor is limited
      new Ada.Finalization.Limited_Controlled with null record;
   overriding procedure Finalize (CD : in out Contexts_Destructor);
   --  Helper to destroy all contexts when terminating the process

   package Context_Pool is

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

   procedure Reset_Envs_Caches (Unit : Internal_Unit);
   --  Reset the env caches of all lexical environments created for ``Unit``

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

   procedure Print
     (Node        : Langkit_Support.Generic_API.Analysis.Lk_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "");
   --  Helper for the public overload, but working on the generic API node type

   ------------------------
   -- Precomputed_Symbol --
   ------------------------

   pragma Warnings (Off, "referenced");
   function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type is
   pragma Warnings (On, "referenced");
   begin
      % if ctx.symbol_literals:
         declare
            Raw_Text : constant Text_Type := (case Index is
            <%
               sym_items = ctx.sorted_symbol_literals
               last_i = len(sym_items) - 1
            %>
            % for i, (sym, name) in enumerate(sym_items):
               when ${name} => ${ascii_repr(sym)}${',' if i < last_i else ''}
            % endfor
            );

            Symbol : constant Symbolization_Result :=
               % if ctx.symbol_canonicalizer:
                  ${ctx.symbol_canonicalizer.fqn} (Raw_Text)
               % else:
                  Create_Symbol (Raw_Text)
               % endif
            ;
         begin
            if Symbol.Success then
               return Symbol.Symbol;
            else
               raise Program_Error with
                 "Cannot canonicalize symbol literal: " & Image (Raw_Text);
            end if;
         end;
      % else:
         return (raise Program_Error);
      % endif
   end Precomputed_Symbol;

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

   -----------
   -- Image --
   -----------

   function Image (Self : Symbol_Type) return ${T.String.name} is
   begin
      return Create_String (Image (Self));
   end Image;

   ------------------
   -- Context_Pool --
   ------------------

   package body Context_Pool is

      -------------
      -- Acquire --
      -------------

      procedure Acquire (Context : out Internal_Context) is
      begin
         GNAT.Task_Lock.Lock;

         if Available.Is_Empty then
            Context := new Analysis_Context_Type;
            Context.Serial_Number := 1;
         else
            Context := Available.Last_Element;
            Available.Delete_Last;
         end if;

         GNAT.Task_Lock.Unlock;

         Context.Initialized := False;
         Context.Ref_Count := 1;

      exception
         when others =>
            GNAT.Task_Lock.Unlock;
            raise;
      end Acquire;

      -------------
      -- Release --
      -------------

      procedure Release (Context : in out Internal_Context) is
      begin
         GNAT.Task_Lock.Lock;

         Available.Append (Context);
         Context.Serial_Number := Context.Serial_Number + 1;
         Context := null;

         GNAT.Task_Lock.Unlock;

      exception
         when others =>
            GNAT.Task_Lock.Unlock;
            raise;
      end Release;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         GNAT.Task_Lock.Lock;

         for C of Available loop
            Free (C);
         end loop;

         GNAT.Task_Lock.Unlock;

      exception
         when others =>
            GNAT.Task_Lock.Unlock;
            raise;
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

   ----------------------
   -- Allocate_Context --
   ----------------------

   function Allocate_Context return Internal_Context is
   begin
      return Context : Internal_Context do
         Context_Pool.Acquire (Context);
      end return;
   end Allocate_Context;

   ------------------------
   -- Initialize_Context --
   ------------------------

   procedure Initialize_Context
     (Context        : Internal_Context;
      Charset        : String;
      File_Reader    : Internal_File_Reader_Access;
      Unit_Provider  : Internal_Unit_Provider_Access;
      Event_Handler  : Internal_Event_Handler_Access;
      With_Trivia    : Boolean;
      Tab_Stop       : Positive)
   is
      Actual_Charset : constant String :=
        (if Charset = "" then Default_Charset else Charset);
      Symbols        : constant Precomputed_Symbol_Table
        := Create_Symbol_Table;
   begin
      Context.Rewriting_Handle := System.Null_Address;
      Context.Rewriting_Version := 1;
      Context.Initialized := True;
      Context.Symbols := Symbol_Table (Symbols);
      Context.Charset := To_Unbounded_String (Actual_Charset);
      Context.Tab_Stop := Tab_Stop;
      Context.With_Trivia := With_Trivia;
      Context.Root_Scope := Create_Static_Lexical_Env
        (Parent    => Null_Lexical_Env,
         Node      => null,
         Sym_Table => Context.Symbols);

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

      % if cfg.library.defaults.unit_provider:
         if Context.Unit_Provider = null then
            Context.Unit_Provider := ${cfg.library.defaults.unit_provider.fqn};
         end if;
      % endif

      Initialize (Context.Parser);

      Context.Discard_Errors_In_Populate_Lexical_Env := True;
      Context.Logic_Resolution_Timeout :=
        Langkit_Support.Adalog.Default_Timeout_Ticks_Number;
      Context.In_Populate_Lexical_Env := False;
      Context.Cache_Version := 0;
      Context.Reparse_Cache_Version := 0;

      Context.Templates_Unit := No_Analysis_Unit;

      Context.Available_Rebindings := Env_Rebindings_Vectors.Empty_Vector;

      ${exts.include_extension(ctx.ext('analysis', 'context', 'create'))}
   end Initialize_Context;

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
      Input             : Langkit_Support.Internal.Analysis.Lexer_Input;
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
      Refined_Input  : Langkit_Support.Internal.Analysis.Lexer_Input := Input;

      Parsing_Happened : Boolean := False;

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
            Parsing_Happened := Reparsed.Present;
            Update_After_Reparse (Unit, Reparsed);
         end;

         --  Now that we have removed reparsed the unit, update its current
         --  charset.

         Unit.Charset := Actual_Charset;
      end if;

      if Context.Event_Handler /= null then
         Context.Event_Handler.Unit_Parsed_Callback
           (Context  => Context,
            Unit     => Unit,
            Reparsed => Parsing_Happened and then not Created);
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
      Input : constant Langkit_Support.Internal.Analysis.Lexer_Input :=
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
      Input : constant Langkit_Support.Internal.Analysis.Lexer_Input :=
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

   % if cfg.library.defaults.unit_provider:

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

      declare
         Result      : Internal_Unit;
         Dummy_Index : Positive;
      begin
         Context.Unit_Provider.Get_Unit_And_PLE_Root
           (Context, Name, Kind, Charset, Reparse, Result, Dummy_Index);
         return Result;
      exception
         when ${ctx.property_exception_matcher} =>
            raise Invalid_Unit_Name_Error with
               "Invalid unit name: " & Image (Name, With_Quotes => True)
               & " (" & Analysis_Unit_Kind'Image (Kind) & ")";
      end;
   end Get_From_Provider;

   % endif

   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Internal_Context) return Internal_Unit_Provider_Access
   is (Context.Unit_Provider);

   ------------------
   -- Resolve_Unit --
   ------------------

   procedure Resolve_Unit
     (Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Unit    : out Resolved_Unit)
   is
      --  Look for the cache entry corresponding to Unit; create one if needed

      Dummy    : Resolved_Unit_Array;
      Key      : constant Symbol_Type := Find (Context.Symbols, Name);
      Pos      : Unit_Provider_Cache_Maps.Cursor;
      Inserted : Boolean;
   begin
      Context.Unit_Provider_Cache.Insert (Key, Dummy, Pos, Inserted);
      declare
         Units : Resolved_Unit_Array renames
           Context.Unit_Provider_Cache.Reference (Pos);
         U     : Resolved_Unit renames Units (Kind);
      begin
         --  If the cache entry is not populated for the requested kind, run
         --  the query and save the result for later requests.

         if U.Filename = null then
            declare
               Provider : Internal_Unit_Provider'Class renames
                 Context.Unit_Provider.all;
               Filename : Unbounded_String;
            begin
               Provider.Get_Unit_Location
                 (Name           => Name,
                  Kind           => Kind,
                  Filename       => Filename,
                  PLE_Root_Index => U.PLE_Root_Index);
               Provider.Get_Unit_And_PLE_Root
                 (Context        => Context,
                  Name           => Name,
                  Kind           => Kind,
                  Unit           => U.Unit,
                  PLE_Root_Index => U.PLE_Root_Index);
               U.Filename := new String'(To_String (Filename));
            end;
         end if;

         Unit := U;
      end;
   end Resolve_Unit;

   -----------------------
   -- Get_Unit_Location --
   -----------------------

   procedure Get_Unit_Location
     (Context        : Internal_Context;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : out String_Access;
      PLE_Root_Index : out Positive)
   is
      U : Resolved_Unit;
   begin
      Resolve_Unit (Context, Name, Kind, U);
      Filename := U.Filename;
      PLE_Root_Index := U.PLE_Root_Index;
   end Get_Unit_Location;

   ---------------------------
   -- Get_Unit_And_PLE_Root --
   ---------------------------

   procedure Get_Unit_And_PLE_Root
     (Context        : Internal_Context;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Unit           : out Internal_Unit;
      PLE_Root_Index : out Positive)
   is
      U : Resolved_Unit;
   begin
      Resolve_Unit (Context, Name, Kind, U);
      Unit := U.Unit;
      PLE_Root_Index := U.PLE_Root_Index;
   end Get_Unit_And_PLE_Root;

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
      return Context.Rewriting_Handle /= System.Null_Address;
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

            --  If this context was not completely initialized, just release
            --  the allocated object. Do the full destruction otherwise.
            if Context.Initialized then
               Destroy (Context);
            end if;
            Context_Pool.Release (Context);
         end if;
      end if;
   end Dec_Ref;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Context : Internal_Context) is
   begin
      ${exts.include_extension(ctx.ext('analysis', 'context', 'destroy'))}

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
      Context.Filenames :=
        Langkit_Support.Internal.Analysis.Empty_Virtual_File_Cache;

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

      for Pos in Context.Unit_Provider_Cache.Iterate loop
         declare
            Units : Resolved_Unit_Array renames
              Context.Unit_Provider_Cache.Reference (Pos);
         begin
            for U of Units loop
               Free (U.Filename);
            end loop;
         end;
      end loop;
      Context.Unit_Provider_Cache.Clear;

      Destroy (Context.Templates_Unit);
      AST_Envs.Destroy (Context.Root_Scope);
      Destroy (Context.Symbols);
      Destroy (Context.Parser);
      Dec_Ref (Context.File_Reader);
      Dec_Ref (Context.Unit_Provider);
      Dec_Ref (Context.Event_Handler);
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

   -----------------------
   -- Reset_Envs_Caches --
   -----------------------

   procedure Reset_Envs_Caches (Unit : Internal_Unit) is
      procedure Internal (Node : ${T.root_node.name});
      --  Reset env caches in ``Node`` and then in its children recursively

      Generic_Unit : constant Generic_Unit_Ptr := Convert_Unit (Unit);

      --------------
      -- Internal --
      --------------

      procedure Internal (Node : ${T.root_node.name}) is
      begin
         if Node = null then
            return;
         end if;
         --  Make sure to only reset caches of envs belonging to this unit
         if Node.Self_Env.Owner = Generic_Unit then
            AST_Envs.Reset_Caches (Node.Self_Env);
         end if;
         for I in 1 .. Children_Count (Node) loop
            Internal (Child (Node, I));
         end loop;
      end Internal;
   begin
      Internal (Unit.Ast_Root);
   end Reset_Envs_Caches;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env
     (Unit           : Internal_Unit;
      PLE_Root_Index : Positive
      ## To simplify code generation, generate the PLE_Root_Index argument even
      ## though we always expect it to be 1 when there are no PLE root for this
      ## language spec. However, still to simplify code generation, give it a
      ## default expression in that case.
      % if not ctx.ple_unit_root:
         := 1
      % endif
   ) is
      Context : constant Internal_Context := Unit.Context;

      Saved_In_Populate_Lexical_Env : constant Boolean :=
        Context.In_Populate_Lexical_Env;

      Has_Errors : Boolean := False;
      --  Whether at least one Property_Error occurred during this PLE pass

   begin
      --  TODO??? Handle env invalidation when reparsing a unit and when a
      --  previous call raised a Property_Error.

      --  If we have already run PLE on this root, there is nothing to do.
      --  Otherwise, keep track of the fact that PLE was requested for it,
      --  possibly extending the vector if needed.

      if Unit.Env_Populated_Roots.Last_Index >= PLE_Root_Index
         and then Unit.Env_Populated_Roots.Get (PLE_Root_Index)
      then
         return;
      end if;
      for Dummy in Unit.Env_Populated_Roots.Last_Index + 1 .. PLE_Root_Index
      loop
         Unit.Env_Populated_Roots.Append (False);
      end loop;
      Unit.Env_Populated_Roots.Set (PLE_Root_Index, True);

      --  Create context for the PLE run: all exit points must call the Cleanup
      --  procedure above first to clean this context.

      Context.In_Populate_Lexical_Env := True;
      if Main_Trace.Active then
         Main_Trace.Trace
           ("Populating lexical envs for"
            % if ctx.ple_unit_root:
            & " root" & PLE_Root_Index'Image & " of"
            % endif
            & " unit: " & Basename (Unit));
         Main_Trace.Increase_Indent;
      end if;

      --  Fetch the node on which to run PLE: it's the unit root node, or one
      --  of its children if PLE roots are enabled and the unit has a list of
      --  PLE roots. Then run PLE itself.

      declare
         PLE_Root : ${T.root_node.name} := Unit.Ast_Root;
      begin
         % if ctx.ple_unit_root:
            if Unit.Ast_Root /= null
               and then Unit.Ast_Root.Kind
                        = ${ctx.ple_unit_root.list.ada_kind_name}
            then
               PLE_Root := Child (Unit.Ast_Root, PLE_Root_Index);
            end if;
         % endif

         if PLE_Root /= null then
            Has_Errors := Populate_Lexical_Env (PLE_Root);
         end if;
      end;

      --  Restore the context for PLE run (undo what was done above)

      Context.In_Populate_Lexical_Env := Saved_In_Populate_Lexical_Env;
      if Main_Trace.Active then
         Main_Trace.Decrease_Indent;
         Main_Trace.Trace
           ("Finished populating lexical envs for"
            % if ctx.ple_unit_root:
            & " root" & PLE_Root_Index'Image & " of"
            % endif
            & " unit: " & Basename (Unit));
      end if;

      Reset_Envs_Caches (Unit);

      if Has_Errors and then not Context.Discard_Errors_In_Populate_Lexical_Env
      then
         raise Property_Error with
            "errors occurred in Populate_Lexical_Env";
      end if;
   end Populate_Lexical_Env;

   -----------------------------------
   -- Populate_Lexical_Env_For_Unit --
   -----------------------------------

   procedure Populate_Lexical_Env_For_Unit (Node : ${T.root_node.name}) is
      Root  : ${T.root_node.name};
      Index : Natural;
   begin
      Lookup_PLE_Root (Node, Root, Index);
      if Index = 0 then
         Index := 1;
      end if;
      Populate_Lexical_Env (Node.Unit, Index);
   end Populate_Lexical_Env_For_Unit;

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
     (Unit.Ast_Root);

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Internal_Unit) return Token_Reference is
     (Wrap_Token_Reference (Unit.Context,
                            Unit.TDH'Access,
                            First_Token_Or_Trivia (Unit.TDH)));

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Internal_Unit) return Token_Reference is
     (Wrap_Token_Reference (Unit.Context,
                            Unit.TDH'Access,
                            Last_Token_Or_Trivia (Unit.TDH)));

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
      return Wrap_Token_Reference (Unit.Context, Unit.TDH'Access, Result);
   end Lookup_Token;

   ---------------------
   -- Lookup_PLE_Root --
   ---------------------

   procedure Lookup_PLE_Root
     (Node  : ${T.root_node.name};
      Root  : out ${T.root_node.name};
      Index : out Natural)
   is
      Unit : constant Internal_Unit := Node.Unit;
   begin
      --  If this unit does not contain a list of PLE roots, just return the
      --  unit root node.

      if Unit.PLE_Roots_Starting_Token.Is_Empty then
         Root := Unit.Ast_Root;
         Index := 0;
         return;
      end if;

      --  Otherwise, look for the last PLE root whose first token (in
      --  Unit.PLE_Roots_Starting_Token) appears before Node's (T). This vector
      --  is sorted by construction, so we can perform a binary search.

      declare
         T      : constant Token_Index := Node.Token_Start_Index;
         Tokens : Token_Index_Vectors.Vector renames
           Unit.PLE_Roots_Starting_Token;

         First : Positive := Tokens.First_Index;
         Last  : Positive := Tokens.Last_Index;
         I     : Positive;
      begin
         while First < Last loop

            --  Because we look for the "floor" (last element that is <= T), we
            --  need to look at the value in Last when there are only two
            --  elements left to look at. If we did not do that, then we would
            --  go into an infinite loop when Tokens[First] < T.

            I := (if First + 1 = Last
                  then Last
                  else (First + Last) / 2);
            declare
               I_T : constant Token_Index := Tokens.Get (I);
            begin
               if I_T <= T then
                  First := I;
               else
                  Last := I - 1;
               end if;
            end;
         end loop;

         Root := Child (Unit.Ast_Root, First);
         Index := First;
      end;
   end Lookup_PLE_Root;

   --------------
   -- Ple_Root --
   --------------

   function Ple_Root
     (Node : ${T.root_node.name}) return ${T.root_node.name}
   is
      Root        : ${T.root_node.name};
      Dummy_Index : Natural;
   begin
      if Node = null then
         raise Property_Error with "null node dereference";
      end if;
      Lookup_PLE_Root (Node, Root, Dummy_Index);
      return Root;
   end Ple_Root;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Internal_Unit) is
      Node     : constant ${T.root_node.name} := Unit.Ast_Root;
      Root_Env : constant Lexical_Env := Unit.Context.Root_Scope;
      State    : Dump_Lexical_Env_State := (Root_Env => Root_Env, others => <>);

      procedure Dump_One_Lexical_Env (Env, Parent : Lexical_Env);
      --  Wrapper around the library-level Dump_One_Lexical_Env procedure, to
      --  automatically compute ids for Env and Parent.

      function Get_Parent (Env : Lexical_Env) return Lexical_Env
      is (AST_Envs.Unwrap (Env).Parent);

      --------------------------
      -- Dump_One_Lexical_Env --
      --------------------------

      procedure Dump_One_Lexical_Env (Env, Parent : Lexical_Env) is
         Env_Id    : constant String := Get_Env_Id (Env, State);
         Parent_Id : constant String := Get_Env_Id (Parent, State);
      begin
         AST_Envs.Dump_One_Lexical_Env (Env, Env_Id, Parent_Id);
      end Dump_One_Lexical_Env;

      --------------------------
      -- Explore_Parent_Chain --
      --------------------------

      procedure Explore_Parent_Chain (Env : Lexical_Env) is
         P : Lexical_Env;
      begin
         if Env /= Null_Lexical_Env then
            P := Get_Parent (Env);
            Dump_One_Lexical_Env (Env, P);
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
            Dump_One_Lexical_Env (Env, Parent);

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
      if Unit.Ast_Root = null then
         Put_Line ("<empty analysis unit>");
      else
         Print (Unit.Ast_Root, Show_Slocs);
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

      PP_Trivia (Unit.Ast_Root);

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

      --  Clear the env caches while the unit is still fully alive to make sure
      --  that what is accessed in ``Lexical_Env_Cache_Updated`` is still
      --  valid, as it will be called back by lexical envs that are being
      --  destroyed.
      Reset_Envs_Caches (Unit);

      Unit.PLE_Roots_Starting_Token.Destroy;
      Unit.Env_Populated_Roots.Destroy;

      Unit.Exiled_Entries.Destroy;
      Unit.Foreign_Nodes.Destroy;
      Unit.Exiled_Entries_In_NED.Destroy;
      Unit.Exiled_Envs.Destroy;
      Unit.Named_Envs.Destroy;

      % if ctx.has_memoization:
         Destroy (Unit.Memoization_Map);
      % endif

      Destroy_Rebindings (Unit.Rebindings'Access);
      Unit.Rebindings.Destroy;

      if Unit.Ast_Root /= null then
         Destroy (Unit.Ast_Root);
      end if;

      Free (Unit.TDH);
      Free (Unit.Ast_Mem_Pool);
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
   -- Raise_Property_Exception --
   ------------------------------

   procedure Raise_Property_Exception
     (Node    : ${T.root_node.name};
      Exc     : Ada.Exceptions.Exception_Id;
      Message : String)
   is
      Sloc_Prefix : constant String :=
        (if Node = null
         then ""
         else Ada.Directories.Simple_Name (Get_Filename (Unit (Node)))
              & ":" & Image (Sloc_Range (Node)) & ": ");
   begin
      Ada.Exceptions.Raise_Exception (Exc, Sloc_Prefix & Message);
   end Raise_Property_Exception;

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

   % for set_type in ctx.set_types:
      ${set_types.body(set_type)}
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

   package Solver_Diagnostic_Vectors is new Langkit_Support.Vectors
     (Internal_Solver_Diagnostic);

   ----------------------------
   -- Allocate_Logic_Context --
   ----------------------------

   function Allocate_Logic_Context
     (Ctx : Internal_Logic_Context) return Internal_Logic_Context_Access
   is ((if Ctx.Ref_Node = ${T.entity.nullexpr}
           and then Ctx.Decl_Node = ${T.entity.nullexpr}
        then null
        else new Internal_Logic_Context'(Ctx)));

   -------------------------
   -- Trace_Logic_Context --
   -------------------------

   function Trace_Logic_Context
     (Ctx : Internal_Logic_Context_Access) return String
   is (Trace_Image (Ctx.all));

   -----------------
   -- Deep_Equals --
   -----------------

   function Deep_Equals
     (X, Y : Internal_Logic_Context_Access) return Boolean
   is (X.all = Y.all);

   ------------------------
   -- Free_Logic_Context --
   ------------------------

   procedure Free_Logic_Context
     (Ctx : in out Internal_Logic_Context_Access)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Internal_Logic_Context, Internal_Logic_Context_Access);
   begin
      Free (Ctx);
   end Free_Logic_Context;

   -------------------
   -- Solve_Wrapper --
   -------------------

   function Solve_Wrapper
     (R            : Solver.Relation;
      Context_Node : ${T.root_node.name}) return Boolean is
   begin
      if Context_Node /= null and then Langkit_Support.Adalog.Debug.Debug then
         Assign_Names_To_Logic_Vars (Context_Node);
      end if;

      begin
         return Solver.Solve_First
           (R, Timeout => Context_Node.Unit.Context.Logic_Resolution_Timeout);
      exception
         when Langkit_Support.Adalog.Early_Binding_Error =>
            Raise_Property_Exception
              (Context_Node,
               Property_Error'Identity,
               "invalid equation for logic resolution");
         when Langkit_Support.Adalog.Timeout_Error =>
            Raise_Property_Exception
              (Context_Node,
               Property_Error'Identity,
               "logic resolution timed out");
      end;
   end Solve_Wrapper;

   ----------------------------
   -- Solve_With_Diagnostics --
   ----------------------------

   function Solve_With_Diagnostics
     (R            : Solver.Relation;
      Context_Node : ${T.root_node.name}) return Internal_Solver_Result
   is
      Ret : Internal_Solver_Result :=
        (True, No_Internal_Solver_Diagnostic_Array_Type);

      Acc : Solver_Diagnostic_Vectors.Vector;
      --  Vectors that will accumulate diagnostic emitted during resolution

      procedure Emit_Diagnostic (Diag : Internal_Solver_Diagnostic) is
      begin
         Acc.Append (Diag);
      end Emit_Diagnostic;
   begin
      Ret.Success := Solve_Wrapper (R, Context_Node);

      if not Ret.Success then
         Ret.Success := Solver.Solve_First
           (R,
            Solve_Options => (Report_Errors => True),
            Diag_Emitter  => Emit_Diagnostic'Unrestricted_Access,
            Timeout       =>
              Context_Node.Unit.Context.Logic_Resolution_Timeout);
         Ret.Diagnostics := Create_Internal_Solver_Diagnostic_Array
           (Acc.Length);
         for I in 1 .. Acc.Length loop
            Ret.Diagnostics.Items (I) := Acc.Get (I);
         end loop;
         Acc.Destroy;
      end if;
      return Ret;
   end Solve_With_Diagnostics;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Env : in out Lexical_Env_Access) is
      Mutable_Env : Lexical_Env :=
        (AST_Envs.Wrap (Env), 0, Env.Kind, No_Generic_Unit, 0);
   begin
      AST_Envs.Destroy (Mutable_Env);
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

   --------------------------------------
   -- Allocate_Synthetic_List_Children --
   --------------------------------------

   function Allocate_Synthetic_List_Children
     (Self  : ${ctx.generic_list_type.name};
      Count : Natural) return Alloc_AST_List_Array.Element_Array_Access
   is
      use Alloc_AST_List_Array;
      use System.Memory;

      Size : constant size_t :=
        ${T.root_node.name}'Max_Size_In_Storage_Elements * size_t (Count);
   begin
      return Result : constant Element_Array_Access :=
        (if Count = 0
         then Empty_Array_Access
         else To_Pointer (System.Memory.Alloc (Size)))
      do
         Self.Count := Count;
         Self.Nodes := Result;
      end return;
   end Allocate_Synthetic_List_Children;

   ----------------------------------
   -- Free_Synthetic_List_Children --
   ----------------------------------

   procedure Free_Synthetic_List_Children
     (Self : ${ctx.generic_list_type.name})
   is
      use Alloc_AST_List_Array;
      use System.Memory;
   begin
      if Self.Nodes /= Empty_Array_Access then
         Free (To_Address (Self.Nodes));
      end if;
   end Free_Synthetic_List_Children;

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
     (Self         : ${T.root_node.name};
      State        : in out PLE_Node_State;
      Env          : ${T.DesignatedEnv.name};
      DSL_Location : String) is
   begin
      case Env.Kind is
         when None =>
            Use_Direct_Env (State, Empty_Env);

         when Current_Env =>
            null;

         when Named_Env =>
            Use_Named_Env (State, Self.Unit.Context, Env.Env_Name);

         when Direct_Env =>

            --  Sanitize this environment value: make sure it's a non-foreign
            --  and primary environment.

            if Env.Direct_Env.Kind /= Static_Primary then
               Raise_Property_Exception
                 (Self,
                  Property_Error'Identity,
                  "Cannot set an env that is not static-primary as the"
                  & " initial env");

            elsif AST_Envs.Is_Foreign_Strict (Env.Direct_Env, Self) then
               Raise_Property_Exception
                 (Self,
                  Property_Error'Identity,
                  "unsound foreign environment in SetInitialEnv ("
                  & DSL_Location & ")");
            end if;
            Use_Direct_Env (State, Env.Direct_Env);
      end case;
   end Set_Initial_Env;

   ----------------
   -- Add_To_Env --
   ----------------

   procedure Add_To_Env
     (Self         : ${T.root_node.name};
      State        : PLE_Node_State;
      Key          : Symbol_Type;
      Value        : ${T.root_node.name};
      Md           : ${T.env_md.name};
      Resolver     : Entity_Resolver;
      Dest_Env     : ${T.DesignatedEnv.name};
      DSL_Location : String)
   is
      Context    : constant Internal_Context := Self.Unit.Context;
      Root_Scope : Lexical_Env renames Context.Root_Scope;
      --  Shortcuts

      Actual_Dest_Env : Lexical_Env;
      Dest_NED        : Named_Env_Descriptor_Access;
      --  Description for the destination environment
   begin
      --  Skip the env addition if explicitly requested

      if Key = No_Symbol
         or else Value = null
         or else (case Dest_Env.Kind is
                  when None        => True,
                  when Current_Env => False,
                  when Named_Env   => Dest_Env.Env_Name = No_Symbol,
                  when Direct_Env  => Dest_Env.Direct_Env = Empty_Env)
      then
         return;
      end if;

      if Value.Unit /= Self.Unit then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "Cannot add_to_env an AST node that comes from another analysis"
            & " unit");
      end if;

      <% astnode_fields = [f for f in T.env_md.get_fields()
                           if f.type.is_ast_node] %>
      % if astnode_fields:
         --  Make sure metadata does not contain any foreign node
         if ${(
            ' or else '.join(
                ['({n} /= null and then {n}.Unit /= Self.Unit)'.format(
                    n='Md.{}'.format(f.names.codegen)
                ) for f in astnode_fields]
            )
         )}
         then
            Raise_Property_Exception
              (Self,
               Property_Error'Identity,
               "Cannot add metadata that contains foreign nodes");
         end if;
      % endif

      --  Then determine the destination environment

      case Dest_Env.Kind is
         when None =>
            raise Program_Error with "unreachable code";

         when Current_Env =>
            --  Just use the current environment
            Dest_NED := State.Current_NED;
            Actual_Dest_Env := State.Current_Env;

         when Named_Env =>
            --  There is an environment name: just lookup the corresponding
            --  NED/env.
            Dest_NED := Get_Named_Env_Descriptor (Context, Dest_Env.Env_Name);
            Actual_Dest_Env := Dest_NED.Env_With_Precedence;

         when Direct_Env =>
            --  There is an explicit destination environment
            Dest_NED := null;
            Actual_Dest_Env := Dest_Env.Direct_Env;
      end case;

      --  Sanitize it

      if Actual_Dest_Env.Kind /= Static_Primary then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "Cannot add elements to a lexical env that is not static-primary");

      elsif
         --  Since lexical envs need to sort the foreign nodes they contain,
         --  and that the total order on nodes is not defined for synthetic
         --  nodes, it is not possible to add a synthetic node to a foreign
         --  lexical environment.
         --
         --  This reasoning applies to environments that belong to foreign
         --  units, but also to the root environment.
         AST_Envs.Is_Foreign (Actual_Dest_Env, Self)
         and then Is_Synthetic (Value)
      then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "Cannot add a synthetic node to a lexical env from another"
            & " analysis unit");

      elsif
         --  Reject direct references to foreign destination environments.
         --
         --  This is an attempt at identifying uses of the unsound relocation
         --  mechanism (as opposed to named environments), so this applies to
         --  all foreign environments (root scope included).
         DSL_Location'Length > 0
         and then Dest_Env.Kind = Direct_Env
         and then AST_Envs.Is_Foreign_Strict (Actual_Dest_Env, Self)
      then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "unsound foreign environment in AddToEnv (" & DSL_Location & ")");
      end if;

      --  Now that everything is sanitized, we can proceed with the actual
      --  key/value pair addition. Note that this does nothing if
      --  Actual_Dest_Env ended up empty.
      AST_Envs.Add (Actual_Dest_Env, Thin (Key), Value, Md, Resolver);

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
               V.Append ((Value, null, Md, Resolver));
               --  Note that the rebindings field is unused by the relocation
               --  mechanism (since we don't even allow adding env entries
               --  with custom rebindings), hence we simply leave it to null.
            end;
         end;
         Value.Unit.Exiled_Entries_In_NED.Append ((Dest_NED, Key, Value));

      --  Otherwise, if we're adding the element to an environment that belongs
      --  to a different unit, or to the root scope, then...
      elsif AST_Envs.Is_Foreign_Not_Empty (Actual_Dest_Env, Self) then
         --  Add the Key/Value association to the list of entries contained in
         --  other units, so we can remove them when reparsing Value's unit.
         Value.Unit.Exiled_Entries.Append ((Actual_Dest_Env, Key, Value));

         if Actual_Dest_Env /= Root_Scope then
            --  Add Val to the list of foreign nodes that Actual_Dest_Env's
            --  unit contains, so that when that unit is reparsed, we can call
            --  Add_To_Env again on those nodes.
            Convert_Unit (Actual_Dest_Env.Owner).Foreign_Nodes.Append
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
               Raise_Property_Exception
                 (Self,
                  Property_Error'Identity,
                  "attempt to add a referenced environment to a foreign unit");
            end if;
            AST_Envs.Reference
              (Dest_Env, N, Resolver, Kind, Cats, Shed_Rebindings);
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
      Names             : in out ${T.Symbol.array.name})
   is
      Parent_From_Name : constant Boolean := State.Current_NED /= null;
      --  Does the parent environment comes from a named environment lookup?

      --  Determine the parent of this new environment:
      --
      --  (1) no parent if requested;
      --  (2) the current environment as the static parent if it comes from a
      --      named env lookup or if it is not foreign (or is the empty/root
      --      environment).
      Parent : constant Lexical_Env :=
        (if No_Parent
         then Null_Lexical_Env
         else State.Current_Env);
   begin
      --  Create the environment itself
      Self.Self_Env := Create_Static_Lexical_Env
        (Parent            => Parent,
         Node              => Self,
         Transitive_Parent => Transitive_Parent,
         Sym_Table         => Self.Unit.Context.Symbols);

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

      % if not any( \
         n.env_spec and n.env_spec.pre_actions for n in ctx.node_types \
      ):
         pragma Unreferenced (State, Add_To_Env_Only);
      % endif
   end Pre_Env_Actions;

   ----------------------
   -- Post_Env_Actions --
   ----------------------

   procedure Post_Env_Actions
     (Self : ${T.root_node.name}; State : in out PLE_Node_State) is
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

      % if not any( \
         n.env_spec and n.env_spec.post_actions for n in ctx.node_types \
      ):
         pragma Unreferenced (State);
      % endif
   end Post_Env_Actions;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Node : ${T.root_node.name}) return Symbol_Type is
   begin
      if Node = null then
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "cannot get the symbol of a null node");
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
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "cannot get the text of a null node");
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
         return (if Node.Parent = null
                 then No_Source_Location_Range
                 else Sloc_Range (Node.Parent));
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
     (Self, Left, Right : ${T.root_node.name};
      Relation          : Comparison_Relation) return Boolean
   is
      LS, RS : Source_Location;
   begin
      if Left = null or else Right = null or else Left.Unit /= Right.Unit then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "invalid node comparison");
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
     (Self    : ${T.root_node.name};
      Node    : ${ctx.generic_list_type.name};
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
         Raise_Property_Exception
           (Self, Property_Error'Identity, "out-of-bounds AST list access");
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
      if Node = null then
         Put_Line (Line_Prefix & "None");
         return;
      end if;
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
            when Exc : ${ctx.property_exception_matcher} =>
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
      --  This is intended to be called on the root node only (when there is no
      --  PLE root) or on a PLE root (child of the root node with a specific
      --  kind).
      if
         Node.Parent /= null
         % if ctx.ple_unit_root:
            and then Node.Kind /= ${ctx.ple_unit_root.ada_kind_name}
         % endif
      then
         raise Program_Error;
      end if;

      return Result : constant Boolean :=
         Populate_Internal (Node, Root_State)
      do
         Update_Named_Envs (Context, Unit_State.Named_Envs_Needing_Update);
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
      <% rebindable_nodes = [n for n in ctx.node_types
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
         AST_Envs.Unwrap (Self.Old_Env).Node.Unit.Context.Available_Rebindings;
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

   % if cache_collection is not None:

      -------------------------------
      -- Lexical_Env_Cache_Updated --
      -------------------------------

      procedure Lexical_Env_Cache_Updated
        (Node         : ${T.root_node.name};
         Delta_Amount : Long_Long_Integer)
      is
         Ctx : constant Internal_Context := Node.Unit.Context;

         Ctx_Stats : Context_Env_Caches_Stats renames Ctx.Env_Caches_Stats;

         All_Env_Caches_Entry_Count : constant Long_Long_Integer :=
           Ctx_Stats.Entry_Count;
         --  Snapshot of the total number of entries before any collection is
         --  attempted. This is needed because if a collection happens,
         --  ``Ctx.Env_Caches_Stats.Entry_Count`` will be updated on the go.
      begin
         Ctx_Stats.Entry_Count :=
           All_Env_Caches_Entry_Count + Delta_Amount;
         Node.Unit.Env_Caches_Stats.Entry_Count :=
           Node.Unit.Env_Caches_Stats.Entry_Count + Delta_Amount;

         --  If the number of entries exceeds the threshold we had set, attempt
         --  to invalidate caches. Don't do anything if this notification was
         --  for removed entries (i.e. if ``Delta_Amount`` is negative) as it
         --  could mean that we are already in the middle of a collection.
         if Delta_Amount > 0 and then
            Ctx_Stats.Entry_Count > Ctx.Env_Caches_Collection_Threshold
         then
            if Cache_Invalidation_Trace.Is_Active then
               Cache_Invalidation_Trace.Trace
                 ("Attempting cache collection because number of entries"
                  & " reached" & All_Env_Caches_Entry_Count'Image);
               Cache_Invalidation_Trace.Increase_Indent;
            end if;

            for Unit of Ctx.Units loop
               % if cache_collection.decision_heuristic:
               if ${cache_collection.decision_heuristic.fqn}
                 (Ctx, Unit, All_Env_Caches_Entry_Count)
               then
               % endif
                  --  Clear all caches and set counters that are meant to
                  --  track events since the unit's last collection.
                  Reset_Envs_Caches (Unit);
                  Unit.Env_Caches_Stats.Lookup_Count := 0;
                  Unit.Env_Caches_Stats.Hit_Count := 0;
                  Unit.Env_Caches_Stats.Last_Overall_Lookup_Count :=
                    Ctx_Stats.Lookup_Count;
               % if cache_collection.decision_heuristic:
               end if;
               % endif
               Unit.Env_Caches_Stats.Previous_Lookup_Count :=
                 Unit.Env_Caches_Stats.Lookup_Count;
            end loop;

            Ctx_Stats.Previous_Lookup_Count := Ctx_Stats.Lookup_Count;

            --  Setup the threshold so that the next collection is triggered
            --  when we reach the current number of entries (after this
            --  collection) plus the increment that is defined for this
            --  language.
            Ctx.Env_Caches_Collection_Threshold :=
              Ctx_Stats.Entry_Count
              + ${cache_collection.threshold_increment};

            if Cache_Invalidation_Trace.Is_Active then
               Cache_Invalidation_Trace.Trace
                 ("New collection threshold :"
                  & Ctx.Env_Caches_Collection_Threshold'Image);
               Cache_Invalidation_Trace.Decrease_Indent;
            end if;
         end if;
      end Lexical_Env_Cache_Updated;

      ---------------------------------
      -- Lexical_Env_Cache_Looked_Up --
      ---------------------------------

      procedure Lexical_Env_Cache_Looked_Up (Node : ${T.root_node.name}) is
         Unit : constant Internal_Unit := Node.Unit;
         Ctx  : constant Internal_Context := Unit.Context;
      begin
         Unit.Env_Caches_Stats.Lookup_Count :=
            Unit.Env_Caches_Stats.Lookup_Count + 1;

         Ctx.Env_Caches_Stats.Lookup_Count :=
            Ctx.Env_Caches_Stats.Lookup_Count + 1;
      end Lexical_Env_Cache_Looked_Up;

      ---------------------------
      -- Lexical_Env_Cache_Hit --
      ---------------------------

      procedure Lexical_Env_Cache_Hit (Node : ${T.root_node.name}) is
         Unit : constant Internal_Unit := Node.Unit;
      begin
         Unit.Env_Caches_Stats.Hit_Count :=
            Unit.Env_Caches_Stats.Hit_Count + 1;
      end Lexical_Env_Cache_Hit;

   % endif

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

   % if T.Char.requires_hash_function:
      function Hash (I : Character_Type) return Hash_Type
      is (Hash_Type'Mod (Character_Type'Pos (I)));
   % endif

   % if T.String.requires_hash_function:
      function Hash (I : String_Type) return Hash_Type is
         function String_Hash is new GNAT.String_Hash.Hash
           (Character_Type, Text_Type, Hash_Type);
      begin
         return String_Hash (I.Content);
      end Hash;
   % endif

   % for enum_type in ctx.enum_types:
      % if enum_type.requires_hash_function:
         ----------
         -- Hash --
         ----------

         function Hash (Self : ${enum_type.name}) return Hash_Type is
         begin
            return ${enum_type.name}'Pos (Self);
         end Hash;
      % endif
   % endfor

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
      Node       : constant ${T.root_node.name} := AST_Envs.Env_Node (Env);
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

   procedure Update_Named_Envs
     (Context : Internal_Context; Named_Envs : NED_Maps.Map)
   is
      Require_Cache_Reset : Boolean := False;
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
                        AST_Envs.Remove
                          (NE.Env_With_Precedence, Thin (Key), N.Node);
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
                     AST_Envs.Add
                       (New_Env, Thin (Key), N.Node, N.Md, N.Resolver);
                  end loop;
               end;
            end loop;

            --  Set the parent environment of all foreign environments
            for Cur in NE.Foreign_Envs.Iterate loop
               declare
                  Env : AST_Envs.Lexical_Env_Record renames
                     AST_Envs.Unwrap (Sorted_Env_Maps.Element (Cur)).all;
               begin
                  Env.Parent := New_Env;

                  --  We have updated the lexical env hierarchy (e.g. an env
                  --  which had no parent may have one now), so the cached
                  --  entries for queries that traveresed the old env hierarchy
                  --  need to be invalidated.
                  Require_Cache_Reset := True;
               end;
            end loop;

            --  Update nodes whose environment was the old env with precedence
            for N of NE.Nodes_With_Foreign_Env loop
               N.Self_Env := New_Env;
            end loop;
         end;
      end loop;
      if Require_Cache_Reset then
         Invalidate_Caches (Context, Invalidate_Envs => True);
      end if;
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

   function To_Integer
     (Self    : ${T.root_node.name};
      Big_Int : Big_Integer_Type) return Integer
   is
      Image : constant String := Big_Int.Value.Image;
   begin
      return Integer'Value (Image);
   exception
      when Constraint_Error =>
         Raise_Property_Exception
           (Self, Property_Error'Identity, "out of range big integer");
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
      else
         Big_Int := null;
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

   ---------
   -- "-" --
   ---------

   function "-" (Value : Big_Integer_Type) return Big_Integer_Type is
      use type GNATCOLL.GMP.Integers.Big_Integer;
   begin
      return Create_Big_Integer (-Value.Value);
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

   ---------------
   --  Self_Env --
   ---------------

   function Self_Env (Node : ${T.root_node.name}) return Lexical_Env is
   begin
      return Node.Self_Env;
   end Self_Env;

   --------------------------
   -- Properties_May_Raise --
   --------------------------

   function Properties_May_Raise
     (Exc : Ada.Exceptions.Exception_Occurrence) return Boolean is
   begin
      return Ada.Exceptions.Exception_Identity (Exc) in
         % for i, exc in enumerate(ctx.property_exceptions):
            % if i > 0:
               |
            % endif
            ${exc}'Identity
         % endfor
      ;
   end Properties_May_Raise;

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
                & " " & Node_Sloc_Image (Self) & ">";
      </%def>
      </%self:case_dispatch>
   end Short_Text_Image;

   ----------------------
   --- Node_Sloc_Image --
   ----------------------

   function Node_Sloc_Image (Self : ${T.root_node.name}) return Text_Type
   is
      <% ext = ctx.ext("analysis", "node_sloc_image") %>
   begin
      % if ext:
         ${exts.include_extension(ext)}
      % else:
         return To_Text
                  (Ada.Directories.Simple_Name (Get_Filename (Unit (Self))))
                & ":" & To_Text (Image (Sloc_Range (Self)));
      % endif
   end Node_Sloc_Image;

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
        root_type = ctx.root_node_type.name

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
                    """.format(i, node_expr, f.names.codegen))
                result.append("""
                        when others => null;
                    end case;
                """)
            return '\n'.join(result)
      %>

      Index_In_Bounds := True;
      Result := null;
      ${ctx.generate_actions_for_hierarchy(
         node_var='Node',
         kind_var='K',
         actions_for_node=get_actions,
         unref_if_empty=["Index"]
      )}

      --  Execution should reach this point iff nothing matched this index, so
      --  we must be out of bounds.
      Index_In_Bounds := False;
   end Get_Child;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Langkit_Support.Generic_API.Analysis.Lk_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "")
   is
      use Langkit_Support.Generic_API.Analysis;
      use Langkit_Support.Generic_API.Introspection;

      T : Type_Ref;
   begin
      if Node.Is_Null then
         Put_Line ("None");
         return;
      end if;

      T := Type_Of (Node);
      Put (Line_Prefix & Image (Node_Type_Repr_Name (T)));
      if Show_Slocs then
         Put ("[" & Image (Node.Sloc_Range) & "]");
      end if;

      if Node.Is_Incomplete then
         Put (" <<INCOMPLETE>>");
      end if;

      if Node.Is_Token_Node then
         Put_Line (": " & Image (Node.Text));

      elsif Is_List_Node (Node) then

         --  List nodes are displayed in a special way (they have no field)

         declare
            Count : constant Natural := Node.Children_Count;
            Child : Lk_Node;
         begin
            if Count = 0 then
               Put_Line (": <empty list>");
               return;
            end if;
            New_Line;

            for I in 1 .. Count loop
               Child := Node.Child (I);
               if not Child.Is_Null then
                  Print (Child, Show_Slocs, Line_Prefix & "|  ");
               end if;
            end loop;
         end;

      else
         --  This is for regular nodes: display each syntax field (i.e.
         --  non-property member).

         declare
            Attr_Prefix     : constant String := Line_Prefix & "|";
            Children_Prefix : constant String := Line_Prefix & "|  ";
            M_List          : constant Struct_Member_Ref_Array := Members (T);
            Child           : Lk_Node;
         begin
            New_Line;
            for M of M_List loop
               if not Is_Property (M) and then not Is_Null_For (M, T) then
                  Child := As_Node (Eval_Node_Member (Node, M));
                  Put (Attr_Prefix
                       & Image (Format_Name (Member_Name (M), Lower)) & ":");
                  if Child.Is_Null then
                     Put_Line (" <null>");
                  else
                     New_Line;
                     Print (Child, Show_Slocs, Children_Prefix);
                  end if;
               end if;
            end loop;
         end;
      end if;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : ${T.root_node.name};
      Show_Slocs  : Boolean;
      Line_Prefix : String := "")
   is
      Entity : constant ${T.entity.name} := (Node, No_Entity_Info);
   begin
      Print (To_Generic_Node (Entity), Show_Slocs, Line_Prefix);
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
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "Cannot associate a token and a node from different analysis"
            & " units");
      elsif Index.Trivia /= No_Token_Index then
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "A node cannot hold trivia");
      end if;

      return Index.Token;
   end Stored_Token;

   -------------------------
   -- Children_And_Trivia --
   -------------------------

   function Children_And_Trivia
     (Node : ${T.root_node.name}) return Bare_Children_Vector
   is
      Ret_Vec : Bare_Children_Vector;
      Ctx     : Internal_Context renames Node.Unit.Context;
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
                     Trivia => Wrap_Token_Reference
                                 (Ctx, TDH'Access, (I, D))));
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

      return Ret_Vec;
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
      Unit    : constant Internal_Unit := Node.Unit;
      Context : constant Internal_Context := Unit.Context;
   begin
      return Wrap_Token_Reference
        (Context, Token_Data (Unit), (Index, No_Token_Index));
   end Token;

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
      E_Info : ${T.EntityInfo.name};
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
      E_Info : ${T.EntityInfo.name} := ${T.EntityInfo.nullexpr})
      return ${root_entity.name} is
   begin
      return Fetch_Sibling (Node, E_Info, -1);
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling
     (Node   : ${T.root_node.name};
      E_Info : ${T.EntityInfo.name} := ${T.EntityInfo.nullexpr})
      return ${root_entity.name} is
   begin
      return Fetch_Sibling (Node, E_Info, 1);
   end Next_Sibling;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Node_Builder_Type) is
   begin
      if Self.Ref_Count > 0 then
         Self.Ref_Count := Self.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Node_Builder_Type) is
   begin
      if Self = null or else Self.Ref_Count < 0 then
         return;
      elsif Self.Ref_Count = 1 then
         Self.Release;
         Free (Self);
      else
         Self.Ref_Count := Self.Ref_Count - 1;
      end if;
   end Dec_Ref;

   ------------------------------
   -- Create_Copy_Node_Builder --
   ------------------------------

   function Create_Copy_Node_Builder
     (Value : ${T.root_node.name}) return Node_Builder_Type is
   begin
      --  No need to allocate a new builder if in practice it cannot be
      --  distinguished from the "null" builder.

      if Value = null then
         return Null_Node_Builder;
      else
         return new Copy_Node_Builder_Record'(Ref_Count => 1, Value => Value);
      end if;
   end Create_Copy_Node_Builder;

   % for t in ctx.node_builder_types:
      % if t.synth_node_builder_needed:
         <%
            constructor_args = t.synth_constructor_args

            list_elements_arg = (
               constructor_args[0] if t.node_type.is_list_type else None
            )
            parse_fields = [
               arg.field for arg in constructor_args if arg.is_parse_field
            ]
            user_fields = [
               arg.field for arg in constructor_args if arg.is_user_field
            ]

            refcount_needed = any(
               arg.type.is_refcounted for arg in constructor_args
            )
         %>

         type ${t.record_type} is new Node_Builder_Record with
         % if constructor_args:
            record
            % for arg in constructor_args:
               ${arg.codegen_name} : ${arg.type.name};
            % endfor
            end record;
         % else:
            null record;
         % endif
         type ${t.access_type} is access all ${t.record_type};

         overriding function Build
           (Self              : ${t.record_type};
            Parent, Self_Node : ${T.root_node.name})
            return ${T.root_node.name};

         overriding function Trace_Image
           (Self : ${t.record_type}) return String
         is ("<NodeBuilder to synthetize ${t.node_type.lkt_name}>");

         % if refcount_needed:
            overriding procedure Release (Self : in out ${t.record_type});
         % endif

         -----------
         -- Build --
         -----------

         overriding function Build
           (Self              : ${t.record_type};
            Parent, Self_Node : ${T.root_node.name})
            return ${T.root_node.name}
         is
            Result : ${T.root_node.name};
            Unit   : constant Internal_Unit := Self_Node.Unit;
            Env    : constant Lexical_Env :=
              (if Parent = null
               then Empty_Env
               else Parent.Self_Env);
         begin
            if Parent /= null and then Parent.Unit /= Unit then
               Raise_Property_Exception
                 (Self_Node,
                  Property_Error'Identity,
                  "synthetic node parents must belong to the same unit as the"
                  & " nodes that trigger node synthetization");
            end if;

            ## Forbid node synthetization when Self.Self_Env is foreign, as in
            ## that case, this new node would escape the relocation mechanism
            ## when that foreign env is terminated.
            ##
            ## Note that we could, in principle, register this synthetized node
            ## so that the relocation mechanism takes care of it, but this
            ## incurs extra complexity for a use case that is not yet proven
            ## useful. So just forbid this situation.
            if AST_Envs.Is_Foreign_Strict (Env, Parent) then
               Raise_Property_Exception
                 (Self_Node,
                  Property_Error'Identity,
                  "synthetic nodes cannot have foreign lexical envs");
            end if;

            ## Allocate the synthetic node and initialize all of its components
            ## except the children.
            Result := new ${T.root_node.value_type_name}
              (${t.node_type.ada_kind_name});
            Initialize
              (Self => Result,
               Kind => ${t.node_type.ada_kind_name},
               Unit => Unit,

               ## Keep the token start/end null, as expected for a synthetized
               ## node.
               Token_Start_Index => No_Token_Index,
               Token_End_Index   => No_Token_Index,

               Parent   => Parent,
               Self_Env => Env);
            Register_Destroyable (Unit, Result);

            ## Build children and then initialize parse fields using the
            ## standard initialize procedure.
            % if parse_fields:
               declare
                  Children : array (1 .. ${len(parse_fields)})
                               of ${T.root_node.name};
               begin
                  <% init_fields_args = ["Self => Result"] %>
                  % for i, field in enumerate(parse_fields, 1):
                     <%
                        child_expr = f"Children ({i})"
                        init_fields_args.append(
                           f"{field.names.codegen} => {child_expr}"
                        )
                     %>
                     ${child_expr} :=
                       Self.${field.names.codegen}.Build (Result, Self_Node);

                     ## Reject null nodes for fields that are not nullable for
                     ## synthetic nodes.
                     % if not field.nullable:
                        if ${child_expr} = null then
                           Raise_Property_Exception
                             (Self_Node,
                              Property_Error'Identity,
                              "${field.qualname} cannot be null in synthetic"
                              & " nodes; add a nullable annotation to this"
                              & " field to allow it");
                        end if;
                     % endif
                  % endfor

                  Initialize_Fields_For_${t.node_type.kwless_raw_name}
                  ${ada_block_with_parens(init_fields_args, 18)};
               end;

            ## If we build a list node, initialize the list elements
            % elif list_elements_arg:
               declare
                  Elements_Builders : ${(
                     list_elements_arg.type.array_type_name
                  )} renames Self.${list_elements_arg.codegen_name}.Items;
                  Children          : constant
                    Alloc_AST_List_Array.Element_Array_Access :=
                      Allocate_Synthetic_List_Children
                        (Result, Elements_Builders'Length);
               begin
                  for I in Elements_Builders'Range loop
                     Children (I) :=
                       Elements_Builders (I).Build (Result, Self_Node);
                  end loop;
               end;
            % endif

            ## Then initialize user fields individually
            % for field in user_fields:
               Result.${field.names.codegen} :=
                  ${field.type.convert_to_storage_expr(
                     "Result", f"Self.{field.names.codegen}"
                  )};
               % if field.type.is_refcounted:
                  Inc_Ref (Result.${field.names.codegen});
               % endif
            % endfor

            return Result;
         end Build;

         % if refcount_needed:

            -------------
            -- Release --
            -------------

            overriding procedure Release (Self : in out ${t.record_type}) is
            begin
               % for arg in constructor_args:
                  ## Non-user fields are parse fields, and we store node
                  ## builders for them, which are refcounted.
                  % if arg.type.is_refcounted:
                     Dec_Ref (Self.${arg.codegen_name});
                  % endif
               % endfor
            end Release;
         % endif

         function ${t.synth_constructor}
           % if constructor_args:
           ${ada_block_with_parens(
              [
                 f"{arg.codegen_name} : {arg.type.name}"
                 for arg in constructor_args
              ],
              12,
              separator=";",
           )}
           % endif
           return ${t.name}
         is
            Builder : constant ${t.access_type} := new ${t.record_type};
         begin
            Builder.Ref_Count := 1;
            % for arg in constructor_args:
               Builder.${arg.codegen_name} := ${arg.codegen_name};
               % if arg.type.is_refcounted:
                  Inc_Ref (Builder.${arg.codegen_name});
               % endif
            % endfor
            return Node_Builder_Type (Builder);
         end ${t.synth_constructor};
      % endif
   % endfor

   ## Env metadata's body

   ----------------------
   -- Compare_Metadata --
   ----------------------

   --  Deactivate "not referenced" warnings because if the metadata struct has
   --  no fields, formals and temporaries won't be referenced in the two
   --  following functions.
   pragma Warnings (Off, "referenced");
   function Compare_Metadata (L, R : ${T.env_md.name}) return Boolean is
   begin
      % for field in T.env_md.get_fields():
         % if field.use_in_equality:
            if L.${field.names.codegen} /= R.${field.names.codegen} then
               return False;
            end if;
         % endif
      % endfor
      return True;
   end Compare_Metadata;

   ----------
   -- Hash --
   ----------

   function Hash (Self : ${T.env_md.name}) return Hash_Type is
      Ret : Hash_Type := Langkit_Support.Hashes.Initial_Hash;
   begin
      % for field in T.env_md.get_fields():
         % if field.use_in_equality:
            % if field.type.is_bool_type:
               Ret := Combine
                 (Ret, Hash_Type (Boolean'Pos (Self.${field.names.codegen})));
            % else:
               Ret := Combine (Ret, Hash (Self.${field.names.codegen}));
            % endif
         % endif
      % endfor
      return Ret;
   end Hash;
   pragma Warnings (On, "referenced");

   -------------
   -- Combine --
   -------------

   function Combine
     (L, R : ${T.env_md.name}) return ${T.env_md.name}
   is
      % if T.env_md.is_empty:
      pragma Unreferenced (L, R);
      % endif
      Ret : ${T.env_md.name} := ${T.env_md.nullexpr};
   begin
      % for field in T.env_md.get_fields():
         % if field.type.is_bool_type:
         Ret.${field.names.codegen} :=
           L.${field.names.codegen} or R.${field.names.codegen};
         % else:
         if L.${field.names.codegen} = ${field.type.nullexpr} then
            Ret.${field.names.codegen} := R.${field.names.codegen};
         elsif R.${field.names.codegen} = ${field.type.nullexpr} then
            Ret.${field.names.codegen} := L.${field.names.codegen};
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
     (Parent            : Lexical_Env;
      Node              : ${T.root_node.name};
      Sym_Table         : Symbol_Table;
      Transitive_Parent : Boolean := False) return Lexical_Env
   is
      Unit : constant Internal_Unit :=
        (if Node = null then null else Node.Unit);
   begin
      return Result : Lexical_Env := AST_Envs.Create_Lexical_Env
        (Parent, Node, Transitive_Parent, Sym_Table, Convert_Unit (Unit))
      do
         if Unit /= null then
            Register_Destroyable (Unit, AST_Envs.Unwrap (Result.Env));
         end if;
      end return;
   end Create_Static_Lexical_Env;

   ---------
   -- Get --
   ---------

   function Get
     (Self  : ${T.root_node.name};
      A     : AST_Envs.Entity_Array;
      Index : Integer) return ${root_entity.name}
   is
      function Length (A : AST_Envs.Entity_Array) return Natural
      is (A'Length);

      function Get
        (A     : AST_Envs.Entity_Array;
         Index : Integer) return ${root_entity.name}
      is (A (Index + 1)); --  A is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => AST_Envs.Entity,
         Sequence_Type => AST_Envs.Entity_Array,
         Length        => Length,
         Get           => Get);
      Result : ${root_entity.name};
   begin
      if Relative_Get (A, Index, Result) then
         return Result;
      else
         Raise_Property_Exception
           (Self, Property_Error'Identity, "out-of-bounds array access");
      end if;
   end Get;

   -----------
   -- Group --
   -----------

   function Group
     (Envs   : ${T.LexicalEnv.array.name};
      Env_Md : ${T.env_md.name} := No_Metadata) return ${T.LexicalEnv.name}
   is (AST_Envs.Group (AST_Envs.Lexical_Env_Array (Envs.Items), Env_Md));

   % for astnode in ctx.node_types:
       ${astnode_types.body_decl(astnode)}
   % endfor

   ------------------
   -- Children_Env --
   ------------------

   function Children_Env
     (Node   : ${T.root_node.name};
      E_Info : ${T.EntityInfo.name} := ${T.EntityInfo.nullexpr})
      return Lexical_Env
   is (AST_Envs.Rebind_Env (Node.Self_Env, E_Info));

   --------------
   -- Node_Env --
   --------------

   function Node_Env
     (Node   : ${T.root_node.name};
      E_Info : ${T.EntityInfo.name} := ${T.EntityInfo.nullexpr})
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
            return (if Is_Null (Parent)
                    then Empty_Env
                    else Parent);
         end Get_Parent_Env;

      begin
         <% node_types = [n.ada_kind_name for n in ctx.node_types
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
      Result   : constant Lexical_Env :=
        AST_Envs.Rebind_Env (Base_Env, E_Info);
   begin
      Dec_Ref (Base_Env);
      return Result;
   end Node_Env;

   ------------
   -- Parent --
   ------------

   function Parent
     (Node   : ${T.root_node.name};
      E_Info : ${T.EntityInfo.name} := ${T.EntityInfo.nullexpr})
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
      E_Info    : ${T.EntityInfo.name} := ${T.EntityInfo.nullexpr})
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
      E_Info : ${T.EntityInfo.name} := ${T.EntityInfo.nullexpr})
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
           new String'(Image (Short_Text_Image (Node)) & "." & Field);
      end Assign;

      K : constant ${T.node_kind} := Node.Kind;

      pragma Warnings (On, "referenced");

   begin
      <%
          def get_actions(astnode, node_expr):
              return "\n".join(
                  f"Assign ({node_expr},"
                  f"        {node_expr}.{field.names.codegen},"
                  f"        {ascii_repr(str(field.original_name))});"
                  for field in astnode.get_user_fields(include_inherited=False)
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

   --------------------------
   -- Initialization_Error --
   --------------------------

   function Initialization_Error
     (Exc : Ada.Exceptions.Exception_Occurrence)
      return Error_Initialization_State
   is
      use Ada.Exceptions;
   begin
      if
      % for i, exc in enumerate(ctx.property_exceptions):
         ${"" if i == 0 else "elsif"}
            Exception_Identity (Exc) = ${exc}'Identity
         then
            return Raised_${exc};
      % endfor
      else
         raise Program_Error;
      end if;
   end Initialization_Error;

   ----------------------------------
   -- Reraise_Initialization_Error --
   ----------------------------------

   procedure Reraise_Initialization_Error
     (Node    : ${T.root_node.name};
      State   : Error_Initialization_State;
      Message : String)
   is
      Exc : Ada.Exceptions.Exception_Id;
   begin
      case State is
         % for exc in ctx.property_exceptions:
            when Raised_${exc} =>
               Exc := ${exc}'Identity;
         % endfor
      end case;
      Raise_Property_Exception (Node, Exc, Message);
   end Reraise_Initialization_Error;

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
   begin
      return Create_String (Res);
   end Full_Sloc_Image;

   ---------------------------------
   -- Completion_Item_Kind_To_Int --
   ---------------------------------

   function Completion_Item_Kind_To_Int
     (Node : ${T.root_node.name};
      Kind : Completion_Item_Kind)
      return Integer
   is
      pragma Unreferenced (Node);
   begin
      return Completion_Item_Kind'Enum_Rep (Kind) + 1;
   end Completion_Item_Kind_To_Int;

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
      return ${T.root_node.kwless_raw_name}_P_Can_Reach (El, From);
   end Can_Reach;

   -----------------
   -- Hash_Entity --
   -----------------

   function Hash_Entity (Self : ${root_entity.name}) return Hash_Type is
   begin
      return Combine
        ((Hash (Self.Node), Hash (Self.Info.Rebindings), Hash (Self.Info.Md)));
   end Hash_Entity;

   --------------------
   -- Compare_Entity --
   --------------------

   function Compare_Entity (Left, Right : ${root_entity.name}) return Boolean
   is
   begin
      return Left.Node = Right.Node
             and then Left.Info.Rebindings = Right.Info.Rebindings
             and then Compare_Metadata (Left.Info.Md, Right.Info.Md);
   end Compare_Entity;

   --------------------------------
   -- Create_Dynamic_Lexical_Env --
   --------------------------------

   function Create_Dynamic_Lexical_Env
     (Self              : ${T.root_node.name};
      Assocs_Getter     : Inner_Env_Assocs_Resolver;
      Assoc_Resolver    : Entity_Resolver;
      Transitive_Parent : Boolean;
      Sym_Table         : Symbol_Table) return Lexical_Env
   is
      Unit : constant Internal_Unit := Self.Unit;
   begin
      --  This restriction is necessary to avoid relocation issues when
      --  Self.Self_Env is terminated.
      if AST_Envs.Is_Foreign_Strict (Self.Self_Env, Self) then
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            "cannot create a dynamic lexical env when Self.Self_Env is"
            & " foreign");
      end if;

      return Result : constant Lexical_Env :=
        AST_Envs.Create_Dynamic_Lexical_Env
          (Parent            => Null_Lexical_Env,
           Node              => Self,
           Transitive_Parent => Transitive_Parent,
           Owner             => Convert_Unit (Unit),
           Assocs_Getter     => Assocs_Getter,
           Assoc_Resolver    => Assoc_Resolver,
           Sym_Table         => Sym_Table)
      do
         --  Since dynamic lexical environments can only be created in lazy
         --  field initializers, it is fine to tie Result's lifetime to the
         --  its owning unit's lifetime.
         Register_Destroyable (Unit, AST_Envs.Unwrap (Result));
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
         return (if S = No_Symbol
                 then "None"
                 else Image (S, With_Quotes => True));
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (C : Character_Type) return String is
         C_Str : constant Text_Type := (1 => C);
      begin
         return "'" & Image (C_Str) & "'";
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (S : String_Type) return String is
      begin
         return Image (S.Content, With_Quotes => True);
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Env : Lexical_Env) return String is
      begin
         case Env.Kind is
         when Static_Primary =>
            return "<LexicalEnv static-primary for "
                   & Trace_Image (AST_Envs.Env_Node (Env)) & ">";
         when others =>
            return "<LexicalEnv synthetic>";
         end case;
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (R : Env_Rebindings) return String is
      begin
         return Image (AST_Envs.Text_Image (R));
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

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Self : Ref_Categories) return String is
         Result : Unbounded_String;
         First  : Boolean := True;
      begin
         Append (Result, "RefCategories(");
         for C in Ref_Category loop
            if Self (C) then
               if First then
                  First := False;
               else
                  Append (Result, ", ");
               end if;
               Append (Result, C'Image);
            end if;
         end loop;
         Append (Result, ")");
         return To_String (Result);
      end Trace_Image;

   % endif

   % for struct_type in ctx.struct_types:
   ${struct_types.body(struct_type)}
   % endfor

   ${astnode_types.logic_helpers()}

   % for astnode in ctx.node_types:
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

      --  Synthetic list have their array of children dynamically allocated:
      --  the children themselves are gone, but not the array: free it now.
      if Is_List_Node (Node.Kind) then
         Free_Synthetic_List_Children (Node);
      end if;

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
                    for cls in ctx.node_types if not cls.abstract)});

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
         Entity_Vars.Reset (LV'Unrestricted_Access);
         Entity_Vars.Destroy (LV);
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
                      result.append(
                          f'{free_subp} ({node_expr}.{field.names.codegen});'
                      )

              return '\n'.join(result)
      %>
      ${ctx.generate_actions_for_hierarchy(
         node_var='Node',
         kind_var='K',
         actions_for_node=get_actions,
         unref_if_empty=["Reset_Logic_Var"]
      )}
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
         return To_Symbol
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
         Ast_Root                     => null,
         Filename                     => Normalized_Filename,
         Charset                      => To_Unbounded_String (Charset),
         TDH                          => <>,
         Diagnostics                  => <>,
         Rule                         => Rule,
         Ast_Mem_Pool                 => No_Pool,
         Destroyables                 => Destroyable_Vectors.Empty_Vector,
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
      Initialize
        (Unit.TDH, Context.Symbols, Unit.all'Address, Context.Tab_Stop);
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
     (Context : Internal_Context; Filename : String) return Virtual_File is
   begin
      return Langkit_Support.Internal.Analysis.Normalized_Unit_Filename
               (Context.Filenames, Filename);
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

      procedure Reset_Refd_Envs (Node : ${T.root_node.name});

      -------------------------
      -- Recompute_Refd_Envs --
      -------------------------

      procedure Reset_Refd_Envs (Node : ${T.root_node.name}) is
      begin
         if Node = null then
            return;
         end if;
         AST_Envs.Reset_Referenced_Envs (Node.Self_Env);
         for I in 1 .. Children_Count (Node) loop
            Reset_Refd_Envs (Child (Node, I));
         end loop;
      end Reset_Refd_Envs;

   begin
      Reset_Refd_Envs (Unit.Ast_Root);
   end Reset_Envs;

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

   ----------------
   -- Do_Parsing --
   ----------------

   procedure Do_Parsing
     (Unit   : Internal_Unit;
      Input  : Langkit_Support.Internal.Analysis.Lexer_Input;
      Result : out Reparsed_Unit)
   is
      Context  : constant Internal_Context := Unit.Context;
      Unit_TDH : constant Token_Data_Handler_Access := Token_Data (Unit);

      Saved_TDH : aliased Token_Data_Handler;
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

      Result :=
        (Present      => True,
         TDH          => <>,
         Diagnostics  => <>,
         Ast_Mem_Pool => <>,
         Ast_Root     => Langkit_Support.Internal.Analysis.No_Internal_Node);

      Move (Saved_TDH, Unit_TDH.all);
      Initialize (Unit_TDH.all,
                  Saved_TDH.Symbols,
                  Unit.all'Address,
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

      declare
         Same_Contents : Boolean;
      begin
         Init_Parser
           (Input,
            Context.With_Trivia,
            Unit,
            Unit_TDH,
            Unit.Context.Parser,
            Saved_TDH'Access,
            Same_Contents);
         if Same_Contents then
            Rotate_TDH;
            Free (Result.TDH);
            Result := (Present => False);
            return;
         end if;
      end;

      --  If we could run the lexer, run the parser and get the root node

      if Unit_TDH.Source_Buffer /= null then
         Result.Ast_Mem_Pool := Create;
         Unit.Context.Parser.Mem_Pool := Result.Ast_Mem_Pool;
         declare
            Ast_Root : constant ${T.root_node.name} :=
              ${T.root_node.name}
                (Parse (Unit.Context.Parser, Rule => Unit.Rule));
            function "+" is new Ada.Unchecked_Conversion
              (${T.root_node.name},
               Langkit_Support.Internal.Analysis.Internal_Node);
         begin
            Result.Ast_Root := +Ast_Root;
         end;
      end if;

      --  Forward token data and diagnostics to the returned unit

      Rotate_TDH;
      Result.Diagnostics.Append_Vector (Unit.Context.Parser.Diagnostics);
   end Do_Parsing;

   --------------------------
   -- Update_After_Reparse --
   --------------------------

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit) is
   begin
      --  If reparsing was skipped (same buffer as before), there is nothing to
      --  update.

      if not Reparsed.Present then
         return;
      end if;

      --  Remove the `symbol -> AST node` associations for Unit's nodes in
      --  foreign lexical environments.
      Remove_Exiled_Entries (Unit);

      --  Remove the named envs that Unit created
      declare
         Named_Envs_Needing_Update : NED_Maps.Map;
      begin
         Remove_Named_Envs (Unit, Named_Envs_Needing_Update);
         Update_Named_Envs (Unit.Context, Named_Envs_Needing_Update);
      end;

      --  Explicitly clear the env caches of this unit while it is still fully
      --  alive to make sure that ``Lexical_Env_Cache_Updated`` accesses valid
      --  data. Otherwise the env caches end up being cleared during the call
      --  to ``Destroy_Unit_Destroyables`` where the unit is already partially
      --  destroyed.
      Reset_Envs_Caches (Unit);

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
      --  As an optimization, invalidate env caches only if PLE has run on this
      --  unit (U1) before: if it's the case, then envs in another unit (U2)
      --  may have cached env lookup results that would be different with the
      --  new version of U1.
      Invalidate_Caches
        (Context         => Unit.Context,
         Invalidate_Envs => (for some B of Unit.Env_Populated_Roots => B));

      --  Likewise for token data
      Free (Unit.TDH);
      Move (Unit.TDH, Reparsed.TDH);

      --  Reparsing will invalidate all lexical environments related to this
      --  unit, so destroy all related rebindings as well. This browses AST
      --  nodes, so we have to do this before destroying the old AST nodes
      --  pool.
      Destroy_Rebindings (Unit.Rebindings'Access);

      --  Destroy the old AST node and replace it by the new one
      if Unit.Ast_Root /= null then
         Destroy (Unit.Ast_Root);
      end if;
      declare
         function "+" is new Ada.Unchecked_Conversion
           (Langkit_Support.Internal.Analysis.Internal_Node,
            ${T.root_node.name});
      begin
         Unit.Ast_Root := +Reparsed.Ast_Root;
      end;

      --  Likewise for memory pools
      Free (Unit.Ast_Mem_Pool);
      Unit.Ast_Mem_Pool := Reparsed.Ast_Mem_Pool;
      Reparsed.Ast_Mem_Pool := No_Pool;

      --  Increment unit version number to invalidate caches and stale node
      --  reference. Also propagate it to the TDH.
      Unit.Unit_Version := Unit.Unit_Version + 1;
      Unit.TDH.Version := Unit.Unit_Version;

      --  Compute the PLE_Roots_Starting_Token table

      Unit.PLE_Roots_Starting_Token.Clear;
      % if ctx.ple_unit_root:
         if Unit.Ast_Root /= null
            and then Unit.Ast_Root.Kind
                     = ${ctx.ple_unit_root.list.ada_kind_name}
         then
            for I in 1 .. Children_Count (Unit.Ast_Root) loop
               Unit.PLE_Roots_Starting_Token.Append
                 (Child (Unit.Ast_Root, I).Token_Start_Index);
            end loop;
         end if;
      % endif

      --  Update all the lexical envs entries affected by the reparse

      declare
         Unit_Name     : constant String := +Unit.Filename.Base_Name;
         Context       : constant Internal_Context := Unit.Context;
         Foreign_Nodes : ${T.root_node.name}_Vectors.Vector :=
           ${T.root_node.name}_Vectors.Empty_Vector;

         Saved_In_Populate_Lexical_Env : constant Boolean :=
           Context.In_Populate_Lexical_Env;
         Saved_Env_Populated_Roots     : constant Boolean_Vectors.Vector :=
           Unit.Env_Populated_Roots;
      begin
         Context.In_Populate_Lexical_Env := True;
         if Main_Trace.Active then
            Main_Trace.Trace
              ("Updating lexical envs for " & Unit_Name & " after reparse");
            Main_Trace.Increase_Indent;
         end if;

         --  Collect all nodes that are foreign in this Unit's lexical envs.
         --  Exclude them from the corresponding lists of exiled entries.
         Extract_Foreign_Nodes (Unit, Foreign_Nodes);

         --  Temporarily reset Env_Populated_Roots so that Populate_Lexical_Env
         --  accepts to do its work on reparsed trees.

         Unit.Env_Populated_Roots := Boolean_Vectors.Empty_Vector;

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

         --  Re-populate all PLE roots that were requested so far for this
         --  unit. In the case where the unit has no PLE root, run PLE on the
         --  whole unit iff it was requested on at least one PLE root.

         declare
            function At_Least_One_Root_Populated return Boolean
            is (for some B of Saved_Env_Populated_Roots => B);
         begin
            % if ctx.ple_unit_root:
               if not Unit.PLE_Roots_Starting_Token.Is_Empty then
                  for I in 1 .. Children_Count (Unit.Ast_Root) loop
                     if Saved_Env_Populated_Roots.Last_Index >= I
                        and then Saved_Env_Populated_Roots.Get (I)
                     then
                        Populate_Lexical_Env (Unit, I);
                     end if;
                  end loop;
               elsif At_Least_One_Root_Populated then
                  Populate_Lexical_Env (Unit, 1);
               end if;
            % else:
               if At_Least_One_Root_Populated then
                  Populate_Lexical_Env (Unit);
               end if;
            % endif
         end;

         --  Restore the unit's original Env_Populated_Roots flags

         Unit.Env_Populated_Roots.Destroy;
         Unit.Env_Populated_Roots := Saved_Env_Populated_Roots;

         Context.In_Populate_Lexical_Env := Saved_In_Populate_Lexical_Env;
         if Main_Trace.Is_Active then
            Main_Trace.Decrease_Indent;
         end if;
      end;

      --  Let extension potentially add new diagnostics

      ${exts.include_extension(ctx.ext("analysis", "post_parsing"))}

      --  Make sure they are sorted afterwards for a better user experience

      Sort (Unit.Diagnostics);
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
         AST_Envs.Remove (EE.Env, Thin (EE.Key), EE.Node);

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
         AST_Envs.Remove
           (EE.Named_Env.Env_With_Precedence, Thin (EE.Key), EE.Node);
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
         EE.Named_Env.Foreign_Envs.Delete (AST_Envs.Env_Node (EE.Env));
      end loop;
      Unit.Exiled_Envs.Clear;

      --  Remove named envs that this unit created
      for NE of Unit.Named_Envs loop
         declare
            NED_Access : constant Named_Env_Descriptor_Access :=
               Unit.Context.Named_Envs.Element (NE.Name);
            NED        : Named_Env_Descriptor renames NED_Access.all;
         begin
            NED.Envs.Delete (AST_Envs.Env_Node (NE.Env));

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
   begin
      return Create_String (Text (Node));
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
               AST_Envs.Unwrap (R.Old_Env).Rebindings_Pool.Delete (R.New_Env);
            else
               Unregister (R, R.Parent.Children);
            end if;

            --  In all cases it's registered in Old_Env's and New_Env's units
            Recurse (R);
         end;
      end loop;
   end Destroy_Rebindings;

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
     (Self    : ${T.root_node.name};
      Context : Internal_Context;
      S       : ${T.String.name}) return Symbol_Type is
   begin
      return (if S.Length > 0
              then Lookup_Symbol (Context, S.Content)
              else No_Symbol);
   exception
      when Exc : Invalid_Symbol_Error =>
         Raise_Property_Exception
           (Self,
            Property_Error'Identity,
            Ada.Exceptions.Exception_Message (Exc));
   end String_To_Symbol;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : String_Type) is
   begin
      if Self.Ref_Count >= 0 then
         Self.Ref_Count := Self.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out String_Type) is
   begin
      if Self = null or else Self.Ref_Count < 0 then
         return;
      end if;

      if Self.Ref_Count = 1 then
         Free (Self);
      else
         Self.Ref_Count := Self.Ref_Count - 1;
         Self := null;
      end if;
   end Dec_Ref;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Content : Text_Type) return String_Type is
   begin
      return Result : constant String_Type := new String_Record'
        (Length    => Content'Length,
         Ref_Count => 1,
         Content   => Content);
   end Create_String;

   -------------------
   -- Create_String --
   -------------------

   function Create_String (Content : Unbounded_Text_Type) return String_Type is
      S : Big_Wide_Wide_String_Access;
      L : Natural;
   begin
      Get_Wide_Wide_String (Content, S, L);
      return Create_String (S.all (1 .. L));
   end Create_String;

   -------------------
   -- Concat_String --
   -------------------

   function Concat_String (Left, Right : String_Type) return String_Type is
   begin
      return Result : constant String_Type :=
        new String_Record (Length => Left.Length + Right.Length)
      do
         Result.Ref_Count := 1;
         Result.Content (1 .. Left.Length) := Left.Content;
         Result.Content (Left.Length + 1 .. Result.Length) := Right.Content;
      end return;
   end Concat_String;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (Left, Right : String_Type) return Boolean is
   begin
      return Left.Content = Right.Content;
   end Equivalent;

begin
   No_Big_Integer.Value.Set (0);
end ${ada_lib_name}.Implementation;
