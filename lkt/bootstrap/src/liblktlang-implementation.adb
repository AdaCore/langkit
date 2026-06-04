










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

with GNAT.Task_Lock;
with GNAT.Traceback.Symbolic;

with GNATCOLL.Traces;

with Liblktlang_Support.Adalog.Debug;
with Liblktlang_Support.Generic_API.Analysis;
with Liblktlang_Support.Generic_API.Introspection;
with Liblktlang_Support.Hashes; use Liblktlang_Support.Hashes;
with Liblktlang_Support.Images; use Liblktlang_Support.Images;
with Liblktlang_Support.Names;  use Liblktlang_Support.Names;
with Liblktlang_Support.Relative_Get;

with Liblktlang.Private_Converters;
use Liblktlang.Private_Converters;

pragma Warnings (Off, "referenced");

       with Liblktlang_Support.Errors;
       with Liblktlang.Default_Provider;
       with Liblktlang.Impl_0;
         use Liblktlang.Impl_0;
       with Liblktlang.Implementation.Extensions;
         use Liblktlang.Implementation.Extensions;

pragma Warnings (On, "referenced");



package body Liblktlang.Implementation is

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

   procedure Reset_Envs_Caches
     (Unit            : Internal_Unit;
      For_Destruction : Boolean := False);
   --  Reset the env caches of all lexical environments created for ``Unit``.
   --  Set ``For_Destruction`` to True if the envs in that unit are not be used
   --  anymore.

   procedure Destroy (Env : in out Lexical_Env_Access);

   function Snaps_At_Start (Self : Bare_Lkt_Node) return Boolean;
   function Snaps_At_End (Self : Bare_Lkt_Node) return Boolean;

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
     (Node        : Liblktlang_Support.Generic_API.Analysis.Lk_Node;
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
         declare
            Raw_Text : constant Text_Type := (case Index is
            
               when Precomputed_Sym_Abstract => "abstract",
               when Precomputed_Sym_All => "all",
               when Precomputed_Sym_Analysisunit => "AnalysisUnit",
               when Precomputed_Sym_Any => "any",
               when Precomputed_Sym_Array => "Array",
               when Precomputed_Sym_As => "as",
               when Precomputed_Sym_As_Bool => "as_bool",
               when Precomputed_Sym_Astlist => "ASTList",
               when Precomputed_Sym_Basictrait => "BasicTrait",
               when Precomputed_Sym_Bigint => "BigInt",
               when Precomputed_Sym_Bool => "Bool",
               when Precomputed_Sym_Builder => "builder",
               when Precomputed_Sym_Call => "__call__",
               when Precomputed_Sym_Char => "Char",
               when Precomputed_Sym_Dedent => "dedent",
               when Precomputed_Sym_Default_Metadata => "__default_metadata",
               when Precomputed_Sym_Domain => "domain",
               when Precomputed_Sym_Dont_Skip => "dont_skip",
               when Precomputed_Sym_Entity => "Entity",
               when Precomputed_Sym_Env_Spec => "env_spec",
               when Precomputed_Sym_Envaction => "EnvAction",
               when Precomputed_Sym_Equation => "Equation",
               when Precomputed_Sym_Errornode => "ErrorNode",
               when Precomputed_Sym_False => "false",
               when Precomputed_Sym_Family => "family",
               when Precomputed_Sym_Ignore_Constructor_Arg => "ignore_constructor_arg",
               when Precomputed_Sym_Indent => "indent",
               when Precomputed_Sym_Indexable => "Indexable",
               when Precomputed_Sym_Int => "Int",
               when Precomputed_Sym_Invalid => "invalid",
               when Precomputed_Sym_Iterator => "Iterator",
               when Precomputed_Sym_Keep => "keep",
               when Precomputed_Sym_Lazy => "lazy",
               when Precomputed_Sym_List => "list",
               when Precomputed_Sym_List_Elements => "list_elements",
               when Precomputed_Sym_Logicvar => "LogicVar",
               when Precomputed_Sym_Metadata => "Metadata",
               when Precomputed_Sym_Metadata_49 => "metadata",
               when Precomputed_Sym_Newline => "newline",
               when Precomputed_Sym_No_Case => "no_case",
               when Precomputed_Sym_Node => "Node",
               when Precomputed_Sym_Node_52 => "node",
               when Precomputed_Sym_Nodebuilder => "NodeBuilder",
               when Precomputed_Sym_Null_Field => "null_field",
               when Precomputed_Sym_Nullable => "nullable",
               when Precomputed_Sym_Open => "open",
               when Precomputed_Sym_Parse_Field => "parse_field",
               when Precomputed_Sym_Pick => "pick",
               when Precomputed_Sym_Prelude => "<prelude>",
               when Precomputed_Sym_Previous_Token => "previous_token",
               when Precomputed_Sym_Property => "property",
               when Precomputed_Sym_Propertyerror => "PropertyError",
               when Precomputed_Sym_Qualifier => "qualifier",
               when Precomputed_Sym_Regexp => "Regexp",
               when Precomputed_Sym_Root_Node => "root_node",
               when Precomputed_Sym_Rootnode => "RootNode__",
               when Precomputed_Sym_Self => "self",
               when Precomputed_Sym_Send => "send",
               when Precomputed_Sym_Skip => "skip",
               when Precomputed_Sym_Stop_Cut => "stop_cut",
               when Precomputed_Sym_Stream => "Stream",
               when Precomputed_Sym_String => "String",
               when Precomputed_Sym_Super => "super",
               when Precomputed_Sym_Symbol => "Symbol",
               when Precomputed_Sym_Synthetic => "synthetic",
               when Precomputed_Sym_T => "T",
               when Precomputed_Sym_Tokennode => "TokenNode",
               when Precomputed_Sym_True => "true",
               when Precomputed_Sym_Update => "update",
               when Precomputed_Sym_User_Metadata => "__user_metadata",
               when Precomputed_Sym_Var => "var",
               when Precomputed_Sym_With_Dynvars => "with_dynvars"
            );

            Symbol : constant Symbolization_Result :=
                  Create_Symbol (Raw_Text)
            ;
         begin
            if Symbol.Success then
               return Symbol.Symbol;
            else
               raise Program_Error with
                 "Cannot canonicalize symbol literal: " & Image (Raw_Text);
            end if;
         end;
   end Precomputed_Symbol;

   ----------------------------
   -- Construct_Entity_Array --
   ----------------------------

   function Construct_Entity_Array
     (V : AST_Envs.Entity_Vectors.Vector) return Internal_Entity_Array_Access
   is
      Ret : Internal_Entity_Array_Access :=
        Create_Internal_Entity_Array (V.Length);
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

   function Image (Self : Symbol_Type) return String_Type is
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

         if Context.Unit_Provider = null then
            Context.Unit_Provider := Liblktlang.Default_Provider.Create_Lkt;
         end if;

      Initialize (Context.Parser);

      Context.Discard_Errors_In_Populate_Lexical_Env := True;
      Context.Logic_Resolution_Timeout :=
        Liblktlang_Support.Adalog.Default_Timeout_Ticks_Number;
      Context.In_Populate_Lexical_Env := False;
      Context.Cache_Version := 0;
      Context.Reparse_Cache_Version := 0;

      Context.Templates_Unit := No_Analysis_Unit;

      Context.Available_Rebindings := Env_Rebindings_Vectors.Empty_Vector;

      

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
      Input             : Liblktlang_Support.Internal.Analysis.Lexer_Input;
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
      Refined_Input  : Liblktlang_Support.Internal.Analysis.Lexer_Input := Input;

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
      Input : constant Liblktlang_Support.Internal.Analysis.Lexer_Input :=
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
      Input : constant Liblktlang_Support.Internal.Analysis.Lexer_Input :=
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
         when Liblktlang_Support.Errors.Property_Error =>
            raise Invalid_Unit_Name_Error with
               "Invalid unit name: " & Image (Name, With_Quotes => True)
               & " (" & Analysis_Unit_Kind'Image (Kind) & ")";
      end;
   end Get_From_Provider;


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
        Liblktlang_Support.Internal.Analysis.Empty_Virtual_File_Cache;

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

   procedure Reset_Envs_Caches
     (Unit            : Internal_Unit;
      For_Destruction : Boolean := False)
   is
      procedure Internal (Node : Bare_Lkt_Node);
      --  Reset env caches in ``Node`` and then in its children recursively

      Generic_Unit : constant Generic_Unit_Ptr := Convert_Unit (Unit);

      --------------
      -- Internal --
      --------------

      procedure Internal (Node : Bare_Lkt_Node) is
      begin
         if Node = null then
            return;
         end if;
         --  Make sure to only reset caches of envs belonging to this unit
         if Node.Self_Env.Owner = Generic_Unit then
            AST_Envs.Reset_Caches (Node.Self_Env, For_Destruction);
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
         := 1
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
            & " unit: " & Basename (Unit));
         Main_Trace.Increase_Indent;
      end if;

      --  Fetch the node on which to run PLE: it's the unit root node, or one
      --  of its children if PLE roots are enabled and the unit has a list of
      --  PLE roots. Then run PLE itself.

      declare
         PLE_Root : Bare_Lkt_Node := Unit.Ast_Root;
      begin

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

   procedure Populate_Lexical_Env_For_Unit (Node : Bare_Lkt_Node) is
      Root  : Bare_Lkt_Node;
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

   function Root (Unit : Internal_Unit) return Bare_Lkt_Node is
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
     (Node  : Bare_Lkt_Node;
      Root  : out Bare_Lkt_Node;
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
     (Node : Bare_Lkt_Node) return Bare_Lkt_Node
   is
      Root        : Bare_Lkt_Node;
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
      Node     : constant Bare_Lkt_Node := Unit.Ast_Root;
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

      procedure Internal (Current : Bare_Lkt_Node) is
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

         for Child of Internal_Bare_Lkt_Node_Array'(Children (Current))
         loop
            Internal (Child);
         end loop;
      end Internal;
      --  This procedure implements the main recursive logic of dumping the
      --  environments.
   begin
      Internal (Bare_Lkt_Node (Node));
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
      Reset_Envs_Caches (Unit, For_Destruction => True);

      Unit.PLE_Roots_Starting_Token.Destroy;
      Unit.Env_Populated_Roots.Destroy;

      Unit.Exiled_Entries.Destroy;
      Unit.Foreign_Nodes.Destroy;
      Unit.Exiled_Entries_In_NED.Destroy;
      Unit.Exiled_Envs.Destroy;
      Unit.Named_Envs.Destroy;

         Destroy (Unit.Memoization_Map);

      Destroy_Rebindings (Unit.Rebindings'Access);
      Unit.Rebindings.Destroy;

      if Unit.Ast_Root /= null then
         Destroy (Unit.Ast_Root);
      end if;

      Free (Unit.TDH);
      Free (Unit.Ast_Mem_Pool);
      Destroy_Unit_Destroyables (Unit);
      Destroyable_Vectors.Destroy (Unit.Destroyables);
      

      Free (Unit);
   end Destroy;

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : Bare_Lkt_Node) return Boolean is
   begin
      return Is_Token_Node (Node.Kind);
   end Is_Token_Node;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic (Node : Bare_Lkt_Node) return Boolean is
   begin
      return Node.Kind in Synthetic_Nodes;
   end Is_Synthetic;

   ------------------------------
   -- Raise_Property_Exception --
   ------------------------------

   procedure Raise_Property_Exception
     (Node    : Bare_Lkt_Node;
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

      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Bare_Id_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Id
   is
      function Absolute_Get
        (T : Bare_Id_Array_Access; Index : Integer)
         return Bare_Id
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Bare_Id,
         Sequence_Type => Bare_Id_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Bare_Id;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Bare_Lkt_Node;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Bare_Id_Array_Access) return Bare_Id_Array_Access is
      Ret : Bare_Id_Array_Access := Create_Bare_Id_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Bare_Id_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Bare_Id_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Bare_Id_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Bare_Id_Array (Items_Count : Natural) return Bare_Id_Array_Access
   is (if Items_Count = 0
       then No_Bare_Id_Array_Type
       else new Bare_Id_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Bare_Id_Array
     (Items : Internal_Bare_Id_Array) return Bare_Id_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Bare_Id_Array_Type;
      end if;

      return new Bare_Id_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Bare_Id_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (L.Items (I) = R.Items (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Bare_Id_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Bare_Lkt_Node_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Lkt_Node
   is
      function Absolute_Get
        (T : Bare_Lkt_Node_Array_Access; Index : Integer)
         return Bare_Lkt_Node
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Bare_Lkt_Node,
         Sequence_Type => Bare_Lkt_Node_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Bare_Lkt_Node;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Bare_Lkt_Node;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Bare_Lkt_Node_Array_Access) return Bare_Lkt_Node_Array_Access is
      Ret : Bare_Lkt_Node_Array_Access := Create_Bare_Lkt_Node_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Bare_Lkt_Node_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Bare_Lkt_Node_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Bare_Lkt_Node_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Bare_Lkt_Node_Array (Items_Count : Natural) return Bare_Lkt_Node_Array_Access
   is (if Items_Count = 0
       then No_Bare_Lkt_Node_Array_Type
       else new Bare_Lkt_Node_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Bare_Lkt_Node_Array
     (Items : Internal_Bare_Lkt_Node_Array) return Bare_Lkt_Node_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Bare_Lkt_Node_Array_Type;
      end if;

      return new Bare_Lkt_Node_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Bare_Lkt_Node_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (L.Items (I) = R.Items (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Bare_Lkt_Node_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Bare_Type_Ref_Node_Builder_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Type_Ref_Node_Builder
   is
      function Absolute_Get
        (T : Bare_Type_Ref_Node_Builder_Array_Access; Index : Integer)
         return Bare_Type_Ref_Node_Builder
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Bare_Type_Ref_Node_Builder,
         Sequence_Type => Bare_Type_Ref_Node_Builder_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Bare_Type_Ref_Node_Builder;
   begin
      if Relative_Get (T, Index, Result) then
            Inc_Ref (Result);
         return Result;
      elsif Or_Null then
         return null;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Bare_Type_Ref_Node_Builder_Array_Access) return Bare_Type_Ref_Node_Builder_Array_Access is
      Ret : Bare_Type_Ref_Node_Builder_Array_Access := Create_Bare_Type_Ref_Node_Builder_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
         for Item of Ret.Items loop
            Inc_Ref (Item);
         end loop;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Bare_Type_Ref_Node_Builder_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Bare_Type_Ref_Node_Builder_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Bare_Type_Ref_Node_Builder_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
            for Item of T.Items loop
               Dec_Ref (Item);
            end loop;
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Bare_Type_Ref_Node_Builder_Array (Items_Count : Natural) return Bare_Type_Ref_Node_Builder_Array_Access
   is (if Items_Count = 0
       then No_Bare_Type_Ref_Node_Builder_Array_Type
       else new Bare_Type_Ref_Node_Builder_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Bare_Type_Ref_Node_Builder_Array
     (Items : Internal_Bare_Type_Ref_Node_Builder_Array) return Bare_Type_Ref_Node_Builder_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Bare_Type_Ref_Node_Builder_Array_Type;
      end if;

         for El of Items loop
            Inc_Ref (El);
         end loop;
      return new Bare_Type_Ref_Node_Builder_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Bare_Type_Ref_Node_Builder_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Bare_Type_Ref_Node_Builder_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;


      ----------
      -- Hash --
      ----------

      function Hash (R : Bare_Type_Ref_Node_Builder_Array_Access) return Hash_Type is
         Result : Hash_Type := Initial_Hash;
      begin
         for I in R.Items'Range loop
            Result := Combine (Result, Hash (R.Items (I)));
         end loop;
         return Result;
      end Hash;


      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Integer_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Integer
   is
      function Absolute_Get
        (T : Integer_Array_Access; Index : Integer)
         return Integer
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Integer,
         Sequence_Type => Integer_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Integer;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return 0;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Integer_Array_Access) return Integer_Array_Access is
      Ret : Integer_Array_Access := Create_Integer_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Integer_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Integer_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Integer_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Integer_Array (Items_Count : Natural) return Integer_Array_Access
   is (if Items_Count = 0
       then No_Integer_Array_Type
       else new Integer_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Integer_Array
     (Items : Internal_Integer_Array) return Integer_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Integer_Array_Type;
      end if;

      return new Integer_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Integer_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (L.Items (I) = R.Items (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Integer_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Complete_Item_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Complete_Item
   is
      function Absolute_Get
        (T : Internal_Complete_Item_Array_Access; Index : Integer)
         return Internal_Complete_Item
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Complete_Item,
         Sequence_Type => Internal_Complete_Item_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Complete_Item;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Complete_Item;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Complete_Item_Array_Access) return Internal_Complete_Item_Array_Access is
      Ret : Internal_Complete_Item_Array_Access := Create_Internal_Complete_Item_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Complete_Item_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Complete_Item_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Complete_Item_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Complete_Item_Array (Items_Count : Natural) return Internal_Complete_Item_Array_Access
   is (if Items_Count = 0
       then No_Internal_Complete_Item_Array_Type
       else new Internal_Complete_Item_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Complete_Item_Array
     (Items : Internal_Internal_Complete_Item_Array) return Internal_Complete_Item_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Complete_Item_Array_Type;
      end if;

      return new Internal_Complete_Item_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Complete_Item_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Complete_Item_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Argument_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Argument
   is
      function Absolute_Get
        (T : Internal_Entity_Argument_Array_Access; Index : Integer)
         return Internal_Entity_Argument
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Argument,
         Sequence_Type => Internal_Entity_Argument_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Argument;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Argument;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Argument_Array_Access) return Internal_Entity_Argument_Array_Access is
      Ret : Internal_Entity_Argument_Array_Access := Create_Internal_Entity_Argument_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Argument_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Argument_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Argument_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Argument_Array (Items_Count : Natural) return Internal_Entity_Argument_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Argument_Array_Type
       else new Internal_Entity_Argument_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Argument_Array
     (Items : Internal_Internal_Entity_Argument_Array) return Internal_Entity_Argument_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Argument_Array_Type;
      end if;

      return new Internal_Entity_Argument_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Argument_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Argument_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity
   is
      function Absolute_Get
        (T : Internal_Entity_Array_Access; Index : Integer)
         return Internal_Entity
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity,
         Sequence_Type => Internal_Entity_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Array_Access) return Internal_Entity_Array_Access is
      Ret : Internal_Entity_Array_Access := Create_Internal_Entity_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Array (Items_Count : Natural) return Internal_Entity_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Array_Type
       else new Internal_Entity_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));

   function Create_Internal_Entity_Array
     (Items : AST_Envs.Entity_Array) return Internal_Entity_Array_Access
   is (if Items'Length = 0
       then No_Internal_Entity_Array_Type
       else new Internal_Entity_Array_Record'
         (N         => Items'Length,
          Items     => Implementation.Internal_Internal_Entity_Array (Items),
          Ref_Count => 1));

   function Create_Internal_Entity_Array
     (Items : Internal_Internal_Entity_Array) return Internal_Entity_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Array_Type;
      end if;

      return new Internal_Entity_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Def_Id_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Def_Id
   is
      function Absolute_Get
        (T : Internal_Entity_Def_Id_Array_Access; Index : Integer)
         return Internal_Entity_Def_Id
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Def_Id,
         Sequence_Type => Internal_Entity_Def_Id_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Def_Id;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Def_Id;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Def_Id_Array_Access) return Internal_Entity_Def_Id_Array_Access is
      Ret : Internal_Entity_Def_Id_Array_Access := Create_Internal_Entity_Def_Id_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Def_Id_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Def_Id_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Def_Id_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Def_Id_Array (Items_Count : Natural) return Internal_Entity_Def_Id_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Def_Id_Array_Type
       else new Internal_Entity_Def_Id_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Def_Id_Array
     (Items : Internal_Internal_Entity_Def_Id_Array) return Internal_Entity_Def_Id_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Def_Id_Array_Type;
      end if;

      return new Internal_Entity_Def_Id_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Def_Id_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Def_Id_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Enum_Class_Alt_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Enum_Class_Alt_Decl
   is
      function Absolute_Get
        (T : Internal_Entity_Enum_Class_Alt_Decl_Array_Access; Index : Integer)
         return Internal_Entity_Enum_Class_Alt_Decl
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Enum_Class_Alt_Decl,
         Sequence_Type => Internal_Entity_Enum_Class_Alt_Decl_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Enum_Class_Alt_Decl;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Enum_Class_Alt_Decl;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Enum_Class_Alt_Decl_Array_Access) return Internal_Entity_Enum_Class_Alt_Decl_Array_Access is
      Ret : Internal_Entity_Enum_Class_Alt_Decl_Array_Access := Create_Internal_Entity_Enum_Class_Alt_Decl_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Enum_Class_Alt_Decl_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Enum_Class_Alt_Decl_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Enum_Class_Alt_Decl_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Enum_Class_Alt_Decl_Array (Items_Count : Natural) return Internal_Entity_Enum_Class_Alt_Decl_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Enum_Class_Alt_Decl_Array_Type
       else new Internal_Entity_Enum_Class_Alt_Decl_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Enum_Class_Alt_Decl_Array
     (Items : Internal_Internal_Entity_Enum_Class_Alt_Decl_Array) return Internal_Entity_Enum_Class_Alt_Decl_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Enum_Class_Alt_Decl_Array_Type;
      end if;

      return new Internal_Entity_Enum_Class_Alt_Decl_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Enum_Class_Alt_Decl_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Enum_Class_Alt_Decl_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Expr_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Expr
   is
      function Absolute_Get
        (T : Internal_Entity_Expr_Array_Access; Index : Integer)
         return Internal_Entity_Expr
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Expr,
         Sequence_Type => Internal_Entity_Expr_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Expr;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Expr;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Expr_Array_Access) return Internal_Entity_Expr_Array_Access is
      Ret : Internal_Entity_Expr_Array_Access := Create_Internal_Entity_Expr_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Expr_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Expr_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Expr_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Expr_Array (Items_Count : Natural) return Internal_Entity_Expr_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Expr_Array_Type
       else new Internal_Entity_Expr_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Expr_Array
     (Items : Internal_Internal_Entity_Expr_Array) return Internal_Entity_Expr_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Expr_Array_Type;
      end if;

      return new Internal_Entity_Expr_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Expr_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Expr_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Field_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Field_Decl
   is
      function Absolute_Get
        (T : Internal_Entity_Field_Decl_Array_Access; Index : Integer)
         return Internal_Entity_Field_Decl
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Field_Decl,
         Sequence_Type => Internal_Entity_Field_Decl_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Field_Decl;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Field_Decl;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Field_Decl_Array_Access) return Internal_Entity_Field_Decl_Array_Access is
      Ret : Internal_Entity_Field_Decl_Array_Access := Create_Internal_Entity_Field_Decl_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Field_Decl_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Field_Decl_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Field_Decl_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Field_Decl_Array (Items_Count : Natural) return Internal_Entity_Field_Decl_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Field_Decl_Array_Type
       else new Internal_Entity_Field_Decl_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Field_Decl_Array
     (Items : Internal_Internal_Entity_Field_Decl_Array) return Internal_Entity_Field_Decl_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Field_Decl_Array_Type;
      end if;

      return new Internal_Entity_Field_Decl_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Field_Decl_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Field_Decl_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Full_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Full_Decl
   is
      function Absolute_Get
        (T : Internal_Entity_Full_Decl_Array_Access; Index : Integer)
         return Internal_Entity_Full_Decl
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Full_Decl,
         Sequence_Type => Internal_Entity_Full_Decl_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Full_Decl;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Full_Decl;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Full_Decl_Array_Access) return Internal_Entity_Full_Decl_Array_Access is
      Ret : Internal_Entity_Full_Decl_Array_Access := Create_Internal_Entity_Full_Decl_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Full_Decl_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Full_Decl_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Full_Decl_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Full_Decl_Array (Items_Count : Natural) return Internal_Entity_Full_Decl_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Full_Decl_Array_Type
       else new Internal_Entity_Full_Decl_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Full_Decl_Array
     (Items : Internal_Internal_Entity_Full_Decl_Array) return Internal_Entity_Full_Decl_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Full_Decl_Array_Type;
      end if;

      return new Internal_Entity_Full_Decl_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Full_Decl_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Full_Decl_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Fun_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Fun_Decl
   is
      function Absolute_Get
        (T : Internal_Entity_Fun_Decl_Array_Access; Index : Integer)
         return Internal_Entity_Fun_Decl
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Fun_Decl,
         Sequence_Type => Internal_Entity_Fun_Decl_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Fun_Decl;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Fun_Decl;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Fun_Decl_Array_Access) return Internal_Entity_Fun_Decl_Array_Access is
      Ret : Internal_Entity_Fun_Decl_Array_Access := Create_Internal_Entity_Fun_Decl_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Fun_Decl_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Fun_Decl_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Fun_Decl_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Fun_Decl_Array (Items_Count : Natural) return Internal_Entity_Fun_Decl_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Fun_Decl_Array_Type
       else new Internal_Entity_Fun_Decl_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Fun_Decl_Array
     (Items : Internal_Internal_Entity_Fun_Decl_Array) return Internal_Entity_Fun_Decl_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Fun_Decl_Array_Type;
      end if;

      return new Internal_Entity_Fun_Decl_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Fun_Decl_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Fun_Decl_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Fun_Param_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Fun_Param_Decl
   is
      function Absolute_Get
        (T : Internal_Entity_Fun_Param_Decl_Array_Access; Index : Integer)
         return Internal_Entity_Fun_Param_Decl
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Fun_Param_Decl,
         Sequence_Type => Internal_Entity_Fun_Param_Decl_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Fun_Param_Decl;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Fun_Param_Decl;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Fun_Param_Decl_Array_Access) return Internal_Entity_Fun_Param_Decl_Array_Access is
      Ret : Internal_Entity_Fun_Param_Decl_Array_Access := Create_Internal_Entity_Fun_Param_Decl_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Fun_Param_Decl_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Fun_Param_Decl_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Fun_Param_Decl_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Fun_Param_Decl_Array (Items_Count : Natural) return Internal_Entity_Fun_Param_Decl_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Fun_Param_Decl_Array_Type
       else new Internal_Entity_Fun_Param_Decl_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Fun_Param_Decl_Array
     (Items : Internal_Internal_Entity_Fun_Param_Decl_Array) return Internal_Entity_Fun_Param_Decl_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Fun_Param_Decl_Array_Type;
      end if;

      return new Internal_Entity_Fun_Param_Decl_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Fun_Param_Decl_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Fun_Param_Decl_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Generic_Param_Type_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Generic_Param_Type_Decl
   is
      function Absolute_Get
        (T : Internal_Entity_Generic_Param_Type_Decl_Array_Access; Index : Integer)
         return Internal_Entity_Generic_Param_Type_Decl
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Generic_Param_Type_Decl,
         Sequence_Type => Internal_Entity_Generic_Param_Type_Decl_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Generic_Param_Type_Decl;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Generic_Param_Type_Decl;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Generic_Param_Type_Decl_Array_Access) return Internal_Entity_Generic_Param_Type_Decl_Array_Access is
      Ret : Internal_Entity_Generic_Param_Type_Decl_Array_Access := Create_Internal_Entity_Generic_Param_Type_Decl_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Generic_Param_Type_Decl_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Generic_Param_Type_Decl_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Generic_Param_Type_Decl_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Generic_Param_Type_Decl_Array (Items_Count : Natural) return Internal_Entity_Generic_Param_Type_Decl_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Generic_Param_Type_Decl_Array_Type
       else new Internal_Entity_Generic_Param_Type_Decl_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Generic_Param_Type_Decl_Array
     (Items : Internal_Internal_Entity_Generic_Param_Type_Decl_Array) return Internal_Entity_Generic_Param_Type_Decl_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Generic_Param_Type_Decl_Array_Type;
      end if;

      return new Internal_Entity_Generic_Param_Type_Decl_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Generic_Param_Type_Decl_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Generic_Param_Type_Decl_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Ref_Id_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Ref_Id
   is
      function Absolute_Get
        (T : Internal_Entity_Ref_Id_Array_Access; Index : Integer)
         return Internal_Entity_Ref_Id
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Ref_Id,
         Sequence_Type => Internal_Entity_Ref_Id_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Ref_Id;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Ref_Id;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Ref_Id_Array_Access) return Internal_Entity_Ref_Id_Array_Access is
      Ret : Internal_Entity_Ref_Id_Array_Access := Create_Internal_Entity_Ref_Id_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Ref_Id_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Ref_Id_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Ref_Id_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Ref_Id_Array (Items_Count : Natural) return Internal_Entity_Ref_Id_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Ref_Id_Array_Type
       else new Internal_Entity_Ref_Id_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Ref_Id_Array
     (Items : Internal_Internal_Entity_Ref_Id_Array) return Internal_Entity_Ref_Id_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Ref_Id_Array_Type;
      end if;

      return new Internal_Entity_Ref_Id_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Ref_Id_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Ref_Id_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Type_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Type_Decl
   is
      function Absolute_Get
        (T : Internal_Entity_Type_Decl_Array_Access; Index : Integer)
         return Internal_Entity_Type_Decl
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Entity_Type_Decl,
         Sequence_Type => Internal_Entity_Type_Decl_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Entity_Type_Decl;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Entity_Type_Decl;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Entity_Type_Decl_Array_Access) return Internal_Entity_Type_Decl_Array_Access is
      Ret : Internal_Entity_Type_Decl_Array_Access := Create_Internal_Entity_Type_Decl_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Type_Decl_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Entity_Type_Decl_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Type_Decl_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Entity_Type_Decl_Array (Items_Count : Natural) return Internal_Entity_Type_Decl_Array_Access
   is (if Items_Count = 0
       then No_Internal_Entity_Type_Decl_Array_Type
       else new Internal_Entity_Type_Decl_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Entity_Type_Decl_Array
     (Items : Internal_Internal_Entity_Type_Decl_Array) return Internal_Entity_Type_Decl_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Entity_Type_Decl_Array_Type;
      end if;

      return new Internal_Entity_Type_Decl_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Entity_Type_Decl_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Type_Decl_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;


      ----------
      -- Hash --
      ----------

      function Hash (R : Internal_Entity_Type_Decl_Array_Access) return Hash_Type is
         Result : Hash_Type := Initial_Hash;
      begin
         for I in R.Items'Range loop
            Result := Combine (Result, Hash (R.Items (I)));
         end loop;
         return Result;
      end Hash;


      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Env_Assoc_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Env_Assoc
   is
      function Absolute_Get
        (T : Internal_Env_Assoc_Array_Access; Index : Integer)
         return Internal_Env_Assoc
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Env_Assoc,
         Sequence_Type => Internal_Env_Assoc_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Env_Assoc;
   begin
      if Relative_Get (T, Index, Result) then
            Inc_Ref (Result);
         return Result;
      elsif Or_Null then
         return No_Env_Assoc;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Env_Assoc_Array_Access) return Internal_Env_Assoc_Array_Access is
      Ret : Internal_Env_Assoc_Array_Access := Create_Internal_Env_Assoc_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
         for Item of Ret.Items loop
            Inc_Ref (Item);
         end loop;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Env_Assoc_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Env_Assoc_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Env_Assoc_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
            for Item of T.Items loop
               Dec_Ref (Item);
            end loop;
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Env_Assoc_Array (Items_Count : Natural) return Internal_Env_Assoc_Array_Access
   is (if Items_Count = 0
       then No_Internal_Env_Assoc_Array_Type
       else new Internal_Env_Assoc_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Env_Assoc_Array
     (Items : Internal_Internal_Env_Assoc_Array) return Internal_Env_Assoc_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Env_Assoc_Array_Type;
      end if;

         for El of Items loop
            Inc_Ref (El);
         end loop;
      return new Internal_Env_Assoc_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Env_Assoc_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Env_Assoc_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Inner_Env_Assoc_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Inner_Env_Assoc
   is
      function Absolute_Get
        (T : Internal_Inner_Env_Assoc_Array_Access; Index : Integer)
         return Internal_Inner_Env_Assoc
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Inner_Env_Assoc,
         Sequence_Type => Internal_Inner_Env_Assoc_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Inner_Env_Assoc;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Inner_Env_Assoc;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Inner_Env_Assoc_Array_Access) return Internal_Inner_Env_Assoc_Array_Access is
      Ret : Internal_Inner_Env_Assoc_Array_Access := Create_Internal_Inner_Env_Assoc_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Inner_Env_Assoc_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Inner_Env_Assoc_Array (Items_Count : Natural) return Internal_Inner_Env_Assoc_Array_Access
   is (if Items_Count = 0
       then No_Internal_Inner_Env_Assoc_Array_Type
       else new Internal_Inner_Env_Assoc_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Inner_Env_Assoc_Array
     (Items : Internal_Internal_Inner_Env_Assoc_Array) return Internal_Inner_Env_Assoc_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Inner_Env_Assoc_Array_Type;
      end if;

      return new Internal_Inner_Env_Assoc_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Inner_Env_Assoc_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (L.Items (I) = R.Items (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Inner_Env_Assoc_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Logic_Context_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Logic_Context
   is
      function Absolute_Get
        (T : Internal_Logic_Context_Array_Access; Index : Integer)
         return Internal_Logic_Context
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Logic_Context,
         Sequence_Type => Internal_Logic_Context_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Logic_Context;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Logic_Context;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Logic_Context_Array_Access) return Internal_Logic_Context_Array_Access is
      Ret : Internal_Logic_Context_Array_Access := Create_Internal_Logic_Context_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Logic_Context_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Logic_Context_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Logic_Context_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Logic_Context_Array (Items_Count : Natural) return Internal_Logic_Context_Array_Access
   is (if Items_Count = 0
       then No_Internal_Logic_Context_Array_Type
       else new Internal_Logic_Context_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Logic_Context_Array
     (Items : Internal_Internal_Logic_Context_Array) return Internal_Logic_Context_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Logic_Context_Array_Type;
      end if;

      return new Internal_Logic_Context_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Logic_Context_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Logic_Context_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Param_Match_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Param_Match
   is
      function Absolute_Get
        (T : Internal_Param_Match_Array_Access; Index : Integer)
         return Internal_Param_Match
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Param_Match,
         Sequence_Type => Internal_Param_Match_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Param_Match;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Param_Match;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Param_Match_Array_Access) return Internal_Param_Match_Array_Access is
      Ret : Internal_Param_Match_Array_Access := Create_Internal_Param_Match_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Param_Match_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Param_Match_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Param_Match_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Param_Match_Array (Items_Count : Natural) return Internal_Param_Match_Array_Access
   is (if Items_Count = 0
       then No_Internal_Param_Match_Array_Type
       else new Internal_Param_Match_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Param_Match_Array
     (Items : Internal_Internal_Param_Match_Array) return Internal_Param_Match_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Param_Match_Array_Type;
      end if;

      return new Internal_Param_Match_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Param_Match_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Param_Match_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Ref_Result_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Ref_Result
   is
      function Absolute_Get
        (T : Internal_Ref_Result_Array_Access; Index : Integer)
         return Internal_Ref_Result
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Ref_Result,
         Sequence_Type => Internal_Ref_Result_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Ref_Result;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Ref_Result;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Ref_Result_Array_Access) return Internal_Ref_Result_Array_Access is
      Ret : Internal_Ref_Result_Array_Access := Create_Internal_Ref_Result_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Ref_Result_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Ref_Result_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Ref_Result_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Ref_Result_Array (Items_Count : Natural) return Internal_Ref_Result_Array_Access
   is (if Items_Count = 0
       then No_Internal_Ref_Result_Array_Type
       else new Internal_Ref_Result_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Ref_Result_Array
     (Items : Internal_Internal_Ref_Result_Array) return Internal_Ref_Result_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Ref_Result_Array_Type;
      end if;

      return new Internal_Ref_Result_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Ref_Result_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Ref_Result_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Resolved_Param_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Resolved_Param
   is
      function Absolute_Get
        (T : Internal_Resolved_Param_Array_Access; Index : Integer)
         return Internal_Resolved_Param
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Resolved_Param,
         Sequence_Type => Internal_Resolved_Param_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Resolved_Param;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Resolved_Param;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Resolved_Param_Array_Access) return Internal_Resolved_Param_Array_Access is
      Ret : Internal_Resolved_Param_Array_Access := Create_Internal_Resolved_Param_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Resolved_Param_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Resolved_Param_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Resolved_Param_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Resolved_Param_Array (Items_Count : Natural) return Internal_Resolved_Param_Array_Access
   is (if Items_Count = 0
       then No_Internal_Resolved_Param_Array_Type
       else new Internal_Resolved_Param_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Resolved_Param_Array
     (Items : Internal_Internal_Resolved_Param_Array) return Internal_Resolved_Param_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Resolved_Param_Array_Type;
      end if;

      return new Internal_Resolved_Param_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Resolved_Param_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Resolved_Param_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;


      ----------
      -- Hash --
      ----------

      function Hash (R : Internal_Resolved_Param_Array_Access) return Hash_Type is
         Result : Hash_Type := Initial_Hash;
      begin
         for I in R.Items'Range loop
            Result := Combine (Result, Hash (R.Items (I)));
         end loop;
         return Result;
      end Hash;


      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Solver_Diagnostic_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Solver_Diagnostic
   is
      function Absolute_Get
        (T : Internal_Solver_Diagnostic_Array_Access; Index : Integer)
         return Internal_Solver_Diagnostic
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Solver_Diagnostic,
         Sequence_Type => Internal_Solver_Diagnostic_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Solver_Diagnostic;
   begin
      if Relative_Get (T, Index, Result) then
            Inc_Ref (Result);
         return Result;
      elsif Or_Null then
         return No_Solver_Diagnostic;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Solver_Diagnostic_Array_Access) return Internal_Solver_Diagnostic_Array_Access is
      Ret : Internal_Solver_Diagnostic_Array_Access := Create_Internal_Solver_Diagnostic_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
         for Item of Ret.Items loop
            Inc_Ref (Item);
         end loop;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Solver_Diagnostic_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Solver_Diagnostic_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Solver_Diagnostic_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
            for Item of T.Items loop
               Dec_Ref (Item);
            end loop;
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Solver_Diagnostic_Array (Items_Count : Natural) return Internal_Solver_Diagnostic_Array_Access
   is (if Items_Count = 0
       then No_Internal_Solver_Diagnostic_Array_Type
       else new Internal_Solver_Diagnostic_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Solver_Diagnostic_Array
     (Items : Internal_Internal_Solver_Diagnostic_Array) return Internal_Solver_Diagnostic_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Solver_Diagnostic_Array_Type;
      end if;

         for El of Items loop
            Inc_Ref (El);
         end loop;
      return new Internal_Solver_Diagnostic_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Solver_Diagnostic_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Solver_Diagnostic_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Unit_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Unit
   is
      function Absolute_Get
        (T : Internal_Unit_Array_Access; Index : Integer)
         return Internal_Unit
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Internal_Unit,
         Sequence_Type => Internal_Unit_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Internal_Unit;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return null;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Internal_Unit_Array_Access) return Internal_Unit_Array_Access is
      Ret : Internal_Unit_Array_Access := Create_Internal_Unit_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Unit_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Internal_Unit_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Unit_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Internal_Unit_Array (Items_Count : Natural) return Internal_Unit_Array_Access
   is (if Items_Count = 0
       then No_Internal_Unit_Array_Type
       else new Internal_Unit_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Internal_Unit_Array
     (Items : Internal_Internal_Unit_Array) return Internal_Unit_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Internal_Unit_Array_Type;
      end if;

      return new Internal_Unit_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Internal_Unit_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (L.Items (I) = R.Items (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Unit_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Lexical_Env_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Lexical_Env
   is
      function Absolute_Get
        (T : Lexical_Env_Array_Access; Index : Integer)
         return Lexical_Env
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Lexical_Env,
         Sequence_Type => Lexical_Env_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Lexical_Env;
   begin
      if Relative_Get (T, Index, Result) then
            Inc_Ref (Result);
         return Result;
      elsif Or_Null then
         return Empty_Env;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Lexical_Env_Array_Access) return Lexical_Env_Array_Access is
      Ret : Lexical_Env_Array_Access := Create_Lexical_Env_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
         for Item of Ret.Items loop
            Inc_Ref (Item);
         end loop;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Lexical_Env_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Lexical_Env_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Lexical_Env_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
            for Item of T.Items loop
               Dec_Ref (Item);
            end loop;
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Lexical_Env_Array (Items_Count : Natural) return Lexical_Env_Array_Access
   is (if Items_Count = 0
       then No_Lexical_Env_Array_Type
       else new Lexical_Env_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Lexical_Env_Array
     (Items : Internal_Lexical_Env_Array) return Lexical_Env_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Lexical_Env_Array_Type;
      end if;

         for El of Items loop
            Inc_Ref (El);
         end loop;
      return new Lexical_Env_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Lexical_Env_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Lexical_Env_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Logic_Equation_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Logic_Equation
   is
      function Absolute_Get
        (T : Logic_Equation_Array_Access; Index : Integer)
         return Logic_Equation
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Logic_Equation,
         Sequence_Type => Logic_Equation_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Logic_Equation;
   begin
      if Relative_Get (T, Index, Result) then
            Inc_Ref (Result);
         return Result;
      elsif Or_Null then
         return Null_Logic_Equation;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Logic_Equation_Array_Access) return Logic_Equation_Array_Access is
      Ret : Logic_Equation_Array_Access := Create_Logic_Equation_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
         for Item of Ret.Items loop
            Inc_Ref (Item);
         end loop;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Logic_Equation_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Logic_Equation_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Logic_Equation_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
            for Item of T.Items loop
               Dec_Ref (Item);
            end loop;
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Logic_Equation_Array (Items_Count : Natural) return Logic_Equation_Array_Access
   is (if Items_Count = 0
       then No_Logic_Equation_Array_Type
       else new Logic_Equation_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Logic_Equation_Array
     (Items : Internal_Logic_Equation_Array) return Logic_Equation_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Logic_Equation_Array_Type;
      end if;

         for El of Items loop
            Inc_Ref (El);
         end loop;
      return new Logic_Equation_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Logic_Equation_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (L.Items (I) = R.Items (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Logic_Equation_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Logic_Var_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Logic_Var
   is
      function Absolute_Get
        (T : Logic_Var_Array_Access; Index : Integer)
         return Logic_Var
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Logic_Var,
         Sequence_Type => Logic_Var_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Logic_Var;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return null;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Logic_Var_Array_Access) return Logic_Var_Array_Access is
      Ret : Logic_Var_Array_Access := Create_Logic_Var_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Logic_Var_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Logic_Var_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Logic_Var_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Logic_Var_Array (Items_Count : Natural) return Logic_Var_Array_Access
   is (if Items_Count = 0
       then No_Logic_Var_Array_Type
       else new Logic_Var_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Logic_Var_Array
     (Items : Internal_Logic_Var_Array) return Logic_Var_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Logic_Var_Array_Type;
      end if;

      return new Logic_Var_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Logic_Var_Array_Access) return Boolean is
      use type Logic_Var;
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (L.Items (I) = R.Items (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Logic_Var_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : String_Type_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return String_Type
   is
      function Absolute_Get
        (T : String_Type_Array_Access; Index : Integer)
         return String_Type
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => String_Type,
         Sequence_Type => String_Type_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : String_Type;
   begin
      if Relative_Get (T, Index, Result) then
            Inc_Ref (Result);
         return Result;
      elsif Or_Null then
         return Empty_String;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : String_Type_Array_Access) return String_Type_Array_Access is
      Ret : String_Type_Array_Access := Create_String_Type_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
         for Item of Ret.Items loop
            Inc_Ref (Item);
         end loop;
      return Ret;
   end Concat;

      ------------------
      -- Join_Strings --
      ------------------

      function Join_Strings
        (Separator : String_Type;
         Strings   : String_Type_Array_Access) return String_Type
      is
         Separator_Length : constant Natural := Separator.Length;
         Length           : Natural := 0;
         First            : Boolean;
      begin
         --  First, compute the length of the result: the sum of all string
         --  lengths in Strings.
         First := True;
         for S of Strings.Items loop
            if First then
               First := False;
            else
               Length := Length + Separator_Length;
            end if;
            Length := Length + S.Length;
         end loop;

         --  Create the result string with the correct length. Do not use our
         --  constructor and initialize the content in-place, to avoid extra
         --  copies.
         return Result : constant String_Type :=
            new String_Record (Length)
         do
            Result.Ref_Count := 1;

            --  Now copy the content of all strings into the result
            declare
               Last : Natural := 0;
            begin
               First := True;
               for S of Strings.Items loop
                  if First then
                     First := False;
                  else
                     Result.Content (Last + 1 .. Last + Separator_Length) :=
                        Separator.Content;
                     Last := Last + Separator_Length;
                  end if;
                  Result.Content (Last + 1 .. Last + S.Length) := S.Content;
                  Last := Last + S.Length;
               end loop;
            end;
         end return;
      end Join_Strings;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : String_Type_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : String_Type_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out String_Type_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
            for Item of T.Items loop
               Dec_Ref (Item);
            end loop;
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_String_Type_Array (Items_Count : Natural) return String_Type_Array_Access
   is (if Items_Count = 0
       then No_String_Type_Array_Type
       else new String_Type_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_String_Type_Array
     (Items : Internal_String_Type_Array) return String_Type_Array_Access is
   begin
      if Items'Length = 0 then
         return No_String_Type_Array_Type;
      end if;

         for El of Items loop
            Inc_Ref (El);
         end loop;
      return new String_Type_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : String_Type_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (Equivalent (L.Items (I), R.Items (I))) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : String_Type_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;




      

   

   ---------
   -- Get --
   ---------

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Symbol_Type_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Symbol_Type
   is
      function Absolute_Get
        (T : Symbol_Type_Array_Access; Index : Integer)
         return Symbol_Type
      is
        (T.Items (Index + 1)); --  T.Items is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Symbol_Type,
         Sequence_Type => Symbol_Type_Array_Access,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Symbol_Type;
   begin
      if Relative_Get (T, Index, Result) then
         return Result;
      elsif Or_Null then
         return No_Symbol;
      else
         Raise_Property_Exception
           (Node,
            Property_Error'Identity,
            "out-of-bounds array access");
      end if;
   end Get;

   ------------
   -- Concat --
   ------------

   function Concat (L, R : Symbol_Type_Array_Access) return Symbol_Type_Array_Access is
      Ret : Symbol_Type_Array_Access := Create_Symbol_Type_Array (Length (L) + Length (R));
   begin
      Ret.Items := L.Items & R.Items;
      return Ret;
   end Concat;


   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Symbol_Type_Array_Access) is
   begin
      if T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   ------------
   -- Length --
   ------------

   function Length (T : Symbol_Type_Array_Access) return Natural is (T.N);

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Symbol_Type_Array_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

   function Create_Symbol_Type_Array (Items_Count : Natural) return Symbol_Type_Array_Access
   is (if Items_Count = 0
       then No_Symbol_Type_Array_Type
       else new Symbol_Type_Array_Record'(N => Items_Count, Ref_Count => 1, Items => <>));


   function Create_Symbol_Type_Array
     (Items : Internal_Symbol_Type_Array) return Symbol_Type_Array_Access is
   begin
      if Items'Length = 0 then
         return No_Symbol_Type_Array_Type;
      end if;

      return new Symbol_Type_Array_Record'
        (N => Items'Length, Ref_Count => 1, Items => Items);
   end;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent (L, R : Symbol_Type_Array_Access) return Boolean is
   begin
      if L.N /= R.N then
         return False;
      end if;

      for I in L.Items'Range loop
         if not (L.Items (I) = R.Items (I)) then
            return False;
         end if;
      end loop;

      return True;
   end Equivalent;


      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Symbol_Type_Array_Access) return String is
         Result : Unbounded_String;
      begin
         Append (Result, "[");
         for I in A.Items'Range loop
            if I > A.Items'First then
               Append (Result, ", ");
            end if;
            Append (Result, Trace_Image (A.Items (I)));
         end loop;
         Append (Result, "]");
         return To_String (Result);
      end Trace_Image;





         

   

   ----------
   -- Next --
   ----------

   function Next
     (T       : Bare_Lkt_Node_Iterator_Access;
      Element : out Bare_Lkt_Node) return Boolean is
   begin
      if T = null then
         raise Property_Error with "null access dereference";
      end if;
      Check_Safety_Net (T.Safety_Net);

      if T.Index > T.Elements.Items'Last then
         return False;
      else
         Element := T.Elements.Items (T.Index);
         T.Index := T.Index + 1;
         return True;
      end if;
   end Next;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Bare_Lkt_Node_Iterator_Access) is
   begin
      if T /= null and then T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Bare_Lkt_Node_Iterator_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Dec_Ref (T.Elements);
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Bare_Lkt_Node_Iterator_Access) return String is
      begin
         return "<Iterator of LktNode, index="
                & A.Index'Image & ">";
      end Trace_Image;


         

   

   ----------
   -- Next --
   ----------

   function Next
     (T       : Internal_Entity_Iterator_Access;
      Element : out Internal_Entity) return Boolean is
   begin
      if T = null then
         raise Property_Error with "null access dereference";
      end if;
      Check_Safety_Net (T.Safety_Net);

      if T.Index > T.Elements.Items'Last then
         return False;
      else
         Element := T.Elements.Items (T.Index);
         T.Index := T.Index + 1;
         return True;
      end if;
   end Next;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Entity_Iterator_Access) is
   begin
      if T /= null and then T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Entity_Iterator_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Dec_Ref (T.Elements);
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Entity_Iterator_Access) return String is
      begin
         return "<Iterator of Entity[LktNode], index="
                & A.Index'Image & ">";
      end Trace_Image;


         

   

   ----------
   -- Next --
   ----------

   function Next
     (T       : Internal_Inner_Env_Assoc_Iterator_Access;
      Element : out Internal_Inner_Env_Assoc) return Boolean is
   begin
      if T = null then
         raise Property_Error with "null access dereference";
      end if;
      Check_Safety_Net (T.Safety_Net);

      if T.Index > T.Elements.Items'Last then
         return False;
      else
         Element := T.Elements.Items (T.Index);
         T.Index := T.Index + 1;
         return True;
      end if;
   end Next;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Iterator_Access) is
   begin
      if T /= null and then T.Ref_Count >= 0 then
         T.Ref_Count := T.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Iterator_Access) is
   begin
      if T = null or else T.Ref_Count < 0 then
         return;
      end if;

      if T.Ref_Count = 1 then
         Dec_Ref (T.Elements);
         Free (T);
      else
         T.Ref_Count := T.Ref_Count - 1;
         T := null;
      end if;
   end Dec_Ref;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (A : Internal_Inner_Env_Assoc_Iterator_Access) return String is
      begin
         return "<Iterator of InnerEnvAssoc, index="
                & A.Index'Image & ">";
      end Trace_Image;




   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Internal_Unit) return Boolean is
   begin
      return Left.Filename < Right.Filename;
   end "<";

      



function Hash (Key : Mmz_Key_Item) return Hash_Type;
function Equivalent (L, R : Mmz_Key_Item) return Boolean;
procedure Destroy (Key : in out Mmz_Key_Array_Access);

----------------
-- Equivalent --
----------------

function Equivalent (L, R : Mmz_Key_Item) return Boolean is
begin
   if L.Kind /= R.Kind then
      return False;
   end if;

   case L.Kind is
         when Mmz_Bare_Argument_List =>
            return L.As_Bare_Argument_List = R.As_Bare_Argument_List;
         when Mmz_Bare_Basic_Class_Decl =>
            return L.As_Bare_Basic_Class_Decl = R.As_Bare_Basic_Class_Decl;
         when Mmz_Bare_Decl =>
            return L.As_Bare_Decl = R.As_Bare_Decl;
         when Mmz_Bare_Dot_Expr =>
            return L.As_Bare_Dot_Expr = R.As_Bare_Dot_Expr;
         when Mmz_Bare_Expr =>
            return L.As_Bare_Expr = R.As_Bare_Expr;
         when Mmz_Bare_Lexer_Decl =>
            return L.As_Bare_Lexer_Decl = R.As_Bare_Lexer_Decl;
         when Mmz_Bare_Lkt_Node =>
            return L.As_Bare_Lkt_Node = R.As_Bare_Lkt_Node;
         when Mmz_Bare_Module_Id =>
            return L.As_Bare_Module_Id = R.As_Bare_Module_Id;
         when Mmz_Bare_Ref_Id =>
            return L.As_Bare_Ref_Id = R.As_Bare_Ref_Id;
         when Mmz_Bare_Struct_Decl =>
            return L.As_Bare_Struct_Decl = R.As_Bare_Struct_Decl;
         when Mmz_Bare_Type_Decl =>
            return L.As_Bare_Type_Decl = R.As_Bare_Type_Decl;
         when Mmz_Bare_Type_Ref =>
            return L.As_Bare_Type_Ref = R.As_Bare_Type_Ref;
         when Mmz_Boolean =>
            return L.As_Boolean = R.As_Boolean;
         when Mmz_Env_Rebindings =>
            return L.As_Env_Rebindings = R.As_Env_Rebindings;
         when Mmz_Internal_Entity =>
            return Equivalent (L.As_Internal_Entity, R.As_Internal_Entity);
         when Mmz_Internal_Entity_Argument_List =>
            return Equivalent (L.As_Internal_Entity_Argument_List, R.As_Internal_Entity_Argument_List);
         when Mmz_Internal_Entity_Decl =>
            return Equivalent (L.As_Internal_Entity_Decl, R.As_Internal_Entity_Decl);
         when Mmz_Internal_Entity_Info =>
            return Equivalent (L.As_Internal_Entity_Info, R.As_Internal_Entity_Info);
         when Mmz_Internal_Entity_Type_Decl =>
            return Equivalent (L.As_Internal_Entity_Type_Decl, R.As_Internal_Entity_Type_Decl);
         when Mmz_Internal_Entity_Type_Decl_Array_Access =>
            return Equivalent (L.As_Internal_Entity_Type_Decl_Array_Access, R.As_Internal_Entity_Type_Decl_Array_Access);
         when Mmz_Internal_Metadata =>
            return L.As_Internal_Metadata = R.As_Internal_Metadata;
         when Mmz_Internal_Resolved_Param_Array_Access =>
            return Equivalent (L.As_Internal_Resolved_Param_Array_Access, R.As_Internal_Resolved_Param_Array_Access);
         when Mmz_Symbol_Type =>
            return L.As_Symbol_Type = R.As_Symbol_Type;
   end case;
end Equivalent;

----------
-- Hash --
----------

function Hash (Key : Mmz_Key_Item) return Hash_Type is
begin
   case Key.Kind is
         when Mmz_Bare_Argument_List =>
            return Hash (Key.As_Bare_Argument_List);
         when Mmz_Bare_Basic_Class_Decl =>
            return Hash (Key.As_Bare_Basic_Class_Decl);
         when Mmz_Bare_Decl =>
            return Hash (Key.As_Bare_Decl);
         when Mmz_Bare_Dot_Expr =>
            return Hash (Key.As_Bare_Dot_Expr);
         when Mmz_Bare_Expr =>
            return Hash (Key.As_Bare_Expr);
         when Mmz_Bare_Lexer_Decl =>
            return Hash (Key.As_Bare_Lexer_Decl);
         when Mmz_Bare_Lkt_Node =>
            return Hash (Key.As_Bare_Lkt_Node);
         when Mmz_Bare_Module_Id =>
            return Hash (Key.As_Bare_Module_Id);
         when Mmz_Bare_Ref_Id =>
            return Hash (Key.As_Bare_Ref_Id);
         when Mmz_Bare_Struct_Decl =>
            return Hash (Key.As_Bare_Struct_Decl);
         when Mmz_Bare_Type_Decl =>
            return Hash (Key.As_Bare_Type_Decl);
         when Mmz_Bare_Type_Ref =>
            return Hash (Key.As_Bare_Type_Ref);
         when Mmz_Boolean =>
            return Hash (Key.As_Boolean);
         when Mmz_Env_Rebindings =>
            return Hash (Key.As_Env_Rebindings);
         when Mmz_Internal_Entity =>
            return Hash (Key.As_Internal_Entity);
         when Mmz_Internal_Entity_Argument_List =>
            return Hash (Key.As_Internal_Entity_Argument_List);
         when Mmz_Internal_Entity_Decl =>
            return Hash (Key.As_Internal_Entity_Decl);
         when Mmz_Internal_Entity_Info =>
            return Hash (Key.As_Internal_Entity_Info);
         when Mmz_Internal_Entity_Type_Decl =>
            return Hash (Key.As_Internal_Entity_Type_Decl);
         when Mmz_Internal_Entity_Type_Decl_Array_Access =>
            return Hash (Key.As_Internal_Entity_Type_Decl_Array_Access);
         when Mmz_Internal_Metadata =>
            return Hash (Key.As_Internal_Metadata);
         when Mmz_Internal_Resolved_Param_Array_Access =>
            return Hash (Key.As_Internal_Resolved_Param_Array_Access);
         when Mmz_Symbol_Type =>
            return Hash (Key.As_Symbol_Type);
   end case;
end Hash;

----------
-- Hash --
----------

function Hash (Key : Mmz_Key) return Hash_Type is
   Result : Hash_Type := Mmz_Property'Pos (Key.Property);
begin
   for K of Key.Items.all loop
      Result := Combine (Result, Hash (K));
   end loop;
   return Result;
end Hash;

----------------
-- Equivalent --
----------------

function Equivalent (L, R : Mmz_Key) return Boolean is
   L_Items : Mmz_Key_Array renames L.Items.all;
   R_Items : Mmz_Key_Array renames R.Items.all;
begin
   if L.Property /= R.Property or else L_Items'Length /= R_Items'Length then
      return False;
   end if;

   for I in L_Items'Range loop
      if not Equivalent (L_Items (I), R_Items (I)) then
         return False;
      end if;
   end loop;

   return True;
end Equivalent;

-------------
-- Destroy --
-------------

procedure Destroy (Map : in out Memoization_Maps.Map) is
   use Memoization_Maps;

   --  We need keys and values to be valid when clearing the memoization map,
   --  but on the other hand we need to free keys and values as well. To
   --  achieve both goals, we first copy key and values into arrays, then we
   --  clear the map, and then we free keys/values in the arrays. Allocate both
   --  arrays on the heap to avoid stack overflow, as they can be quite big.

   Length : constant Natural := Natural (Map.Length);

   type Key_Array is array (1 .. Length) of Mmz_Key_Array_Access;
   type Key_Array_Access is access Key_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Key_Array, Key_Array_Access);

   type Value_Array is array (1 .. Length) of Mmz_Value;
   type Value_Array_Access is access Value_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Value_Array, Value_Array_Access);

   Keys   : Key_Array_Access := new Key_Array;
   Values : Value_Array_Access := new Value_Array;
   I      : Positive := 1;
begin
   for Cur in Map.Iterate loop
      Keys (I) := Key (Cur).Items;
      Values (I) := Element (Cur);
      I := I + 1;
   end loop;

   Map.Clear;

   for K_Array of Keys.all loop
      Destroy (K_Array);
   end loop;

   
   for V of Values.all loop
      case V.Kind is
         when Mmz_Error =>
            Free_Memoized_Error (V.Exc_Id, V.Exc_Msg);

            when Mmz_Internal_Env_Assoc_Array_Access =>
               Dec_Ref (V.As_Internal_Env_Assoc_Array_Access);
            when Mmz_Internal_Param_Match_Array_Access =>
               Dec_Ref (V.As_Internal_Param_Match_Array_Access);
            when Mmz_Internal_Resolved_Param_Array_Access =>
               Dec_Ref (V.As_Internal_Resolved_Param_Array_Access);
            when Mmz_Internal_Solver_Result =>
               Dec_Ref (V.As_Internal_Solver_Result);
            when Mmz_Lexical_Env =>
               Dec_Ref (V.As_Lexical_Env);

         when others => null;
      end case;
   end loop;

   Free (Keys);
   Free (Values);
end Destroy;

-------------
-- Destroy --
-------------

procedure Destroy (Key : in out Mmz_Key_Array_Access) is
   procedure Free is new Ada.Unchecked_Deallocation
     (Mmz_Key_Array, Mmz_Key_Array_Access);
begin
   

      for K of Key.all loop
         case K.Kind is
               when Mmz_Internal_Entity_Type_Decl_Array_Access =>
                  Dec_Ref (K.As_Internal_Entity_Type_Decl_Array_Access);
               when Mmz_Internal_Resolved_Param_Array_Access =>
                  Dec_Ref (K.As_Internal_Resolved_Param_Array_Access);

            when others => null;
         end case;
      end loop;
   Free (Key);
end Destroy;

-------------------------
-- Find_Memoized_Value --
-------------------------

function Find_Memoized_Value
  (Unit       : Internal_Unit;
   Handle     : out Memoization_Handle;
   Value      : out Mmz_Value;
   Create_Key : access function return Mmz_Key) return Boolean
is
   Inserted : Boolean;
begin
   --  Make sure that we don't lookup stale caches
   Reset_Caches (Unit);

   --  Initialize handle: create the key and create a cursor pointing to an
   --  existing entry.
   Handle.Key := Create_Key.all;
   Handle.Cache_Version := Unit.Cache_Version;
   Value := (Kind => Mmz_Evaluating);
   Unit.Memoization_Map.Insert (Handle.Key, Value, Handle.Cur, Inserted);

   --  No existing entry yet? The above just created one. Otherwise, destroy
   --  our key and reuse the existing entry's.
   if not Inserted then
      Destroy (Handle.Key.Items);
      Handle.Key := Memoization_Maps.Key (Handle.Cur);
      Value := Memoization_Maps.Element (Handle.Cur);
   end if;

   return not Inserted;
end Find_Memoized_Value;

------------------------
-- Add_Memoized_Value --
------------------------

procedure Add_Memoized_Value
  (Unit   : Internal_Unit;
   Handle : in out Memoization_Handle;
   Value  : Mmz_Value;
   Stored : out Boolean) is
begin
   --  If Handle was created using a memoization map that has been since then
   --  reset, do nothing: the result can be partly stale due to the event that
   --  triggered the memoization tables reset.

   Stored := Unit.Cache_Version <= Handle.Cache_Version;
   if Stored then
      Unit.Memoization_Map.Replace_Element (Handle.Cur, Value);
   end if;
end Add_Memoized_Value;

------------------------
-- Add_Memoized_Error --
------------------------

procedure Add_Memoized_Error
  (Unit   : Internal_Unit;
   Handle : in out Memoization_Handle;
   Exc    : Ada.Exceptions.Exception_Occurrence;
   Stored : out Boolean)
is
   Value : Mmz_Value (Kind => Mmz_Error);
begin
   Store_Memoized_Error (Exc, Value.Exc_Id, Value.Exc_Msg);
   Add_Memoized_Value (Unit, Handle, Value, Stored);
   if not Stored then
      Free_Memoized_Error (Value.Exc_Id, Value.Exc_Msg);
   end if;
end Add_Memoized_Error;

----------------------------
-- Reraise_Memoized_Error --
----------------------------

procedure Reraise_Memoized_Error (Value : Mmz_Value) is
begin
   Reraise_Memoized_Error (Value.Exc_Id, Value.Exc_Msg);
end Reraise_Memoized_Error;

--------------------------
-- Store_Memoized_Error --
--------------------------

procedure Store_Memoized_Error
  (Exc     : Ada.Exceptions.Exception_Occurrence;
   Exc_Id  : out Ada.Exceptions.Exception_Id;
   Exc_Msg : out String_Access) is
begin
   Exc_Id := Ada.Exceptions.Exception_Identity (Exc);
   Exc_Msg := new String'(Ada.Exceptions.Exception_Message (Exc));
end Store_Memoized_Error;

-------------------------
-- Free_Memoized_Error --
-------------------------

procedure Free_Memoized_Error
  (Exc_Id  : in out Ada.Exceptions.Exception_Id;
   Exc_Msg : in out String_Access)
is
   pragma Unreferenced (Exc_Id);
begin
   Free (Exc_Msg);
end Free_Memoized_Error;

----------------------------
-- Reraise_Memoized_Error --
----------------------------

procedure Reraise_Memoized_Error
  (Exc_Id  : Ada.Exceptions.Exception_Id;
   Exc_Msg : String_Access) is
begin
   Ada.Exceptions.Raise_Exception (Exc_Id, Exc_Msg.all & " (memoized)");
end Reraise_Memoized_Error;



   package Solver_Diagnostic_Vectors is new Liblktlang_Support.Vectors
     (Internal_Solver_Diagnostic);

   ----------------------------
   -- Allocate_Logic_Context --
   ----------------------------

   function Allocate_Logic_Context
     (Ctx : Internal_Logic_Context) return Internal_Logic_Context_Access
   is ((if Ctx.Ref_Node = No_Entity
           and then Ctx.Decl_Node = No_Entity
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
      Context_Node : Bare_Lkt_Node) return Boolean is
   begin
      if Context_Node /= null and then Liblktlang_Support.Adalog.Debug.Debug then
         Assign_Names_To_Logic_Vars (Context_Node);
      end if;

      begin
         return Solver.Solve_First
           (R, Timeout => Context_Node.Unit.Context.Logic_Resolution_Timeout);
      exception
         when Liblktlang_Support.Adalog.Early_Binding_Error =>
            Raise_Property_Exception
              (Context_Node,
               Property_Error'Identity,
               "invalid equation for logic resolution");
         when Liblktlang_Support.Adalog.Timeout_Error =>
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
      Context_Node : Bare_Lkt_Node) return Internal_Solver_Result
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
     (Self              : Bare_Lkt_Node;
      Kind              : Lkt_Node_Kind_Type;
      Unit              : Internal_Unit;
      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      Parent            : Bare_Lkt_Node := null;
      Self_Env          : Lexical_Env := AST_Envs.Empty_Env) is
   begin
      pragma Unreferenced (Kind);
      Self.Parent := Parent;
      Self.Unit := Unit;

      Self.Token_Start_Index := Token_Start_Index;
      Self.Token_End_Index := Token_End_Index;

      Self.Self_Env := Self_Env;
      Self.Last_Attempted_Child := -1;

      

   end Initialize;

   --------------------------------------
   -- Allocate_Synthetic_List_Children --
   --------------------------------------

   function Allocate_Synthetic_List_Children
     (Self  : Bare_Lkt_Node_Base_List;
      Count : Natural) return Alloc_AST_List_Array.Element_Array_Access
   is
      use Alloc_AST_List_Array;
      use System.Memory;

      Size : constant size_t :=
        Bare_Lkt_Node'Max_Size_In_Storage_Elements * size_t (Count);
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
     (Self : Bare_Lkt_Node_Base_List)
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
     (Self         : Bare_Lkt_Node;
      State        : in out PLE_Node_State;
      Env          : Internal_Designated_Env;
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
     (Self         : Bare_Lkt_Node;
      State        : PLE_Node_State;
      Key          : Symbol_Type;
      Value        : Bare_Lkt_Node;
      Md           : Internal_Metadata;
      Resolver     : Entity_Resolver;
      Dest_Env     : Internal_Designated_Env;
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
     (Self                : Bare_Lkt_Node;
      Dest_Env            : Lexical_Env;
      Ref_Env_Nodes       : in out Bare_Lkt_Node_Array_Access;
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
     (Self              : Bare_Lkt_Node;
      State             : in out PLE_Node_State;
      No_Parent         : Boolean;
      Transitive_Parent : Boolean;
      Names             : in out Symbol_Type_Array_Access)
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
     (Self            : Bare_Lkt_Node;
      State           : in out PLE_Node_State;
      Add_To_Env_Only : Boolean := False) is
   begin
      
   

   case Self.Kind is
            when Lkt_Decl_Block =>
            
            Decl_Block_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Lambda_Expr =>
            
            Lambda_Expr_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Block_Expr =>
            
            Block_Expr_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Enum_Type_Decl =>
            
            Enum_Type_Decl_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Enum_Class_Decl =>
            
            Enum_Class_Decl_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Enum_Class_Alt_Decl =>
            
            null;
      
            when Lkt_Lexer_Decl =>
            
            Lexer_Decl_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Langkit_Root =>
            
            Langkit_Root_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Grammar_Decl =>
            
            Grammar_Decl_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Generic_Decl =>
            
            Generic_Decl_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Env_Spec_Decl =>
            
            Env_Spec_Decl_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Fun_Decl =>
            
            Fun_Decl_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Field_Decl =>
            
            Field_Decl_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Enum_Lit_Decl =>
            
            null;
      
            when Lkt_Grammar_Rule_Decl .. Lkt_Binding_Val_Decl | Lkt_Fun_Param_Decl .. Lkt_Val_Decl | Lkt_Error_Decl | Lkt_Lexer_Family_Decl .. Lkt_Any_Type_Decl | Lkt_Function_Type .. Lkt_Class_Decl | Lkt_Struct_Decl .. Lkt_Trait_Decl =>
            
            Decl_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Match_Branch .. Lkt_Pattern_Match_Branch =>
            
            Base_Match_Branch_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Import_From =>
            
            Import_From_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Import_All_From =>
            
            Import_All_From_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
            when Lkt_Import =>
            
            Import_Pre_Env_Actions (Self, State, Add_To_Env_Only);
      
      when others =>  null; 
   end case;


   end Pre_Env_Actions;

   ----------------------
   -- Post_Env_Actions --
   ----------------------

   procedure Post_Env_Actions
     (Self : Bare_Lkt_Node; State : in out PLE_Node_State) is
   begin
      
   

   case Self.Kind is
            when Lkt_Decl_Block =>
            
            null;
      
            when Lkt_Lambda_Expr =>
            
            null;
      
            when Lkt_Block_Expr =>
            
            null;
      
            when Lkt_Enum_Type_Decl =>
            
            Enum_Type_Decl_Post_Env_Actions (Self, State);
      
            when Lkt_Enum_Class_Decl =>
            
            Enum_Class_Decl_Post_Env_Actions (Self, State);
      
            when Lkt_Enum_Class_Alt_Decl =>
            
            null;
      
            when Lkt_Lexer_Decl =>
            
            null;
      
            when Lkt_Langkit_Root =>
            
            null;
      
            when Lkt_Grammar_Decl =>
            
            null;
      
            when Lkt_Generic_Decl =>
            
            null;
      
            when Lkt_Env_Spec_Decl =>
            
            null;
      
            when Lkt_Fun_Decl =>
            
            null;
      
            when Lkt_Field_Decl =>
            
            null;
      
            when Lkt_Enum_Lit_Decl =>
            
            null;
      
            when Lkt_Grammar_Rule_Decl .. Lkt_Binding_Val_Decl | Lkt_Fun_Param_Decl .. Lkt_Val_Decl | Lkt_Error_Decl | Lkt_Lexer_Family_Decl .. Lkt_Any_Type_Decl | Lkt_Function_Type .. Lkt_Class_Decl | Lkt_Struct_Decl .. Lkt_Trait_Decl =>
            
            null;
      
            when Lkt_Match_Branch .. Lkt_Pattern_Match_Branch =>
            
            null;
      
            when Lkt_Import_From =>
            
            null;
      
            when Lkt_Import_All_From =>
            
            null;
      
            when Lkt_Import =>
            
            null;
      
      when others =>  null; 
   end case;


   end Post_Env_Actions;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Node : Bare_Lkt_Node) return Symbol_Type is
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
     (Node : Bare_Lkt_Node) return Text_Type
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

   function Unit (Node : Bare_Lkt_Node) return Internal_Unit is
   begin
      return Node.Unit;
   end Unit;

   function Lookup_Internal
     (Node : Bare_Lkt_Node;
      Sloc : Source_Location) return Bare_Lkt_Node;
   procedure Lookup_Relative
     (Node       : Bare_Lkt_Node;
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out Bare_Lkt_Node);
   --  Implementation helpers for the looking up process

   -----------------
   -- Set_Parents --
   -----------------

   procedure Set_Parents
     (Node, Parent : Bare_Lkt_Node)
   is
   begin
      if Node = null then
         return;
      end if;

      Node.Parent := Bare_Lkt_Node (Parent);

      for I in 1 .. Children_Count (Node) loop
         Set_Parents (Child (Node, I), Node);
      end loop;
   end Set_Parents;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Node : Bare_Lkt_Node) is
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

   function Child (Node  : Bare_Lkt_Node;
                   Index : Positive) return Bare_Lkt_Node
   is
      Result          : Bare_Lkt_Node;
      Index_In_Bounds : Boolean;
   begin
      Get_Child (Node, Index, Index_In_Bounds, Result);
      return Result;
   end Child;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : Bare_Lkt_Node;
      Visit : access function (Node : Bare_Lkt_Node)
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
                  Cur_Child : constant Bare_Lkt_Node :=
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
     (Node  : Bare_Lkt_Node;
      Visit : access function (Node : Bare_Lkt_Node)
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
     (Node  : Bare_Lkt_Node;
      Visit : access function (Node : Bare_Lkt_Node;
                               Data : in out Data_Type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status
   is
      function Helper (Node : Bare_Lkt_Node) return Visit_Status;

      ------------
      -- Helper --
      ------------

      function Helper (Node : Bare_Lkt_Node) return Visit_Status is
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
     (Node : Bare_Lkt_Node) return Source_Location_Range
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
     (Node : Bare_Lkt_Node;
      Sloc : Source_Location) return Bare_Lkt_Node
   is
      Position : Relative_Position;
      Result   : Bare_Lkt_Node;
   begin
      if Sloc = No_Source_Location then
         return null;
      end if;

      Lookup_Relative
        (Bare_Lkt_Node (Node), Sloc, Position, Result);
      return Result;
   end Lookup;

   ---------------------
   -- Lookup_Internal --
   ---------------------

   function Lookup_Internal
     (Node : Bare_Lkt_Node;
      Sloc : Source_Location) return Bare_Lkt_Node
   is
      --  For this implementation helper (i.e. internal primitive), we can
      --  assume that all lookups fall into this node's sloc range.
      pragma Assert (Compare (Sloc_Range (Node), Sloc) = Inside);

      Children : constant Internal_Bare_Lkt_Node_Array :=
         Implementation.Children (Node);
      Pos      : Relative_Position;
      Result   : Bare_Lkt_Node;
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
     (Node : Bare_Lkt_Node;
      Sloc : Source_Location) return Relative_Position is
   begin
      return Compare (Sloc_Range (Node), Sloc);
   end Compare;

   ---------------------
   -- Lookup_Relative --
   ---------------------

   procedure Lookup_Relative
     (Node       : Bare_Lkt_Node;
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out Bare_Lkt_Node)
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
     (Self, Left, Right : Bare_Lkt_Node;
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
              when Liblktlang_Support.Types.Less_Than        => LS < RS,
              when Liblktlang_Support.Types.Less_Or_Equal    => LS <= RS,
              when Liblktlang_Support.Types.Greater_Than     => LS > RS,
              when Liblktlang_Support.Types.Greater_Or_Equal => LS >= RS);
   end Compare;

   --------------
   -- Children --
   --------------

   function Children
     (Node : Bare_Lkt_Node) return Internal_Bare_Lkt_Node_Array
   is
      First : constant Integer := Bare_Lkt_Node_Vectors.Index_Type'First;
      Last  : constant Integer := First + Children_Count (Node) - 1;
   begin
      return A : Internal_Bare_Lkt_Node_Array (First .. Last)
      do
         for I in First .. Last loop
            A (I) := Child (Node, I);
         end loop;
      end return;
   end Children;

   function Children
     (Node : Bare_Lkt_Node) return Bare_Lkt_Node_Array_Access
   is
      C : Internal_Bare_Lkt_Node_Array := Children (Node);
   begin
      return Ret : Bare_Lkt_Node_Array_Access :=
         Create_Bare_Lkt_Node_Array (C'Length)
      do
         Ret.Items := C;
      end return;
   end Children;

   ---------
   -- Get --
   ---------

   function Get
     (Self    : Bare_Lkt_Node;
      Node    : Bare_Lkt_Node_Base_List;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Lkt_Node
   is
      function Length (Node : Bare_Lkt_Node_Base_List) return Natural
      is (Node.Count);
      --  Wrapper around the Length primitive to get the compiler happy for the
      --  the package instantiation below.

      function Absolute_Get
        (L     : Bare_Lkt_Node_Base_List;
         Index : Integer) return Bare_Lkt_Node
      is (L.Nodes.all (Index + 1));
      --  L.Nodes is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => Bare_Lkt_Node,
         Sequence_Type => Bare_Lkt_Node_Base_List,
         Length        => Length,
         Get           => Absolute_Get);

      Result : Bare_Lkt_Node;
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
     (Node        : Bare_Lkt_Node;
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

   function Populate_Lexical_Env (Node : Bare_Lkt_Node) return Boolean is

      Context    : constant Internal_Context := Node.Unit.Context;
      Unit_State : aliased PLE_Unit_State := (Named_Envs_Needing_Update => <>);
      Root_State : constant PLE_Node_State :=
        (Unit_State  => Unit_State'Unchecked_Access,
         Current_Env => Context.Root_Scope,
         Current_NED => null);

      function Populate_Internal
        (Node         : Bare_Lkt_Node;
         Parent_State : PLE_Node_State) return Boolean;
      --  Do the lexical env population on Node and recurse on its children

      procedure Register_Foreign_Env
        (Node : Bare_Lkt_Node; State : PLE_Node_State);
      --  Given a node and its PLE state, register Node.Self_Env as being
      --  initialized through the named environment mechanism, if that's indeed
      --  the case. Do nothing otherwise.

      -----------------------
      -- Populate_Internal --
      -----------------------

      function Populate_Internal
        (Node         : Bare_Lkt_Node;
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
            when Exc : Liblktlang_Support.Errors.Property_Error =>
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
        (Node : Bare_Lkt_Node; State : PLE_Node_State) is
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
     (Node  : Bare_Lkt_Node;
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

   function Is_Rebindable (Node : Bare_Lkt_Node) return Boolean is
   begin
      
         return Node.Kind in Lkt_Generic_Decl;
   end Is_Rebindable;

   -----------------------
   -- Acquire_Rebinding --
   -----------------------

   function Acquire_Rebinding
     (Node             : Bare_Lkt_Node;
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
     (Node : Bare_Lkt_Node; Rebinding : Env_Rebindings) is
   begin
      Node.Unit.Rebindings.Append (Rebinding);
   end Register_Rebinding;


   --------------------
   -- Element_Parent --
   --------------------

   function Element_Parent
     (Node : Bare_Lkt_Node) return Bare_Lkt_Node
   is (Node.Parent);

   ---------------
   -- Node_Unit --
   ---------------

   function Node_Unit (Node : Bare_Lkt_Node) return Generic_Unit_Ptr is
   begin
      return Convert_Unit (Node.Unit);
   end Node_Unit;

   ----------
   -- Hash --
   ----------

   function Hash (Node : Bare_Lkt_Node) return Hash_Type
   is
      function H is new Hash_Access
        (Root_Node_Record, Bare_Lkt_Node);
   begin
      return H (Node);
   end Hash;

      function Hash (B : Boolean) return Hash_Type is (Boolean'Pos (B));





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
      Node : Bare_Lkt_Node)
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
      Node       : constant Bare_Lkt_Node := AST_Envs.Env_Node (Env);
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
                     Key   : constant Symbol_Type :=
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
                  Key   : constant Symbol_Type :=
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

   -----------------
   -- Trace_Image --
   -----------------

   function Trace_Image (I : Big_Integer_Type) return String is
   begin
      return GNATCOLL.GMP.Integers.Image (I.Value);
   end Trace_Image;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer
     (Self    : Bare_Lkt_Node;
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
     (Node : Bare_Lkt_Node) return Version_Number is
   begin
      return Node.Unit.Context.Cache_Version;
   end Get_Context_Version;

   ---------------
   --  Self_Env --
   ---------------

   function Self_Env (Node : Bare_Lkt_Node) return Lexical_Env is
   begin
      return Node.Self_Env;
   end Self_Env;

   --------------------------
   -- Properties_May_Raise --
   --------------------------

   function Properties_May_Raise
     (Id : Ada.Exceptions.Exception_Id) return Boolean is
   begin
      return Id in
        Liblktlang_Support.Errors.Property_Error'Identity;
   end Properties_May_Raise;

   ----------------------
   -- Short_Text_Image --
   ----------------------

   function Short_Text_Image (Self : Bare_Lkt_Node) return Text_Type
   is
   begin
      if Self = null then
         return "None";
      end if;

      
   

   case Self.Kind is
            when Lkt_Id .. Lkt_Ref_Id =>
            
         return Id_Short_Image (Self);
      
            when Lkt_Grammar_Rule_Decl .. Lkt_Trait_Decl =>
            
         return Decl_Short_Image (Self);
      
      when others => 
         return "<" & To_Text (Kind_Name (Self))
                & " " & Node_Sloc_Image (Self) & ">";
      
   end case;

   end Short_Text_Image;

   ----------------------
   --- Node_Sloc_Image --
   ----------------------

   function Node_Sloc_Image (Self : Bare_Lkt_Node) return Text_Type
   is
      
   begin
         return To_Text
                  (Ada.Directories.Simple_Name (Get_Filename (Unit (Self))))
                & ":" & To_Text (Image (Sloc_Range (Self)));
   end Node_Sloc_Image;

   --------------------
   -- Snaps_At_Start --
   --------------------

   function Snaps_At_Start (Self : Bare_Lkt_Node) return Boolean is
   begin
      
   

   case Self.Kind is
      when others => 
         return False;
      
   end case;

   end Snaps_At_Start;

   ------------------
   -- Snaps_At_End --
   ------------------

   function Snaps_At_End (Self : Bare_Lkt_Node) return Boolean is
   begin
      
   

   case Self.Kind is
      when others => 
         return Is_Incomplete (Self);
      
   end case;

   end Snaps_At_End;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node      : Bare_Lkt_Node;
      With_Self : Boolean := True)
      return Bare_Lkt_Node_Array_Access
   is
      Count : Natural := 0;
      Start : Bare_Lkt_Node :=
        (if With_Self then Node else Node.Parent);
      Cur   : Bare_Lkt_Node := Start;
   begin
      while Cur /= null loop
         Count := Count + 1;
         Cur := Cur.Parent;
      end loop;

      declare
         Result : constant Bare_Lkt_Node_Array_Access :=
            Create_Bare_Lkt_Node_Array (Count);
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

   function First_Child_Index (Node : Bare_Lkt_Node) return Natural
   is (1);

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index (Node : Bare_Lkt_Node) return Natural
   is (Children_Count (Node));

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node            : Bare_Lkt_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Bare_Lkt_Node)
   is
      K : constant Lkt_Node_Kind_Type := Node.Kind;
   begin
      

      Index_In_Bounds := True;
      Result := null;
      case Lkt_Lkt_Node (K) is
when Lkt_Argument_Range =>
declare
N_Bare_Argument : constant Bare_Argument := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Argument.Argument_F_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Argument.Argument_F_Value;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Import_Range =>
declare
N_Bare_Import : constant Bare_Import := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Import.Import_F_Imported_Names;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Import_All_From_Range =>
declare
N_Bare_Import_All_From : constant Bare_Import_All_From := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Import_All_From.Import_All_From_F_Module_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Import_From_Range =>
declare
N_Bare_Import_From : constant Bare_Import_From := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Import_From.Import_From_F_Module_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Import_From.Import_From_F_Imported_Names;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Lexer_Case_Rule_Cond_Alt_Range =>
declare
N_Bare_Lexer_Case_Rule_Cond_Alt : constant Bare_Lexer_Case_Rule_Cond_Alt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Lexer_Case_Rule_Cond_Alt.Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Lexer_Case_Rule_Cond_Alt.Lexer_Case_Rule_Cond_Alt_F_Send;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Lexer_Case_Rule_Default_Alt_Range =>
declare
N_Bare_Lexer_Case_Rule_Default_Alt : constant Bare_Lexer_Case_Rule_Default_Alt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Lexer_Case_Rule_Default_Alt.Lexer_Case_Rule_Default_Alt_F_Send;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Match_Branch_Range =>
declare
N_Bare_Match_Branch : constant Bare_Match_Branch := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Match_Branch.Match_Branch_F_Decl;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Match_Branch.Match_Branch_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Pattern_Match_Branch_Range =>
declare
N_Bare_Pattern_Match_Branch : constant Bare_Pattern_Match_Branch := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Pattern_Match_Branch.Pattern_Match_Branch_F_Pattern;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Pattern_Match_Branch.Pattern_Match_Branch_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Block_Expr_Clause_Range =>
declare
N_Bare_Block_Expr_Clause : constant Bare_Block_Expr_Clause := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Block_Expr_Clause.Block_Expr_Clause_F_Clause;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Rule_Decl_Range =>
declare
N_Bare_Grammar_Rule_Decl : constant Bare_Grammar_Rule_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Rule_Decl.Grammar_Rule_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Grammar_Rule_Decl.Grammar_Rule_Decl_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Binding_Val_Decl_Range =>
declare
N_Bare_Binding_Val_Decl : constant Bare_Binding_Val_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Binding_Val_Decl.Binding_Val_Decl_F_Syn_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Enum_Lit_Decl_Range =>
declare
N_Bare_Enum_Lit_Decl : constant Bare_Enum_Lit_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Enum_Lit_Decl.Enum_Lit_Decl_F_Syn_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Field_Decl_Range =>
declare
N_Bare_Field_Decl : constant Bare_Field_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Field_Decl.Field_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Field_Decl.Field_Decl_F_Decl_Type;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Field_Decl.Field_Decl_F_Trait_Ref;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Field_Decl.Field_Decl_F_Default_Val;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Fun_Param_Decl_Range =>
declare
N_Bare_Fun_Param_Decl : constant Bare_Fun_Param_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Fun_Param_Decl.Fun_Param_Decl_F_Decl_Annotations;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Fun_Param_Decl.Fun_Param_Decl_F_Syn_Name;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Fun_Param_Decl.Fun_Param_Decl_F_Decl_Type;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Fun_Param_Decl.Fun_Param_Decl_F_Default_Val;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Lambda_Param_Decl_Range =>
declare
N_Bare_Lambda_Param_Decl : constant Bare_Lambda_Param_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Lambda_Param_Decl.Lambda_Param_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Lambda_Param_Decl.Lambda_Param_Decl_F_Decl_Type;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Lambda_Param_Decl.Lambda_Param_Decl_F_Default_Val;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Dyn_Var_Decl_Range =>
declare
N_Bare_Dyn_Var_Decl : constant Bare_Dyn_Var_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Dyn_Var_Decl.Dyn_Var_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Dyn_Var_Decl.Dyn_Var_Decl_F_Decl_Type;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Match_Val_Decl_Range =>
declare
N_Bare_Match_Val_Decl : constant Bare_Match_Val_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Match_Val_Decl.Match_Val_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Match_Val_Decl.Match_Val_Decl_F_Decl_Type;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Val_Decl_Range =>
declare
N_Bare_Val_Decl : constant Bare_Val_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Val_Decl.Val_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Val_Decl.Val_Decl_F_Decl_Type;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Val_Decl.Val_Decl_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Fun_Decl_Range =>
declare
N_Bare_Fun_Decl : constant Bare_Fun_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Fun_Decl.Fun_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Fun_Decl.Fun_Decl_F_Params;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Fun_Decl.Fun_Decl_F_Return_Type;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Fun_Decl.Fun_Decl_F_Trait_Ref;
                            return;
                    

                        when 5 =>
                            Result := N_Bare_Fun_Decl.Fun_Decl_F_Body;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Env_Spec_Decl_Range =>
declare
N_Bare_Env_Spec_Decl : constant Bare_Env_Spec_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Env_Spec_Decl.Env_Spec_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Env_Spec_Decl.Env_Spec_Decl_F_Actions;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Generic_Decl_Range =>
declare
N_Bare_Generic_Decl : constant Bare_Generic_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Generic_Decl.Generic_Decl_F_Generic_Param_Decls;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Generic_Decl.Generic_Decl_F_Decl;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Decl_Range =>
declare
N_Bare_Grammar_Decl : constant Bare_Grammar_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Decl.Grammar_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Grammar_Decl.Grammar_Decl_F_Rules;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Langkit_Root_Range =>
declare
N_Bare_Langkit_Root : constant Bare_Langkit_Root := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Langkit_Root.Langkit_Root_F_Doc;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Langkit_Root.Langkit_Root_F_Imports;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Langkit_Root.Langkit_Root_F_Decls;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Lexer_Decl_Range =>
declare
N_Bare_Lexer_Decl : constant Bare_Lexer_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Lexer_Decl.Lexer_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Lexer_Decl.Lexer_Decl_F_Rules;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Lexer_Family_Decl_Range =>
declare
N_Bare_Lexer_Family_Decl : constant Bare_Lexer_Family_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Lexer_Family_Decl.Lexer_Family_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Lexer_Family_Decl.Lexer_Family_Decl_F_Rules;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Any_Type_Decl_Range =>
declare
N_Bare_Any_Type_Decl : constant Bare_Any_Type_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Any_Type_Decl.Any_Type_Decl_F_Traits;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Enum_Class_Alt_Decl_Range =>
declare
N_Bare_Enum_Class_Alt_Decl : constant Bare_Enum_Class_Alt_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Enum_Class_Alt_Decl.Enum_Class_Alt_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Enum_Class_Alt_Decl.Enum_Class_Alt_Decl_F_Traits;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Function_Type_Range =>
declare
N_Bare_Function_Type : constant Bare_Function_Type := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Function_Type.Function_Type_F_Traits;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Generic_Param_Type_Decl_Range =>
declare
N_Bare_Generic_Param_Type_Decl : constant Bare_Generic_Param_Type_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Generic_Param_Type_Decl.Generic_Param_Type_Decl_F_Has_Class;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Generic_Param_Type_Decl.Generic_Param_Type_Decl_F_Syn_Name;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Generic_Param_Type_Decl.Generic_Param_Type_Decl_F_Traits;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Basic_Class_Decl =>
declare
N_Bare_Basic_Class_Decl : constant Bare_Basic_Class_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Basic_Class_Decl.Basic_Class_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Basic_Class_Decl.Basic_Class_Decl_F_Syn_Base_Type;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Basic_Class_Decl.Basic_Class_Decl_F_Traits;
                            return;
                    

                        when others => null;
                    end case;
                
case Lkt_Basic_Class_Decl (K) is
when Lkt_Class_Decl_Range =>
declare
N_Bare_Class_Decl : constant Bare_Class_Decl := N_Bare_Basic_Class_Decl;
begin
case Index is

                        when 4 =>
                            Result := N_Bare_Class_Decl.Class_Decl_F_Decls;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Enum_Class_Decl_Range =>
declare
N_Bare_Enum_Class_Decl : constant Bare_Enum_Class_Decl := N_Bare_Basic_Class_Decl;
begin
case Index is

                        when 4 =>
                            Result := N_Bare_Enum_Class_Decl.Enum_Class_Decl_F_Branches;
                            return;
                    

                        when 5 =>
                            Result := N_Bare_Enum_Class_Decl.Enum_Class_Decl_F_Decls;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when others => null;
end case;
end;
when Lkt_Enum_Type_Decl_Range =>
declare
N_Bare_Enum_Type_Decl : constant Bare_Enum_Type_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Enum_Type_Decl.Enum_Type_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Enum_Type_Decl.Enum_Type_Decl_F_Traits;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Enum_Type_Decl.Enum_Type_Decl_F_Literals;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Enum_Type_Decl.Enum_Type_Decl_F_Decls;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Struct_Decl_Range =>
declare
N_Bare_Struct_Decl : constant Bare_Struct_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Struct_Decl.Struct_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Struct_Decl.Struct_Decl_F_Traits;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Struct_Decl.Struct_Decl_F_Decls;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Trait_Decl_Range =>
declare
N_Bare_Trait_Decl : constant Bare_Trait_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Trait_Decl.Trait_Decl_F_Syn_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Trait_Decl.Trait_Decl_F_Traits;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Trait_Decl.Trait_Decl_F_Decls;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Decl_Annotation_Range =>
declare
N_Bare_Decl_Annotation : constant Bare_Decl_Annotation := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Decl_Annotation.Decl_Annotation_F_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Decl_Annotation.Decl_Annotation_F_Args;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Decl_Annotation_Args_Range =>
declare
N_Bare_Decl_Annotation_Args : constant Bare_Decl_Annotation_Args := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Decl_Annotation_Args.Decl_Annotation_Args_F_Args;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Elsif_Branch_Range =>
declare
N_Bare_Elsif_Branch : constant Bare_Elsif_Branch := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Elsif_Branch.Elsif_Branch_F_Cond_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Elsif_Branch.Elsif_Branch_F_Then_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Enum_Class_Case_Range =>
declare
N_Bare_Enum_Class_Case : constant Bare_Enum_Class_Case := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Enum_Class_Case.Enum_Class_Case_F_Decls;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Any_Of_Range =>
declare
N_Bare_Any_Of : constant Bare_Any_Of := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Any_Of.Any_Of_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Any_Of.Any_Of_F_Values;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Array_Literal_Range =>
declare
N_Bare_Array_Literal : constant Bare_Array_Literal := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Array_Literal.Array_Literal_F_Exprs;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Array_Literal.Array_Literal_F_Element_Type;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Base_Call_Expr =>
declare
N_Bare_Base_Call_Expr : constant Bare_Base_Call_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Base_Call_Expr.Base_Call_Expr_F_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Base_Call_Expr.Base_Call_Expr_F_Args;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Bin_Op_Range =>
declare
N_Bare_Bin_Op : constant Bare_Bin_Op := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Bin_Op.Bin_Op_F_Left;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Bin_Op.Bin_Op_F_Op;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Bin_Op.Bin_Op_F_Right;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Block_Expr_Range =>
declare
N_Bare_Block_Expr : constant Bare_Block_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Block_Expr.Block_Expr_F_Clauses;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Cast_Expr_Range =>
declare
N_Bare_Cast_Expr : constant Bare_Cast_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Cast_Expr.Cast_Expr_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Cast_Expr.Cast_Expr_F_Null_Cond;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Cast_Expr.Cast_Expr_F_Excludes_Null;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Cast_Expr.Cast_Expr_F_Dest_Type;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Dot_Expr_Range =>
declare
N_Bare_Dot_Expr : constant Bare_Dot_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Dot_Expr.Dot_Expr_F_Prefix;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Dot_Expr.Dot_Expr_F_Null_Cond;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Dot_Expr.Dot_Expr_F_Suffix;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Error_On_Null_Range =>
declare
N_Bare_Error_On_Null : constant Bare_Error_On_Null := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Error_On_Null.Error_On_Null_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Generic_Instantiation_Range =>
declare
N_Bare_Generic_Instantiation : constant Bare_Generic_Instantiation := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Generic_Instantiation.Generic_Instantiation_F_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Generic_Instantiation.Generic_Instantiation_F_Args;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Discard_Range =>
declare
N_Bare_Grammar_Discard : constant Bare_Grammar_Discard := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Discard.Grammar_Discard_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Dont_Skip_Range =>
declare
N_Bare_Grammar_Dont_Skip : constant Bare_Grammar_Dont_Skip := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Dont_Skip.Grammar_Dont_Skip_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Grammar_Dont_Skip.Grammar_Dont_Skip_F_Dont_Skip;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_List_Range =>
declare
N_Bare_Grammar_List : constant Bare_Grammar_List := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_List.Grammar_List_F_List_Type;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Grammar_List.Grammar_List_F_Kind;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Grammar_List.Grammar_List_F_Expr;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Grammar_List.Grammar_List_F_Sep;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Null_Range =>
declare
N_Bare_Grammar_Null : constant Bare_Grammar_Null := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Null.Grammar_Null_F_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Opt_Range =>
declare
N_Bare_Grammar_Opt : constant Bare_Grammar_Opt := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Opt.Grammar_Opt_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Opt_Error_Range =>
declare
N_Bare_Grammar_Opt_Error : constant Bare_Grammar_Opt_Error := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Opt_Error.Grammar_Opt_Error_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Opt_Error_Group_Range =>
declare
N_Bare_Grammar_Opt_Error_Group : constant Bare_Grammar_Opt_Error_Group := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Opt_Error_Group.Grammar_Opt_Error_Group_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Opt_Group_Range =>
declare
N_Bare_Grammar_Opt_Group : constant Bare_Grammar_Opt_Group := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Opt_Group.Grammar_Opt_Group_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Or_Expr_Range =>
declare
N_Bare_Grammar_Or_Expr : constant Bare_Grammar_Or_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Or_Expr.Grammar_Or_Expr_F_Sub_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Pick_Range =>
declare
N_Bare_Grammar_Pick : constant Bare_Grammar_Pick := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Pick.Grammar_Pick_F_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Predicate_Range =>
declare
N_Bare_Grammar_Predicate : constant Bare_Grammar_Predicate := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Predicate.Grammar_Predicate_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Grammar_Predicate.Grammar_Predicate_F_Prop_Ref;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Rule_Ref_Range =>
declare
N_Bare_Grammar_Rule_Ref : constant Bare_Grammar_Rule_Ref := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Rule_Ref.Grammar_Rule_Ref_F_Node_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Skip_Range =>
declare
N_Bare_Grammar_Skip : constant Bare_Grammar_Skip := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Skip.Grammar_Skip_F_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_Stop_Cut_Range =>
declare
N_Bare_Grammar_Stop_Cut : constant Bare_Grammar_Stop_Cut := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_Stop_Cut.Grammar_Stop_Cut_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Parse_Node_Expr_Range =>
declare
N_Bare_Parse_Node_Expr : constant Bare_Parse_Node_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Parse_Node_Expr.Parse_Node_Expr_F_Node_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Parse_Node_Expr.Parse_Node_Expr_F_Sub_Exprs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Token_No_Case_Lit_Range =>
declare
N_Bare_Token_No_Case_Lit : constant Bare_Token_No_Case_Lit := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Token_No_Case_Lit.Token_No_Case_Lit_F_Lit;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Token_Pattern_Concat_Range =>
declare
N_Bare_Token_Pattern_Concat : constant Bare_Token_Pattern_Concat := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Token_Pattern_Concat.Token_Pattern_Concat_F_Left;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Token_Pattern_Concat.Token_Pattern_Concat_F_Right;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Token_Ref_Range =>
declare
N_Bare_Token_Ref : constant Bare_Token_Ref := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Token_Ref.Token_Ref_F_Token_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Token_Ref.Token_Ref_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_If_Expr_Range =>
declare
N_Bare_If_Expr : constant Bare_If_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_If_Expr.If_Expr_F_Cond_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_If_Expr.If_Expr_F_Then_Expr;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_If_Expr.If_Expr_F_Alternatives;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_If_Expr.If_Expr_F_Else_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Isa_Range =>
declare
N_Bare_Isa : constant Bare_Isa := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Isa.Isa_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Isa.Isa_F_Pattern;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Keep_Expr_Range =>
declare
N_Bare_Keep_Expr : constant Bare_Keep_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Keep_Expr.Keep_Expr_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Keep_Expr.Keep_Expr_F_Null_Cond;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Keep_Expr.Keep_Expr_F_Keep_Type;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Lambda_Expr_Range =>
declare
N_Bare_Lambda_Expr : constant Bare_Lambda_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Lambda_Expr.Lambda_Expr_F_Params;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Lambda_Expr.Lambda_Expr_F_Return_Type;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Lambda_Expr.Lambda_Expr_F_Body;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Null_Lit_Range =>
declare
N_Bare_Null_Lit : constant Bare_Null_Lit := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Null_Lit.Null_Lit_F_Dest_Type;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Block_String_Lit_Range =>
declare
N_Bare_Block_String_Lit : constant Bare_Block_String_Lit := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Block_String_Lit.Block_String_Lit_F_Lines;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Module_Doc_String_Lit_Range =>
declare
N_Bare_Module_Doc_String_Lit : constant Bare_Module_Doc_String_Lit := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Module_Doc_String_Lit.Module_Doc_String_Lit_F_Lines;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Logic_Assign_Range =>
declare
N_Bare_Logic_Assign : constant Bare_Logic_Assign := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Logic_Assign.Logic_Assign_F_Dest_Var;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Logic_Assign.Logic_Assign_F_Value;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Logic_Expr_Range =>
declare
N_Bare_Logic_Expr : constant Bare_Logic_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Logic_Expr.Logic_Expr_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Logic_Propagate_Range =>
declare
N_Bare_Logic_Propagate : constant Bare_Logic_Propagate := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Logic_Propagate.Logic_Propagate_F_Dest_Var;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Logic_Propagate.Logic_Propagate_F_Call;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Logic_Unify_Range =>
declare
N_Bare_Logic_Unify : constant Bare_Logic_Unify := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Logic_Unify.Logic_Unify_F_Lhs;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Logic_Unify.Logic_Unify_F_Rhs;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Match_Expr_Range =>
declare
N_Bare_Match_Expr : constant Bare_Match_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Match_Expr.Match_Expr_F_Match_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Match_Expr.Match_Expr_F_Branches;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Not_Expr_Range =>
declare
N_Bare_Not_Expr : constant Bare_Not_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Not_Expr.Not_Expr_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Paren_Expr_Range =>
declare
N_Bare_Paren_Expr : constant Bare_Paren_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Paren_Expr.Paren_Expr_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Query_Range =>
declare
N_Bare_Query : constant Bare_Query := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Query.Query_F_Source;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Query.Query_F_Pattern;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Query.Query_F_Mapping;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Query.Query_F_Guard;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Raise_Expr_Range =>
declare
N_Bare_Raise_Expr : constant Bare_Raise_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Raise_Expr.Raise_Expr_F_Dest_Type;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Raise_Expr.Raise_Expr_F_Except_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Subscript_Expr_Range =>
declare
N_Bare_Subscript_Expr : constant Bare_Subscript_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Subscript_Expr.Subscript_Expr_F_Prefix;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Subscript_Expr.Subscript_Expr_F_Null_Cond;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Subscript_Expr.Subscript_Expr_F_Index;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Try_Expr_Range =>
declare
N_Bare_Try_Expr : constant Bare_Try_Expr := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Try_Expr.Try_Expr_F_Try_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Try_Expr.Try_Expr_F_Or_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Un_Op_Range =>
declare
N_Bare_Un_Op : constant Bare_Un_Op := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Un_Op.Un_Op_F_Op;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Un_Op.Un_Op_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Full_Decl_Range =>
declare
N_Bare_Full_Decl : constant Bare_Full_Decl := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Full_Decl.Full_Decl_F_Doc;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Full_Decl.Full_Decl_F_Decl_Annotations;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Full_Decl.Full_Decl_F_Decl;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Grammar_List_Sep_Range =>
declare
N_Bare_Grammar_List_Sep : constant Bare_Grammar_List_Sep := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Grammar_List_Sep.Grammar_List_Sep_F_Token;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Grammar_List_Sep.Grammar_List_Sep_F_Extra;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Imported_Name_Range =>
declare
N_Bare_Imported_Name : constant Bare_Imported_Name := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Imported_Name.Imported_Name_F_Original_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Imported_Name.Imported_Name_F_Renaming;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Lexer_Case_Rule_Range =>
declare
N_Bare_Lexer_Case_Rule : constant Bare_Lexer_Case_Rule := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Lexer_Case_Rule.Lexer_Case_Rule_F_Expr;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Lexer_Case_Rule.Lexer_Case_Rule_F_Alts;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Lexer_Case_Rule_Send_Range =>
declare
N_Bare_Lexer_Case_Rule_Send : constant Bare_Lexer_Case_Rule_Send := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Lexer_Case_Rule_Send.Lexer_Case_Rule_Send_F_Sent;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Lexer_Case_Rule_Send.Lexer_Case_Rule_Send_F_Match_Size;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Lkt_Node_Base_List =>
declare
N_Bare_Lkt_Node_Base_List : constant Bare_Lkt_Node_Base_List := Node;
begin

                    if Index > N_Bare_Lkt_Node_Base_List.Count then
                        Index_In_Bounds := False;
                    else
                        Result := N_Bare_Lkt_Node_Base_List.Nodes (Index);
                    end if;
                    return;
                
end;
when Lkt_Complex_Pattern_Range =>
declare
N_Bare_Complex_Pattern : constant Bare_Complex_Pattern := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Complex_Pattern.Complex_Pattern_F_Decl;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Complex_Pattern.Complex_Pattern_F_Pattern;
                            return;
                    

                        when 3 =>
                            Result := N_Bare_Complex_Pattern.Complex_Pattern_F_Details;
                            return;
                    

                        when 4 =>
                            Result := N_Bare_Complex_Pattern.Complex_Pattern_F_Predicate;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Ellipsis_Pattern_Range =>
declare
N_Bare_Ellipsis_Pattern : constant Bare_Ellipsis_Pattern := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Ellipsis_Pattern.Ellipsis_Pattern_F_Binding;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_List_Pattern_Range =>
declare
N_Bare_List_Pattern : constant Bare_List_Pattern := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_List_Pattern.List_Pattern_F_Sub_Patterns;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Not_Pattern_Range =>
declare
N_Bare_Not_Pattern : constant Bare_Not_Pattern := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Not_Pattern.Not_Pattern_F_Sub_Pattern;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Or_Pattern_Range =>
declare
N_Bare_Or_Pattern : constant Bare_Or_Pattern := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Or_Pattern.Or_Pattern_F_Left_Sub_Pattern;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Or_Pattern.Or_Pattern_F_Right_Sub_Pattern;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Paren_Pattern_Range =>
declare
N_Bare_Paren_Pattern : constant Bare_Paren_Pattern := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Paren_Pattern.Paren_Pattern_F_Sub_Pattern;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Type_Pattern_Range =>
declare
N_Bare_Type_Pattern : constant Bare_Type_Pattern := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Type_Pattern.Type_Pattern_F_Type_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Destructuring_Pattern_Detail_Range =>
declare
N_Bare_Destructuring_Pattern_Detail : constant Bare_Destructuring_Pattern_Detail := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Destructuring_Pattern_Detail.Destructuring_Pattern_Detail_F_Decl;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Field_Pattern_Detail_Range =>
declare
N_Bare_Field_Pattern_Detail : constant Bare_Field_Pattern_Detail := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Field_Pattern_Detail.Field_Pattern_Detail_F_Id;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Field_Pattern_Detail.Field_Pattern_Detail_F_Expected_Value;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Property_Pattern_Detail_Range =>
declare
N_Bare_Property_Pattern_Detail : constant Bare_Property_Pattern_Detail := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Property_Pattern_Detail.Property_Pattern_Detail_F_Call;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Property_Pattern_Detail.Property_Pattern_Detail_F_Expected_Value;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Function_Type_Ref_Range =>
declare
N_Bare_Function_Type_Ref : constant Bare_Function_Type_Ref := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Function_Type_Ref.Function_Type_Ref_F_Param_Types;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Function_Type_Ref.Function_Type_Ref_F_Return_Type;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Generic_Type_Ref_Range =>
declare
N_Bare_Generic_Type_Ref : constant Bare_Generic_Type_Ref := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Generic_Type_Ref.Generic_Type_Ref_F_Type_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Generic_Type_Ref.Generic_Type_Ref_F_Args;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Simple_Type_Ref_Range =>
declare
N_Bare_Simple_Type_Ref : constant Bare_Simple_Type_Ref := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Simple_Type_Ref.Simple_Type_Ref_F_Type_Name;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when Lkt_Var_Bind_Range =>
declare
N_Bare_Var_Bind : constant Bare_Var_Bind := Node;
begin
case Index is

                        when 1 =>
                            Result := N_Bare_Var_Bind.Var_Bind_F_Name;
                            return;
                    

                        when 2 =>
                            Result := N_Bare_Var_Bind.Var_Bind_F_Expr;
                            return;
                    

                        when others => null;
                    end case;
                
end;
when others => null;
end case;

      --  Execution should reach this point iff nothing matched this index, so
      --  we must be out of bounds.
      Index_In_Bounds := False;
   end Get_Child;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : Liblktlang_Support.Generic_API.Analysis.Lk_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "")
   is
      use Liblktlang_Support.Generic_API.Analysis;
      use Liblktlang_Support.Generic_API.Introspection;

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
     (Node        : Bare_Lkt_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "")
   is
      Entity : constant Internal_Entity := (Node, No_Entity_Info);
   begin
      Print (To_Generic_Node (Entity), Show_Slocs, Line_Prefix);
   end Print;

   ------------
   -- Parent --
   ------------

   function Parent (Node : Bare_Lkt_Node) return Bare_Lkt_Node is
   begin
      return Node.Parent;
   end Parent;

   ------------------
   -- Stored_Token --
   ------------------

   function Stored_Token
     (Node  : Bare_Lkt_Node;
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
     (Node : Bare_Lkt_Node) return Bare_Children_Vector
   is
      Ret_Vec : Bare_Children_Vector;
      Ctx     : Internal_Context renames Node.Unit.Context;
      TDH     : Token_Data_Handler renames Node.Unit.TDH;

      procedure Append_Trivias (First, Last : Token_Index);
      --  Append all the trivias of tokens between indices First and Last to
      --  the returned vector.

      function Filter_Children
        (Parent : Bare_Lkt_Node)
         return Internal_Bare_Lkt_Node_Array;
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
        (Parent : Bare_Lkt_Node)
         return Internal_Bare_Lkt_Node_Array
      is
         Children : constant Internal_Bare_Lkt_Node_Array :=
            Implementation.Children (Parent);
         Result   : Internal_Bare_Lkt_Node_Array (Children'Range);
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
      N_Children  : constant Internal_Bare_Lkt_Node_Array :=
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

   function Is_Ghost (Node : Bare_Lkt_Node) return Boolean
   is (Node.Token_End_Index = No_Token_Index);

   -------------------
   -- Is_Incomplete --
   -------------------

   function Is_Incomplete (Node : Bare_Lkt_Node) return Boolean
   is
      LGC : Bare_Lkt_Node;
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

   function Token_Start (Node : Bare_Lkt_Node) return Token_Reference
   is (Token (Node, Node.Token_Start_Index));

   ---------------
   -- Token_End --
   ---------------

   function Token_End (Node : Bare_Lkt_Node) return Token_Reference
   is
     (if Node.Token_End_Index = No_Token_Index
      then Token_Start (Node)
      else Token (Node, Node.Token_End_Index));

   -----------
   -- Token --
   -----------

   function Token
     (Node  : Bare_Lkt_Node;
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

   function "<" (Left, Right : Bare_Lkt_Node) return Boolean is
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

   function Is_Null (Node : Bare_Lkt_Node) return Boolean
   is (Node = null);

   ----------
   -- Kind --
   ----------

   function Kind (Node : Bare_Lkt_Node) return Lkt_Node_Kind_Type
   is (Node.Kind);

   -----------------
   -- Child_Index --
   -----------------

   function Child_Index (Node : Bare_Lkt_Node) return Integer
   is
      N : Bare_Lkt_Node := null;
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
     (Node   : Bare_Lkt_Node;
      Offset : Integer) return Bare_Lkt_Node is
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
     (Node   : Bare_Lkt_Node;
      E_Info : Internal_Entity_Info;
      Offset : Integer) return Internal_Entity
   is
      Sibling : constant Bare_Lkt_Node := Fetch_Sibling (Node, Offset);
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
     (Node   : Bare_Lkt_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity is
   begin
      return Fetch_Sibling (Node, E_Info, -1);
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling
     (Node   : Bare_Lkt_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity is
   begin
      return Fetch_Sibling (Node, E_Info, 1);
   end Next_Sibling;

   --------------------------------
   -- Node_Builder_Build_Wrapper --
   --------------------------------

   function Node_Builder_Build_Wrapper
     (Self              : Node_Builder_Type;
      Parent, Self_Node : Bare_Lkt_Node) return Bare_Lkt_Node
   is
      Synthetizing : constant Boolean := Is_Synthetizing (Self);
      Unit         : constant Internal_Unit := Self_Node.Unit;
      Env          : constant Lexical_Env :=
        (if Parent = null
         then Empty_Env
         else Parent.Self_Env);
   begin
      if Synthetizing and then Parent /= null and then Parent.Unit /= Unit then
         Raise_Property_Exception
           (Self_Node,
            Property_Error'Identity,
            "synthetic node parents must belong to the same unit as the nodes"
            & " that trigger node synthetization");
      end if;

      --  Forbid node synthetization when Self.Self_Env is foreign, as in that
      --  case, this new node would escape the relocation mechanism when that
      --  foreign env is terminated.
      --
      --  Note that we could, in principle, register this synthetized node so
      --  that the relocation mechanism takes care of it, but this incurs extr
      --  a complexity for a use case that is not yet proven useful. So just
      --  forbid this situation.

      if Synthetizing and then AST_Envs.Is_Foreign_Strict (Env, Self_Node) then
         Raise_Property_Exception
           (Self_Node,
            Property_Error'Identity,
            "synthetic nodes cannot have foreign lexical envs");
      end if;

      Self.all.Validate (Self_Node);
      return Self.all.Build (Parent, Self_Node);
   end Node_Builder_Build_Wrapper;

   --------------------------
   -- Validate_Check_Child --
   --------------------------

   procedure Validate_Check_Child
     (Desc      : String;
      Builder   : Node_Builder_Type;
      Nullable  : Boolean;
      Self_Node : Bare_Lkt_Node)
   is
      Child : Bare_Lkt_Node;
   begin
      --  There is no need to do the checks below on synthetizing node
      --  builders, as they are guaranteed by construction to return non-null
      --  values that belong to the same unit as Self_Node.

      if Is_Synthetizing (Builder) then
         Builder.Validate (Self_Node);
         return;
      end if;

      Child := Builder.Build (null, Self_Node);
      if Child = null and then not Nullable then
         Raise_Property_Exception
           (Self_Node,
            Property_Error'Identity,
            Desc & " cannot be null; add a nullable annotation to this field"
            & " to allow it");
      end if;
   end Validate_Check_Child;

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
     (Value : Bare_Lkt_Node) return Node_Builder_Type is
   begin
      --  No need to allocate a new builder if in practice it cannot be
      --  distinguished from the "null" builder.

      if Value = null then
         return Null_Node_Builder;
      else
         return new Copy_Node_Builder_Record'
           (Ref_Count => 1, Hash => Hash (Value), Value => Value);
      end if;
   end Create_Copy_Node_Builder;

         

         type Bare_Synthetic_Type_Ref_List_Node_Builder_Record is new Node_Builder_Record with
            record
               List_Elements : Bare_Type_Ref_Node_Builder_Array_Access;
            end record;
         type Bare_Synthetic_Type_Ref_List_Node_Builder_Access is access all Bare_Synthetic_Type_Ref_List_Node_Builder_Record;

         overriding procedure Validate
           (Self : Bare_Synthetic_Type_Ref_List_Node_Builder_Record; Self_Node : Bare_Lkt_Node);

         overriding function Build
           (Self              : Bare_Synthetic_Type_Ref_List_Node_Builder_Record;
            Parent, Self_Node : Bare_Lkt_Node)
            return Bare_Lkt_Node;

         overriding function Is_Equivalent
           (Left  : Bare_Synthetic_Type_Ref_List_Node_Builder_Record;
            Right : Node_Builder_Record'Class) return Boolean;

         overriding function Trace_Image
           (Self : Bare_Synthetic_Type_Ref_List_Node_Builder_Record) return String
         is ("<NodeBuilder to synthetize SyntheticTypeRefList>");

            overriding procedure Release (Self : in out Bare_Synthetic_Type_Ref_List_Node_Builder_Record);

         --------------
         -- Validate --
         --------------

         overriding procedure Validate
           (Self : Bare_Synthetic_Type_Ref_List_Node_Builder_Record; Self_Node : Bare_Lkt_Node) is
         begin

               declare
                  Elements_Builders : Internal_Bare_Type_Ref_Node_Builder_Array renames Self.List_Elements.Items;
               begin
                  for I in Elements_Builders'Range loop
                     Validate_Check_Child
                       (Desc      =>
                          "list child of "
                          & "SyntheticTypeRefList",
                        Builder   => Elements_Builders (I),
                        Nullable  => False,
                        Self_Node => Self_Node);
                  end loop;
               end;

            null;
         end Validate;

         -----------
         -- Build --
         -----------

         overriding function Build
           (Self              : Bare_Synthetic_Type_Ref_List_Node_Builder_Record;
            Parent, Self_Node : Bare_Lkt_Node)
            return Bare_Lkt_Node
         is
            Result : Bare_Lkt_Node;
            Unit   : constant Internal_Unit := Self_Node.Unit;
            Env    : constant Lexical_Env :=
              (if Parent = null
               then Empty_Env
               else Parent.Self_Env);
         begin
            Result := new Root_Node_Record
              (Lkt_Synthetic_Type_Ref_List);
            Initialize
              (Self => Result,
               Kind => Lkt_Synthetic_Type_Ref_List,
               Unit => Unit,

               Token_Start_Index => No_Token_Index,
               Token_End_Index   => No_Token_Index,

               Parent   => Parent,
               Self_Env => Env);
            Register_Destroyable (Unit, Result);

               declare
                  Elements_Builders : Internal_Bare_Type_Ref_Node_Builder_Array renames Self.List_Elements.Items;
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


            return Result;
         end Build;

         -------------------
         -- Is_Equivalent --
         -------------------

         overriding function Is_Equivalent
           (Left  : Bare_Synthetic_Type_Ref_List_Node_Builder_Record;
            Right : Node_Builder_Record'Class) return Boolean
         is
               L : Bare_Synthetic_Type_Ref_List_Node_Builder_Record renames Left;
               R : Bare_Synthetic_Type_Ref_List_Node_Builder_Record renames Bare_Synthetic_Type_Ref_List_Node_Builder_Record (Right);
         begin
               return Equivalent (L.List_Elements, R.List_Elements);
         end Is_Equivalent;


            -------------
            -- Release --
            -------------

            overriding procedure Release (Self : in out Bare_Synthetic_Type_Ref_List_Node_Builder_Record) is
            begin
                     Dec_Ref (Self.List_Elements);
            end Release;

         function Create_Bare_Synthetic_Type_Ref_List_Node_Builder
             (List_Elements : Bare_Type_Ref_Node_Builder_Array_Access)
           return Bare_Synthetic_Type_Ref_List_Node_Builder
         is
            Builder : constant Bare_Synthetic_Type_Ref_List_Node_Builder_Access := new Bare_Synthetic_Type_Ref_List_Node_Builder_Record;
         begin
            Builder.Ref_Count := 1;
               Builder.List_Elements := List_Elements;
                  Inc_Ref (Builder.List_Elements);

               Builder.Hash := Combine
                 (  (1 => Hash (List_Elements)));
            return Node_Builder_Type (Builder);
         end Create_Bare_Synthetic_Type_Ref_List_Node_Builder;


   ----------------------
   -- Compare_Metadata --
   ----------------------

   --  Deactivate "not referenced" warnings because if the metadata struct has
   --  no fields, formals and temporaries won't be referenced in the two
   --  following functions.
   pragma Warnings (Off, "referenced");
   function Compare_Metadata (L, R : Internal_Metadata) return Boolean is
   begin
      return True;
   end Compare_Metadata;

   ----------
   -- Hash --
   ----------

   function Hash (Self : Internal_Metadata) return Hash_Type is
      Ret : Hash_Type := Liblktlang_Support.Hashes.Initial_Hash;
   begin
      return Ret;
   end Hash;
   pragma Warnings (On, "referenced");

   -------------
   -- Combine --
   -------------

   function Combine
     (L, R : Internal_Metadata) return Internal_Metadata
   is
      pragma Unreferenced (L, R);
      Ret : Internal_Metadata := No_Metadata;
   begin
      return Ret;
   end Combine;

   -------------------------------
   -- Create_Static_Lexical_Env --
   -------------------------------

   function Create_Static_Lexical_Env
     (Parent            : Lexical_Env;
      Node              : Bare_Lkt_Node;
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
     (Self  : Bare_Lkt_Node;
      A     : AST_Envs.Entity_Array;
      Index : Integer) return Internal_Entity
   is
      function Length (A : AST_Envs.Entity_Array) return Natural
      is (A'Length);

      function Get
        (A     : AST_Envs.Entity_Array;
         Index : Integer) return Internal_Entity
      is (A (Index + 1)); --  A is 1-based but Index is 0-based

      function Relative_Get is new Liblktlang_Support.Relative_Get
        (Item_Type     => AST_Envs.Entity,
         Sequence_Type => AST_Envs.Entity_Array,
         Length        => Length,
         Get           => Get);
      Result : Internal_Entity;
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
     (Envs   : Lexical_Env_Array_Access;
      Env_Md : Internal_Metadata := No_Metadata) return Lexical_Env
   is (AST_Envs.Group (AST_Envs.Lexical_Env_Array (Envs.Items), Env_Md));

   ------------------
   -- Children_Env --
   ------------------

   function Children_Env
     (Node   : Bare_Lkt_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Lexical_Env
   is (AST_Envs.Rebind_Env (Node.Self_Env, E_Info));

   --------------
   -- Node_Env --
   --------------

   function Node_Env
     (Node   : Bare_Lkt_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
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
         
         return
           (if Node.Kind in Lkt_Match_Branch | Lkt_Pattern_Match_Branch | Lkt_Field_Decl | Lkt_Fun_Decl | Lkt_Env_Spec_Decl | Lkt_Generic_Decl | Lkt_Grammar_Decl | Lkt_Langkit_Root | Lkt_Lexer_Decl | Lkt_Block_Expr | Lkt_Lambda_Expr | Lkt_Decl_Block
            then Get_Parent_Env
            else Node.Self_Env);
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
     (Node   : Bare_Lkt_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity is
   begin
      --  TODO: shed entity information as appropriate
      return (Node.Parent, E_Info);
   end Parent;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node      : Bare_Lkt_Node;
      With_Self : Boolean := True;
      E_Info    : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity_Array_Access
   is
      Bare_Parents : Bare_Lkt_Node_Array_Access := Parents (Node, With_Self);
      Result       : Internal_Entity_Array_Access :=
         Create_Internal_Entity_Array (Bare_Parents.N);
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
     (Node   : Bare_Lkt_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info)
      return Internal_Entity_Array_Access
   is
      Bare_Children : Bare_Lkt_Node_Array_Access := Children (Node);
      Result        : Internal_Entity_Array_Access :=
         Create_Internal_Entity_Array (Bare_Children.N);
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

   procedure Assign_Names_To_Logic_Vars (Node : Bare_Lkt_Node) is

      pragma Warnings (Off, "referenced");

      procedure Assign
        (Node  : Bare_Lkt_Node;
         LV    : in out Logic_Var_Record;
         Field : String);
      --  Assign a name to the LV logic variable. Node must be the node that
      --  owns LV, and Field must be the name of the field in Node that holds
      --  LV.

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Node  : Bare_Lkt_Node;
         LV    : in out Logic_Var_Record;
         Field : String) is
      begin
         LV.Dbg_Name :=
           new String'(Image (Short_Text_Image (Node)) & "." & Field);
      end Assign;

      K : constant Lkt_Node_Kind_Type := Node.Kind;

      pragma Warnings (On, "referenced");

   begin
      
      case Lkt_Lkt_Node (K) is
when Lkt_Lambda_Param_Decl_Range =>
declare
N_Bare_Lambda_Param_Decl : constant Bare_Lambda_Param_Decl := Node;
begin
Assign (N_Bare_Lambda_Param_Decl,        N_Bare_Lambda_Param_Decl.Lambda_Param_Decl_F_Type_Var,        "type_var");
end;
when Lkt_Expr =>
declare
N_Bare_Expr : constant Bare_Expr := Node;
begin
Assign (N_Bare_Expr,        N_Bare_Expr.Expr_F_Expected_Type_Var,        "expected_type_var");
Assign (N_Bare_Expr,        N_Bare_Expr.Expr_F_Actual_Type_Var,        "actual_type_var");
Assign (N_Bare_Expr,        N_Bare_Expr.Expr_F_Generic_Func_Type_Var,        "generic_func_type_var");
case Lkt_Expr (K) is
when Lkt_Array_Literal_Range =>
declare
N_Bare_Array_Literal : constant Bare_Array_Literal := N_Bare_Expr;
begin
Assign (N_Bare_Array_Literal,        N_Bare_Array_Literal.Array_Literal_F_Expected_Exprs_Type_Var,        "expected_exprs_type_var");
Assign (N_Bare_Array_Literal,        N_Bare_Array_Literal.Array_Literal_F_Actual_Element_Type,        "actual_element_type");
end;
when Lkt_Generic_Instantiation_Range =>
declare
N_Bare_Generic_Instantiation : constant Bare_Generic_Instantiation := N_Bare_Expr;
begin
Assign (N_Bare_Generic_Instantiation,        N_Bare_Generic_Instantiation.Generic_Instantiation_F_Rebinded_Var,        "rebinded_var");
end;
when Lkt_Ref_Id_Range =>
declare
N_Bare_Ref_Id : constant Bare_Ref_Id := N_Bare_Expr;
begin
Assign (N_Bare_Ref_Id,        N_Bare_Ref_Id.Ref_Id_F_Ref_Var,        "ref_var");
end;
when Lkt_If_Expr_Range =>
declare
N_Bare_If_Expr : constant Bare_If_Expr := N_Bare_Expr;
begin
Assign (N_Bare_If_Expr,        N_Bare_If_Expr.If_Expr_F_Expected_Branch_Type_Var,        "expected_branch_type_var");
end;
when Lkt_Keep_Expr_Range =>
declare
N_Bare_Keep_Expr : constant Bare_Keep_Expr := N_Bare_Expr;
begin
Assign (N_Bare_Keep_Expr,        N_Bare_Keep_Expr.Keep_Expr_F_Array_Element_Type,        "array_element_type");
end;
when Lkt_Match_Expr_Range =>
declare
N_Bare_Match_Expr : constant Bare_Match_Expr := N_Bare_Expr;
begin
Assign (N_Bare_Match_Expr,        N_Bare_Match_Expr.Match_Expr_F_Expected_Branch_Type_Var,        "expected_branch_type_var");
end;
when Lkt_Try_Expr_Range =>
declare
N_Bare_Try_Expr : constant Bare_Try_Expr := N_Bare_Expr;
begin
Assign (N_Bare_Try_Expr,        N_Bare_Try_Expr.Try_Expr_F_Expected_Expr_Type_Var,        "expected_expr_type_var");
end;
when others => null;
end case;
end;
when Lkt_Type_Ref =>
declare
N_Bare_Type_Ref : constant Bare_Type_Ref := Node;
begin
Assign (N_Bare_Type_Ref,        N_Bare_Type_Ref.Type_Ref_F_Type_Var,        "type_var");
end;
when others => null;
end case;
      for Child of Internal_Bare_Lkt_Node_Array'(Children (Node)) loop
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
         
            Exception_Identity (Exc) = Liblktlang_Support.Errors.Property_Error'Identity
         then
            return Raised_Property_Error;
      else
         raise Program_Error;
      end if;
   end Initialization_Error;

   ----------------------------------
   -- Reraise_Initialization_Error --
   ----------------------------------

   procedure Reraise_Initialization_Error
     (Node    : Bare_Lkt_Node;
      State   : Error_Initialization_State;
      Message : String)
   is
      Exc : Ada.Exceptions.Exception_Id;
   begin
      case State is
            when Raised_Property_Error =>
               Exc := Liblktlang_Support.Errors.Property_Error'Identity;
      end case;
      Raise_Property_Exception (Node, Exc, Message);
   end Reraise_Initialization_Error;

   ----------------
   -- Text_Image --
   ----------------

   function Text_Image (Ent : Internal_Entity) return Text_Type is
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

   function Full_Sloc_Image (Node : Bare_Lkt_Node) return String_Type
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
     (Node : Bare_Lkt_Node;
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

   function Image (Ent : Internal_Entity) return String is
      Result : constant Text_Type := Text_Image (Ent);
   begin
      return Image (Result);
   end Image;

   ---------------
   -- Can_Reach --
   ---------------

   function Can_Reach (El, From : Bare_Lkt_Node) return Boolean is
      function Property (El, From : Bare_Lkt_Node) return Boolean
      with
        Import,
        External_Name => "Liblktlang__can_reach";
   begin
      return Property (El, From);
   end Can_Reach;

   -----------------
   -- Hash_Entity --
   -----------------

   function Hash_Entity (Self : Internal_Entity) return Hash_Type is
   begin
      return Combine
        ((Hash (Self.Node), Hash (Self.Info.Rebindings), Hash (Self.Info.Md)));
   end Hash_Entity;

   --------------------
   -- Compare_Entity --
   --------------------

   function Compare_Entity (Left, Right : Internal_Entity) return Boolean
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
     (Self              : Bare_Lkt_Node;
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

   procedure Destroy_Synthetic_Node (Node : in out Bare_Lkt_Node);
   --  Helper for the Register_Destroyable above

   ------------
   -- Length --
   ------------

   function Length (Node : Bare_Lkt_Node_Base_List) return Natural
   is (if Node = null then 0 else Children_Count (Node));


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


   

   




   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Metadata) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                  & "null record"
               & ")");
      end Trace_Image;


   

   




   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Entity_Info) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Combine
              (Hash (R.Md), Hash (R.Rebindings));
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Info) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Md => "
                     & Trace_Image (R.Md)
                        & ", "
                     & "Rebindings => "
                     & Trace_Image (R.Rebindings)
                        & ", "
                     & "From_Rebound => "
                     & Trace_Image (R.From_Rebound)
               & ")");
      end Trace_Image;


   

   



      function Create_Internal_Entity
        (Node : Bare_Lkt_Node; Info : Internal_Entity_Info)
         return Internal_Entity is
      begin
         if Node = null then
            return No_Entity;
         end if;
         return (Node => Node, Info => Info);
      end;



   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Entity) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Combine
              (Hash (R.Node), Hash (R.Info));
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Decl
        (Node : Bare_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Decl is
      begin
         if Node = null then
            return No_Entity_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Entity_Decl) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Combine
              (Hash (R.Node), Hash (R.Info));
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Complete_Item) return Boolean is
      begin
         return Equivalent (L.Declaration, R.Declaration);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Complete_Item) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Declaration => "
                     & Trace_Image (R.Declaration)
               & ")");
      end Trace_Image;


   

   


      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : Internal_Decoded_Char_Value) is
      begin
               Inc_Ref (R.Error_Message);
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out Internal_Decoded_Char_Value) is
      begin
               Dec_Ref (R.Error_Message);
      end Dec_Ref;




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Decoded_Char_Value) return Boolean is
      begin
         return L.Value = R.Value and then L.Has_Error = R.Has_Error and then L.Error_Sloc = R.Error_Sloc and then Equivalent (L.Error_Message, R.Error_Message);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Decoded_Char_Value) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Value => "
                     & Trace_Image (R.Value)
                        & ", "
                     & "Has_Error => "
                     & Trace_Image (R.Has_Error)
                        & ", "
                     & "Error_Sloc => "
                     & Trace_Image (R.Error_Sloc)
                        & ", "
                     & "Error_Message => "
                     & Trace_Image (R.Error_Message)
               & ")");
      end Trace_Image;


   

   


      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : Internal_Decoded_String_Value) is
      begin
               Inc_Ref (R.Value);
               Inc_Ref (R.Error_Message);
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out Internal_Decoded_String_Value) is
      begin
               Dec_Ref (R.Value);
               Dec_Ref (R.Error_Message);
      end Dec_Ref;




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Decoded_String_Value) return Boolean is
      begin
         return Equivalent (L.Value, R.Value) and then L.Has_Error = R.Has_Error and then L.Error_Sloc = R.Error_Sloc and then Equivalent (L.Error_Message, R.Error_Message);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Decoded_String_Value) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Value => "
                     & Trace_Image (R.Value)
                        & ", "
                     & "Has_Error => "
                     & Trace_Image (R.Has_Error)
                        & ", "
                     & "Error_Sloc => "
                     & Trace_Image (R.Error_Sloc)
                        & ", "
                     & "Error_Message => "
                     & Trace_Image (R.Error_Message)
               & ")");
      end Trace_Image;


   

   


      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : Internal_Designated_Env) is
      begin
               Inc_Ref (R.Direct_Env);
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out Internal_Designated_Env) is
      begin
               Dec_Ref (R.Direct_Env);
      end Dec_Ref;




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Designated_Env) return Boolean is
      begin
         return L.Kind = R.Kind and then L.Env_Name = R.Env_Name and then Equivalent (L.Direct_Env, R.Direct_Env);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Designated_Env) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Kind => "
                     & Trace_Image (R.Kind)
                        & ", "
                     & "Env_Name => "
                     & Trace_Image (R.Env_Name)
                        & ", "
                     & "Direct_Env => "
                     & Trace_Image (R.Direct_Env)
               & ")");
      end Trace_Image;


   

   



      function Create_Internal_Entity_Expr
        (Node : Bare_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Expr is
      begin
         if Node = null then
            return No_Entity_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Any_Of
        (Node : Bare_Any_Of; Info : Internal_Entity_Info)
         return Internal_Entity_Any_Of is
      begin
         if Node = null then
            return No_Entity_Any_Of;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Any_Of) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Any_Of) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lkt_Node_Base_List
        (Node : Bare_Lkt_Node_Base_List; Info : Internal_Entity_Info)
         return Internal_Entity_Lkt_Node_Base_List is
      begin
         if Node = null then
            return No_Entity_Lkt_Node_Base_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lkt_Node_Base_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lkt_Node_Base_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Expr_List
        (Node : Bare_Expr_List; Info : Internal_Entity_Info)
         return Internal_Entity_Expr_List is
      begin
         if Node = null then
            return No_Entity_Expr_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Expr_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Expr_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Any_Of_List
        (Node : Bare_Any_Of_List; Info : Internal_Entity_Info)
         return Internal_Entity_Any_Of_List is
      begin
         if Node = null then
            return No_Entity_Any_Of_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Any_Of_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Any_Of_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Type_Decl
        (Node : Bare_Type_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Type_Decl is
      begin
         if Node = null then
            return No_Entity_Type_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Type_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Entity_Type_Decl) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Combine
              (Hash (R.Node), Hash (R.Info));
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Type_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Any_Type_Decl
        (Node : Bare_Any_Type_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Any_Type_Decl is
      begin
         if Node = null then
            return No_Entity_Any_Type_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Any_Type_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Any_Type_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Pattern
        (Node : Bare_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Pattern is
      begin
         if Node = null then
            return No_Entity_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Any_Type_Pattern
        (Node : Bare_Any_Type_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Any_Type_Pattern is
      begin
         if Node = null then
            return No_Entity_Any_Type_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Any_Type_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Any_Type_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Argument
        (Node : Bare_Argument; Info : Internal_Entity_Info)
         return Internal_Entity_Argument is
      begin
         if Node = null then
            return No_Entity_Argument;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Argument) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Argument) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Argument_List
        (Node : Bare_Argument_List; Info : Internal_Entity_Info)
         return Internal_Entity_Argument_List is
      begin
         if Node = null then
            return No_Entity_Argument_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Argument_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Entity_Argument_List) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Combine
              (Hash (R.Node), Hash (R.Info));
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Argument_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Array_Literal
        (Node : Bare_Array_Literal; Info : Internal_Entity_Info)
         return Internal_Entity_Array_Literal is
      begin
         if Node = null then
            return No_Entity_Array_Literal;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Array_Literal) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Array_Literal) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Base_Call_Expr
        (Node : Bare_Base_Call_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Call_Expr is
      begin
         if Node = null then
            return No_Entity_Base_Call_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Base_Call_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Base_Call_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Base_Grammar_Rule_Decl
        (Node : Bare_Base_Grammar_Rule_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Grammar_Rule_Decl is
      begin
         if Node = null then
            return No_Entity_Base_Grammar_Rule_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Base_Grammar_Rule_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Base_Grammar_Rule_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Base_Import
        (Node : Bare_Base_Import; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Import is
      begin
         if Node = null then
            return No_Entity_Base_Import;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Base_Import) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Base_Import) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Base_Import_List
        (Node : Bare_Base_Import_List; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Import_List is
      begin
         if Node = null then
            return No_Entity_Base_Import_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Base_Import_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Base_Import_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Base_Lexer_Case_Rule_Alt
        (Node : Bare_Base_Lexer_Case_Rule_Alt; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Lexer_Case_Rule_Alt is
      begin
         if Node = null then
            return No_Entity_Base_Lexer_Case_Rule_Alt;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Base_Lexer_Case_Rule_Alt) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Base_Lexer_Case_Rule_Alt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Base_Lexer_Case_Rule_Alt_List
        (Node : Bare_Base_Lexer_Case_Rule_Alt_List; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Lexer_Case_Rule_Alt_List is
      begin
         if Node = null then
            return No_Entity_Base_Lexer_Case_Rule_Alt_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Base_Lexer_Case_Rule_Alt_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Base_Lexer_Case_Rule_Alt_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Base_Match_Branch
        (Node : Bare_Base_Match_Branch; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Match_Branch is
      begin
         if Node = null then
            return No_Entity_Base_Match_Branch;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Base_Match_Branch) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Base_Match_Branch) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Base_Match_Branch_List
        (Node : Bare_Base_Match_Branch_List; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Match_Branch_List is
      begin
         if Node = null then
            return No_Entity_Base_Match_Branch_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Base_Match_Branch_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Base_Match_Branch_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Base_Val_Decl
        (Node : Bare_Base_Val_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Val_Decl is
      begin
         if Node = null then
            return No_Entity_Base_Val_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Base_Val_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Base_Val_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Named_Type_Decl
        (Node : Bare_Named_Type_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Named_Type_Decl is
      begin
         if Node = null then
            return No_Entity_Named_Type_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Named_Type_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Named_Type_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Basic_Class_Decl
        (Node : Bare_Basic_Class_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Basic_Class_Decl is
      begin
         if Node = null then
            return No_Entity_Basic_Class_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Basic_Class_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Basic_Class_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lit
        (Node : Bare_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Lit is
      begin
         if Node = null then
            return No_Entity_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Big_Num_Lit
        (Node : Bare_Big_Num_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Big_Num_Lit is
      begin
         if Node = null then
            return No_Entity_Big_Num_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Big_Num_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Big_Num_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Bin_Op
        (Node : Bare_Bin_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Bin_Op is
      begin
         if Node = null then
            return No_Entity_Bin_Op;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Bin_Op) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Bin_Op) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_User_Val_Decl
        (Node : Bare_User_Val_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_User_Val_Decl is
      begin
         if Node = null then
            return No_Entity_User_Val_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_User_Val_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_User_Val_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Binding_Val_Decl
        (Node : Bare_Binding_Val_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Binding_Val_Decl is
      begin
         if Node = null then
            return No_Entity_Binding_Val_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Binding_Val_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Binding_Val_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Block_Expr
        (Node : Bare_Block_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Block_Expr is
      begin
         if Node = null then
            return No_Entity_Block_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Block_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Block_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Block_Expr_Clause
        (Node : Bare_Block_Expr_Clause; Info : Internal_Entity_Info)
         return Internal_Entity_Block_Expr_Clause is
      begin
         if Node = null then
            return No_Entity_Block_Expr_Clause;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Block_Expr_Clause) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Block_Expr_Clause) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Block_String_Line
        (Node : Bare_Block_String_Line; Info : Internal_Entity_Info)
         return Internal_Entity_Block_String_Line is
      begin
         if Node = null then
            return No_Entity_Block_String_Line;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Block_String_Line) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Block_String_Line) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Block_String_Line_List
        (Node : Bare_Block_String_Line_List; Info : Internal_Entity_Info)
         return Internal_Entity_Block_String_Line_List is
      begin
         if Node = null then
            return No_Entity_Block_String_Line_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Block_String_Line_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Block_String_Line_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_String_Lit
        (Node : Bare_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_String_Lit is
      begin
         if Node = null then
            return No_Entity_String_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_String_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_String_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Block_String_Lit
        (Node : Bare_Block_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Block_String_Lit is
      begin
         if Node = null then
            return No_Entity_Block_String_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Block_String_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Block_String_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Bool_Pattern
        (Node : Bare_Bool_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Bool_Pattern is
      begin
         if Node = null then
            return No_Entity_Bool_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Bool_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Bool_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Bool_Pattern_False
        (Node : Bare_Bool_Pattern_False; Info : Internal_Entity_Info)
         return Internal_Entity_Bool_Pattern_False is
      begin
         if Node = null then
            return No_Entity_Bool_Pattern_False;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Bool_Pattern_False) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Bool_Pattern_False) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Bool_Pattern_True
        (Node : Bare_Bool_Pattern_True; Info : Internal_Entity_Info)
         return Internal_Entity_Bool_Pattern_True is
      begin
         if Node = null then
            return No_Entity_Bool_Pattern_True;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Bool_Pattern_True) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Bool_Pattern_True) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Call_Expr
        (Node : Bare_Call_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Call_Expr is
      begin
         if Node = null then
            return No_Entity_Call_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Call_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Call_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Call_Expr_List
        (Node : Bare_Call_Expr_List; Info : Internal_Entity_Info)
         return Internal_Entity_Call_Expr_List is
      begin
         if Node = null then
            return No_Entity_Call_Expr_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Call_Expr_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Call_Expr_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Cast_Expr
        (Node : Bare_Cast_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Cast_Expr is
      begin
         if Node = null then
            return No_Entity_Cast_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Cast_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Cast_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Char_Lit
        (Node : Bare_Char_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Char_Lit is
      begin
         if Node = null then
            return No_Entity_Char_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Char_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Char_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Class_Decl
        (Node : Bare_Class_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Class_Decl is
      begin
         if Node = null then
            return No_Entity_Class_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Class_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Class_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Class_Qualifier
        (Node : Bare_Class_Qualifier; Info : Internal_Entity_Info)
         return Internal_Entity_Class_Qualifier is
      begin
         if Node = null then
            return No_Entity_Class_Qualifier;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Class_Qualifier) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Class_Qualifier) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Class_Qualifier_Absent
        (Node : Bare_Class_Qualifier_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Class_Qualifier_Absent is
      begin
         if Node = null then
            return No_Entity_Class_Qualifier_Absent;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Class_Qualifier_Absent) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Class_Qualifier_Absent) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Class_Qualifier_Present
        (Node : Bare_Class_Qualifier_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Class_Qualifier_Present is
      begin
         if Node = null then
            return No_Entity_Class_Qualifier_Present;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Class_Qualifier_Present) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Class_Qualifier_Present) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Complex_Pattern
        (Node : Bare_Complex_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Complex_Pattern is
      begin
         if Node = null then
            return No_Entity_Complex_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Complex_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Complex_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Explicitly_Typed_Decl
        (Node : Bare_Explicitly_Typed_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Explicitly_Typed_Decl is
      begin
         if Node = null then
            return No_Entity_Explicitly_Typed_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Explicitly_Typed_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Explicitly_Typed_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Component_Decl
        (Node : Bare_Component_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Component_Decl is
      begin
         if Node = null then
            return No_Entity_Component_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Component_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Component_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Decl_Annotation
        (Node : Bare_Decl_Annotation; Info : Internal_Entity_Info)
         return Internal_Entity_Decl_Annotation is
      begin
         if Node = null then
            return No_Entity_Decl_Annotation;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Decl_Annotation) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Decl_Annotation) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Decl_Annotation_Args
        (Node : Bare_Decl_Annotation_Args; Info : Internal_Entity_Info)
         return Internal_Entity_Decl_Annotation_Args is
      begin
         if Node = null then
            return No_Entity_Decl_Annotation_Args;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Decl_Annotation_Args) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Decl_Annotation_Args) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Decl_Annotation_List
        (Node : Bare_Decl_Annotation_List; Info : Internal_Entity_Info)
         return Internal_Entity_Decl_Annotation_List is
      begin
         if Node = null then
            return No_Entity_Decl_Annotation_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Decl_Annotation_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Decl_Annotation_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Full_Decl_List
        (Node : Bare_Full_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Full_Decl_List is
      begin
         if Node = null then
            return No_Entity_Full_Decl_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Full_Decl_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Full_Decl_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Decl_Block
        (Node : Bare_Decl_Block; Info : Internal_Entity_Info)
         return Internal_Entity_Decl_Block is
      begin
         if Node = null then
            return No_Entity_Decl_Block;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Decl_Block) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Decl_Block) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Id
        (Node : Bare_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Id is
      begin
         if Node = null then
            return No_Entity_Id;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Id) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Id) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Def_Id
        (Node : Bare_Def_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Def_Id is
      begin
         if Node = null then
            return No_Entity_Def_Id;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Def_Id) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Def_Id) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Type_Ref
        (Node : Bare_Type_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Type_Ref is
      begin
         if Node = null then
            return No_Entity_Type_Ref;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Type_Ref) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Type_Ref) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Default_List_Type_Ref
        (Node : Bare_Default_List_Type_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Default_List_Type_Ref is
      begin
         if Node = null then
            return No_Entity_Default_List_Type_Ref;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Default_List_Type_Ref) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Default_List_Type_Ref) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Pattern_Detail
        (Node : Bare_Pattern_Detail; Info : Internal_Entity_Info)
         return Internal_Entity_Pattern_Detail is
      begin
         if Node = null then
            return No_Entity_Pattern_Detail;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Pattern_Detail) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Pattern_Detail) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Destructuring_Pattern_Detail
        (Node : Bare_Destructuring_Pattern_Detail; Info : Internal_Entity_Info)
         return Internal_Entity_Destructuring_Pattern_Detail is
      begin
         if Node = null then
            return No_Entity_Destructuring_Pattern_Detail;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Destructuring_Pattern_Detail) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Destructuring_Pattern_Detail) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Dot_Expr
        (Node : Bare_Dot_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Dot_Expr is
      begin
         if Node = null then
            return No_Entity_Dot_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Dot_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Dot_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Dyn_Env_Wrapper
        (Node : Bare_Dyn_Env_Wrapper; Info : Internal_Entity_Info)
         return Internal_Entity_Dyn_Env_Wrapper is
      begin
         if Node = null then
            return No_Entity_Dyn_Env_Wrapper;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Dyn_Env_Wrapper) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Dyn_Env_Wrapper) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Dyn_Var_Decl
        (Node : Bare_Dyn_Var_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Dyn_Var_Decl is
      begin
         if Node = null then
            return No_Entity_Dyn_Var_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Dyn_Var_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Dyn_Var_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ellipsis_Pattern
        (Node : Bare_Ellipsis_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Ellipsis_Pattern is
      begin
         if Node = null then
            return No_Entity_Ellipsis_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Ellipsis_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ellipsis_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Elsif_Branch
        (Node : Bare_Elsif_Branch; Info : Internal_Entity_Info)
         return Internal_Entity_Elsif_Branch is
      begin
         if Node = null then
            return No_Entity_Elsif_Branch;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Elsif_Branch) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Elsif_Branch) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Elsif_Branch_List
        (Node : Bare_Elsif_Branch_List; Info : Internal_Entity_Info)
         return Internal_Entity_Elsif_Branch_List is
      begin
         if Node = null then
            return No_Entity_Elsif_Branch_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Elsif_Branch_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Elsif_Branch_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Enum_Class_Alt_Decl
        (Node : Bare_Enum_Class_Alt_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Class_Alt_Decl is
      begin
         if Node = null then
            return No_Entity_Enum_Class_Alt_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Enum_Class_Alt_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Enum_Class_Alt_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Enum_Class_Alt_Decl_List
        (Node : Bare_Enum_Class_Alt_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Class_Alt_Decl_List is
      begin
         if Node = null then
            return No_Entity_Enum_Class_Alt_Decl_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Enum_Class_Alt_Decl_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Enum_Class_Alt_Decl_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Enum_Class_Case
        (Node : Bare_Enum_Class_Case; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Class_Case is
      begin
         if Node = null then
            return No_Entity_Enum_Class_Case;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Enum_Class_Case) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Enum_Class_Case) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Enum_Class_Case_List
        (Node : Bare_Enum_Class_Case_List; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Class_Case_List is
      begin
         if Node = null then
            return No_Entity_Enum_Class_Case_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Enum_Class_Case_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Enum_Class_Case_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Enum_Class_Decl
        (Node : Bare_Enum_Class_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Class_Decl is
      begin
         if Node = null then
            return No_Entity_Enum_Class_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Enum_Class_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Enum_Class_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Enum_Lit_Decl
        (Node : Bare_Enum_Lit_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Lit_Decl is
      begin
         if Node = null then
            return No_Entity_Enum_Lit_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Enum_Lit_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Enum_Lit_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Enum_Lit_Decl_List
        (Node : Bare_Enum_Lit_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Lit_Decl_List is
      begin
         if Node = null then
            return No_Entity_Enum_Lit_Decl_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Enum_Lit_Decl_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Enum_Lit_Decl_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Enum_Type_Decl
        (Node : Bare_Enum_Type_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Type_Decl is
      begin
         if Node = null then
            return No_Entity_Enum_Type_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Enum_Type_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Enum_Type_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Env_Spec_Decl
        (Node : Bare_Env_Spec_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Env_Spec_Decl is
      begin
         if Node = null then
            return No_Entity_Env_Spec_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Env_Spec_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Env_Spec_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Error_Decl
        (Node : Bare_Error_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Error_Decl is
      begin
         if Node = null then
            return No_Entity_Error_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Error_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Error_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Expr
        (Node : Bare_Grammar_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Expr is
      begin
         if Node = null then
            return No_Entity_Grammar_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Error_Grammar_Expr
        (Node : Bare_Error_Grammar_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Error_Grammar_Expr is
      begin
         if Node = null then
            return No_Entity_Error_Grammar_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Error_Grammar_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Error_Grammar_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Error_Lexer_Case_Rule_Alt
        (Node : Bare_Error_Lexer_Case_Rule_Alt; Info : Internal_Entity_Info)
         return Internal_Entity_Error_Lexer_Case_Rule_Alt is
      begin
         if Node = null then
            return No_Entity_Error_Lexer_Case_Rule_Alt;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Error_Lexer_Case_Rule_Alt) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Error_Lexer_Case_Rule_Alt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Error_On_Null
        (Node : Bare_Error_On_Null; Info : Internal_Entity_Info)
         return Internal_Entity_Error_On_Null is
      begin
         if Node = null then
            return No_Entity_Error_On_Null;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Error_On_Null) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Error_On_Null) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Excludes_Null
        (Node : Bare_Excludes_Null; Info : Internal_Entity_Info)
         return Internal_Entity_Excludes_Null is
      begin
         if Node = null then
            return No_Entity_Excludes_Null;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Excludes_Null) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Excludes_Null) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Excludes_Null_Absent
        (Node : Bare_Excludes_Null_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Excludes_Null_Absent is
      begin
         if Node = null then
            return No_Entity_Excludes_Null_Absent;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Excludes_Null_Absent) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Excludes_Null_Absent) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Excludes_Null_Present
        (Node : Bare_Excludes_Null_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Excludes_Null_Present is
      begin
         if Node = null then
            return No_Entity_Excludes_Null_Present;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Excludes_Null_Present) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Excludes_Null_Present) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Field_Decl
        (Node : Bare_Field_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Field_Decl is
      begin
         if Node = null then
            return No_Entity_Field_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Field_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Field_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Field_Pattern_Detail
        (Node : Bare_Field_Pattern_Detail; Info : Internal_Entity_Info)
         return Internal_Entity_Field_Pattern_Detail is
      begin
         if Node = null then
            return No_Entity_Field_Pattern_Detail;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Field_Pattern_Detail) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Field_Pattern_Detail) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Full_Decl
        (Node : Bare_Full_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Full_Decl is
      begin
         if Node = null then
            return No_Entity_Full_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Full_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Full_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Fun_Decl
        (Node : Bare_Fun_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Fun_Decl is
      begin
         if Node = null then
            return No_Entity_Fun_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Fun_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Fun_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Fun_Param_Decl
        (Node : Bare_Fun_Param_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Fun_Param_Decl is
      begin
         if Node = null then
            return No_Entity_Fun_Param_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Fun_Param_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Fun_Param_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Fun_Param_Decl_List
        (Node : Bare_Fun_Param_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Fun_Param_Decl_List is
      begin
         if Node = null then
            return No_Entity_Fun_Param_Decl_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Fun_Param_Decl_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Fun_Param_Decl_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Function_Type
        (Node : Bare_Function_Type; Info : Internal_Entity_Info)
         return Internal_Entity_Function_Type is
      begin
         if Node = null then
            return No_Entity_Function_Type;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Function_Type) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Function_Type) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Function_Type_Ref
        (Node : Bare_Function_Type_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Function_Type_Ref is
      begin
         if Node = null then
            return No_Entity_Function_Type_Ref;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Function_Type_Ref) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Function_Type_Ref) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Generic_Decl
        (Node : Bare_Generic_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Generic_Decl is
      begin
         if Node = null then
            return No_Entity_Generic_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Generic_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Generic_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Generic_Instantiation
        (Node : Bare_Generic_Instantiation; Info : Internal_Entity_Info)
         return Internal_Entity_Generic_Instantiation is
      begin
         if Node = null then
            return No_Entity_Generic_Instantiation;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Generic_Instantiation) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Generic_Instantiation) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Generic_Param_Decl_List
        (Node : Bare_Generic_Param_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Generic_Param_Decl_List is
      begin
         if Node = null then
            return No_Entity_Generic_Param_Decl_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Generic_Param_Decl_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Generic_Param_Decl_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Generic_Param_Type_Decl
        (Node : Bare_Generic_Param_Type_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Generic_Param_Type_Decl is
      begin
         if Node = null then
            return No_Entity_Generic_Param_Type_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Generic_Param_Type_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Generic_Param_Type_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Generic_Type_Ref
        (Node : Bare_Generic_Type_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Generic_Type_Ref is
      begin
         if Node = null then
            return No_Entity_Generic_Type_Ref;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Generic_Type_Ref) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Generic_Type_Ref) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Cut
        (Node : Bare_Grammar_Cut; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Cut is
      begin
         if Node = null then
            return No_Entity_Grammar_Cut;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Cut) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Cut) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Decl
        (Node : Bare_Grammar_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Decl is
      begin
         if Node = null then
            return No_Entity_Grammar_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Discard
        (Node : Bare_Grammar_Discard; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Discard is
      begin
         if Node = null then
            return No_Entity_Grammar_Discard;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Discard) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Discard) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Dont_Skip
        (Node : Bare_Grammar_Dont_Skip; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Dont_Skip is
      begin
         if Node = null then
            return No_Entity_Grammar_Dont_Skip;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Dont_Skip) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Dont_Skip) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Expr_List
        (Node : Bare_Grammar_Expr_List; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Expr_List is
      begin
         if Node = null then
            return No_Entity_Grammar_Expr_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Expr_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Expr_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Expr_List_List
        (Node : Bare_Grammar_Expr_List_List; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Expr_List_List is
      begin
         if Node = null then
            return No_Entity_Grammar_Expr_List_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Expr_List_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Expr_List_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Pick
        (Node : Bare_Grammar_Pick; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Pick is
      begin
         if Node = null then
            return No_Entity_Grammar_Pick;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Pick) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Pick) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Implicit_Pick
        (Node : Bare_Grammar_Implicit_Pick; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Implicit_Pick is
      begin
         if Node = null then
            return No_Entity_Grammar_Implicit_Pick;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Implicit_Pick) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Implicit_Pick) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_List
        (Node : Bare_Grammar_List; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_List is
      begin
         if Node = null then
            return No_Entity_Grammar_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_List_Sep
        (Node : Bare_Grammar_List_Sep; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_List_Sep is
      begin
         if Node = null then
            return No_Entity_Grammar_List_Sep;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_List_Sep) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_List_Sep) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Null
        (Node : Bare_Grammar_Null; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Null is
      begin
         if Node = null then
            return No_Entity_Grammar_Null;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Null) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Null) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Opt
        (Node : Bare_Grammar_Opt; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Opt is
      begin
         if Node = null then
            return No_Entity_Grammar_Opt;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Opt) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Opt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Opt_Error
        (Node : Bare_Grammar_Opt_Error; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Opt_Error is
      begin
         if Node = null then
            return No_Entity_Grammar_Opt_Error;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Opt_Error) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Opt_Error) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Opt_Error_Group
        (Node : Bare_Grammar_Opt_Error_Group; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Opt_Error_Group is
      begin
         if Node = null then
            return No_Entity_Grammar_Opt_Error_Group;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Opt_Error_Group) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Opt_Error_Group) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Opt_Group
        (Node : Bare_Grammar_Opt_Group; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Opt_Group is
      begin
         if Node = null then
            return No_Entity_Grammar_Opt_Group;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Opt_Group) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Opt_Group) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Or_Expr
        (Node : Bare_Grammar_Or_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Or_Expr is
      begin
         if Node = null then
            return No_Entity_Grammar_Or_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Or_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Or_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Predicate
        (Node : Bare_Grammar_Predicate; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Predicate is
      begin
         if Node = null then
            return No_Entity_Grammar_Predicate;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Predicate) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Predicate) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Rule_Decl
        (Node : Bare_Grammar_Rule_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Rule_Decl is
      begin
         if Node = null then
            return No_Entity_Grammar_Rule_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Rule_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Rule_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Rule_Ref
        (Node : Bare_Grammar_Rule_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Rule_Ref is
      begin
         if Node = null then
            return No_Entity_Grammar_Rule_Ref;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Rule_Ref) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Rule_Ref) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Skip
        (Node : Bare_Grammar_Skip; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Skip is
      begin
         if Node = null then
            return No_Entity_Grammar_Skip;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Skip) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Skip) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Grammar_Stop_Cut
        (Node : Bare_Grammar_Stop_Cut; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Stop_Cut is
      begin
         if Node = null then
            return No_Entity_Grammar_Stop_Cut;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Grammar_Stop_Cut) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Grammar_Stop_Cut) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_If_Expr
        (Node : Bare_If_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_If_Expr is
      begin
         if Node = null then
            return No_Entity_If_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_If_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_If_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Import
        (Node : Bare_Import; Info : Internal_Entity_Info)
         return Internal_Entity_Import is
      begin
         if Node = null then
            return No_Entity_Import;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Import) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Import) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Import_All_From
        (Node : Bare_Import_All_From; Info : Internal_Entity_Info)
         return Internal_Entity_Import_All_From is
      begin
         if Node = null then
            return No_Entity_Import_All_From;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Import_All_From) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Import_All_From) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Import_From
        (Node : Bare_Import_From; Info : Internal_Entity_Info)
         return Internal_Entity_Import_From is
      begin
         if Node = null then
            return No_Entity_Import_From;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Import_From) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Import_From) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Imported_Id
        (Node : Bare_Imported_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Imported_Id is
      begin
         if Node = null then
            return No_Entity_Imported_Id;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Imported_Id) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Imported_Id) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Imported_Name
        (Node : Bare_Imported_Name; Info : Internal_Entity_Info)
         return Internal_Entity_Imported_Name is
      begin
         if Node = null then
            return No_Entity_Imported_Name;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Imported_Name) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Imported_Name) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Imported_Name_List
        (Node : Bare_Imported_Name_List; Info : Internal_Entity_Info)
         return Internal_Entity_Imported_Name_List is
      begin
         if Node = null then
            return No_Entity_Imported_Name_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Imported_Name_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Imported_Name_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Integer_Pattern
        (Node : Bare_Integer_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Integer_Pattern is
      begin
         if Node = null then
            return No_Entity_Integer_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Integer_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Integer_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Isa
        (Node : Bare_Isa; Info : Internal_Entity_Info)
         return Internal_Entity_Isa is
      begin
         if Node = null then
            return No_Entity_Isa;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Isa) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Isa) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Keep_Expr
        (Node : Bare_Keep_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Keep_Expr is
      begin
         if Node = null then
            return No_Entity_Keep_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Keep_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Keep_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lambda_Expr
        (Node : Bare_Lambda_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Lambda_Expr is
      begin
         if Node = null then
            return No_Entity_Lambda_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lambda_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lambda_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lambda_Param_Decl
        (Node : Bare_Lambda_Param_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Lambda_Param_Decl is
      begin
         if Node = null then
            return No_Entity_Lambda_Param_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lambda_Param_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lambda_Param_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lambda_Param_Decl_List
        (Node : Bare_Lambda_Param_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Lambda_Param_Decl_List is
      begin
         if Node = null then
            return No_Entity_Lambda_Param_Decl_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lambda_Param_Decl_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lambda_Param_Decl_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Langkit_Root
        (Node : Bare_Langkit_Root; Info : Internal_Entity_Info)
         return Internal_Entity_Langkit_Root is
      begin
         if Node = null then
            return No_Entity_Langkit_Root;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Langkit_Root) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Langkit_Root) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lexer_Case_Rule
        (Node : Bare_Lexer_Case_Rule; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Case_Rule is
      begin
         if Node = null then
            return No_Entity_Lexer_Case_Rule;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lexer_Case_Rule) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lexer_Case_Rule) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lexer_Case_Rule_Cond_Alt
        (Node : Bare_Lexer_Case_Rule_Cond_Alt; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Case_Rule_Cond_Alt is
      begin
         if Node = null then
            return No_Entity_Lexer_Case_Rule_Cond_Alt;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lexer_Case_Rule_Cond_Alt) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lexer_Case_Rule_Cond_Alt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lexer_Case_Rule_Default_Alt
        (Node : Bare_Lexer_Case_Rule_Default_Alt; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Case_Rule_Default_Alt is
      begin
         if Node = null then
            return No_Entity_Lexer_Case_Rule_Default_Alt;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lexer_Case_Rule_Default_Alt) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lexer_Case_Rule_Default_Alt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lexer_Case_Rule_Send
        (Node : Bare_Lexer_Case_Rule_Send; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Case_Rule_Send is
      begin
         if Node = null then
            return No_Entity_Lexer_Case_Rule_Send;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lexer_Case_Rule_Send) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lexer_Case_Rule_Send) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lexer_Decl
        (Node : Bare_Lexer_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Decl is
      begin
         if Node = null then
            return No_Entity_Lexer_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lexer_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lexer_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lexer_Family_Decl
        (Node : Bare_Lexer_Family_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Family_Decl is
      begin
         if Node = null then
            return No_Entity_Lexer_Family_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lexer_Family_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lexer_Family_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_List_Kind
        (Node : Bare_List_Kind; Info : Internal_Entity_Info)
         return Internal_Entity_List_Kind is
      begin
         if Node = null then
            return No_Entity_List_Kind;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_List_Kind) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_List_Kind) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_List_Kind_One
        (Node : Bare_List_Kind_One; Info : Internal_Entity_Info)
         return Internal_Entity_List_Kind_One is
      begin
         if Node = null then
            return No_Entity_List_Kind_One;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_List_Kind_One) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_List_Kind_One) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_List_Kind_Zero
        (Node : Bare_List_Kind_Zero; Info : Internal_Entity_Info)
         return Internal_Entity_List_Kind_Zero is
      begin
         if Node = null then
            return No_Entity_List_Kind_Zero;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_List_Kind_Zero) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_List_Kind_Zero) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_List_Pattern
        (Node : Bare_List_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_List_Pattern is
      begin
         if Node = null then
            return No_Entity_List_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_List_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_List_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Lkt_Node_List
        (Node : Bare_Lkt_Node_List; Info : Internal_Entity_Info)
         return Internal_Entity_Lkt_Node_List is
      begin
         if Node = null then
            return No_Entity_Lkt_Node_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Lkt_Node_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Lkt_Node_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Logic_Assign
        (Node : Bare_Logic_Assign; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Assign is
      begin
         if Node = null then
            return No_Entity_Logic_Assign;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Logic_Assign) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Logic_Assign) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Logic_Call_Expr
        (Node : Bare_Logic_Call_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Call_Expr is
      begin
         if Node = null then
            return No_Entity_Logic_Call_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Logic_Call_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Logic_Call_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Logic_Expr
        (Node : Bare_Logic_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Expr is
      begin
         if Node = null then
            return No_Entity_Logic_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Logic_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Logic_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Logic_Predicate
        (Node : Bare_Logic_Predicate; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Predicate is
      begin
         if Node = null then
            return No_Entity_Logic_Predicate;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Logic_Predicate) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Logic_Predicate) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Logic_Propagate
        (Node : Bare_Logic_Propagate; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Propagate is
      begin
         if Node = null then
            return No_Entity_Logic_Propagate;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Logic_Propagate) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Logic_Propagate) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Logic_Propagate_Call
        (Node : Bare_Logic_Propagate_Call; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Propagate_Call is
      begin
         if Node = null then
            return No_Entity_Logic_Propagate_Call;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Logic_Propagate_Call) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Logic_Propagate_Call) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Logic_Unify
        (Node : Bare_Logic_Unify; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Unify is
      begin
         if Node = null then
            return No_Entity_Logic_Unify;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Logic_Unify) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Logic_Unify) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Match_Branch
        (Node : Bare_Match_Branch; Info : Internal_Entity_Info)
         return Internal_Entity_Match_Branch is
      begin
         if Node = null then
            return No_Entity_Match_Branch;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Match_Branch) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Match_Branch) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Match_Expr
        (Node : Bare_Match_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Match_Expr is
      begin
         if Node = null then
            return No_Entity_Match_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Match_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Match_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Match_Val_Decl
        (Node : Bare_Match_Val_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Match_Val_Decl is
      begin
         if Node = null then
            return No_Entity_Match_Val_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Match_Val_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Match_Val_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Module_Doc_String_Line
        (Node : Bare_Module_Doc_String_Line; Info : Internal_Entity_Info)
         return Internal_Entity_Module_Doc_String_Line is
      begin
         if Node = null then
            return No_Entity_Module_Doc_String_Line;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Module_Doc_String_Line) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Module_Doc_String_Line) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Module_Doc_String_Line_List
        (Node : Bare_Module_Doc_String_Line_List; Info : Internal_Entity_Info)
         return Internal_Entity_Module_Doc_String_Line_List is
      begin
         if Node = null then
            return No_Entity_Module_Doc_String_Line_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Module_Doc_String_Line_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Module_Doc_String_Line_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Module_Doc_String_Lit
        (Node : Bare_Module_Doc_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Module_Doc_String_Lit is
      begin
         if Node = null then
            return No_Entity_Module_Doc_String_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Module_Doc_String_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Module_Doc_String_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Module_Id
        (Node : Bare_Module_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Module_Id is
      begin
         if Node = null then
            return No_Entity_Module_Id;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Module_Id) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Module_Id) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Node_Decl
        (Node : Bare_Node_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Node_Decl is
      begin
         if Node = null then
            return No_Entity_Node_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Node_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Node_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Not_Expr
        (Node : Bare_Not_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Not_Expr is
      begin
         if Node = null then
            return No_Entity_Not_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Not_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Not_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Not_Pattern
        (Node : Bare_Not_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Not_Pattern is
      begin
         if Node = null then
            return No_Entity_Not_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Not_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Not_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Null_Cond_Qualifier
        (Node : Bare_Null_Cond_Qualifier; Info : Internal_Entity_Info)
         return Internal_Entity_Null_Cond_Qualifier is
      begin
         if Node = null then
            return No_Entity_Null_Cond_Qualifier;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Null_Cond_Qualifier) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Null_Cond_Qualifier) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Null_Cond_Qualifier_Absent
        (Node : Bare_Null_Cond_Qualifier_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Null_Cond_Qualifier_Absent is
      begin
         if Node = null then
            return No_Entity_Null_Cond_Qualifier_Absent;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Null_Cond_Qualifier_Absent) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Null_Cond_Qualifier_Absent) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Null_Cond_Qualifier_Present
        (Node : Bare_Null_Cond_Qualifier_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Null_Cond_Qualifier_Present is
      begin
         if Node = null then
            return No_Entity_Null_Cond_Qualifier_Present;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Null_Cond_Qualifier_Present) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Null_Cond_Qualifier_Present) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Null_Lit
        (Node : Bare_Null_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Null_Lit is
      begin
         if Node = null then
            return No_Entity_Null_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Null_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Null_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Null_Pattern
        (Node : Bare_Null_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Null_Pattern is
      begin
         if Node = null then
            return No_Entity_Null_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Null_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Null_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Num_Lit
        (Node : Bare_Num_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Num_Lit is
      begin
         if Node = null then
            return No_Entity_Num_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Num_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Num_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op
        (Node : Bare_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Op is
      begin
         if Node = null then
            return No_Entity_Op;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Amp
        (Node : Bare_Op_Amp; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Amp is
      begin
         if Node = null then
            return No_Entity_Op_Amp;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Amp) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Amp) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_And
        (Node : Bare_Op_And; Info : Internal_Entity_Info)
         return Internal_Entity_Op_And is
      begin
         if Node = null then
            return No_Entity_Op_And;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_And) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_And) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Div
        (Node : Bare_Op_Div; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Div is
      begin
         if Node = null then
            return No_Entity_Op_Div;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Div) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Div) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Eq
        (Node : Bare_Op_Eq; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Eq is
      begin
         if Node = null then
            return No_Entity_Op_Eq;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Eq) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Eq) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Gt
        (Node : Bare_Op_Gt; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Gt is
      begin
         if Node = null then
            return No_Entity_Op_Gt;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Gt) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Gt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Gte
        (Node : Bare_Op_Gte; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Gte is
      begin
         if Node = null then
            return No_Entity_Op_Gte;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Gte) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Gte) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Logic_And
        (Node : Bare_Op_Logic_And; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Logic_And is
      begin
         if Node = null then
            return No_Entity_Op_Logic_And;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Logic_And) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Logic_And) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Logic_Or
        (Node : Bare_Op_Logic_Or; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Logic_Or is
      begin
         if Node = null then
            return No_Entity_Op_Logic_Or;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Logic_Or) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Logic_Or) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Lt
        (Node : Bare_Op_Lt; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Lt is
      begin
         if Node = null then
            return No_Entity_Op_Lt;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Lt) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Lt) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Lte
        (Node : Bare_Op_Lte; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Lte is
      begin
         if Node = null then
            return No_Entity_Op_Lte;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Lte) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Lte) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Minus
        (Node : Bare_Op_Minus; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Minus is
      begin
         if Node = null then
            return No_Entity_Op_Minus;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Minus) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Minus) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Mult
        (Node : Bare_Op_Mult; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Mult is
      begin
         if Node = null then
            return No_Entity_Op_Mult;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Mult) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Mult) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Ne
        (Node : Bare_Op_Ne; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Ne is
      begin
         if Node = null then
            return No_Entity_Op_Ne;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Ne) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Ne) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Or
        (Node : Bare_Op_Or; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Or is
      begin
         if Node = null then
            return No_Entity_Op_Or;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Or) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Or) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Or_Int
        (Node : Bare_Op_Or_Int; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Or_Int is
      begin
         if Node = null then
            return No_Entity_Op_Or_Int;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Or_Int) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Or_Int) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Plus
        (Node : Bare_Op_Plus; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Plus is
      begin
         if Node = null then
            return No_Entity_Op_Plus;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Plus) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Plus) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Stream_Concat
        (Node : Bare_Op_Stream_Concat; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Stream_Concat is
      begin
         if Node = null then
            return No_Entity_Op_Stream_Concat;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Stream_Concat) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Stream_Concat) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Op_Stream_Cons
        (Node : Bare_Op_Stream_Cons; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Stream_Cons is
      begin
         if Node = null then
            return No_Entity_Op_Stream_Cons;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Op_Stream_Cons) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Op_Stream_Cons) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Or_Pattern
        (Node : Bare_Or_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Or_Pattern is
      begin
         if Node = null then
            return No_Entity_Or_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Or_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Or_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Paren_Expr
        (Node : Bare_Paren_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Paren_Expr is
      begin
         if Node = null then
            return No_Entity_Paren_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Paren_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Paren_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Paren_Pattern
        (Node : Bare_Paren_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Paren_Pattern is
      begin
         if Node = null then
            return No_Entity_Paren_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Paren_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Paren_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Parse_Node_Expr
        (Node : Bare_Parse_Node_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Parse_Node_Expr is
      begin
         if Node = null then
            return No_Entity_Parse_Node_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Parse_Node_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Parse_Node_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Pattern_Detail_List
        (Node : Bare_Pattern_Detail_List; Info : Internal_Entity_Info)
         return Internal_Entity_Pattern_Detail_List is
      begin
         if Node = null then
            return No_Entity_Pattern_Detail_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Pattern_Detail_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Pattern_Detail_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Pattern_List
        (Node : Bare_Pattern_List; Info : Internal_Entity_Info)
         return Internal_Entity_Pattern_List is
      begin
         if Node = null then
            return No_Entity_Pattern_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Pattern_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Pattern_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Pattern_Match_Branch
        (Node : Bare_Pattern_Match_Branch; Info : Internal_Entity_Info)
         return Internal_Entity_Pattern_Match_Branch is
      begin
         if Node = null then
            return No_Entity_Pattern_Match_Branch;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Pattern_Match_Branch) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Pattern_Match_Branch) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Single_Line_String_Lit
        (Node : Bare_Single_Line_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Single_Line_String_Lit is
      begin
         if Node = null then
            return No_Entity_Single_Line_String_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Single_Line_String_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Single_Line_String_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Pattern_Single_Line_String_Lit
        (Node : Bare_Pattern_Single_Line_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Pattern_Single_Line_String_Lit is
      begin
         if Node = null then
            return No_Entity_Pattern_Single_Line_String_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Pattern_Single_Line_String_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Pattern_Single_Line_String_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Property_Pattern_Detail
        (Node : Bare_Property_Pattern_Detail; Info : Internal_Entity_Info)
         return Internal_Entity_Property_Pattern_Detail is
      begin
         if Node = null then
            return No_Entity_Property_Pattern_Detail;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Property_Pattern_Detail) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Property_Pattern_Detail) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Query
        (Node : Bare_Query; Info : Internal_Entity_Info)
         return Internal_Entity_Query is
      begin
         if Node = null then
            return No_Entity_Query;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Query) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Query) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Raise_Expr
        (Node : Bare_Raise_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Raise_Expr is
      begin
         if Node = null then
            return No_Entity_Raise_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Raise_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Raise_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ref_Id
        (Node : Bare_Ref_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Ref_Id is
      begin
         if Node = null then
            return No_Entity_Ref_Id;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Ref_Id) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ref_Id) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Ref_Id_List
        (Node : Bare_Ref_Id_List; Info : Internal_Entity_Info)
         return Internal_Entity_Ref_Id_List is
      begin
         if Node = null then
            return No_Entity_Ref_Id_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Ref_Id_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Ref_Id_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Regex_Pattern
        (Node : Bare_Regex_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Regex_Pattern is
      begin
         if Node = null then
            return No_Entity_Regex_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Regex_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Regex_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Renaming_Complex_Pattern
        (Node : Bare_Renaming_Complex_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Renaming_Complex_Pattern is
      begin
         if Node = null then
            return No_Entity_Renaming_Complex_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Renaming_Complex_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Renaming_Complex_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Self_Decl
        (Node : Bare_Self_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Self_Decl is
      begin
         if Node = null then
            return No_Entity_Self_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Self_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Self_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Simple_Type_Ref
        (Node : Bare_Simple_Type_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Simple_Type_Ref is
      begin
         if Node = null then
            return No_Entity_Simple_Type_Ref;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Simple_Type_Ref) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Simple_Type_Ref) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Struct_Decl
        (Node : Bare_Struct_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Struct_Decl is
      begin
         if Node = null then
            return No_Entity_Struct_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Struct_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Struct_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Subscript_Expr
        (Node : Bare_Subscript_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Subscript_Expr is
      begin
         if Node = null then
            return No_Entity_Subscript_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Subscript_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Subscript_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Synth_Fun_Decl
        (Node : Bare_Synth_Fun_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Synth_Fun_Decl is
      begin
         if Node = null then
            return No_Entity_Synth_Fun_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Synth_Fun_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Synth_Fun_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Synth_Param_Decl
        (Node : Bare_Synth_Param_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Synth_Param_Decl is
      begin
         if Node = null then
            return No_Entity_Synth_Param_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Synth_Param_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Synth_Param_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Synthetic_Lexer_Decl
        (Node : Bare_Synthetic_Lexer_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Synthetic_Lexer_Decl is
      begin
         if Node = null then
            return No_Entity_Synthetic_Lexer_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Synthetic_Lexer_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Synthetic_Lexer_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Type_Ref_List
        (Node : Bare_Type_Ref_List; Info : Internal_Entity_Info)
         return Internal_Entity_Type_Ref_List is
      begin
         if Node = null then
            return No_Entity_Type_Ref_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Type_Ref_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Type_Ref_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Synthetic_Type_Ref_List
        (Node : Bare_Synthetic_Type_Ref_List; Info : Internal_Entity_Info)
         return Internal_Entity_Synthetic_Type_Ref_List is
      begin
         if Node = null then
            return No_Entity_Synthetic_Type_Ref_List;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Synthetic_Type_Ref_List) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Synthetic_Type_Ref_List) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Token_Lit
        (Node : Bare_Token_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Token_Lit is
      begin
         if Node = null then
            return No_Entity_Token_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Token_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Token_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Token_No_Case_Lit
        (Node : Bare_Token_No_Case_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Token_No_Case_Lit is
      begin
         if Node = null then
            return No_Entity_Token_No_Case_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Token_No_Case_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Token_No_Case_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Token_Pattern_Concat
        (Node : Bare_Token_Pattern_Concat; Info : Internal_Entity_Info)
         return Internal_Entity_Token_Pattern_Concat is
      begin
         if Node = null then
            return No_Entity_Token_Pattern_Concat;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Token_Pattern_Concat) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Token_Pattern_Concat) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Token_Pattern_Lit
        (Node : Bare_Token_Pattern_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Token_Pattern_Lit is
      begin
         if Node = null then
            return No_Entity_Token_Pattern_Lit;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Token_Pattern_Lit) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Token_Pattern_Lit) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Token_Ref
        (Node : Bare_Token_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Token_Ref is
      begin
         if Node = null then
            return No_Entity_Token_Ref;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Token_Ref) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Token_Ref) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Trait_Decl
        (Node : Bare_Trait_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Trait_Decl is
      begin
         if Node = null then
            return No_Entity_Trait_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Trait_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Trait_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Try_Expr
        (Node : Bare_Try_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Try_Expr is
      begin
         if Node = null then
            return No_Entity_Try_Expr;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Try_Expr) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Try_Expr) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Type_Pattern
        (Node : Bare_Type_Pattern; Info : Internal_Entity_Info)
         return Internal_Entity_Type_Pattern is
      begin
         if Node = null then
            return No_Entity_Type_Pattern;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Type_Pattern) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Type_Pattern) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Un_Op
        (Node : Bare_Un_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Un_Op is
      begin
         if Node = null then
            return No_Entity_Un_Op;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Un_Op) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Un_Op) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Val_Decl
        (Node : Bare_Val_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Val_Decl is
      begin
         if Node = null then
            return No_Entity_Val_Decl;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Val_Decl) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Val_Decl) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   



      function Create_Internal_Entity_Var_Bind
        (Node : Bare_Var_Bind; Info : Internal_Entity_Info)
         return Internal_Entity_Var_Bind is
      begin
         if Node = null then
            return No_Entity_Var_Bind;
         end if;
         return (Node => Node, Info => Info);
      end;



      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Entity_Var_Bind) return Boolean is
      begin
         return L.Node = R.Node and then Equivalent (L.Info, R.Info);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Entity_Var_Bind) return String is
         pragma Warnings (On, "referenced");
      begin
            return Image (Internal_Entity'(Node => R.Node, Info => R.Info));
      end Trace_Image;


   

   


      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : Internal_Env_Assoc) is
      begin
               Inc_Ref (R.Dest_Env);
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out Internal_Env_Assoc) is
      begin
               Dec_Ref (R.Dest_Env);
      end Dec_Ref;




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Env_Assoc) return Boolean is
      begin
         return L.Key = R.Key and then L.Value = R.Value and then Equivalent (L.Dest_Env, R.Dest_Env) and then L.Metadata = R.Metadata;
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Env_Assoc) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Key => "
                     & Trace_Image (R.Key)
                        & ", "
                     & "Value => "
                     & Trace_Image (R.Value)
                        & ", "
                     & "Dest_Env => "
                     & Trace_Image (R.Dest_Env)
                        & ", "
                     & "Metadata => "
                     & Trace_Image (R.Metadata)
               & ")");
      end Trace_Image;


   

   




   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Inner_Env_Assoc) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Key => "
                     & Trace_Image (R.Key)
                        & ", "
                     & "Value => "
                     & Trace_Image (R.Value)
                        & ", "
                     & "Rebindings => "
                     & Trace_Image (R.Rebindings)
                        & ", "
                     & "Metadata => "
                     & Trace_Image (R.Metadata)
               & ")");
      end Trace_Image;


   

   




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Logic_Context) return Boolean is
      begin
         return Equivalent (L.Ref_Node, R.Ref_Node) and then Equivalent (L.Decl_Node, R.Decl_Node);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Logic_Context) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Ref_Node => "
                     & Trace_Image (R.Ref_Node)
                        & ", "
                     & "Decl_Node => "
                     & Trace_Image (R.Decl_Node)
               & ")");
      end Trace_Image;


   

   




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Resolved_Param) return Boolean is
      begin
         return L.Name = R.Name and then Equivalent (L.Param_Type, R.Param_Type) and then L.Has_Default_Value = R.Has_Default_Value and then L.Accept_Logical_Var = R.Accept_Logical_Var and then Equivalent (L.Decl, R.Decl);
      end Equivalent;


   

      ----------
      -- Hash --
      ----------

      pragma Warnings (Off, "referenced");
      function Hash (R : Internal_Resolved_Param) return Hash_Type is
         pragma Warnings (On, "referenced");
      begin
         
            return Combine ((Hash (R.Name), Hash (R.Param_Type), Hash (R.Has_Default_Value), Hash (R.Accept_Logical_Var), Hash (R.Decl)));
      end Hash;


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Resolved_Param) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Name => "
                     & Trace_Image (R.Name)
                        & ", "
                     & "Param_Type => "
                     & Trace_Image (R.Param_Type)
                        & ", "
                     & "Has_Default_Value => "
                     & Trace_Image (R.Has_Default_Value)
                        & ", "
                     & "Accept_Logical_Var => "
                     & Trace_Image (R.Accept_Logical_Var)
                        & ", "
                     & "Decl => "
                     & Trace_Image (R.Decl)
               & ")");
      end Trace_Image;


   

   




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Param_Match) return Boolean is
      begin
         return L.Has_Matched = R.Has_Matched and then Equivalent (L.Arg, R.Arg) and then Equivalent (L.Param, R.Param);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Param_Match) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Has_Matched => "
                     & Trace_Image (R.Has_Matched)
                        & ", "
                     & "Arg => "
                     & Trace_Image (R.Arg)
                        & ", "
                     & "Param => "
                     & Trace_Image (R.Param)
               & ")");
      end Trace_Image;


   

   




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Ref_Result) return Boolean is
      begin
         return Equivalent (L.Ref, R.Ref);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Ref_Result) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Ref => "
                     & Trace_Image (R.Ref)
               & ")");
      end Trace_Image;


   

   


      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : Internal_Solver_Diagnostic) is
      begin
               Inc_Ref (R.Message_Template);
               Inc_Ref (R.Args);
               Inc_Ref (R.Contexts);
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out Internal_Solver_Diagnostic) is
      begin
               Dec_Ref (R.Message_Template);
               Dec_Ref (R.Args);
               Dec_Ref (R.Contexts);
      end Dec_Ref;




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Solver_Diagnostic) return Boolean is
      begin
         return Equivalent (L.Message_Template, R.Message_Template) and then Equivalent (L.Args, R.Args) and then L.Location = R.Location and then Equivalent (L.Contexts, R.Contexts) and then L.Round = R.Round;
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Solver_Diagnostic) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Message_Template => "
                     & Trace_Image (R.Message_Template)
                        & ", "
                     & "Args => "
                     & Trace_Image (R.Args)
                        & ", "
                     & "Location => "
                     & Trace_Image (R.Location)
                        & ", "
                     & "Contexts => "
                     & Trace_Image (R.Contexts)
                        & ", "
                     & "Round => "
                     & Trace_Image (R.Round)
               & ")");
      end Trace_Image;


   

   


      -------------
      -- Inc_Ref --
      -------------

      procedure Inc_Ref (R : Internal_Solver_Result) is
      begin
               Inc_Ref (R.Diagnostics);
      end Inc_Ref;

      -------------
      -- Dec_Ref --
      -------------

      procedure Dec_Ref (R : in out Internal_Solver_Result) is
      begin
               Dec_Ref (R.Diagnostics);
      end Dec_Ref;




      ----------------
      -- Equivalent --
      ----------------

      function Equivalent (L, R : Internal_Solver_Result) return Boolean is
      begin
         return L.Success = R.Success and then Equivalent (L.Diagnostics, R.Diagnostics);
      end Equivalent;


   


      -----------------
      -- Trace_Image --
      -----------------

      pragma Warnings (Off, "referenced");
      function Trace_Image (R : Internal_Solver_Result) return String is
         pragma Warnings (On, "referenced");
      begin
            return
              ("("
                     & "Success => "
                     & Trace_Image (R.Success)
                        & ", "
                     & "Diagnostics => "
                     & Trace_Image (R.Diagnostics)
               & ")");
      end Trace_Image;



      

   --
   --  Primitives for Bare_Lkt_Node
   --

   




   


      

   --
   --  Primitives for Bare_Argument
   --

   

      
      procedure Initialize_Fields_For_Argument
        (Self : Bare_Argument
         ; Argument_F_Name : Bare_Ref_Id
         ; Argument_F_Value : Bare_Expr
        ) is
      begin

            Self.Argument_F_Name := Argument_F_Name;
            Self.Argument_F_Value := Argument_F_Value;
         

      end Initialize_Fields_For_Argument;

      
   function Argument_F_Name
     (Node : Bare_Argument) return Bare_Ref_Id
   is
      

   begin
         
         return Node.Argument_F_Name;
      
   end;

      
   function Argument_F_Value
     (Node : Bare_Argument) return Bare_Expr
   is
      

   begin
         
         return Node.Argument_F_Value;
      
   end;



   


      

   --
   --  Primitives for Bare_Base_Import
   --

   




   


      

   --
   --  Primitives for Bare_Import
   --

   

      
      procedure Initialize_Fields_For_Import
        (Self : Bare_Import
         ; Import_F_Imported_Names : Bare_Imported_Name_List
        ) is
      begin

            Self.Import_F_Imported_Names := Import_F_Imported_Names;
         

      end Initialize_Fields_For_Import;

      
   function Import_F_Imported_Names
     (Node : Bare_Import) return Bare_Imported_Name_List
   is
      

   begin
         
         return Node.Import_F_Imported_Names;
      
   end;



   


      

   --
   --  Primitives for Bare_Import_All_From
   --

   

      
      procedure Initialize_Fields_For_Import_All_From
        (Self : Bare_Import_All_From
         ; Import_All_From_F_Module_Name : Bare_Module_Id
        ) is
      begin

            Self.Import_All_From_F_Module_Name := Import_All_From_F_Module_Name;
         

      end Initialize_Fields_For_Import_All_From;

      
   function Import_All_From_F_Module_Name
     (Node : Bare_Import_All_From) return Bare_Module_Id
   is
      

   begin
         
         return Node.Import_All_From_F_Module_Name;
      
   end;



   


      

   --
   --  Primitives for Bare_Import_From
   --

   

      
      procedure Initialize_Fields_For_Import_From
        (Self : Bare_Import_From
         ; Import_From_F_Module_Name : Bare_Module_Id
         ; Import_From_F_Imported_Names : Bare_Imported_Name_List
        ) is
      begin

            Self.Import_From_F_Module_Name := Import_From_F_Module_Name;
            Self.Import_From_F_Imported_Names := Import_From_F_Imported_Names;
         

      end Initialize_Fields_For_Import_From;

      
   function Import_From_F_Module_Name
     (Node : Bare_Import_From) return Bare_Module_Id
   is
      

   begin
         
         return Node.Import_From_F_Module_Name;
      
   end;

      
   function Import_From_F_Imported_Names
     (Node : Bare_Import_From) return Bare_Imported_Name_List
   is
      

   begin
         
         return Node.Import_From_F_Imported_Names;
      
   end;



   


      

   --
   --  Primitives for Bare_Base_Lexer_Case_Rule_Alt
   --

   




   


      

   --
   --  Primitives for Bare_Error_Lexer_Case_Rule_Alt
   --

   




   


      

   --
   --  Primitives for Bare_Lexer_Case_Rule_Cond_Alt
   --

   

      
      procedure Initialize_Fields_For_Lexer_Case_Rule_Cond_Alt
        (Self : Bare_Lexer_Case_Rule_Cond_Alt
         ; Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : Bare_Ref_Id_List
         ; Lexer_Case_Rule_Cond_Alt_F_Send : Bare_Lexer_Case_Rule_Send
        ) is
      begin

            Self.Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs := Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs;
            Self.Lexer_Case_Rule_Cond_Alt_F_Send := Lexer_Case_Rule_Cond_Alt_F_Send;
         

      end Initialize_Fields_For_Lexer_Case_Rule_Cond_Alt;

      
   function Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs
     (Node : Bare_Lexer_Case_Rule_Cond_Alt) return Bare_Ref_Id_List
   is
      

   begin
         
         return Node.Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs;
      
   end;

      
   function Lexer_Case_Rule_Cond_Alt_F_Send
     (Node : Bare_Lexer_Case_Rule_Cond_Alt) return Bare_Lexer_Case_Rule_Send
   is
      

   begin
         
         return Node.Lexer_Case_Rule_Cond_Alt_F_Send;
      
   end;



   


      

   --
   --  Primitives for Bare_Lexer_Case_Rule_Default_Alt
   --

   

      
      procedure Initialize_Fields_For_Lexer_Case_Rule_Default_Alt
        (Self : Bare_Lexer_Case_Rule_Default_Alt
         ; Lexer_Case_Rule_Default_Alt_F_Send : Bare_Lexer_Case_Rule_Send
        ) is
      begin

            Self.Lexer_Case_Rule_Default_Alt_F_Send := Lexer_Case_Rule_Default_Alt_F_Send;
         

      end Initialize_Fields_For_Lexer_Case_Rule_Default_Alt;

      
   function Lexer_Case_Rule_Default_Alt_F_Send
     (Node : Bare_Lexer_Case_Rule_Default_Alt) return Bare_Lexer_Case_Rule_Send
   is
      

   begin
         
         return Node.Lexer_Case_Rule_Default_Alt_F_Send;
      
   end;



   


      

   --
   --  Primitives for Bare_Base_Match_Branch
   --

   


      
   function Base_Match_Branch_F_Expr
     (Node : Bare_Base_Match_Branch) return Bare_Expr
   is
      

         Kind : constant Lkt_Base_Match_Branch := Node.Kind;
   begin
         case Kind is
               when Lkt_Match_Branch =>
                     
         return Node.Match_Branch_F_Expr;
      
               when Lkt_Pattern_Match_Branch =>
                     
         return Node.Pattern_Match_Branch_F_Expr;
      
         end case;
   end;



   


      

   --
   --  Primitives for Bare_Match_Branch
   --

   

      
      procedure Initialize_Fields_For_Match_Branch
        (Self : Bare_Match_Branch
         ; Match_Branch_F_Decl : Bare_Match_Val_Decl
         ; Match_Branch_F_Expr : Bare_Expr
        ) is
      begin

            Self.Match_Branch_F_Decl := Match_Branch_F_Decl;
            Self.Match_Branch_F_Expr := Match_Branch_F_Expr;
         

      end Initialize_Fields_For_Match_Branch;

      
   function Match_Branch_F_Decl
     (Node : Bare_Match_Branch) return Bare_Match_Val_Decl
   is
      

   begin
         
         return Node.Match_Branch_F_Decl;
      
   end;



   


      

   --
   --  Primitives for Bare_Pattern_Match_Branch
   --

   

      
      procedure Initialize_Fields_For_Pattern_Match_Branch
        (Self : Bare_Pattern_Match_Branch
         ; Pattern_Match_Branch_F_Pattern : Bare_Pattern
         ; Pattern_Match_Branch_F_Expr : Bare_Expr
        ) is
      begin

            Self.Pattern_Match_Branch_F_Pattern := Pattern_Match_Branch_F_Pattern;
            Self.Pattern_Match_Branch_F_Expr := Pattern_Match_Branch_F_Expr;
         

      end Initialize_Fields_For_Pattern_Match_Branch;

      
   function Pattern_Match_Branch_F_Pattern
     (Node : Bare_Pattern_Match_Branch) return Bare_Pattern
   is
      

   begin
         
         return Node.Pattern_Match_Branch_F_Pattern;
      
   end;



   


      

   --
   --  Primitives for Bare_Block_Expr_Clause
   --

   

      
      procedure Initialize_Fields_For_Block_Expr_Clause
        (Self : Bare_Block_Expr_Clause
         ; Block_Expr_Clause_F_Clause : Bare_Lkt_Node
        ) is
      begin

            Self.Block_Expr_Clause_F_Clause := Block_Expr_Clause_F_Clause;
         

      end Initialize_Fields_For_Block_Expr_Clause;

      
   function Block_Expr_Clause_F_Clause
     (Node : Bare_Block_Expr_Clause) return Bare_Lkt_Node
   is
      

   begin
         
         return Node.Block_Expr_Clause_F_Clause;
      
   end;



   


      

   --
   --  Primitives for Bare_Block_String_Line
   --

   




   


      

   --
   --  Primitives for Bare_Class_Qualifier
   --

   




   


      

   --
   --  Primitives for Bare_Class_Qualifier_Absent
   --

   




   


      

   --
   --  Primitives for Bare_Class_Qualifier_Present
   --

   




   


      

   --
   --  Primitives for Bare_Decl
   --

   


      
   function Decl_F_Syn_Name
     (Node : Bare_Decl) return Bare_Def_Id
   is
      

         Kind : constant Lkt_Decl := Node.Kind;
   begin
         case Kind is
               when Lkt_Error_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Grammar_Rule_Decl =>
                     
         return Node.Grammar_Rule_Decl_F_Syn_Name;
      
               when Lkt_Synthetic_Lexer_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Node_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Self_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Enum_Lit_Decl =>
                     
         return Node.Enum_Lit_Decl_F_Syn_Name;
      
               when Lkt_Field_Decl =>
                     
         return Node.Field_Decl_F_Syn_Name;
      
               when Lkt_Fun_Param_Decl =>
                     
         return Node.Fun_Param_Decl_F_Syn_Name;
      
               when Lkt_Lambda_Param_Decl =>
                     
         return Node.Lambda_Param_Decl_F_Syn_Name;
      
               when Lkt_Dyn_Var_Decl =>
                     
         return Node.Dyn_Var_Decl_F_Syn_Name;
      
               when Lkt_Binding_Val_Decl =>
                     
         return Node.Binding_Val_Decl_F_Syn_Name;
      
               when Lkt_Match_Val_Decl =>
                     
         return Node.Match_Val_Decl_F_Syn_Name;
      
               when Lkt_Val_Decl =>
                     
         return Node.Val_Decl_F_Syn_Name;
      
               when Lkt_Fun_Decl =>
                     
         return Node.Fun_Decl_F_Syn_Name;
      
               when Lkt_Env_Spec_Decl =>
                     
         return Node.Env_Spec_Decl_F_Syn_Name;
      
               when Lkt_Generic_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Grammar_Decl =>
                     
         return Node.Grammar_Decl_F_Syn_Name;
      
               when Lkt_Lexer_Decl =>
                     
         return Node.Lexer_Decl_F_Syn_Name;
      
               when Lkt_Lexer_Family_Decl =>
                     
         return Node.Lexer_Family_Decl_F_Syn_Name;
      
               when Lkt_Synth_Param_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Synth_Fun_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Any_Type_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Enum_Class_Alt_Decl =>
                     
         return Node.Enum_Class_Alt_Decl_F_Syn_Name;
      
               when Lkt_Function_Type =>
                     return No_Bare_Lkt_Node;
               when Lkt_Generic_Param_Type_Decl =>
                     
         return Node.Generic_Param_Type_Decl_F_Syn_Name;
      
               when Lkt_Class_Decl | Lkt_Enum_Class_Decl =>
                     
         return Node.Basic_Class_Decl_F_Syn_Name;
      
               when Lkt_Enum_Type_Decl =>
                     
         return Node.Enum_Type_Decl_F_Syn_Name;
      
               when Lkt_Struct_Decl =>
                     
         return Node.Struct_Decl_F_Syn_Name;
      
               when Lkt_Trait_Decl =>
                     
         return Node.Trait_Decl_F_Syn_Name;
      
               when Lkt_Langkit_Root =>
                     return No_Bare_Lkt_Node;
         end case;
   end;



   


      

   --
   --  Primitives for Bare_Base_Grammar_Rule_Decl
   --

   


      
   function Base_Grammar_Rule_Decl_F_Expr
     (Node : Bare_Base_Grammar_Rule_Decl) return Bare_Grammar_Expr
   is
      

         Kind : constant Lkt_Base_Grammar_Rule_Decl := Node.Kind;
   begin
         case Kind is
               when Lkt_Grammar_Rule_Decl =>
                     
         return Node.Grammar_Rule_Decl_F_Expr;
      
               when Lkt_Synthetic_Lexer_Decl =>
                     return No_Bare_Lkt_Node;
         end case;
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Rule_Decl
   --

   

      
      procedure Initialize_Fields_For_Grammar_Rule_Decl
        (Self : Bare_Grammar_Rule_Decl
         ; Grammar_Rule_Decl_F_Syn_Name : Bare_Def_Id
         ; Grammar_Rule_Decl_F_Expr : Bare_Grammar_Expr
        ) is
      begin

            Self.Grammar_Rule_Decl_F_Syn_Name := Grammar_Rule_Decl_F_Syn_Name;
            Self.Grammar_Rule_Decl_F_Expr := Grammar_Rule_Decl_F_Expr;
         

      end Initialize_Fields_For_Grammar_Rule_Decl;



   


      

   --
   --  Primitives for Bare_Synthetic_Lexer_Decl
   --

   

      
      procedure Initialize_Fields_For_Synthetic_Lexer_Decl
        (Self : Bare_Synthetic_Lexer_Decl
        ) is
      begin

         
      Self.Synthetic_Lexer_Decl_F_Sym := No_Symbol;

      end Initialize_Fields_For_Synthetic_Lexer_Decl;



   


      

   --
   --  Primitives for Bare_Base_Val_Decl
   --

   




   


      

   --
   --  Primitives for Bare_Node_Decl
   --

   




   


      

   --
   --  Primitives for Bare_Self_Decl
   --

   




   


      

   --
   --  Primitives for Bare_User_Val_Decl
   --

   




   


      

   --
   --  Primitives for Bare_Binding_Val_Decl
   --

   

      
      procedure Initialize_Fields_For_Binding_Val_Decl
        (Self : Bare_Binding_Val_Decl
         ; Binding_Val_Decl_F_Syn_Name : Bare_Def_Id
        ) is
      begin

            Self.Binding_Val_Decl_F_Syn_Name := Binding_Val_Decl_F_Syn_Name;
         

      end Initialize_Fields_For_Binding_Val_Decl;



   


      

   --
   --  Primitives for Bare_Enum_Lit_Decl
   --

   

      
      procedure Initialize_Fields_For_Enum_Lit_Decl
        (Self : Bare_Enum_Lit_Decl
         ; Enum_Lit_Decl_F_Syn_Name : Bare_Def_Id
        ) is
      begin

            Self.Enum_Lit_Decl_F_Syn_Name := Enum_Lit_Decl_F_Syn_Name;
         

      end Initialize_Fields_For_Enum_Lit_Decl;



   


      

   --
   --  Primitives for Bare_Explicitly_Typed_Decl
   --

   


      
   function Explicitly_Typed_Decl_F_Decl_Type
     (Node : Bare_Explicitly_Typed_Decl) return Bare_Type_Ref
   is
      

         Kind : constant Lkt_Explicitly_Typed_Decl := Node.Kind;
   begin
         case Kind is
               when Lkt_Field_Decl =>
                     
         return Node.Field_Decl_F_Decl_Type;
      
               when Lkt_Fun_Param_Decl =>
                     
         return Node.Fun_Param_Decl_F_Decl_Type;
      
               when Lkt_Lambda_Param_Decl =>
                     
         return Node.Lambda_Param_Decl_F_Decl_Type;
      
               when Lkt_Dyn_Var_Decl =>
                     
         return Node.Dyn_Var_Decl_F_Decl_Type;
      
               when Lkt_Match_Val_Decl =>
                     
         return Node.Match_Val_Decl_F_Decl_Type;
      
               when Lkt_Val_Decl =>
                     
         return Node.Val_Decl_F_Decl_Type;
      
         end case;
   end;



   


      

   --
   --  Primitives for Bare_Component_Decl
   --

   


      
   function Component_Decl_F_Default_Val
     (Node : Bare_Component_Decl) return Bare_Expr
   is
      

         Kind : constant Lkt_Component_Decl := Node.Kind;
   begin
         case Kind is
               when Lkt_Field_Decl =>
                     
         return Node.Field_Decl_F_Default_Val;
      
               when Lkt_Fun_Param_Decl =>
                     
         return Node.Fun_Param_Decl_F_Default_Val;
      
               when Lkt_Lambda_Param_Decl =>
                     
         return Node.Lambda_Param_Decl_F_Default_Val;
      
         end case;
   end;



   


      

   --
   --  Primitives for Bare_Field_Decl
   --

   

      
      procedure Initialize_Fields_For_Field_Decl
        (Self : Bare_Field_Decl
         ; Field_Decl_F_Syn_Name : Bare_Def_Id
         ; Field_Decl_F_Decl_Type : Bare_Type_Ref
         ; Field_Decl_F_Trait_Ref : Bare_Dot_Expr
         ; Field_Decl_F_Default_Val : Bare_Expr
        ) is
      begin

            Self.Field_Decl_F_Syn_Name := Field_Decl_F_Syn_Name;
            Self.Field_Decl_F_Decl_Type := Field_Decl_F_Decl_Type;
            Self.Field_Decl_F_Trait_Ref := Field_Decl_F_Trait_Ref;
            Self.Field_Decl_F_Default_Val := Field_Decl_F_Default_Val;
         

      end Initialize_Fields_For_Field_Decl;

      
   function Field_Decl_F_Trait_Ref
     (Node : Bare_Field_Decl) return Bare_Dot_Expr
   is
      

   begin
         
         return Node.Field_Decl_F_Trait_Ref;
      
   end;



   


      

   --
   --  Primitives for Bare_Fun_Param_Decl
   --

   

      
      procedure Initialize_Fields_For_Fun_Param_Decl
        (Self : Bare_Fun_Param_Decl
         ; Fun_Param_Decl_F_Decl_Annotations : Bare_Decl_Annotation_List
         ; Fun_Param_Decl_F_Syn_Name : Bare_Def_Id
         ; Fun_Param_Decl_F_Decl_Type : Bare_Type_Ref
         ; Fun_Param_Decl_F_Default_Val : Bare_Expr
        ) is
      begin

            Self.Fun_Param_Decl_F_Decl_Annotations := Fun_Param_Decl_F_Decl_Annotations;
            Self.Fun_Param_Decl_F_Syn_Name := Fun_Param_Decl_F_Syn_Name;
            Self.Fun_Param_Decl_F_Decl_Type := Fun_Param_Decl_F_Decl_Type;
            Self.Fun_Param_Decl_F_Default_Val := Fun_Param_Decl_F_Default_Val;
         

      end Initialize_Fields_For_Fun_Param_Decl;

      
   function Fun_Param_Decl_F_Decl_Annotations
     (Node : Bare_Fun_Param_Decl) return Bare_Decl_Annotation_List
   is
      

   begin
         
         return Node.Fun_Param_Decl_F_Decl_Annotations;
      
   end;



   


      

   --
   --  Primitives for Bare_Lambda_Param_Decl
   --

   

      
      procedure Initialize_Fields_For_Lambda_Param_Decl
        (Self : Bare_Lambda_Param_Decl
         ; Lambda_Param_Decl_F_Syn_Name : Bare_Def_Id
         ; Lambda_Param_Decl_F_Decl_Type : Bare_Type_Ref
         ; Lambda_Param_Decl_F_Default_Val : Bare_Expr
        ) is
      begin

            Self.Lambda_Param_Decl_F_Syn_Name := Lambda_Param_Decl_F_Syn_Name;
            Self.Lambda_Param_Decl_F_Decl_Type := Lambda_Param_Decl_F_Decl_Type;
            Self.Lambda_Param_Decl_F_Default_Val := Lambda_Param_Decl_F_Default_Val;
         
      Self.Lambda_Param_Decl_F_Type_Var := Null_Var_Record;

      end Initialize_Fields_For_Lambda_Param_Decl;



   


      

   --
   --  Primitives for Bare_Dyn_Var_Decl
   --

   

      
      procedure Initialize_Fields_For_Dyn_Var_Decl
        (Self : Bare_Dyn_Var_Decl
         ; Dyn_Var_Decl_F_Syn_Name : Bare_Def_Id
         ; Dyn_Var_Decl_F_Decl_Type : Bare_Type_Ref
        ) is
      begin

            Self.Dyn_Var_Decl_F_Syn_Name := Dyn_Var_Decl_F_Syn_Name;
            Self.Dyn_Var_Decl_F_Decl_Type := Dyn_Var_Decl_F_Decl_Type;
         

      end Initialize_Fields_For_Dyn_Var_Decl;



   


      

   --
   --  Primitives for Bare_Match_Val_Decl
   --

   

      
      procedure Initialize_Fields_For_Match_Val_Decl
        (Self : Bare_Match_Val_Decl
         ; Match_Val_Decl_F_Syn_Name : Bare_Def_Id
         ; Match_Val_Decl_F_Decl_Type : Bare_Type_Ref
        ) is
      begin

            Self.Match_Val_Decl_F_Syn_Name := Match_Val_Decl_F_Syn_Name;
            Self.Match_Val_Decl_F_Decl_Type := Match_Val_Decl_F_Decl_Type;
         

      end Initialize_Fields_For_Match_Val_Decl;



   


      

   --
   --  Primitives for Bare_Val_Decl
   --

   

      
      procedure Initialize_Fields_For_Val_Decl
        (Self : Bare_Val_Decl
         ; Val_Decl_F_Syn_Name : Bare_Def_Id
         ; Val_Decl_F_Decl_Type : Bare_Type_Ref
         ; Val_Decl_F_Expr : Bare_Expr
        ) is
      begin

            Self.Val_Decl_F_Syn_Name := Val_Decl_F_Syn_Name;
            Self.Val_Decl_F_Decl_Type := Val_Decl_F_Decl_Type;
            Self.Val_Decl_F_Expr := Val_Decl_F_Expr;
         

      end Initialize_Fields_For_Val_Decl;

      
   function Val_Decl_F_Expr
     (Node : Bare_Val_Decl) return Bare_Expr
   is
      

   begin
         
         return Node.Val_Decl_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Fun_Decl
   --

   

      
      procedure Initialize_Fields_For_Fun_Decl
        (Self : Bare_Fun_Decl
         ; Fun_Decl_F_Syn_Name : Bare_Def_Id
         ; Fun_Decl_F_Params : Bare_Fun_Param_Decl_List
         ; Fun_Decl_F_Return_Type : Bare_Type_Ref
         ; Fun_Decl_F_Trait_Ref : Bare_Dot_Expr
         ; Fun_Decl_F_Body : Bare_Expr
        ) is
      begin

            Self.Fun_Decl_F_Syn_Name := Fun_Decl_F_Syn_Name;
            Self.Fun_Decl_F_Params := Fun_Decl_F_Params;
            Self.Fun_Decl_F_Return_Type := Fun_Decl_F_Return_Type;
            Self.Fun_Decl_F_Trait_Ref := Fun_Decl_F_Trait_Ref;
            Self.Fun_Decl_F_Body := Fun_Decl_F_Body;
         

      end Initialize_Fields_For_Fun_Decl;

      
   function Fun_Decl_F_Params
     (Node : Bare_Fun_Decl) return Bare_Fun_Param_Decl_List
   is
      

   begin
         
         return Node.Fun_Decl_F_Params;
      
   end;

      
   function Fun_Decl_F_Return_Type
     (Node : Bare_Fun_Decl) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Fun_Decl_F_Return_Type;
      
   end;

      
   function Fun_Decl_F_Trait_Ref
     (Node : Bare_Fun_Decl) return Bare_Dot_Expr
   is
      

   begin
         
         return Node.Fun_Decl_F_Trait_Ref;
      
   end;

      
   function Fun_Decl_F_Body
     (Node : Bare_Fun_Decl) return Bare_Expr
   is
      

   begin
         
         return Node.Fun_Decl_F_Body;
      
   end;



   


      

   --
   --  Primitives for Bare_Env_Spec_Decl
   --

   

      
      procedure Initialize_Fields_For_Env_Spec_Decl
        (Self : Bare_Env_Spec_Decl
         ; Env_Spec_Decl_F_Syn_Name : Bare_Def_Id
         ; Env_Spec_Decl_F_Actions : Bare_Call_Expr_List
        ) is
      begin

            Self.Env_Spec_Decl_F_Syn_Name := Env_Spec_Decl_F_Syn_Name;
            Self.Env_Spec_Decl_F_Actions := Env_Spec_Decl_F_Actions;
         

      end Initialize_Fields_For_Env_Spec_Decl;

      
   function Env_Spec_Decl_F_Actions
     (Node : Bare_Env_Spec_Decl) return Bare_Call_Expr_List
   is
      

   begin
         
         return Node.Env_Spec_Decl_F_Actions;
      
   end;



   


      

   --
   --  Primitives for Bare_Error_Decl
   --

   




   


      

   --
   --  Primitives for Bare_Generic_Decl
   --

   

      
      procedure Initialize_Fields_For_Generic_Decl
        (Self : Bare_Generic_Decl
         ; Generic_Decl_F_Generic_Param_Decls : Bare_Generic_Param_Decl_List
         ; Generic_Decl_F_Decl : Bare_Decl
        ) is
      begin

            Self.Generic_Decl_F_Generic_Param_Decls := Generic_Decl_F_Generic_Param_Decls;
            Self.Generic_Decl_F_Decl := Generic_Decl_F_Decl;
         

      end Initialize_Fields_For_Generic_Decl;

      
   function Generic_Decl_F_Generic_Param_Decls
     (Node : Bare_Generic_Decl) return Bare_Generic_Param_Decl_List
   is
      

   begin
         
         return Node.Generic_Decl_F_Generic_Param_Decls;
      
   end;

      
   function Generic_Decl_F_Decl
     (Node : Bare_Generic_Decl) return Bare_Decl
   is
      

   begin
         
         return Node.Generic_Decl_F_Decl;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Decl
   --

   

      
      procedure Initialize_Fields_For_Grammar_Decl
        (Self : Bare_Grammar_Decl
         ; Grammar_Decl_F_Syn_Name : Bare_Def_Id
         ; Grammar_Decl_F_Rules : Bare_Full_Decl_List
        ) is
      begin

            Self.Grammar_Decl_F_Syn_Name := Grammar_Decl_F_Syn_Name;
            Self.Grammar_Decl_F_Rules := Grammar_Decl_F_Rules;
         

      end Initialize_Fields_For_Grammar_Decl;

      
   function Grammar_Decl_F_Rules
     (Node : Bare_Grammar_Decl) return Bare_Full_Decl_List
   is
      

   begin
         
         return Node.Grammar_Decl_F_Rules;
      
   end;



   


      

   --
   --  Primitives for Bare_Langkit_Root
   --

   

      
      procedure Initialize_Fields_For_Langkit_Root
        (Self : Bare_Langkit_Root
         ; Langkit_Root_F_Doc : Bare_Module_Doc_String_Lit
         ; Langkit_Root_F_Imports : Bare_Base_Import_List
         ; Langkit_Root_F_Decls : Bare_Full_Decl_List
        ) is
      begin

            Self.Langkit_Root_F_Doc := Langkit_Root_F_Doc;
            Self.Langkit_Root_F_Imports := Langkit_Root_F_Imports;
            Self.Langkit_Root_F_Decls := Langkit_Root_F_Decls;
         
      Self.Internal_Bare_Langkit_Root_Lf_State_Empty_Type_Ref_List_27 := Uninitialized;
      Self.Internal_Bare_Langkit_Root_Lf_Stg_Empty_Type_Ref_List_28 := No_Bare_Lkt_Node;

      end Initialize_Fields_For_Langkit_Root;

      
   function Langkit_Root_F_Doc
     (Node : Bare_Langkit_Root) return Bare_Module_Doc_String_Lit
   is
      

   begin
         
         return Node.Langkit_Root_F_Doc;
      
   end;

      
   function Langkit_Root_F_Imports
     (Node : Bare_Langkit_Root) return Bare_Base_Import_List
   is
      

   begin
         
         return Node.Langkit_Root_F_Imports;
      
   end;

      
   function Langkit_Root_F_Decls
     (Node : Bare_Langkit_Root) return Bare_Full_Decl_List
   is
      

   begin
         
         return Node.Langkit_Root_F_Decls;
      
   end;



   


      

   --
   --  Primitives for Bare_Lexer_Decl
   --

   

      
      procedure Initialize_Fields_For_Lexer_Decl
        (Self : Bare_Lexer_Decl
         ; Lexer_Decl_F_Syn_Name : Bare_Def_Id
         ; Lexer_Decl_F_Rules : Bare_Lkt_Node_List
        ) is
      begin

            Self.Lexer_Decl_F_Syn_Name := Lexer_Decl_F_Syn_Name;
            Self.Lexer_Decl_F_Rules := Lexer_Decl_F_Rules;
         

      end Initialize_Fields_For_Lexer_Decl;

      
   function Lexer_Decl_F_Rules
     (Node : Bare_Lexer_Decl) return Bare_Lkt_Node_List
   is
      

   begin
         
         return Node.Lexer_Decl_F_Rules;
      
   end;



   


      

   --
   --  Primitives for Bare_Lexer_Family_Decl
   --

   

      
      procedure Initialize_Fields_For_Lexer_Family_Decl
        (Self : Bare_Lexer_Family_Decl
         ; Lexer_Family_Decl_F_Syn_Name : Bare_Def_Id
         ; Lexer_Family_Decl_F_Rules : Bare_Full_Decl_List
        ) is
      begin

            Self.Lexer_Family_Decl_F_Syn_Name := Lexer_Family_Decl_F_Syn_Name;
            Self.Lexer_Family_Decl_F_Rules := Lexer_Family_Decl_F_Rules;
         

      end Initialize_Fields_For_Lexer_Family_Decl;

      
   function Lexer_Family_Decl_F_Rules
     (Node : Bare_Lexer_Family_Decl) return Bare_Full_Decl_List
   is
      

   begin
         
         return Node.Lexer_Family_Decl_F_Rules;
      
   end;



   


      

   --
   --  Primitives for Bare_Synth_Fun_Decl
   --

   

      
      procedure Initialize_Fields_For_Synth_Fun_Decl
        (Self : Bare_Synth_Fun_Decl
        ) is
      begin

         
      Self.Synth_Fun_Decl_F_Params := No_Internal_Resolved_Param_Array_Type;
      Self.Synth_Fun_Decl_F_Return_Type := No_Entity_Type_Decl;

      end Initialize_Fields_For_Synth_Fun_Decl;



   


      

   --
   --  Primitives for Bare_Synth_Param_Decl
   --

   




   


      

   --
   --  Primitives for Bare_Type_Decl
   --

   


      
   function Type_Decl_F_Traits
     (Node : Bare_Type_Decl) return Bare_Type_Ref_List
   is
      

         Kind : constant Lkt_Type_Decl := Node.Kind;
   begin
         case Kind is
               when Lkt_Any_Type_Decl =>
                     
         return Node.Any_Type_Decl_F_Traits;
      
               when Lkt_Enum_Class_Alt_Decl =>
                     
         return Node.Enum_Class_Alt_Decl_F_Traits;
      
               when Lkt_Function_Type =>
                     
         return Node.Function_Type_F_Traits;
      
               when Lkt_Generic_Param_Type_Decl =>
                     
         return Node.Generic_Param_Type_Decl_F_Traits;
      
               when Lkt_Class_Decl | Lkt_Enum_Class_Decl =>
                     
         return Node.Basic_Class_Decl_F_Traits;
      
               when Lkt_Enum_Type_Decl =>
                     
         return Node.Enum_Type_Decl_F_Traits;
      
               when Lkt_Struct_Decl =>
                     
         return Node.Struct_Decl_F_Traits;
      
               when Lkt_Trait_Decl =>
                     
         return Node.Trait_Decl_F_Traits;
      
         end case;
   end;

      
   function Type_Decl_F_Syn_Base_Type
     (Node : Bare_Type_Decl) return Bare_Type_Ref
   is
      

         Kind : constant Lkt_Type_Decl := Node.Kind;
   begin
         case Kind is
               when Lkt_Any_Type_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Enum_Class_Alt_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Function_Type =>
                     return No_Bare_Lkt_Node;
               when Lkt_Generic_Param_Type_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Class_Decl | Lkt_Enum_Class_Decl =>
                     
         return Node.Basic_Class_Decl_F_Syn_Base_Type;
      
               when Lkt_Enum_Type_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Struct_Decl =>
                     return No_Bare_Lkt_Node;
               when Lkt_Trait_Decl =>
                     return No_Bare_Lkt_Node;
         end case;
   end;



   


      

   --
   --  Primitives for Bare_Any_Type_Decl
   --

   

      
      procedure Initialize_Fields_For_Any_Type_Decl
        (Self : Bare_Any_Type_Decl
         ; Any_Type_Decl_F_Traits : Bare_Synthetic_Type_Ref_List
        ) is
      begin

            Self.Any_Type_Decl_F_Traits := Any_Type_Decl_F_Traits;
         

      end Initialize_Fields_For_Any_Type_Decl;



   


      

   --
   --  Primitives for Bare_Enum_Class_Alt_Decl
   --

   

      
      procedure Initialize_Fields_For_Enum_Class_Alt_Decl
        (Self : Bare_Enum_Class_Alt_Decl
         ; Enum_Class_Alt_Decl_F_Syn_Name : Bare_Def_Id
         ; Enum_Class_Alt_Decl_F_Traits : Bare_Type_Ref_List
        ) is
      begin

            Self.Enum_Class_Alt_Decl_F_Syn_Name := Enum_Class_Alt_Decl_F_Syn_Name;
            Self.Enum_Class_Alt_Decl_F_Traits := Enum_Class_Alt_Decl_F_Traits;
         

      end Initialize_Fields_For_Enum_Class_Alt_Decl;



   


      

   --
   --  Primitives for Bare_Function_Type
   --

   

      
      procedure Initialize_Fields_For_Function_Type
        (Self : Bare_Function_Type
         ; Function_Type_F_Traits : Bare_Synthetic_Type_Ref_List
        ) is
      begin

            Self.Function_Type_F_Traits := Function_Type_F_Traits;
         
      Self.Function_Type_F_Params := No_Internal_Entity_Type_Decl_Array_Type;
      Self.Function_Type_F_Return_Type := No_Entity_Type_Decl;
      Self.Function_Type_F_Origin := No_Entity_Decl;

      end Initialize_Fields_For_Function_Type;



   


      

   --
   --  Primitives for Bare_Generic_Param_Type_Decl
   --

   

      
      procedure Initialize_Fields_For_Generic_Param_Type_Decl
        (Self : Bare_Generic_Param_Type_Decl
         ; Generic_Param_Type_Decl_F_Has_Class : Bare_Class_Qualifier
         ; Generic_Param_Type_Decl_F_Syn_Name : Bare_Def_Id
         ; Generic_Param_Type_Decl_F_Traits : Bare_Type_Ref_List
        ) is
      begin

            Self.Generic_Param_Type_Decl_F_Has_Class := Generic_Param_Type_Decl_F_Has_Class;
            Self.Generic_Param_Type_Decl_F_Syn_Name := Generic_Param_Type_Decl_F_Syn_Name;
            Self.Generic_Param_Type_Decl_F_Traits := Generic_Param_Type_Decl_F_Traits;
         

      end Initialize_Fields_For_Generic_Param_Type_Decl;

      
   function Generic_Param_Type_Decl_F_Has_Class
     (Node : Bare_Generic_Param_Type_Decl) return Bare_Class_Qualifier
   is
      

   begin
         
         return Node.Generic_Param_Type_Decl_F_Has_Class;
      
   end;



   


      

   --
   --  Primitives for Bare_Named_Type_Decl
   --

   


      
   function Named_Type_Decl_F_Decls
     (Node : Bare_Named_Type_Decl) return Bare_Decl_Block
   is
      

         Kind : constant Lkt_Named_Type_Decl := Node.Kind;
   begin
         case Kind is
               when Lkt_Class_Decl =>
                     
         return Node.Class_Decl_F_Decls;
      
               when Lkt_Enum_Class_Decl =>
                     
         return Node.Enum_Class_Decl_F_Decls;
      
               when Lkt_Enum_Type_Decl =>
                     
         return Node.Enum_Type_Decl_F_Decls;
      
               when Lkt_Struct_Decl =>
                     
         return Node.Struct_Decl_F_Decls;
      
               when Lkt_Trait_Decl =>
                     
         return Node.Trait_Decl_F_Decls;
      
         end case;
   end;



   


      

   --
   --  Primitives for Bare_Basic_Class_Decl
   --

   

      
      procedure Initialize_Fields_For_Basic_Class_Decl
        (Self : Bare_Basic_Class_Decl
         ; Basic_Class_Decl_F_Syn_Name : Bare_Def_Id
         ; Basic_Class_Decl_F_Syn_Base_Type : Bare_Type_Ref
         ; Basic_Class_Decl_F_Traits : Bare_Type_Ref_List
        ) is
      begin

            Self.Basic_Class_Decl_F_Syn_Name := Basic_Class_Decl_F_Syn_Name;
            Self.Basic_Class_Decl_F_Syn_Base_Type := Basic_Class_Decl_F_Syn_Base_Type;
            Self.Basic_Class_Decl_F_Traits := Basic_Class_Decl_F_Traits;
         

      end Initialize_Fields_For_Basic_Class_Decl;



   


      

   --
   --  Primitives for Bare_Class_Decl
   --

   

      
      procedure Initialize_Fields_For_Class_Decl
        (Self : Bare_Class_Decl
         ; Basic_Class_Decl_F_Syn_Name : Bare_Def_Id
         ; Basic_Class_Decl_F_Syn_Base_Type : Bare_Type_Ref
         ; Basic_Class_Decl_F_Traits : Bare_Type_Ref_List
         ; Class_Decl_F_Decls : Bare_Decl_Block
        ) is
      begin
            Initialize_Fields_For_Basic_Class_Decl
              (Self, Basic_Class_Decl_F_Syn_Name, Basic_Class_Decl_F_Syn_Base_Type, Basic_Class_Decl_F_Traits);

            Self.Class_Decl_F_Decls := Class_Decl_F_Decls;
         

      end Initialize_Fields_For_Class_Decl;



   


      

   --
   --  Primitives for Bare_Enum_Class_Decl
   --

   

      
      procedure Initialize_Fields_For_Enum_Class_Decl
        (Self : Bare_Enum_Class_Decl
         ; Basic_Class_Decl_F_Syn_Name : Bare_Def_Id
         ; Basic_Class_Decl_F_Syn_Base_Type : Bare_Type_Ref
         ; Basic_Class_Decl_F_Traits : Bare_Type_Ref_List
         ; Enum_Class_Decl_F_Branches : Bare_Enum_Class_Case_List
         ; Enum_Class_Decl_F_Decls : Bare_Decl_Block
        ) is
      begin
            Initialize_Fields_For_Basic_Class_Decl
              (Self, Basic_Class_Decl_F_Syn_Name, Basic_Class_Decl_F_Syn_Base_Type, Basic_Class_Decl_F_Traits);

            Self.Enum_Class_Decl_F_Branches := Enum_Class_Decl_F_Branches;
            Self.Enum_Class_Decl_F_Decls := Enum_Class_Decl_F_Decls;
         

      end Initialize_Fields_For_Enum_Class_Decl;

      
   function Enum_Class_Decl_F_Branches
     (Node : Bare_Enum_Class_Decl) return Bare_Enum_Class_Case_List
   is
      

   begin
         
         return Node.Enum_Class_Decl_F_Branches;
      
   end;



   


      

   --
   --  Primitives for Bare_Enum_Type_Decl
   --

   

      
      procedure Initialize_Fields_For_Enum_Type_Decl
        (Self : Bare_Enum_Type_Decl
         ; Enum_Type_Decl_F_Syn_Name : Bare_Def_Id
         ; Enum_Type_Decl_F_Traits : Bare_Type_Ref_List
         ; Enum_Type_Decl_F_Literals : Bare_Enum_Lit_Decl_List
         ; Enum_Type_Decl_F_Decls : Bare_Decl_Block
        ) is
      begin

            Self.Enum_Type_Decl_F_Syn_Name := Enum_Type_Decl_F_Syn_Name;
            Self.Enum_Type_Decl_F_Traits := Enum_Type_Decl_F_Traits;
            Self.Enum_Type_Decl_F_Literals := Enum_Type_Decl_F_Literals;
            Self.Enum_Type_Decl_F_Decls := Enum_Type_Decl_F_Decls;
         

      end Initialize_Fields_For_Enum_Type_Decl;

      
   function Enum_Type_Decl_F_Literals
     (Node : Bare_Enum_Type_Decl) return Bare_Enum_Lit_Decl_List
   is
      

   begin
         
         return Node.Enum_Type_Decl_F_Literals;
      
   end;



   


      

   --
   --  Primitives for Bare_Struct_Decl
   --

   

      
      procedure Initialize_Fields_For_Struct_Decl
        (Self : Bare_Struct_Decl
         ; Struct_Decl_F_Syn_Name : Bare_Def_Id
         ; Struct_Decl_F_Traits : Bare_Type_Ref_List
         ; Struct_Decl_F_Decls : Bare_Decl_Block
        ) is
      begin

            Self.Struct_Decl_F_Syn_Name := Struct_Decl_F_Syn_Name;
            Self.Struct_Decl_F_Traits := Struct_Decl_F_Traits;
            Self.Struct_Decl_F_Decls := Struct_Decl_F_Decls;
         

      end Initialize_Fields_For_Struct_Decl;



   


      

   --
   --  Primitives for Bare_Trait_Decl
   --

   

      
      procedure Initialize_Fields_For_Trait_Decl
        (Self : Bare_Trait_Decl
         ; Trait_Decl_F_Syn_Name : Bare_Def_Id
         ; Trait_Decl_F_Traits : Bare_Type_Ref_List
         ; Trait_Decl_F_Decls : Bare_Decl_Block
        ) is
      begin

            Self.Trait_Decl_F_Syn_Name := Trait_Decl_F_Syn_Name;
            Self.Trait_Decl_F_Traits := Trait_Decl_F_Traits;
            Self.Trait_Decl_F_Decls := Trait_Decl_F_Decls;
         

      end Initialize_Fields_For_Trait_Decl;



   


      

   --
   --  Primitives for Bare_Decl_Annotation
   --

   

      
      procedure Initialize_Fields_For_Decl_Annotation
        (Self : Bare_Decl_Annotation
         ; Decl_Annotation_F_Name : Bare_Id
         ; Decl_Annotation_F_Args : Bare_Decl_Annotation_Args
        ) is
      begin

            Self.Decl_Annotation_F_Name := Decl_Annotation_F_Name;
            Self.Decl_Annotation_F_Args := Decl_Annotation_F_Args;
         

      end Initialize_Fields_For_Decl_Annotation;

      
   function Decl_Annotation_F_Name
     (Node : Bare_Decl_Annotation) return Bare_Id
   is
      

   begin
         
         return Node.Decl_Annotation_F_Name;
      
   end;

      
   function Decl_Annotation_F_Args
     (Node : Bare_Decl_Annotation) return Bare_Decl_Annotation_Args
   is
      

   begin
         
         return Node.Decl_Annotation_F_Args;
      
   end;



   


      

   --
   --  Primitives for Bare_Decl_Annotation_Args
   --

   

      
      procedure Initialize_Fields_For_Decl_Annotation_Args
        (Self : Bare_Decl_Annotation_Args
         ; Decl_Annotation_Args_F_Args : Bare_Argument_List
        ) is
      begin

            Self.Decl_Annotation_Args_F_Args := Decl_Annotation_Args_F_Args;
         

      end Initialize_Fields_For_Decl_Annotation_Args;

      
   function Decl_Annotation_Args_F_Args
     (Node : Bare_Decl_Annotation_Args) return Bare_Argument_List
   is
      

   begin
         
         return Node.Decl_Annotation_Args_F_Args;
      
   end;



   


      

   --
   --  Primitives for Bare_Dyn_Env_Wrapper
   --

   

      
      procedure Initialize_Fields_For_Dyn_Env_Wrapper
        (Self : Bare_Dyn_Env_Wrapper
        ) is
      begin

         
      Self.Dyn_Env_Wrapper_F_Names := No_Symbol_Type_Array_Type;
      Self.Dyn_Env_Wrapper_F_Types := No_Internal_Entity_Type_Decl_Array_Type;
      Self.Internal_Bare_Dyn_Env_Wrapper_Lf_State_Dynenvwrapper_Instantiation_Env_29 := Uninitialized;
      Self.Internal_Bare_Dyn_Env_Wrapper_Lf_Stg_Dynenvwrapper_Instantiation_Env_30 := Empty_Env;

      end Initialize_Fields_For_Dyn_Env_Wrapper;



   


      

   --
   --  Primitives for Bare_Elsif_Branch
   --

   

      
      procedure Initialize_Fields_For_Elsif_Branch
        (Self : Bare_Elsif_Branch
         ; Elsif_Branch_F_Cond_Expr : Bare_Expr
         ; Elsif_Branch_F_Then_Expr : Bare_Expr
        ) is
      begin

            Self.Elsif_Branch_F_Cond_Expr := Elsif_Branch_F_Cond_Expr;
            Self.Elsif_Branch_F_Then_Expr := Elsif_Branch_F_Then_Expr;
         

      end Initialize_Fields_For_Elsif_Branch;

      
   function Elsif_Branch_F_Cond_Expr
     (Node : Bare_Elsif_Branch) return Bare_Expr
   is
      

   begin
         
         return Node.Elsif_Branch_F_Cond_Expr;
      
   end;

      
   function Elsif_Branch_F_Then_Expr
     (Node : Bare_Elsif_Branch) return Bare_Expr
   is
      

   begin
         
         return Node.Elsif_Branch_F_Then_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Enum_Class_Case
   --

   

      
      procedure Initialize_Fields_For_Enum_Class_Case
        (Self : Bare_Enum_Class_Case
         ; Enum_Class_Case_F_Decls : Bare_Enum_Class_Alt_Decl_List
        ) is
      begin

            Self.Enum_Class_Case_F_Decls := Enum_Class_Case_F_Decls;
         

      end Initialize_Fields_For_Enum_Class_Case;

      
   function Enum_Class_Case_F_Decls
     (Node : Bare_Enum_Class_Case) return Bare_Enum_Class_Alt_Decl_List
   is
      

   begin
         
         return Node.Enum_Class_Case_F_Decls;
      
   end;



   


      

   --
   --  Primitives for Bare_Excludes_Null
   --

   




   


      

   --
   --  Primitives for Bare_Excludes_Null_Absent
   --

   




   


      

   --
   --  Primitives for Bare_Excludes_Null_Present
   --

   




   


      

   --
   --  Primitives for Bare_Expr
   --

   

      
      procedure Initialize_Fields_For_Expr
        (Self : Bare_Expr
        ) is
      begin

         
      Self.Expr_F_Expected_Type_Var := Null_Var_Record;
      Self.Expr_F_Actual_Type_Var := Null_Var_Record;
      Self.Expr_F_Generic_Func_Type_Var := Null_Var_Record;

      end Initialize_Fields_For_Expr;



   


      

   --
   --  Primitives for Bare_Any_Of
   --

   

      
      procedure Initialize_Fields_For_Any_Of
        (Self : Bare_Any_Of
         ; Any_Of_F_Expr : Bare_Expr
         ; Any_Of_F_Values : Bare_Any_Of_List
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Any_Of_F_Expr := Any_Of_F_Expr;
            Self.Any_Of_F_Values := Any_Of_F_Values;
         

      end Initialize_Fields_For_Any_Of;

      
   function Any_Of_F_Expr
     (Node : Bare_Any_Of) return Bare_Expr
   is
      

   begin
         
         return Node.Any_Of_F_Expr;
      
   end;

      
   function Any_Of_F_Values
     (Node : Bare_Any_Of) return Bare_Any_Of_List
   is
      

   begin
         
         return Node.Any_Of_F_Values;
      
   end;



   


      

   --
   --  Primitives for Bare_Array_Literal
   --

   

      
      procedure Initialize_Fields_For_Array_Literal
        (Self : Bare_Array_Literal
         ; Array_Literal_F_Exprs : Bare_Expr_List
         ; Array_Literal_F_Element_Type : Bare_Type_Ref
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Array_Literal_F_Exprs := Array_Literal_F_Exprs;
            Self.Array_Literal_F_Element_Type := Array_Literal_F_Element_Type;
         
      Self.Array_Literal_F_Expected_Exprs_Type_Var := Null_Var_Record;
      Self.Array_Literal_F_Actual_Element_Type := Null_Var_Record;

      end Initialize_Fields_For_Array_Literal;

      
   function Array_Literal_F_Exprs
     (Node : Bare_Array_Literal) return Bare_Expr_List
   is
      

   begin
         
         return Node.Array_Literal_F_Exprs;
      
   end;

      
   function Array_Literal_F_Element_Type
     (Node : Bare_Array_Literal) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Array_Literal_F_Element_Type;
      
   end;



   


      

   --
   --  Primitives for Bare_Base_Call_Expr
   --

   

      
      procedure Initialize_Fields_For_Base_Call_Expr
        (Self : Bare_Base_Call_Expr
         ; Base_Call_Expr_F_Name : Bare_Expr
         ; Base_Call_Expr_F_Args : Bare_Argument_List
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Base_Call_Expr_F_Name := Base_Call_Expr_F_Name;
            Self.Base_Call_Expr_F_Args := Base_Call_Expr_F_Args;
         

      end Initialize_Fields_For_Base_Call_Expr;

      
   function Base_Call_Expr_F_Name
     (Node : Bare_Base_Call_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Base_Call_Expr_F_Name;
      
   end;

      
   function Base_Call_Expr_F_Args
     (Node : Bare_Base_Call_Expr) return Bare_Argument_List
   is
      

   begin
         
         return Node.Base_Call_Expr_F_Args;
      
   end;



   


      

   --
   --  Primitives for Bare_Call_Expr
   --

   

      
      procedure Initialize_Fields_For_Call_Expr
        (Self : Bare_Call_Expr
         ; Base_Call_Expr_F_Name : Bare_Expr
         ; Base_Call_Expr_F_Args : Bare_Argument_List
        ) is
      begin
            Initialize_Fields_For_Base_Call_Expr
              (Self, Base_Call_Expr_F_Name, Base_Call_Expr_F_Args);

         

      end Initialize_Fields_For_Call_Expr;



   


      

   --
   --  Primitives for Bare_Logic_Call_Expr
   --

   

      
      procedure Initialize_Fields_For_Logic_Call_Expr
        (Self : Bare_Logic_Call_Expr
         ; Base_Call_Expr_F_Name : Bare_Expr
         ; Base_Call_Expr_F_Args : Bare_Argument_List
        ) is
      begin
            Initialize_Fields_For_Base_Call_Expr
              (Self, Base_Call_Expr_F_Name, Base_Call_Expr_F_Args);

         

      end Initialize_Fields_For_Logic_Call_Expr;



   


      

   --
   --  Primitives for Bare_Logic_Predicate
   --

   

      
      procedure Initialize_Fields_For_Logic_Predicate
        (Self : Bare_Logic_Predicate
         ; Base_Call_Expr_F_Name : Bare_Expr
         ; Base_Call_Expr_F_Args : Bare_Argument_List
        ) is
      begin
            Initialize_Fields_For_Logic_Call_Expr
              (Self, Base_Call_Expr_F_Name, Base_Call_Expr_F_Args);

         

      end Initialize_Fields_For_Logic_Predicate;



   


      

   --
   --  Primitives for Bare_Logic_Propagate_Call
   --

   

      
      procedure Initialize_Fields_For_Logic_Propagate_Call
        (Self : Bare_Logic_Propagate_Call
         ; Base_Call_Expr_F_Name : Bare_Expr
         ; Base_Call_Expr_F_Args : Bare_Argument_List
        ) is
      begin
            Initialize_Fields_For_Logic_Call_Expr
              (Self, Base_Call_Expr_F_Name, Base_Call_Expr_F_Args);

         

      end Initialize_Fields_For_Logic_Propagate_Call;



   


      

   --
   --  Primitives for Bare_Bin_Op
   --

   

      
      procedure Initialize_Fields_For_Bin_Op
        (Self : Bare_Bin_Op
         ; Bin_Op_F_Left : Bare_Expr
         ; Bin_Op_F_Op : Bare_Op
         ; Bin_Op_F_Right : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Bin_Op_F_Left := Bin_Op_F_Left;
            Self.Bin_Op_F_Op := Bin_Op_F_Op;
            Self.Bin_Op_F_Right := Bin_Op_F_Right;
         

      end Initialize_Fields_For_Bin_Op;

      
   function Bin_Op_F_Left
     (Node : Bare_Bin_Op) return Bare_Expr
   is
      

   begin
         
         return Node.Bin_Op_F_Left;
      
   end;

      
   function Bin_Op_F_Op
     (Node : Bare_Bin_Op) return Bare_Op
   is
      

   begin
         
         return Node.Bin_Op_F_Op;
      
   end;

      
   function Bin_Op_F_Right
     (Node : Bare_Bin_Op) return Bare_Expr
   is
      

   begin
         
         return Node.Bin_Op_F_Right;
      
   end;



   


      

   --
   --  Primitives for Bare_Block_Expr
   --

   

      
      procedure Initialize_Fields_For_Block_Expr
        (Self : Bare_Block_Expr
         ; Block_Expr_F_Clauses : Bare_Lkt_Node_List
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Block_Expr_F_Clauses := Block_Expr_F_Clauses;
         

      end Initialize_Fields_For_Block_Expr;

      
   function Block_Expr_F_Clauses
     (Node : Bare_Block_Expr) return Bare_Lkt_Node_List
   is
      

   begin
         
         return Node.Block_Expr_F_Clauses;
      
   end;



   


      

   --
   --  Primitives for Bare_Cast_Expr
   --

   

      
      procedure Initialize_Fields_For_Cast_Expr
        (Self : Bare_Cast_Expr
         ; Cast_Expr_F_Expr : Bare_Expr
         ; Cast_Expr_F_Null_Cond : Bare_Null_Cond_Qualifier
         ; Cast_Expr_F_Excludes_Null : Bare_Excludes_Null
         ; Cast_Expr_F_Dest_Type : Bare_Type_Ref
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Cast_Expr_F_Expr := Cast_Expr_F_Expr;
            Self.Cast_Expr_F_Null_Cond := Cast_Expr_F_Null_Cond;
            Self.Cast_Expr_F_Excludes_Null := Cast_Expr_F_Excludes_Null;
            Self.Cast_Expr_F_Dest_Type := Cast_Expr_F_Dest_Type;
         

      end Initialize_Fields_For_Cast_Expr;

      
   function Cast_Expr_F_Expr
     (Node : Bare_Cast_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Cast_Expr_F_Expr;
      
   end;

      
   function Cast_Expr_F_Null_Cond
     (Node : Bare_Cast_Expr) return Bare_Null_Cond_Qualifier
   is
      

   begin
         
         return Node.Cast_Expr_F_Null_Cond;
      
   end;

      
   function Cast_Expr_F_Excludes_Null
     (Node : Bare_Cast_Expr) return Bare_Excludes_Null
   is
      

   begin
         
         return Node.Cast_Expr_F_Excludes_Null;
      
   end;

      
   function Cast_Expr_F_Dest_Type
     (Node : Bare_Cast_Expr) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Cast_Expr_F_Dest_Type;
      
   end;



   


      

   --
   --  Primitives for Bare_Dot_Expr
   --

   

      
      procedure Initialize_Fields_For_Dot_Expr
        (Self : Bare_Dot_Expr
         ; Dot_Expr_F_Prefix : Bare_Expr
         ; Dot_Expr_F_Null_Cond : Bare_Null_Cond_Qualifier
         ; Dot_Expr_F_Suffix : Bare_Ref_Id
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Dot_Expr_F_Prefix := Dot_Expr_F_Prefix;
            Self.Dot_Expr_F_Null_Cond := Dot_Expr_F_Null_Cond;
            Self.Dot_Expr_F_Suffix := Dot_Expr_F_Suffix;
         

      end Initialize_Fields_For_Dot_Expr;

      
   function Dot_Expr_F_Prefix
     (Node : Bare_Dot_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Dot_Expr_F_Prefix;
      
   end;

      
   function Dot_Expr_F_Null_Cond
     (Node : Bare_Dot_Expr) return Bare_Null_Cond_Qualifier
   is
      

   begin
         
         return Node.Dot_Expr_F_Null_Cond;
      
   end;

      
   function Dot_Expr_F_Suffix
     (Node : Bare_Dot_Expr) return Bare_Ref_Id
   is
      

   begin
         
         return Node.Dot_Expr_F_Suffix;
      
   end;



   


      

   --
   --  Primitives for Bare_Error_On_Null
   --

   

      
      procedure Initialize_Fields_For_Error_On_Null
        (Self : Bare_Error_On_Null
         ; Error_On_Null_F_Expr : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Error_On_Null_F_Expr := Error_On_Null_F_Expr;
         

      end Initialize_Fields_For_Error_On_Null;

      
   function Error_On_Null_F_Expr
     (Node : Bare_Error_On_Null) return Bare_Expr
   is
      

   begin
         
         return Node.Error_On_Null_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Generic_Instantiation
   --

   

      
      procedure Initialize_Fields_For_Generic_Instantiation
        (Self : Bare_Generic_Instantiation
         ; Generic_Instantiation_F_Name : Bare_Expr
         ; Generic_Instantiation_F_Args : Bare_Type_Ref_List
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Generic_Instantiation_F_Name := Generic_Instantiation_F_Name;
            Self.Generic_Instantiation_F_Args := Generic_Instantiation_F_Args;
         
      Self.Generic_Instantiation_F_Rebinded_Var := Null_Var_Record;

      end Initialize_Fields_For_Generic_Instantiation;

      
   function Generic_Instantiation_F_Name
     (Node : Bare_Generic_Instantiation) return Bare_Expr
   is
      

   begin
         
         return Node.Generic_Instantiation_F_Name;
      
   end;

      
   function Generic_Instantiation_F_Args
     (Node : Bare_Generic_Instantiation) return Bare_Type_Ref_List
   is
      

   begin
         
         return Node.Generic_Instantiation_F_Args;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Expr
   --

   

      
      procedure Initialize_Fields_For_Grammar_Expr
        (Self : Bare_Grammar_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

         

      end Initialize_Fields_For_Grammar_Expr;



   


      

   --
   --  Primitives for Bare_Error_Grammar_Expr
   --

   

      
      procedure Initialize_Fields_For_Error_Grammar_Expr
        (Self : Bare_Error_Grammar_Expr
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

         

      end Initialize_Fields_For_Error_Grammar_Expr;



   


      

   --
   --  Primitives for Bare_Grammar_Cut
   --

   

      
      procedure Initialize_Fields_For_Grammar_Cut
        (Self : Bare_Grammar_Cut
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

         

      end Initialize_Fields_For_Grammar_Cut;



   


      

   --
   --  Primitives for Bare_Grammar_Discard
   --

   

      
      procedure Initialize_Fields_For_Grammar_Discard
        (Self : Bare_Grammar_Discard
         ; Grammar_Discard_F_Expr : Bare_Grammar_Expr
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Discard_F_Expr := Grammar_Discard_F_Expr;
         

      end Initialize_Fields_For_Grammar_Discard;

      
   function Grammar_Discard_F_Expr
     (Node : Bare_Grammar_Discard) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Grammar_Discard_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Dont_Skip
   --

   

      
      procedure Initialize_Fields_For_Grammar_Dont_Skip
        (Self : Bare_Grammar_Dont_Skip
         ; Grammar_Dont_Skip_F_Expr : Bare_Grammar_Expr
         ; Grammar_Dont_Skip_F_Dont_Skip : Bare_Grammar_Expr
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Dont_Skip_F_Expr := Grammar_Dont_Skip_F_Expr;
            Self.Grammar_Dont_Skip_F_Dont_Skip := Grammar_Dont_Skip_F_Dont_Skip;
         

      end Initialize_Fields_For_Grammar_Dont_Skip;

      
   function Grammar_Dont_Skip_F_Expr
     (Node : Bare_Grammar_Dont_Skip) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Grammar_Dont_Skip_F_Expr;
      
   end;

      
   function Grammar_Dont_Skip_F_Dont_Skip
     (Node : Bare_Grammar_Dont_Skip) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Grammar_Dont_Skip_F_Dont_Skip;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_List
   --

   

      
      procedure Initialize_Fields_For_Grammar_List
        (Self : Bare_Grammar_List
         ; Grammar_List_F_List_Type : Bare_Type_Ref
         ; Grammar_List_F_Kind : Bare_List_Kind
         ; Grammar_List_F_Expr : Bare_Grammar_Expr
         ; Grammar_List_F_Sep : Bare_Grammar_List_Sep
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_List_F_List_Type := Grammar_List_F_List_Type;
            Self.Grammar_List_F_Kind := Grammar_List_F_Kind;
            Self.Grammar_List_F_Expr := Grammar_List_F_Expr;
            Self.Grammar_List_F_Sep := Grammar_List_F_Sep;
         

      end Initialize_Fields_For_Grammar_List;

      
   function Grammar_List_F_List_Type
     (Node : Bare_Grammar_List) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Grammar_List_F_List_Type;
      
   end;

      
   function Grammar_List_F_Kind
     (Node : Bare_Grammar_List) return Bare_List_Kind
   is
      

   begin
         
         return Node.Grammar_List_F_Kind;
      
   end;

      
   function Grammar_List_F_Expr
     (Node : Bare_Grammar_List) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Grammar_List_F_Expr;
      
   end;

      
   function Grammar_List_F_Sep
     (Node : Bare_Grammar_List) return Bare_Grammar_List_Sep
   is
      

   begin
         
         return Node.Grammar_List_F_Sep;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Null
   --

   

      
      procedure Initialize_Fields_For_Grammar_Null
        (Self : Bare_Grammar_Null
         ; Grammar_Null_F_Name : Bare_Type_Ref
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Null_F_Name := Grammar_Null_F_Name;
         

      end Initialize_Fields_For_Grammar_Null;

      
   function Grammar_Null_F_Name
     (Node : Bare_Grammar_Null) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Grammar_Null_F_Name;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Opt
   --

   

      
      procedure Initialize_Fields_For_Grammar_Opt
        (Self : Bare_Grammar_Opt
         ; Grammar_Opt_F_Expr : Bare_Grammar_Expr
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Opt_F_Expr := Grammar_Opt_F_Expr;
         

      end Initialize_Fields_For_Grammar_Opt;

      
   function Grammar_Opt_F_Expr
     (Node : Bare_Grammar_Opt) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Grammar_Opt_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Opt_Error
   --

   

      
      procedure Initialize_Fields_For_Grammar_Opt_Error
        (Self : Bare_Grammar_Opt_Error
         ; Grammar_Opt_Error_F_Expr : Bare_Grammar_Expr
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Opt_Error_F_Expr := Grammar_Opt_Error_F_Expr;
         

      end Initialize_Fields_For_Grammar_Opt_Error;

      
   function Grammar_Opt_Error_F_Expr
     (Node : Bare_Grammar_Opt_Error) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Grammar_Opt_Error_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Opt_Error_Group
   --

   

      
      procedure Initialize_Fields_For_Grammar_Opt_Error_Group
        (Self : Bare_Grammar_Opt_Error_Group
         ; Grammar_Opt_Error_Group_F_Expr : Bare_Grammar_Expr_List
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Opt_Error_Group_F_Expr := Grammar_Opt_Error_Group_F_Expr;
         

      end Initialize_Fields_For_Grammar_Opt_Error_Group;

      
   function Grammar_Opt_Error_Group_F_Expr
     (Node : Bare_Grammar_Opt_Error_Group) return Bare_Grammar_Expr_List
   is
      

   begin
         
         return Node.Grammar_Opt_Error_Group_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Opt_Group
   --

   

      
      procedure Initialize_Fields_For_Grammar_Opt_Group
        (Self : Bare_Grammar_Opt_Group
         ; Grammar_Opt_Group_F_Expr : Bare_Grammar_Expr_List
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Opt_Group_F_Expr := Grammar_Opt_Group_F_Expr;
         

      end Initialize_Fields_For_Grammar_Opt_Group;

      
   function Grammar_Opt_Group_F_Expr
     (Node : Bare_Grammar_Opt_Group) return Bare_Grammar_Expr_List
   is
      

   begin
         
         return Node.Grammar_Opt_Group_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Or_Expr
   --

   

      
      procedure Initialize_Fields_For_Grammar_Or_Expr
        (Self : Bare_Grammar_Or_Expr
         ; Grammar_Or_Expr_F_Sub_Exprs : Bare_Grammar_Expr_List_List
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Or_Expr_F_Sub_Exprs := Grammar_Or_Expr_F_Sub_Exprs;
         

      end Initialize_Fields_For_Grammar_Or_Expr;

      
   function Grammar_Or_Expr_F_Sub_Exprs
     (Node : Bare_Grammar_Or_Expr) return Bare_Grammar_Expr_List_List
   is
      

   begin
         
         return Node.Grammar_Or_Expr_F_Sub_Exprs;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Pick
   --

   

      
      procedure Initialize_Fields_For_Grammar_Pick
        (Self : Bare_Grammar_Pick
         ; Grammar_Pick_F_Exprs : Bare_Grammar_Expr_List
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Pick_F_Exprs := Grammar_Pick_F_Exprs;
         

      end Initialize_Fields_For_Grammar_Pick;

      
   function Grammar_Pick_F_Exprs
     (Node : Bare_Grammar_Pick) return Bare_Grammar_Expr_List
   is
      

   begin
         
         return Node.Grammar_Pick_F_Exprs;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Implicit_Pick
   --

   

      
      procedure Initialize_Fields_For_Grammar_Implicit_Pick
        (Self : Bare_Grammar_Implicit_Pick
         ; Grammar_Pick_F_Exprs : Bare_Grammar_Expr_List
        ) is
      begin
            Initialize_Fields_For_Grammar_Pick
              (Self, Grammar_Pick_F_Exprs);

         

      end Initialize_Fields_For_Grammar_Implicit_Pick;



   


      

   --
   --  Primitives for Bare_Grammar_Predicate
   --

   

      
      procedure Initialize_Fields_For_Grammar_Predicate
        (Self : Bare_Grammar_Predicate
         ; Grammar_Predicate_F_Expr : Bare_Grammar_Expr
         ; Grammar_Predicate_F_Prop_Ref : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Predicate_F_Expr := Grammar_Predicate_F_Expr;
            Self.Grammar_Predicate_F_Prop_Ref := Grammar_Predicate_F_Prop_Ref;
         

      end Initialize_Fields_For_Grammar_Predicate;

      
   function Grammar_Predicate_F_Expr
     (Node : Bare_Grammar_Predicate) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Grammar_Predicate_F_Expr;
      
   end;

      
   function Grammar_Predicate_F_Prop_Ref
     (Node : Bare_Grammar_Predicate) return Bare_Expr
   is
      

   begin
         
         return Node.Grammar_Predicate_F_Prop_Ref;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Rule_Ref
   --

   

      
      procedure Initialize_Fields_For_Grammar_Rule_Ref
        (Self : Bare_Grammar_Rule_Ref
         ; Grammar_Rule_Ref_F_Node_Name : Bare_Ref_Id
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Rule_Ref_F_Node_Name := Grammar_Rule_Ref_F_Node_Name;
         

      end Initialize_Fields_For_Grammar_Rule_Ref;

      
   function Grammar_Rule_Ref_F_Node_Name
     (Node : Bare_Grammar_Rule_Ref) return Bare_Ref_Id
   is
      

   begin
         
         return Node.Grammar_Rule_Ref_F_Node_Name;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Skip
   --

   

      
      procedure Initialize_Fields_For_Grammar_Skip
        (Self : Bare_Grammar_Skip
         ; Grammar_Skip_F_Name : Bare_Type_Ref
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Skip_F_Name := Grammar_Skip_F_Name;
         

      end Initialize_Fields_For_Grammar_Skip;

      
   function Grammar_Skip_F_Name
     (Node : Bare_Grammar_Skip) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Grammar_Skip_F_Name;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_Stop_Cut
   --

   

      
      procedure Initialize_Fields_For_Grammar_Stop_Cut
        (Self : Bare_Grammar_Stop_Cut
         ; Grammar_Stop_Cut_F_Expr : Bare_Grammar_Expr
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Grammar_Stop_Cut_F_Expr := Grammar_Stop_Cut_F_Expr;
         

      end Initialize_Fields_For_Grammar_Stop_Cut;

      
   function Grammar_Stop_Cut_F_Expr
     (Node : Bare_Grammar_Stop_Cut) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Grammar_Stop_Cut_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Parse_Node_Expr
   --

   

      
      procedure Initialize_Fields_For_Parse_Node_Expr
        (Self : Bare_Parse_Node_Expr
         ; Parse_Node_Expr_F_Node_Name : Bare_Type_Ref
         ; Parse_Node_Expr_F_Sub_Exprs : Bare_Grammar_Expr_List
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Parse_Node_Expr_F_Node_Name := Parse_Node_Expr_F_Node_Name;
            Self.Parse_Node_Expr_F_Sub_Exprs := Parse_Node_Expr_F_Sub_Exprs;
         

      end Initialize_Fields_For_Parse_Node_Expr;

      
   function Parse_Node_Expr_F_Node_Name
     (Node : Bare_Parse_Node_Expr) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Parse_Node_Expr_F_Node_Name;
      
   end;

      
   function Parse_Node_Expr_F_Sub_Exprs
     (Node : Bare_Parse_Node_Expr) return Bare_Grammar_Expr_List
   is
      

   begin
         
         return Node.Parse_Node_Expr_F_Sub_Exprs;
      
   end;



   


      

   --
   --  Primitives for Bare_Token_Lit
   --

   

      
      procedure Initialize_Fields_For_Token_Lit
        (Self : Bare_Token_Lit
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

         

      end Initialize_Fields_For_Token_Lit;



   


      

   --
   --  Primitives for Bare_Token_No_Case_Lit
   --

   

      
      procedure Initialize_Fields_For_Token_No_Case_Lit
        (Self : Bare_Token_No_Case_Lit
         ; Token_No_Case_Lit_F_Lit : Bare_Token_Lit
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Token_No_Case_Lit_F_Lit := Token_No_Case_Lit_F_Lit;
         

      end Initialize_Fields_For_Token_No_Case_Lit;

      
   function Token_No_Case_Lit_F_Lit
     (Node : Bare_Token_No_Case_Lit) return Bare_Token_Lit
   is
      

   begin
         
         return Node.Token_No_Case_Lit_F_Lit;
      
   end;



   


      

   --
   --  Primitives for Bare_Token_Pattern_Concat
   --

   

      
      procedure Initialize_Fields_For_Token_Pattern_Concat
        (Self : Bare_Token_Pattern_Concat
         ; Token_Pattern_Concat_F_Left : Bare_Grammar_Expr
         ; Token_Pattern_Concat_F_Right : Bare_Token_Pattern_Lit
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Token_Pattern_Concat_F_Left := Token_Pattern_Concat_F_Left;
            Self.Token_Pattern_Concat_F_Right := Token_Pattern_Concat_F_Right;
         

      end Initialize_Fields_For_Token_Pattern_Concat;

      
   function Token_Pattern_Concat_F_Left
     (Node : Bare_Token_Pattern_Concat) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Token_Pattern_Concat_F_Left;
      
   end;

      
   function Token_Pattern_Concat_F_Right
     (Node : Bare_Token_Pattern_Concat) return Bare_Token_Pattern_Lit
   is
      

   begin
         
         return Node.Token_Pattern_Concat_F_Right;
      
   end;



   


      

   --
   --  Primitives for Bare_Token_Pattern_Lit
   --

   

      
      procedure Initialize_Fields_For_Token_Pattern_Lit
        (Self : Bare_Token_Pattern_Lit
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

         

      end Initialize_Fields_For_Token_Pattern_Lit;



   


      

   --
   --  Primitives for Bare_Token_Ref
   --

   

      
      procedure Initialize_Fields_For_Token_Ref
        (Self : Bare_Token_Ref
         ; Token_Ref_F_Token_Name : Bare_Ref_Id
         ; Token_Ref_F_Expr : Bare_Token_Lit
        ) is
      begin
            Initialize_Fields_For_Grammar_Expr
              (Self);

            Self.Token_Ref_F_Token_Name := Token_Ref_F_Token_Name;
            Self.Token_Ref_F_Expr := Token_Ref_F_Expr;
         

      end Initialize_Fields_For_Token_Ref;

      
   function Token_Ref_F_Token_Name
     (Node : Bare_Token_Ref) return Bare_Ref_Id
   is
      

   begin
         
         return Node.Token_Ref_F_Token_Name;
      
   end;

      
   function Token_Ref_F_Expr
     (Node : Bare_Token_Ref) return Bare_Token_Lit
   is
      

   begin
         
         return Node.Token_Ref_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Id
   --

   

      
      procedure Initialize_Fields_For_Id
        (Self : Bare_Id
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

         

      end Initialize_Fields_For_Id;



   


      

   --
   --  Primitives for Bare_Def_Id
   --

   

      
      procedure Initialize_Fields_For_Def_Id
        (Self : Bare_Def_Id
        ) is
      begin
            Initialize_Fields_For_Id
              (Self);

         

      end Initialize_Fields_For_Def_Id;



   


      

   --
   --  Primitives for Bare_Imported_Id
   --

   

      
      procedure Initialize_Fields_For_Imported_Id
        (Self : Bare_Imported_Id
        ) is
      begin
            Initialize_Fields_For_Id
              (Self);

         

      end Initialize_Fields_For_Imported_Id;



   


      

   --
   --  Primitives for Bare_Module_Id
   --

   

      
      procedure Initialize_Fields_For_Module_Id
        (Self : Bare_Module_Id
        ) is
      begin
            Initialize_Fields_For_Id
              (Self);

         

      end Initialize_Fields_For_Module_Id;



   


      

   --
   --  Primitives for Bare_Ref_Id
   --

   

      
      procedure Initialize_Fields_For_Ref_Id
        (Self : Bare_Ref_Id
        ) is
      begin
            Initialize_Fields_For_Id
              (Self);

         
      Self.Ref_Id_F_Ref_Var := Null_Var_Record;

      end Initialize_Fields_For_Ref_Id;



   


      

   --
   --  Primitives for Bare_If_Expr
   --

   

      
      procedure Initialize_Fields_For_If_Expr
        (Self : Bare_If_Expr
         ; If_Expr_F_Cond_Expr : Bare_Expr
         ; If_Expr_F_Then_Expr : Bare_Expr
         ; If_Expr_F_Alternatives : Bare_Elsif_Branch_List
         ; If_Expr_F_Else_Expr : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.If_Expr_F_Cond_Expr := If_Expr_F_Cond_Expr;
            Self.If_Expr_F_Then_Expr := If_Expr_F_Then_Expr;
            Self.If_Expr_F_Alternatives := If_Expr_F_Alternatives;
            Self.If_Expr_F_Else_Expr := If_Expr_F_Else_Expr;
         
      Self.If_Expr_F_Expected_Branch_Type_Var := Null_Var_Record;

      end Initialize_Fields_For_If_Expr;

      
   function If_Expr_F_Cond_Expr
     (Node : Bare_If_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.If_Expr_F_Cond_Expr;
      
   end;

      
   function If_Expr_F_Then_Expr
     (Node : Bare_If_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.If_Expr_F_Then_Expr;
      
   end;

      
   function If_Expr_F_Alternatives
     (Node : Bare_If_Expr) return Bare_Elsif_Branch_List
   is
      

   begin
         
         return Node.If_Expr_F_Alternatives;
      
   end;

      
   function If_Expr_F_Else_Expr
     (Node : Bare_If_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.If_Expr_F_Else_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Isa
   --

   

      
      procedure Initialize_Fields_For_Isa
        (Self : Bare_Isa
         ; Isa_F_Expr : Bare_Expr
         ; Isa_F_Pattern : Bare_Pattern
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Isa_F_Expr := Isa_F_Expr;
            Self.Isa_F_Pattern := Isa_F_Pattern;
         

      end Initialize_Fields_For_Isa;

      
   function Isa_F_Expr
     (Node : Bare_Isa) return Bare_Expr
   is
      

   begin
         
         return Node.Isa_F_Expr;
      
   end;

      
   function Isa_F_Pattern
     (Node : Bare_Isa) return Bare_Pattern
   is
      

   begin
         
         return Node.Isa_F_Pattern;
      
   end;



   


      

   --
   --  Primitives for Bare_Keep_Expr
   --

   

      
      procedure Initialize_Fields_For_Keep_Expr
        (Self : Bare_Keep_Expr
         ; Keep_Expr_F_Expr : Bare_Expr
         ; Keep_Expr_F_Null_Cond : Bare_Null_Cond_Qualifier
         ; Keep_Expr_F_Keep_Type : Bare_Type_Ref
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Keep_Expr_F_Expr := Keep_Expr_F_Expr;
            Self.Keep_Expr_F_Null_Cond := Keep_Expr_F_Null_Cond;
            Self.Keep_Expr_F_Keep_Type := Keep_Expr_F_Keep_Type;
         
      Self.Keep_Expr_F_Array_Element_Type := Null_Var_Record;

      end Initialize_Fields_For_Keep_Expr;

      
   function Keep_Expr_F_Expr
     (Node : Bare_Keep_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Keep_Expr_F_Expr;
      
   end;

      
   function Keep_Expr_F_Null_Cond
     (Node : Bare_Keep_Expr) return Bare_Null_Cond_Qualifier
   is
      

   begin
         
         return Node.Keep_Expr_F_Null_Cond;
      
   end;

      
   function Keep_Expr_F_Keep_Type
     (Node : Bare_Keep_Expr) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Keep_Expr_F_Keep_Type;
      
   end;



   


      

   --
   --  Primitives for Bare_Lambda_Expr
   --

   

      
      procedure Initialize_Fields_For_Lambda_Expr
        (Self : Bare_Lambda_Expr
         ; Lambda_Expr_F_Params : Bare_Lambda_Param_Decl_List
         ; Lambda_Expr_F_Return_Type : Bare_Type_Ref
         ; Lambda_Expr_F_Body : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Lambda_Expr_F_Params := Lambda_Expr_F_Params;
            Self.Lambda_Expr_F_Return_Type := Lambda_Expr_F_Return_Type;
            Self.Lambda_Expr_F_Body := Lambda_Expr_F_Body;
         

      end Initialize_Fields_For_Lambda_Expr;

      
   function Lambda_Expr_F_Params
     (Node : Bare_Lambda_Expr) return Bare_Lambda_Param_Decl_List
   is
      

   begin
         
         return Node.Lambda_Expr_F_Params;
      
   end;

      
   function Lambda_Expr_F_Return_Type
     (Node : Bare_Lambda_Expr) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Lambda_Expr_F_Return_Type;
      
   end;

      
   function Lambda_Expr_F_Body
     (Node : Bare_Lambda_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Lambda_Expr_F_Body;
      
   end;



   


      

   --
   --  Primitives for Bare_Lit
   --

   

      
      procedure Initialize_Fields_For_Lit
        (Self : Bare_Lit
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

         

      end Initialize_Fields_For_Lit;



   


      

   --
   --  Primitives for Bare_Big_Num_Lit
   --

   

      
      procedure Initialize_Fields_For_Big_Num_Lit
        (Self : Bare_Big_Num_Lit
        ) is
      begin
            Initialize_Fields_For_Lit
              (Self);

         

      end Initialize_Fields_For_Big_Num_Lit;



   


      

   --
   --  Primitives for Bare_Char_Lit
   --

   

      
      procedure Initialize_Fields_For_Char_Lit
        (Self : Bare_Char_Lit
        ) is
      begin
            Initialize_Fields_For_Lit
              (Self);

         

      end Initialize_Fields_For_Char_Lit;



   


      

   --
   --  Primitives for Bare_Null_Lit
   --

   

      
      procedure Initialize_Fields_For_Null_Lit
        (Self : Bare_Null_Lit
         ; Null_Lit_F_Dest_Type : Bare_Type_Ref
        ) is
      begin
            Initialize_Fields_For_Lit
              (Self);

            Self.Null_Lit_F_Dest_Type := Null_Lit_F_Dest_Type;
         

      end Initialize_Fields_For_Null_Lit;

      
   function Null_Lit_F_Dest_Type
     (Node : Bare_Null_Lit) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Null_Lit_F_Dest_Type;
      
   end;



   


      

   --
   --  Primitives for Bare_Num_Lit
   --

   

      
      procedure Initialize_Fields_For_Num_Lit
        (Self : Bare_Num_Lit
        ) is
      begin
            Initialize_Fields_For_Lit
              (Self);

         

      end Initialize_Fields_For_Num_Lit;



   


      

   --
   --  Primitives for Bare_String_Lit
   --

   

      
      procedure Initialize_Fields_For_String_Lit
        (Self : Bare_String_Lit
        ) is
      begin
            Initialize_Fields_For_Lit
              (Self);

         

      end Initialize_Fields_For_String_Lit;



   


      

   --
   --  Primitives for Bare_Block_String_Lit
   --

   

      
      procedure Initialize_Fields_For_Block_String_Lit
        (Self : Bare_Block_String_Lit
         ; Block_String_Lit_F_Lines : Bare_Block_String_Line_List
        ) is
      begin
            Initialize_Fields_For_String_Lit
              (Self);

            Self.Block_String_Lit_F_Lines := Block_String_Lit_F_Lines;
         

      end Initialize_Fields_For_Block_String_Lit;

      
   function Block_String_Lit_F_Lines
     (Node : Bare_Block_String_Lit) return Bare_Block_String_Line_List
   is
      

   begin
         
         return Node.Block_String_Lit_F_Lines;
      
   end;



   


      

   --
   --  Primitives for Bare_Module_Doc_String_Lit
   --

   

      
      procedure Initialize_Fields_For_Module_Doc_String_Lit
        (Self : Bare_Module_Doc_String_Lit
         ; Module_Doc_String_Lit_F_Lines : Bare_Module_Doc_String_Line_List
        ) is
      begin
            Initialize_Fields_For_String_Lit
              (Self);

            Self.Module_Doc_String_Lit_F_Lines := Module_Doc_String_Lit_F_Lines;
         

      end Initialize_Fields_For_Module_Doc_String_Lit;

      
   function Module_Doc_String_Lit_F_Lines
     (Node : Bare_Module_Doc_String_Lit) return Bare_Module_Doc_String_Line_List
   is
      

   begin
         
         return Node.Module_Doc_String_Lit_F_Lines;
      
   end;



   


      

   --
   --  Primitives for Bare_Single_Line_String_Lit
   --

   

      
      procedure Initialize_Fields_For_Single_Line_String_Lit
        (Self : Bare_Single_Line_String_Lit
        ) is
      begin
            Initialize_Fields_For_String_Lit
              (Self);

         

      end Initialize_Fields_For_Single_Line_String_Lit;



   


      

   --
   --  Primitives for Bare_Pattern_Single_Line_String_Lit
   --

   

      
      procedure Initialize_Fields_For_Pattern_Single_Line_String_Lit
        (Self : Bare_Pattern_Single_Line_String_Lit
        ) is
      begin
            Initialize_Fields_For_Single_Line_String_Lit
              (Self);

         

      end Initialize_Fields_For_Pattern_Single_Line_String_Lit;



   


      

   --
   --  Primitives for Bare_Logic_Assign
   --

   

      
      procedure Initialize_Fields_For_Logic_Assign
        (Self : Bare_Logic_Assign
         ; Logic_Assign_F_Dest_Var : Bare_Expr
         ; Logic_Assign_F_Value : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Logic_Assign_F_Dest_Var := Logic_Assign_F_Dest_Var;
            Self.Logic_Assign_F_Value := Logic_Assign_F_Value;
         

      end Initialize_Fields_For_Logic_Assign;

      
   function Logic_Assign_F_Dest_Var
     (Node : Bare_Logic_Assign) return Bare_Expr
   is
      

   begin
         
         return Node.Logic_Assign_F_Dest_Var;
      
   end;

      
   function Logic_Assign_F_Value
     (Node : Bare_Logic_Assign) return Bare_Expr
   is
      

   begin
         
         return Node.Logic_Assign_F_Value;
      
   end;



   


      

   --
   --  Primitives for Bare_Logic_Expr
   --

   

      
      procedure Initialize_Fields_For_Logic_Expr
        (Self : Bare_Logic_Expr
         ; Logic_Expr_F_Expr : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Logic_Expr_F_Expr := Logic_Expr_F_Expr;
         

      end Initialize_Fields_For_Logic_Expr;

      
   function Logic_Expr_F_Expr
     (Node : Bare_Logic_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Logic_Expr_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Logic_Propagate
   --

   

      
      procedure Initialize_Fields_For_Logic_Propagate
        (Self : Bare_Logic_Propagate
         ; Logic_Propagate_F_Dest_Var : Bare_Expr
         ; Logic_Propagate_F_Call : Bare_Logic_Propagate_Call
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Logic_Propagate_F_Dest_Var := Logic_Propagate_F_Dest_Var;
            Self.Logic_Propagate_F_Call := Logic_Propagate_F_Call;
         

      end Initialize_Fields_For_Logic_Propagate;

      
   function Logic_Propagate_F_Dest_Var
     (Node : Bare_Logic_Propagate) return Bare_Expr
   is
      

   begin
         
         return Node.Logic_Propagate_F_Dest_Var;
      
   end;

      
   function Logic_Propagate_F_Call
     (Node : Bare_Logic_Propagate) return Bare_Logic_Propagate_Call
   is
      

   begin
         
         return Node.Logic_Propagate_F_Call;
      
   end;



   


      

   --
   --  Primitives for Bare_Logic_Unify
   --

   

      
      procedure Initialize_Fields_For_Logic_Unify
        (Self : Bare_Logic_Unify
         ; Logic_Unify_F_Lhs : Bare_Expr
         ; Logic_Unify_F_Rhs : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Logic_Unify_F_Lhs := Logic_Unify_F_Lhs;
            Self.Logic_Unify_F_Rhs := Logic_Unify_F_Rhs;
         

      end Initialize_Fields_For_Logic_Unify;

      
   function Logic_Unify_F_Lhs
     (Node : Bare_Logic_Unify) return Bare_Expr
   is
      

   begin
         
         return Node.Logic_Unify_F_Lhs;
      
   end;

      
   function Logic_Unify_F_Rhs
     (Node : Bare_Logic_Unify) return Bare_Expr
   is
      

   begin
         
         return Node.Logic_Unify_F_Rhs;
      
   end;



   


      

   --
   --  Primitives for Bare_Match_Expr
   --

   

      
      procedure Initialize_Fields_For_Match_Expr
        (Self : Bare_Match_Expr
         ; Match_Expr_F_Match_Expr : Bare_Expr
         ; Match_Expr_F_Branches : Bare_Base_Match_Branch_List
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Match_Expr_F_Match_Expr := Match_Expr_F_Match_Expr;
            Self.Match_Expr_F_Branches := Match_Expr_F_Branches;
         
      Self.Match_Expr_F_Expected_Branch_Type_Var := Null_Var_Record;

      end Initialize_Fields_For_Match_Expr;

      
   function Match_Expr_F_Match_Expr
     (Node : Bare_Match_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Match_Expr_F_Match_Expr;
      
   end;

      
   function Match_Expr_F_Branches
     (Node : Bare_Match_Expr) return Bare_Base_Match_Branch_List
   is
      

   begin
         
         return Node.Match_Expr_F_Branches;
      
   end;



   


      

   --
   --  Primitives for Bare_Not_Expr
   --

   

      
      procedure Initialize_Fields_For_Not_Expr
        (Self : Bare_Not_Expr
         ; Not_Expr_F_Expr : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Not_Expr_F_Expr := Not_Expr_F_Expr;
         

      end Initialize_Fields_For_Not_Expr;

      
   function Not_Expr_F_Expr
     (Node : Bare_Not_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Not_Expr_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Paren_Expr
   --

   

      
      procedure Initialize_Fields_For_Paren_Expr
        (Self : Bare_Paren_Expr
         ; Paren_Expr_F_Expr : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Paren_Expr_F_Expr := Paren_Expr_F_Expr;
         

      end Initialize_Fields_For_Paren_Expr;

      
   function Paren_Expr_F_Expr
     (Node : Bare_Paren_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Paren_Expr_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Query
   --

   

      
      procedure Initialize_Fields_For_Query
        (Self : Bare_Query
         ; Query_F_Source : Bare_Expr
         ; Query_F_Pattern : Bare_Pattern
         ; Query_F_Mapping : Bare_Expr
         ; Query_F_Guard : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Query_F_Source := Query_F_Source;
            Self.Query_F_Pattern := Query_F_Pattern;
            Self.Query_F_Mapping := Query_F_Mapping;
            Self.Query_F_Guard := Query_F_Guard;
         

      end Initialize_Fields_For_Query;

      
   function Query_F_Source
     (Node : Bare_Query) return Bare_Expr
   is
      

   begin
         
         return Node.Query_F_Source;
      
   end;

      
   function Query_F_Pattern
     (Node : Bare_Query) return Bare_Pattern
   is
      

   begin
         
         return Node.Query_F_Pattern;
      
   end;

      
   function Query_F_Mapping
     (Node : Bare_Query) return Bare_Expr
   is
      

   begin
         
         return Node.Query_F_Mapping;
      
   end;

      
   function Query_F_Guard
     (Node : Bare_Query) return Bare_Expr
   is
      

   begin
         
         return Node.Query_F_Guard;
      
   end;



   


      

   --
   --  Primitives for Bare_Raise_Expr
   --

   

      
      procedure Initialize_Fields_For_Raise_Expr
        (Self : Bare_Raise_Expr
         ; Raise_Expr_F_Dest_Type : Bare_Type_Ref
         ; Raise_Expr_F_Except_Expr : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Raise_Expr_F_Dest_Type := Raise_Expr_F_Dest_Type;
            Self.Raise_Expr_F_Except_Expr := Raise_Expr_F_Except_Expr;
         

      end Initialize_Fields_For_Raise_Expr;

      
   function Raise_Expr_F_Dest_Type
     (Node : Bare_Raise_Expr) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Raise_Expr_F_Dest_Type;
      
   end;

      
   function Raise_Expr_F_Except_Expr
     (Node : Bare_Raise_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Raise_Expr_F_Except_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Subscript_Expr
   --

   

      
      procedure Initialize_Fields_For_Subscript_Expr
        (Self : Bare_Subscript_Expr
         ; Subscript_Expr_F_Prefix : Bare_Expr
         ; Subscript_Expr_F_Null_Cond : Bare_Null_Cond_Qualifier
         ; Subscript_Expr_F_Index : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Subscript_Expr_F_Prefix := Subscript_Expr_F_Prefix;
            Self.Subscript_Expr_F_Null_Cond := Subscript_Expr_F_Null_Cond;
            Self.Subscript_Expr_F_Index := Subscript_Expr_F_Index;
         

      end Initialize_Fields_For_Subscript_Expr;

      
   function Subscript_Expr_F_Prefix
     (Node : Bare_Subscript_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Subscript_Expr_F_Prefix;
      
   end;

      
   function Subscript_Expr_F_Null_Cond
     (Node : Bare_Subscript_Expr) return Bare_Null_Cond_Qualifier
   is
      

   begin
         
         return Node.Subscript_Expr_F_Null_Cond;
      
   end;

      
   function Subscript_Expr_F_Index
     (Node : Bare_Subscript_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Subscript_Expr_F_Index;
      
   end;



   


      

   --
   --  Primitives for Bare_Try_Expr
   --

   

      
      procedure Initialize_Fields_For_Try_Expr
        (Self : Bare_Try_Expr
         ; Try_Expr_F_Try_Expr : Bare_Expr
         ; Try_Expr_F_Or_Expr : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Try_Expr_F_Try_Expr := Try_Expr_F_Try_Expr;
            Self.Try_Expr_F_Or_Expr := Try_Expr_F_Or_Expr;
         
      Self.Try_Expr_F_Expected_Expr_Type_Var := Null_Var_Record;

      end Initialize_Fields_For_Try_Expr;

      
   function Try_Expr_F_Try_Expr
     (Node : Bare_Try_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Try_Expr_F_Try_Expr;
      
   end;

      
   function Try_Expr_F_Or_Expr
     (Node : Bare_Try_Expr) return Bare_Expr
   is
      

   begin
         
         return Node.Try_Expr_F_Or_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Un_Op
   --

   

      
      procedure Initialize_Fields_For_Un_Op
        (Self : Bare_Un_Op
         ; Un_Op_F_Op : Bare_Op
         ; Un_Op_F_Expr : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Expr
              (Self);

            Self.Un_Op_F_Op := Un_Op_F_Op;
            Self.Un_Op_F_Expr := Un_Op_F_Expr;
         

      end Initialize_Fields_For_Un_Op;

      
   function Un_Op_F_Op
     (Node : Bare_Un_Op) return Bare_Op
   is
      

   begin
         
         return Node.Un_Op_F_Op;
      
   end;

      
   function Un_Op_F_Expr
     (Node : Bare_Un_Op) return Bare_Expr
   is
      

   begin
         
         return Node.Un_Op_F_Expr;
      
   end;



   


      

   --
   --  Primitives for Bare_Full_Decl
   --

   

      
      procedure Initialize_Fields_For_Full_Decl
        (Self : Bare_Full_Decl
         ; Full_Decl_F_Doc : Bare_String_Lit
         ; Full_Decl_F_Decl_Annotations : Bare_Decl_Annotation_List
         ; Full_Decl_F_Decl : Bare_Decl
        ) is
      begin

            Self.Full_Decl_F_Doc := Full_Decl_F_Doc;
            Self.Full_Decl_F_Decl_Annotations := Full_Decl_F_Decl_Annotations;
            Self.Full_Decl_F_Decl := Full_Decl_F_Decl;
         

      end Initialize_Fields_For_Full_Decl;

      
   function Full_Decl_F_Doc
     (Node : Bare_Full_Decl) return Bare_String_Lit
   is
      

   begin
         
         return Node.Full_Decl_F_Doc;
      
   end;

      
   function Full_Decl_F_Decl_Annotations
     (Node : Bare_Full_Decl) return Bare_Decl_Annotation_List
   is
      

   begin
         
         return Node.Full_Decl_F_Decl_Annotations;
      
   end;

      
   function Full_Decl_F_Decl
     (Node : Bare_Full_Decl) return Bare_Decl
   is
      

   begin
         
         return Node.Full_Decl_F_Decl;
      
   end;



   


      

   --
   --  Primitives for Bare_Grammar_List_Sep
   --

   

      
      procedure Initialize_Fields_For_Grammar_List_Sep
        (Self : Bare_Grammar_List_Sep
         ; Grammar_List_Sep_F_Token : Bare_Grammar_Expr
         ; Grammar_List_Sep_F_Extra : Bare_Id
        ) is
      begin

            Self.Grammar_List_Sep_F_Token := Grammar_List_Sep_F_Token;
            Self.Grammar_List_Sep_F_Extra := Grammar_List_Sep_F_Extra;
         

      end Initialize_Fields_For_Grammar_List_Sep;

      
   function Grammar_List_Sep_F_Token
     (Node : Bare_Grammar_List_Sep) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Grammar_List_Sep_F_Token;
      
   end;

      
   function Grammar_List_Sep_F_Extra
     (Node : Bare_Grammar_List_Sep) return Bare_Id
   is
      

   begin
         
         return Node.Grammar_List_Sep_F_Extra;
      
   end;



   


      

   --
   --  Primitives for Bare_Imported_Name
   --

   

      
      procedure Initialize_Fields_For_Imported_Name
        (Self : Bare_Imported_Name
         ; Imported_Name_F_Original_Name : Bare_Imported_Id
         ; Imported_Name_F_Renaming : Bare_Def_Id
        ) is
      begin

            Self.Imported_Name_F_Original_Name := Imported_Name_F_Original_Name;
            Self.Imported_Name_F_Renaming := Imported_Name_F_Renaming;
         

      end Initialize_Fields_For_Imported_Name;

      
   function Imported_Name_F_Original_Name
     (Node : Bare_Imported_Name) return Bare_Imported_Id
   is
      

   begin
         
         return Node.Imported_Name_F_Original_Name;
      
   end;

      
   function Imported_Name_F_Renaming
     (Node : Bare_Imported_Name) return Bare_Def_Id
   is
      

   begin
         
         return Node.Imported_Name_F_Renaming;
      
   end;



   


      

   --
   --  Primitives for Bare_Lexer_Case_Rule
   --

   

      
      procedure Initialize_Fields_For_Lexer_Case_Rule
        (Self : Bare_Lexer_Case_Rule
         ; Lexer_Case_Rule_F_Expr : Bare_Grammar_Expr
         ; Lexer_Case_Rule_F_Alts : Bare_Base_Lexer_Case_Rule_Alt_List
        ) is
      begin

            Self.Lexer_Case_Rule_F_Expr := Lexer_Case_Rule_F_Expr;
            Self.Lexer_Case_Rule_F_Alts := Lexer_Case_Rule_F_Alts;
         

      end Initialize_Fields_For_Lexer_Case_Rule;

      
   function Lexer_Case_Rule_F_Expr
     (Node : Bare_Lexer_Case_Rule) return Bare_Grammar_Expr
   is
      

   begin
         
         return Node.Lexer_Case_Rule_F_Expr;
      
   end;

      
   function Lexer_Case_Rule_F_Alts
     (Node : Bare_Lexer_Case_Rule) return Bare_Base_Lexer_Case_Rule_Alt_List
   is
      

   begin
         
         return Node.Lexer_Case_Rule_F_Alts;
      
   end;



   


      

   --
   --  Primitives for Bare_Lexer_Case_Rule_Send
   --

   

      
      procedure Initialize_Fields_For_Lexer_Case_Rule_Send
        (Self : Bare_Lexer_Case_Rule_Send
         ; Lexer_Case_Rule_Send_F_Sent : Bare_Ref_Id
         ; Lexer_Case_Rule_Send_F_Match_Size : Bare_Num_Lit
        ) is
      begin

            Self.Lexer_Case_Rule_Send_F_Sent := Lexer_Case_Rule_Send_F_Sent;
            Self.Lexer_Case_Rule_Send_F_Match_Size := Lexer_Case_Rule_Send_F_Match_Size;
         

      end Initialize_Fields_For_Lexer_Case_Rule_Send;

      
   function Lexer_Case_Rule_Send_F_Sent
     (Node : Bare_Lexer_Case_Rule_Send) return Bare_Ref_Id
   is
      

   begin
         
         return Node.Lexer_Case_Rule_Send_F_Sent;
      
   end;

      
   function Lexer_Case_Rule_Send_F_Match_Size
     (Node : Bare_Lexer_Case_Rule_Send) return Bare_Num_Lit
   is
      

   begin
         
         return Node.Lexer_Case_Rule_Send_F_Match_Size;
      
   end;



   


      

   --
   --  Primitives for Bare_List_Kind
   --

   




   


      

   --
   --  Primitives for Bare_List_Kind_One
   --

   




   


      

   --
   --  Primitives for Bare_List_Kind_Zero
   --

   




   


      

   --
   --  Primitives for Bare_Lkt_Node_Base_List
   --

   




   


      

   --
   --  Primitives for Bare_Argument_List
   --

   




   


      

   --
   --  Primitives for Bare_Base_Import_List
   --

   




   


      

   --
   --  Primitives for Bare_Base_Lexer_Case_Rule_Alt_List
   --

   




   


      

   --
   --  Primitives for Bare_Base_Match_Branch_List
   --

   




   


      

   --
   --  Primitives for Bare_Block_String_Line_List
   --

   




   


      

   --
   --  Primitives for Bare_Call_Expr_List
   --

   




   


      

   --
   --  Primitives for Bare_Decl_Annotation_List
   --

   




   


      

   --
   --  Primitives for Bare_Elsif_Branch_List
   --

   




   


      

   --
   --  Primitives for Bare_Enum_Class_Alt_Decl_List
   --

   




   


      

   --
   --  Primitives for Bare_Enum_Class_Case_List
   --

   




   


      

   --
   --  Primitives for Bare_Enum_Lit_Decl_List
   --

   




   


      

   --
   --  Primitives for Bare_Expr_List
   --

   




   


      

   --
   --  Primitives for Bare_Any_Of_List
   --

   




   


      

   --
   --  Primitives for Bare_Full_Decl_List
   --

   




   


      

   --
   --  Primitives for Bare_Decl_Block
   --

   




   


      

   --
   --  Primitives for Bare_Generic_Param_Decl_List
   --

   




   


      

   --
   --  Primitives for Bare_Fun_Param_Decl_List
   --

   




   


      

   --
   --  Primitives for Bare_Grammar_Expr_List
   --

   




   


      

   --
   --  Primitives for Bare_Grammar_Expr_List_List
   --

   




   


      

   --
   --  Primitives for Bare_Imported_Name_List
   --

   




   


      

   --
   --  Primitives for Bare_Lambda_Param_Decl_List
   --

   




   


      

   --
   --  Primitives for Bare_Lkt_Node_List
   --

   




   


      

   --
   --  Primitives for Bare_Module_Doc_String_Line_List
   --

   




   


      

   --
   --  Primitives for Bare_Pattern_Detail_List
   --

   




   


      

   --
   --  Primitives for Bare_Pattern_List
   --

   




   


      

   --
   --  Primitives for Bare_Ref_Id_List
   --

   




   


      

   --
   --  Primitives for Bare_Type_Ref_List
   --

   




   


      

   --
   --  Primitives for Bare_Synthetic_Type_Ref_List
   --

   




   


      

   --
   --  Primitives for Bare_Module_Doc_String_Line
   --

   




   


      

   --
   --  Primitives for Bare_Null_Cond_Qualifier
   --

   




   


      

   --
   --  Primitives for Bare_Null_Cond_Qualifier_Absent
   --

   




   


      

   --
   --  Primitives for Bare_Null_Cond_Qualifier_Present
   --

   




   


      

   --
   --  Primitives for Bare_Op
   --

   




   


      

   --
   --  Primitives for Bare_Op_Amp
   --

   




   


      

   --
   --  Primitives for Bare_Op_And
   --

   




   


      

   --
   --  Primitives for Bare_Op_Div
   --

   




   


      

   --
   --  Primitives for Bare_Op_Eq
   --

   




   


      

   --
   --  Primitives for Bare_Op_Gt
   --

   




   


      

   --
   --  Primitives for Bare_Op_Gte
   --

   




   


      

   --
   --  Primitives for Bare_Op_Logic_And
   --

   




   


      

   --
   --  Primitives for Bare_Op_Logic_Or
   --

   




   


      

   --
   --  Primitives for Bare_Op_Lt
   --

   




   


      

   --
   --  Primitives for Bare_Op_Lte
   --

   




   


      

   --
   --  Primitives for Bare_Op_Minus
   --

   




   


      

   --
   --  Primitives for Bare_Op_Mult
   --

   




   


      

   --
   --  Primitives for Bare_Op_Ne
   --

   




   


      

   --
   --  Primitives for Bare_Op_Or
   --

   




   


      

   --
   --  Primitives for Bare_Op_Or_Int
   --

   




   


      

   --
   --  Primitives for Bare_Op_Plus
   --

   




   


      

   --
   --  Primitives for Bare_Op_Stream_Concat
   --

   




   


      

   --
   --  Primitives for Bare_Op_Stream_Cons
   --

   




   


      

   --
   --  Primitives for Bare_Pattern
   --

   




   


      

   --
   --  Primitives for Bare_Any_Type_Pattern
   --

   




   


      

   --
   --  Primitives for Bare_Bool_Pattern
   --

   




   


      

   --
   --  Primitives for Bare_Bool_Pattern_False
   --

   




   


      

   --
   --  Primitives for Bare_Bool_Pattern_True
   --

   




   


      

   --
   --  Primitives for Bare_Complex_Pattern
   --

   

      
      procedure Initialize_Fields_For_Complex_Pattern
        (Self : Bare_Complex_Pattern
         ; Complex_Pattern_F_Decl : Bare_Binding_Val_Decl
         ; Complex_Pattern_F_Pattern : Bare_Pattern
         ; Complex_Pattern_F_Details : Bare_Pattern_Detail_List
         ; Complex_Pattern_F_Predicate : Bare_Expr
        ) is
      begin

            Self.Complex_Pattern_F_Decl := Complex_Pattern_F_Decl;
            Self.Complex_Pattern_F_Pattern := Complex_Pattern_F_Pattern;
            Self.Complex_Pattern_F_Details := Complex_Pattern_F_Details;
            Self.Complex_Pattern_F_Predicate := Complex_Pattern_F_Predicate;
         

      end Initialize_Fields_For_Complex_Pattern;

      
   function Complex_Pattern_F_Decl
     (Node : Bare_Complex_Pattern) return Bare_Binding_Val_Decl
   is
      

   begin
         
         return Node.Complex_Pattern_F_Decl;
      
   end;

      
   function Complex_Pattern_F_Pattern
     (Node : Bare_Complex_Pattern) return Bare_Pattern
   is
      

   begin
         
         return Node.Complex_Pattern_F_Pattern;
      
   end;

      
   function Complex_Pattern_F_Details
     (Node : Bare_Complex_Pattern) return Bare_Pattern_Detail_List
   is
      

   begin
         
         return Node.Complex_Pattern_F_Details;
      
   end;

      
   function Complex_Pattern_F_Predicate
     (Node : Bare_Complex_Pattern) return Bare_Expr
   is
      

   begin
         
         return Node.Complex_Pattern_F_Predicate;
      
   end;



   


      

   --
   --  Primitives for Bare_Renaming_Complex_Pattern
   --

   

      
      procedure Initialize_Fields_For_Renaming_Complex_Pattern
        (Self : Bare_Renaming_Complex_Pattern
         ; Complex_Pattern_F_Decl : Bare_Binding_Val_Decl
         ; Complex_Pattern_F_Pattern : Bare_Pattern
         ; Complex_Pattern_F_Details : Bare_Pattern_Detail_List
         ; Complex_Pattern_F_Predicate : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Complex_Pattern
              (Self, Complex_Pattern_F_Decl, Complex_Pattern_F_Pattern, Complex_Pattern_F_Details, Complex_Pattern_F_Predicate);

         

      end Initialize_Fields_For_Renaming_Complex_Pattern;



   


      

   --
   --  Primitives for Bare_Ellipsis_Pattern
   --

   

      
      procedure Initialize_Fields_For_Ellipsis_Pattern
        (Self : Bare_Ellipsis_Pattern
         ; Ellipsis_Pattern_F_Binding : Bare_Id
        ) is
      begin

            Self.Ellipsis_Pattern_F_Binding := Ellipsis_Pattern_F_Binding;
         

      end Initialize_Fields_For_Ellipsis_Pattern;

      
   function Ellipsis_Pattern_F_Binding
     (Node : Bare_Ellipsis_Pattern) return Bare_Id
   is
      

   begin
         
         return Node.Ellipsis_Pattern_F_Binding;
      
   end;



   


      

   --
   --  Primitives for Bare_Integer_Pattern
   --

   




   


      

   --
   --  Primitives for Bare_List_Pattern
   --

   

      
      procedure Initialize_Fields_For_List_Pattern
        (Self : Bare_List_Pattern
         ; List_Pattern_F_Sub_Patterns : Bare_Pattern_List
        ) is
      begin

            Self.List_Pattern_F_Sub_Patterns := List_Pattern_F_Sub_Patterns;
         

      end Initialize_Fields_For_List_Pattern;

      
   function List_Pattern_F_Sub_Patterns
     (Node : Bare_List_Pattern) return Bare_Pattern_List
   is
      

   begin
         
         return Node.List_Pattern_F_Sub_Patterns;
      
   end;



   


      

   --
   --  Primitives for Bare_Not_Pattern
   --

   

      
      procedure Initialize_Fields_For_Not_Pattern
        (Self : Bare_Not_Pattern
         ; Not_Pattern_F_Sub_Pattern : Bare_Pattern
        ) is
      begin

            Self.Not_Pattern_F_Sub_Pattern := Not_Pattern_F_Sub_Pattern;
         

      end Initialize_Fields_For_Not_Pattern;

      
   function Not_Pattern_F_Sub_Pattern
     (Node : Bare_Not_Pattern) return Bare_Pattern
   is
      

   begin
         
         return Node.Not_Pattern_F_Sub_Pattern;
      
   end;



   


      

   --
   --  Primitives for Bare_Null_Pattern
   --

   




   


      

   --
   --  Primitives for Bare_Or_Pattern
   --

   

      
      procedure Initialize_Fields_For_Or_Pattern
        (Self : Bare_Or_Pattern
         ; Or_Pattern_F_Left_Sub_Pattern : Bare_Pattern
         ; Or_Pattern_F_Right_Sub_Pattern : Bare_Pattern
        ) is
      begin

            Self.Or_Pattern_F_Left_Sub_Pattern := Or_Pattern_F_Left_Sub_Pattern;
            Self.Or_Pattern_F_Right_Sub_Pattern := Or_Pattern_F_Right_Sub_Pattern;
         

      end Initialize_Fields_For_Or_Pattern;

      
   function Or_Pattern_F_Left_Sub_Pattern
     (Node : Bare_Or_Pattern) return Bare_Pattern
   is
      

   begin
         
         return Node.Or_Pattern_F_Left_Sub_Pattern;
      
   end;

      
   function Or_Pattern_F_Right_Sub_Pattern
     (Node : Bare_Or_Pattern) return Bare_Pattern
   is
      

   begin
         
         return Node.Or_Pattern_F_Right_Sub_Pattern;
      
   end;



   


      

   --
   --  Primitives for Bare_Paren_Pattern
   --

   

      
      procedure Initialize_Fields_For_Paren_Pattern
        (Self : Bare_Paren_Pattern
         ; Paren_Pattern_F_Sub_Pattern : Bare_Pattern
        ) is
      begin

            Self.Paren_Pattern_F_Sub_Pattern := Paren_Pattern_F_Sub_Pattern;
         

      end Initialize_Fields_For_Paren_Pattern;

      
   function Paren_Pattern_F_Sub_Pattern
     (Node : Bare_Paren_Pattern) return Bare_Pattern
   is
      

   begin
         
         return Node.Paren_Pattern_F_Sub_Pattern;
      
   end;



   


      

   --
   --  Primitives for Bare_Regex_Pattern
   --

   




   


      

   --
   --  Primitives for Bare_Type_Pattern
   --

   

      
      procedure Initialize_Fields_For_Type_Pattern
        (Self : Bare_Type_Pattern
         ; Type_Pattern_F_Type_Name : Bare_Type_Ref
        ) is
      begin

            Self.Type_Pattern_F_Type_Name := Type_Pattern_F_Type_Name;
         

      end Initialize_Fields_For_Type_Pattern;

      
   function Type_Pattern_F_Type_Name
     (Node : Bare_Type_Pattern) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Type_Pattern_F_Type_Name;
      
   end;



   


      

   --
   --  Primitives for Bare_Pattern_Detail
   --

   




   


      

   --
   --  Primitives for Bare_Destructuring_Pattern_Detail
   --

   

      
      procedure Initialize_Fields_For_Destructuring_Pattern_Detail
        (Self : Bare_Destructuring_Pattern_Detail
         ; Destructuring_Pattern_Detail_F_Decl : Bare_Binding_Val_Decl
        ) is
      begin

            Self.Destructuring_Pattern_Detail_F_Decl := Destructuring_Pattern_Detail_F_Decl;
         

      end Initialize_Fields_For_Destructuring_Pattern_Detail;

      
   function Destructuring_Pattern_Detail_F_Decl
     (Node : Bare_Destructuring_Pattern_Detail) return Bare_Binding_Val_Decl
   is
      

   begin
         
         return Node.Destructuring_Pattern_Detail_F_Decl;
      
   end;



   


      

   --
   --  Primitives for Bare_Field_Pattern_Detail
   --

   

      
      procedure Initialize_Fields_For_Field_Pattern_Detail
        (Self : Bare_Field_Pattern_Detail
         ; Field_Pattern_Detail_F_Id : Bare_Id
         ; Field_Pattern_Detail_F_Expected_Value : Bare_Pattern
        ) is
      begin

            Self.Field_Pattern_Detail_F_Id := Field_Pattern_Detail_F_Id;
            Self.Field_Pattern_Detail_F_Expected_Value := Field_Pattern_Detail_F_Expected_Value;
         

      end Initialize_Fields_For_Field_Pattern_Detail;

      
   function Field_Pattern_Detail_F_Id
     (Node : Bare_Field_Pattern_Detail) return Bare_Id
   is
      

   begin
         
         return Node.Field_Pattern_Detail_F_Id;
      
   end;

      
   function Field_Pattern_Detail_F_Expected_Value
     (Node : Bare_Field_Pattern_Detail) return Bare_Pattern
   is
      

   begin
         
         return Node.Field_Pattern_Detail_F_Expected_Value;
      
   end;



   


      

   --
   --  Primitives for Bare_Property_Pattern_Detail
   --

   

      
      procedure Initialize_Fields_For_Property_Pattern_Detail
        (Self : Bare_Property_Pattern_Detail
         ; Property_Pattern_Detail_F_Call : Bare_Call_Expr
         ; Property_Pattern_Detail_F_Expected_Value : Bare_Pattern
        ) is
      begin

            Self.Property_Pattern_Detail_F_Call := Property_Pattern_Detail_F_Call;
            Self.Property_Pattern_Detail_F_Expected_Value := Property_Pattern_Detail_F_Expected_Value;
         

      end Initialize_Fields_For_Property_Pattern_Detail;

      
   function Property_Pattern_Detail_F_Call
     (Node : Bare_Property_Pattern_Detail) return Bare_Call_Expr
   is
      

   begin
         
         return Node.Property_Pattern_Detail_F_Call;
      
   end;

      
   function Property_Pattern_Detail_F_Expected_Value
     (Node : Bare_Property_Pattern_Detail) return Bare_Pattern
   is
      

   begin
         
         return Node.Property_Pattern_Detail_F_Expected_Value;
      
   end;



   


      

   --
   --  Primitives for Bare_Type_Ref
   --

   

      
      procedure Initialize_Fields_For_Type_Ref
        (Self : Bare_Type_Ref
        ) is
      begin

         
      Self.Type_Ref_F_Type_Var := Null_Var_Record;

      end Initialize_Fields_For_Type_Ref;



   


      

   --
   --  Primitives for Bare_Default_List_Type_Ref
   --

   

      
      procedure Initialize_Fields_For_Default_List_Type_Ref
        (Self : Bare_Default_List_Type_Ref
        ) is
      begin
            Initialize_Fields_For_Type_Ref
              (Self);

         

      end Initialize_Fields_For_Default_List_Type_Ref;



   


      

   --
   --  Primitives for Bare_Function_Type_Ref
   --

   

      
      procedure Initialize_Fields_For_Function_Type_Ref
        (Self : Bare_Function_Type_Ref
         ; Function_Type_Ref_F_Param_Types : Bare_Type_Ref_List
         ; Function_Type_Ref_F_Return_Type : Bare_Type_Ref
        ) is
      begin
            Initialize_Fields_For_Type_Ref
              (Self);

            Self.Function_Type_Ref_F_Param_Types := Function_Type_Ref_F_Param_Types;
            Self.Function_Type_Ref_F_Return_Type := Function_Type_Ref_F_Return_Type;
         

      end Initialize_Fields_For_Function_Type_Ref;

      
   function Function_Type_Ref_F_Param_Types
     (Node : Bare_Function_Type_Ref) return Bare_Type_Ref_List
   is
      

   begin
         
         return Node.Function_Type_Ref_F_Param_Types;
      
   end;

      
   function Function_Type_Ref_F_Return_Type
     (Node : Bare_Function_Type_Ref) return Bare_Type_Ref
   is
      

   begin
         
         return Node.Function_Type_Ref_F_Return_Type;
      
   end;



   


      

   --
   --  Primitives for Bare_Generic_Type_Ref
   --

   

      
      procedure Initialize_Fields_For_Generic_Type_Ref
        (Self : Bare_Generic_Type_Ref
         ; Generic_Type_Ref_F_Type_Name : Bare_Expr
         ; Generic_Type_Ref_F_Args : Bare_Type_Ref_List
        ) is
      begin
            Initialize_Fields_For_Type_Ref
              (Self);

            Self.Generic_Type_Ref_F_Type_Name := Generic_Type_Ref_F_Type_Name;
            Self.Generic_Type_Ref_F_Args := Generic_Type_Ref_F_Args;
         

      end Initialize_Fields_For_Generic_Type_Ref;

      
   function Generic_Type_Ref_F_Type_Name
     (Node : Bare_Generic_Type_Ref) return Bare_Expr
   is
      

   begin
         
         return Node.Generic_Type_Ref_F_Type_Name;
      
   end;

      
   function Generic_Type_Ref_F_Args
     (Node : Bare_Generic_Type_Ref) return Bare_Type_Ref_List
   is
      

   begin
         
         return Node.Generic_Type_Ref_F_Args;
      
   end;



   


      

   --
   --  Primitives for Bare_Simple_Type_Ref
   --

   

      
      procedure Initialize_Fields_For_Simple_Type_Ref
        (Self : Bare_Simple_Type_Ref
         ; Simple_Type_Ref_F_Type_Name : Bare_Expr
        ) is
      begin
            Initialize_Fields_For_Type_Ref
              (Self);

            Self.Simple_Type_Ref_F_Type_Name := Simple_Type_Ref_F_Type_Name;
         

      end Initialize_Fields_For_Simple_Type_Ref;

      
   function Simple_Type_Ref_F_Type_Name
     (Node : Bare_Simple_Type_Ref) return Bare_Expr
   is
      

   begin
         
         return Node.Simple_Type_Ref_F_Type_Name;
      
   end;



   


      

   --
   --  Primitives for Bare_Var_Bind
   --

   

      
      procedure Initialize_Fields_For_Var_Bind
        (Self : Bare_Var_Bind
         ; Var_Bind_F_Name : Bare_Ref_Id
         ; Var_Bind_F_Expr : Bare_Expr
        ) is
      begin

            Self.Var_Bind_F_Name := Var_Bind_F_Name;
            Self.Var_Bind_F_Expr := Var_Bind_F_Expr;
         

      end Initialize_Fields_For_Var_Bind;

      
   function Var_Bind_F_Name
     (Node : Bare_Var_Bind) return Bare_Ref_Id
   is
      

   begin
         
         return Node.Var_Bind_F_Name;
      
   end;

      
   function Var_Bind_F_Expr
     (Node : Bare_Var_Bind) return Bare_Expr
   is
      

   begin
         
         return Node.Var_Bind_F_Expr;
      
   end;



   



   ----------------------------
   -- Destroy_Synthetic_Node --
   ----------------------------

   procedure Destroy_Synthetic_Node (Node : in out Bare_Lkt_Node) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Root_Node_Record, Bare_Lkt_Node);
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

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image
        (Node       : Bare_Lkt_Node;
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

   Kind_Names : array (Lkt_Node_Kind_Type) of Unbounded_String :=
     (Lkt_Argument => To_Unbounded_String ("Argument"), 
Lkt_Import => To_Unbounded_String ("Import"), 
Lkt_Import_All_From => To_Unbounded_String ("ImportAllFrom"), 
Lkt_Import_From => To_Unbounded_String ("ImportFrom"), 
Lkt_Error_Lexer_Case_Rule_Alt => To_Unbounded_String ("ErrorLexerCaseRuleAlt"), 
Lkt_Lexer_Case_Rule_Cond_Alt => To_Unbounded_String ("LexerCaseRuleCondAlt"), 
Lkt_Lexer_Case_Rule_Default_Alt => To_Unbounded_String ("LexerCaseRuleDefaultAlt"), 
Lkt_Match_Branch => To_Unbounded_String ("MatchBranch"), 
Lkt_Pattern_Match_Branch => To_Unbounded_String ("PatternMatchBranch"), 
Lkt_Block_Expr_Clause => To_Unbounded_String ("BlockExprClause"), 
Lkt_Block_String_Line => To_Unbounded_String ("BlockStringLine"), 
Lkt_Class_Qualifier_Absent => To_Unbounded_String ("ClassQualifierAbsent"), 
Lkt_Class_Qualifier_Present => To_Unbounded_String ("ClassQualifierPresent"), 
Lkt_Grammar_Rule_Decl => To_Unbounded_String ("GrammarRuleDecl"), 
Lkt_Synthetic_Lexer_Decl => To_Unbounded_String ("SyntheticLexerDecl"), 
Lkt_Node_Decl => To_Unbounded_String ("NodeDecl"), 
Lkt_Self_Decl => To_Unbounded_String ("SelfDecl"), 
Lkt_Binding_Val_Decl => To_Unbounded_String ("BindingValDecl"), 
Lkt_Enum_Lit_Decl => To_Unbounded_String ("EnumLitDecl"), 
Lkt_Field_Decl => To_Unbounded_String ("FieldDecl"), 
Lkt_Fun_Param_Decl => To_Unbounded_String ("FunParamDecl"), 
Lkt_Lambda_Param_Decl => To_Unbounded_String ("LambdaParamDecl"), 
Lkt_Dyn_Var_Decl => To_Unbounded_String ("DynVarDecl"), 
Lkt_Match_Val_Decl => To_Unbounded_String ("MatchValDecl"), 
Lkt_Val_Decl => To_Unbounded_String ("ValDecl"), 
Lkt_Fun_Decl => To_Unbounded_String ("FunDecl"), 
Lkt_Env_Spec_Decl => To_Unbounded_String ("EnvSpecDecl"), 
Lkt_Error_Decl => To_Unbounded_String ("ErrorDecl"), 
Lkt_Generic_Decl => To_Unbounded_String ("GenericDecl"), 
Lkt_Grammar_Decl => To_Unbounded_String ("GrammarDecl"), 
Lkt_Langkit_Root => To_Unbounded_String ("LangkitRoot"), 
Lkt_Lexer_Decl => To_Unbounded_String ("LexerDecl"), 
Lkt_Lexer_Family_Decl => To_Unbounded_String ("LexerFamilyDecl"), 
Lkt_Synth_Fun_Decl => To_Unbounded_String ("SynthFunDecl"), 
Lkt_Synth_Param_Decl => To_Unbounded_String ("SynthParamDecl"), 
Lkt_Any_Type_Decl => To_Unbounded_String ("AnyTypeDecl"), 
Lkt_Enum_Class_Alt_Decl => To_Unbounded_String ("EnumClassAltDecl"), 
Lkt_Function_Type => To_Unbounded_String ("FunctionType"), 
Lkt_Generic_Param_Type_Decl => To_Unbounded_String ("GenericParamTypeDecl"), 
Lkt_Class_Decl => To_Unbounded_String ("ClassDecl"), 
Lkt_Enum_Class_Decl => To_Unbounded_String ("EnumClassDecl"), 
Lkt_Enum_Type_Decl => To_Unbounded_String ("EnumTypeDecl"), 
Lkt_Struct_Decl => To_Unbounded_String ("StructDecl"), 
Lkt_Trait_Decl => To_Unbounded_String ("TraitDecl"), 
Lkt_Decl_Annotation => To_Unbounded_String ("DeclAnnotation"), 
Lkt_Decl_Annotation_Args => To_Unbounded_String ("DeclAnnotationArgs"), 
Lkt_Dyn_Env_Wrapper => To_Unbounded_String ("DynEnvWrapper"), 
Lkt_Elsif_Branch => To_Unbounded_String ("ElsifBranch"), 
Lkt_Enum_Class_Case => To_Unbounded_String ("EnumClassCase"), 
Lkt_Excludes_Null_Absent => To_Unbounded_String ("ExcludesNullAbsent"), 
Lkt_Excludes_Null_Present => To_Unbounded_String ("ExcludesNullPresent"), 
Lkt_Any_Of => To_Unbounded_String ("AnyOf"), 
Lkt_Array_Literal => To_Unbounded_String ("ArrayLiteral"), 
Lkt_Call_Expr => To_Unbounded_String ("CallExpr"), 
Lkt_Logic_Predicate => To_Unbounded_String ("LogicPredicate"), 
Lkt_Logic_Propagate_Call => To_Unbounded_String ("LogicPropagateCall"), 
Lkt_Bin_Op => To_Unbounded_String ("BinOp"), 
Lkt_Block_Expr => To_Unbounded_String ("BlockExpr"), 
Lkt_Cast_Expr => To_Unbounded_String ("CastExpr"), 
Lkt_Dot_Expr => To_Unbounded_String ("DotExpr"), 
Lkt_Error_On_Null => To_Unbounded_String ("ErrorOnNull"), 
Lkt_Generic_Instantiation => To_Unbounded_String ("GenericInstantiation"), 
Lkt_Error_Grammar_Expr => To_Unbounded_String ("ErrorGrammarExpr"), 
Lkt_Grammar_Cut => To_Unbounded_String ("GrammarCut"), 
Lkt_Grammar_Discard => To_Unbounded_String ("GrammarDiscard"), 
Lkt_Grammar_Dont_Skip => To_Unbounded_String ("GrammarDontSkip"), 
Lkt_Grammar_List => To_Unbounded_String ("GrammarList"), 
Lkt_Grammar_Null => To_Unbounded_String ("GrammarNull"), 
Lkt_Grammar_Opt => To_Unbounded_String ("GrammarOpt"), 
Lkt_Grammar_Opt_Error => To_Unbounded_String ("GrammarOptError"), 
Lkt_Grammar_Opt_Error_Group => To_Unbounded_String ("GrammarOptErrorGroup"), 
Lkt_Grammar_Opt_Group => To_Unbounded_String ("GrammarOptGroup"), 
Lkt_Grammar_Or_Expr => To_Unbounded_String ("GrammarOrExpr"), 
Lkt_Grammar_Pick => To_Unbounded_String ("GrammarPick"), 
Lkt_Grammar_Implicit_Pick => To_Unbounded_String ("GrammarImplicitPick"), 
Lkt_Grammar_Predicate => To_Unbounded_String ("GrammarPredicate"), 
Lkt_Grammar_Rule_Ref => To_Unbounded_String ("GrammarRuleRef"), 
Lkt_Grammar_Skip => To_Unbounded_String ("GrammarSkip"), 
Lkt_Grammar_Stop_Cut => To_Unbounded_String ("GrammarStopCut"), 
Lkt_Parse_Node_Expr => To_Unbounded_String ("ParseNodeExpr"), 
Lkt_Token_Lit => To_Unbounded_String ("TokenLit"), 
Lkt_Token_No_Case_Lit => To_Unbounded_String ("TokenNoCaseLit"), 
Lkt_Token_Pattern_Concat => To_Unbounded_String ("TokenPatternConcat"), 
Lkt_Token_Pattern_Lit => To_Unbounded_String ("TokenPatternLit"), 
Lkt_Token_Ref => To_Unbounded_String ("TokenRef"), 
Lkt_Id => To_Unbounded_String ("Id"), 
Lkt_Def_Id => To_Unbounded_String ("DefId"), 
Lkt_Imported_Id => To_Unbounded_String ("ImportedId"), 
Lkt_Module_Id => To_Unbounded_String ("ModuleId"), 
Lkt_Ref_Id => To_Unbounded_String ("RefId"), 
Lkt_If_Expr => To_Unbounded_String ("IfExpr"), 
Lkt_Isa => To_Unbounded_String ("Isa"), 
Lkt_Keep_Expr => To_Unbounded_String ("KeepExpr"), 
Lkt_Lambda_Expr => To_Unbounded_String ("LambdaExpr"), 
Lkt_Big_Num_Lit => To_Unbounded_String ("BigNumLit"), 
Lkt_Char_Lit => To_Unbounded_String ("CharLit"), 
Lkt_Null_Lit => To_Unbounded_String ("NullLit"), 
Lkt_Num_Lit => To_Unbounded_String ("NumLit"), 
Lkt_Block_String_Lit => To_Unbounded_String ("BlockStringLit"), 
Lkt_Module_Doc_String_Lit => To_Unbounded_String ("ModuleDocStringLit"), 
Lkt_Single_Line_String_Lit => To_Unbounded_String ("SingleLineStringLit"), 
Lkt_Pattern_Single_Line_String_Lit => To_Unbounded_String ("PatternSingleLineStringLit"), 
Lkt_Logic_Assign => To_Unbounded_String ("LogicAssign"), 
Lkt_Logic_Expr => To_Unbounded_String ("LogicExpr"), 
Lkt_Logic_Propagate => To_Unbounded_String ("LogicPropagate"), 
Lkt_Logic_Unify => To_Unbounded_String ("LogicUnify"), 
Lkt_Match_Expr => To_Unbounded_String ("MatchExpr"), 
Lkt_Not_Expr => To_Unbounded_String ("NotExpr"), 
Lkt_Paren_Expr => To_Unbounded_String ("ParenExpr"), 
Lkt_Query => To_Unbounded_String ("Query"), 
Lkt_Raise_Expr => To_Unbounded_String ("RaiseExpr"), 
Lkt_Subscript_Expr => To_Unbounded_String ("SubscriptExpr"), 
Lkt_Try_Expr => To_Unbounded_String ("TryExpr"), 
Lkt_Un_Op => To_Unbounded_String ("UnOp"), 
Lkt_Full_Decl => To_Unbounded_String ("FullDecl"), 
Lkt_Grammar_List_Sep => To_Unbounded_String ("GrammarListSep"), 
Lkt_Imported_Name => To_Unbounded_String ("ImportedName"), 
Lkt_Lexer_Case_Rule => To_Unbounded_String ("LexerCaseRule"), 
Lkt_Lexer_Case_Rule_Send => To_Unbounded_String ("LexerCaseRuleSend"), 
Lkt_List_Kind_One => To_Unbounded_String ("ListKindOne"), 
Lkt_List_Kind_Zero => To_Unbounded_String ("ListKindZero"), 
Lkt_Argument_List => To_Unbounded_String ("ArgumentList"), 
Lkt_Base_Import_List => To_Unbounded_String ("BaseImportList"), 
Lkt_Base_Lexer_Case_Rule_Alt_List => To_Unbounded_String ("BaseLexerCaseRuleAltList"), 
Lkt_Base_Match_Branch_List => To_Unbounded_String ("BaseMatchBranchList"), 
Lkt_Block_String_Line_List => To_Unbounded_String ("BlockStringLineList"), 
Lkt_Call_Expr_List => To_Unbounded_String ("CallExprList"), 
Lkt_Decl_Annotation_List => To_Unbounded_String ("DeclAnnotationList"), 
Lkt_Elsif_Branch_List => To_Unbounded_String ("ElsifBranchList"), 
Lkt_Enum_Class_Alt_Decl_List => To_Unbounded_String ("EnumClassAltDeclList"), 
Lkt_Enum_Class_Case_List => To_Unbounded_String ("EnumClassCaseList"), 
Lkt_Enum_Lit_Decl_List => To_Unbounded_String ("EnumLitDeclList"), 
Lkt_Expr_List => To_Unbounded_String ("ExprList"), 
Lkt_Any_Of_List => To_Unbounded_String ("AnyOfList"), 
Lkt_Full_Decl_List => To_Unbounded_String ("FullDeclList"), 
Lkt_Decl_Block => To_Unbounded_String ("DeclBlock"), 
Lkt_Generic_Param_Decl_List => To_Unbounded_String ("GenericParamDeclList"), 
Lkt_Fun_Param_Decl_List => To_Unbounded_String ("FunParamDeclList"), 
Lkt_Grammar_Expr_List => To_Unbounded_String ("GrammarExprList"), 
Lkt_Grammar_Expr_List_List => To_Unbounded_String ("GrammarExprListList"), 
Lkt_Imported_Name_List => To_Unbounded_String ("ImportedNameList"), 
Lkt_Lambda_Param_Decl_List => To_Unbounded_String ("LambdaParamDeclList"), 
Lkt_Lkt_Node_List => To_Unbounded_String ("LktNodeList"), 
Lkt_Module_Doc_String_Line_List => To_Unbounded_String ("ModuleDocStringLineList"), 
Lkt_Pattern_Detail_List => To_Unbounded_String ("PatternDetailList"), 
Lkt_Pattern_List => To_Unbounded_String ("PatternList"), 
Lkt_Ref_Id_List => To_Unbounded_String ("RefIdList"), 
Lkt_Type_Ref_List => To_Unbounded_String ("TypeRefList"), 
Lkt_Synthetic_Type_Ref_List => To_Unbounded_String ("SyntheticTypeRefList"), 
Lkt_Module_Doc_String_Line => To_Unbounded_String ("ModuleDocStringLine"), 
Lkt_Null_Cond_Qualifier_Absent => To_Unbounded_String ("NullCondQualifierAbsent"), 
Lkt_Null_Cond_Qualifier_Present => To_Unbounded_String ("NullCondQualifierPresent"), 
Lkt_Op_Amp => To_Unbounded_String ("OpAmp"), 
Lkt_Op_And => To_Unbounded_String ("OpAnd"), 
Lkt_Op_Div => To_Unbounded_String ("OpDiv"), 
Lkt_Op_Eq => To_Unbounded_String ("OpEq"), 
Lkt_Op_Gt => To_Unbounded_String ("OpGt"), 
Lkt_Op_Gte => To_Unbounded_String ("OpGte"), 
Lkt_Op_Logic_And => To_Unbounded_String ("OpLogicAnd"), 
Lkt_Op_Logic_Or => To_Unbounded_String ("OpLogicOr"), 
Lkt_Op_Lt => To_Unbounded_String ("OpLt"), 
Lkt_Op_Lte => To_Unbounded_String ("OpLte"), 
Lkt_Op_Minus => To_Unbounded_String ("OpMinus"), 
Lkt_Op_Mult => To_Unbounded_String ("OpMult"), 
Lkt_Op_Ne => To_Unbounded_String ("OpNe"), 
Lkt_Op_Or => To_Unbounded_String ("OpOr"), 
Lkt_Op_Or_Int => To_Unbounded_String ("OpOrInt"), 
Lkt_Op_Plus => To_Unbounded_String ("OpPlus"), 
Lkt_Op_Stream_Concat => To_Unbounded_String ("OpStreamConcat"), 
Lkt_Op_Stream_Cons => To_Unbounded_String ("OpStreamCons"), 
Lkt_Any_Type_Pattern => To_Unbounded_String ("AnyTypePattern"), 
Lkt_Bool_Pattern_False => To_Unbounded_String ("BoolPatternFalse"), 
Lkt_Bool_Pattern_True => To_Unbounded_String ("BoolPatternTrue"), 
Lkt_Complex_Pattern => To_Unbounded_String ("ComplexPattern"), 
Lkt_Renaming_Complex_Pattern => To_Unbounded_String ("RenamingComplexPattern"), 
Lkt_Ellipsis_Pattern => To_Unbounded_String ("EllipsisPattern"), 
Lkt_Integer_Pattern => To_Unbounded_String ("IntegerPattern"), 
Lkt_List_Pattern => To_Unbounded_String ("ListPattern"), 
Lkt_Not_Pattern => To_Unbounded_String ("NotPattern"), 
Lkt_Null_Pattern => To_Unbounded_String ("NullPattern"), 
Lkt_Or_Pattern => To_Unbounded_String ("OrPattern"), 
Lkt_Paren_Pattern => To_Unbounded_String ("ParenPattern"), 
Lkt_Regex_Pattern => To_Unbounded_String ("RegexPattern"), 
Lkt_Type_Pattern => To_Unbounded_String ("TypePattern"), 
Lkt_Destructuring_Pattern_Detail => To_Unbounded_String ("DestructuringPatternDetail"), 
Lkt_Field_Pattern_Detail => To_Unbounded_String ("FieldPatternDetail"), 
Lkt_Property_Pattern_Detail => To_Unbounded_String ("PropertyPatternDetail"), 
Lkt_Default_List_Type_Ref => To_Unbounded_String ("DefaultListTypeRef"), 
Lkt_Function_Type_Ref => To_Unbounded_String ("FunctionTypeRef"), 
Lkt_Generic_Type_Ref => To_Unbounded_String ("GenericTypeRef"), 
Lkt_Simple_Type_Ref => To_Unbounded_String ("SimpleTypeRef"), 
Lkt_Var_Bind => To_Unbounded_String ("VarBind"));

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : Bare_Lkt_Node) return String is
   begin
      return To_String (Kind_Names (Node.Kind));
   end Kind_Name;

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count (Node : Bare_Lkt_Node) return Natural is
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

   procedure Free_User_Fields (Node : Bare_Lkt_Node) is

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

      K : constant Lkt_Node_Kind_Type := Node.Kind;

   begin
      
      case Lkt_Lkt_Node (K) is
when Lkt_Lambda_Param_Decl_Range =>
declare
N_Bare_Lambda_Param_Decl : constant Bare_Lambda_Param_Decl := Node;
begin
Reset_Logic_Var (N_Bare_Lambda_Param_Decl.Lambda_Param_Decl_F_Type_Var);
end;
when Lkt_Synth_Fun_Decl_Range =>
declare
N_Bare_Synth_Fun_Decl : constant Bare_Synth_Fun_Decl := Node;
begin
Dec_Ref (N_Bare_Synth_Fun_Decl.Synth_Fun_Decl_F_Params);
end;
when Lkt_Function_Type_Range =>
declare
N_Bare_Function_Type : constant Bare_Function_Type := Node;
begin
Dec_Ref (N_Bare_Function_Type.Function_Type_F_Params);
end;
when Lkt_Dyn_Env_Wrapper_Range =>
declare
N_Bare_Dyn_Env_Wrapper : constant Bare_Dyn_Env_Wrapper := Node;
begin
Dec_Ref (N_Bare_Dyn_Env_Wrapper.Dyn_Env_Wrapper_F_Names);
Dec_Ref (N_Bare_Dyn_Env_Wrapper.Dyn_Env_Wrapper_F_Types);
Dec_Ref (N_Bare_Dyn_Env_Wrapper.Internal_Bare_Dyn_Env_Wrapper_Lf_Stg_Dynenvwrapper_Instantiation_Env_30);
end;
when Lkt_Expr =>
declare
N_Bare_Expr : constant Bare_Expr := Node;
begin
Reset_Logic_Var (N_Bare_Expr.Expr_F_Expected_Type_Var);
Reset_Logic_Var (N_Bare_Expr.Expr_F_Actual_Type_Var);
Reset_Logic_Var (N_Bare_Expr.Expr_F_Generic_Func_Type_Var);
case Lkt_Expr (K) is
when Lkt_Array_Literal_Range =>
declare
N_Bare_Array_Literal : constant Bare_Array_Literal := N_Bare_Expr;
begin
Reset_Logic_Var (N_Bare_Array_Literal.Array_Literal_F_Expected_Exprs_Type_Var);
Reset_Logic_Var (N_Bare_Array_Literal.Array_Literal_F_Actual_Element_Type);
end;
when Lkt_Generic_Instantiation_Range =>
declare
N_Bare_Generic_Instantiation : constant Bare_Generic_Instantiation := N_Bare_Expr;
begin
Reset_Logic_Var (N_Bare_Generic_Instantiation.Generic_Instantiation_F_Rebinded_Var);
end;
when Lkt_Ref_Id_Range =>
declare
N_Bare_Ref_Id : constant Bare_Ref_Id := N_Bare_Expr;
begin
Reset_Logic_Var (N_Bare_Ref_Id.Ref_Id_F_Ref_Var);
end;
when Lkt_If_Expr_Range =>
declare
N_Bare_If_Expr : constant Bare_If_Expr := N_Bare_Expr;
begin
Reset_Logic_Var (N_Bare_If_Expr.If_Expr_F_Expected_Branch_Type_Var);
end;
when Lkt_Keep_Expr_Range =>
declare
N_Bare_Keep_Expr : constant Bare_Keep_Expr := N_Bare_Expr;
begin
Reset_Logic_Var (N_Bare_Keep_Expr.Keep_Expr_F_Array_Element_Type);
end;
when Lkt_Match_Expr_Range =>
declare
N_Bare_Match_Expr : constant Bare_Match_Expr := N_Bare_Expr;
begin
Reset_Logic_Var (N_Bare_Match_Expr.Match_Expr_F_Expected_Branch_Type_Var);
end;
when Lkt_Try_Expr_Range =>
declare
N_Bare_Try_Expr : constant Bare_Try_Expr := N_Bare_Expr;
begin
Reset_Logic_Var (N_Bare_Try_Expr.Try_Expr_F_Expected_Expr_Type_Var);
end;
when others => null;
end case;
end;
when Lkt_Type_Ref =>
declare
N_Bare_Type_Ref : constant Bare_Type_Ref := Node;
begin
Reset_Logic_Var (N_Bare_Type_Ref.Type_Ref_F_Type_Var);
end;
when others => null;
end case;
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
            Create_Symbol (Symbol)
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
         Memoization_Map            => <>,
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
            Rule                => Main_Rule_Rule);
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
      return Liblktlang_Support.Internal.Analysis.Normalized_Unit_Filename
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
     (Unit : Internal_Unit; Node : Bare_Lkt_Node)
   is
      procedure Helper is new Register_Destroyable_Gen
        (Root_Node_Record,
         Bare_Lkt_Node,
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

      procedure Reset_Refd_Envs (Node : Bare_Lkt_Node);

      -------------------------
      -- Recompute_Refd_Envs --
      -------------------------

      procedure Reset_Refd_Envs (Node : Bare_Lkt_Node) is
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
            Destroy (Unit.Memoization_Map);
      end if;
   end Reset_Caches;

   ----------------
   -- Do_Parsing --
   ----------------

   procedure Do_Parsing
     (Unit   : Internal_Unit;
      Input  : Liblktlang_Support.Internal.Analysis.Lexer_Input;
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
         Ast_Root     => Liblktlang_Support.Internal.Analysis.No_Internal_Node);

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
            Ast_Root : constant Bare_Lkt_Node :=
              Bare_Lkt_Node
                (Parse (Unit.Context.Parser, Rule => Unit.Rule));
            function "+" is new Ada.Unchecked_Conversion
              (Bare_Lkt_Node,
               Liblktlang_Support.Internal.Analysis.Internal_Node);
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
           (Liblktlang_Support.Internal.Analysis.Internal_Node,
            Bare_Lkt_Node);
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

      --  Update all the lexical envs entries affected by the reparse

      declare
         Unit_Name     : constant String := +Unit.Filename.Base_Name;
         Context       : constant Internal_Context := Unit.Context;
         Foreign_Nodes : Bare_Lkt_Node_Vectors.Vector :=
           Bare_Lkt_Node_Vectors.Empty_Vector;

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
               if At_Least_One_Root_Populated then
                  Populate_Lexical_Env (Unit);
               end if;
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
            Node : constant Bare_Lkt_Node := Key (Cur);
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
      Foreign_Nodes : in out Bare_Lkt_Node_Vectors.Vector) is
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

   procedure Reroot_Foreign_Node (Node : Bare_Lkt_Node) is
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

   function Text (Node : Bare_Lkt_Node) return String_Type is
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
     (Self    : Bare_Lkt_Node;
      Context : Internal_Context;
      S       : String_Type) return Symbol_Type is
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
end Liblktlang.Implementation;
