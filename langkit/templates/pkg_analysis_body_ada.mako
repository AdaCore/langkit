## vim: filetype=makoada

<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="exts"          file="extensions.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />
<%namespace name="public_properties"
            file="properties/public_wrappers_ada.mako" />

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

with ${ada_lib_name}.Converters; use ${ada_lib_name}.Converters;
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
   -- Disable_Lookup_Cache --
   --------------------------

   procedure Disable_Lookup_Cache (Disable : Boolean := True) is
   begin
      Implementation.AST_Envs.Activate_Lookup_Cache := not Disable;
   end Disable_Lookup_Cache;

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
      return Wrap_Node (Root (Internal_Unit (Unit)));
   end Root;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Analysis_Unit) return Token_Reference is
   begin
      return First_Token (Internal_Unit (Unit));
   end First_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Analysis_Unit) return Token_Reference is
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
     (Unit : Analysis_Unit; Sloc : Source_Location) return Token_Reference is
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

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : ${root_entity.api_name}'Class) return Boolean is
     (Node.Internal.El = null);

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : ${root_entity.api_name}'Class) return Boolean
   is (Node.Internal.El.Is_Token_Node);

   ---------
   -- "=" --
   ---------

   function "=" (L, R : ${root_entity.api_name}'Class) return Boolean is
   begin
      return L.Internal = R.Internal;
   end "=";

   -----------------
   -- Short_Image --
   -----------------

   function Short_Image
     (Node : ${root_entity.api_name}'Class) return Text_Type
   is (if Node.Is_Null then "None" else Node.Internal.El.Short_Image);

   function Short_Image (Node : ${root_entity.api_name}'Class) return String is
     (Image (Node.Short_Image));

   -----------
   -- Image --
   -----------

   function Image (Node : ${root_entity.api_name}'Class) return Text_Type is
     (Image (Node.Internal));

   -----------
   -- Image --
   -----------

   function Image (Node : ${root_entity.api_name}'Class) return String is
     (Image (Node.Image));

   ----------
   -- Hash --
   ----------

   function Hash
     (Node : ${root_entity.api_name}'Class) return Ada.Containers.Hash_Type is
   begin
      return Hash (Node.Internal);
   end Hash;

   -----------------------
   -- Entity converters --
   -----------------------

   % for e in ctx.entity_types:
      function As_${e.el_type.kwless_raw_name}
        (Node : ${root_entity.api_name}'Class) return ${e.api_name}
      is
         N : constant ${root_node_type_name} := Node.Internal.El;
      begin
         if N = null then
            return No_${e.api_name};
         elsif N.all in ${e.el_type.value_type_name()}'Class then
            return (Internal => (El => N, Info => Node.Internal.Info));
         else
            raise Constraint_Error with "Invalid type conversion";
         end if;
      end;
   % endfor

   -----------------------
   -- Entity primitives --
   -----------------------

   ----------
   -- Kind --
   ----------

   function Kind
     (Node : ${root_entity.api_name}'Class) return ${root_node_kind_name} is
   begin
      return Node.Internal.El.Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : ${root_entity.api_name}'Class) return String is
   begin
      return Node.Internal.El.Kind_Name;
   end Kind_Name;

   % for e in ctx.entity_types:

      % for f in e.el_type.get_parse_fields( \
         include_inherited=False, \
         predicate=lambda f: f.is_public \
      ):
         ${astnode_types.field_body(f)}
      % endfor

      % for p in e.el_type.get_properties( \
         include_inherited=False, \
         predicate=lambda p: p.is_public and not p.overriding \
      ):
         ${public_properties.body(p)}
      % endfor

   % endfor

   --------------------
   -- Children_Count --
   --------------------

   function Children_Count
     (Node : ${root_entity.api_name}'Class) return Natural is
   begin
      return Node.Internal.El.Abstract_Children_Count;
   end Children_Count;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural is
   begin
      return Node.Internal.El.First_Child_Index;
   end First_Child_Index;

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural is
   begin
      return Node.Internal.El.Last_Child_Index;
   end Last_Child_Index;

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node            : ${root_entity.api_name}'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${root_entity.api_name})
   is
      N : ${root_node_type_name};
   begin
      Node.Internal.El.Get_Child (Index, Index_In_Bounds, N);
      Result := Wrap_Node (N, Node.Internal.Info);
   end Get_Child;

   -----------
   -- Child --
   -----------

   function Child
     (Node  : ${root_entity.api_name}'Class;
      Index : Positive) return ${root_entity.api_name}
   is
   begin
      return Wrap_Node (Node.Internal.El.Child (Index), Node.Internal.Info);
   end Child;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : ${root_entity.api_name}'Class) return Source_Location_Range is
   begin
      return Node.Internal.El.Sloc_Range;
   end Sloc_Range;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return Relative_Position is
   begin
      return Node.Internal.El.Compare (Sloc);
   end Compare;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return ${root_entity.api_name} is
   begin
      return Wrap_Node (Node.Internal.El.Lookup (Sloc));
   end Lookup;

   ----------
   -- Text --
   ----------

   function Text (Node : ${root_entity.api_name}'Class) return Text_Type is
   begin
      return Text (Node.Token_Start, Node.Token_End);
   end Text;

   ----------
   -- Text --
   ----------

   function Text (Node : ${root_entity.api_name}'Class) return String is
   begin
      return Image (Node.Text);
   end Text;

   -----------------
   -- Token_Range --
   -----------------

   function Token_Range
     (Node : ${root_entity.api_name}'Class) return Token_Iterator is
   begin
      return Token_Iterator'(Node.As_${T.root_node.kwless_raw_name},
                             Node.Internal.El.Token_End_Index);
   end Token_Range;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : ${root_entity.api_name}'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "") is
   begin
      Node.Internal.El.Print (Show_Slocs, Line_Prefix);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node : ${root_entity.api_name}'Class; Line_Prefix : String := "") is
   begin
      Node.Internal.El.PP_Trivia (Line_Prefix);
   end PP_Trivia;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : ${root_entity.api_name}'Class;
      Visit : access function (Node : ${root_entity.api_name}'Class)
              return Visit_Status)
      return Visit_Status
   is
      Info : constant Entity_Info := Node.Internal.Info;

      -------------
      -- Wrapper --
      -------------

      function Wrapper
        (Node : access ${root_node_value_type}'Class) return Visit_Status
      is
         Public_Node : constant ${root_entity.api_name} :=
           Wrap_Node (${root_node_type_name} (Node), Info);
      begin
         return Visit (Public_Node);
      end Wrapper;

   begin
      return Node.Internal.El.Traverse (Wrapper'Access);
   end Traverse;

   --------------
   -- Traverse --
   --------------

   procedure Traverse
     (Node  : ${root_entity.api_name}'Class;
      Visit : access function (Node : ${root_entity.api_name}'Class)
                               return Visit_Status)
   is
      Result_Status : Visit_Status;
      pragma Unreferenced (Result_Status);
   begin
      Result_Status := Traverse (Node, Visit);
   end Traverse;

   -----------------
   -- Child_Index --
   -----------------

   function Child_Index (Node : ${root_entity.api_name}'Class) return Natural
   is
   begin
      return Node.Internal.El.Child_Index;
   end Child_Index;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : ${root_entity.api_name}'Class)
   is
   begin
      Assign_Names_To_Logic_Vars (Node.Internal.El);
   end Assign_Names_To_Logic_Vars;

   --------------------------
   -- Children_With_Trivia --
   --------------------------

   function Children_With_Trivia
     (Node : ${root_entity.api_name}'Class) return Children_Array
   is
      Bare_Result : constant Bare_Children_Array :=
         Children_With_Trivia (Unwrap_Node (Node));
      Result      : Children_Array (Bare_Result'Range);
   begin
      for I in Bare_Result'Range loop
         declare
            BR : Bare_Child_Record renames Bare_Result (I);
            R  : Child_Record renames Result (I);
         begin
            case BR.Kind is
               when Child =>
                  R := (Child, Wrap_Node (BR.Node));
               when Trivia =>
                  R := (Trivia, BR.Trivia);
            end case;
         end;
      end loop;
      return Result;
   end Children_With_Trivia;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Self : Token_Iterator) return Token_Reference
   is (Token_Start (Self.Node));

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference
   is (Next (Tok));

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean
   is (Get_Token_Index (Tok).Token <= Self.Last);

   -------------
   -- Element --
   -------------

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference
   is (Tok);

   ----------------------------------------------------
   -- Soft links for public/internal type converters --
   ----------------------------------------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context
   is (Analysis_Context (Context));

   function Unwrap_Context (Context : Analysis_Context) return Internal_Context
   is (Internal_Context (Context));

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit
   is (Analysis_Unit (Unit));

   function Unwrap_Unit (Unit : Analysis_Unit) return Internal_Unit
   is (Internal_Unit (Unit));

   function Wrap_Node
     (Node : access ${root_node_value_type}'Class;
      Info : AST_Envs.Entity_Info := AST_Envs.No_Entity_Info)
      return ${root_entity.api_name}
   is ((Internal => (Node, Info)));

   function Unwrap_Node
     (Node : ${root_entity.api_name}'Class) return ${root_node_type_name}
   is (Node.Internal.El);

begin
   Converters.Wrap_Context := Wrap_Context'Access;
   Converters.Unwrap_Context := Unwrap_Context'Access;
   Converters.Wrap_Unit := Wrap_Unit'Access;
   Converters.Unwrap_Unit := Unwrap_Unit'Access;
   Converters.Wrap_Node := Wrap_Node'Access;
   Converters.Unwrap_Node := Unwrap_Node'Access;
end ${ada_lib_name}.Analysis;
