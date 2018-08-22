## vim: filetype=makoada

<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="exts"          file="extensions.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />
<%namespace name="public_properties"
            file="properties/public_wrappers_ada.mako" />

<% root_node_array = T.root_node.array %>

with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Conversion;

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
      Internal : Unit_Provider_Reference;
   end record;

   type Unit_Provider_Wrapper_Access is access all Unit_Provider_Wrapper;

   overriding function Get_Unit_Filename
     (Provider : Unit_Provider_Wrapper;
      Name     : Text_Type;
      Kind     : Unit_Kind) return String;
   overriding function Get_Unit
     (Provider    : Unit_Provider_Wrapper;
      Context     : Internal_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Internal_Unit;

   % for cls in ctx.array_types:
      ${array_types.ada_api_converters_decl(cls)}
   % endfor

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Unit_Provider_Wrapper;
      Name     : Text_Type;
      Kind     : Unit_Kind) return String
   is (Provider.Internal.Get.Get_Unit_Filename (Name, Kind));

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Unit_Provider_Wrapper;
      Context     : Internal_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Internal_Unit
   is
      Ctx : constant Analysis_Context := Wrap_Context (Context);
   begin
      return Unwrap_Unit (Provider.Internal.Get.Get_Unit
        (Ctx, Name, Kind, Charset, Reparse));
   end Get_Unit;

   ----------------
   -- Do_Release --
   ----------------

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class) is
   begin
      Provider.Release;
   end Do_Release;

   ------------------------------------
   -- Create_Unit_Provider_Reference --
   ------------------------------------

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference
   is
   begin
      return Result : Unit_Provider_Reference do
         Result.Set (Provider);
      end return;
   end Create_Unit_Provider_Reference;

   --------------------
   -- Create_Context --
   --------------------

   function Create_Context
     (Charset       : String := Default_Charset;
      With_Trivia   : Boolean := True;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference)
      return Analysis_Context
   is
      use Unit_Provider_References;

      Provider : Unit_Provider_Reference := Unit_Provider;
      Result   : Internal_Context;
   begin
      % if ctx.default_unit_provider:
         if Provider = No_Unit_Provider_Reference then
            Provider.Set (${ctx.default_unit_provider.fqn});
         end if;
      % endif

      declare
         Provider_Wrapper : constant Unit_Provider_Wrapper_Access :=
            new Unit_Provider_Wrapper'(Internal => Provider);
      begin
         Result := Create_Context
           (Charset, With_Trivia,
            Internal_Unit_Provider_Access (Provider_Wrapper));
      end;

      return Context : constant Analysis_Context := Wrap_Context (Result)
      do
         --  Result has one ownership share and the call to Wrap_Context
         --  creates a new one, so don't forget to dec-ref before returning.
         Dec_Ref (Result);
      end return;
   end Create_Context;

   --------------
   -- Has_Unit --
   --------------

   function Has_Unit
     (Context       : Analysis_Context'Class;
      Unit_Filename : String) return Boolean is
   begin
      return Has_Unit (Unwrap_Context (Context), Unit_Filename);
   end Has_Unit;

   -------------------
   -- Get_From_File --
   -------------------

   function Get_From_File
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      return Wrap_Unit
        (Get_From_File (Unwrap_Context (Context), Filename, Charset,
                        Reparse, Rule));
   end Get_From_File;

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit is
   begin
      return Wrap_Unit
        (Get_From_Buffer (Unwrap_Context (Context), Filename, Charset,
                          Buffer, Rule));
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Analysis_Context'Class;
      Filename : String;
      Error    : String;
      Charset  : String := "";
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Result : constant Internal_Unit :=
         Implementation.Get_With_Error (Unwrap_Context (Context), Filename,
                                        Error, Charset, Rule);
   begin
      return Wrap_Unit (Result);
   end Get_With_Error;

   % if ctx.default_unit_provider:

   -----------------------
   -- Get_From_Provider --
   -----------------------

   function Get_From_Provider
     (Context : Analysis_Context'Class;
      Name    : Text_Type;
      Kind    : Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit is
   begin
      return Wrap_Unit
        (Get_From_Provider (Unwrap_Context (Context), Name, Kind,
                            Charset, Reparse));
   end Get_From_Provider;

   % endif

   -------------------
   -- Unit_Provider --
   -------------------

   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference
   is
      Provider : constant Internal_Unit_Provider_Access :=
         Unit_Provider (Unwrap_Context (Context));
   begin
      --  By design, Unit_Provider_Wrapper is supposed to be the only
      --  implementation of the Internal_Unit_Provider interface.
      if Provider.all not in Unit_Provider_Wrapper'Class then
         raise Program_Error;
      end if;

      return Unit_Provider_Wrapper (Provider.all).Internal;
   end Unit_Provider;

   ----------
   -- Hash --
   ----------

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Context (Context));
   end Hash;

   ---------------------
   -- Has_With_Trivia --
   ---------------------

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean
   is
   begin
      return Has_With_Trivia (Unwrap_Context (Context));
   end Has_With_Trivia;

   --------------------------------------------
   -- Discard_Errors_In_Populate_Lexical_Env --
   --------------------------------------------

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean) is
   begin
      Discard_Errors_In_Populate_Lexical_Env
        (Unwrap_Context (Context), Discard);
   end Discard_Errors_In_Populate_Lexical_Env;

   ----------------------------------
   -- Set_Logic_Resolution_Timeout --
   ----------------------------------

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural) is
   begin
      Set_Logic_Resolution_Timeout (Unwrap_Context (Context), Timeout);
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

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean is
   begin
      return Has_Rewriting_Handle (Unwrap_Context (Context));
   end Has_Rewriting_Handle;

   -------------
   -- Context --
   -------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context is
   begin
      return Wrap_Context (Context (Unwrap_Unit (Unit)));
   end Context;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type
   is
   begin
      return Hash (Unwrap_Unit (Unit));
   end Hash;

   -------------
   -- Reparse --
   -------------

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "") is
   begin
      Reparse (Unwrap_Unit (Unit), Charset);
   end Reparse;

   -------------
   -- Reparse --
   -------------

   procedure Reparse
     (Unit : Analysis_Unit'Class; Charset : String := ""; Buffer  : String) is
   begin
      Reparse (Unwrap_Unit (Unit), Charset, Buffer);
   end Reparse;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      Populate_Lexical_Env (Unwrap_Unit (Unit));
   end Populate_Lexical_Env;

   ------------------
   -- Get_Filename --
   ------------------

   function Get_Filename (Unit : Analysis_Unit'Class) return String is
   begin
      return Get_Filename (Unwrap_Unit (Unit));
   end Get_Filename;

   -----------------
   -- Get_Charset --
   -----------------

   function Get_Charset (Unit : Analysis_Unit'Class) return String is
   begin
      return Get_Charset (Unwrap_Unit (Unit));
   end Get_Charset;

   ---------------------
   -- Has_Diagnostics --
   ---------------------

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean is
   begin
      return Has_Diagnostics (Unwrap_Unit (Unit));
   end Has_Diagnostics;

   -----------------
   -- Diagnostics --
   -----------------

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array
   is
   begin
      return Diagnostics (Unwrap_Unit (Unit));
   end Diagnostics;

   ---------------------------
   -- Format_GNU_Diagnostic --
   ---------------------------

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String is
   begin
      return Format_GNU_Diagnostic (Unwrap_Unit (Unit), D);
   end Format_GNU_Diagnostic;

   ----------
   -- Root --
   ----------

   function Root (Unit : Analysis_Unit'Class) return ${root_entity.api_name} is
   begin
      return Wrap_Node (Root (Unwrap_Unit (Unit)));
   end Root;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      return First_Token (Unwrap_Unit (Unit));
   end First_Token;

   ----------------
   -- Last_Token --
   ----------------

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference is
   begin
      return Last_Token (Unwrap_Unit (Unit));
   end Last_Token;

   -----------------
   -- Token_Count --
   -----------------

   function Token_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      return Token_Count (Unwrap_Unit (Unit));
   end Token_Count;

   ------------------
   -- Trivia_Count --
   ------------------

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural is
   begin
      return Trivia_Count (Unwrap_Unit (Unit));
   end Trivia_Count;

   ----------
   -- Text --
   ----------

   function Text (Unit : Analysis_Unit'Class) return Text_Type is
   begin
      return Text (Unwrap_Unit (Unit));
   end Text;

   ------------------
   -- Lookup_Token --
   ------------------

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference is
   begin
      return Lookup_Token (Unwrap_Unit (Unit), Sloc);
   end Lookup_Token;

   ----------------------
   -- Dump_Lexical_Env --
   ----------------------

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class) is
   begin
      Dump_Lexical_Env (Unwrap_Unit (Unit));
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

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True)
   is
   begin
      Print (Unwrap_Unit (Unit), Show_Slocs);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia (Unit : Analysis_Unit'Class) is
   begin
      PP_Trivia (Unwrap_Unit (Unit));
   end PP_Trivia;

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : ${root_entity.api_name}'Class) return Boolean is
     (Node.Internal.Node = null);

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : ${root_entity.api_name}'Class) return Boolean
   is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.Is_Token_Node;
   end Is_Token_Node;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : ${root_entity.api_name}'Class) return Boolean is
   begin
      Check_Safety_Net (L.Safety_Net);
      Check_Safety_Net (R.Safety_Net);
      return Compare_Entity (L.Internal, R.Internal);
   end "=";

   -----------------
   -- Short_Image --
   -----------------

   function Short_Image
     (Node : ${root_entity.api_name}'Class) return Text_Type is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return (if Node.Is_Null then "None" else Node.Internal.Node.Short_Image);
   end Short_Image;

   function Short_Image (Node : ${root_entity.api_name}'Class) return String is
     (Image (Node.Short_Image));

   -----------
   -- Image --
   -----------

   function Image (Node : ${root_entity.api_name}'Class) return Text_Type is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Image (Node.Internal);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Node : ${root_entity.api_name}'Class) return String is
     (Image (Node.Image));

   -----------------------
   -- Entity converters --
   -----------------------

   % for e in ctx.entity_types:
      function As_${e.element_type.kwless_raw_name}
        (Node : ${root_entity.api_name}'Class) return ${e.api_name}
      is
         N : constant ${root_node_type_name} := Node.Internal.Node;
      begin
         if N = null then
            return No_${e.api_name};
         end if;

         Check_Safety_Net (Node.Safety_Net);
         if N.all in ${e.element_type.value_type_name()}'Class then
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         else
            raise Constraint_Error with "Invalid type conversion";
         end if;
      end;
   % endfor

   -----------------------
   -- Entity primitives --
   -----------------------

   ----------
   -- Hash --
   ----------

   function Hash
     (Node : ${root_entity.api_name}) return Ada.Containers.Hash_Type is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Hash_Entity (Node.Internal);
   end Hash;

   ----------
   -- Kind --
   ----------

   function Kind
     (Node : ${root_entity.api_name}'Class) return ${root_node_kind_name} is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : ${root_entity.api_name}'Class) return String is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.Kind_Name;
   end Kind_Name;

   % for cls in ctx.array_types:
      ${array_types.ada_api_converters_body(cls)}
   % endfor

   % for e in ctx.entity_types:

      % for f in e.element_type.get_parse_fields( \
         include_inherited=False, \
         predicate=lambda f: f.is_public \
      ):
         ${astnode_types.field_body(f)}
      % endfor

      % for p in e.element_type.get_properties( \
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
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.Abstract_Children_Count;
   end Children_Count;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.First_Child_Index;
   end First_Child_Index;

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.Last_Child_Index;
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
      Check_Safety_Net (Node.Safety_Net);
      Node.Internal.Node.Get_Child (Index, Index_In_Bounds, N);
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
      Check_Safety_Net (Node.Safety_Net);
      return Wrap_Node (Node.Internal.Node.Child (Index), Node.Internal.Info);
   end Child;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : ${root_entity.api_name}'Class) return Source_Location_Range is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.Sloc_Range;
   end Sloc_Range;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return Relative_Position is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.Compare (Sloc);
   end Compare;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return ${root_entity.api_name} is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Wrap_Node (Node.Internal.Node.Lookup (Sloc));
   end Lookup;

   ----------
   -- Text --
   ----------

   function Text (Node : ${root_entity.api_name}'Class) return Text_Type is
   begin
      Check_Safety_Net (Node.Safety_Net);
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
      Check_Safety_Net (Node.Safety_Net);
      return Token_Iterator'(Node.As_${T.root_node.kwless_raw_name},
                             Node.Internal.Node.Token_End_Index);
   end Token_Range;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : ${root_entity.api_name}'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node.Safety_Net);
      Node.Internal.Node.Print (Show_Slocs, Line_Prefix);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node : ${root_entity.api_name}'Class; Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node.Safety_Net);
      Node.Internal.Node.PP_Trivia (Line_Prefix);
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
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.Traverse (Wrapper'Access);
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
      Check_Safety_Net (Node.Safety_Net);
      return Node.Internal.Node.Child_Index;
   end Child_Index;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : ${root_entity.api_name}'Class)
   is
   begin
      Check_Safety_Net (Node.Safety_Net);
      Assign_Names_To_Logic_Vars (Node.Internal.Node);
   end Assign_Names_To_Logic_Vars;

   --------------------------
   -- Children_With_Trivia --
   --------------------------

   function Children_With_Trivia
     (Node : ${root_entity.api_name}'Class) return Children_Array
   is
   begin
      Check_Safety_Net (Node.Safety_Net);
      declare
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
      end;
   end Children_With_Trivia;

   -----------------
   -- First_Token --
   -----------------

   function First_Token (Self : Token_Iterator) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node.Safety_Net);
      return Token_Start (Self.Node);
   end First_Token;

   ----------------
   -- Next_Token --
   ----------------

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node.Safety_Net);
      return Next (Tok);
   end Next_Token;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean is
   begin
      Check_Safety_Net (Self.Node.Safety_Net);
      return Get_Token_Index (Tok).Token <= Self.Last;
   end Has_Element;

   -------------
   -- Element --
   -------------

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference is
   begin
      Check_Safety_Net (Self.Node.Safety_Net);
      return Tok;
   end Element;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Context : in out Analysis_Context) is
   begin
      Context.Internal := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Context : in out Analysis_Context) is
   begin
      Inc_Ref (Unwrap_Context (Context));
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Context : in out Analysis_Context) is
      Ctx : Internal_Context := Unwrap_Context (Context);
   begin
      Dec_Ref (Ctx);
      Context.Internal := null;
   end Finalize;

   ----------------------------------------------------
   -- Soft links for public/internal type converters --
   ----------------------------------------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context;
   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context;

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit;
   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit;

   function Wrap_Node
     (Node : access ${root_node_value_type}'Class;
      Info : AST_Envs.Entity_Info := AST_Envs.No_Entity_Info)
      return ${root_entity.api_name};
   function Unwrap_Node
     (Node : ${root_entity.api_name}'Class) return ${root_node_type_name};

   ------------------
   -- Wrap_Context --
   ------------------

   function Wrap_Context (Context : Internal_Context) return Analysis_Context
   is
   begin
      Inc_Ref (Context);
      return (Ada.Finalization.Controlled with
              Internal => Internal_Context_Access (Context));
   end Wrap_Context;

   --------------------
   -- Unwrap_Context --
   --------------------

   function Unwrap_Context
     (Context : Analysis_Context'Class) return Internal_Context
   is (Internal_Context (Context.Internal));

   ---------------
   -- Wrap_Unit --
   ---------------

   function Wrap_Unit (Unit : Internal_Unit) return Analysis_Unit
   is ((Internal => Internal_Unit_Access (Unit),
        Context  => Wrap_Context (Context (Unit))));

   -----------------
   -- Unwrap_Unit --
   -----------------

   function Unwrap_Unit (Unit : Analysis_Unit'Class) return Internal_Unit
   is (Internal_Unit (Unit.Internal));

   ---------------
   -- Wrap_Node --
   ---------------

   function Wrap_Node
     (Node : access ${root_node_value_type}'Class;
      Info : AST_Envs.Entity_Info := AST_Envs.No_Entity_Info)
      return ${root_entity.api_name} is
   begin
      if Node = null then
         return No_${root_entity.api_name};
      end if;

      declare
         Unit    : constant Internal_Unit := Node.Unit;
         Context : constant Internal_Context := Unit.Context;
      begin
         return ((Internal   => (Node, Info),
                  Safety_Net => (Context        => Context,
                                 Context_Serial => Context.Serial_Number,
                                 Unit           => Unit,
                                 Unit_Version   => Unit.Unit_Version)));
      end;
   end;

   -----------------
   -- Unwrap_Node --
   -----------------

   function Unwrap_Node
     (Node : ${root_entity.api_name}'Class) return ${root_node_type_name}
   is (Node.Internal.Node);

   ${exts.include_extension(ctx.ext('analysis', 'bodies'))}

begin
   Converters.Wrap_Context := Wrap_Context'Access;
   Converters.Unwrap_Context := Unwrap_Context'Access;
   Converters.Wrap_Unit := Wrap_Unit'Access;
   Converters.Unwrap_Unit := Unwrap_Unit'Access;
   Converters.Wrap_Node := Wrap_Node'Access;
   Converters.Unwrap_Node := Unwrap_Node'Access;
end ${ada_lib_name}.Analysis;
