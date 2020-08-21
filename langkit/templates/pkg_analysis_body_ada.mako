## vim: filetype=makoada

<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="exts"          file="extensions.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />
<%namespace name="public_properties"
            file="properties/public_wrappers_ada.mako" />

with Ada.Containers;            use Ada.Containers;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
pragma Warnings (Off, "is an internal GNAT unit");
with Ada.Strings.Unbounded.Aux; use Ada.Strings.Unbounded.Aux;
pragma Warnings (On, "is an internal GNAT unit");
with Ada.Unchecked_Conversion;

with GNATCOLL.Traces;

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

with ${ada_lib_name}.Common;
use ${ada_lib_name}.Common.Symbols;
with ${ada_lib_name}.Private_Converters;
use ${ada_lib_name}.Private_Converters;
with ${ada_lib_name}.Public_Converters; use ${ada_lib_name}.Public_Converters;

${exts.with_clauses(with_clauses)}

package body ${ada_lib_name}.Analysis is

   use ${ada_lib_name}.Implementation;
   use AST_Envs;

   % for array_type in ctx.array_types:
      ${array_types.ada_api_converters_decl(array_type)}
   % endfor

   % for struct_type in ctx.struct_types:
      % if struct_type.exposed and not struct_type.is_entity_type:
         ${struct_types.ada_api_converters_decl(struct_type)}
      % endif
   % endfor

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
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := ${ctx.default_tab_stop})
      return Analysis_Context
   is
      use Unit_Provider_References;

      UP     : Internal_Unit_Provider_Access :=
         Wrap_Public_Provider (Unit_Provider);
      Result : Internal_Context := Create_Context
        (Charset, UP, With_Trivia, Tab_Stop);
   begin
      --  Create_Context created an owneship for itself, so don't forget to
      --  remove the share on UP.
      Dec_Ref (UP);

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

   ---------------------
   -- Get_From_Buffer --
   ---------------------

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit
   is
      Bytes       : Big_String_Access;
      Bytes_Count : Natural;
   begin
      Get_String (Buffer, Bytes, Bytes_Count);
      return Wrap_Unit
        (Get_From_Buffer (Unwrap_Context (Context), Filename, Charset,
                          Bytes (1 .. Bytes_Count), Rule));
   end Get_From_Buffer;

   --------------------
   -- Get_With_Error --
   --------------------

   function Get_With_Error
     (Context  : Analysis_Context'Class;
      Filename : String;
      Error    : Text_Type;
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
      Kind    : Analysis_Unit_Kind;
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
      return Implementation.Diagnostics (Unwrap_Unit (Unit));
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
      return Implementation.Text (Unwrap_Unit (Unit));
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

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Unit : Analysis_Unit; Line_Number : Positive) return Text_Type
   is
   begin
      return Get_Line (Unwrap_Unit (Unit), Line_Number);
   end Get_Line;

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
      return Is_Token_Node (Node.Internal.Node);
   end Is_Token_Node;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic (Node : ${root_entity.api_name}'Class) return Boolean
   is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Is_Synthetic (Node.Internal.Node);
   end Is_Synthetic;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : ${root_entity.api_name}'Class) return Boolean is
   begin
      Check_Safety_Net (L.Safety_Net);
      Check_Safety_Net (R.Safety_Net);
      return Compare_Entity (L.Internal, R.Internal);
   end "=";

   -----------
   -- Image --
   -----------

   function Image (Node : ${root_entity.api_name}'Class) return String is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Image (Node.Internal);
   end Image;

   -----------------------
   -- Entity converters --
   -----------------------

   % for e in ctx.entity_types:
      function As_${e.element_type.kwless_raw_name}
        (Node : ${root_entity.api_name}'Class) return ${e.api_name}
      is
         N : constant ${T.root_node.name} := Node.Internal.Node;
      begin
         if N = null then
            return No_${e.api_name};
         end if;

         Check_Safety_Net (Node.Safety_Net);

         <%def name="emit_return()">
            return (Internal   => (Node => N, Info => Node.Internal.Info),
                    Safety_Net => Node.Safety_Net);
         </%def>
         <%def name="emit_error(e)">
            raise Constraint_Error with
              "${ada_lib_name}: invalid type conversion from "
              & Node.Kind_Name
              & " to ${e.element_type.dsl_name}";
         </%def>

         % if e.element_type.equivalent_to_root:
            ${emit_return()}

         % elif not e.element_type.concrete_subclasses:
            ## If there are no concrete subclasses, then no kind can possibly
            ## match this conversion.
            ${emit_error(e)}

         % else:
            if N.Kind in ${e.element_type.ada_kind_range_name} then
               ${emit_return()}
            else
               ${emit_error(e)}
            end if;
         % endif
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

   function Kind (Node : ${root_entity.api_name}'Class) return ${T.node_kind}
   is
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
      return Kind_Name (Node.Internal.Node);
   end Kind_Name;

   % for array_type in ctx.array_types:
      ${array_types.ada_api_converters_body(array_type)}
   % endfor

   % for struct_type in ctx.struct_types:
      % if struct_type.exposed and not struct_type.is_entity_type:
         ${struct_types.public_api_body(struct_type)}
      % endif
   % endfor

   % for e in ctx.entity_types:

      % if (e.element_type.is_root_list_type and \
            not e.element_type.element_type.is_root_node):
         <% rtype = e.element_type.element_type.entity.api_name %>
         ----------------
         -- List_Child --
         ----------------

         function List_Child
           (Node : ${e.api_name}'Class; Index : Positive) return ${rtype}
         is
            Result : constant ${root_entity.api_name} := Node.Child (Index);
         begin
            return Result.As_${rtype};
         end List_Child;
      % endif

      % if e.element_type.is_root_list_type:
         <% rtype = e.element_type.element_type.entity.api_name %>

         function ${e.api_name}_First (Node : ${e.api_name}) return Positive
         is
           pragma Unreferenced (Node);
         begin
            return 1;
         end;

         function ${e.api_name}_Next
           (Node : ${e.api_name}; Cursor : Positive) return Positive
         is
           pragma Unreferenced (Node);
         begin
            return Cursor + 1;
         end;

         function ${e.api_name}_Has_Element
           (Node : ${e.api_name}; Cursor : Positive) return Boolean
         is
         begin
            return Cursor in 1 .. Node.Children_Count;
         end;

         function ${e.api_name}_Element
           (Node : ${e.api_name}; Cursor : Positive) return ${rtype}'Class
         is
            Child : constant ${root_entity.api_name} := Node.Child (Cursor);
         begin
            return ${rtype}'(Child.As_${rtype});
         end;
      % endif

      % for f in e.element_type.get_parse_fields( \
         include_inherited=False, \
         predicate=lambda f: f.is_public and not f.overriding \
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
      return Children_Count (Node.Internal.Node);
   end Children_Count;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return First_Child_Index (Node.Internal.Node);
   end First_Child_Index;

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Last_Child_Index (Node.Internal.Node);
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
      N : ${T.root_node.name};
   begin
      Check_Safety_Net (Node.Safety_Net);
      Get_Child (Node.Internal.Node, Index, Index_In_Bounds, N);
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
      return Wrap_Node (Child (Node.Internal.Node, Index), Node.Internal.Info);
   end Child;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : ${root_entity.api_name}'Class) return Source_Location_Range is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Sloc_Range (Node.Internal.Node);
   end Sloc_Range;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return Relative_Position is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Compare (Node.Internal.Node, Sloc);
   end Compare;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return ${root_entity.api_name} is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Wrap_Node (Lookup (Node.Internal.Node, Sloc));
   end Lookup;

   ----------
   -- Text --
   ----------

   function Text (Node : ${root_entity.api_name}'Class) return Text_Type is
   begin
      Check_Safety_Net (Node.Safety_Net);
      return Implementation.Text (Node.Internal.Node);
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
      Print (Node.Internal.Node, Show_Slocs, Line_Prefix);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node : ${root_entity.api_name}'Class; Line_Prefix : String := "") is
   begin
      Check_Safety_Net (Node.Safety_Net);
      PP_Trivia (Node.Internal.Node, Line_Prefix);
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
      Info : constant ${T.entity_info.name} := Node.Internal.Info;

      -------------
      -- Wrapper --
      -------------

      function Wrapper (Node : ${T.root_node.name}) return Visit_Status
      is
         Public_Node : constant ${root_entity.api_name} :=
           Wrap_Node (${T.root_node.name} (Node), Info);
      begin
         return Visit (Public_Node);
      end Wrapper;

   begin
      Check_Safety_Net (Node.Safety_Net);
      return Traverse (Node.Internal.Node, Wrapper'Access);
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

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : ${root_entity.api_name}'Class)
   is
   begin
      Check_Safety_Net (Node.Safety_Net);
      Assign_Names_To_Logic_Vars (Node.Internal.Node);
   end Assign_Names_To_Logic_Vars;

   -------------------------
   -- Children_And_Trivia --
   -------------------------

   function Children_And_Trivia
     (Node : ${root_entity.api_name}'Class) return Children_Array
   is
   begin
      Check_Safety_Net (Node.Safety_Net);
      declare
         Bare_Result : constant Bare_Children_Array :=
            Children_And_Trivia (Unwrap_Node (Node));
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
   end Children_And_Trivia;

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
     (Node : ${T.root_node.name};
      Info : ${T.entity_info.name} := ${T.entity_info.nullexpr})
      return ${root_entity.api_name};
   function Unwrap_Node
     (Node : ${root_entity.api_name}'Class) return ${T.root_node.name};
   function Unwrap_Entity
     (Entity : ${root_entity.api_name}'Class) return ${root_entity.name};

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
     (Node : ${T.root_node.name};
      Info : ${T.entity_info.name} := ${T.entity_info.nullexpr})
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
     (Node : ${root_entity.api_name}'Class) return ${T.root_node.name}
   is (Node.Internal.Node);

   -------------------
   -- Unwrap_Entity --
   -------------------

   function Unwrap_Entity
     (Entity : ${root_entity.api_name}'Class) return ${root_entity.name}
   is ((Entity.Internal));

   ${exts.include_extension(ctx.ext('analysis', 'bodies'))}

begin
   Public_Converters.Wrap_Context := Wrap_Context'Access;
   Public_Converters.Unwrap_Context := Unwrap_Context'Access;
   Public_Converters.Wrap_Unit := Wrap_Unit'Access;
   Public_Converters.Unwrap_Unit := Unwrap_Unit'Access;
   Public_Converters.Wrap_Node := Wrap_Node'Access;
   Public_Converters.Unwrap_Node := Unwrap_Node'Access;
   Public_Converters.Unwrap_Entity := Unwrap_Entity'Access;
end ${ada_lib_name}.Analysis;
