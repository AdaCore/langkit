## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="exts"          file="extensions.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />
<%namespace name="memoization"   file="memoization_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />

<% root_node_array = T.root_node.array %>

with Ada.Containers;                  use Ada.Containers;
with Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNATCOLL.VFS;

with Langkit_Support.Hashes;      use Langkit_Support.Hashes;
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

with ${ada_lib_name}.Lexer;

with ${ada_lib_name}.Introspection;
${(exts.with_clauses(with_clauses + [
   ((ctx.env_hook_subprogram.unit_fqn, False)
    if ctx.env_hook_subprogram else None)
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

package body ${ada_lib_name}.Analysis.Implementation is

   generic
      type T (<>) is limited private;
      type T_Access is access all T;
      with procedure Destroy (Object : in out T_Access);
   procedure Register_Destroyable_Gen
     (Unit : Analysis_Unit; Object : T_Access);
   --  Generic procedure to register an object so that it is automatically
   --  destroyed when Unit is destroyed.

   procedure Register_Destroyable_Helper
     (Unit    : Analysis_Unit;
      Object  : System.Address;
      Destroy : Destroy_Procedure);
   --  Common underlying implementation for Register_Destroyable_Gen

   function Solve_Wrapper
     (R            : Relation;
      Context_Node : ${root_node_type_name}) return Boolean;
   --  Wrapper for Langkit_Support.Adalog.Solve; will handle setting the debug
   --  strings in the equation if in debug mode.

   function Convert is new Ada.Unchecked_Conversion
     (Entity_Info, Public_Entity_Info);

   type Internal_Analysis_Unit is access all Analysis_Unit_Type;
   function Convert is new Ada.Unchecked_Conversion
     (Analysis_Unit, Internal_Analysis_Unit);

   procedure Destroy (Env : in out Lexical_Env_Access);

   function Snaps_At_Start
     (Self : access ${root_node_value_type}'Class) return Boolean;

   function Snaps_At_End
     (Self : access ${root_node_value_type}'Class) return Boolean;

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node
     (Node : access ${root_node_value_type}'Class) return Boolean
   is
   begin
      return Is_Token_Node_Kind (Node.Kind);
   end Is_Token_Node;

   ------------------------------
   -- Register_Destroyable_Gen --
   ------------------------------

   procedure Register_Destroyable_Gen
     (Unit : Analysis_Unit; Object : T_Access)
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

   % for array_type in ctx.sorted_types(ctx.array_types):
   % if array_type.element_type.should_emit_array_type:
   ${array_types.body(array_type)}
   % endif
   % endfor

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Analysis_Unit) return Boolean is
      use GNATCOLL.VFS;
   begin
      return Left.File_Name < Right.File_Name;
   end "<";

   % if ctx.has_memoization:
      ${memoization.body()}
   % endif

   % if ctx.has_env_assoc:
      procedure Add_To_Env
        (Self     : ${root_node_type_name};
         Env      : Lexical_Env;
         Mapping  : ${T.env_assoc.name};
         MD       : ${T.env_md.name};
         Resolver : Entity_Resolver);
      --  Helper for Populate_Lexical_Env: add the key/element Mapping in the
      --  Env lexical environment using the given metadata (MD).
   % endif

   % if ctx.has_env_assoc_array:
      procedure Add_To_Env
        (Self     : ${root_node_type_name};
         Env      : Lexical_Env;
         Mappings : in out ${T.env_assoc.array.name};
         MD       : ${T.env_md.name};
         Resolver : Entity_Resolver);
      --  Add_To_Env overload to work on an array of key/element mappings.
      --  Calling this takes an ownership share for Mappings.
   % endif

   % if ctx.has_ref_env:
      procedure Ref_Env
        (Self                : ${root_node_type_name};
         Dest_Env            : Lexical_Env;
         Ref_Env_Nodes       : in out ${T.root_node.array.name};
         Resolver            : Lexical_Env_Resolver;
         Transitive          : Boolean);
      --  Add referenced environments to Self.Self_Env. Calling this takes an
      --  ownership share for Ref_Env_Nodes.
   % endif

   -------------------
   -- Solve_Wrapper --
   -------------------

   function Solve_Wrapper
     (R            : Relation;
      Context_Node : ${root_node_type_name}) return Boolean is
   begin
      if Context_Node /= null 
         and then Langkit_Support.Adalog.Debug.Debug
      then
         Context_Node.Assign_Names_To_Logic_Vars;
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
        (Self     : ${root_node_type_name};
         Env      : Lexical_Env;
         Mapping  : ${T.env_assoc.name};
         MD       : ${T.env_md.name};
         Resolver : Entity_Resolver)
      is
         Root_Scope : Lexical_Env renames Self.Unit.Context.Root_Scope;
      begin
         if Mapping = No_Env_Assoc then
            return;
         end if;

         if Mapping.F_Val.Unit /= Self.Unit then
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
         Add (Self  => Env,
              Key   => Mapping.F_Key,
              Value => Mapping.F_Val,
              MD    => MD,
              Resolver => Resolver);

         --  If we're adding the element to an environment that belongs to a
         --  different unit, then:
         if Env /= Empty_Env
            and then (Env = Root_Scope
                      or else Env.Env.Node.Unit /= Self.Unit)
         then
            --  Add the environment, the key, and the value to the list of
            --  entries contained in other units, so we can remove them when
            --  reparsing Val's unit.
            Mapping.F_Val.Unit.Exiled_Entries.Append
              ((Env, Mapping.F_Key, Mapping.F_Val));

            if Env /= Root_Scope then
               --  Add Val to the list of foreign nodes that Env's unit
               --  contains, so that when that unit is reparsed, we can call
               --  Add_To_Env again on those nodes.
               Env.Env.Node.Unit.Foreign_Nodes.Append
                 ((Mapping.F_Val, Self.Unit));
            end if;
         end if;
      end Add_To_Env;
   % endif

   % if ctx.has_env_assoc_array:
      ----------------
      -- Add_To_Env --
      ----------------

      procedure Add_To_Env
        (Self     : ${root_node_type_name};
         Env      : Lexical_Env;
         Mappings : in out ${T.env_assoc.array.name};
         MD       : ${T.env_md.name};
         Resolver : Entity_Resolver)
      is
      begin
         for M of Mappings.Items loop
            Add_To_Env (Self, Env, M, MD, Resolver);
         end loop;
         Dec_Ref (Mappings);
      end Add_To_Env;
   % endif

   % if ctx.has_ref_env:
      -------------
      -- Ref_Env --
      -------------

      procedure Ref_Env
        (Self                : ${root_node_type_name};
         Dest_Env            : Lexical_Env;
         Ref_Env_Nodes       : in out ${T.root_node.array.name};
         Resolver            : Lexical_Env_Resolver;
         Transitive          : Boolean)
      is
      begin
         for N of Ref_Env_Nodes.Items loop
            if N /= null then
               if N.Unit /= Self.Unit then
                  raise Property_Error with
                     "attempt to add a referenced environment to a foreign"
                     & " unit";
               end if;
               Reference
                 (Dest_Env, N, Resolver,
                  Transitive);
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

   ---------------------
   -- Pre_Env_Actions --
   ---------------------

   function Pre_Env_Actions
     (Self                : access ${root_node_value_type}'Class;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env
   is
   begin

      <%self:case_dispatch
        pred="${lambda n: n.env_spec and not n.is_env_spec_inherited}">
      <%def name="action(node)">
         return ${node.name}_Pre_Env_Actions
           (${node.name} (Self), Bound_Env, Root_Env, Add_To_Env_Only);
      </%def>
      <%def name="default()"> return Null_Lexical_Env; </%def>
      </%self:case_dispatch>

   end Pre_Env_Actions;

   ----------------------
   -- Post_Env_Actions --
   ----------------------

   procedure Post_Env_Actions
     (Self                : access ${root_node_value_type}'Class;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env) is
   begin
      <%self:case_dispatch pred="${lambda n: n.env_spec \
                                   and not n.is_env_spec_inherited \
                                   and n.env_spec.post_actions}">
      <%def name="action(n)">
         ${n.name}_Post_Env_Actions (${n.name} (Self), Bound_Env, Root_Env);
      </%def>
      <%def name="default()"> null; </%def>
      </%self:case_dispatch>
   end Post_Env_Actions;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol
     (Node : access ${root_node_value_type}'Class) return Symbol_Type is
   begin
      return Get_Symbol (Node.Token (Node.Token_Start_Index));
   end Get_Symbol;

   ----------
   -- Text --
   ----------

   function Text
     (Node : access ${root_node_value_type}'Class) return Text_Type
   is
      Start_T : constant Token_Type := Node.Token (Node.Token_Start_Index);
      End_T   : constant Token_Type := Node.Token (Node.Token_End_Index);
   begin
      return Text (Start_T, End_T);
   end Text;

   ---------------------
   -- Is_Visible_From --
   ---------------------

   function Is_Visible_From
     (Referenced_Env, Base_Env : AST_Envs.Lexical_Env) return Boolean is
      Referenced_Node : constant ${root_node_type_name} :=
         Referenced_Env.Env.Node;
      Base_Node       : constant ${root_node_type_name} := Base_Env.Env.Node;
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

   function Unit
     (Node : access ${root_node_value_type}'Class) return Analysis_Unit is
   begin
      return Node.Unit;
   end Unit;

   ${array_types.body(T.LexicalEnvType.array)}
   ${array_types.body(T.root_node.entity.array)}

   function Lookup_Internal
     (Node : ${root_node_type_name};
      Sloc : Source_Location) return ${root_node_type_name};
   procedure Lookup_Relative
     (Node       : ${root_node_type_name};
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out ${root_node_type_name});
   --  Implementation helpers for the looking up process

   -----------------
   -- Set_Parents --
   -----------------

   procedure Set_Parents
     (Node, Parent : access ${root_node_value_type}'Class)
   is
   begin
      if Node = null then
         return;
      end if;

      Node.Parent := ${root_node_type_name} (Parent);

      for I in 1 .. Node.Abstract_Children_Count loop
         Set_Parents (Node.Child (I), Node);
      end loop;
   end Set_Parents;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Node : access ${root_node_value_type}'Class) is
   begin
      if Node = null then
         return;
      end if;

      Node.Free_Extensions;
      Node.Reset_Logic_Vars;
      for I in 1 .. Node.Abstract_Children_Count loop
         Destroy (Node.Child (I));
      end loop;
   end Destroy;

   -----------
   -- Child --
   -----------

   function Child (Node  : access ${root_node_value_type}'Class;
                   Index : Positive) return ${root_node_type_name}
   is
      Result          : ${root_node_type_name};
      Index_In_Bounds : Boolean;
   begin
      Get_Child (Node, Index, Index_In_Bounds, Result);
      return (if Index_In_Bounds then Result else null);
   end Child;

   --------------
   -- Traverse --
   --------------

   function Traverse
     (Node  : access ${root_node_value_type}'Class;
      Visit : access function (Node : access ${root_node_value_type}'Class)
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
            for I in 1 .. Node.Abstract_Children_Count loop
               declare
                  Cur_Child : constant ${root_node_type_name} :=
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
     (Node  : access ${root_node_value_type}'Class;
      Visit : access function (Node : access ${root_node_value_type}'Class)
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
     (Node  : access ${root_node_value_type}'Class;
      Visit : access function (Node : access ${root_node_value_type}'Class;
                               Data : in out Data_Type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status
   is
      function Helper (Node : access ${root_node_value_type}'Class)
                       return Visit_Status;

      ------------
      -- Helper --
      ------------

      function Helper (Node : access ${root_node_value_type}'Class)
                       return Visit_Status
      is
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
     (Node : access ${root_node_value_type}'Class) return Source_Location_Range
   is
      type Token_Anchor is (T_Start, T_End);
      type Token_Pos is record
         Pos    : Token_Index;
         Anchor : Token_Anchor;
      end record;

      TDH : Token_Data_Handler renames Convert (Node.Unit).TDH;
      Token_Start, Token_End : Token_Pos;

      function Get
        (Index : Token_Index) return Lexer.Token_Data_Type
      is
        (Get_Token (TDH, Index));

      function Sloc
        (T : Token_Pos) return Source_Location
      is
        (if T.Anchor = T_Start
         then Start_Sloc (Get (T.Pos).Sloc_Range)
         else End_Sloc (Get (T.Pos).Sloc_Range));

   begin
      if Node.Is_Synthetic then
         return Sloc_Range (Node.Parent);
      end if;

      if Node.Is_Ghost then
         Token_Start := (if Node.Token_Start_Index = 1
                         then (1, T_Start)
                         else (Node.Token_Start_Index - 1, T_End));
         Token_End := Token_Start;
      else
         Token_Start := (Node.Token_Start_Index, T_Start);
         Token_End := (Node.Token_End_Index, T_End);
      end if;

      if Snaps_At_Start (Node)
         and then not Node.Is_Ghost
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
     (Node : access ${root_node_value_type}'Class;
      Sloc : Source_Location) return ${root_node_type_name}
   is
      Position : Relative_Position;
      Result   : ${root_node_type_name};
   begin
      if Sloc = No_Source_Location then
         return null;
      end if;

      Lookup_Relative
        (${root_node_type_name} (Node), Sloc, Position, Result);
      return Result;
   end Lookup;

   ---------------------
   -- Lookup_Internal --
   ---------------------

   function Lookup_Internal
     (Node : ${root_node_type_name};
      Sloc : Source_Location) return ${root_node_type_name}
   is
      --  For this implementation helper (i.e. internal primitive), we can
      --  assume that all lookups fall into this node's sloc range.
      pragma Assert (Compare (Sloc_Range (Node), Sloc) = Inside);

      Children : constant ${root_node_array.api_name} := Node.Children;
      Pos      : Relative_Position;
      Result   : ${root_node_type_name};
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
     (Node : access ${root_node_value_type}'Class;
      Sloc : Source_Location) return Relative_Position is
   begin
      return Compare (Sloc_Range (Node), Sloc);
   end Compare;

   ---------------------
   -- Lookup_Relative --
   ---------------------

   procedure Lookup_Relative
     (Node       : ${root_node_type_name};
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out ${root_node_type_name})
   is
      Result : constant Relative_Position :=
        Compare (Node, Sloc);
   begin
      Position := Result;
      Node_Found := (if Result = Inside
                     then Lookup_Internal (Node, Sloc)
                     else null);
   end Lookup_Relative;

   -------------------
   -- Get_Extension --
   -------------------

   function Get_Extension
     (Node : access ${root_node_value_type}'Class;
      ID   : Extension_ID;
      Dtor : Extension_Destructor) return Extension_Access
   is
      use Extension_Vectors;
   begin
      for Slot of Node.Extensions loop
         if Slot.ID = ID then
            return Slot.Extension;
         end if;
      end loop;

      declare
         New_Ext : constant Extension_Access :=
           new Extension_Type'(Extension_Type (System.Null_Address));
      begin
         Append (Node.Extensions,
                 Extension_Slot'(ID        => ID,
                                 Extension => New_Ext,
                                 Dtor      => Dtor));
         return New_Ext;
      end;
   end Get_Extension;

   ---------------------
   -- Free_Extensions --
   ---------------------

   procedure Free_Extensions (Node : access ${root_node_value_type}'Class) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Extension_Type, Extension_Access);
      use Extension_Vectors;
      Slot : Extension_Slot;
   begin
      --  Explicit iteration for perf
      for J in First_Index (Node.Extensions) .. Last_Index (Node.Extensions)
      loop
         Slot := Get (Node.Extensions, J);
         Slot.Dtor (Node, Slot.Extension.all);
         Free (Slot.Extension);
      end loop;
   end Free_Extensions;

   --------------
   -- Children --
   --------------

   function Children
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_array.api_name}
   is
      First : constant Integer
        := ${root_node_array.index_type()}'First;
      Last  : constant Integer := First + Node.Abstract_Children_Count - 1;
   begin
      return A : ${root_node_array.api_name} (First .. Last)
      do
         for I in First .. Last loop
            A (I) := Child (Node, I);
         end loop;
      end return;
   end Children;

   function Children
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_array.name}
   is
      C : ${root_node_array.api_name} := Children (Node);
   begin
      return Ret : ${root_node_array.name} := Create (C'Length) do
         Ret.Items := C;
      end return;
   end Children;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node        : access ${root_node_value_type}'Class;
      Line_Prefix : String := "")
   is
      Children_Prefix : constant String := Line_Prefix & "|  ";
      N               : constant ${root_entity.api_name} :=
        (Node, No_Public_Entity_Info);
   begin
      Put_Line (Line_Prefix & Kind_Name (Node));
      for C of Children_With_Trivia (N) loop
         case C.Kind is
            when Trivia =>
               Put_Line (Children_Prefix & Text (C.Trivia));
            when Child =>
               PP_Trivia (C.Node, Children_Prefix);
         end case;
      end loop;
   end PP_Trivia;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   function Populate_Lexical_Env
     (Node : access ${root_node_value_type}'Class) return Boolean
   is

      Context  : constant Analysis_Context := Node.Unit.Context;
      Root_Env : constant Lexical_Env := Context.Root_Scope;

      function Populate_Internal
        (Node      : access ${root_node_value_type}'Class;
         Bound_Env : Lexical_Env) return Boolean;
      --  Do the lexical env population on Node and recurse on its children

      -----------------------
      -- Populate_Internal --
      -----------------------

      function Populate_Internal
        (Node      : access ${root_node_value_type}'Class;
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
            Initial_Env := Node.Pre_Env_Actions (Bound_Env, Root_Env);

            if Initial_Env /= Null_Lexical_Env then
               Node.Self_Env := Initial_Env;
            end if;

            --  Call recursively on children
            for C of ${root_node_array.api_name}'(Children (Node)) loop
               Result := Populate_Internal (C, Node.Self_Env) or else Result;
            end loop;

            Node.Post_Env_Actions (Initial_Env, Root_Env);
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
            PLE_Unit_Root : constant ${ctx.ple_unit_root.name} :=
               ${ctx.ple_unit_root.name} (Node);
         begin
            if PLE_Unit_Root.Is_Env_Populated then
               return False;
            end if;
            PLE_Unit_Root.Is_Env_Populated := True;
         end;

      % else:
         --  This is intended to be called on the root node only
         if Node.Parent /= null then
            raise Program_Error;
         end if;
      % endif

      return Populate_Internal (Node, Root_Env);
   end Populate_Lexical_Env;

   ----------------------------
   -- AST_Envs_Element_Image --
   ----------------------------

   function AST_Envs_Element_Image
     (Node  : ${root_node_type_name};
      Short : Boolean := True) return Text_Type is
   begin
      if Short then
         return To_Text (Basename (Node.Unit))
           & ":" & To_Text (Image (Start_Sloc (Sloc_Range (Node))));
      else
         return Node.Short_Image;
      end if;
   end AST_Envs_Element_Image;

   --------------------------
   -- Raise_Property_Error --
   --------------------------

   procedure Raise_Property_Error (Message : String := "") is
   begin
      if Message'Length = 0 then
         raise Property_Error;
      else
         raise Property_Error with Message;
      end if;
   end Raise_Property_Error;

   -------------------
   -- Is_Rebindable --
   -------------------

   function Is_Rebindable (Node : ${root_node_type_name}) return Boolean is
   begin
      <% rebindable_nodes = [n for n in ctx.astnode_types
                             if n.annotations.rebindable] %>
      % if not rebindable_nodes:
         return True;
      % else:
         <% type_sum = ' | '.join("{}_Type'Class".format(n.name)
                                  for n in rebindable_nodes) %>
         return Node.all in ${type_sum};
      % endif
   end Is_Rebindable;

   ------------------------
   -- Register_Rebinding --
   ------------------------

   procedure Register_Rebinding
     (Node : ${root_node_type_name}; Rebinding : System.Address)
   is
      pragma Warnings (Off, "possible aliasing problem for type");
      function Convert is new Ada.Unchecked_Conversion
        (System.Address, Env_Rebindings);
      pragma Warnings (Off, "possible aliasing problem for type");
   begin
      Convert (Node.Unit).Rebindings.Append (Convert (Rebinding));
   end Register_Rebinding;

   --------------------
   -- Element_Parent --
   --------------------

   function Element_Parent
     (Node : ${root_node_type_name}) return ${root_node_type_name}
   is (Node.Parent);

   ----------
   -- Hash --
   ----------

   function Hash (Node : access ${root_node_value_type}'Class) return Hash_Type
   is
      function H is new Hash_Access
        (${root_node_value_type}'Class, ${root_node_type_name});
   begin
      return H (Node);
   end Hash;

   % if T.BoolType.requires_hash_function:
      function Hash (B : Boolean) return Hash_Type is (Boolean'Pos (B));
   % endif

   % if T.LongType.requires_hash_function:
      function Hash (I : Integer) return Hash_Type is (Hash_Type'Mod (I));
   % endif

   % if T.entity_info.requires_hash_function:
      function Hash (Info : Entity_Info) return Hash_Type is
        (Combine (Hash (Info.MD), Hash (Info.Rebindings)));
   % endif

   % if T.AnalysisUnitType.requires_hash_function:
      function Hash (Unit : Analysis_Unit) return Hash_Type is
        (GNATCOLL.VFS.Full_Name_Hash (Unit.File_Name));
   % endif

   -------------
   -- Version --
   -------------

   function Version (Unit : Analysis_Unit) return Natural is
   begin
      return Unit.Unit_Version;
   end Version;

   -----------------
   -- Short_Image --
   -----------------

   function Short_Image
     (Self : access ${root_node_value_type}'Class) return Text_Type is
   begin
      <%self:case_dispatch
         pred="${lambda n: n.annotations.custom_short_image}">
      <%def name="action(node)">
         return ${node.name}_Short_Image (${node.name} (Self));
      </%def>
      <%def name="default()">
         return "<" & To_Text (Self.Kind_Name)
                & " " & To_Text (Image (Self.Sloc_Range)) & ">";
      </%def>
      </%self:case_dispatch>
   end Short_Image;

   --------------------
   -- Snaps_At_Start --
   --------------------

   function Snaps_At_Start
     (Self : access ${root_node_value_type}'Class) return Boolean is
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

   function Snaps_At_End
     (Self : access ${root_node_value_type}'Class) return Boolean is
   begin
      <%self:case_dispatch pred="${lambda n: n.snaps_at_end}">
      <%def name="action(node)">
         return True;
      </%def>
      <%def name="default()">
         return Self.Is_Incomplete;
      </%def>
      </%self:case_dispatch>
   end Snaps_At_End;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node         : access ${root_node_value_type}'Class;
      Include_Self : Boolean := True)
      return ${root_node_array.name}
   is
      Count : Natural := 0;
      Start : ${root_node_type_name} :=
        ${root_node_type_name} (if Include_Self then Node else Node.Parent);
      Cur   : ${root_node_type_name} := Start;
   begin
      while Cur /= null loop
         Count := Count + 1;
         Cur := Cur.Parent;
      end loop;

      declare
         Result : constant ${root_node_array.name} := Create (Count);
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

   function First_Child_Index
     (Node : access ${root_node_value_type}'Class) return Natural
   is (1);

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index
     (Node : access ${root_node_value_type}'Class) return Natural
   is (Node.Abstract_Children_Count);

   ---------------
   -- Get_Child --
   ---------------

   procedure Get_Child
     (Node            : access ${root_node_value_type}'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${root_node_type_name})
   is
      K : constant ${root_node_kind_name} := Node.Kind;

   begin
      <%
        root_type = ctx.root_grammar_class.name

        def get_actions(astnode, node_expr):
            specific_fields = astnode.get_parse_fields(
                lambda f: f.type.is_ast_node,
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
                    lambda f: f.type.is_ast_node,
                    include_inherited=True
                )
                first_field_index = len(all_fields) - len(specific_fields) + 1

                result.append('case Index is')
                for i, f in enumerate(specific_fields, first_field_index):
                    result.append("""
                        when {} =>
                            Result := {} ({}.{});
                            return;
                    """.format(i, root_type, node_expr, f.name))
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
     (Node        : access ${root_node_value_type}'Class;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "")
   is
      K               : constant ${root_node_kind_name} := Node.Kind;
      Attr_Prefix     : constant String := Line_Prefix & "|";
      Children_Prefix : constant String := Line_Prefix & "|  ";

   begin
      Put (Line_Prefix & Node.Kind_Name);
      if Show_Slocs then
         Put ("[" & Image (Node.Sloc_Range) & "]");
      end if;

      if Node.Is_Incomplete then
         Put (" <<INCOMPLETE>>");
      end if;

      if Is_Token_Node_Kind (Node.Kind) then
         Put_Line (": " & Image (Node.Text));
      elsif Node.all not in ${generic_list_value_type}'Class then
         New_Line;
      end if;

      % if ctx.generic_list_type.concrete_subclasses:
         --  List nodes are displayed in a special way (they have no field)
         if K in ${ctx.generic_list_type.ada_kind_range_name} then
            declare
               List : constant ${ctx.generic_list_type.name} :=
                  ${ctx.generic_list_type.name} (Node);
            begin
               if List.Count = 0 then
                  Put_Line (": <empty list>");
                  return;
               end if;

               New_Line;
               for Child of List.Nodes (1 .. List.Count) loop
                  if Child /= null then
                     Child.Print (Show_Slocs, Line_Prefix & "|  ");
                  end if;
               end loop;
            end;
            return;
         end if;
      % endif

      % if ctx.sorted_parse_fields:
         --  This is for regular nodes: display each field
         declare
            use ${ada_lib_name}.Introspection;
            Field_List : constant Field_Reference_Array := Fields (K);
         begin
            for I in Field_List'Range loop
               declare
                  Child : constant ${root_node_type_name} :=
                     Node.Child (I);
               begin
                  Put (Attr_Prefix & Field_Name (Field_List (I)) & ":");
                  if Child /= null then
                     New_Line;
                     Child.Print (Show_Slocs, Children_Prefix);
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

   function Parent
     (Node : access ${root_node_value_type}'Class) return ${root_node_type_name}
   is
   begin
      return Node.Parent;
   end Parent;

   ------------------
   -- Stored_Token --
   ------------------

   function Stored_Token
     (Node  : access ${root_node_value_type}'Class;
      Token : Token_Type) return Token_Index is
   begin
      if Convert (Node.Unit).TDH'Access /= Token.TDH then
         raise Property_Error with
           ("Cannot associate a token and a node from different analysis"
            & " units");
      elsif Token.Index.Trivia /= No_Token_Index then
         raise Property_Error with
           ("A node cannot hold trivia");
      end if;

      return Token.Index.Token;
   end Stored_Token;

   --------------
   -- Is_Ghost --
   --------------

   function Is_Ghost
     (Node : access ${root_node_value_type}'Class) return Boolean
   is (Node.Token_End_Index = No_Token_Index);

   -------------------
   -- Is_Incomplete --
   -------------------

   function Is_Incomplete
     (Node : access ${root_node_value_type}'Class) return Boolean
   is
      LGC : ${root_node_type_name};
   begin
     if Is_List_Node (Node.Kind) then
        LGC := (if Node.Last_Child_Index /= 0
                then Node.Child (Node.Last_Child_Index)
                else null);
        return LGC /= null and then LGC.Is_Incomplete;
      else
         return Node.Last_Attempted_Child > -1;
      end if;
   end;

   ------------------
   -- Is_Synthetic --
   ------------------

   function Is_Synthetic
     (Node : access ${root_node_value_type}'Class) return Boolean
   is (Node.Token_Start_Index = No_Token_Index);

   -----------------
   -- Token_Start --
   -----------------

   function Token_Start
     (Node : access ${root_node_value_type}'Class) return Token_Type
   is (Node.Token (Node.Token_Start_Index));

   ---------------
   -- Token_End --
   ---------------

   function Token_End
     (Node : access ${root_node_value_type}'Class) return Token_Type
   is
     (if Node.Token_End_Index = No_Token_Index
      then Node.Token_Start
      else Node.Token (Node.Token_End_Index));

   -------------
   -- Convert --
   -------------

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Type;
      Raw_Data : Lexer.Token_Data_Type) return Analysis.Token_Data_Type is
   begin
      return (Kind          => Raw_Data.Kind,
              Is_Trivia     => Token.Index.Trivia /= No_Token_Index,
              Index         => (if Token.Index.Trivia = No_Token_Index
                                then Token.Index.Token
                                else Token.Index.Trivia),
              Source_Buffer => Text_Cst_Access (TDH.Source_Buffer),
              Source_First  => Raw_Data.Source_First,
              Source_Last   => Raw_Data.Source_Last,
              Sloc_Range    => Raw_Data.Sloc_Range);
   end Convert;

   -----------
   -- Token --
   -----------

   function Token
     (Node  : access ${root_node_value_type}'Class;
      Index : Token_Index) return Token_Type
   is
     (if Index = No_Token_Index
      then No_Token
      else (TDH    => Token_Data (Node.Unit),
            Index  => (Index, No_Token_Index)));

   -------------
   -- Is_Null --
   -------------

   function Is_Null
     (Node : access ${root_node_value_type}'Class) return Boolean
   is (Node = null);

   -----------------
   -- Child_Index --
   -----------------

   function Child_Index
     (Node : access ${root_node_value_type}'Class)
      return Natural
   is
      N : ${root_node_type_name} := null;
   begin
      if Node.Parent = null then
         raise Property_Error with
            "Trying to get the child index of a root node";
      end if;

      for I in Node.Parent.First_Child_Index .. Node.Parent.Last_Child_Index
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

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling
     (Node : access ${root_node_value_type}'Class;
      E_Info : Entity_Info := No_Entity_Info) return Entity
   is
      N : constant Positive := Child_Index (Node) + 1;
   begin
      return (if N = 1
              then No_Entity
              else (Node.Parent.Child (N - 1), E_Info));
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling
     (Node : access ${root_node_value_type}'Class;
      E_Info : Entity_Info := No_Entity_Info) return Entity
   is
      Ret : constant ${root_node_type_name} := 
        Node.Parent.Child (Child_Index (Node) + 1);
   begin
      --  If Node is the last sibling, then Child will return null
      return (if Ret /= null then (Ret, E_Info) else No_Entity);
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
      Index : Integer)
      return Entity
   is
      function Length (A : AST_Envs.Entity_Array) return Natural
      is (A'Length);

      function Get
        (A     : AST_Envs.Entity_Array;
         Index : Integer)
         return Entity
      is (A (Index + 1)); --  A is 1-based but Index is 0-based

      function Relative_Get is new Langkit_Support.Relative_Get
        (Item_Type     => Entity,
         Sequence_Type => AST_Envs.Entity_Array,
         Length        => Length,
         Get           => Get);
      Result : Entity;
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
     (Envs : ${T.LexicalEnvType.array.name}; 
      Env_Md : ${T.env_md.name} := No_Metadata) return ${T.LexicalEnvType.name}
   is (Group (AST_Envs.Lexical_Env_Array (Envs.Items), Env_Md));

   % for astnode in ctx.astnode_types:
       ${astnode_types.body_decl(astnode)}
   % endfor

   ------------------
   -- Children_Env --
   ------------------

   function Children_Env
     (Node   : access ${root_node_value_type}'Class;
      E_Info : Entity_Info := No_Entity_Info) return Lexical_Env
   is (Rebind_Env (Node.Self_Env, E_Info));

   --------------
   -- Node_Env --
   --------------

   function Node_Env
     (Node   : access ${root_node_value_type}'Class;
      E_Info : Entity_Info := No_Entity_Info) return Lexical_Env
   is
      <%
         nodes_adding_env = [
            n for n in ctx.astnode_types
            if n.env_spec and
               not n.abstract and
               n.env_spec.adds_env
         ]

         nodes_not_adding_env = [
            n for n in ctx.astnode_types
            if n.env_spec and
               not n.abstract and
               not n.env_spec.adds_env
         ]
      %>
      Base_Env : Lexical_Env :=
         % if nodes_adding_env:
            (if Node.Kind in
               ${' | '.join(n.ada_kind_name for n in nodes_adding_env)}

               % if nodes_not_adding_env:
               ## Env specs might be overriden, so node kind that don't add
               ## envs might be derived from one that do, so we need to
               ## blacklist concrete nodes that we know are not adding envs.
               and then Node.Kind not in
               ${' | '.join(n.ada_kind_name
                            for n in nodes_not_adding_env)}
               % endif
             then AST_Envs.Get_Env (Node.Self_Env.Env.Parent)
             else Node.Self_Env)
         % else:
            Node.Self_Env
         % endif
      ;
      Result : constant Lexical_Env := Rebind_Env (Base_Env, E_Info);
   begin
      Dec_Ref (Base_Env);
      return Result;
   end Node_Env;

   ------------
   -- Parent --
   ------------

   function Parent
     (Node   : access ${root_node_value_type}'Class;
      E_Info : Entity_Info := No_Entity_Info) return Entity is
   begin
      --  TODO: shed entity information as appropriate
      return (Node.Parent, E_Info);
   end Parent;

   -------------
   -- Parents --
   -------------

   function Parents
     (Node   : access ${root_node_value_type}'Class;
      E_Info : Entity_Info := No_Entity_Info) return Entity_Array_Access
   is
      Bare_Parents : ${root_node_array.name} := Node.Parents;
      Result       : Entity_Array_Access := Create (Bare_Parents.N);
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
     (Node   : access ${root_node_value_type}'Class;
      E_Info : Entity_Info := No_Entity_Info) return Entity_Array_Access
   is
      Bare_Children : ${root_node_array.name} := Node.Children;
      Result        : Entity_Array_Access := Create (Bare_Children.N);
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

   procedure Assign_Names_To_Logic_Vars
     (Node : access ${root_node_value_type}'Class)
   is

      procedure Assign
        (Node  : access ${root_node_value_type}'Class;
         LV    : in out Logic_Var_Record;
         Field : String);
      --  Assign a name to the LV logic variable. Node must be the node that
      --  owns LV, and Field must be the name of the field in Node that holds
      --  LV.

      ------------
      -- Assign --
      ------------

      procedure Assign
        (Node  : access ${root_node_value_type}'Class;
         LV    : in out Logic_Var_Record;
         Field : String) is
      begin
         LV.Dbg_Name := new String'(Image (Node.Short_Image) & "." & Field);
      end Assign;

      K : constant ${root_node_kind_name} := Node.Kind;

   begin
      <%
          def get_actions(astnode, node_expr):
              return '\n'.join(
                  'Assign ({node}, {node}.{field}, "{field}");'.format(
                     node=node_expr,
                     field=field.name
                  )
                  for field in astnode.get_user_fields(
                      include_inherited=False)
                  if field.type.is_logic_var_type
              )
      %>
      ${ctx.generate_actions_for_hierarchy('Node', 'K', get_actions)}
      for Child of ${root_node_array.api_name}'(Children (Node)) loop
         if Child /= null then
            Assign_Names_To_Logic_Vars (Child);
         end if;
      end loop;
   end Assign_Names_To_Logic_Vars;

   -----------
   -- Image --
   -----------

   function Image (Ent : ${T.entity.name}) return Text_Type is
   begin
      if Ent.El /= null then
         declare
            Node_Image : constant Text_Type := Ent.El.Short_Image;
         begin
            return
            (if Ent.Info.Rebindings /= null
             then "<| "
             & Node_Image (Node_Image'First + 1 .. Node_Image'Last -1) & " "
             & AST_Envs.Image (Ent.Info.Rebindings) & " |>"
             else Node_Image);
         end;
      else
         return "None";
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (Ent : ${T.entity.name}) return String is
      Result : constant Text_Type := Image (Ent);
   begin
      return Image (Result);
   end Image;

   ---------------
   -- Can_Reach --
   ---------------

   function Can_Reach (El, From : ${root_node_type_name}) return Boolean is
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

   ------------
   -- Create --
   ------------

   function Create
      (El : ${root_node_type_name}; Info : Entity_Info)
       return Entity is
    begin
      return (El => El, Info => Info);
    end Create;

   procedure Destroy_Synthetic_Node (Node : in out ${root_node_type_name});
   --  Helper for the Register_Destroyable above

   ------------
   -- Length --
   ------------

   function Length
     (Node : access ${generic_list_value_type}'Class) return Natural
   is (Node.Abstract_Children_Count);

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

      function Trace_Image (E : Entity) return String is
      begin
         if E.El = null then
            return "None";
         else
            return ("<|" & Trace_Image (E.El, Decoration => False)
                    & " " & Trace_Image (E.Info) & "|>");
         end if;
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Info : Entity_Info) return String is
      begin
         return ("(MD => " & Trace_Image (Info.MD)
                 & ", Rebindings => " & Trace_Image (Info.Rebindings));
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (R : Env_Rebindings) return String is
      begin
         return Image (Image (R));
      end Trace_Image;

      -----------------
      -- Trace_Image --
      -----------------

      function Trace_Image (Unit : Analysis_Unit) return String is
      begin
         return "Analysis_Unit (""" & Basename (Unit) & """)";
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

   procedure Destroy_Synthetic_Node (Node : in out ${root_node_type_name}) is
      procedure Free is new Ada.Unchecked_Deallocation
        (${root_node_value_type}'Class, ${root_node_type_name});
   begin
      --  Don't call Node.Destroy, as Node's children may be gone already: they
      --  have their own destructor and there is no specified order for the
      --  call of these destructors.
      Node.Free_Extensions;
      Node.Reset_Logic_Vars;

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
        (Node       : access ${root_node_value_type}'Class;
         Decoration : Boolean := True) return String is
      begin
         if Node = null then
            return "None";
         else
            declare
               Result : constant String :=
                 (Node.Kind_Name & " "
                  & Basename (Node.Unit) & ":"
                  & Image (Node.Sloc_Range));
            begin
               return (if Decoration then "<" & Result & ">" else Result);
            end;
         end if;
      end Trace_Image;
   % endif

   Kind_Names : array (${root_node_kind_name}) of Unbounded_String :=
     (${", \n".join(cls.ada_kind_name
                    + " => To_Unbounded_String (\""
                    + cls.repr_name() + "\")"
                    for cls in ctx.astnode_types if not cls.abstract)});

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name
     (Node : access ${root_node_value_type}'Class) return String
   is
   begin
      return To_String (Kind_Names (Node.Kind));
   end Kind_Name;

   -----------------------------
   -- Abstract_Children_Count --
   -----------------------------

   function Abstract_Children_Count
     (Node : access ${root_node_value_type}) return Natural
   is
      C : Integer := Kind_To_Node_Children_Count (Node.Kind);
   begin
      if C = -1 then
         declare
            N : constant ${root_node_type_name} :=
               ${root_node_type_name} (Node);
         begin
            return ${generic_list_type_name} (N).Count;
         end;
      else
         return C;
      end if;
   end Abstract_Children_Count;

   -------------------
   -- Abstract_Kind --
   -------------------

   overriding function Abstract_Kind
     (Node : access ${root_node_value_type}) return ${root_node_kind_name} is
   begin
      return Node.Kind;
   end Abstract_Kind;

   --------------------
   -- Abstract_Child --
   --------------------

   overriding function Abstract_Child
     (Node  : access ${root_node_value_type};
      Index : Positive) return Abstract_Node is
   begin
      return Abstract_Node (${root_node_type_name}'(Node.Child (Index)));
   end Abstract_Child;

   -------------------
   -- Abstract_Text --
   -------------------

   overriding function Abstract_Text
     (Node : access ${root_node_value_type}) return Text_Type is
   begin
      return Node.Text;
   end Abstract_Text;

   -----------------------------
   -- Abstract_Rewritten_Node --
   -----------------------------

   overriding function Abstract_Rewritten_Node
     (Node : access ${root_node_value_type}) return ${root_node_type_name} is
   begin
      return null;
   end Abstract_Rewritten_Node;

   ----------------------
   -- Reset_Logic_Vars --
   ----------------------

   procedure Reset_Logic_Vars (Node : access ${root_node_value_type}'Class) is

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

      K : constant ${root_node_kind_name} := Node.Kind;

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

   ----------
   -- Unit --
   ----------

   function Unit
     (Node : ${root_entity.api_name}'Class) return Analysis_Unit is
   begin
      return Node.Node.Unit;
   end Unit;

   ----------------
   -- Token_Data --
   ----------------

   function Token_Data (Unit : Analysis_Unit) return Token_Data_Handler_Access
   is (Unit.TDH'Access);

   ---------------------
   -- Symbol_Literals --
   ---------------------

   function Symbol_Literals
     (Context : Analysis_Context) return Symbol_Literal_Array_Access
   is
   begin
      return Context.Symbol_Literals'Access;
   end Symbol_Literals;

   -------------------
   -- Lookup_Symbol --
   -------------------

   function Lookup_Symbol
     (Context : Analysis_Context; Symbol : Text_Type) return Symbol_Type is
   begin
      return Find (Context.Symbols, Symbol);
   end Lookup_Symbol;

   -------------------------
   -- Create_Special_Unit --
   -------------------------

   function Create_Special_Unit
     (Context             : Analysis_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Analysis_Unit
   is
      Unit : Analysis_Unit := new Analysis_Unit_Type'
        (Context           => Context,
         Ref_Count         => 1,
         AST_Root          => null,
         File_Name         => Normalized_Filename,
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

   function Templates_Unit (Context : Analysis_Context) return Analysis_Unit is
   begin
      if Context.Templates_Unit = No_Analysis_Unit then
         Context.Templates_Unit := Create_Special_Unit
           (Context             => Context,
            Normalized_Filename => GNATCOLL.VFS.No_File,
            Charset             => Default_Charset,
            Rule                =>
               ${Name.from_lower(ctx.main_rule_name)}_Rule);
      end if;
      return Context.Templates_Unit;
   end Templates_Unit;

   --------------
   -- Set_Rule --
   --------------

   procedure Set_Rule (Unit : Analysis_Unit; Rule : Grammar_Rule) is
   begin
      Unit.Rule := Rule;
   end Set_Rule;

   ------------------------------
   -- Normalized_Unit_Filename --
   ------------------------------

   function Normalized_Unit_Filename
     (Context : Analysis_Context; Filename : String)
      return GNATCOLL.VFS.Virtual_File
   is
      use GNATCOLL.VFS;
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
     (Unit    : Analysis_Unit;
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
     (Unit : Analysis_Unit; Node : ${root_node_type_name})
   is
      procedure Helper is new Register_Destroyable_Gen
        (${root_node_value_type}'Class,
         ${root_node_type_name},
         Destroy_Synthetic_Node);
   begin
      Helper (Unit, Node);
   end Register_Destroyable;

   ------------
   -- Create --
   ------------

   function Create
     (Node   : ${root_node_type_name};
      E_Info : Entity_Info := No_Entity_Info) return ${root_entity.api_name}
   is
   begin
      return (Node, Convert (E_Info));
   end Create;

   ---------------
   -- Bare_Node --
   ---------------

   function Bare_Node
     (Node : ${root_entity.api_name}'Class) return ${root_node_type_name} is
   begin
      return Node.Node;
   end Bare_Node;

   -----------------------
   -- Invalidate_Caches --
   -----------------------

   procedure Invalidate_Caches (Context : Analysis_Context) is
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
   end Invalidate_Caches;

   ------------------
   --  Reset_Envs  --
   ------------------

   procedure Reset_Envs (Unit : Analysis_Unit) is

      procedure Deactivate_Refd_Envs
        (Node : access ${root_node_value_type}'Class);
      procedure Recompute_Refd_Envs
        (Node : access ${root_node_value_type}'Class);

      --------------------------
      -- Deactivate_Refd_Envs --
      --------------------------

      procedure Deactivate_Refd_Envs
        (Node : access ${root_node_value_type}'Class) is
      begin
         if Node = null then
            return;
         end if;
         Deactivate_Referenced_Envs (Node.Self_Env);
         for I in 1 .. Node.Abstract_Children_Count loop
            Deactivate_Refd_Envs (Node.Child (I));
         end loop;
      end Deactivate_Refd_Envs;

      -------------------------
      -- Recompute_Refd_Envs --
      -------------------------

      procedure Recompute_Refd_Envs
        (Node : access ${root_node_value_type}'Class) is
      begin
         if Node = null then
            return;
         end if;
         Recompute_Referenced_Envs (Node.Self_Env);
         for I in 1 .. Node.Abstract_Children_Count loop
            Recompute_Refd_Envs (Node.Child (I));
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
      use GNATCOLL.VFS;
   begin
      return +Create (+Filename).Base_Name;
   end Basename;

   --------------
   -- Basename --
   --------------

   function Basename (Unit : Analysis_Unit) return String is
      use GNATCOLL.VFS;
   begin
      return +Unit.File_Name.Base_Name;
   end Basename;

   ------------------
   -- Reset_Caches --
   ------------------

   procedure Reset_Caches (Unit : Analysis_Unit) is
   begin
      if Unit.Cache_Version < Unit.Context.Cache_Version then
         Traces.Trace
           (Main_Trace,
            "In reset caches for unit " & Basename (Unit));
         Unit.Cache_Version := Unit.Context.Cache_Version;
         Reset_Envs (Unit);
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
        (Unit   : Analysis_Unit;
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

   procedure Reference_Unit (From, Referenced : Analysis_Unit) is
      Dummy : Boolean;
   begin
      Dummy := Analysis_Unit_Sets.Add (From.Referenced_Units, Referenced);
   end Reference_Unit;

   ------------------------
   -- Is_Referenced_From --
   ------------------------

   function Is_Referenced_From
     (Referenced, Unit : Analysis_Unit) return Boolean is
   begin
      if Unit = null or else Referenced = null then
         return False;
      elsif Unit = Referenced then
         return True;
      else
         return Analysis_Unit_Sets.Has (Unit.Referenced_Units, Referenced);
      end if;
   end Is_Referenced_From;

   ----------------
   -- Do_Parsing --
   ----------------

   procedure Do_Parsing
     (Unit        : Analysis_Unit;
      Read_BOM    : Boolean;
      Init_Parser :
        access procedure (Unit     : Analysis_Unit;
                          Read_BOM : Boolean;
                          Parser   : in out Parser_Type);
      Result      : out Reparsed_Unit)
   is
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
      Traces.Trace (Main_Trace, "Parsing unit " & Basename (Unit));

      Result.AST_Root := null;

      Move (Saved_TDH, Unit_TDH.all);
      Initialize (Unit_TDH.all, Saved_TDH.Symbols);

      --  This is where lexing occurs, so this is where we get most "setup"
      --  issues: missing input file, bad charset, etc. If we have such an
      --  error, catch it, turn it into diagnostics and abort parsing.

      declare
         use Ada.Exceptions;
      begin
         Init_Parser (Unit, Read_BOM, Unit.Context.Parser);
      exception
         when Exc : Name_Error =>
            --  This happens when we cannot open the source file for lexing:
            --  return a unit anyway with diagnostics indicating what happens.

            Traces.Trace
              (Main_Trace,
               "WARNING: Could not open file " & Basename (Unit));

            Add_Diagnostic (Exception_Message (Exc));
            Rotate_TDH;
            return;

         when Lexer.Unknown_Charset =>
            Add_Diagnostic
              ("Unknown charset """ & To_String (Unit.Charset) & """");
            Rotate_TDH;
            return;

         when Lexer.Invalid_Input =>
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

      Result.AST_Root := ${root_node_type_name}
        (Parse (Unit.Context.Parser, Rule => Unit.Rule));
      Result.Diagnostics.Append (Unit.Context.Parser.Diagnostics);
      Rotate_TDH;
   end Do_Parsing;

   --------------------------
   -- Update_After_Reparse --
   --------------------------

   procedure Update_After_Reparse
     (Unit : Analysis_Unit; Reparsed : in out Reparsed_Unit) is
   begin
      --  Replace Unit's diagnostics by Reparsed's
      Unit.Diagnostics := Reparsed.Diagnostics;
      Reparsed.Diagnostics.Clear;

      --  Likewise for token data
      Free (Unit.TDH);
      Move (Unit.TDH, Reparsed.TDH);

      --  As (re-)loading a unit can change how any AST node property in the
      --  whole analysis context behaves, we have to invalidate caches. This is
      --  likely overkill, but kill all caches here as it's easy to do.
      Invalidate_Caches (Unit.Context);

      --  Reparsing will invalidate all lexical environments related to this
      --  unit, so destroy all related rebindings as well. This browses AST
      --  nodes, so we have to do this before destroying the old AST nodes
      --  pool.
      Destroy_Rebindings (Unit.Rebindings'Access);

      --  Destroy the old AST node and replace it by the new one
      if Unit.AST_Root /= null then
         Unit.AST_Root.Destroy;
      end if;
      Unit.AST_Root := Reparsed.AST_Root;

      --  Likewise for memory pools
      Free (Unit.AST_Mem_Pool);
      Unit.AST_Mem_Pool := Reparsed.AST_Mem_Pool;
      Reparsed.AST_Mem_Pool := No_Pool;

      --  If Unit had its lexical environments populated, schedule a lexical
      --  environment update for Unit.
      if Unit.Is_Env_Populated then
         Unit.Context.Populate_Lexical_Env_Queue.Include (Unit);
      end if;
   end Update_After_Reparse;

   -------------------------------
   -- Destroy_Unit_Destroyables --
   -------------------------------

   procedure Destroy_Unit_Destroyables (Unit : Analysis_Unit) is
   begin
      for D of Unit.Destroyables loop
         D.Destroy (D.Object);
      end loop;
      Destroyable_Vectors.Clear (Unit.Destroyables);
   end Destroy_Unit_Destroyables;

   --------------------------------------
   -- Flush_Populate_Lexical_Env_Queue --
   --------------------------------------

   procedure Flush_Populate_Lexical_Env_Queue (Context : Analysis_Context) is
      Foreign_Nodes : ${root_node_type_name}_Vectors.Vector :=
         ${root_node_type_name}_Vectors.Empty_Vector;
   begin
      Traces.Trace (Main_Trace, "Flushing the populate lexical env queue");
      Traces.Increase_Indent (Main_Trace);
      Context.In_Populate_Lexical_Env := True;

      --  Remove traces of queued units in other units' lexical environments
      --  and collect information about other units' nodes in queued units'
      --  lexical environments.
      for Unit of Context.Populate_Lexical_Env_Queue loop
         Traces.Trace
           (Main_Trace, "Remove exiled entries and extract foreign nodes for: "
                        & Basename (Unit));

         --  Remove the `symbol -> AST node` association affecting environments
         --  that don't belong to the set of units to re-populate.
         Remove_Exiled_Entries (Unit);

         --  Collect all nodes that are foreign to the units to re-populate.
         --  Exclude them from the corresponding lists of exiled entries.
         Extract_Foreign_Nodes (Unit, Foreign_Nodes);

         --  Reset the flag so that the call to Populate_Lexical_Env below does
         --  its work, and increment unit version number to invalidate caches.
         Unit.Is_Env_Populated := False;
         Unit.Unit_Version := Unit.Unit_Version + 1;

         --  Now that Unit has been reparsed, we can destroy all its
         --  destroyables, which refer to the old tree (i.e. dangling
         --  pointers).
         Destroy_Unit_Destroyables (Unit);
      end loop;

      --  Reroot all foreign nodes. Do this before we re-run PLE on queued
      --  units so that we get a chance to run PLE in dependency order.
      Traces.Trace (Main_Trace, "Reroot foreign nodes (PLE queue flush)");
      Traces.Increase_Indent (Main_Trace);
      for FN of Foreign_Nodes loop
         declare
            use GNATCOLL.VFS;

            Node_Image : constant String := Image (FN.Short_Image);
            Unit_Name  : constant String := +FN.Unit.File_Name.Base_Name;
         begin
            Traces.Trace (Main_Trace, "Rerooting: " & Node_Image
                                      & " (from " & Unit_Name & ")");
         end;
         Reroot_Foreign_Node (FN);
      end loop;
      Traces.Decrease_Indent (Main_Trace);

      --  Recreate the lexical env structure for queued units, unless they were
      --- removed.
      for Unit of Context.Populate_Lexical_Env_Queue loop
         if Context.Units.Contains (Unit.File_Name) then
            Populate_Lexical_Env (Unit);
         end if;
      end loop;

      Foreign_Nodes.Destroy;
      Context.Populate_Lexical_Env_Queue.Clear;
      Context.In_Populate_Lexical_Env := False;
      Traces.Decrease_Indent (Main_Trace);
   end Flush_Populate_Lexical_Env_Queue;

   ---------------------------
   -- Remove_Exiled_Entries --
   ---------------------------

   procedure Remove_Exiled_Entries (Unit : Analysis_Unit) is
   begin
      for EE of Unit.Exiled_Entries loop
         if EE.Env.Owner = No_Analysis_Unit
              or else
            not Unit.Context.Populate_Lexical_Env_Queue.Contains (EE.Env.Owner)
         then
            AST_Envs.Remove (EE.Env, EE.Key, EE.Node);

            --  Also strip foreign nodes information from "outer" units so that
            --  it does not contain stale information (i.e. dangling pointers
            --  to nodes that belong to the units in the queue).
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
         end if;
      end loop;

      Unit.Exiled_Entries.Clear;
   end Remove_Exiled_Entries;

   ---------------------------
   -- Extract_Foreign_Nodes --
   ---------------------------

   procedure Extract_Foreign_Nodes
     (Unit          : Analysis_Unit;
      Foreign_Nodes : in out ${root_node_type_name}_Vectors.Vector) is
   begin
      for FN of Unit.Foreign_Nodes loop
         if not Unit.Context.Populate_Lexical_Env_Queue.Contains (FN.Unit) then
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
         end if;
      end loop;
      Unit.Foreign_Nodes.Clear;
   end Extract_Foreign_Nodes;

   --------------------------
   -- Reroot_Foreign_Nodes --
   --------------------------

   procedure Reroot_Foreign_Node (Node : access ${root_node_value_type}'Class)
   is
      Unit : constant Analysis_Unit := Node.Unit;
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
            Node.Pre_Env_Actions (Node.Self_Env, Root_Scope, True);
      begin
         Node.Post_Env_Actions (Env, Root_Scope);
      end;
   end Reroot_Foreign_Node;

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
     (Context : Analysis_Context) return Rewriting_Handle_Pointer is
   begin
      return Context.Rewriting_Handle;
   end Get_Rewriting_Handle;

   --------------------------
   -- Set_Rewriting_Handle --
   --------------------------

   procedure Set_Rewriting_Handle
     (Context : Analysis_Context; Handle : Rewriting_Handle_Pointer) is
   begin
      Context.Rewriting_Handle := Handle;
   end Set_Rewriting_Handle;

end ${ada_lib_name}.Analysis.Implementation;
