## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />
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

${(exts.with_clauses(with_clauses + [
   ((ctx.env_hook_subprogram.unit_fqn, False)
    if ctx.env_hook_subprogram else None)
]))}

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
               Env.Env.Node.Unit.Foreign_Nodes.Append (Mapping.F_Val);
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
      <%
         matchers = list(reversed([
            n for n in ctx.astnode_types
            if n.env_spec and not n.is_env_spec_inherited
         ]))
         matched_types, _ = ctx.collapse_concrete_nodes(
            ctx.root_grammar_class, matchers
         )
      %>
      case Self.Kind is
         % for matcher, matched in zip(matchers, matched_types):
            % if matched:
               when ${ctx.astnode_kind_set(matched)} =>
                  return ${matcher.name}_Pre_Env_Actions
                    (${matcher.name} (Self),
                     Bound_Env, Root_Env, Add_To_Env_Only);
            % endif
         % endfor
         when others => return Null_Lexical_Env;
      end case;
   end Pre_Env_Actions;

   ----------------------
   -- Post_Env_Actions --
   ----------------------

   procedure Post_Env_Actions
     (Self                : access ${root_node_value_type}'Class;
      Bound_Env, Root_Env : AST_Envs.Lexical_Env) is
   begin
      <%
         matchers = list(reversed([
            n for n in ctx.astnode_types
            if n.env_spec and
               not n.is_env_spec_inherited
               and n.env_spec.post_actions
         ]))
         matched_types, _ = ctx.collapse_concrete_nodes(
            ctx.root_grammar_class, matchers
         )
      %>
      case Self.Kind is
         % for matcher, matched in zip(matchers, matched_types):
            % if matched:
               when ${ctx.astnode_kind_set(matched)} =>
                  ${matcher.name}_Post_Env_Actions
                    (${matcher.name} (Self), Bound_Env, Root_Env);
            % endif
         % endfor
         when others => null;
      end case;
   end Post_Env_Actions;

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

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Node : access ${root_node_value_type}'Class) return Analysis_Unit is
   begin
      return Node.Unit;
   end Get_Unit;

   ${array_types.body(T.LexicalEnvType.array)}
   ${array_types.body(T.root_node.entity.array)}

   function Lookup_Internal
     (Node : ${root_node_type_name};
      Sloc : Source_Location;
      Snap : Boolean := False) return ${root_node_type_name};
   procedure Lookup_Relative
     (Node       : ${root_node_type_name};
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out ${root_node_type_name};
      Snap       : Boolean := False);
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

      for I in 1 .. Node.Child_Count loop
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
      for I in 1 .. Node.Child_Count loop
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
            for I in 1 .. Child_Count (Node) loop
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
     (Node : access ${root_node_value_type}'Class;
      Snap : Boolean := False) return Source_Location_Range
   is
      TDH                  : Token_Data_Handler renames
         Convert (Node.Unit).TDH;
      Sloc_Start, Sloc_End : Source_Location;

      function Get
        (Index : Token_Index) return Lexer.Token_Data_Type is
        (Get_Token (TDH, Index));

   begin
      if Node.Is_Synthetic then
         return Sloc_Range (Node.Parent);
      end if;

      --  Snapping: We'll go one token before the start token, and one token
      --  after the end token, and the sloc range will extend from the end of
      --  the start token to the start of the end token, including any
      --  whitespace and trivia that might be surrounding the node.
      --
      --  TODO: Only nodes we're gonna try to snap are nodes with default
      --  anchors. However, we can make the logic more specific, eg:
      --
      --  * If the start anchor is beginning, then snap the start sloc.
      --
      --  * If the end anchor is ending, then snap the end sloc.
      --
      --  This way composite cases can work too.

      if Snap then
         declare
            Tok_Start : constant Token_Index :=
              Token_Index'Max (Node.Token_Start_Index - 1, 0);
            Tok_End : constant Token_Index :=
              Token_Index'Min (Node.Token_End_Index + 1, Last_Token (TDH));
         begin
            Sloc_Start := End_Sloc (Get (Tok_Start).Sloc_Range);
            Sloc_End := Start_Sloc (Get (Tok_End).Sloc_Range);
         end;
      else
         Sloc_Start := Start_Sloc (Get (Node.Token_Start_Index).Sloc_Range);
         Sloc_End :=
           (if Node.Token_End_Index /= No_Token_Index
            then End_Sloc (Get (Node.Token_End_Index).Sloc_Range)
            else Start_Sloc (Get (Node.Token_Start_Index).Sloc_Range));
      end if;
      return Make_Range (Sloc_Start, Sloc_End);
   end Sloc_Range;

   ------------
   -- Lookup --
   ------------

   function Lookup (Node : access ${root_node_value_type}'Class;
                    Sloc : Source_Location;
                    Snap : Boolean := False) return ${root_node_type_name}
   is
      Position : Relative_Position;
      Result   : ${root_node_type_name};
   begin
      if Sloc = No_Source_Location then
         return null;
      end if;

      Lookup_Relative
        (${root_node_type_name} (Node), Sloc, Position, Result, Snap);
      return Result;
   end Lookup;

   ---------------------
   -- Lookup_Internal --
   ---------------------

   function Lookup_Internal
     (Node : ${root_node_type_name};
      Sloc : Source_Location;
      Snap : Boolean := False) return ${root_node_type_name}
   is
      --  For this implementation helper (i.e. internal primitive), we can
      --  assume that all lookups fall into this node's sloc range.
      pragma Assert (Compare (Sloc_Range (Node, Snap), Sloc) = Inside);

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
            Lookup_Relative (Child, Sloc, Pos, Result, Snap);
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

   function Compare (Node : access ${root_node_value_type}'Class;
                     Sloc : Source_Location;
                     Snap : Boolean := False) return Relative_Position is
   begin
      return Compare (Sloc_Range (Node, Snap), Sloc);
   end Compare;

   ---------------------
   -- Lookup_Relative --
   ---------------------

   procedure Lookup_Relative
     (Node       : ${root_node_type_name};
      Sloc       : Source_Location;
      Position   : out Relative_Position;
      Node_Found : out ${root_node_type_name};
      Snap       : Boolean := False)
   is
      Result : constant Relative_Position :=
        Compare (Node, Sloc, Snap);
   begin
      Position := Result;
      Node_Found := (if Result = Inside
                     then Lookup_Internal (Node, Sloc, Snap)
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
      Last  : constant Integer := First + Child_Count (Node) - 1;
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
     (Node     : access ${root_node_value_type}'Class;
      Root_Env : AST_Envs.Lexical_Env) return Boolean
   is

      function Populate_Internal
        (Node      : access ${root_node_value_type}'Class;
         Bound_Env : Lexical_Env) return Boolean;
      --  Do the lexical env population on Node and recurse on its children

      procedure Default_Initialize
        (Node : access ${root_node_value_type}'Class);
      --  If Node.Self_Env is left null, initialize it to Empty_Env. Recurse on
      --  children.

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

            --  Call recursively on children
            for C of ${root_node_array.api_name}'(Children (Node)) loop
               Result := Populate_Internal (C, Node.Self_Env) or else Result;
            end loop;

            Node.Post_Env_Actions (Initial_Env, Root_Env);
         exception
            when Property_Error =>
               Default_Initialize (Node);
               return True;
         end;

         return Result;
      end Populate_Internal;

      ------------------------
      -- Default_Initialize --
      ------------------------

      procedure Default_Initialize
        (Node : access ${root_node_value_type}'Class) is
      begin
         if Node = null then
            return;
         end if;

         if Node.Self_Env.Env = null then
            Node.Self_Env := Empty_Env;
         end if;

         for C of ${root_node_array.api_name}'(Children (Node)) loop
            Default_Initialize (C);
         end loop;
      end Default_Initialize;

      Env : AST_Envs.Lexical_Env := Root_Env;
   begin

      --  If we reach this point, one caller is supposed to have set the
      --  following flag.
      if not Node.Unit.Context.In_Populate_Lexical_Env then
         raise Program_Error;
      end if;

      return Populate_Internal (Node, Env);
   end Populate_Lexical_Env;

   ----------------------------
   -- AST_Envs_Element_Image --
   ----------------------------

   function AST_Envs_Element_Image
     (Node  : ${root_node_type_name};
      Short : Boolean := True) return Text_Type
   is 
     (if Short
      then To_Text (To_String (Node.Unit.File_Name))
           & ":" & To_Text (Image (Start_Sloc (Sloc_Range (Node))))
      else Node.Short_Image);

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
     (Node : access ${root_node_value_type}'Class) return Text_Type is
   begin
      <%
         matchers = list(reversed([
            n for n in ctx.astnode_types
            if n.annotations.custom_short_image
         ]))
         matched_types, _ = ctx.collapse_concrete_nodes(
            ctx.root_grammar_class, matchers
         )
      %>
      case Node.Kind is
         % for matcher, matched in zip(matchers, matched_types):
            % if matched:
               when ${ctx.astnode_kind_set(matched)} =>
                  return ${matcher.name}_Short_Image
                    (${matcher.name} (Node));
            % endif
         % endfor
         when others =>
            return "<" & To_Text (Node.Kind_Name)
                   & " " & To_Text (Image (Node.Sloc_Range)) & ">";
      end case;
   end Short_Image;

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
   is (Node.Child_Count);

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

     if Node.all not in ${generic_list_value_type}'Class then
        New_Line;
     end if;

      <%
        root_type = ctx.root_grammar_class.name

        def get_actions(astnode, node_expr):
            repr_fields = astnode.get_parse_fields(
                lambda f: f.repr,
                include_inherited=False
            )

            result = []

            # Emit only one processing code for all list types: no need to
            # repeat it multiple times.
            if astnode.is_generic_list_type:
                result.append("""
                    if {node}.Count = 0 then
                       Put_Line (": <empty list>");
                       return;
                    end if;

                    New_Line;
                    for Child of {node}.Nodes (1 .. {node}.Count) loop
                       if Child /= null then
                          Child.Print (Show_Slocs, Line_Prefix & "|  ");
                       end if;
                    end loop;
                """.format(node=node_expr))
            elif astnode.is_list:
                pass

            elif repr_fields:
                for i, field in enumerate(repr_fields):
                    if field.type.is_ptr:
                        handler = """
                           Put (Attr_Prefix & "{print_name}:");
                           if {node}.{field_name} /= null then
                              New_Line;
                              {node}.{field_name}.Print
                                (Show_Slocs, Children_Prefix);
                           else
                              Put_Line (" <null>");
                           end if;
                        """

                    elif field.type.is_token_type:
                        handler = """
                           Put_Line
                             (Attr_Prefix & "{print_name}: "
                              & Text ({field_name} ({node})));
                        """

                    else:
                        handler = """
                           Put_Line (Attr_Prefix & "{print_name}: "
                                     & Image ({node}.{field_name}));
                        """

                    result.append(handler.format(
                        node=node_expr,
                        print_name=field._name.lower,
                        field_name=field.name
                    ))
            return '\n'.join(result)
      %>
      ${ctx.generate_actions_for_hierarchy('Node', 'K', get_actions)}
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
      Token : Token_Type)
      return Token_Index
   is
   begin
      if Convert (Node.Unit).TDH'Access /= Token.TDH then
         raise Property_Error with
           ("Cannot associate a token and a node from different analysis"
            & " units");
      elsif Token.Trivia /= No_Token_Index then
         raise Property_Error with
           ("A node cannot hold trivia");
      end if;

      return Token.Token;
   end Stored_Token;

   --------------
   -- Is_Ghost --
   --------------

   function Is_Ghost
     (Node : access ${root_node_value_type}'Class) return Boolean
   is (Node.Token_End_Index = No_Token_Index);

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
     (Node : access ${root_node_value_type}'Class)
      return Token_Type
   is (Node.Token (Node.Token_Start_Index));

   ---------------
   -- Token_End --
   ---------------

   function Token_End
     (Node : access ${root_node_value_type}'Class)
      return Token_Type
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
              Is_Trivia     => Token.Trivia /= No_Token_Index,
              Index         => (if Token.Trivia = No_Token_Index
                                then Token.Token
                                else Token.Trivia),
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
            Token  => Index,
            Trivia => No_Token_Index));

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
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_type_name}
   is
      N : constant Positive := Child_Index (Node) + 1;
   begin
      return (if N = 1
              then null
              else Node.Parent.Child (N - 1));
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling
     (Node : access ${root_node_value_type}'Class)
     return ${root_node_type_name}
   is
      N : constant Positive := Child_Index (Node) + 1;
   begin
      --  If Node is the last sibling, then Child will return null
      return Node.Parent.Child (N + 1);
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
   is (Group (Envs.Items, Env_Md));

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
               not n.is_env_spec_inherited and
               n.env_spec.adds_env
         ]
      %>
      Base_Env : Lexical_Env :=
         % if nodes_adding_env:
            (if Node.Kind in
               ${' | '.join(n.ada_kind_range_name for n in nodes_adding_env)}
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

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling
     (Node   : access ${root_node_value_type}'Class;
      E_Info : Entity_Info := No_Entity_Info) return Entity is
   begin
      return (Node.Previous_Sibling, E_Info);
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling
     (Node   : access ${root_node_value_type}'Class;
      E_Info : Entity_Info := No_Entity_Info) return Entity is
   begin
      return (Node.Next_Sibling, E_Info);
   end Next_Sibling;

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
   is (Node.Child_Count);

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
         return "Analysis_Unit (""" & Get_Filename (Unit) & """)";
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
                  & Get_Filename (Node.Unit) & ":"
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

   Kind_To_Counts : array (${root_node_kind_name}) of Integer :=
     (${", \n".join(cls.ada_kind_name
                    + " => {}".format(
                        len(cls.get_parse_fields(lambda f: f.type.is_ast_node))
                        if not cls.is_list_type
                        else -1
                    )
                    for cls in ctx.astnode_types if not cls.abstract)});

   -----------------
   -- Child_Count --
   -----------------

   function Child_Count
     (Node : access ${root_node_value_type}'Class) return Natural
   is
      C : Integer := Kind_To_Counts (Node.Kind);
   begin
      if C = -1 then
         return ${generic_list_type_name} (Node).Count;
      else
         return C;
      end if;
   end Child_Count;

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

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Node : ${root_entity.api_name}'Class) return Analysis_Unit is
   begin
      return Node.Node.Get_Unit;
   end Get_Unit;

   ----------------
   -- Token_Data --
   ----------------

   function Token_Data (Unit : Analysis_Unit) return Token_Data_Handler_Access
   is (Unit.TDH'Access);

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

   ------------------
   -- Reset_Caches --
   ------------------

   procedure Reset_Caches (Context : Analysis_Context) is
   begin
      --  Increase Context's version number. If we are about to overflow, reset
      --  all version numbers from analysis units.
      if Context.Cache_Version = Natural'Last then
         Context.Cache_Version := 1;
         for Unit of Context.Units_Map loop
            Unit.Cache_Version := 0;
         end loop;
      else
         Context.Cache_Version := Context.Cache_Version + 1;
      end if;
   end Reset_Caches;

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
         for I in 1 .. Node.Child_Count loop
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
         for I in 1 .. Node.Child_Count loop
            Recompute_Refd_Envs (Node.Child (I));
         end loop;
      end Recompute_Refd_Envs;

   begin
      --  First pass will deactivate every referenced envs that Unit possesses
      Deactivate_Refd_Envs (Unit.AST_Root);

      --  Second pass will recompute the env they are pointing to
      Recompute_Refd_Envs (Unit.AST_Root);
   end Reset_Envs;

   ------------------
   -- Reset_Caches --
   ------------------

   procedure Reset_Caches (Unit : Analysis_Unit) is
   begin
      if Unit.Cache_Version < Unit.Context.Cache_Version then
         Traces.Trace
           (Main_Trace,
            "In reset caches for unit " & To_String (Unit.File_Name));
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

      procedure Add_Diagnostic (Message : String);
      --  Helper to add a sloc-less diagnostic to Unit

      --------------------
      -- Add_Diagnostic --
      --------------------

      procedure Add_Diagnostic (Message : String) is
      begin
         Append (Result.Diagnostics, No_Source_Location_Range,
                 To_Text (Message));
      end Add_Diagnostic;

   begin
      Traces.Trace (Main_Trace, "Parsing unit " & To_String (Unit.File_Name));

      Result.AST_Root := null;

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
               "WARNING: Could not open file " & To_String (Unit.File_Name));

            Add_Diagnostic
              (Exception_Message (Exc));
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

      Result.AST_Mem_Pool := Create;
      Unit.Context.Parser.Mem_Pool := Result.AST_Mem_Pool;

      Result.AST_Root := ${root_node_type_name}
        (Parse (Unit.Context.Parser, Rule => Unit.Rule));
      Result.Diagnostics.Append (Unit.Context.Parser.Diagnostics);
   end Do_Parsing;

end ${ada_lib_name}.Analysis.Implementation;
