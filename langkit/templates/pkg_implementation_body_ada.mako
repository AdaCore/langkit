## vim: filetype=makoada

<%namespace name="array_types"       file="array_types_ada.mako" />
<%namespace name="astnode_types"     file="astnode_types_ada.mako" />
<%namespace name="enum_types"        file="enum_types_ada.mako" />
<%namespace name="list_types"        file="list_types_ada.mako" />
<%namespace name="struct_types"      file="struct_types_ada.mako" />
<%namespace name="memoization"       file="memoization_ada.mako" />

<% root_node_array = T.root_node.array %>

with Ada.Containers;                  use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

% if ctx.has_memoization:
with Langkit_Support.Hashes; use Langkit_Support.Hashes;
% endif
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

%if ctx.env_hook_subprogram:
with ${ctx.env_hook_subprogram.unit_fqn};
%endif

package body ${ada_lib_name}.Analysis.Implementation is

   function Convert is new Ada.Unchecked_Conversion
     (Entity_Info, Public_Entity_Info);

   type Internal_Analysis_Unit is access all Analysis_Unit_Type;
   function Convert is new Ada.Unchecked_Conversion
     (Analysis_Unit, Internal_Analysis_Unit);

   function Get_Lex_Env_Data
     (Node : access ${root_node_value_type}'Class) return Lex_Env_Data
   is (Lex_Env_Data (${ada_lib_name}.Analysis.Get_Lex_Env_Data (Node.Unit)));

   procedure Register_Destroyable is new Register_Destroyable_Gen
     (AST_Envs.Lexical_Env_Type, AST_Envs.Lexical_Env, AST_Envs.Destroy);

   ${array_types.body(root_node_array)}

   % for array_type in ctx.sorted_types(ctx.array_types):
   % if array_type.element_type.should_emit_array_type:
   ${array_types.body(array_type)}
   % endif
   % endfor

   % if ctx.has_memoization:
      ${memoization.body()}
   % endif

   ---------------------
   -- Pre_Env_Actions --
   ---------------------

   function Pre_Env_Actions
     (Self                : access ${root_node_value_type};
      Bound_Env, Root_Env : AST_Envs.Lexical_Env;
      Add_To_Env_Only     : Boolean := False) return AST_Envs.Lexical_Env
   is (null);

   ---------------------
   -- Is_Visible_From --
   ---------------------

   function Is_Visible_From
     (Referenced_Env, Base_Env : AST_Envs.Lexical_Env) return Boolean is
      Referenced_Node : constant ${root_node_type_name} := Referenced_Env.Node;
      Base_Node       : constant ${root_node_type_name} := Base_Env.Node;
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
      Node.Reset_Caches;
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
                               Data : in out Data_type)
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
      for C of N.Children_With_Trivia loop
         case C.Kind is
            when Trivia =>
               Put_Line (Children_Prefix & Text (C.Trivia));
            when Child =>
               C.Node.PP_Trivia (Children_Prefix);
         end case;
      end loop;
   end PP_Trivia;

   --------------------------
   -- Populate_Lexical_Env --
   --------------------------

   procedure Populate_Lexical_Env
     (Node     : access ${root_node_value_type}'Class;
      Root_Env : AST_Envs.Lexical_Env)
   is

      procedure Populate_Internal
        (Node      : access ${root_node_value_type}'Class;
         Bound_Env : Lexical_Env);

      -----------------------
      -- Populate_Internal --
      -----------------------

      procedure Populate_Internal
        (Node      : access ${root_node_value_type}'Class;
         Bound_Env : Lexical_Env)
      is
         Initial_Env : Lexical_Env;
      begin
         if Node = null then
            return;
         end if;

         --  By default (i.e. unless env actions add a new env),
         --  the environment we store in Node is the current one.
         Node.Self_Env := Bound_Env;

         Initial_Env := Node.Pre_Env_Actions (Bound_Env, Root_Env);

         --  Call recursively on children
         for C of ${root_node_array.api_name}'(Children (Node)) loop
            Populate_Internal (C, Node.Self_Env);
         end loop;

         Node.Post_Env_Actions (Initial_Env, Root_Env);
      end Populate_Internal;

      Env : AST_Envs.Lexical_Env := Root_Env;
   begin
      Populate_Internal (Node, Env);
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

   -----------------
   -- Short_Image --
   -----------------

   function Short_Image
     (Node : access ${root_node_value_type})
      return Text_Type
   is
      Self : access ${root_node_value_type}'Class := Node;
   begin
      return "<" & To_Text (Kind_Name (Self))
             & " " & To_Text (Image (Sloc_Range (Node))) & ">";
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
     (Envs : ${T.LexicalEnvType.array.name})
      return ${T.LexicalEnvType.name}
   is (Group (Envs.Items));

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
     (Node   : access ${root_node_value_type};
      E_Info : Entity_Info := No_Entity_Info) return Lexical_Env
   is (Rebind_Env (Node.Self_Env, E_Info));

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
    (Node : access ${root_node_value_type}'Class) is
   begin
      % for f in T.root_node.get_fields( \
           include_inherited=False, \
           predicate=lambda f: f.type.is_logic_var_type \
      ):
         Node.${f.name}.Dbg_Name :=
            new String'(Image (Node.Short_Image) & ".${f.name}");
      % endfor
      Assign_Names_To_Logic_Vars_Impl (Node);
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

   ---------------
   -- Get_Child --
   ---------------

   overriding procedure Get_Child
     (Node            : access ${generic_list_value_type};
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${root_node_type_name}) is
   begin
      if Index > Node.Count then
         Index_In_Bounds := False;
      else
         Index_In_Bounds := True;
         Result := Node.Nodes (Index);
      end if;
   end Get_Child;

   -----------
   -- Print --
   -----------

   overriding procedure Print
     (Node : access ${generic_list_value_type}; Line_Prefix : String := "")
   is
      Class_Wide_Node : constant ${root_node_type_name} :=
         ${root_node_type_name} (Node);
   begin
      Put
        (Line_Prefix & Class_Wide_Node.Kind_Name
         & "[" & Image (Node.Sloc_Range) & "]");
      if Node.Count = 0 then
         Put_Line (": <empty list>");
         return;
      end if;

      New_Line;
      for Child of Node.Nodes (1 .. Node.Count) loop
         if Child /= null then
            Child.Print (Line_Prefix & "|  ");
         end if;
      end loop;
   end Print;

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
      Node.Reset_Caches;

      Free (Node);
   end Destroy_Synthetic_Node;

   -----------
   -- Image --
   -----------

   function Image (Value : Boolean) return String
   is (if Value then "True" else "False");


   Kind_Names : array (${root_node_kind_name}) of Unbounded_String :=
     (${", \n".join(cls.ada_kind_name()
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
     (${", \n".join(cls.ada_kind_name()
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

   ------------------
   -- Reset_Caches --
   ------------------

   procedure Reset_Caches (Node : access ${root_node_value_type}'Class) is
      K : ${root_node_kind_name} := Node.Kind;
   begin
   case K is
   % for cls in ctx.astnode_types:
      % if not cls.abstract:
         <%
            logic_vars = [fld for fld in cls.get_user_fields()
                          if fld.type.is_logic_var_type]
         %>
         % if logic_vars:
            when ${cls.ada_kind_name()} =>
               declare
                  N : ${cls.name} := ${cls.name} (Node);
               begin
                  % if logic_vars:
                     % for field in logic_vars:
                        --  TODO??? Fix Adalog so that Destroy resets the
                        --  value it stores.
                        N.${field.name}.Value := No_Entity;
                        Eq_Node.Refs.Reset (N.${field.name});
                        Eq_Node.Refs.Destroy (N.${field.name});
                     % endfor
                  % endif
               end;
         % endif
      % endif
   % endfor
   when others => null;
   end case;
   end Reset_Caches;

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

end ${ada_lib_name}.Analysis.Implementation;
