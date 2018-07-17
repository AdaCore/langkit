## vim: filetype=makoada

<%namespace name="astnode_types"     file="astnode_types_ada.mako" />
<%namespace name="public_properties"
            file="properties/public_wrappers_ada.mako" />

<%def name="decls1()">

   function Is_Null (Node : ${root_entity.api_name}'Class) return Boolean;
   ${ada_doc('langkit.node_is_null', 3)}

   function Is_Token_Node
     (Node : ${root_entity.api_name}'Class) return Boolean;
   ${ada_doc('langkit.node_is_token_node', 3)}

   function "=" (L, R : ${root_entity.api_name}'Class) return Boolean;
   --  Return whether L and R designate the same entity

   function Short_Image
     (Node : ${root_entity.api_name}'Class) return Text_Type;
   function Short_Image (Node : ${root_entity.api_name}'Class) return String;
   --  Return a short string describing Node, or "None" if Node.Is_Null is
   --  true.

   function Image (Node : ${root_entity.api_name}'Class) return Text_Type;
   function Image (Node : ${root_entity.api_name}'Class) return String;
   --  Like Short_Image, also including its rebinding metadata

   function Hash
     (Node : ${root_entity.api_name}'Class) return Ada.Containers.Hash_Type;
   --  Generic hash function, to be used for nodes as keys in hash tables

   pragma Warnings (Off, "defined after private extension");
   % for e in ctx.entity_types:
      function As_${e.el_type.kwless_raw_name}
        (Node : ${root_entity.api_name}'Class) return ${e.api_name};
   % endfor
   pragma Warnings (On, "defined after private extension");

</%def>

<%def name="decls2()">

   type Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : ${root_entity.api_name};
         when Trivia =>
            Trivia : Token_Type;
      end case;
   end record;
   --  Variant that holds either an AST node or a token

   type Children_Array is array (Positive range <>) of Child_Record;

   function Children_With_Trivia
     (Node : ${root_entity.api_name}'Class) return Children_Array;
   --  Return the children of this node interleaved with Trivia token nodes, so
   --  that:
   --  - Every trivia contained between Node.Start_Token and Node.End_Token - 1
   --    will be part of the returned array;
   --  - Nodes and trivias will be lexically ordered.

</%def>

<%def name="decls3()">

   -------------------------
   -- AST Node primitives --
   -------------------------

   function Kind
     (Node : ${root_entity.api_name}'Class) return ${root_node_kind_name};
   function Kind_Name (Node : ${root_entity.api_name}'Class) return String;
   --  Return the concrete kind for Node

   pragma Warnings (Off, "defined after private extension");
   % for e in ctx.entity_types:

      % for f in e.el_type.get_parse_fields( \
         include_inherited=False, \
         predicate=lambda f: f.is_public, \
      ):
         ${astnode_types.field_decl(f)}
      % endfor

      % for p in e.el_type.get_properties( \
         include_inherited=False, \
         predicate=lambda p: p.is_public and not p.overriding \
      ):
         ${public_properties.decl(p)}
      % endfor

   % endfor
   pragma Warnings (On, "defined after private extension");

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   function Children_Count
     (Node : ${root_entity.api_name}'Class) return Natural;
   --  Return the number of children Node has

   function First_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural;
   --  Return the index of the first child Node has

   function Last_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural;
   --  Return the index of the last child Node has, or 0 if there is no child

   pragma Warnings (Off, "defined after private extension");
   procedure Get_Child
     (Node            : ${root_entity.api_name}'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out ${root_entity.api_name});
   --  Return the Index'th child of node, storing it into Result.
   --
   --  Child indexing is 1-based. Store in Index_In_Bounds whether Node had
   --  such a child: if not (i.e. Index is out-of-bounds), the content
   --  of Result is undefined.

   function Child
     (Node  : ${root_entity.api_name}'Class;
      Index : Positive)
      return ${root_entity.api_name};
   --  Return the Index'th child of Node, or null if Node has no such child
   pragma Warnings (On, "defined after private extension");

   function Traverse
     (Node  : ${root_entity.api_name}'Class;
      Visit : access function (Node : ${root_entity.api_name}'Class)
                               return Visit_Status)
     return Visit_Status;
   --  Given the parent node for a subtree, traverse all syntactic nodes of
   --  this tree, calling the given function on each node in prefix order (i.e.
   --  top-down). The order of traversing subtrees follows the order of
   --  declaration of the corresponding attributes in the grammar. The
   --  traversal is controlled as follows by the result returned by Visit:
   --
   --     Into   The traversal continues normally with the syntactic
   --            children of the node just processed.
   --
   --     Over   The children of the node just processed are skipped and
   --            excluded from the traversal, but otherwise processing
   --            continues elsewhere in the tree.
   --
   --     Stop   The entire traversal is immediately abandoned, and the
   --            original call to Traverse returns Stop.

   procedure Traverse
     (Node  : ${root_entity.api_name}'Class;
      Visit : access function (Node : ${root_entity.api_name}'Class)
                               return Visit_Status);
   --  This is the same as Traverse function except that no result is returned
   --  i.e. the Traverse function is called and the result is simply discarded.

   function Child_Index (Node : ${root_entity.api_name}'Class) return Natural;
   --  Return the 0-based index for Node in its parent's children

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : ${root_entity.api_name}'Class) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   pragma Warnings (Off, "defined after private extension");
   function Lookup
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return ${root_entity.api_name};
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.
   pragma Warnings (On, "defined after private extension");

   -----------------------
   -- Lexical utilities --
   -----------------------

   function Text (Node : ${root_entity.api_name}'Class) return Text_Type;
   --  Shortcut to get the source buffer slice corresponding to the text that
   --  spans between the first and last tokens of an AST node.

   function Text (Node : ${root_entity.api_name}'Class) return String;
   --  Overload to get the source buffer slice as a string

   function Token_Range
     (Node : ${root_entity.api_name}'Class) return Token_Iterator;
   --  Return an iterator on the range of tokens encompassed by Node

   -------------------
   -- Debug helpers --
   -------------------

   procedure Print
     (Node        : ${root_entity.api_name}'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.
   --
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia
     (Node        : ${root_entity.api_name}'Class;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : ${root_entity.api_name}'Class);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

</%def>

<%def name="bodies()">

   -------------
   -- Is_Null --
   -------------

   function Is_Null (Node : ${root_entity.api_name}'Class) return Boolean is
     (Node.Node = null);

   -------------------
   -- Is_Token_Node --
   -------------------

   function Is_Token_Node (Node : ${root_entity.api_name}'Class) return Boolean
   is (Node.Node.Is_Token_Node);

   ---------
   -- "=" --
   ---------

   function "=" (L, R : ${root_entity.api_name}'Class) return Boolean is
   begin
      return L.Node = R.Node and then L.E_Info = R.E_Info;
   end "=";

   -----------------
   -- Short_Image --
   -----------------

   function Short_Image
     (Node : ${root_entity.api_name}'Class) return Text_Type
   is (if Is_Null (Node) then "None" else Node.Node.Short_Image);

   function Short_Image (Node : ${root_entity.api_name}'Class) return String is
     (Image (Short_Image (Node)));

   -----------
   -- Image --
   -----------

   function Image (Node : ${root_entity.api_name}'Class) return Text_Type is
     (Image (${T.entity.name}'(Node.Node, Node.E_Info)));

   -----------
   -- Image --
   -----------

   function Image (Node : ${root_entity.api_name}'Class) return String is
     (Image (Image (Node)));

   ----------
   -- Hash --
   ----------

   function Hash
     (Node : ${root_entity.api_name}'Class) return Ada.Containers.Hash_Type
   is
      N : constant ${root_entity.name} := (Node.Node, Node.E_Info);
   begin
      return Hash (N);
   end Hash;

   -----------------------
   -- Entity converters --
   -----------------------

   % for e in ctx.entity_types:
      function As_${e.el_type.kwless_raw_name}
        (Node : ${root_entity.api_name}'Class) return ${e.api_name} is
      begin
         if Node.Node = null then
            return No_${e.api_name};
         elsif Node.Node.all in ${e.el_type.value_type_name()}'Class then
            return (Node => Node.Node, E_Info => Node.E_Info);
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
      return Node.Node.Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Node : ${root_entity.api_name}'Class) return String is
   begin
      return Node.Node.Kind_Name;
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
     (Node : ${root_entity.api_name}'Class) return Natural is begin
      return Node.Node.Abstract_Children_Count;
   end Children_Count;

   -----------------------
   -- First_Child_Index --
   -----------------------

   function First_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural is
   begin
      return Node.Node.First_Child_Index;
   end First_Child_Index;

   ----------------------
   -- Last_Child_Index --
   ----------------------

   function Last_Child_Index
     (Node : ${root_entity.api_name}'Class) return Natural is
   begin
      return Node.Node.Last_Child_Index;
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
      Node.Node.Get_Child (Index, Index_In_Bounds, N);
      Result := (N, Node.E_Info);
   end Get_Child;

   -----------
   -- Child --
   -----------

   function Child
     (Node  : ${root_entity.api_name}'Class;
      Index : Positive) return ${root_entity.api_name}
   is
   begin
      return (Node.Node.Child (Index), Node.E_Info);
   end Child;

   ----------------
   -- Sloc_Range --
   ----------------

   function Sloc_Range
     (Node : ${root_entity.api_name}'Class) return Source_Location_Range is
   begin
      return Node.Node.Sloc_Range;
   end Sloc_Range;

   -------------
   -- Compare --
   -------------

   function Compare
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return Relative_Position is
   begin
      return Node.Node.Compare (Sloc);
   end Compare;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Node : ${root_entity.api_name}'Class;
      Sloc : Source_Location) return ${root_entity.api_name} is
   begin
      return Create_Entity (Node.Node.Lookup (Sloc));
   end Lookup;

   ----------
   -- Text --
   ----------

   function Text (Node : ${root_entity.api_name}'Class) return Text_Type is
   begin
      return Text (Token_Start (Node), Token_End (Node));
   end Text;

   ----------
   -- Text --
   ----------

   function Text (Node : ${root_entity.api_name}'Class) return String is
   begin
      return Image (Text (Node));
   end Text;

   -----------------
   -- Token_Range --
   -----------------

   function Token_Range
     (Node : ${root_entity.api_name}'Class)
      return Token_Iterator is
   begin
      return Token_Iterator'(As_${T.root_node.kwless_raw_name} (Node),
                             Node.Node.Token_End_Index);
   end Token_Range;

   -----------
   -- Print --
   -----------

   procedure Print
     (Node        : ${root_entity.api_name}'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "") is
   begin
      Node.Node.Print (Show_Slocs, Line_Prefix);
   end Print;

   ---------------
   -- PP_Trivia --
   ---------------

   procedure PP_Trivia
     (Node        : ${root_entity.api_name}'Class;
      Line_Prefix : String := "") is
   begin
      Node.Node.PP_Trivia (Line_Prefix);
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
      E_Info : constant Entity_Info := Node.E_Info;

      -------------
      -- Wrapper --
      -------------

      function Wrapper
        (Node : access ${root_node_value_type}'Class) return Visit_Status
      is
         Public_Node : constant ${root_entity.api_name} :=
           Create_Entity (${root_node_type_name} (Node), E_Info);
      begin
         return Visit (Public_Node);
      end Wrapper;

   begin
      return Node.Node.Traverse (Wrapper'Access);
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
      return Node.Node.Child_Index;
   end Child_Index;

   --------------------------------
   -- Assign_Names_To_Logic_Vars --
   --------------------------------

   procedure Assign_Names_To_Logic_Vars (Node : ${root_entity.api_name}'Class)
   is
   begin
      Assign_Names_To_Logic_Vars (Node.Node);
   end Assign_Names_To_Logic_Vars;

   --------------------------
   -- Children_With_Trivia --
   --------------------------

   function Children_With_Trivia
     (Node : ${root_entity.api_name}'Class) return Children_Array
   is
      Bare_Result : constant Bare_Children_Array :=
         Children_With_Trivia (Bare_Node (Node));
      Result      : Children_Array (Bare_Result'Range);
   begin
      for I in Bare_Result'Range loop
         declare
            BR : Bare_Child_Record renames Bare_Result (I);
            R  : Child_Record renames Result (I);
         begin
            case BR.Kind is
               when Child =>
                  R := (Child, Create_Entity (BR.Node));
               when Trivia =>
                  R := (Trivia, BR.Trivia);
            end case;
         end;
      end loop;
      return Result;
   end Children_With_Trivia;

</%def>
