## vim: filetype=makoada

with ${ada_lib_name}.Common; use ${ada_lib_name}.Common;

--  This package provides primitives to inspect the structure of parse trees.
--  It answers questions such as: what is the index of a syntax field in a
--  node? or conversely, to what syntax field does a couple (index, node kind)
--  correspond?
--
--  For instance, the following code snippet prints the name of the first
--  syntax field in ``Node``:
--
--  .. code-block:: ada
--
--     declare
--        Field_Ref : constant Field_Reference :=
--           Field_Reference_From_Index (Node.Kind, 1);
--     begin
--        Ada.Text_IO.Put_Line (Field_Name (Field_Ref));
--     end;

package ${ada_lib_name}.Introspection is

   ----------------
   -- Node types --
   ----------------

   --  Unlike Analysis.${root_node_kind_name}, the following enumeration
   --  contains entries for abstract nodes.

   type Node_Type_Id is (
      ${', '.join(n.introspection_name for n in ctx.astnode_types)}
   );

   type Node_Type_Id_Array is array (Positive range <>) of Node_Type_Id;

   function Is_Abstract (Id : Node_Type_Id) return Boolean;
   --  Return whether Id designates an abstract node

   function Is_Concrete (Id : Node_Type_Id) return Boolean
   is (not Is_Abstract (Id));

   function Kind_For (Id : Node_Type_Id) return ${root_node_kind_name};
   --  Return the node kind corresponding to Id. This raises a Constraint_Error
   --  if Id designates an abstract node.

   -------------------
   -- Syntax fields --
   -------------------

   ## In a lot of testcases, there is a single concrete node that has no
   ## field. For these, generate a type that has no valid value.
   type Field_Reference is
      % if ctx.sorted_parse_fields:
         (${', '.join(f.introspection_enum_literal
                      for f in ctx.sorted_parse_fields)})
      % else:
         new Integer range 1 .. 0
      % endif
   ;
   --  Enumeration of all fields for regular nodes

   function Field_Name (Field : Field_Reference) return String;
   --  Return a lower-case name for ``Field``

   function Index
     (Kind : ${root_node_kind_name}; Field : Field_Reference) return Positive;
   --  Return the index in nodes to access the given ``Field`` considering the
   --  given ``Kind`` of node.

   function Field_Reference_From_Index
     (Kind : ${root_node_kind_name}; Index : Positive) return Field_Reference;
   --  Return the field reference corresponding to the given ``Index`` in nodes
   --  of the given ``Kind``. Raise an ``Invalid_Field`` exception if there is
   --  no field corresponding to this index.

   type Field_Reference_Array is array (Positive range <>) of Field_Reference;

   function Fields
     (Kind : ${root_node_kind_name}) return Field_Reference_Array;
   --  Return the list of fields that nodes of the given ``Kind`` have. This
   --  returns an empty array for list nodes.

   % if ctx.sorted_properties:
   ----------------
   -- Properties --
   ----------------

   type Property_Reference is
      (${', '.join(p.introspection_enum_literal
                   for p in ctx.sorted_properties)});
   --  Enumeration of all available node properties

   function Property_Name (Property : Property_Reference) return String;
   --  Return a lower-case name for ``Property``

   type Property_Reference_Array is
      array (Positive range <>) of Property_Reference;

   function Properties
     (Kind : ${root_node_kind_name}) return Property_Reference_Array;
   --  Return the list of properties that nodes of the given ``Kind`` have

   % endif

   ------------
   -- Tokens --
   ------------

   function Token_Node_Kind
     (Kind : ${root_node_kind_name}) return Token_Kind
      with Pre => Is_Token_Node (Kind);
   --  Return the token kind corresponding to the given token node kind
   % if not ctx.generate_unparser:
   --
   --  As unparser are not generated, this always raises a ``Program_Error``
   --  exception.
   % endif

end ${ada_lib_name}.Introspection;
