## vim: filetype=makoada

with ${ada_lib_name}.Common; use ${ada_lib_name}.Common;

package ${ada_lib_name}.Introspection is

   Invalid_Field : exception;

   ## In a lot of testcases, there is a single concrete node that has no
   ## field. For these, generates a type that has no valid value.
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

   function Index (Field : Field_Reference) return Positive;
   --  Return the index in nodes to access the given ``Field``

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
