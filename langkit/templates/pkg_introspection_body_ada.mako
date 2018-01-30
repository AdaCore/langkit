## vim: filetype=makoada

<% list_kind_range = ctx.generic_list_type.ada_kind_range_name %>

package body ${ada_lib_name}.Introspection is

   Field_Indexes : constant array (Field_Reference) of Natural := (
      ${(', '.join('{} => {}'.format(f.introspection_enum_literal,
                                     f.index)
                   for f in ctx.sorted_parse_fields)
         if ctx.sorted_parse_fields else '1 .. 0 => 0')}
   );
   --  For each field reference, provide the corresponding field index in AST
   --  nodes.

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name (Field : Field_Reference) return String is
      pragma Unreferenced (Field);
   begin
      return (raise Program_Error); --  TODO
   end Field_Name;

   -----------
   -- Index --
   -----------

   function Index (Field : Field_Reference) return Positive is
   begin
      return Field_Indexes (Field);
   end Index;

   --------------------------------
   -- Field_Reference_From_Index --
   --------------------------------

   function Field_Reference_From_Index
     (Kind : ${root_node_kind_name}; Index : Positive) return Field_Reference
   is
      pragma Unreferenced (Kind, Index);
   begin
      return (raise Program_Error); --  TODO
   end Field_Reference_From_Index;

   ------------
   -- Fields --
   ------------

   function Fields (Kind : ${root_node_kind_name}) return Field_Reference_Array
   is
      pragma Unreferenced (Kind);
   begin
      return (raise Program_Error); --  TODO
   end Fields;

end ${ada_lib_name}.Introspection;
