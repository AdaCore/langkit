## vim: filetype=makoada

<% list_kind_range = ctx.generic_list_type.ada_kind_range_name %>

with ${ada_lib_name}.Implementation;
use ${ada_lib_name}.Implementation;

package body ${ada_lib_name}.Introspection is

   Field_Indexes : constant array (Field_Reference) of Natural := (
      ${(', '.join('{} => {}'.format(f.introspection_enum_literal,
                                     f.index + 1)
                   for f in ctx.sorted_parse_fields)
         if ctx.sorted_parse_fields else '1 .. 0 => 0')}
   );
   --  For each field reference, provide the corresponding 1-based field index
   --  in AST nodes.

   ----------------
   -- Field_Name --
   ----------------

   function Field_Name (Field : Field_Reference) return String is
   begin
      return (
         % if ctx.sorted_parse_fields:
            case Field is
                 % for i, f in enumerate(ctx.sorted_parse_fields):
                    ${',' if i else ''} when ${f.introspection_enum_literal} =>
                       ${string_repr(f._name.lower)}
                 % endfor
         % else:
            raise Program_Error
         % endif
      );
   end Field_Name;

   -----------
   -- Index --
   -----------

   function Index (Field : Field_Reference) return Positive is
   begin
      return
         % if ctx.sorted_parse_fields:
            Field_Indexes (Field)
         % else:
            (raise Program_Error)
         % endif
      ;
   end Index;

   --------------------------------
   -- Field_Reference_From_Index --
   --------------------------------

   function Field_Reference_From_Index
     (Kind : ${root_node_kind_name}; Index : Positive) return Field_Reference
   is
   begin
      <%
         def get_actions(astnode, node_expr):
            fields = astnode.get_parse_fields(include_inherited=False)
            result = []

            # List types have no field, so just raise an error if a list kind
            # is passed.
            if astnode.is_generic_list_type:
               result.append(
                  'raise Invalid_Field with "List AST nodes have no field";'
               )
            elif astnode.is_list:
               pass

            # Otherwise, dispatch on the index to return the corresponding
            # field enum value.
            elif fields:
               result.append('case Index is')
               for f in fields:
                  result.append('when {} => return {};'.format(
                     f.index + 1, f.introspection_enum_literal
                  ))
               result.append('when others => null;')
               result.append('end case;')

            return '\n'.join(result)
      %>
      ${ctx.generate_actions_for_hierarchy(None, 'Kind', get_actions)}

      pragma Warnings (Off, "value not in range of type");
      return (raise Invalid_Field with "Index is out of bounds");
      pragma Warnings (On, "value not in range of type");
   end Field_Reference_From_Index;

   ------------
   -- Fields --
   ------------

   function Fields (Kind : ${root_node_kind_name}) return Field_Reference_Array
   is
   begin
      % if ctx.generic_list_type.concrete_subclasses:
         if Kind in ${ctx.generic_list_type.ada_kind_range_name} then
            return (1 .. 0 => <>);
         end if;
      % endif

      % if ctx.sorted_parse_fields:
         declare
            Result : Field_Reference_Array
              (1 .. Kind_To_Node_Children_Count (Kind));
         begin
            for I in Result'Range loop
               Result (I) := Field_Reference_From_Index (Kind, I);
            end loop;
            return Result;
         end;
      % else:
         return (raise Program_Error);
      % endif
   end Fields;

   ---------------------
   -- Token_Node_Kind --
   ---------------------

   function Token_Node_Kind
     (Kind : ${root_node_kind_name}) return Token_Kind
   is
      <% token_nodes = [n for n in ctx.astnode_types
                        if not n.abstract and n.is_token_node] %>
   begin
      % if ctx.generate_unparser:
         case Kind is
            % for n in token_nodes:
               when ${n.ada_kind_name} =>
                  return ${n.token_kind.ada_name};
            % endfor

            when others =>
               --  Kind is not a token node, and thus the precondition does not
               --  hold.
               return (raise Program_Error);
         end case;

      % else:
         pragma Unreferenced (Kind);
         return (raise Program_Error);
      % endif
   end Token_Node_Kind;

end ${ada_lib_name}.Introspection;
