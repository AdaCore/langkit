## vim: filetype=makoada

<% list_kind_range = ctx.generic_list_type.ada_kind_range_name %>

with ${ada_lib_name}.Implementation;
use ${ada_lib_name}.Implementation;

package body ${ada_lib_name}.Introspection is

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

   function Index
     (Kind : ${root_node_kind_name}; Field : Field_Reference) return Positive
   is
   begin
      % if ctx.sorted_parse_fields:
         <%
            concrete_astnodes = [n for n in ctx.astnode_types
                                 if not n.abstract]
            def enum_literal(f):
               return (f.overriding or f).introspection_enum_literal
         %>
         case Kind is
            % for n in concrete_astnodes:
               when ${n.ada_kind_name} =>
               return (case Field is
                       % for f in n.get_parse_fields( \
                          predicate=lambda f: not f.null \
                       ):
                       when ${enum_literal(f)} => ${f.index + 1},
                       % endfor
                       when others => raise Constraint_Error);
            % endfor
         end case;

      % else:
         return (raise Program_Error);
      % endif
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
            fields = astnode.get_parse_fields(
               predicate=lambda f: not f.abstract and not f.null,
               include_inherited=False
            )
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
                     f.index + 1,
                     (f.overriding or f).introspection_enum_literal
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

   % if ctx.sorted_properties:

   type Property_Descriptor is record
      Kind_First, Kind_Last : ${root_node_kind_name};
      --  Kind range for nodes that implement this property
   end record;

   function Kind_Matches
     (Kind       : ${root_node_kind_name};
      Descriptor : Property_Descriptor)
      return Boolean
   is (Kind in Descriptor.Kind_First .. Descriptor.Kind_Last);

   Property_Descriptors : constant
      array (Property_Reference) of Property_Descriptor :=
   (
      % for i, p in enumerate(ctx.sorted_properties):
         <% kind_first, kind_last = p.struct.ada_kind_range_bounds %>
         ${', ' if i else ''}${p.introspection_enum_literal} =>
           (Kind_First => ${kind_first},
            Kind_Last  => ${kind_last})
      % endfor
   );

   -------------------
   -- Property_Name --
   -------------------

   function Property_Name (Property : Property_Reference) return String is
   begin
      return (case Property is
         % for i, p in enumerate(ctx.sorted_properties):
            ${',' if i else ''} when ${p.introspection_enum_literal} =>
               ${string_repr(p._name.lower)}
         % endfor
      );
   end Property_Name;

   ----------------
   -- Properties --
   ----------------

   function Properties
     (Kind : ${root_node_kind_name}) return Property_Reference_Array
   is
      Count : Natural := 0;
   begin
      --  Count how many properties we will return
      for Desc of Property_Descriptors loop
         if Kind_Matches (Kind, Desc) then
            Count := Count + 1;
         end if;
      end loop;

      --  Now create the result array and fill it
      return Result : Property_Reference_Array (1 .. Count) do
         declare
            Next : Positive := 1;
         begin
            for Property in Property_Descriptors'Range loop
               if Kind_Matches (Kind, Property_Descriptors (Property)) then
                  Result (Next) := Property;
                  Next := Next + 1;
               end if;
            end loop;
         end;
      end return;
   end Properties;

   % endif

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
