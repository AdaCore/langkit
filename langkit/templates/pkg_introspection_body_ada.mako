## vim: filetype=makoada

<% list_kind_range = ctx.generic_list_type.ada_kind_range_name %>

with ${ada_lib_name}.Implementation;
use ${ada_lib_name}.Implementation;

package body ${ada_lib_name}.Introspection is

   type Node_Type_Descriptor
     (Is_Abstract       : Boolean;
      Derivations_Count : Natural)
   is record
      Base_Type : Any_Node_Type_Id;
      --  Reference to the node type from which this derives

      Derivations : Node_Type_Id_Array (1 .. Derivations_Count);
      --  List of references for all node types that derives from this

      case Is_Abstract is
         when False =>
            Kind : ${root_node_kind_name};
            --  Kind corresponding this this node type
         when True =>
            null;
      end case;
   end record;

   type Node_Type_Descriptor_Access is access constant Node_Type_Descriptor;

   % for n in ctx.astnode_types:
   Desc_For_${n.kwless_raw_name} : aliased constant Node_Type_Descriptor := (
      Is_Abstract       => ${n.abstract},
      Derivations_Count => ${len(n.subclasses)},

      Base_Type   => ${n.base.introspection_name if n.base else 'None'},
      Derivations =>
         ${('({})'.format(', '.join(
            '{} => {}'.format(i, child.introspection_name)
            for i, child in enumerate(n.subclasses, 1)
         )) if n.subclasses else '(1 .. 0 => <>)')}

      % if not n.abstract:
      , Kind => ${n.ada_kind_name}
      % endif
   );
   % endfor

   Node_Type_Descriptors : constant
      array (Node_Type_Id) of Node_Type_Descriptor_Access
   := (${', '.join("Desc_For_{}'Access".format(n.kwless_raw_name)
                   for n in ctx.astnode_types)});

   -----------------
   -- Is_Abstract --
   -----------------

   function Is_Abstract (Id : Node_Type_Id) return Boolean is
   begin
      return Node_Type_Descriptors (Id).Is_Abstract;
   end Is_Abstract;

   --------------
   -- Kind_For --
   --------------

   function Kind_For (Id : Node_Type_Id) return ${root_node_kind_name} is
      Desc : Node_Type_Descriptor renames Node_Type_Descriptors (Id).all;
   begin
      if Desc.Is_Abstract then
         raise Constraint_Error with "trying to get kind for abstract node";
      end if;
      return Desc.Kind;
   end Kind_For;

   ------------------
   -- Is_Root_Node --
   ------------------

   function Is_Root_Node (Id : Node_Type_Id) return Boolean is
   begin
      return Id = ${T.root_node.introspection_name};
   end Is_Root_Node;

   ---------------
   -- Base_Type --
   ---------------

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id is
   begin
      if Is_Root_Node (Id) then
         raise Constraint_Error with "trying to get base type of root node";
      end if;
      return Node_Type_Descriptors (Id).Base_Type;
   end Base_Type;

   -------------------
   -- Derived_Types --
   -------------------

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array is
   begin
      return Node_Type_Descriptors (Id).Derivations;
   end Derived_Types;

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
