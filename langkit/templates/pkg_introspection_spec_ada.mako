## vim: filetype=makoada

private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;

with Langkit_Support.Text; use Langkit_Support.Text;

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;

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

   type Any_Node_Type_Id is (
      None, ${', '.join(n.introspection_name for n in ctx.astnode_types)}
   );

   subtype Node_Type_Id is Any_Node_Type_Id
      range ${ctx.astnode_types[0].introspection_name}
            .. ${ctx.astnode_types[-1].introspection_name};

   type Node_Type_Id_Array is array (Positive range <>) of Node_Type_Id;

   function DSL_Name (Id : Node_Type_Id) return String;
   --  Return the name corresponding to Id in the Langkit DSL

   function Lookup_DSL_Name (Name : String) return Any_Node_Type_Id;
   --  Look for the node type for which Name is in the Lankgit DSL. Return it
   --  if found, otherwise return None.

   function Is_Abstract (Id : Node_Type_Id) return Boolean;
   --  Return whether Id designates an abstract node

   function Is_Concrete (Id : Node_Type_Id) return Boolean
   is (not Is_Abstract (Id));

   function Kind_For (Id : Node_Type_Id) return ${root_node_kind_name};
   --  Return the node kind corresponding to Id. This raises a Constraint_Error
   --  if Id designates an abstract node.

   function Id_For_Kind (Kind : ${root_node_kind_name}) return Node_Type_Id;
   --  Return the node type corresponding to the given node Kind

   function Is_Root_Node (Id : Node_Type_Id) return Boolean;
   --  Return whether Id is a reference for the root node type

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id;
   --  If Id is the root node type, raise a Constaint_Error. Otherwise, return
   --  a reference to Id's base type.

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array;
   --  Return type references for all direct derivations for Id

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean;
   --  Return whether the type that Id represents is derives (directly or
   --  indirectly) from the type that Parent represents.

   ------------------------
   -- Polymorphic values --
   ------------------------

   type Any_Value_Kind is (
      None,
      Boolean_Value,
      Integer_Value,
      Big_Integer_Value,
      Character_Value,
      Token_Value,
      Unbounded_Text_Value,
      Analysis_Unit_Value,
      Node_Value

      % for enum_type in ctx.enum_types:
      , ${enum_type.introspection_kind}
      % endfor

      % for t in ctx.composite_types:
      % if t.exposed and not t.is_entity_type:
      , ${t.introspection_kind}
      % endif
      % endfor
   );
   subtype Value_Kind is
      Any_Value_Kind range Boolean_Value ..  Any_Value_Kind'Last;
   --  Enumeration for all types used to interact with properties

   type Any_Value_Type is private;
   --  Polymorphic value to contain Kind values. This type has by-reference
   --  semantics, so copying it is cheap.

   No_Value : constant Any_Value_Type;
   --  Special Any_Value_Type to mean: no reference to a value

   subtype Value_Type is Any_Value_Type
      with Dynamic_Predicate => Value_Type /= No_Value;

   function Kind (Self : Value_Type) return Value_Kind;
   --  Return the kind of values that Value holds

   --  Accessors and constructors for inner value

   function As_Boolean (Self : Value_Type) return Boolean
      with Pre => Kind (Self) = Boolean_Value;
   function Create_Boolean (Value : Boolean) return Value_Type;

   function As_Integer (Self : Value_Type) return Integer
      with Pre => Kind (Self) = Integer_Value;
   function Create_Integer (Value : Integer) return Value_Type;

   function As_Big_Integer (Self : Value_Type) return Big_Integer
      with Pre => Kind (Self) = Big_Integer_Value;
   function Create_Big_Integer (Value : Big_Integer) return Value_Type;

   function As_Character (Self : Value_Type) return Character_Type
      with Pre => Kind (Self) = Character_Value;
   function Create_Character (Value : Character_Type) return Value_Type;

   function As_Token (Self : Value_Type) return Token_Reference
      with Pre => Kind (Self) = Token_Value;
   function Create_Token (Value : Token_Reference) return Value_Type;

   function As_Unbounded_Text (Self : Value_Type) return Unbounded_Text_Type
      with Pre => Kind (Self) = Unbounded_Text_Value;
   function Create_Unbounded_Text
     (Value : Unbounded_Text_Type) return Value_Type;

   function As_Analysis_Unit (Self : Value_Type) return Analysis_Unit
      with Pre => Kind (Self) = Analysis_Unit_Value;
   function Create_Analysis_Unit (Value : Analysis_Unit) return Value_Type;

   function As_Node (Self : Value_Type) return ${root_entity.api_name}
      with Pre => Kind (Self) = Node_Value;
   function Create_Node
     (Value : ${root_entity.api_name}'Class) return Value_Type;

   % for enum_type in ctx.enum_types:
      function As_${enum_type.api_name}
        (Self : Value_Type) return ${enum_type.api_name}
         with Pre => Kind (Self) = ${enum_type.introspection_kind};
      function Create_${enum_type.api_name}
        (Value : ${enum_type.api_name}) return Value_Type;
   % endfor

   % for t in ctx.composite_types:
      % if t.exposed and not t.is_entity_type:
         function As_${t.api_name} (Self : Value_Type) return ${t.api_name}
            with Pre => Kind (Self) = ${t.introspection_kind};
         function Create_${t.api_name}
           (Value : ${t.api_name}) return Value_Type;
      % endif
   % endfor

   type Value_Array is array (Positive range <>) of Value_Type;

   type Value_Constraint (Kind : Value_Kind := Value_Kind'First) is record
      case Kind is
         when Node_Value =>
            Node_Type : Node_Type_Id;
            --  Base type for nodes that satisfy this constraint

         when others =>
            null;
      end case;
   end record;
   --  Constraint for a polymorphic value

   function Satisfies
     (Value : Value_Type; Constraint : Value_Constraint) return Boolean;
   --  Return whether the given Value satisfy the given Constraint

   type Value_Constraint_Array is
      array (Positive range <>) of Value_Constraint;

   ---------------
   -- Node data --
   ---------------

   <% all_abstract = ctx.sorted_parse_fields + ctx.sorted_properties %>

   type Node_Data_Reference is
      (${', '.join(f.introspection_enum_literal for f in all_abstract)});
   --  Enumeration of all data attached to nodes (syntax fields and properties)

   type Node_Data_Reference_Array is
      array (Positive range <>) of Node_Data_Reference;

   function Node_Data_Name (Node_Data : Node_Data_Reference) return String;
   --  Return a lower-case name for Node_Data

   -------------------
   -- Syntax fields --
   -------------------

   ## In a lot of testcases, there is a single concrete node that has no
   ## field. For these, generate a type that has no valid value.
   subtype Field_Reference is Node_Data_Reference range
      % if ctx.sorted_parse_fields:
         <%
            first = ctx.sorted_parse_fields[0]
            last = ctx.sorted_parse_fields[-1]
         %>
      % else:
         <%
            first = all_abstract[-1]
            last = all_abstract[0]
         %>
      % endif
      ${first.introspection_enum_literal}
      .. ${last.introspection_enum_literal}
   ;
   --  Enumeration of all syntax fields for regular nodes

   function Field_Name (Field : Field_Reference) return String;
   --  Return a lower-case name for ``Field``

   function Field_Type (Field : Field_Reference) return Node_Type_Id;
   --  Return a reference to the node type that covers what Field can contain

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

   function Fields (Id : Node_Type_Id) return Field_Reference_Array;
   --  Likewise, but taking a reference to a node type instead

   % if ctx.sorted_properties:
   ----------------
   -- Properties --
   ----------------

   subtype Property_Reference is Node_Data_Reference
      range ${ctx.sorted_properties[0].introspection_enum_literal}
         .. ${ctx.sorted_properties[-1].introspection_enum_literal};
   --  Enumeration of all available node properties

   function Property_Name (Property : Property_Reference) return String;
   --  Return a lower-case name for ``Property``

   function Property_Return_Type
     (Property : Property_Reference) return Value_Constraint;
   --  Return the type constraint for Property's return type

   function Property_Argument_Types
     (Property : Property_Reference) return Value_Constraint_Array
      with Post => Property_Argument_Types'Result'Length = 0
                   or else Property_Argument_Types'Result'First = 1;
   --  Return the type constraints for Property's arguments

   function Property_Argument_Name
     (Property : Property_Reference; Argument_Number : Positive) return String;
   --  Return the lower-cased name for Property's argument whose index is
   --  Argument_Number.

   type Property_Reference_Array is
      array (Positive range <>) of Property_Reference;

   function Properties
     (Kind : ${root_node_kind_name}) return Property_Reference_Array;
   --  Return the list of properties that nodes of the given ``Kind`` have

   function Properties (Id : Node_Type_Id) return Property_Reference_Array;
   --  Likewise, but taking a reference to a node type instead

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

private

   type Value_Record;
   type Value_Access is access all Value_Record;

   --  In order to avoid Any_Value_Type to be tagged (which makes all its
   --  primitives dispatching, which has awful consequences, such as making
   --  some code patterns illegal, or making GNAT slow, wrap the access in a
   --  dedicated controlled object and make Any_Value_Type contain this
   --  wrapper.

   type Value_Access_Wrapper is new Ada.Finalization.Controlled with record
      Value : Value_Access;
   end record;

   overriding procedure Adjust (Self : in out Value_Access_Wrapper);
   overriding procedure Finalize (Self : in out Value_Access_Wrapper);

   type Any_Value_Type is record
      Value : Value_Access_Wrapper;
   end record;

   No_Value : constant Any_Value_Type :=
     (Value => (Ada.Finalization.Controlled with Value => null));

   % for t in ctx.array_types:
      % if t.exposed:
         type ${t.api_name}_Access is access all ${t.api_name};
         procedure Free is new Ada.Unchecked_Deallocation
           (${t.api_name}, ${t.api_name}_Access);
      % endif
   % endfor

   type Value_Record (Kind : Value_Kind := Value_Kind'First) is
   limited record
      Ref_Count : Natural;

      case Kind is
         when Boolean_Value =>
            Boolean_Value : Boolean;

         when Integer_Value =>
            Integer_Value : Integer;

         when Big_Integer_Value =>
            Big_Integer_Value : Big_Integer;

         when Character_Value =>
            Character_Value : Character_Type;

         when Token_Value =>
            Token_Value : Token_Reference;

         when Unbounded_Text_Value =>
            Unbounded_Text_Value : Unbounded_Text_Type;

         when Analysis_Unit_Value =>
            Analysis_Unit_Value : Analysis_Unit;

         when Node_Value =>
            Node_Value : ${root_entity.api_name};

         % for enum_type in ctx.enum_types:
         when ${enum_type.api_name}_Value =>
            ${enum_type.introspection_kind} : ${enum_type.api_name};
         % endfor

         ## Store records as in the public API, but store accesses to arrays as
         ## they are unconstrained.

         % for t in ctx.composite_types:
         % if t.exposed and not t.is_entity_type:
         when ${t.api_name}_Value =>
            ${t.introspection_kind} :
               % if t.is_array_type:
                  ${t.api_name}_Access;
               % else:
                  ${t.api_name};
               % endif
         % endif
         % endfor
      end case;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Value_Record, Value_Access);

end ${ada_lib_name}.Introspection;
