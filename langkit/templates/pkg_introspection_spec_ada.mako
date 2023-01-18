## vim: filetype=makoada

private with Ada.Finalization;
private with Ada.Unchecked_Deallocation;

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
--        Field_Ref : constant Syntax_Field_Reference :=
--           Syntax_Field_Reference_From_Index (Node.Kind, 1);
--     begin
--        Ada.Text_IO.Put_Line (Field_Name (Field_Ref));
--     end;

package ${ada_lib_name}.Introspection is

   use Support.Text;

   ----------------
   -- Node types --
   ----------------

   function DSL_Name (Id : Node_Type_Id) return Text_Type;
   --  Return the name corresponding to ``Id`` in the Langkit DSL

   function Lookup_DSL_Name (Name : Text_Type) return Any_Node_Type_Id;
   --  Look for the node type for which Name is in the Lankgit DSL. Return it
   --  if found, otherwise return None.

   function Is_Abstract (Id : Node_Type_Id) return Boolean;
   --  Return whether ``Id`` designates an abstract node

   function Is_Concrete (Id : Node_Type_Id) return Boolean
   is (not Is_Abstract (Id));

   function Kind_For (Id : Node_Type_Id) return ${T.node_kind};
   --  Return the node kind corresponding to ``Id``. This raises a
   --  ``Bad_Type_Error`` if ``Id`` designates an abstract node.

   function First_Kind_For (Id : Node_Type_Id) return ${T.node_kind};
   function Last_Kind_For (Id : Node_Type_Id) return ${T.node_kind};
   --  Return the node kinds corresponding to respectively the first and
   --  last concrete nodes that derive from ``Id`` (included).

   function Id_For_Kind (Kind : ${T.node_kind}) return Node_Type_Id;
   --  Return the node type corresponding to the given node ``Kind``

   function Is_Root_Node (Id : Node_Type_Id) return Boolean;
   --  Return whether ``Id`` is a reference for the root node type

   function Base_Type (Id : Node_Type_Id) return Node_Type_Id;
   --  If Id is the root node type, raise a ``Bad_Type_Error``. Otherwise,
   --  return a reference to ``Id``'s base type.

   function Derived_Types (Id : Node_Type_Id) return Node_Type_Id_Array;
   --  Return type references for all direct derivations for ``Id``

   function Is_Derived_From (Id, Parent : Node_Type_Id) return Boolean;
   --  Return whether the type that ``Id`` represents is derives (directly or
   --  indirectly) from the type that ``Parent`` represents.

   ------------------------
   -- Polymorphic values --
   ------------------------

   type Any_Value_Type is private;
   --  Polymorphic value to contain ``Kind`` values. This type has by-reference
   --  semantics, so copying it is cheap.

   No_Value : constant Any_Value_Type;
   --  Special ``Any_Value_Type`` to mean: no reference to a value

   subtype Value_Type is Any_Value_Type
      with Dynamic_Predicate => Value_Type /= No_Value;

   function Kind (Self : Value_Type) return Value_Kind;
   --  Return the kind of values that ``Value`` holds

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

   function As_String (Self : Value_Type) return Text_Type
      with Pre => Kind (Self) = String_Value;
   function Create_String (Value : Text_Type) return Value_Type;

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
   type Any_Value_Array is array (Positive range <>) of Any_Value_Type;

   function DSL_Name (Constraint : Type_Constraint) return Text_Type;
   --  Return the name corresponding to ``Constraint`` in the Langkit DSL

   function Satisfies
     (Value : Value_Type; Constraint : Type_Constraint) return Boolean;
   --  Return whether the given ``Value`` satisfy the given ``Constraint``

   -----------
   -- Enums --
   -----------

   type Any_Enum_Value_Index is new Natural;
   subtype Enum_Value_Index is Any_Enum_Value_Index
      range 0 ..  Any_Enum_Value_Index'Last;
   --  Index of an enum value for a given enum type

   No_Enum_Value_Index : constant Any_Enum_Value_Index := 0;

   function Enum_Last_Value (Kind : Enum_Value_Kind) return Enum_Value_Index;
   --  Return the index of the last enum value for the given ``Kind`` enum type

   function Enum_Default_Value
     (Kind : Enum_Value_Kind) return Any_Enum_Value_Index;
   --  Return the index of the default enum value for the given ``Kind`` enum
   --  type, or No_Enum_Value_Index if this type does not have a default value.

   function Enum_Value_Name
     (Kind : Enum_Value_Kind; Index : Enum_Value_Index) return Text_Type;
   --  Return the name corresponding to the ``Index``th value for the ``Kind``
   --  enum type. This raises a ``Out_Of_Bounds_Error`` if ``Index`` is too big
   --  for this enum type.

   function Lookup_Enum_Value
     (Kind : Enum_Value_Kind; Name : Text_Type) return Any_Enum_Value_Index;
   --  Return the index for the enumeration value in the ``Kind`` enum type
   --  whose name is ``Name``. Return ``No_Enum_Value_Index`` if no value in
   --  that enum type has such a name.

   function Create_Enum
     (Kind : Enum_Value_Kind; Index : Enum_Value_Index) return Value_Type;
   --  Return the enum value corresponding to the given ``Index`` and ``Kind``
   --  enum type. This raises a ``Out_Of_Bounds_Error`` if ``Index`` is too big
   --  for this enum type.

   function Enum_Index (Value : Value_Type) return Enum_Value_Index;
   --  Return the index for the given enum value, relative to its type. This
   --  raises a ``Bad_Type_Error`` if ``Value`` is not an enum value.

   ------------
   -- Arrays --
   ------------

   function Array_Element_Constraint
     (Kind : Array_Value_Kind) return Type_Constraint;
   --  Return the constraint for elements of ``Kind`` arrays

   function Array_Length (Self : Value_Type) return Natural;
   --  Assuming that ``Self`` is an array (regardless of its element type),
   --  return the number of elements it contains.
   --
   --  This raises a ``Bad_Type_Error`` if Value is not an array.

   function Array_Element
     (Self : Value_Type; Index : Positive) return Value_Type;
   --  Assuming that ``Self`` is an array (regardless of its element type),
   --  return the value at the given 1-based index.
   --
   --  This raises a ``Bad_Type_Error`` if ``Value`` is not an array, and an
   --  ``Out_Of_Bounds_Error`` if ``Index`` is out of ``Value``'s bounds.

   function Create_Array
     (Kind : Array_Value_Kind; Values : Value_Array) return Value_Type;
   --  Return an array of the given kind that contains the given values.
   --
   --  This raises a ``Bad_Type_Error`` if a value in ``Values`` does not have
   --  the type that ``Kind`` implies.

   -------------
   -- Structs --
   -------------

   function Struct_Fields
     (Kind : Struct_Value_Kind) return Struct_Field_Reference_Array;
   --  Return the list of fields for ``Kind`` structs

   function Create_Struct
     (Kind : Struct_Value_Kind; Values : Value_Array) return Value_Type;
   --  Return a struct of the given kind and assign the given values to each
   --  field.
   --
   --  ``Struct_Fields (Kind)`` describes what ``Values`` should be: both
   --  arrays must have the same length and the type of each value in
   --  ``Values`` must satisfy the corresponding field type.
   --
   --  This raises a ``Bad_Type_Error`` if ``Values`` does not have the
   --  expected size or if the values it contains do not have the expected
   --  type.

   -------------
   -- Members --
   -------------

   function Member_Name (Member : Member_Reference) return Text_Type;
   --  Return a lower-case name for ``Member``

   function Member_Type (Member : Member_Reference) return Type_Constraint;
   --  Return the constraint associated with ``Member``'s type (or its return
   --  type).

   function Eval_Member
     (Prefix    : Value_Type;
      Member    : Member_Reference;
      Arguments : Value_Array) return Value_Type;
   --  Evaluate ``Member`` on the given ``Prefix`` value and the given
   --  ``Arguments`` and return the result of this evaluation.
   --
   --  This raises a ``Property_Error`` If ``Member`` is a node property and
   --  that its execution raises a ``Property_Error``.
   --
   --  This raises a ``Bad_Type_Error`` if ``Prefix`` is not a struct and not a
   --  node, if ``Prefix`` has no such member or if the provided arguments are
   --  invalid for that member.

   function Eval_Member
     (Prefix : Value_Type; Field : Struct_Field_Reference) return Value_Type;
   --  Evaluate ``Field`` on the given ``Prefix`` struct value and return the
   --  result of this evaluation.
   --
   --  This raises a ``Bad_Type_Error`` if ``Prefix`` is not a struct or if
   --  ``Prefix`` has no such field.

   function Eval_Member
     (Node      : ${T.entity.api_name}'Class;
      Member    : Node_Member_Reference;
      Arguments : Value_Array) return Value_Type;
   --  Evaluate ``Member on`` the given ``Node`` and the given ``Arguments``.
   --  If the evaluation raises a ``Property_Error``, forward it. Otherwise,
   --  return its result.
   --
   --  This raises a ``Bad_Type_Error`` if ``Node`` has no such member or if
   --  the provided arguments are invalid for it.

   function Lookup_Member
     (Prefix : Value_Type;
      Name   : Text_Type) return Any_Member_Reference;
   --  Look for the member corresponding to the given ``Name`` (lower-case
   --  name) in the given ``Prefix`` value. Return it if found, otherwise
   --  return None.
   --
   --  This raises a ``Bad_Type_Error`` if ``Prefix`` is not a struct and not a
   --  node, if ``Prefix`` is a null node, if ``Prefix`` has no such member or
   --  if the provided arguments are invalid for that member.

   function Lookup_Member
     (Id   : Node_Type_Id;
      Name : Text_Type) return Any_Member_Reference;
   --  Look for the member corresponding to the given ``Name`` (lower-case
   --  name) in the given node type reference (``Id``). Return it if found,
   --  otherwise return None.

   -------------------------------
   -- Syntax fields (for nodes) --
   -------------------------------

   function Eval_Syntax_Field
     (Node  : ${T.entity.api_name}'Class;
      Field : Syntax_Field_Reference) return ${T.entity.api_name};
   --  Evaluate ``Field`` on the given ``Node``. Return the corresponding
   --  children ``Node``.
   --
   --  This raises a Bad_Type_Error if ``Node`` has no such field.

   function Index
     (Kind : ${T.node_kind}; Field : Syntax_Field_Reference) return Positive;
   --  Return the index in nodes to access the given ``Field`` considering the
   --  given ``Kind`` of node.
   --
   --  This raises an ``Bad_Type_Error`` exception if ``Kind`` nodes do not
   --  have the given ``Field``.

   function Syntax_Field_Reference_From_Index
     (Kind : ${T.node_kind}; Index : Positive) return Syntax_Field_Reference;
   --  Return the field reference corresponding to the given ``Index`` in nodes
   --  of the given ``Kind``. Raise an ``Bad_Type_Error`` exception if there is
   --  no field corresponding to this index.

   function Syntax_Fields
     (Kind : ${T.node_kind}) return Syntax_Field_Reference_Array;
   --  Return the list of fields that nodes of the given ``Kind`` have. This
   --  returns an empty array for list nodes.

   function Syntax_Fields
     (Id : Node_Type_Id) return Syntax_Field_Reference_Array;
   --  Likewise, but taking a reference to a node type instead

   ----------------------------
   -- Properties (for nodes) --
   ----------------------------

   function Property_Argument_Types
     (Property : Property_Reference) return Type_Constraint_Array
      with Post => Property_Argument_Types'Result'Length = 0
                   or else Property_Argument_Types'Result'First = 1;
   --  Return the type constraints for ``Property``'s arguments

   function Property_Argument_Name
     (Property        : Property_Reference;
      Argument_Number : Positive) return Text_Type;
   --  Return the lower-cased name for ``Property``'s argument whose index is
   --  ``Argument_Number``. This raises a ``Property_Error`` if ``Property``
   --  has no such argument.

   function Property_Argument_Default_Value
     (Property        : Property_Reference;
      Argument_Number : Positive) return Any_Value_Type;
   --  If the argument corresponding to ``Argument_Number`` of the given
   --  ``Property`` has a default value, return it. Return ``No_Value``
   --  otherwise.  This raises a ``Property_Error`` if Property has no such
   --  argument.

   function Eval_Property
     (Node      : ${T.entity.api_name}'Class;
      Property  : Property_Reference;
      Arguments : Value_Array) return Value_Type;
   --  Evaluate ``Property`` on the given ``Node`` and the given arguments. If
   --  the property raises a ``Property_Error``, forward it, otherwise return
   --  its result.
   --
   --  This raises a ``Bad_Type_Error`` if ``Node`` has no such property or if
   --  the provided arguments are invalid for this property.

   function Properties (Kind : ${T.node_kind}) return Property_Reference_Array;
   --  Return the list of properties that nodes of the given ``Kind`` have

   function Properties (Id : Node_Type_Id) return Property_Reference_Array;
   --  Likewise, but taking a reference to a node type instead

private

   type Value_Record;
   type Value_Access is access all Value_Record;

   --  In order to avoid ``Any_Value_Type`` to be tagged (which makes all its
   --  primitives dispatching, which has awful consequences, such as making
   --  some code patterns illegal, or making GNAT slow, wrap the access in a
   --  dedicated controlled object and make ``Any_Value_Type`` contain this
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

         when String_Value =>
            String_Value : Unbounded_Text_Type;

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
