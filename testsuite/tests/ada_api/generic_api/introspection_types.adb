with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors;      use Langkit_Support.Errors;
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Names;       use Langkit_Support.Names;
with Langkit_Support.Names.Maps;
with Langkit_Support.Symbols;     use Langkit_Support.Symbols;
with Langkit_Support.Text;        use Langkit_Support.Text;

with Libfoolang.Generic_API;

procedure Introspection_Types is

   use Langkit_Support.Errors.Introspection;

   Id : Language_Id renames Libfoolang.Generic_API.Foo_Lang_Id;

   procedure Put_Title (Label : String);
   --  Print a section title

   procedure Put_Exc (Exc : Exception_Occurrence);
   --  Print info about the given exception occurence

   package Node_Type_Maps is new Langkit_Support.Names.Maps (Type_Ref);
   Node_Types : Node_Type_Maps.Map (Camel);
   --  Mapping from node type names to node type indexes

   function "+" (Name : Name_Type) return String
   is (Image (Format_Name (Name, Camel_With_Underscores)));

   function Node_Repr (Node : Type_Ref) return String
   is (+Node_Type_Name (Node) & " (" & To_Index (Node)'Image & ")");

   function Member_Repr (Member : Struct_Member_Ref) return String
   is (+Member_Name (Member) & " (" & Debug_Name (Member) & ":"
       & To_Index (Member)'Image & ")");

   procedure Assert (Predicate : Boolean; Message : String);
   --  Print Message and raise a Program_Error if Predicate is false

   ---------------
   -- Put_Title --
   ---------------

   procedure Put_Title (Label : String) is
   begin
      Put_Line (Label);
      Put_Line ((Label'Range => '='));
      New_Line;
   end Put_Title;

   -------------
   -- Put_Exc --
   -------------

   procedure Put_Exc (Exc : Exception_Occurrence) is
   begin
      Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
   end Put_Exc;

   ------------
   -- Assert --
   ------------

   procedure Assert (Predicate : Boolean; Message : String) is
   begin
      Put_Line ("Checking: " & Message);
      if not Predicate then
         Put_Line ("Error: " & Message);
         raise Program_Error;
      end if;
   end Assert;

   Enums        : constant Type_Ref_Array := All_Enum_Types (Id);
   Arrays       : constant Type_Ref_Array := All_Array_Types (Id);
   Iters        : constant Type_Ref_Array := All_Iterator_Types (Id);
   Base_Structs : constant Type_Ref_Array := All_Base_Struct_Types (Id);
   Structs      : constant Type_Ref_Array := All_Struct_Types (Id);
   Nodes        : constant Type_Ref_Array := All_Node_Types (Id);

   Last_Member : constant Struct_Member_Ref :=
     From_Index (Id, Last_Struct_Member (Id));

   Invalid_Node   : constant Type_Ref := Enums (1);
   Invalid_Enum   : constant Type_Ref := Arrays (1);
   Invalid_Array  : constant Type_Ref := Enums (1);
   Invalid_Iter   : constant Type_Ref := Enums (1);
   Invalid_Struct : constant Type_Ref := Enums (1);

   Dummy_Bool       : Boolean;
   Dummy_Name       : Name_Type;
   Dummy_Type       : Type_Ref;
   Dummy_Type_Index : Any_Type_Index;
   Dummy_Enum_Index : Any_Enum_Value_Index;
   Dummy_Enum_Value : Enum_Value_Ref;
   Dummy_Value      : Value_Ref;

begin
   New_Line;

   -----------------------------
   -- Generic type primitives --
   -----------------------------

   Put_Title ("All types");
   for T of All_Types (Id) loop
      Put_Line ("* " & Debug_Name (T));
      Put_Line ("  " & Type_Category'Image (Category (T)));

      --  Check the specific kind of type T is

      if Is_Enum_Type (T) then
         Put_Line ("  is an enum");

      elsif Is_Array_Type (T) then
         Put_Line ("  is an array");

      elsif Is_Iterator_Type (T) then
         Put_Line ("  is an iterator");

      elsif Is_Struct_Type (T) then
         Put_Line ("  is a struct");

      elsif Is_Node_Type (T) then
         Put_Line ("  is a node");
      end if;
   end loop;
   New_Line;

   Put ("Language: null T argument: ");
   begin
      declare
         Dummy : constant Language_Id := Language (No_Type_Ref);
      begin
         raise Program_Error;
      end;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("To_Index: Null T argument: ");
   begin
      Dummy_Type_Index := To_Index (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("From_Index: out of range type index: ");
   begin
      Dummy_Type := From_Index (Id, Last_Type (Id) + 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put_Line ("Debug_Name: Null T argument: " & Debug_Name (No_Type_Ref));

   Put ("Category: Null T argument: ");
   declare
      Dummy : Type_Category;
   begin
      Dummy := Category (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   --------------------------
   -- Enum type primitives --
   --------------------------

   Put_Title ("Enum types");
   for Enum of All_Enum_Types (Id) loop
      Put (+Enum_Type_Name (Enum));
      Put_Line (" (" & To_Index (Enum)'Image & ")");
      declare
         DV : constant Enum_Value_Ref := Enum_Default_Value (Enum);
      begin
         Put_Line ("  Default value: " & Debug_Name (DV));
      end;
      New_Line;
      for Index in 1 .. Enum_Last_Value (Enum) loop
         declare
            Value : constant Enum_Value_Ref := From_Index (Enum, Index);
         begin
            Put_Line
              (" " & Index'Image & ": " & (+Enum_Value_Name (Value)));
         end;
      end loop;
      New_Line;
   end loop;

   Put ("Is_Enum_Type: Null T argument: ");
   begin
      Dummy_Bool := Is_Enum_Type (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Enum_Type_Name:");
   Put ("Null Enum argument: ");
   begin
      Dummy_Name := Enum_Type_Name (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-enum Enum argument: ");
   begin
      Dummy_Name := Enum_Type_Name (Invalid_Enum);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Enum_Last_Value:");
   Put ("Null Enum argument: ");
   begin
      Dummy_Enum_Index := Enum_Last_Value (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-enum Enum argument: ");
   begin
      Dummy_Enum_Index := Enum_Last_Value (Invalid_Enum);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Enum_Default_Value:");
   Put ("Null Enum argument: ");
   begin
      Dummy_Enum_Value := Enum_Default_Value (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-enum Enum argument: ");
   begin
      Dummy_Enum_Value := Enum_Default_Value (Invalid_Enum);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put ("Enum_Value_Name: Null Value argument: ");
   begin
      Dummy_Name := Enum_Value_Name (No_Enum_Value_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put ("To_Index: Null Value argument: ");
   begin
      Dummy_Enum_Index := To_Index (No_Enum_Value_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("From_Index: out of range enum value index: ");
   declare
      Enum : constant Type_Ref := Enums (1);
   begin
      Dummy_Enum_Value := From_Index (Enum, Enum_Last_Value (Enum) + 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   ---------------------------
   -- Array type primitives --
   ---------------------------

   Put_Title ("Arrays");
   for T of Arrays loop
      Put_Line (Debug_Name (T));
      Put_Line ("Array of " & Debug_Name (Array_Element_Type (T)));
      New_Line;
   end loop;

   Put ("Is_Array_Type: Null T argument: ");
   begin
      Dummy_Bool := Is_Array_Type (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Array_Element_Type:");
   Put ("Null T argument: ");
   begin
      Dummy_Type := Array_Element_Type (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-array T argument: ");
   begin
      Dummy_Type := Array_Element_Type (Invalid_Array);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   ------------------------------
   -- Iterator type primitives --
   ------------------------------

   Put_Title ("Iterator types");
   for T of Iters loop
      Put (Debug_Name (T));
      Put_Line ("Iterator of " & Debug_Name (Iterator_Element_Type (T)));
      New_Line;
   end loop;

   Put ("Is_Iterator_Type: Null T argument: ");
   begin
      Dummy_Bool := Is_Iterator_Type (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Iterator_Element_Type:");
   Put ("Null T argument: ");
   begin
      Dummy_Type := Iterator_Element_Type (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-iterator T argument: ");
   begin
      Dummy_Type := Iterator_Element_Type (Invalid_Iter);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   ----------------------------
   -- Struct type primitives --
   ----------------------------

   Put_Title ("Structs");
   for T of Structs loop
      Put_Line (+Struct_Type_Name (T));
      New_Line;
   end loop;

   Put ("Is_Struct_Type: Null T argument: ");
   begin
      Dummy_Bool := Is_Struct_Type (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Struct_Type_Name:");
   Put ("Null Struct argument: ");
   begin
      Dummy_Name := Struct_Type_Name (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-struct Struct argument: ");
   begin
      Dummy_Name := Struct_Type_Name (Invalid_Struct);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   --------------------------
   -- Node type primitives --
   --------------------------

   Put_Title ("Nodes");
   for Node of Nodes loop
      Put_Line (Node_Repr (Node));
      Node_Type_Maps.Insert (Node_Types, Node_Type_Name (Node), Node);

      if Is_Abstract (Node) then
         Put_Line ("  is abstract");
      end if;

      if Is_Token_Node (Node) then
         Put_Line ("  is a token node");
      end if;

      if Is_List_Node (Node) then
         Put_Line ("  is a list node");
      end if;

      Put ("  base = ");
      declare
         Base : Type_Ref;
      begin
         Base := Base_Type (Node);
         Put_Line (Node_Repr (Base));
      exception
         when Exc : Bad_Type_Error =>
            Put_Line ("Bad_Type_Error: " & Exception_Message (Exc));
      end;

      declare
         LDT : constant Type_Ref :=
           From_Index (Id, Last_Derived_Type (Node));
      begin
         Put_Line ("  last derivation = " & Node_Repr (LDT));
      end;

      Put_Line ("  derivations:");
      declare
         Derivations : constant Type_Ref_Array :=
           Derived_Types (Node);
      begin
         if Derivations'Length = 0 then
            Put_Line ("    <none>");
         else
            for D of Derivations loop
               Put_Line ("    " & Node_Repr (D));
            end loop;
         end if;
      end;
      New_Line;
   end loop;

   Put ("Is_Node_Type: Null T argument: ");
   begin
      Dummy_Bool := Is_Node_Type (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Node_Type_Name:");
   Put ("Null Node argument: ");
   begin
      Dummy_Name := Node_Type_Name (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Name := Node_Type_Name (Invalid_Node);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Is_Abstract:");
   Put ("Null Node argument: ");
   begin
      Dummy_Bool := Is_Abstract (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Bool := Is_Abstract (Invalid_Node);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Is_Concrete:");
   Put ("Null Node argument: ");
   begin
      Dummy_Bool := Is_Concrete (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Bool := Is_Concrete (Invalid_Node);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Base_Type:");
   Put ("Null Node argument: ");
   begin
      Dummy_Type := Base_Type (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Type := Base_Type (Invalid_Node);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Root Node argument: ");
   begin
      Dummy_Type := Base_Type (Root_Node_Type (Id));
      raise Program_Error;
   exception
      when Exc : Bad_Type_Error =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Derived_Types:");
   Put ("Null Node argument: ");
   begin
      Dummy_Bool := Derived_Types (No_Type_Ref)'Length = 0;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Bool := Derived_Types (Invalid_Node)'Length = 0;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Last_Derived_Type:");
   Put ("Null Node argument: ");
   begin
      Dummy_Type_Index := Last_Derived_Type (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Type_Index := Last_Derived_Type (Invalid_Node);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Check Is_Derived_From:");
   declare
      Foo_Node      : constant Type_Ref := Node_Types.Get ("FooNode");
      Expr_Node     : constant Type_Ref := Node_Types.Get ("Expr");
      Addition_Node : constant Type_Ref := Node_Types.Get ("Addition");
      Number_Node   : constant Type_Ref := Node_Types.Get ("Number");
      Ref_Node      : constant Type_Ref := Node_Types.Get ("Ref");
   begin
      Assert (Is_Derived_From (Foo_Node, Foo_Node),
              "root derives from root");

      Assert (Is_Derived_From (Expr_Node, Foo_Node),
              "expr derives from root");

      Assert (Is_Derived_From (Addition_Node, Foo_Node),
              "addition derives from root");

      Assert (not Is_Derived_From (Ref_Node, Number_Node),
              "ref does not derive from number");

      Put ("Null Node argument: ");
      begin
         Dummy_Bool := Is_Derived_From (No_Type_Ref, Foo_Node);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;

      Put ("Non-node Node argument: ");
      begin
         Dummy_Bool := Is_Derived_From (Invalid_Node, Foo_Node);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;

      Put ("Null Parent argument: ");
      begin
         Dummy_Bool :=
            Is_Derived_From (Foo_Node, No_Type_Ref);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;

      Put ("Non-node Parent argument: ");
      begin
         Dummy_Bool := Is_Derived_From (Foo_Node, Invalid_Node);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;
   end;
   New_Line;

   Put_Title ("Members");
   for T of Base_Structs loop
      Put_Line ("For " & (+Base_Struct_Type_Name (T)));
      for M of Members (T) loop
         Put_Line ("  " & Member_Repr (M));
      end loop;
      New_Line;
   end loop;

   Put_Title ("Detailed list of members");
   for Index in Struct_Member_Index'First .. Last_Struct_Member (Id) loop
      declare
         M : constant Struct_Member_Ref := From_Index (Id, Index);
      begin
         Put_Line (Member_Repr (M));
         Put_Line ("  owner: " & Debug_Name (Owner (M)));
         if Is_Property (M) then
            Put_Line ("  is a property");
         end if;
         Put_Line ("  type: " & Debug_Name (Member_Type (M)));

         if Member_Last_Argument (M) = No_Argument_Index then
            Put_Line ("  no argument");
         else
            Put_Line ("  arguments:");
            for A in 1 .. Member_Last_Argument (M) loop
               Put ("    " & (+Member_Argument_Name (M, A))
                    & ": " & Debug_Name (Member_Argument_Type (M, A)));
               declare
                  V : constant Value_Ref :=
                    Member_Argument_Default_Value (M, A);
               begin
                  if V = No_Value_Ref then
                     New_Line;
                  else
                     Put_Line (" := " & Image (V));
                  end if;
               end;
            end loop;
         end if;
      end;
      New_Line;
   end loop;

   Put ("Is_Property: Null Member argument: ");
   begin
      Dummy_Bool := Is_Property (No_Struct_Member_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Members:");
   Put ("Null Struct argument: ");
   begin
      Dummy_Bool := Members (No_Type_Ref)'Length = 0;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-struct Struct argument: ");
   begin
      Dummy_Bool := Members (Invalid_Struct)'Length = 0;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put ("Member_Name: Null Member argument: ");
   begin
      Dummy_Name := Member_Name (No_Struct_Member_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put ("Member_Type: Null Member argument: ");
   begin
      Dummy_Type := Member_Type (No_Struct_Member_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put ("To_Index: Null Member argument: ");
   declare
      Dummy : Struct_Member_Index;
   begin
      Dummy := To_Index (No_Struct_Member_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("From_Index: out of range member index: ");
   declare
      Dummy : Struct_Member_Ref;
   begin
      Dummy := From_Index (Id, Last_Struct_Member (Id) + 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Member_Last_Argument: Null Member argument: ");
   begin
      Dummy_Bool := Member_Last_Argument (No_Struct_Member_Ref) = 0;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Member_Argument_Type:");
   Put ("Null Member argument: ");
   begin
      Dummy_Type := Member_Argument_Type (No_Struct_Member_Ref, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   Put ("Invalid Argument argument: ");
   begin
      Dummy_Type := Member_Argument_Type
        (Last_Member, Member_Last_Argument (Last_Member) + 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Member_Argument_Name:");
   Put ("Null Member argument: ");
   begin
      Dummy_Name := Member_Argument_Name (No_Struct_Member_Ref, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   Put ("Invalid Argument argument: ");
   begin
      Dummy_Name := Member_Argument_Name
        (Last_Member, Member_Last_Argument (Last_Member) + 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Member_Argument_Default_Value:");
   Put ("Null Member argument: ");
   begin
      Dummy_Value := Member_Argument_Default_Value (No_Struct_Member_Ref, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   Put ("Invalid Argument argument: ");
   begin
      Dummy_Value := Member_Argument_Default_Value
        (Last_Member, Member_Last_Argument (Last_Member) + 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Debug_Name: Null Member argument: "
             & Debug_Name (No_Struct_Member_Ref));
   New_Line;

   Put_Title ("Name maps");
   declare
      procedure Check
        (Enum_Types     : Casing_Convention := Lower;
         Enum_Values    : Casing_Convention := Lower;
         Struct_Types   : Casing_Convention := Lower;
         Struct_Members : Casing_Convention := Lower);
      --  Create a name map using the given casing conventions and perform
      --  several lookups.

      -----------
      -- Check --
      -----------

      procedure Check
        (Enum_Types     : Casing_Convention := Lower;
         Enum_Values    : Casing_Convention := Lower;
         Struct_Types   : Casing_Convention := Lower;
         Struct_Members : Casing_Convention := Lower)
      is
         Symbols : Symbol_Table := Create_Symbol_Table;
         Map     : constant Name_Map := Create_Name_Map
           (Id, Symbols, Enum_Types, Enum_Values, Struct_Types,
            Struct_Members);

         Valid, Invalid : Symbol_Type;
         ET, ST         : Type_Ref;
         EV             : Enum_Value_Ref;
         SM             : Struct_Member_Ref;
      begin
         if Enum_Types /= Lower then
            Put_Line ("Enum types as " & Enum_Types'Image);
         end if;
         if Enum_Values /= Lower then
            Put_Line ("Enum values as " & Enum_Values'Image);
         end if;
         if Struct_Types /= Lower then
            Put_Line ("Struct types as " & Struct_Types'Image);
         end if;
         if Struct_Members /= Lower then
            Put_Line ("Struct members as " & Struct_Members'Image);
         end if;

         --  Try to look up an enum type

         if Enum_Types = Lower then
            Valid := Find (Symbols, "analysis_unit_kind");
            Invalid := Find (Symbols, "ANALYSIS_UNIT_KIND");
         else
            Valid := Find (Symbols, "ANALYSIS_UNIT_KIND");
            Invalid := Find (Symbols, "analysis_unit_kind");
         end if;
         ET := Lookup_Type (Map, Invalid);
         Put_Line (Image (Invalid) & " => " & Debug_Name (ET));
         ET := Lookup_Type (Map, Valid);
         Put_Line (Image (Valid) & " => " & Debug_Name (ET));

         --  Try to look up an enum value

         if Enum_Values = Lower then
            Valid := Find (Symbols, "unit_body");
            Invalid := Find (Symbols, "UNIT_BODY");
         else
            Valid := Find (Symbols, "UNIT_BODY");
            Invalid := Find (Symbols, "unit_body");
         end if;
         EV := Lookup_Enum_Value (Map, ET, Invalid);
         Put_Line (Image (Invalid) & " => " & Debug_Name (EV));
         EV := Lookup_Enum_Value (Map, ET, Valid);
         Put_Line (Image (Valid) & " => " & Debug_Name (EV));

         --  Try to look up a struct type

         if Struct_Types = Lower then
            Valid := Find (Symbols, "var_decl");
            Invalid := Find (Symbols, "VAR_DECL");
         else
            Valid := Find (Symbols, "VAR_DECL");
            Invalid := Find (Symbols, "var_decl");
         end if;
         ST := Lookup_Type (Map, Invalid);
         Put_Line (Image (Invalid) & " => " & Debug_Name (ST));
         ST := Lookup_Type (Map, Valid);
         Put_Line (Image (Valid) & " => " & Debug_Name (ST));

         --  Try to look up a struct member

         if Struct_Members = Lower then
            Valid := Find (Symbols, "f_name");
            Invalid := Find (Symbols, "F_NAME");
         else
            Valid := Find (Symbols, "F_NAME");
            Invalid := Find (Symbols, "f_name");
         end if;
         SM := Lookup_Struct_Member (Map, ST, Invalid);
         Put_Line (Image (Invalid) & " => " & Debug_Name (SM));
         SM := Lookup_Struct_Member (Map, ST, Valid);
         Put_Line (Image (Valid) & " => " & Debug_Name (SM));

         New_Line;
         Destroy (Symbols);
      end Check;

   begin
      Check (Enum_Types => Upper);
      Check (Enum_Values => Upper);
      Check (Struct_Types => Upper);
      Check (Struct_Members => Upper);
   end;

   declare
      Symbols    : Symbol_Table := Create_Symbol_Table;
      S          : constant Symbol_Type := Find (Symbols, "foo");
      Map        : constant Name_Map := Create_Name_Map
        (Id, Symbols, Lower, Lower, Lower, Lower);
      Uninit_Map : Name_Map;
      Enum       : constant Type_Ref := Enums (1);
      Struct     : constant Type_Ref := Structs (1);

      Dummy_Struct_Member : Struct_Member_Ref;
   begin
      Put_Line ("Invalid args for Lookup_Type");
      Put ("Uninitialized name map: ");
      begin
         Dummy_Type := Lookup_Type (Uninit_Map, S);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;
      Put ("Null symbol: ");
      begin
         Dummy_Type := Lookup_Type (Map, null);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;
      New_Line;

      Put_Line ("Invalid args for Lookup_Enum_Value");
      Put ("Uninitialized name map: ");
      begin
         Dummy_Enum_Value := Lookup_Enum_Value (Uninit_Map, Enum, S);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;
      Put ("Null enum type: ");
      begin
         Dummy_Enum_Value := Lookup_Enum_Value (Map, No_Type_Ref, S);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;
      Put ("Null symbol: ");
      begin
         Dummy_Enum_Value := Lookup_Enum_Value (Map, Enum, null);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;
      New_Line;

      Put_Line ("Invalid args for Lookup_Struct_Member");
      Put ("Uninitialized name map: ");
      begin
         Dummy_Struct_Member := Lookup_Struct_Member (Uninit_Map, Struct, S);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;
      Put ("Null struct type: ");
      begin
         Dummy_Struct_Member := Lookup_Struct_Member (Map, No_Type_Ref, S);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;
      Put ("Null symbol: ");
      begin
         Dummy_Struct_Member := Lookup_Struct_Member (Map, Struct, null);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;

      Destroy (Symbols);
   end;

end Introspection_Types;
