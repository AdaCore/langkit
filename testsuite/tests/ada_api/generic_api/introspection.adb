with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors;      use Langkit_Support.Errors;
with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Names;       use Langkit_Support.Names;
with Langkit_Support.Names.Maps;
with Langkit_Support.Text;        use Langkit_Support.Text;

with Libfoolang.Generic_API;

procedure Introspection is

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

   function Member_Repr (Member : Struct_Member) return String
   is (+Member_Name (Id, Member) & " (" & Member'Image & ")");

   procedure Assert (Predicate : Boolean; Message : String);
   --  Print Message and raise a Program_Error if Predicate is false

   ---------------
   -- Put_Title --
   ---------------

   procedure Put_Title (Label : String) is
   begin
      Put_Line (Label);
      Put_Line ((Label'Range => '='));
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

   First_Node, Last_Node     : Any_Type_Index := No_Type_Index;
   First_Enum, Last_Enum     : Any_Type_Index := No_Type_Index;
   First_Array, Last_Array   : Any_Type_Index := No_Type_Index;
   First_Struct, Last_Struct : Any_Type_Index := No_Type_Index;

   First_Member : constant Struct_Member := Struct_Member'First;
   Last_Member  : Struct_Member := First_Member;

   Invalid_Node   : Type_Ref;
   Invalid_Enum   : Type_Ref;
   Invalid_Array  : Type_Ref;
   Invalid_Struct : Type_Ref;

   Invalid_Member : Struct_Member;

   Dummy_Bool       : Boolean;
   Dummy_Name       : Name_Type;
   Dummy_Type       : Type_Ref;
   Dummy_Type_Index : Any_Type_Index;
   Dummy_Enum_Index : Any_Enum_Value_Index;

begin
   New_Line;

   -----------------------------
   -- Generic type primitives --
   -----------------------------

   Put_Title ("All types");
   New_Line;
   for Index in Type_Index'First .. Last_Type (Id) loop
      declare
         T : constant Type_Ref := From_Index (Id, Index);
      begin
         Put_Line ("* " & Debug_Name (T));

         --  Check the specific kind of type T is

         if Is_Enum_Type (T) then
            Put_Line ("  is an enum");
            if First_Enum = No_Type_Index then
               First_Enum := Index;
            end if;
            Last_Enum := Index;

         elsif Is_Array_Type (T) then
            Put_Line ("  is an array");
            if First_Array = No_Type_Index then
               First_Array := Index;
            end if;
            Last_Array := Index;

         elsif Is_Struct_Type (T) then
            Put_Line ("  is a struct");
            if First_Struct = No_Type_Index then
               First_Struct := Index;
            end if;
            Last_Struct := Index;

         elsif Is_Node_Type (T) then
            Put_Line ("  is a node");
            if First_Node = No_Type_Index then
               First_Node := Index;
            end if;
            Last_Node := Index;
         end if;
      end;
   end loop;
   New_Line;
   Invalid_Node := From_Index (Id, First_Node - 1);
   Invalid_Enum := From_Index (Id, First_Node);
   Invalid_Array := From_Index (Id, First_Node);
   Invalid_Struct := From_Index (Id, First_Enum);

   Put ("Language_For: null T argument: ");
   begin
      declare
         Dummy : constant Language_Id := Language_For (No_Type_Ref);
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

   Put ("Debug_Name: Null T argument: ");
   begin
      declare
         Dummy : constant String := Debug_Name (No_Type_Ref);
      begin
         raise Program_Error;
      end;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   --------------------------
   -- Enum type primitives --
   --------------------------

   Put_Title ("Enum types");
   New_Line;
   for Index in First_Enum .. Last_Enum loop
      declare
         Enum : constant Type_Ref := From_Index (Id, Index);
      begin
         Put (+Enum_Type_Name (Enum));
         Put_Line (" (" & Index'Image & ")");
         Put_Line ("  Default value:" & Enum_Default_Value (Enum)'Image);
         New_Line;
         for V in 1 .. Enum_Last_Value (Enum) loop
            Put_Line (" " & V'Image & ": " & (+Enum_Value_Name (Enum, V)));
         end loop;
         New_Line;
      end;
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
      Dummy_Enum_Index := Enum_Default_Value (No_Type_Ref);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-enum Enum argument: ");
   begin
      Dummy_Enum_Index := Enum_Default_Value (Invalid_Enum);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Enum_Value_Name:");
   Put ("Null Enum argument: ");
   begin
      Dummy_Name := Enum_Value_Name (No_Type_Ref, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-enum Enum argument: ");
   begin
      Dummy_Name := Enum_Value_Name (Invalid_Enum, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Out-of-bounds Index argument: ");
   declare
      Enum : constant Type_Ref := From_Index (Id, First_Enum);
   begin
      Dummy_Name := Enum_Value_Name (Enum, 100);
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
   New_Line;
   for Index in First_Array .. Last_Array loop
      declare
         T : constant Type_Ref := From_Index (Id, Index);
      begin
         Put_Line (Debug_Name (T));
         Put_Line ("Array of " & Debug_Name (Array_Element_Type (T)));
         New_Line;
      end;
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

   ----------------------------
   -- Struct type primitives --
   ----------------------------

   Put_Title ("Structs");
   New_Line;
   for Index in First_Struct .. Last_Struct loop
      declare
         T : constant Type_Ref := From_Index (Id, Index);
      begin
         Put_Line (+Struct_Type_Name (T));
         New_Line;
      end;
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
   New_Line;
   for Index in First_Node .. Last_Node loop
      declare
         Node : constant Type_Ref := From_Index (Id, Index);
      begin
         Put_Line (Node_Repr (Node));
         Node_Type_Maps.Insert (Node_Types, Node_Type_Name (Node), Node);

         if Is_Abstract (Node) then
            Put_Line ("  is abstract");
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
      end;
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
   New_Line;
   for Index in First_Struct .. Last_Node loop
      declare
         T : constant Type_Ref := From_Index (Id, Index);
      begin
         Put_Line ("For " & (+Base_Struct_Type_Name (T)));
         for M of Members (T) loop
            Last_Member := Struct_Member'Max (Last_Member, M);
            Put_Line ("  " & Member_Repr (M));
         end loop;
         New_Line;
      end;
   end loop;
   Invalid_Member := Last_Member + 1;

   Put_Title ("Detailed list of members");
   New_Line;
   for M in First_Member .. Last_Member loop
      Put_Line (Member_Repr (M));
      if Is_Property (Id, M) then
         Put_Line ("  is a property");
      end if;
      Put_Line ("  type: " & Debug_Name (Member_Type (Id, M)));

      if Member_Last_Argument (Id, M) = No_Argument_Index then
         Put_Line ("  no argument");
      else
         Put_Line ("  arguments:");
         for A in 1 .. Member_Last_Argument (Id, M) loop
            Put_Line ("    "
                      & (+Member_Argument_Name (Id, M, A))
                      & ": "
                      & Debug_Name (Member_Argument_Type (Id, M, A)));
         end loop;
      end if;
      New_Line;
   end loop;

   Put ("Is_Property: Invalid Member argument: ");
   begin
      Dummy_Bool := Is_Property (Id, Invalid_Member);
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

   Put ("Member_Name: Invalid Member argument: ");
   begin
      Dummy_Name := Member_Name (Id, Invalid_Member);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put ("Member_Type: Invalid Member argument: ");
   begin
      Dummy_Type := Member_Type (Id, Invalid_Member);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put ("Member_Last_Argument: Invalid Member argument: ");
   begin
      Dummy_Bool := Member_Last_Argument (Id, Invalid_Member) = 0;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Member_Argument_Type:");
   Put ("Invalid Member argument: ");
   begin
      Dummy_Type := Member_Argument_Type (Id, Invalid_Member, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   Put ("Invalid Argument argument: ");
   begin
      Dummy_Type := Member_Argument_Type
        (Id, Last_Member, Member_Last_Argument (Id, Last_Member) + 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Member_Argument_Name:");
   Put ("Invalid Member argument: ");
   begin
      Dummy_Name := Member_Argument_Name (Id, Invalid_Member, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   Put ("Invalid Argument argument: ");
   begin
      Dummy_Name := Member_Argument_Name
        (Id, Last_Member, Member_Last_Argument (Id, Last_Member) + 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

end Introspection;
