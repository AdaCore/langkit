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

   package Node_Type_Maps is new Langkit_Support.Names.Maps (Value_Type);
   Node_Types : Node_Type_Maps.Map (Camel);
   --  Mapping from node type names to node type indexes

   function "+" (Name : Name_Type) return String
   is (Image (Format_Name (Name, Camel_With_Underscores)));

   function Node_Repr (Node : Value_Type) return String
   is (+Node_Type_Name (Id, Node) & " (" & Node'Image & ")");

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

   First_Node, Last_Node     : Any_Value_Type := No_Value_Type;
   First_Enum, Last_Enum     : Any_Value_Type := No_Value_Type;
   First_Array, Last_Array   : Any_Value_Type := No_Value_Type;
   First_Struct, Last_Struct : Any_Value_Type := No_Value_Type;

   First_Member : constant Struct_Member := Struct_Member'First;
   Last_Member  : Struct_Member := First_Member;

   Invalid_Type   : constant Value_Type := Last_Value_Type (Id) + 1;
   Invalid_Node   : Value_Type;
   Invalid_Enum   : Value_Type;
   Invalid_Array  : Value_Type;
   Invalid_Struct : Value_Type;

   Invalid_Member : Struct_Member;

   Dummy_Bool       : Boolean;
   Dummy_Name       : Name_Type;
   Dummy_Type       : Any_Value_Type;
   Dummy_Enum_Index : Any_Enum_Value_Index;

begin
   New_Line;

   -----------------------------
   -- Generic type primitives --
   -----------------------------

   Put_Title ("All types");
   New_Line;
   for T in Value_Type'First .. Last_Value_Type (Id) loop
      Put_Line ("* " & Debug_Name (Id, T));

      --  Check the specific kind of type T is

      if Is_Enum_Type (Id, T) then
         Put_Line ("  is an enum");
         if First_Enum = No_Value_Type then
            First_Enum := T;
         end if;
         Last_Enum := T;

      elsif Is_Array_Type (Id, T) then
         Put_Line ("  is an array");
         if First_Array = No_Value_Type then
            First_Array := T;
         end if;
         Last_Array := T;

      elsif Is_Struct_Type (Id, T) then
         Put_Line ("  is a struct");
         if First_Struct = No_Value_Type then
            First_Struct := T;
         end if;
         Last_Struct := T;

      elsif Is_Node_Type (Id, T) then
         Put_Line ("  is a node");
         if First_Node = No_Value_Type then
            First_Node := T;
         end if;
         Last_Node := T;
      end if;
   end loop;
   New_Line;
   Invalid_Node := First_Node - 1;
   Invalid_Enum := First_Node;
   Invalid_Array := First_Node;
   Invalid_Struct := First_Enum;

   Put_Line ("Trying to get the debug name of an invalid type...");
   begin
      declare
         Dummy : constant String := Debug_Name (Id, Invalid_Type);
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
   for Enum in First_Enum .. Last_Enum loop
      Put (+Enum_Type_Name (Id, Enum));
      Put_Line (" (" & Enum'Image & ")");
      Put_Line ("  Default value:" & Enum_Default_Value (Id, Enum)'Image);
      New_Line;
      for V in 1 .. Enum_Last_Value (Id, Enum) loop
         Put_Line (" " & V'Image & ": " & (+Enum_Value_Name (Id, Enum, V)));
      end loop;
      New_Line;
   end loop;

   Put ("Is_Enum_Type: invalid T argument: ");
   begin
      Dummy_Bool := Is_Enum_Type (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Enum_Type_Name:");
   Put ("Invalid Enum argument: ");
   begin
      Dummy_Name := Enum_Type_Name (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-enum Enum argument: ");
   begin
      Dummy_Name := Enum_Type_Name (Id, Invalid_Enum);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Enum_Last_Value:");
   Put ("Invalid Enum argument: ");
   begin
      Dummy_Enum_Index := Enum_Last_Value (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-enum Enum argument: ");
   begin
      Dummy_Enum_Index := Enum_Last_Value (Id, Invalid_Enum);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Enum_Default_Value:");
   Put ("Invalid Enum argument: ");
   begin
      Dummy_Enum_Index := Enum_Default_Value (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-enum Enum argument: ");
   begin
      Dummy_Enum_Index := Enum_Default_Value (Id, Invalid_Enum);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Enum_Value_Name:");
   Put ("Invalid Enum argument: ");
   begin
      Dummy_Name := Enum_Value_Name (Id, Invalid_Type, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-enum Enum argument: ");
   begin
      Dummy_Name := Enum_Value_Name (Id, Invalid_Enum, 1);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Out-of-bounds Index argument: ");
   begin
      Dummy_Name := Enum_Value_Name (Id, First_Enum, 100);
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
   for T in First_Array .. Last_Array loop
      Put_Line (Debug_Name (Id, T));
      Put_Line ("Array of " & Debug_Name (Id, Array_Element_Type (Id, T)));
      New_Line;
   end loop;

   Put ("Is_Array_Type: invalid T argument: ");
   begin
      Dummy_Bool := Is_Array_Type (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Array_Element_Type:");
   Put ("Invalid T argument: ");
   begin
      Dummy_Type := Array_Element_Type (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-array T argument: ");
   begin
      Dummy_Type := Array_Element_Type (Id, Invalid_Array);
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
   for T in First_Struct .. Last_Struct loop
      Put_Line (+Struct_Type_Name (Id, T));
      New_Line;
   end loop;

   Put ("Is_Struct_Type: Invalid T argument: ");
   begin
      Dummy_Bool := Is_Struct_Type (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Struct_Type_Name:");
   Put ("Invalid Struct argument: ");
   begin
      Dummy_Name := Struct_Type_Name (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-struct Struct argument: ");
   begin
      Dummy_Name := Struct_Type_Name (Id, Invalid_Struct);
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
   for Node in First_Node .. Last_Node loop
      Put_Line (Node_Repr (Node));
      Node_Type_Maps.Insert (Node_Types, Node_Type_Name (Id, Node), Node);

      if Is_Abstract (Id, Node) then
         Put_Line ("  is abstract");
      end if;

      Put ("  base = ");
      declare
         Base : Value_Type;
      begin
         Base := Base_Type (Id, Node);
         Put_Line (Node_Repr (Base));
      exception
         when Exc : Bad_Type_Error =>
            Put_Line ("Bad_Type_Error: " & Exception_Message (Exc));
      end;

      Put_Line
        ("  last derivation = " & Node_Repr (Last_Derived_Type (Id, Node)));

      Put_Line ("  derivations:");
      declare
         Derivations : constant Value_Type_Array := Derived_Types (Id, Node);
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

   Put ("Is_Node_Type: Invalid T argument: ");
   begin
      Dummy_Bool := Is_Node_Type (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Node_Type_Name:");
   Put ("Invalid Node argument: ");
   begin
      Dummy_Name := Node_Type_Name (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Name := Node_Type_Name (Id, Invalid_Node);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Is_Abstract:");
   Put ("Invalid Node argument: ");
   begin
      Dummy_Bool := Is_Abstract (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Bool := Is_Abstract (Id, Invalid_Node);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Is_Concrete:");
   Put ("Invalid Node argument: ");
   begin
      Dummy_Bool := Is_Concrete (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Bool := Is_Concrete (Id, Invalid_Node);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Base_Type:");
   Put ("Invalid Node argument: ");
   begin
      Dummy_Type := Base_Type (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Type := Base_Type (Id, Invalid_Node);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Root Node argument: ");
   begin
      Dummy_Type := Base_Type (Id, Root_Node_Type (Id));
      raise Program_Error;
   exception
      when Exc : Bad_Type_Error =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Derived_Types:");
   Put ("Invalid Node argument: ");
   begin
      Dummy_Bool := Derived_Types (Id, Invalid_Type)'Length = 0;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Bool := Derived_Types (Id, Invalid_Node)'Length = 0;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Invalid args for Last_Derived_Type:");
   Put ("Invalid Node argument: ");
   begin
      Dummy_Type := Last_Derived_Type (Id, Invalid_Type);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-node Node argument: ");
   begin
      Dummy_Type := Last_Derived_Type (Id, Invalid_Node);
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;
   New_Line;

   Put_Line ("Check Is_Derived_From:");
   declare
      Foo_Node      : constant Value_Type := Node_Types.Get ("FooNode");
      Expr_Node     : constant Value_Type := Node_Types.Get ("Expr");
      Addition_Node : constant Value_Type := Node_Types.Get ("Addition");
      Number_Node   : constant Value_Type := Node_Types.Get ("Number");
      Ref_Node      : constant Value_Type := Node_Types.Get ("Ref");
   begin
      Assert (Is_Derived_From (Id, Foo_Node, Foo_Node),
              "root derives from root");

      Assert (Is_Derived_From (Id, Expr_Node, Foo_Node),
              "expr derives from root");

      Assert (Is_Derived_From (Id, Addition_Node, Foo_Node),
              "addition derives from root");

      Assert (not Is_Derived_From (Id, Ref_Node, Number_Node),
              "ref does not derive from number");

      Put ("Invalid Node argument: ");
      begin
         Dummy_Bool :=
            Is_Derived_From (Id, Invalid_Type, Foo_Node);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;

      Put ("Non-node Node argument: ");
      begin
         Dummy_Bool := Is_Derived_From (Id, Invalid_Node, Foo_Node);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;

      Put ("Invalid Parent argument: ");
      begin
         Dummy_Bool :=
            Is_Derived_From (Id, Foo_Node, Invalid_Type);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;

      Put ("Non-node Parent argument: ");
      begin
         Dummy_Bool := Is_Derived_From (Id, Foo_Node, Invalid_Node);
         raise Program_Error;
      exception
         when Exc : Precondition_Failure =>
            Put_Exc (Exc);
      end;
   end;
   New_Line;

   Put_Title ("Members");
   New_Line;
   for T in First_Struct .. Last_Node loop
      Put_Line ("For " & (+Base_Struct_Type_Name (Id, T)));
      for M of Members (Id, T) loop
         Last_Member := Struct_Member'Max (Last_Member, M);
         Put_Line ("  " & Member_Repr (M));
      end loop;
      New_Line;
   end loop;
   Invalid_Member := Last_Member + 1;

   Put_Title ("Detailed list of members");
   New_Line;
   for M in First_Member .. Last_Member loop
      Put_Line (Member_Repr (M));
      if Is_Property (Id, M) then
         Put_Line ("  is a property");
      end if;
      Put_Line ("  type: " & Debug_Name (Id, Member_Type (Id, M)));

      if Member_Last_Argument (Id, M) = No_Argument_Index then
         Put_Line ("  no argument");
      else
         Put_Line ("  arguments:");
         for A in 1 .. Member_Last_Argument (Id, M) loop
            Put_Line ("    "
                      & (+Member_Argument_Name (Id, M, A))
                      & ": "
                      & Debug_Name (Id, Member_Argument_Type (Id, M, A)));
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
   Put ("Invalid Struct argument: ");
   begin
      Dummy_Bool := Members (Id, Invalid_Type)'Length = 0;
      raise Program_Error;
   exception
      when Exc : Precondition_Failure =>
         Put_Exc (Exc);
   end;

   Put ("Non-struct Struct argument: ");
   begin
      Dummy_Bool := Members (Id, Invalid_Struct)'Length = 0;
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
