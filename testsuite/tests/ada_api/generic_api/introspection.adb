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

   Id : Language_Id renames Libfoolang.Generic_API.Id;

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

   First_Node, Last_Node : Any_Value_Type := No_Value_Type;
   First_Enum, Last_Enum : Any_Value_Type := No_Value_Type;

   Invalid_Type : constant Value_Type := Last_Value_Type (Id) + 1;
   Invalid_Node : Value_Type;
   Invalid_Enum : Value_Type;

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

end Introspection;
