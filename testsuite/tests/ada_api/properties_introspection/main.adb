with Ada.Directories;
with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;      use Libfoolang.Analysis;
with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is

   function Build_Big_Integer_Array return Big_Integer_Array;
   --  Helper to workaround a GNAT crash

   function Image (Value : Value_Type; Indent : String := "") return String;
   --  Helper to print a value

   -----------------------------
   -- Build_Big_Integer_Array --
   -----------------------------

   function Build_Big_Integer_Array return Big_Integer_Array is
   begin
      return Result : Big_Integer_Array (1 .. 3) do
         Result (1).Set (10);
         Result (2).Set (20);
         Result (3).Set (300);
      end return;
   end Build_Big_Integer_Array;

   -----------
   -- Image --
   -----------

   function Image (Value : Value_Type; Indent : String := "") return String is
      Result : Unbounded_String := To_Unbounded_String (Indent);
   begin
      case Kind (Value) is
         when Boolean_Value =>
            Append (Result, As_Boolean (Value)'Image);
         when Integer_Value =>
            Append (Result, As_Integer (Value)'Image);
         when Big_Integer_Value =>
            Append (Result, As_Big_Integer (Value).Image);
         when Character_Value =>
            declare
               C : constant Character :=
                  Character'Val (Character_Type'Pos (As_Character (Value)));
            begin
               if C in ' ' .. '~' then
                  Append (Result, "'" & C & "'");
               else
                  Append (Result, "[""" & Character'Pos (C)'Image & """]");
               end if;
            end;
         when Token_Value =>
            Append (Result, Image (As_Token (Value)));
         when Unbounded_Text_Value =>
            Append (Result, Image (To_Text (As_Unbounded_Text (Value))));
         when Analysis_Unit_Value =>
            Append
              (Result, Ada.Directories.Simple_Name
                 (As_Analysis_Unit (Value).Get_Filename));
         when Node_Value =>
            Append (Result, As_Node (Value).Image);

         when Analysis_Unit_Kind_Value =>
            Append (Result, As_Analysis_Unit_Kind (Value)'Image);

         when Foo_Node_Array_Value =>
            declare
               Array_Value : constant Foo_Node_Array :=
                  As_Foo_Node_Array (Value);
               First       : Boolean := True;
            begin
               if Array_Value'Length = 0 then
                  Append (Result, "<none>");
               else
                  Result := Null_Unbounded_String;
                  for V of Array_Value loop
                     if First then
                        First := False;
                     else
                        Append (Result, ASCII.LF);
                     end if;
                     Append (Result, Indent & "* " & V.Image);
                  end loop;
               end if;
            end;

         when Big_Integer_Array_Value =>
            declare
               Array_Value : constant Big_Integer_Array :=
                  As_Big_Integer_Array (Value);
               First       : Boolean := True;
            begin
               if Array_Value'Length = 0 then
                  Append (Result, "<none>");
               else
                  Result := Null_Unbounded_String;
                  for V of Array_Value loop
                     if First then
                        First := False;
                     else
                        Append (Result, ASCII.LF);
                     end if;
                     Append (Result, Indent & "* " & V.Image);
                  end loop;
               end if;
            end;

         when others =>
            raise Program_Error;
      end case;
      return To_String (Result);
   end Image;

begin
   --  Do an exhaustive dump of all available properties in this language

   for Kind in Foo_Node_Kind_Type'Range loop
      Put_Line ("Properties for " & Kind'Image & ":");
      declare
         P_List : constant Property_Reference_Array := Properties (Kind);
      begin
         if P_List'Length = 0 then
            Put_Line ("   <none>");
         else
            for P of Properties (Kind) loop
               Put_Line ("   " & Property_Name (P));
               Put_Line ("   return type: "
                         & DSL_Name (Property_Return_Type (P)));

               Put_Line ("   arguments:");
               declare
                  A_Types          : constant Value_Constraint_Array :=
                     Property_Argument_Types (P);
                  A_Default_Values : Any_Value_Array (A_Types'Range);
               begin
                  for I in A_Default_Values'Range loop
                     A_Default_Values (I) :=
                        Property_Argument_Default_Value (P, I);
                  end loop;

                  if A_Types'Length = 0 then
                     Put_Line ("      <none>");
                  else
                     for I in A_Types'Range loop
                        Put ("      " & Property_Argument_Name (P, I));
                        Put (": " & DSL_Name (A_Types (I)));
                        if A_Default_Values (I) /= No_Value then
                           Put (" := " & Image (A_Default_Values (I)));
                        end if;
                        New_Line;
                     end loop;
                     New_Line;
                  end if;
               end;

               New_Line;
            end loop;
         end if;
      end;
      New_Line;
   end loop;

   --  Test that Property_Argument_Name and Property_Argument_Default_Value
   --  reject out-of-bounds indexes.

   begin
      declare
         Dummy : constant String :=
            Property_Argument_Name (Foo_Node_P_Id_Bool, 3);
      begin
         Put_Line ("ERROR: no exception...");
      end;
   exception
      when Exc : Property_Error =>
         Put_Line
           ("Property_Argument_Name rejected out-of-bound index:"
            & Exception_Message (Exc));
   end;

   declare
      Value : Any_Value_Type;
   begin
      Value := Property_Argument_Default_Value (Foo_Node_Parent, 2);
      Put_Line ("ERROR: no exception...");
   exception
      when Exc : Property_Error =>
         Put_Line
           ("Property_Argument_Default_Value rejected out-of-bound index:"
            & Exception_Message (Exc));
   end;
   New_Line;

   --  Test that Eval_Property works as expected

   declare
      procedure Test
        (Label    : String;
         Node     : Foo_Node'Class;
         Property : Property_Reference;
         Argument : Value_Type);

      ----------
      -- Test --
      ----------

      procedure Test
        (Label    : String;
         Node     : Foo_Node'Class;
         Property : Property_Reference;
         Argument : Value_Type)
      is
         Result : Any_Value_Type := No_Value;
      begin
         begin
            Result := Eval_Property (Node, Property, (1 => Argument));
         exception
            when Bad_Type_Error =>
               null;
         end;

         if Result = No_Value then
            Put_Line (Label & ": <no such property>");
         else
            Put_Line (Label & ":");
            Put_Line (Image (Result, "  "));
         end if;
      end Test;

      U    : constant Analysis_Unit := Create_Context.Get_From_Buffer
        (Filename => "foo.txt", Buffer => "var a = 1;");
      Root : constant Foo_Node := U.Root;

      Bool   : constant Value_Type := Create_Boolean (True);
      Int    : constant Value_Type := Create_Integer (1);
      Bigint : constant Value_Type := Create_Big_Integer (Make ("12"));
      Char   : constant Value_Type := Create_Character ('A');
      Token  : constant Value_Type := Create_Token (U.First_Token);
      Sym    : constant Value_Type := Create_Unbounded_Text
        (To_Unbounded_Wide_Wide_String ("a"));
      Unit   : constant Value_Type := Create_Analysis_Unit (U);
      RN     : constant Value_Type := Create_Node (Root);
      NN     : constant Value_Type := Create_Node (No_Foo_Node);
      Name   : constant Value_Type := Create_Node
        (Root.Child (1).As_Var_Decl.F_Name);

      UK : constant Value_Type := Create_Analysis_Unit_Kind (Unit_Body);

      NA  : constant Value_Type := Create_Foo_Node_Array
        ((1 => Root, 2 => As_Node (Name)));
      BIA : constant Value_Type := Create_Big_Integer_Array
        (Build_Big_Integer_Array);
   begin
      Test ("P_Id_Bool", Root, Foo_Node_P_Id_Bool, Bool);
      Test ("P_Id_Int", Root, Foo_Node_P_Id_Int, Int);
      Test ("P_Id_Bigint", Root, Foo_Node_P_Id_Bigint, Bigint);
      Test ("P_Id_Char", Root, Foo_Node_P_Id_Char, Char);
      Test ("P_Id_Token", Root, Foo_Node_P_Id_Token, Token);
      Test ("P_Id_Sym", Root, Foo_Node_P_Id_Sym, Sym);
      Test ("P_Id_Unit", Root, Foo_Node_P_Id_Unit, Unit);
      Test ("P_Id_Root_Node", Root, Foo_Node_P_Id_Root_Node, RN);
      Test ("P_Id_Root_Node", Root, Foo_Node_P_Id_Root_Node, NN);
      Test ("P_Id_Name", Root, Foo_Node_P_Id_Name, Name);
      New_Line;

      Test ("P_Id_Unit_Kind", Root, Foo_Node_P_Id_Unit_Kind, UK);
      New_Line;

      Test ("P_Id_Node_Array", Root, Foo_Node_P_Id_Node_Array, NA);

      --  TODO: there is an invalid read in Eval_Property and nothing looks
      --  suspicious, there. There was already a GNAT bug workaround involved,
      --  so to be investigated, but not blocking.
      if False then
         Test ("P_Id_Bigint_Array", Root, Foo_Node_P_Id_Bigint_Array, BIA);
      end if;

      New_Line;

      Test ("P_Eval", Root, Expr_P_Eval_Plus, Int);
      New_Line;
   end;

   --  Test node data lookup by name

   declare
      procedure Test (Id : Node_Type_Id; Name : String);

      ----------
      -- Test --
      ----------

      procedure Test (Id : Node_Type_Id; Name : String) is
      begin
         Put_Line ("Lookup_Node_Data (" & Id'Image & ", " & Name & ") = "
                   & Lookup_Node_Data (Id, Name)'Image);
      end Test;
   begin
      Test (Foo_Node_Type_Id, "name");
      Test (Foo_Node_Type_Id, "parent");

      Test (Var_Decl_Type_Id, "parent");
      Test (Var_Decl_Type_Id, "name");
      Test (Var_Decl_Type_Id, "eval");
      New_Line;
   end;

   Put_Line ("Done.");
end Main;
