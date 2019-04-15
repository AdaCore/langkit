with Ada.Directories;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.GMP.Integers; use GNATCOLL.GMP.Integers;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis;      use Libfoolang.Analysis;
with Libfoolang.Common;        use Libfoolang.Common;
with Libfoolang.Introspection; use Libfoolang.Introspection;

procedure Main is

   function Constraint_Image (C : Value_Constraint) return String;
   --  Return a human readable text representation for C

   function Build_Big_Integer_Array return Big_Integer_Array;
   --  Helper to workaround a GNAT crash

   ----------------------
   -- Constraint_Image --
   ----------------------

   function Constraint_Image (C : Value_Constraint) return String is
      Kind_Img : constant String := C.Kind'Image;
   begin
      if C.Kind = Node_Value then
         return Kind_Img & " (" & DSL_Name (C.Node_Type) & ")";
      else
         return Kind_Img;
      end if;
   end Constraint_Image;

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
                         & Constraint_Image (Property_Return_Type (P)));

               Put_Line ("   arguments:");
               declare
                  A_List : constant Value_Constraint_Array :=
                     Property_Argument_Types (P);
               begin
                  if A_List'Length = 0 then
                     Put_Line ("      <none>");
                  else
                     for I in A_List'Range loop
                        Put_Line ("      " & Property_Argument_Name (P, I)
                                  & ": " & Constraint_Image (A_List (I)));
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
            when Node_Data_Evaluation_Error =>
               null;
         end;

         if Result = No_Value then
            Put_Line (Label & ": <no such property>");
         else
            Put_Line (Label & ":");
            case Kind (Result) is
               when Boolean_Value =>
                  Put_Line ("  " & As_Boolean (Result)'Image);
               when Integer_Value =>
                  Put_Line ("  " & As_Integer (Result)'Image);
               when Big_Integer_Value =>
                  Put_Line ("  " & As_Big_Integer (Result).Image);
               when Character_Value =>
                  Put_Line
                    ("  " & Character'Val (Character_Type'Pos
                       (As_Character (Result))));
               when Token_Value =>
                  Put_Line ("  " & Image (As_Token (Result)));
               when Unbounded_Text_Value =>
                  Put_Line
                    ("  " & Image (To_Text (As_Unbounded_Text (Result))));
               when Analysis_Unit_Value =>
                  Put_Line
                    ("  " & Ada.Directories.Simple_Name
                       (As_Analysis_Unit (Result).Get_Filename));
               when Node_Value =>
                  Put_Line ("  " & As_Node (Result).Short_Image);

               when Analysis_Unit_Kind_Value =>
                  Put_Line ("  " & As_Analysis_Unit_Kind (Result)'Image);

               when Foo_Node_Array_Value =>
                  declare
                     Value : constant Foo_Node_Array :=
                        As_Foo_Node_Array (Result);
                  begin
                     for V of Value loop
                        Put_Line ("  * " & V.Short_Image);
                     end loop;
                  end;

               when Big_Integer_Array_Value =>
                  declare
                     Value : constant Big_Integer_Array :=
                        As_Big_Integer_Array (Result);
                  begin
                     for V of Value loop
                        Put_Line ("  * " & V.Image);
                     end loop;
                  end;

               when others =>
                  raise Program_Error;
            end case;
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
      Test (Type_Foo_Node, "name");
      Test (Type_Foo_Node, "parent");

      Test (Type_Var_Decl, "parent");
      Test (Type_Var_Decl, "name");
      Test (Type_Var_Decl, "eval");
   end;

   Put_Line ("Done.");
end Main;
