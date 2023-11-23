with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is

   package Node_Image_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Foo_Node,
      Element_Type    => Unbounded_String,
      Equivalent_Keys => "=",
      Hash            => Hash);
   Node_Images : Node_Image_Maps.Map;

   function Img (Node : Foo_Node) return String
   is (To_String (Node_Images.Element (Node)));

   procedure Crash_On_Parsing_Error (U : Analysis_Unit);
   procedure Build_Node_Images (Prefix : String; Node : Foo_Node);
   procedure Check (Left, Right : Foo_Node);

   ----------------------------
   -- Crash_On_Parsing_Error --
   ----------------------------

   procedure Crash_On_Parsing_Error (U : Analysis_Unit) is
   begin
      if U.Has_Diagnostics then
         for D of U.Diagnostics loop
            Put_Line (U.Format_GNU_Diagnostic (D));
         end loop;
         raise Program_Error;
      end if;
   end Crash_On_Parsing_Error;

   -----------------------
   -- Build_Node_Images --
   -----------------------

   procedure Build_Node_Images (Prefix : String; Node : Foo_Node)
   is
      Filling : constant String := (1 .. 6 - Prefix'Length => ' ');
   begin
      Node_Images.Insert (Node, To_Unbounded_String (Filling & Prefix));
      if not Node.Is_Null then
         for I in 1 .. Node.Children_Count loop
            declare
               Suffix : constant String := I'Image;
            begin
               Build_Node_Images
                 (Prefix & Suffix (Suffix'First + 1 .. Suffix'Last),
                  Node.Child (I));
            end;
         end loop;
      end if;
   end Build_Node_Images;

   -----------
   -- Check --
   -----------

   procedure Check (Left, Right : Foo_Node) is
   begin
      Put_Line
        ("CCP (" & Img (Left) & ", " & Img (Right) & ") = "
         & Img (Left.Closest_Common_Parent (Right)));
   end Check;

   Ctx : constant Analysis_Context := Create_Context;
   U1  : constant Analysis_Unit :=
     Ctx.Get_From_Buffer ("s1.txt", Buffer => "example");
   U2  : constant Analysis_Unit :=
     Ctx.Get_From_Buffer
       ("s2.txt", Buffer => "((example example) (example example))");

   U2_N   : constant Foo_Node := U2.Root;
   U2_N1  : constant Foo_Node := U2_N.As_Cons.F_Left;
   U2_N11 : constant Foo_Node := U2_N1.As_Cons.F_Left;
   U2_N2  : constant Foo_Node := U2_N.As_Cons.F_Right;
begin
   Crash_On_Parsing_Error (U1);
   Crash_On_Parsing_Error (U2);

   Build_Node_Images ("None", No_Foo_Node);
   Build_Node_Images ("U1_N", U1.Root);
   Build_Node_Images ("U2_N", U2.Root);

   Check (No_Foo_Node, No_Foo_Node);
   Check (No_Foo_Node, U1.Root);
   Check (U1.Root, No_Foo_Node);
   Check (U1.Root, U2.Root);
   New_Line;
   Check (U2_N, U2_N);
   Check (U2_N, U2_N1);
   Check (U2_N, U2_N11);
   Check (U2_N1, U2_N2);
   Check (U2_N1, U2_N11);
   Check (U2_N2, U2_N11);

   Put_Line ("Done.");
end Main;
