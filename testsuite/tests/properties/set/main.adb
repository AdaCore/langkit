with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U_1 : constant Example_Unit := Ctx.Get_From_Buffer
     (Filename => "example1", Buffer => "example example")
     .Root.As_Example_Unit;
   U_2 : constant Example_Unit := Ctx.Get_From_Buffer
     (Filename => "example2", Buffer => "example")
     .Root.As_Example_Unit;
begin
   Put_Line ("Checking 'contains' operation...");
   Put_Line (U_1.P_Has (U_1.F_Examples.List_Child (1))'Image);
   Put_Line (U_1.P_Has (U_1.F_Examples.List_Child (2))'Image);
   Put_Line (U_1.P_Has (U_2.F_Examples.List_Child (1))'Image);

   Put_Line ("Checking 'length' operation...");
   Put_Line (U_1.P_Count'Image);
   Put_Line (U_2.P_Count'Image);

   Put_Line ("Checking equality operators");
   Put_Line (U_1.P_Is_Eq ((1 => U_1.F_Examples.List_Child (1)))'Image);
   Put_Line (U_1.P_Is_Eq ((1 => U_1.F_Examples.List_Child (2),
                           2 => U_1.F_Examples.List_Child (1)))'Image);
   Put_Line (U_1.P_Is_Eq ((1 => U_1.F_Examples.List_Child (2),
                           2 => U_1.F_Examples.List_Child (1),
                           3 => U_1.F_Examples.List_Child (1)))'Image);
   Put_Line (U_2.P_Is_Eq ((1 => U_1.F_Examples.List_Child (2),
                           2 => U_1.F_Examples.List_Child (1)))'Image);

   Put_Line ("Checking refcounted instances");
   Put_Line (U_1.P_Image_Count'Image);
   Put_Line (U_2.P_Image_Count'Image);
end Main;
