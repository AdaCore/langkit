with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

package body User_API is

   -------------------
   -- Check_Context --
   -------------------

   procedure Check_Context
     (Context : System.Address; Copy_Context : out System.Address)
   is
      C : constant Analysis_Context := Ada_Context (Context);
   begin
      Copy_Context := Context;

      Put_Line ("Context has unit foo.txt:");
      Put_Line (C.Has_Unit ("foo.txt")'Image);
      New_Line;

      Put_Line ("Context has unit bar.txt:");
      Put_Line (C.Has_Unit ("bar.txt")'Image);
      New_Line;
   end Check_Context;

   ----------------
   -- Check_Unit --
   ----------------

   procedure Check_Unit
     (Unit : System.Address; Copy_Unit : out System.Address)
   is
      U : constant Analysis_Unit := Ada_Unit (Unit);
   begin
      Copy_Unit := Unit;

      Put_Line ("Given analysis unit:");
      Put_Line (Simple_Name (U.Get_Filename));
      New_Line;
   end Check_Unit;

   ----------------
   -- Check_Node --
   ----------------

   procedure Check_Node
     (Node : C_Node_Type; Copy_Node : out C_Node_Type)
   is
      N : constant Foo_Node := Ada_Node (Node);
   begin
      Copy_Node := Node;

      Put_Line ("Given node:");
      Put_Line (N.Image);
      Put_Line ("F1: " & N.P_Get_F1'Image);
      Put_Line ("F2: " & N.P_Get_F2'Image);
      New_Line;
   end Check_Node;

end User_API;
