with Ada.Text_IO; use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is
   Ctx : Analysis_Context;
   U   : Analysis_Unit;
   N   : Foo_Node;

   procedure Put (S : Empty_Struct);
   procedure Put (S : Wrapper_Struct);

   ---------
   -- Put --
   ---------

   procedure Put (S : Empty_Struct) is
      pragma Unreferenced (S);
   begin
      Put ("EmptyStruct()");
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (S : Wrapper_Struct) is
   begin
      Put ("Wrapper_Struct (");
      Put ("a =>" & A (S)'Image);
      Put (", b => ");
      Put (B (S));
      Put (", c =>" & C (S)'Image);
      Put (")");
   end Put;

begin
   Put_Line ("main.adb: Running...");

   Ctx := Create_Context;
   U := Ctx.Get_From_Buffer ("main.txt", Buffer => "example");
   N := U.Root;
   if U.Has_Diagnostics then
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      raise Program_Error;
   end if;

   Put ("p_get_empty: ");
   Put (N.P_Get_Empty);
   New_Line;

   Put ("p_get_wrapper: ");
   Put (N.P_Get_Wrapper);
   New_Line;

   Put ("p_id_empty: ");
   Put (N.P_Id_Empty (Create_Empty_Struct));
   New_Line;

   Put ("p_id_wrapper: ");
   Put (N.P_Id_Wrapper (Create_Wrapper_Struct (10, Create_Empty_Struct, 20)));
   New_Line;

   Put_Line ("main.adb: Done");
end Main;
