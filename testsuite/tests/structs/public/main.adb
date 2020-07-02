with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.GMP.Integers;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is

   procedure Dump (Label : String; S : My_Struct);

   Unit : constant Analysis_Unit := Create_Context.Get_From_Buffer
     (Filename => "main.txt", Buffer => "example");
   E    : Example;
   S    : My_Struct;

   procedure Dump (Label : String; S : My_Struct) is
   begin
      Put_Line (Label & ":");
      Put_Line ("Entity_Field = " & Entity_Field (S).Image);
      Put_Line ("Array_Field:");
      for Item of Array_Field (S) loop
         Put_Line ("  " & Item.Image);
      end loop;
      Put_Line ("Bigint_Field = " & Bigint_Field (S).Image);
      New_Line;
   end Dump;

begin
   if Unit.Has_Diagnostics then
      raise Program_Error;
   end if;

   E := Unit.Root.Child (1).As_Example;

   S := E.P_Get_Struct;
   Dump ("First struct", S);

   S := E.P_Struct_Identity
     (Create_My_Struct (Entity_Field => Unit.Root,
                        Array_Field  => (1 .. 0 => <>),
                        Bigint_Field => GNATCOLL.GMP.Integers.Make ("2")));
   Dump ("Second struct:", S);
end Main;
