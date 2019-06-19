with Ada.Text_IO; use Ada.Text_IO;

with Support;

procedure Main is
   use Support.Int_Lists;

   A : List := Create;
   --  This list is init with Create, so will contain a pool

   B : List;
   --  This one is just init to ``No_List``. Careful! pools will be created
   --  when concat-ing to that.

   D : List := 1 & (2 & (3 & B));
   --  This one contains a new pool

   E : List := 1 & (2 & (9 & No_List));
   --  This one contains a new pool too

   ----------------
   -- Print_List --
   ----------------

   procedure Print_List (S : List) is
      Tmp : List := S;
   begin
      if not Has_Element (S) then
         Put_Line ("<empty>");

      else
         while Has_Element (Tmp) loop
            Put (Head (Tmp)'Image);
            Tmp := Tail (Tmp);
         end loop;
         New_Line;
      end if;
   end Print_List;
begin
   Print_List (A);
   Print_List (1 & (2 & (3 & A)));
   Print_List (D);
   Print_List (122 & Tail (Tail (D)));
   Print_List (E);

   Destroy (A);
   Destroy (D);
   Destroy (E);
end Main;
