--  Check that we cannot create aliasing "loops" with logic variables.
--
--  Aliasing for logic variables is implemented maintaining an "alias" link
--  from one variable to another. It used to be possible to build a loop using
--  this link, making all aliasing-sensitive operations (Is_Defined, Get_Value,
--  ...) on each variable in this loop non-terminating.

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Adalog.Logic_Var;

procedure Main is

   package Int_Vars is new Langkit_Support.Adalog.Logic_Var
     (Value_Type => Integer, Value_Image => Integer'Image);
   use Int_Vars;

   N    : constant Natural := 5;
   Recs : array (1 .. N) of Logic_Var_Record;
   Vars : array (1 .. N) of Logic_Var;

   procedure Alias (Self, To : Positive);
   --  Alias Vars (Self) and Vars (To)

   procedure Cleanup;
   --  Unalias all variables and print a newline

   -----------
   -- Alias --
   -----------

   procedure Alias (Self, To : Positive) is
   begin
      Alias (Vars (Self), Vars (To));
   end Alias;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup is
   begin
      for I in Vars'Range loop
         if Is_Defined (Vars (I)) then
            Put_Line ("Var" & I'Image & " is defined");
         end if;
      end loop;
      Put_Line ("OK");

      for I in Vars'Range loop
         Unalias (Vars (I));
      end loop;
      New_Line;
   end Cleanup;

begin
   --  Initialize all variables

   for I in Vars'Range loop
      Vars (I) := Recs (I)'Unrestricted_Access;
      Reset (Vars (I));
   end loop;

   Put_Line ("Simple circular linked list:");
   Alias (1, 2);
   Alias (2, 3);
   Alias (3, 1);
   Cleanup;

   Put_Line ("Crossing non-circular linked lists (1)");
   Alias (1, 2);
   Alias (3, 4);
   Alias (2, 5);
   Alias (4, 5);
   Alias (1, 4);
   Alias (3, 2);
   Cleanup;

   Put_Line ("Crossing non-circular linked lists (2)");
   Alias (1, 2);
   Alias (3, 4);
   Alias (1, 4);
   Alias (3, 2);
   Alias (2, 5);
   Alias (4, 5);
   Cleanup;

   Put_Line ("Done.");
end Main;
