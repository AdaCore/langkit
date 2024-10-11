with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

procedure Main is

   procedure Check_Sloc (S : String);
   procedure Check_Sloc_Range (S : String);
   procedure Check_Sloc_Range_Order (Left, Right : String);

   ----------------
   -- Check_Sloc --
   ----------------

   procedure Check_Sloc (S : String) is
      Sloc : Source_Location;
   begin
      Put ("""" & S & """ -> ");
      Sloc := Value (S);
      Put_Line (Image (Sloc));
   exception
      when Exc : Constraint_Error =>
         Put_Line ("error: " & Exception_Message (Exc));
   end Check_Sloc;

   ----------------------
   -- Check_Sloc_Range --
   ----------------------

   procedure Check_Sloc_Range (S : String) is
      SR : Source_Location_Range;
   begin
      Put ("""" & S & """ -> ");
      SR := Value (S);
      Put_Line (Image (SR));
   exception
      when Exc : Constraint_Error =>
         Put_Line ("error: " & Exception_Message (Exc));
   end Check_Sloc_Range;

   ----------------------------
   -- Check_Sloc_Range_Order --
   ----------------------------

   procedure Check_Sloc_Range_Order (Left, Right : String) is
      L : constant Source_Location_Range := Value (Left);
      R : constant Source_Location_Range := Value (Right);
   begin
      Put_Line
        (Left & " < " & Right & " = "
         & (if L < R then "True" else "False"));
      Put_Line
        (Right & " < " & Left & " = "
         & (if R < L then "True" else "False"));
   end Check_Sloc_Range_Order;

begin
   Put_Line ("== Value/Image for Source_Location ==");
   New_Line;
   Check_Sloc ("0:0");
   Check_Sloc ("123:456");
   Check_Sloc ("");
   Check_Sloc ("1");
   Check_Sloc (":1");
   Check_Sloc ("-1:2");
   Check_Sloc ("a:2");
   New_Line;

   Put_Line ("== Value/Image for Source_Location_Range ==");
   New_Line;
   Check_Sloc_Range ("1:2-3:4");
   Check_Sloc_Range ("");
   Check_Sloc_Range ("1:2");
   Check_Sloc_Range ("1:2-3:");
   Check_Sloc_Range (":2-3:4");
   Check_Sloc_Range ("1:-3:4");
   Check_Sloc_Range ("1:2-:4");
   Check_Sloc_Range ("1:2-3:");
   Check_Sloc_Range ("1:2-3:-1");
   New_Line;

   Put_Line ("== Ordering for Source_Location_Range ==");
   New_Line;

   Check_Sloc_Range_Order ("1:1-1:1", "1:1-1:1");
   New_Line;

   Check_Sloc_Range_Order ("2:1-1:1", "1:1-1:1");
   Check_Sloc_Range_Order ("1:2-1:1", "1:1-1:1");
   Check_Sloc_Range_Order ("1:1-2:1", "1:1-1:1");
   Check_Sloc_Range_Order ("1:1-1:2", "1:1-1:1");
   New_Line;

   Check_Sloc_Range_Order ("2:1-1:1", "1:9-9:9");
   Check_Sloc_Range_Order ("1:2-1:1", "1:1-9:9");
   Check_Sloc_Range_Order ("1:1-2:1", "1:1-1:9");
   Check_Sloc_Range_Order ("1:9-9:9", "2:1-1:1");
   Check_Sloc_Range_Order ("1:1-9:9", "1:2-1:1");
   Check_Sloc_Range_Order ("1:1-1:9", "1:1-2:1");
end Main;
