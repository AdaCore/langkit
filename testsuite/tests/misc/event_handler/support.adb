with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   -----------------------------
   -- Unit_Requested_Callback --
   -----------------------------

   overriding procedure Unit_Requested_Callback
     (Self               : Event_Handler;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean)
   is
   begin
      Put_Line ("In Event_Handler.Unit_Requested_Callback");
      Put_Line ("Name: " & Image (Name) & ", From: "
                & Ada.Directories.Simple_Name (From.Get_Filename)
                &  ", Found: " & Found'Image);
   end Unit_Requested_Callback;

   --------------------------
   -- Unit_Parsed_Callback --
   --------------------------

   overriding procedure Unit_Parsed_Callback
     (Self     : Event_Handler;
      Context  : Analysis_Context'Class;
      Unit     : Analysis_Unit'Class;
      Reparsed : Boolean)
   is
   begin
      Put_Line ("In Event_Handler.Unit_Parsed_Callback");

      Dump (Analysis_Unit (Unit));

      Put_Line ("Out of Event_Handler.Unit_Parsed_Callback");
   end Unit_Parsed_Callback;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out Event_Handler) is
   begin
      Put_Line ("In Event_Handler.Release");
   end Release;

   ----------
   -- Dump --
   ----------

   procedure Dump (Unit : Analysis_Unit) is
   begin
      if Unit.Has_Diagnostics then
         Put_Line ("Errors:");
         for D of Unit.Diagnostics loop
            Put_Line ("  " & Unit.Format_GNU_Diagnostic (D));
         end loop;
      else
         Put_Line ("Success: " & Image (Unit.Text, With_Quotes => True));
      end if;
      New_Line;
   end Dump;

end Support;
