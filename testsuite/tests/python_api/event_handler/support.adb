with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

package body Support is

   function Image (Unit : Analysis_Unit'Class) return String
   is ("<AnalysisUnit " & Ada.Directories.Simple_Name (Unit.Get_Filename)
       & ">");

   ---------
   -- Log --
   ---------

   procedure Log (Self : Event_Handler; Subp : String) is
   begin
      Put_Line (To_String (Self.Label) & ": " & Subp);
   end Log;

   -----------------------------
   -- Unit_Requested_Callback --
   -----------------------------

   overriding procedure Unit_Requested_Callback
     (Self               : in out Event_Handler;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is
   begin
      Log (Self, "Unit_Requested_Callback");
      Put_Line ("  Name: " & Image (Name));
      Put_Line ("  From: " & Image (From));
      Put_Line ("  Found: " & Found'Image);
      Put_Line ("  Is_Not_Found_Error: " & Is_Not_Found_Error'Image);
      New_Line;
   end Unit_Requested_Callback;

   --------------------------
   -- Unit_Parsed_Callback --
   --------------------------

   overriding procedure Unit_Parsed_Callback
     (Self     : in out Event_Handler;
      Context  : Analysis_Context'Class;
      Unit     : Analysis_Unit'Class;
      Reparsed : Boolean) is
   begin
      Log (Self, "Unit_Parsed_Callback");
      Put_Line ("  Unit: " & Image (Unit));
      Put_Line ("  Reparsed: " & Reparsed'Image);
      New_Line;
   end Unit_Parsed_Callback;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out Event_Handler) is
   begin
      Log (Self, "Release");
      New_Line;
   end Release;

   --------------------------
   -- Create_Event_Handler --
   --------------------------

   function Create_Event_Handler
     (Label : String) return Event_Handler_Reference is
   begin
      return Create_Event_Handler_Reference
        (Event_Handler'(Label => To_Unbounded_String (Label)));
   end Create_Event_Handler;

end Support;
