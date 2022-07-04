--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

package body Langkit_Support.Diagnostics is

   ----------------------
   -- To_Pretty_String --
   ----------------------

   function To_Pretty_String (D : Diagnostic) return String is
      Sloc        : constant Source_Location := Start_Sloc (D.Sloc_Range);
      Sloc_Prefix : constant String :=
        (if Sloc = No_Source_Location
         then ""
         else Image (Sloc) & ": ");
   begin
      return Sloc_Prefix & Image (To_Text (D.Message));
   end To_Pretty_String;

   ------------
   -- Create --
   ------------

   function Create
     (Sloc_Range : Source_Location_Range;
      Message    : Wide_Wide_String) return Diagnostic
   is
   begin
      return (Sloc_Range, To_Unbounded_Text (Message));
   end Create;

   ------------
   -- Append --
   ------------

   procedure Append
     (Diagnostics : in out Diagnostics_Vectors.Vector;
      Sloc_Range  : Source_Location_Range := No_Source_Location_Range;
      Message     : Wide_Wide_String)
   is
   begin
      Diagnostics.Append (Create (Sloc_Range, Message));
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Diagnostics : in out Diagnostics_Vectors.Vector;
      Sloc_Range  : Source_Location_Range := No_Source_Location_Range;
      Exc         : Ada.Exceptions.Exception_Occurrence)
   is
      Msg : constant String := Ada.Exceptions.Exception_Message (Exc);
   begin
      Append (Diagnostics, Sloc_Range, To_Text (Msg));
   end Append;

end Langkit_Support.Diagnostics;
