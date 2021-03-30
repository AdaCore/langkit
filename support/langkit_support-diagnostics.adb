------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

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

end Langkit_Support.Diagnostics;
