------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2022, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Exceptions;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text; use Langkit_Support.Text;

--  This package provides a data type for diagnostics produced by Langkit
--  generated libraries.

package Langkit_Support.Diagnostics is

   type Diagnostic is record
      Sloc_Range : Source_Location_Range;
      --  The source location range that the diagnostic message refers to

      Message    : Unbounded_Text_Type;
      --  Message for this diagnostic
   end record;

   function To_Pretty_String (D : Diagnostic) return String;
   --  Pretty prints the given diagnostic.
   --
   --  .. note:: This is only intended for quick pretty printing in debugging
   --     for example. If you want more powerful and pretty output of
   --     diagnostics, use :ada:ref`Langkit_Support.Diagnostics.Output`.

   package Diagnostics_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Diagnostic);

   type Diagnostics_Array is array (Positive range <>) of Diagnostic;

   function Create
     (Sloc_Range : Source_Location_Range;
      Message    : Wide_Wide_String) return Diagnostic;
   --  Shortcut to create a diagnostic

   procedure Append
     (Diagnostics : in out Diagnostics_Vectors.Vector;
      Sloc_Range  : Source_Location_Range := No_Source_Location_Range;
      Message     : Wide_Wide_String);
   --  Shortcut to append a diagnostic to a vector

   procedure Append
     (Diagnostics : in out Diagnostics_Vectors.Vector;
      Sloc_Range  : Source_Location_Range := No_Source_Location_Range;
      Exc         : Ada.Exceptions.Exception_Occurrence);
   --  Shortcut to append an exception message to a vector

end Langkit_Support.Diagnostics;
