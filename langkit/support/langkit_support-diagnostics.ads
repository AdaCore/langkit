--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
