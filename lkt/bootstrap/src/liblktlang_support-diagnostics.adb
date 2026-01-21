--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO; use Ada.Text_IO;

package body Liblktlang_Support.Diagnostics is

   function Sloc_Prefix (Sloc_Range : Source_Location_Range) return String;
   --  If ``Sloc_Range`` is not null, return a "X:Y: " prefix with the
   --  corresponding start line/column numbers.

   ---------------------------------
   -- Diagnostics sorting helpers --
   ---------------------------------

   --  Keep track of the original order for diagnostics so that we can preserve
   --  order for diagnostics with the same sloc range.

   type Ordered_Diag is record
      Sloc_Range     : Source_Location_Range;
      Original_Index : Positive;
      Message        : Unbounded_Text_Type;
   end record;

   function "<" (Left, Right : Ordered_Diag) return Boolean
   is (if Left.Sloc_Range = Right.Sloc_Range
       then Left.Original_Index < Right.Original_Index
       else Left.Sloc_Range < Right.Sloc_Range);

   package Ordered_Diag_Vectors is new Ada.Containers.Vectors
     (Positive, Ordered_Diag);

   package Sorting is new Ordered_Diag_Vectors.Generic_Sorting;

   -----------------
   -- Sloc_Prefix --
   -----------------

   function Sloc_Prefix (Sloc_Range : Source_Location_Range) return String is
      Sloc : constant Source_Location := Start_Sloc (Sloc_Range);
   begin
      return
        (if Sloc = No_Source_Location
         then ""
         else Image (Sloc) & ": ");
   end Sloc_Prefix;

   ----------------------
   -- To_Pretty_String --
   ----------------------

   function To_Pretty_String (D : Diagnostic) return String is
   begin
      return Sloc_Prefix (D.Sloc_Range) & Image (To_Text (D.Message));
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

   ----------
   -- Sort --
   ----------

   procedure Sort (Diagnostics : in out Diagnostics_Vectors.Vector) is
      OD : Ordered_Diag_Vectors.Vector;
   begin
      for I in 1 .. Diagnostics.Last_Index loop
         declare
            D : Diagnostic renames Diagnostics (I);
         begin
            OD.Append (Ordered_Diag'(D.Sloc_Range, I, D.Message));
         end;
      end loop;
      Sorting.Sort (OD);

      Diagnostics.Clear;
      for D of OD loop
         Diagnostics.Append (Diagnostic'(D.Sloc_Range, D.Message));
      end loop;
   end Sort;

   -----------
   -- Print --
   -----------

   procedure Print
     (Diagnostics : Diagnostics_Vectors.Vector;
      Prefix      : String := "error: ";
      Indent      : Natural := 2) is
   begin
      for D of Diagnostics loop
         Put (Sloc_Prefix (D.Sloc_Range));
         Put (Prefix);
         declare
            Empty_Line : Boolean := False;
            Msg        : constant String := To_UTF8 (To_Text (D.Message));
         begin
            for C of Msg loop
               if C = ASCII.LF then
                  New_Line;
                  Empty_Line := True;
               else
                  if Empty_Line then
                     Put ((1 .. Indent => ' '));
                     Empty_Line := False;
                  end if;
                  Put (C);
               end if;
            end loop;
         end;
         New_Line;
      end loop;
   end Print;

end Liblktlang_Support.Diagnostics;
