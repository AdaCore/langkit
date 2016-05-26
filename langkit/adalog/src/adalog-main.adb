------------------------------------------------------------------------------
--                               A D A L O G                                --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;              use Ada.Text_IO;

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;
with Adalog.Dynamic_Ops;       use Adalog.Dynamic_Ops;
with Adalog.Main_Support;      use Adalog.Main_Support;

procedure Adalog.Main is
   pragma Warnings (Off, "reference");

   function Is_Even (X : Integer) return Boolean is ((X mod 2) = 0);

   use Eq_Int; use Eq_Int.Raw_Impl;

   X : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;
   Y : constant Eq_Int.Refs.Raw_Var := Eq_Int.Refs.Create;

   A : constant Raw_Member_Array := (5, 11, 12, 13, 6, 14);

   R3 : Relation :=
     (Member (X, (1, 2, 3, 4, 5, 6))
      and Member (Y, A)
      and X = Y);
--        and Pred_Int.Create (X, Is_Even'Access));

   Discard : Boolean;

begin
   for R in 1 .. 10 loop

      while Call (R3) loop
         Put_Line (Eq_Int.Refs.GetL (X)'Img);
      end loop;

      Reset (R3);
   end loop;

   Free (R3);
end Adalog.Main;
