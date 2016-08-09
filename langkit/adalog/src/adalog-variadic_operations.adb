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

with Adalog.Operations; use Adalog.Operations;

package body Adalog.Variadic_Operations is

   ------------------
   -- Variadic_And --
   ------------------

   function Variadic_And (Rels : Relation_Array) return Relation is
      Ret, Old_Ret : Relation;
   begin
      pragma Assert (Rels'Length > 0);

      Ret := Rels (Rels'First);
      Inc_Ref (Ret);

      --  We Inc_Ref here because:
      --
      --  - If Rels has only one element, we return a new ownership share for
      --    an already existing relation.
      --
      --  - If Rels has several elements, this share will be automatically
      --    Dec_Ref'd in the for loop below.

      for I in Rels'First + 1 .. Rels'Last loop
         Old_Ret := Ret;
         Ret := Relation (Ret and Rels (I));

         Dec_Ref (Old_Ret);
         --  Here we Dec_Ref, because either Old_Ret is the first rel and
         --  has been Inc_Ref'd before, either it is an And relation that was
         --  created with an ownership that we now renounce, because it is
         --  owned by the new And.
      end loop;

      return Ret;
   end Variadic_And;

   -----------------
   -- Variadic_Or --
   -----------------

   function Variadic_Or (Rels : Relation_Array) return Relation is
      Ret, Old_Ret : Relation;
   begin
      --  See Variadic_Or for documentation of the memory management
      pragma Assert (Rels'Length > 0);

      Ret := Rels (Rels'First);
      Inc_Ref (Ret);

      for I in Rels'First + 1 .. Rels'Last loop
         Old_Ret := Ret;
         Ret := Ret or Rels (I);
         Dec_Ref (Old_Ret);
      end loop;

      return Ret;
   end Variadic_Or;

end Adalog.Variadic_Operations;
