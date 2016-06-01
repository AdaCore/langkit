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

with Adalog.Abstract_Relation; use Adalog.Abstract_Relation;

--  This package implement high level logical relation operators on other
--  relations, namely `logical and` and `logical or`.

package Adalog.Operations is

   ---------------------------------
   --  Or relation implementation --
   ---------------------------------

   type Or_Rec is new I_Relation with record
      Left, Right : Relation;
      State : Integer := 0;
   end record;

   overriding function Call (Inst : in out Or_Rec) return Boolean;
   overriding procedure Reset (Inst : in out Or_Rec);
   overriding procedure Free (Inst : in out Or_Rec);

   ----------------------------------
   --  And relation implementation --
   ----------------------------------

   type And_Rec is new I_Relation with record
      Left, Right : Relation;
      State : Integer := 0;
   end record;

   overriding function Call (Inst : in out And_Rec) return Boolean;
   overriding procedure Reset (Inst : in out And_Rec);
   overriding procedure Free (Inst : in out And_Rec);

   ----------------------------------------
   --  Operator overloading constructors --
   ----------------------------------------

   function "or" (L, R : Relation) return access I_Relation'Class
   is (new Or_Rec'(Left => L, Right => R, others => <>)) with Inline_Always;

   function "and" (L, R : Relation) return access I_Relation'Class
   is (new And_Rec'(Left => L, Right => R, others => <>)) with Inline_Always;

   function Logic_Or
     (L, R : Relation) return access I_Relation'Class renames "or";

   function Logic_And
     (L, R : Relation) return access I_Relation'Class renames "and";

end Adalog.Operations;
