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
with Adalog.Relation_Interface;

--  This package implement high level logical relation operators on other
--  relations, namely `logical and` and `logical or`. It is implemented on
--  generic instances of the Relation_Interface package, so that to use it
--  you must:
--
--  1. Have a/several types that implements the relation protocol.
--
--  2. Instantiate a Relation_Interface package on each.
--
--  3. Instantiate Adalog.Operations between all relation types on which you
--  want to use Operations.
--
--  If you have dynamic relations and you just want to use Or/And on them, see
--  the Adalog.Dynamic_Ops package.

generic
   with package L_Rel is new Relation_Interface (<>);
   with package R_Rel is new Relation_Interface (<>);
package Adalog.Operations is

   ---------------------------------
   --  Or relation implementation --
   ---------------------------------

   type L_Type_Array is array (Natural range <>) of L_Rel.Ty;
   type R_Type_Array is array (Natural range <>) of R_Rel.Ty;

   type Or_Rec is record
      Left  : L_Rel.Ty;
      Right : R_Rel.Ty;
      State : Integer := 0;
   end record;
   function Call (Inst : in out Or_Rec) return Boolean;
   procedure Reset (Inst : in out Or_Rec);
   package Impl is new Relation_Interface (Or_Rec);

   ----------------------------------
   --  And relation implementation --
   ----------------------------------

   type And_Rec is record
      Left  : L_Rel.Ty;
      Right : R_Rel.Ty;
      State : Integer := 0;
   end record;

   function Call (Inst : in out And_Rec) return Boolean;
   procedure Reset (Inst : in out And_Rec);
   package AImpl is new Relation_Interface (And_Rec);

   ----------------------------------------
   --  Operator overloading constructors --
   ----------------------------------------

   function "or" (Left : L_Rel.Ty; Right : R_Rel.Ty) return Or_Rec
   is (Or_Rec'(Left, Right, others => <>)) with Inline_Always;

   function "or" (Left : L_Rel.Ty; Right : R_Rel.Ty) return Rel
   is (Impl.Dynamic (Left or Right)) with Inline_Always;

   function "and" (Left : L_Rel.Ty; Right : R_Rel.Ty) return And_Rec
   is (And_Rec'(Left, Right, others => <>)) with Inline_Always;

   function "and" (Left : L_Rel.Ty; Right : R_Rel.Ty) return Rel
   is (AImpl.Dynamic (Left and Right)) with Inline_Always;

end Adalog.Operations;
