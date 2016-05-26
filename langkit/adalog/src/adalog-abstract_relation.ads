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

package Adalog.Abstract_Relation is

   ----------------
   -- I_Relation --
   ----------------

   --  I_Relation is the base dynamic type for a relation. Since in Adalog
   --  the base for a relation is static and ad-hoc (any type implementing the
   --  necessary subprograms can be considered a relation), wrappers can be
   --  created automatically to convert a type implementing the static contract
   --  into a I_Relation. See Adalog.Dynamic_Relation.
   --
   --  Dynamic relations are provided for two reasons:
   --
   --  1. For convenience. Creating the necessary generic instantiations for
   --  complex relation trees can be very tedious.
   --
   --  2. For expressivity. Sometimes you want to be able to create a relation
   --  between two relations A and B, such as for example `A or B`, without
   --  knowing the exact static types of A and B.

   type I_Relation is interface;

   function Call (Inst : in out I_Relation) return Boolean is abstract;
   pragma Inline (Call);

   procedure Reset (Self : in out I_Relation) is abstract;
   pragma Inline (Call);

   procedure Free (Self : in out I_Relation) is abstract;
   pragma Inline (Free);

   type I_Relation_Access is access all I_Relation'Class;

   subtype Relation is I_Relation_Access;

   function Call (Self : in out Relation) return Boolean
   is (Call (Self.all));

   procedure Reset (Self : in out Relation);

   procedure Free (Self : in out Relation);

end Adalog.Abstract_Relation;
