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

   type I_Relation is abstract tagged null record;
   --  Base type for a type implementing the relation interface. A relation has
   --  the following properties:
   --
   --  - It keeps references to zero or more sub-relations, relations that
   --    it will expand. For example, an AND relation will have two (or more)
   --    sub-relations.
   --
   --  - It keeps reference to zero or more logical variables, that it will
   --    bound when solved.
   --
   --  - It can be solved, and that implies eventually solving sub relations,
   --    and eventually binding logic variables.
   --
   --  - It keeps the current state of solving, which is necessary since
   --    relation systems possibly have multiple solutions. This state can
   --    be reset via the Reset primitive.

   function Call (Inst : in out I_Relation) return Boolean is abstract;
   --  Solve the relation system. Iff the solve process did issue a correct
   --  solution, this will return True, and all logic variables bound by the
   --  relation will have a value.

   procedure Reset (Self : in out I_Relation) is abstract;
   --  Reset the state of the relation and all sub-relations

   procedure Free (Self : in out I_Relation) is abstract;
   --  Frees ressources associated with Self. Relation trees are owning, so
   --  that when you free one relation, you will free all sub relations. This
   --  means that a sub-relation cannot be shared in different relation trees.
   --  This is in line with the relation design, since a relation can have
   --  state specific to a system, it should not be shared.

   type Relation is access all I_Relation'Class;
   --  Since relations are trees, they're not manipulated by value, but instead
   --  via this class-wide access type.

   function Call (Self : Relation) return Boolean is (Self.all.Call);
   --  Shortcut to call the underlying relation, used by langkit

   type Relation_Array is array (Natural range <>) of Relation;
   --  Some relations will need to keep/provide arrays of sub-relations

end Adalog.Abstract_Relation;
