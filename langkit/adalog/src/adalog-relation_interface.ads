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

--  This generic wrapper package will take a private type Ty, that implicitly
--  implements the relation protocol, and will create:
--  * A wrapper subtype to I_Relation that implements the dynamic interface.
--  * A constructor, that will take an instance of Ty and return a Rel.

generic
   type Ty is private;
   with function Call (Inst : in out Ty) return Boolean is <>;
   with procedure Reset (Inst : in out Ty) is <>;
   with procedure Free (Inst : in out Ty) is <>;
package Adalog.Relation_Interface is

   pragma Suppress (Accessibility_Check);

   type Rel is new I_Relation with record
      Inst : Ty;
   end record;
   type Relation_Access is access all Rel;

   overriding function Call (Inst : in out Rel) return Boolean;
   overriding procedure Reset (Inst : in out Rel);
   overriding procedure Free (Inst : in out Rel);

   function Dynamic (From : Ty) return access I_Relation'Class;
end Adalog.Relation_Interface;
