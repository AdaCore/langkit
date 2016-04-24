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

with GNATCOLL.Refcount; use GNATCOLL.Refcount;

package body Adalog.Relation_Interface is

   ----------
   -- Call --
   ----------

   overriding function Call (Inst : in out Relation) return Boolean is
   begin
      return Call (Inst.Inst);
   end Call;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Inst : in out Relation) is
   begin
      Reset (Inst.Inst);
   end Reset;

   -------------
   -- Dynamic --
   -------------

   function Dynamic (From : Ty) return Rel
   is
      R : Rel;
      Inst : constant Relation_Access := new Relation'(Inst => From);
   begin
      R.Set
        (Rel_Record'(Refcounted with I_Rel => Inst.all'Unrestricted_Access));
      return R;
   end Dynamic;

end Adalog.Relation_Interface;
