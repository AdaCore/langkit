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

package body Adalog.Relations is

   -------------------
   -- Pure_Relation --
   -------------------

   package body Pure_Relation is

      ----------
      -- Call --
      ----------

      function Call (Inst : in out Rel) return Boolean is
      begin
         if Inst.Done then
            return False;
         end if;
         Inst.Done := True;
         return  Apply (Inst.Rel);
      end Call;

      ----------
      -- Free --
      ----------

      procedure Free (Inst : in out Rel) is
      begin
         Free (Inst.Rel);
      end Free;

   end Pure_Relation;

   -----------------------
   -- Stateful_Relation --
   -----------------------

   package body Stateful_Relation is

      ----------
      -- Call --
      ----------

      function Call (Inst : in out Rel) return Boolean is
      begin
         case Inst.State is
            when Start =>
               if Apply (Inst.Rel) then
                  Inst.State := Success;
                  return True;
               else
                  Inst.State := Finish;
                  return False;
               end if;
            when Success =>
               Revert (Inst.Rel);
               Inst.State := Finish;
               return False;
            when Finish =>
               return False;
         end case;
      end Call;

      -----------
      -- Reset --
      -----------

      procedure Reset (Inst : in out Rel) is
      begin
         Inst.State := Start;
         Revert (Inst.Rel);
      end Reset;

      ----------
      -- Free --
      ----------

      procedure Free (Inst : in out Rel) is
      begin
         Free (Inst.Rel);
      end Free;

   end Stateful_Relation;

end Adalog.Relations;
