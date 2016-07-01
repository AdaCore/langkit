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

with Adalog.Debug; use Adalog.Debug;

package body Adalog.Unify_LR is
   use Left_Var; use Right_Var;

   -----------
   -- Apply --
   -----------

   function Apply (Self : in out Unify_LR) return Boolean is
   begin
      Trace ("In Unify_LR");

      if Is_Defined (Self.Left) then

         --  Both values are defined, return true if they are equal
         if Is_Defined (Self.Right) then
            return C : Boolean do
               C :=
                 Convert (Self.L_Data, GetL (Self.Left)) = GetL (Self.Right);
               Trace ("In Unify_LR, both defined, returning " & C'Image);
            end return;

         end if;

         --  Left is defined, right is not, give right the value of left and
         --  return true.
         if SetL (Self.Right, Convert (Self.L_Data, GetL (Self.Left))) then
            Self.State := Right_Changed;
            Trace ("In Unify_LR, propagating right");
            return True;
         else
            Trace ("In Unify_LR, propagating right failed ! ");
            return False;
         end if;
      end if;

      if not Is_Defined (Self.Right) then
         --  TODO??? Another solution, rather than force the users to define
         --  domains for variables before binding them together, would be to
         --  defer unification, like it is done for predicates.
         --
         --  It is not sure it would be better: The outcome would probably
         --  be potentially slower solving. However, it might be that some
         --  equations cannot be expressed easily if we don't have unification
         --  defer.

         raise Early_Binding_Error
           with "Variable " & Image (Self.Right) & " and " & Image (Self.Left)
           & " are not defined yet.";
      end if;

      --  Right is defined, left is not, give left the value of right and
      --  return true.
      if SetL (Self.Left, Convert (Self.R_Data, GetL (Self.Right))) then
         Self.State := Left_Changed;
         return True;
      else
         Trace ("In Unify_LR, propagating left failed ! ");
         return False;
      end if;
   end Apply;

   ------------
   -- Revert --
   ------------

   procedure Revert (Self : in out Unify_LR) is
   begin
      case Self.State is
         when Left_Changed => Reset (Self.Left);
         when Right_Changed => Reset (Self.Right);
         when others => null;
      end case;
      Self.State := No_Change;
   end Revert;
end Adalog.Unify_LR;
