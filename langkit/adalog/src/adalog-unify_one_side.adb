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

with Ada.Unchecked_Deallocation;

package body Adalog.Unify_One_Side is

   use Var;

   -----------
   -- Apply --
   -----------

   function Apply (Self : in out Unify) return Boolean is
   begin
      if Is_Defined (Self.Left) then
         return Equals (GetL (Self.Left), Self.Right);
      end if;

      Self.Changed := True;
      SetL (Self.Left, Convert (Self.Right));
      return True;
   end Apply;

   ------------
   -- Revert --
   ------------

   procedure Revert (Self : in out Unify) is
   begin
      if Self.Changed then
         Reset (Self.Left);
         Self.Changed := False;
      end if;
   end Revert;

   ----------
   -- Call --
   ----------

   function Call (Self : in out Member_T) return Boolean is
   begin
      if Self.Current_Index in Self.Values.all'Range then
         if Is_Defined (Self.Left) and then not Self.Changed then

            if Self.Domain_Checked then
               return False;
            end if;

            Self.Domain_Checked := True;
            for V of Self.Values.all loop
               if GetL (Self.Left) = Convert (V) then
                  return True;
               end if;
            end loop;
            return False;
         else
            Self.Current_Index := Self.Current_Index + 1;
            SetL (Self.Left, Convert (Self.Values (Self.Current_Index - 1)));
            Self.Changed := True;
            return True;
         end if;
      else
         if Self.Changed then
            Reset (Self.Left);
         end if;
         return False;
      end if;
   end Call;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Member_T) is
   begin
      Self.Domain_Checked := False;
      if Self.Changed then
         Reset (Self.Left);
         Self.Changed := False;
      end if;
      Self.Current_Index := 1;
   end Reset;

   ------------
   -- Member --
   ------------

   function Member
     (R : Var.Var; Vals : R_Type_Array) return Relation is
   begin
      return new Member_T'
        (Left           => R,
         Values         => new R_Type_Array'(Vals),
         Current_Index  => 1,
         Changed        => False,
         Domain_Checked => False,
         others         => <>);
   end Member;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Member_T) is
      procedure Unchecked_Free
      is new Ada.Unchecked_Deallocation (R_Type_Array, R_Type_Array_Access);
   begin
      Unchecked_Free (Self.Values);
   end Free;

end Adalog.Unify_One_Side;
