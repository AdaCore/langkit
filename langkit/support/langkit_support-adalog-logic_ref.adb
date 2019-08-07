------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Langkit_Support.Adalog.Debug; use Langkit_Support.Adalog.Debug;

package body Langkit_Support.Adalog.Logic_Ref is

   pragma Warnings (Off, "always False");

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Var) is
   begin
      Self.Reset := True;
   end Reset;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined (Self : Var) return Boolean is
   begin
      return not Self.Reset;
   end Is_Defined;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Self : in out Var; Data : Element_Type) is
   begin
      if Debug.Debug then
         Verbose_Trace.Trace ("Setting the value of " & Image (Self) & " to "
                & Element_Image (Data));
         Verbose_Trace.Trace ("Old value is " & Element_Image (Self.Value));
      end if;

      Dec_Ref (Self.Value);
      Self.Value := Data;
      Inc_Ref (Self.Value);
      Self.Reset := False;
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Self : Var) return Element_Type is
   begin
      Inc_Ref (Self.Value);
      --  TODO??? We removed an assert about Self.Reset being False, because
      --  we want to be able to access the variable even if the element is
      --  unset, eg. null. However, we need to have a definite null value for
      --  elements, which could even replace the Reset flag altogether maybe.
      return Self.Value;
   end Get_Value;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : Raw_Var) is
   begin
      Reset (Self.all);
   end Reset;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined (Self : Raw_Var) return Boolean is
   begin
      if Self.Aliased_To /= null then
         return Is_Defined (Self.Aliased_To);
      end if;
      return Is_Defined (Self.all);
   end Is_Defined;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Self : Raw_Var; Data : Element_Type) is
   begin
      if Self.Aliased_To /= null then
         Set_Value (Self.Aliased_To, Data);
      end if;
      Set_Value (Self.all, Data);
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Self : Raw_Var) return Element_Type is
   begin
      if Self.Aliased_To /= null then
         return Get_Value (Self.Aliased_To);
      end if;
      return Get_Value (Self.all);
   end Get_Value;

   -----------
   -- Alias --
   -----------

   procedure Alias (Self, To : Raw_Var) is
   begin
      if To = Self or else To.Aliased_To = Self then
         return;
      elsif Self.Aliased_To = null then
         Self.Aliased_To := To;
      else
         Alias (Self.Aliased_To, To);
      end if;
   end Alias;

   -------------
   -- Unalias --
   -------------

   procedure Unalias (Self : Raw_Var) is
   begin
      Self.Aliased_To := null;
   end Unalias;

   -----------
   -- Alias --
   -----------

   function Get_Alias (Self : Raw_Var) return Raw_Var is
   begin
      return Self.Aliased_To;
   end Get_Alias;

   ------------
   -- Create --
   ------------

   function Create return Raw_Var is
   begin
      return new Var'(Reset => True, others => <>);
   end Create;

   --------
   -- Id --
   --------

   function Id (Self : Raw_Var) return Natural
   is
     (if Self.Aliased_To /= null then Id (Self.Aliased_To) else Self.Id);

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id (Self : Raw_Var; Id : Natural)
   is
   begin
      if Id = 0 then
         Self.Aliased_To := null;
      elsif Self.Aliased_To /= null then
         Set_Id (Self.Aliased_To, Id);
      end if;
      Self.Id := Id;
   end Set_Id;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Var) is
      procedure Destroy is new Ada.Unchecked_Deallocation
        (String, String_Access);
   begin
      Dec_Ref (Self.Value);
      Destroy (Self.Dbg_Name);
   end Destroy;

end Langkit_Support.Adalog.Logic_Ref;
