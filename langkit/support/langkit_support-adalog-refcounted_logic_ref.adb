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

with GNATCOLL.Refcount; use GNATCOLL.Refcount;

package body Langkit_Support.Adalog.Refcounted_Logic_Ref is

   pragma Warnings (Off, "always False");

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Self : Ref; Data : Element_Type) is
   begin
      LRef.Set_Value (Self.Unchecked_Get.Content, Data);
   end Set_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value (Self : Ref) return Element_Type is
   begin
      return LRef.Get_Value (Self.Unchecked_Get.Content);
   end Get_Value;

   ------------
   -- Create --
   ------------

   function Create return Ref is
   begin
      return Self : Ref do
         Refs.Set
           (Refs.Ref (Self),
            Refcounted_El'
              (Refcounted with Content => (Reset => True, others => <>)));
      end return;
   end Create;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : Ref) is
   begin
      LRef.Reset (Self.Unchecked_Get.Content);
   end Reset;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined (Self : Ref) return Boolean is
     (LRef.Is_Defined (Self.Unchecked_Get.Content));

   --------
   -- Id --
   --------

   function Id (Self : Ref) return Natural is
     (LRef.Id (Self.Unchecked_Get.Content'Unrestricted_Access));

   ------------
   -- Set_Id --
   ------------

   procedure Set_Id (Self : Ref; Id : Natural) is
   begin
      LRef.Set_Id (Self.Unchecked_Get.Content'Unrestricted_Access, Id);
   end Set_Id;

   -----------
   -- Alias --
   -----------

   procedure Alias (Self, Other : Ref) is
   begin
      LRef.Alias (Self.Unchecked_Get.Content'Unrestricted_Access,
                  Other.Unchecked_Get.Content'Unrestricted_Access);
   end Alias;

   procedure Unalias (Self : Ref) is
   begin
      LRef.Unalias (Self.Unchecked_Get.Content'Unrestricted_Access);
   end Unalias;

   ---------------
   -- Get_Alias --
   ---------------

   function Get_Alias (Self : Ref) return Ref is
      use LRef;
      Raw : constant Raw_Var :=
        Get_Alias (Self.Unchecked_Get.Content'Unrestricted_Access);
      Ret : Ref;
   begin
      if Raw = No_Var then
         return No_Ref;
      else
         Refs.Set
           (Ret,
            Refcounted_El'
              (Refcounted with Content => Raw.all));
         return Ret;
      end if;
   end Get_Alias;

end Langkit_Support.Adalog.Refcounted_Logic_Ref;
