------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
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

   procedure Set_Value (Self : in out Ref; Data : Element_Type) is
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

   procedure Reset (Self : in out Ref) is
   begin
      LRef.Reset (Self.Unchecked_Get.Content);
   end Reset;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined (Self : Ref) return Boolean is
     (LRef.Is_Defined (Self.Unchecked_Get.Content));

end Langkit_Support.Adalog.Refcounted_Logic_Ref;
