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

package body Langkit_Support.Boxes is

   --------------------
   -- Create_Element --
   --------------------

   function Create_Element return Reference is
   begin
      return Ref : Reference do
         Ref.Internal := new Element_Type;
         Set_Refcount (Ref.Internal.all, 1);
      end return;
   end Create_Element;

   ------------
   -- Update --
   ------------

   procedure Update
     (Self     : Reference;
      Callback : access procedure (Self : in out Element_Type)) is
   begin
      Callback.all (Self.Internal.all);
   end Update;

   ---------------------
   -- Internal_Access --
   ---------------------

   function Internal_Access (Self : Reference) return Element_Access is
   begin
      return Self.Internal;
   end Internal_Access;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out Reference) is
   begin
      Self.Internal := null;
   end Initialize;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Reference) is
   begin
      if Self.Internal = null then
         return;
      end if;

      declare
         Element : Element_Type renames Self.Internal.all;
      begin
         Set_Refcount (Element, Refcount (Element) + 1);
      end;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Reference) is
   begin
      if Self.Internal = null then
         return;
      end if;

      declare
         Next_Count : constant Natural := Refcount (Self.Internal.all) - 1;
      begin
         if Next_Count = 0 then
            Release (Self.Internal.all);
            Free (Self.Internal);
         else
            Set_Refcount (Self.Internal.all, Next_Count);
         end if;
      end;
   end Finalize;

end Langkit_Support.Boxes;
