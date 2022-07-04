--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

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
