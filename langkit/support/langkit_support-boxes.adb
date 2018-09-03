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
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.   See the  GNU  General --
-- Public License for more details.  You should have received a copy of the --
-- GNU  General  Public  License  distributed with this software;  see file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
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
      Element : Element_Type renames Self.Internal.all;
   begin
      Set_Refcount (Element, Refcount (Element) + 1);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Reference) is
      Next_Count : constant Natural := Refcount (Self.Internal.all);
   begin
      if Next_Count = 0 then
         Release (Self.Internal.all);
         Free (Self.Internal);
      else
         Set_Refcount (Self.Internal.all, Next_Count);
      end if;
   end Finalize;

end Langkit_Support.Boxes;
