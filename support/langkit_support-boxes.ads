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

--  This package provides a simple implementation for boxing types: types that
--  are dynamically allocated, handled by reference and with automatic memory
--  management thanks to reference counting.
--
--  The main difference with GNATCOLL.Refcount is that here, Element_Type is
--  limited and constrained while in GNATCOLL.Refcount, Element_Type is not
--  limited and unconstrained.

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

generic
   type Element_Type is limited private;
   --  The type to be boxed

   with function Refcount (Self : Element_Type) return Positive is <>;
   --  Return Self's reference count

   with procedure Set_Refcount (Self : in out Element_Type; Count : Positive);
   --  Set Self's reference count to Count

   with procedure Release (Self : in out Element_Type);
   --  Release all resources in Element_Type. This is called when the reference
   --  count drops to 0.

package Langkit_Support.Boxes is

   type Element_Access is access all Element_Type;

   type Reference is private;
   --  Ref-counted reference to an Element_Type value

   No_Reference : constant Reference;
   --  Null reference (no element is referenced)

   function Create_Element return Reference;
   --  Allocate a new (uninitialized) element and return a reference to it

   procedure Update
     (Self     : Reference;
      Callback : access procedure (Self : in out Element_Type));
   --  Call Callback on the referenced Element_Type value

   function Internal_Access (Self : Reference) return Element_Access;
   --  Return an access to the referenced Element_Type value. This is unsafe,
   --  but can be a lot easier to use than the Update reference. As long as the
   --  returned Element_Access value does not outlive Self, all should be fine.

private

   procedure Free is new Ada.Unchecked_Deallocation
     (Element_Type, Element_Access);

   type Reference is new Ada.Finalization.Controlled with record
      Internal : Element_Access;
   end record;

   overriding procedure Initialize (Self : in out Reference);
   overriding procedure Adjust (Self : in out Reference);
   overriding procedure Finalize (Self : in out Reference);

   No_Reference : constant Reference :=
     (Ada.Finalization.Controlled with Internal => null);

end Langkit_Support.Boxes;
