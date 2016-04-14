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

with GNATCOLL.Refcount; use GNATCOLL.Refcount;

package body Adalog.Logic_Ref is

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

   ----------
   -- SetL --
   ----------

   procedure SetL (Self : in out Var; Data : Element_Type) is
   begin
      Self.El := Data;
      Self.Reset := False;
   end SetL;

   ----------
   -- GetL --
   ----------

   function GetL (Self : Var) return Element_Type is
   begin
      pragma Assert (Self.Reset = False);
      return Self.El;
   end GetL;

   ---------
   -- Set --
   ---------

   procedure SetL (Self : in out Ref; Data : Element_Type) is
   begin
      SetL (Self.Unchecked_Get.Content, Data);
   end SetL;

   ---------
   -- Get --
   ---------

   function GetL (Self : Ref) return Element_Type is
   begin
      return GetL (Self.Unchecked_Get.Content);
   end GetL;

   ------------
   -- Create --
   ------------

   function Create (El : Element_Type) return Ref is
      R : Ref := Create;
   begin
      R.SetL (El);
      return R;
   end Create;

   ------------
   -- Create --
   ------------

   function Create return Ref is
   begin
      return Self : Ref do
         Refs.Set
           (Refs.Ref (Self),
            Refcounted_El'
              (Refcounted with Content => (El => <>, Reset => True)));
      end return;
   end Create;

   -----------
   -- Reset --
   -----------

   procedure Reset (Self : in out Ref) is
   begin
      Reset (Self.Unchecked_Get.Content);
   end Reset;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined (Self : Ref) return Boolean is
     (Is_Defined (Self.Unchecked_Get.Content));

   -----------
   -- Reset --
   -----------

   pragma Warnings (Off);
   procedure Reset (Self : in out Raw_Var) is
   begin
      Reset (Self.all);
   end Reset;
   pragma Warnings (On);

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined (Self : Raw_Var) return Boolean is
   begin
      return Is_Defined (Self.all);
   end Is_Defined;

   ----------
   -- SetL --
   ----------

   pragma Warnings (Off);
   procedure SetL (Self : in out Raw_Var; Data : Element_Type) is
   begin
      SetL (Self.all, Data);
   end SetL;
   pragma Warnings (On);

   ----------
   -- GetL --
   ----------

   function GetL (Self : Raw_Var) return Element_Type is
   begin
      return GetL (Self.all);
   end GetL;

   ------------
   -- Create --
   ------------

   function Create return Raw_Var is
   begin
      return new Var'(Reset => True, El => <>);
   end Create;

end Adalog.Logic_Ref;
