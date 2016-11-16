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

with Adalog.Debug;      use Adalog.Debug;

package body Adalog.Logic_Ref is

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

   ----------
   -- SetL --
   ----------

   function Set_Value (Self : access Var; Data : Element_Type) return Boolean
   is
      Old   : aliased constant Var := Self.all;
   begin
      if Debug_State = Trace then
         Trace ("Setting the value of " & Image (Raw_Var (Self)) & " to "
                & Element_Image (Data));
         Trace ("Old value is " & Element_Image (Old.El));
      end if;
      --  First set the value

      Self.El := Data;
      Self.Reset := False;

      --  Then check if we have pending relations, and if they evaluate to
      --  True.

      for El of Pred_Sets.Elements (Self.Pending_Relations) loop
         if Debug_State = Trace then
            Trace ("Applying predicate on " & Image (Raw_Var (Self)));
         end if;

         if not El.Apply then
            Trace ("Applying predicate failed");
            Self.all := Old;

            if Debug_State = Trace then
               Trace ("Self element value is now " & Element_Image (Self.El));
            end if;

            return False;
         end if;
      end loop;

      return True;
   end Set_Value;

   ----------
   -- GetL --
   ----------

   function GetL (Self : Var) return Element_Type is
   begin
      --  TODO??? We removed an assert about Self.Reset being False, because
      --  we want to be able to access the variable even if the element is
      --  unset, eg. null. However, we need to have a definite null value for
      --  elements, which could even replace the Reset flag altogether maybe.
      return Self.El;
   end GetL;

   ---------
   -- Set --
   ---------

   function SetL (Self : in out Ref; Data : Element_Type) return Boolean is
   begin
      return Set_Value (Self.Unchecked_Get.Content'Unrestricted_Access, Data);
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
   function SetL (Self : in out Raw_Var; Data : Element_Type) return Boolean is
   begin
      return Set_Value (Self, Data);
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
      return new Var'(Reset => True, others => <>);
   end Create;

   -------------------
   -- Add_Predicate --
   -------------------

   procedure Add_Predicate (Self : in out Var; Pred : Var_Predicate) is
      use Pred_Sets;
      Dummy : Boolean := Add (Self.Pending_Relations, Pred);
   begin
      null;
   end Add_Predicate;

   ----------------------
   -- Remove_Predicate --
   ----------------------

   procedure Remove_Predicate (Self : in out Var; Pred : Var_Predicate) is
      use Pred_Sets;
      Dummy : Boolean := Remove (Self.Pending_Relations, Pred);
   begin
      Trace ("In remove predicate");
      null;
   end Remove_Predicate;

   ----------------------
   -- Remove_Predicate --
   ----------------------

   procedure Remove_Predicate (Self : Ref; Pred : Var_Predicate)
   is
   begin
      Remove_Predicate (Self.Unchecked_Get.Content, Pred);
   end Remove_Predicate;

   -------------------
   -- Add_Predicate --
   -------------------

   procedure Add_Predicate (Self : Ref; Pred : Var_Predicate) is
   begin
      Add_Predicate (Self.Unchecked_Get.Content, Pred);
   end Add_Predicate;

   ----------------------
   -- Remove_Predicate --
   ----------------------

   procedure Remove_Predicate (Self : Raw_Var; Pred : Var_Predicate) is
   begin
      Remove_Predicate (Self.all, Pred);
   end Remove_Predicate;

   -------------------
   -- Add_Predicate --
   -------------------

   procedure Add_Predicate (Self : Raw_Var; Pred : Var_Predicate) is
   begin
      Add_Predicate (Self.all, Pred);
   end Add_Predicate;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Var) is
   begin
      Pred_Sets.Destroy (Self.Pending_Relations);
   end Destroy;

end Adalog.Logic_Ref;
