--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Unchecked_Deallocation;
with Langkit_Support.Array_Utils;

package body Langkit_Support.Cheap_Sets is

   function Find
     (Self      : Set;
      E         : Element_Type;
      Do_Remove : Boolean := True) return Boolean;

   ------------
   -- Remove --
   ------------

   function Remove
     (Self      : Set;
      E         : Element_Type) return Boolean is
   begin
      return Find (Self, E, Do_Remove => True);
   end Remove;

   ---------
   -- Add --
   ---------

   function Add (Self : in out Set; E : Element_Type) return Boolean is
      use Elements_Vectors;

      type El_Access is access all Element_Type;
      First_No : El_Access := null;
   begin

      --  Create the inner vector if it has not been created yet

      if Self.Elements = null then
         Self.Elements := new Elements_Vectors.Vector;
      end if;

      --  Search for an element equal to E. Also search for the first
      --  No_Element in the same run.

      for I in First_Index (Self.Elements.all)
        .. Last_Index (Self.Elements.all)
      loop
         declare
            Current : constant Element_Access
              := Get_Access (Self.Elements.all, I);
         begin

            --  We found a No_Element. Keep it for later if we have to add E to
            --  the set.

            if Current.all = No_Element then
               First_No := El_Access (Current);
            end if;

            --  We found E. Return False, no need to insert anything in the
            --  set.

            if Current.all = E then
               return False;
            end if;
         end;
      end loop;

      --  Add the element to the set, either at the first empty cell we found,
      --  or in a new cell.

      if First_No /= null then
         First_No.all := E;
      else
         Append (Self.Elements.all, E);
      end if;

      return True;
   end Add;

   ----------
   -- Find --
   ----------

   function Find
     (Self      : Set;
      E         : Element_Type;
      Do_Remove : Boolean := True) return Boolean
   is
      use Elements_Vectors;
   begin
      --  If inner vector was not created, we're sure there is no element
      --  corresponding to E.

      if Self.Elements = null then
         return False;
      end if;

      --  Else, let's search in the inner vector for E

      for I in First_Index (Self.Elements.all)
        .. Last_Index (Self.Elements.all)
      loop
         declare
            Current : constant Element_Access
              := Get_Access (Self.Elements.all, I);
         begin

            --  We found something, delete the element if specified and return
            --  True.

            if Current.all = E then
               if Do_Remove then
                  Current.all := No_Element;
               end if;
               return True;
            end if;
         end;
      end loop;

      --  We found nothing

      return False;
   end Find;

   ---------
   -- Has --
   ---------

   function Has (Self : Set; E : Element_Type) return Boolean is
   begin
      return Find (Self, E, False);
   end Has;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Set) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Elements_Vectors.Vector, Elements_Vector);
   begin
      if Self.Elements /= null then
         Elements_Vectors.Destroy (Self.Elements.all);
         Unchecked_Free (Self.Elements);
      end if;
   end Destroy;

   package Elements_Arrays
   is new Langkit_Support.Array_Utils
     (Element_Type, Positive, Elements_Vectors.Elements_Array);

   --------------
   -- Elements --
   --------------

   function Elements (Self : Set) return Elements_Vectors.Elements_Array is

      function Not_Null (E : Element_Type) return Boolean
      is (E /= No_Element);
      function Filter_Null is new Elements_Arrays.Filter_Gen (Not_Null);
   begin
      return (if Self.Elements = null
              then Elements_Arrays.Empty_Array
              else Filter_Null
                (Elements_Vectors.To_Array (Self.Elements.all)));
   end Elements;

end Langkit_Support.Cheap_Sets;
