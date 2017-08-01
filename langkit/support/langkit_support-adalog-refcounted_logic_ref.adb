with GNATCOLL.Refcount; use GNATCOLL.Refcount;

package body Langkit_Support.Adalog.Refcounted_Logic_Ref is

   pragma Warnings (Off, "always False");

   ---------------
   -- Set_Value --
   ---------------

   function Set_Value
     (Self : in out Ref; Data : Element_Type) return Boolean
   is
   begin
      return LRef.Set_Value (Self.Unchecked_Get.Content, Data);
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
