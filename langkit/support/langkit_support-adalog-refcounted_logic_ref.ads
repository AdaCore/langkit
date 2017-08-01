with GNATCOLL.Refcount;

with Langkit_Support.Adalog.Logic_Ref;
with Langkit_Support.Adalog.Logic_Var;

--  This package contains one implementation of logic variables. This is done
--  by implementing base simple types, and instantiating the Adalog.Logic_Var
--  formal package. This package contains the refcounted implementation.

generic
   type Element_Type is private;
   with function Element_Image (E : Element_Type) return String;
package Langkit_Support.Adalog.Refcounted_Logic_Ref is

   procedure Inc_Ref (E : Element_Type) is null;
   procedure Dec_Ref (E : in out Element_Type) is null;

   package LRef is new Langkit_Support.Adalog.Logic_Ref
     (Element_Type, Inc_Ref, Dec_Ref, Element_Image);

   --------------------------------------
   -- Referenced counted variable type --
   --------------------------------------

   --  This type is a reference counted logic variable type, to use if you
   --  don't care about performance and want automatic deallocation.

   type Refcounted_El is new GNATCOLL.Refcount.Refcounted with record
      Content : LRef.Var;
   end record;

   package Refs is new GNATCOLL.Refcount.Shared_Pointers (Refcounted_El);
   type Ref is new Refs.Ref with null record;

   procedure Inc_Ref (Self : Ref) is null;
   procedure Dec_Ref (Self : in out Ref) is null;

   procedure Reset (Self : in out Ref);
   function Is_Defined (Self : Ref) return Boolean;
   procedure Set_Value (Self : in out Ref; Data : Element_Type);
   function Get_Value (Self : Ref) return Element_Type;

   function Image (Self : Ref) return String is
     (LRef.Image (Self.Unchecked_Get.Content));

   function Create return Ref;

   --  Refcounted one

   package Refcounted_Logic_Var is new Adalog.Logic_Var
     (Ref, Element_Type, Inc_Ref, Dec_Ref);

end Langkit_Support.Adalog.Refcounted_Logic_Ref;
