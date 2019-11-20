with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Strings; use GNATCOLL.Strings;

with Langkit_Support.Adalog.Generic_Main_Support;
with Langkit_Support.Images;

package Main_Support is

   type Kind is (Int, Str);

   type Val_Type (K : Kind := Int) is record
      case K is
         when Int => Int_Val : Integer;
         when Str => String_Val : String_Access;
      end case;
   end record;

   function Image (Self : Val_Type) return String is
     (case Self.K is
         when Int => Langkit_Support.Images.Stripped_Image (Self.Int_Val),
         when Str => Self.String_Val.all);

   package Val_Support
   is new Langkit_Support.Adalog.Generic_Main_Support (Val_Type);

end Main_Support;
