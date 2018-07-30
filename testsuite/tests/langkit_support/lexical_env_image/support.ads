with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;

with System;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Text; use Langkit_Support.Text;

package Support is

   type String_Access is access all String;
   procedure Destroy is new Ada.Unchecked_Deallocation (String, String_Access);

   type Metadata is record
      I : Integer;
   end record;

   Default_MD : constant Metadata := (I => 0);

   function Element_Hash (S : String_Access) return Hash_Type is (0);
   function Metadata_Hash (MD : Metadata) return Hash_Type is (0);
   procedure Raise_Property_Error (Message : String := "");
   function Combine (L, R : Metadata) return Metadata is ((I => L.I + R.I));
   function Parent (S : String_Access) return String_Access is
     (new String'("Parent(" & S.all & ")"));
   function Can_Reach (Node, From : String_Access) return Boolean is (True);
   function Is_Rebindable (Node : String_Access) return Boolean is (True);

   function Element_Image
     (Node : String_Access; Short : Boolean := True) return Text_Type
   is (To_Text ("<" & Node.all & ">"));

   procedure Register_Rebinding
     (Node : String_Access; Rebinding : System.Address) is null;

   function Get_Version (B : Boolean) return Natural is (0);

   package Envs is new Langkit_Support.Lexical_Env
     (Unit_T               => Boolean,
      Get_Version          => Get_Version,
      No_Unit              => False,
      Element_T            => String_Access,
      Element_Metadata     => Metadata,
      No_Element           => new String'(""),
      Empty_Metadata       => Default_MD,
      Element_Hash         => Element_Hash,
      Metadata_Hash        => Metadata_Hash,
      Raise_Property_Error => Raise_Property_Error,
      Combine              => Combine,
      Can_Reach            => Can_Reach,
      Is_Rebindable        => Is_Rebindable,
      Element_Image        => Element_Image,
      Register_Rebinding   => Register_Rebinding);

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Envs.Env_Rebindings_Type, Envs.Env_Rebindings);

end Support;
