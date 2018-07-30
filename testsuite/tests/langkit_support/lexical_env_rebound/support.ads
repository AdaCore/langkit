with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;

with System;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Text; use Langkit_Support.Text;

package Support is

   type Metadata is null record;
   Default_MD : constant Metadata := (others => <>);

   Property_Error: exception;

   function Element_Hash (C : Character) return Hash_Type is (0);
   function Metadata_Hash (MD : Metadata) return Hash_Type is (0);
   procedure Raise_Property_Error (Message : String := "");
   function Combine (L, R : Metadata) return Metadata is ((others => <>));
   function Parent (Node : Character) return Character is (' ');
   function Can_Reach (Node, From : Character) return Boolean is (True);
   function Is_Rebindable (Node : Character) return Boolean is (True);

   function Element_Image
     (Node : Character; Short : Boolean := True) return Text_Type
   is (To_Text ("'" & Node & "'"));

   procedure Register_Rebinding (Node : Character; Rebinding : System.Address)
   is null;

   function Get_Version (B : Boolean) return Natural is (0);

   package Envs is new Langkit_Support.Lexical_Env
     (Unit_T               => Boolean,
      Get_Version          => Get_Version,
      No_Unit              => False,
      Element_T            => Character,
      Element_Metadata     => Metadata,
      No_Element           => ' ',
      Empty_Metadata       => Default_MD,
      Element_Hash         => Element_Hash,
      Metadata_Hash        => Metadata_Hash,
      Raise_Property_Error => Raise_Property_Error,
      Combine              => Combine,
      Can_Reach            => Can_Reach,
      Is_Rebindable        => Is_Rebindable,
      Element_Image        => Element_Image,
      Register_Rebinding   => Register_Rebinding);

   procedure Put_Line (Elements : Envs.Entity_Array);

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Envs.Env_Rebindings_Type, Envs.Env_Rebindings);

end Support;
