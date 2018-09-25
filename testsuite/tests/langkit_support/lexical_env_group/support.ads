with Ada.Containers; use Ada.Containers;

with System;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Types; use Langkit_Support.Types;

package Support is

   type Metadata is null record;
   Default_MD : constant Metadata := (others => <>);

   function Node_Hash (C : Character) return Hash_Type is (0);
   function Metadata_Hash (MD : Metadata) return Hash_Type is (0);
   procedure Raise_Property_Error (Message : String := "");
   function Combine (L, R : Metadata) return Metadata is ((others => <>));
   function Parent (Node : Character) return Character is (' ');
   function Can_Reach (Node, From : Character) return Boolean is (True);
   function Is_Rebindable (Node : Character) return Boolean is (True);

   function Node_Image
     (Node : Character; Short : Boolean := True) return Text_Type
   is (To_Text ("'" & Node & "'"));

   procedure Register_Rebinding (Node : Character; Rebinding : System.Address)
   is null;

   function Get_Version (B : Boolean) return Version_Number is (0);

   type Ref_Category is (No_Cat);
   type Ref_Categories is array (Ref_Category) of Boolean;

   package Envs is new Langkit_Support.Lexical_Env
     (Unit_T               => Boolean,
      Get_Version          => Get_Version,
      No_Unit              => False,
      Node_Type            => Character,
      Node_Metadata        => Metadata,
      No_Node              => ' ',
      Empty_Metadata       => Default_MD,
      Node_Hash            => Node_Hash,
      Metadata_Hash        => Metadata_Hash,
      Raise_Property_Error => Raise_Property_Error,
      Combine              => Combine,
      Can_Reach            => Can_Reach,
      Is_Rebindable        => Is_Rebindable,
      Node_Text_Image      => Node_Image,
      Register_Rebinding   => Register_Rebinding,
      Ref_Category         => Ref_Category,
      Ref_Categories       => Ref_Categories);


   procedure Put_Line (Elements : Envs.Entity_Array);

end Support;
