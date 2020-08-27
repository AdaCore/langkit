with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;

with System;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Types; use Langkit_Support.Types;

package Support is

   type Metadata is null record;
   Default_MD : constant Metadata := (null record);

   function Node_Hash (Dummy_C : Character) return Hash_Type is (0);
   function Node_Unit (Dummy_C : Character) return Boolean is (True);
   function Metadata_Hash (Dummy_MD : Metadata) return Hash_Type is (0);
   function Combine (Dummy_L, Dummy_R : Metadata) return Metadata
   is ((null record));
   function Parent (Dummy_Node : Character) return Character is (' ');
   function Can_Reach (Dummy_Node, Dummy_From : Character) return Boolean
   is (True);
   function Is_Rebindable (Dummy_Node : Character) return Boolean is (True);

   function Node_Image
     (Node : Character; Dummy_Short : Boolean := True) return Text_Type
   is (To_Text ("'" & Node & "'"));

   procedure Register_Rebinding
     (Dummy_Node : Character; Dummy_Rebinding : System.Address) is null;

   function Get_Unit_Version (Dummy : Boolean) return Version_Number is (0);
   function Get_Context_Version (Dummy : Boolean) return Integer is (0);

   type Ref_Category is (No_Cat);
   type Ref_Categories is array (Ref_Category) of Boolean;

   package Envs is new Langkit_Support.Lexical_Env
     (Unit_T                   => Boolean,
      Get_Unit_Version         => Get_Unit_Version,
      Get_Context_Version      => Get_Context_Version,
      No_Unit                  => False,
      Node_Type                => Character,
      Node_Metadata            => Metadata,
      No_Node                  => ' ',
      Empty_Metadata           => Default_MD,
      Node_Hash                => Node_Hash,
      Metadata_Hash            => Metadata_Hash,
      Combine                  => Combine,
      Can_Reach                => Can_Reach,
      Is_Rebindable            => Is_Rebindable,
      Node_Text_Image          => Node_Image,
      Register_Rebinding       => Register_Rebinding,
      Ref_Category             => Ref_Category,
      Ref_Categories           => Ref_Categories);

   procedure Put_Line (Elements : Envs.Entity_Array);

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Envs.Env_Rebindings_Type, Envs.Env_Rebindings);

end Support;
