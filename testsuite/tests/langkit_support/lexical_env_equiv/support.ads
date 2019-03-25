with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;

with System;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Symbols;
with Langkit_Support.Text;  use Langkit_Support.Text;
with Langkit_Support.Types; use Langkit_Support.Types;

package Support is

   type Metadata is record
      I : Integer;
   end record;

   Default_MD : constant Metadata := (I => 0);

   function Node_Hash (Dummy_C : Character) return Hash_Type is (0);
   function Metadata_Hash (Dummy_MD : Metadata) return Hash_Type is (0);
   procedure Raise_Property_Error (Message : String := "");
   function Combine (L, R : Metadata) return Metadata is ((I => L.I + R.I));
   function Parent (Dummy_Node : Character) return Character is (' ');
   function Can_Reach (Dummy_Node, Dummy_From : Character) return Boolean
   is (True);
   function Is_Rebindable (Dummy_Node : Character) return Boolean is (True);

   function Node_Image
     (Node : Character; Dummy_Short : Boolean := True) return Text_Type
   is (To_Text ("'" & Node & "'"));

   procedure Register_Rebinding (Node : Character; Rebinding : System.Address)
   is null;

   function Get_Version (Dummy : Boolean) return Version_Number is (0);

   type Ref_Category is (No_Cat);
   type Ref_Categories is array (Ref_Category) of Boolean;

   type Precomputed_Symbol_Index is new Integer range 1 .. 0;
   function Precomputed_Symbol
     (Dummy : Precomputed_Symbol_Index) return Text_Type
   is (raise Program_Error);

   package Symbols is new Langkit_Support.Symbols
     (Precomputed_Symbol_Index, Precomputed_Symbol);

   package Envs is new Langkit_Support.Lexical_Env
     (Precomputed_Symbol_Index => Precomputed_Symbol_Index,
      Precomputed_Symbol       => Precomputed_Symbol,
      Symbols                  => Symbols,
      Unit_T                   => Boolean,
      Get_Version              => Get_Version,
      No_Unit                  => False,
      Node_Type                => Character,
      Node_Metadata            => Metadata,
      No_Node                  => ' ',
      Empty_Metadata           => Default_MD,
      Node_Hash                => Node_Hash,
      Metadata_Hash            => Metadata_Hash,
      Raise_Property_Error     => Raise_Property_Error,
      Combine                  => Combine,
      Can_Reach                => Can_Reach,
      Is_Rebindable            => Is_Rebindable,
      Node_Text_Image          => Node_Image,
      Register_Rebinding       => Register_Rebinding,
      Ref_Category             => Ref_Category,
      Ref_Categories           => Ref_Categories);

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Envs.Env_Rebindings_Type, Envs.Env_Rebindings);

end Support;
