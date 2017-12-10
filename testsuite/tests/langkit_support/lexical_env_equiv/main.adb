--  Test that the Equivalence function for lexical envs works properly

with Ada.Containers; use Ada.Containers;
with Ada.Text_IO;    use Ada.Text_IO;

with System;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Text; use Langkit_Support.Text;

procedure Main is

   type Metadata is record
      I : Integer;
   end record;

   Default_MD : constant Metadata := (I => 0);

   function Element_Hash (C : Character) return Hash_Type is (0);
   function Metadata_Hash (MD : Metadata) return Hash_Type is (0);

   procedure Raise_Property_Error (Message : String := "") is
   begin
      raise Program_Error;
   end Raise_Property_Error;

   function Combine (L, R : Metadata) return Metadata is ((I => L.I + R.I));
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
   use Envs;

   Root_A1 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Node => 'A', Ref_Count => No_Refcount, others => <>));
   Root_A2 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Root_A1.Env.all'Update (Ref_Count => 1)));
   Root_A3 : Lexical_Env := Wrap (new Lexical_Env_Type'(Root_A2.Env.all));

   Root_A2_Getter : constant Env_Getter := Simple_Env_Getter (Root_A2);
   Root_A3_Getter : constant Env_Getter := Simple_Env_Getter (Root_A3);

   Root_B1 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Root_A2.Env.all'Update (Node => 'B')));

   Child : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Root_A2.Env.all'Update (Parent => Root_A2_Getter)));

   function To_Refs
     (Ref : Referenced_Env) return Referenced_Envs_Vectors.Vector
   is
      V : Referenced_Envs_Vectors.Vector;
   begin
      V.Append (Ref);
      return V;
   end To_Refs;

   Ref_1 : constant Referenced_Env :=
     (Is_Transitive => False, Getter => Root_A2_Getter,
      Being_Visited => False, State => Active);
   Ref_2 : constant Referenced_Env :=
     (Is_Transitive => False, Getter => Root_A3_Getter,
      Being_Visited => False, State => Inactive);

   Env_Ref_1 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Root_A2.Env.all'Update (Referenced_Envs => To_Refs (Ref_1))));
   Env_Ref_2 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Root_A2.Env.all'Update (Referenced_Envs => To_Refs (Ref_2))));
   Env_Ref_3 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Root_A2.Env.all'Update (Referenced_Envs => To_Refs (Ref_1))));

   MD : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Root_A2.Env.all'Update (Default_MD => (I => 1))));
begin
   --  Check for various combinations of equivalent environments with
   --  enabled/disable ref-counting.
   pragma Assert (Equivalent (Root_A1, Root_A1));
   pragma Assert (Equivalent (Root_A2, Root_A2));
   pragma Assert (Equivalent (Root_A1, Root_A2));
   pragma Assert (Equivalent (Root_A2, Root_A3));

   --  Check different Parent
   pragma Assert (not Equivalent (Root_A2, Child));

   --  Check different Node
   pragma Assert (not Equivalent (Root_A3, Root_B1));

   --  Check Referenced_Envs
   pragma Assert (not Equivalent (Root_A2, Env_Ref_1));
   pragma Assert (Equivalent (Env_Ref_1, Env_Ref_3));
   pragma Assert (not Equivalent (Env_Ref_1, Env_Ref_2));

   --  Check different Default_MD
   pragma Assert (not Equivalent (Root_A2, MD));
end Main;
