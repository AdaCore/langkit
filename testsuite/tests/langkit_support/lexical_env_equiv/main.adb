--  Test that the Equivalence function for lexical envs works properly

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Text; use Langkit_Support.Text;

with Support; use Support;

procedure Main is
   use Envs;

   Root_A1 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Kind => Primary, Node => 'A', Ref_Count => No_Refcount, others => <>));
   Root_A2 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Kind => Rebound,
      Parent => Root_A1.Env.Parent,
      Transitive_Parent => Root_A1.Env.Transitive_Parent,
      Node => Root_A1.Env.Node,
      Referenced_Envs => Root_A1.Env.Referenced_Envs,
      Map => Root_A1.Env.Map,
      Default_MD => Root_A1.Env.Default_MD,
      Rebindings => Root_A1.Env.Rebindings,
      Rebindings_Pool => Root_A1.Env.Rebindings_Pool,
      Lookup_Cache => Root_A1.Env.Lookup_Cache,
      Lookup_Cache_Active => Root_A1.Env.Lookup_Cache_Active,
      Lookup_Cache_Valid => Root_A1.Env.Lookup_Cache_Valid,
      Ref_Count => 1));

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
