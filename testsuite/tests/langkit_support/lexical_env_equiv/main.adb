--  Test that the Equivalence function for lexical envs works properly

with Langkit_Support.Lexical_Envs; use Langkit_Support.Lexical_Envs;

with Support; use Support;

procedure Main is
   use Envs;

   Old_Env_1 : Lexical_Env := Wrap (new Lexical_Env_Record'
     (Kind => Static_Primary, Node => 'O', others => <>));
   New_Env_1 : Lexical_Env := Wrap (new Lexical_Env_Record'
     (Kind => Static_Primary, Node => 'N', others => <>));
   Old_Env_2 : Lexical_Env := Wrap (new Lexical_Env_Record'
     (Kind => Static_Primary, Node => 'o', others => <>));
   New_Env_2 : Lexical_Env := Wrap (new Lexical_Env_Record'
     (Kind => Static_Primary, Node => 'n', others => <>));

   Prim_A : Lexical_Env := Wrap (new Lexical_Env_Record'
     (Kind => Static_Primary, Node => 'A', others => <>));
   Prim_B : Lexical_Env := Wrap (new Lexical_Env_Record'
     (Kind => Static_Primary, Node => 'B', others => <>));

   Orphaned_A1 : Lexical_Env := Orphan (Prim_A);
   Orphaned_A2 : Lexical_Env := Orphan (Prim_A);
   Orphaned_B  : Lexical_Env := Orphan (Prim_B);

   Grouped_1 : Lexical_Env := Group ((Orphaned_A1, Orphaned_B));
   Grouped_2 : Lexical_Env := Group ((Orphaned_A1, Orphaned_B));
   Grouped_3 : Lexical_Env := Group ((Orphaned_B, Orphaned_A1));
   Grouped_4 : Lexical_Env := Group ((Orphaned_A1, Orphaned_B, Orphaned_A1));
   Grouped_5 : Lexical_Env := Group ((Orphaned_A1, Orphaned_B), (I => 1));

   R1 : Env_Rebindings := Append (null, Old_Env_1, New_Env_1);
   R2 : Env_Rebindings := Append (null, Old_Env_2, New_Env_2);
   pragma Assert (R1 /= R2);

   Rebound_A1X : Lexical_Env := Rebind_Env (Prim_A, R1);
   Rebound_A1Y : Lexical_Env := Rebind_Env (Prim_A, R1);
   Rebound_A2  : Lexical_Env := Rebind_Env (Prim_A, R2);
   Rebound_B   : Lexical_Env := Rebind_Env (Prim_B, R1);

begin
   --  Two primary environments are considered different unless it's actually
   --  the same.
   pragma Assert (not Equivalent (Prim_A, Prim_B));
   pragma Assert (Equivalent (Prim_A, Prim_A));

   --  Two environments that have different kinds can never be equivalent
   pragma Assert (not Equivalent (Prim_A, Orphaned_A1));

   --  Two orphaned environments are equivalent iff they have the same
   --  referenced environment.
   pragma Assert (Orphaned_A1 /= Orphaned_A2);
   pragma Assert (Orphaned_A1 /= Orphaned_B);
   pragma Assert (Equivalent (Orphaned_A1, Orphaned_A2));
   pragma Assert (not Equivalent (Orphaned_A1, Orphaned_B));

   --  Two grouped environments are equivalent iff:
   --    * they contain the same number of environment;
   --    * these environments are equivalent two by two;
   --    * the attached metadata is the same.
   --  Additionally the Group primitive performs flattening and only keeps one
   --  of each env.
   pragma Assert (Grouped_1 /= Grouped_2);
   pragma Assert (Grouped_1 /= Grouped_3);
   pragma Assert (Grouped_1 /= Grouped_4);
   pragma Assert (Grouped_1 /= Grouped_5);
   pragma Assert (Equivalent (Grouped_1, Grouped_2));
   pragma Assert (not Equivalent (Grouped_1, Grouped_3));
   pragma Assert (Equivalent (Grouped_1, Grouped_4));
   pragma Assert (not Equivalent (Grouped_1, Grouped_5));

   --  Two rebound environments are equivalent iff:
   --    * they point to equivalent environments;
   --    * they contain equivalent rebindings.
   pragma Assert (Rebound_A1X /= Rebound_A1Y);
   pragma Assert (Equivalent (Rebound_A1X, Rebound_A1Y));
   pragma Assert (not Equivalent (Rebound_A1X, Rebound_A2));
   pragma Assert (not Equivalent (Rebound_A1X, Rebound_B));

   Dec_Ref (Orphaned_A1);
   Dec_Ref (Orphaned_A2);
   Dec_Ref (Orphaned_B);

   Dec_Ref (Grouped_1);
   Dec_Ref (Grouped_2);
   Dec_Ref (Grouped_3);
   Dec_Ref (Grouped_4);
   Dec_Ref (Grouped_5);

   Dec_Ref (Rebound_A1X);
   Dec_Ref (Rebound_A1Y);
   Dec_Ref (Rebound_A2);
   Dec_Ref (Rebound_B);

   Destroy (Old_Env_1);
   Destroy (New_Env_1);
   Destroy (Old_Env_2);
   Destroy (New_Env_2);

   Destroy (Prim_A);
   Destroy (Prim_B);

   Destroy (R1);
   Destroy (R2);
end Main;
