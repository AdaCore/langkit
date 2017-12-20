--  Test that the Equivalence function for lexical envs works properly

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Lexical_Env;
with Langkit_Support.Text; use Langkit_Support.Text;

with Support; use Support;

procedure Main is
   use Envs;

   Old_Env_1 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Kind => Primary, others => <>));
   New_Env_1 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Kind => Primary, others => <>));
   Old_Env_2 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Kind => Primary, others => <>));
   New_Env_2 : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Kind => Primary, others => <>));

   Prim_A : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Kind => Primary, others => <>));
   Prim_B : Lexical_Env := Wrap (new Lexical_Env_Type'
     (Kind => Primary, others => <>));

   Orphaned_A1 : constant Lexical_Env := Orphan (Prim_A);
   Orphaned_A2 : constant Lexical_Env := Orphan (Prim_A);
   Orphaned_B  : constant Lexical_Env := Orphan (Prim_B);

   function Group is new Envs.Group (Positive, Lexical_Env_Array);
   Grouped_1 : constant Lexical_Env := Group ((Orphaned_A1, Orphaned_B));
   Grouped_2 : constant Lexical_Env := Group ((Orphaned_A1, Orphaned_B));
   Grouped_3 : constant Lexical_Env := Group ((Orphaned_B, Orphaned_A1));
   Grouped_4 : constant Lexical_Env :=
      Group ((Orphaned_A1, Orphaned_B, Orphaned_A1));
   Grouped_5 : constant Lexical_Env :=
      Group ((Orphaned_A1, Orphaned_B), (I => 1));

   R1 : constant Env_Rebindings := Append (null, Old_Env_1, New_Env_1);
   R2 : constant Env_Rebindings := Append (null, Old_Env_2, New_Env_2);
   pragma Assert (R1 /= R2);

   Rebound_A1X : constant Lexical_Env := Rebind_Env (Prim_A, R1);
   Rebound_A1Y : constant Lexical_Env := Rebind_Env (Prim_A, R1);
   Rebound_A2  : constant Lexical_Env := Rebind_Env (Prim_A, R2);
   Rebound_B   : constant Lexical_Env := Rebind_Env (Prim_B, R1);

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
   pragma Assert (Grouped_1 /= Grouped_2);
   pragma Assert (Grouped_1 /= Grouped_3);
   pragma Assert (Grouped_1 /= Grouped_4);
   pragma Assert (Grouped_1 /= Grouped_5);
   pragma Assert (Equivalent (Grouped_1, Grouped_2));
   pragma Assert (not Equivalent (Grouped_1, Grouped_3));
   pragma Assert (not Equivalent (Grouped_1, Grouped_4));
   pragma Assert (not Equivalent (Grouped_1, Grouped_5));

   --  Two rebound environments are equivalent iff:
   --    * they point to equivalent environments;
   --    * they contain equivalent rebindings.
   pragma Assert (Rebound_A1X /= Rebound_A1Y);
   pragma Assert (Equivalent (Rebound_A1X, Rebound_A1Y));
   pragma Assert (not Equivalent (Rebound_A1X, Rebound_A2));
   pragma Assert (not Equivalent (Rebound_A1X, Rebound_B));
end Main;
