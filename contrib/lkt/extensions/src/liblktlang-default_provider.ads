with Langkit_Support.Text; use Langkit_Support.Text;

with Liblktlang.Implementation; use Liblktlang.Implementation;

--  This package provides the default unit provider for Lkt contexts. This
--  provider behaves similarly to CPython's mechanism to find modules: use an
--  environment variable (LKT_PATH) that contains a list of directories in
--  which to find Lkt source files.

private package Liblktlang.Default_Provider is

   Path_Var_Name : constant String := "LKT_PATH";
   --  Name of the environment variable that contains the path to Lkt source
   --  files.

   function Unit_Base_Filename (Name : Text_Type) return String;
   --  Return the base filename corresponding to this unit name, or an empty
   --  string if Name is not a valid unit name.

   function Create return Internal_Unit_Provider_Access;
   --  Create a new default unit provider instance

end Liblktlang.Default_Provider;
