with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Liblktlang_Support.Text; use Liblktlang_Support.Text;

with Liblktlang.Implementation; use Liblktlang.Implementation;

--  This package provides the default unit provider for Lkt contexts. This
--  provider behaves similarly to CPython's mechanism to find modules: use an
--  environment variable (LKT_PATH) that contains a list of directories in
--  which to find Lkt source files.

private package Liblktlang.Default_Provider is

   package US renames Ada.Strings.Unbounded;

   Path_Var_Name : constant String := "LKT_PATH";
   --  Name of the environment variable that contains the path to Lkt source
   --  files.

   function Unit_Base_Filename (Name : Text_Type) return String;
   --  Return the base filename corresponding to this unit name, or an empty
   --  string if Name is not a valid unit name.

   function Create return Internal_Unit_Provider_Access;
   --  Create a new default unit provider instance from the ``LKT_PATH``
   --  environment variable.

   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => US.Unbounded_String,
      "="          => US."=");

   function Create_From_Dirs
     (Directories : String_Vectors.Vector)
      return Internal_Unit_Provider_Access;
   --  Create a new default unit provider instance from an explicit list of
   --  directories. Note that the current directory is still implicitly looked
   --  at first.

end Liblktlang.Default_Provider;
