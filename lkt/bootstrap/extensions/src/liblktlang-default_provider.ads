with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Liblktlang.Common;         use Liblktlang.Common;
with Liblktlang.Implementation; use Liblktlang.Implementation;
with Liblktlang_Support.Text;   use Liblktlang_Support.Text;

--  This package provides the default unit provider for Lkt and LKQL contexts.
--  This provider behaves similarly to CPython's mechanism to find modules:
--  first the current working directory is searched, then the provider uses an
--  environment variable that contains a list of directories in which to find
--  source files.

private package Liblktlang.Default_Provider is

   package US renames Ada.Strings.Unbounded;

   package String_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => US.Unbounded_String,
        "="          => US."=");

   function Create
      (Mode : Language_Mode) return Internal_Unit_Provider_Access;
   --  Create a new default unit provider instance with the provided language
   --  mode.

   function Create_Lkt return Internal_Unit_Provider_Access is (Create (Lkt));
   --  Create a new default unit provider instance for the Lkt language

   function Create_From_Dirs
     (Mode : Language_Mode; Directories : String_Vectors.Vector)
      return Internal_Unit_Provider_Access;
   --  Create a new default unit provider instance from an explicit list of
   --  directories. Note that the current directory is still implicitly looked
   --  at first.
end Liblktlang.Default_Provider;
