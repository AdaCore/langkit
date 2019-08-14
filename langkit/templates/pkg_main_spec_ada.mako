## vim: filetype=makoada

with Langkit_Support;
with Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;
with Langkit_Support.Text;

package ${ada_lib_name} is

   ## It is up to each Langkit user to update these constants to whatever
   ## appropriate.
   Version      : constant String := "dev";
   Current_Year : constant String := "1";

   --  ${ada_lib_name}'s main entry point is the ${ada_lib_name}.Analysis
   --  package.

   --  Convenience renaming for support package that Langkit provides

   package Support renames Langkit_Support;
   package Diagnostics renames Support.Diagnostics;
   package Slocs renames Support.Slocs;
   package Text renames Support.Text;

end;
