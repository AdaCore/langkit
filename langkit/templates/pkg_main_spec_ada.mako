## vim: filetype=makoada

with Langkit_Support;
with Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;
with Langkit_Support.Text;

package ${ada_lib_name} is

   Version    : constant String := ${ascii_repr(ctx.actual_version)};
   Build_Date : constant String := ${ascii_repr(ctx.actual_build_date)};

   --  ${ada_lib_name}'s main entry point is the ${ada_lib_name}.Analysis
   --  package.

   --  Convenience renaming for support package that Langkit provides

   package Support renames Langkit_Support;
   package Diagnostics renames Support.Diagnostics;
   package Slocs renames Support.Slocs;
   package Text renames Support.Text;

end;
