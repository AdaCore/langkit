
with Liblktlang_Support;
with Liblktlang_Support.Diagnostics;
with Liblktlang_Support.Slocs;
with Liblktlang_Support.Text;

package Liblktlang is

   

   Version    : constant String := "undefined";
   Build_Date : constant String := "undefined";

   --  Liblktlang's main entry point is the Liblktlang.Analysis
   --  package.

   --  Convenience renaming for support package that Langkit provides

   package Support renames Liblktlang_Support;
   package Diagnostics renames Support.Diagnostics;
   package Slocs renames Support.Slocs;
   package Text renames Support.Text;

end;
