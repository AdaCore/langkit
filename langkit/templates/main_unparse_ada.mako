## vim: filetype=makoada

with Langkit_Support.Generic_API.Unparsing;
use Langkit_Support.Generic_API.Unparsing;

with ${ada_lib_name}.Generic_API; use ${ada_lib_name}.Generic_API;

procedure Unparse is
begin
   Pretty_Print_Main (Self_Id);
end Unparse;
