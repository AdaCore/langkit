## vim: filetype=makoada

--  This package provides contants to refer to ${ada_lib_name} types and struct
--  members in the generic introspection API
--  (``Langkit_Support.Generic_API.Introspection``).

with Langkit_Support.Generic_API.Introspection;

package ${ada_lib_name}.Generic_API.Introspection is

   package G renames Langkit_Support.Generic_API.Introspection;

   ---------------------
   -- Type references --
   ---------------------

   package Type_Refs is
      % for i, t in enumerate(generic_api.all_types, 1):
         ${t.api_name} : constant G.Type_Ref :=
           G.From_Index (Self_Id, ${i});
      % endfor
   end Type_Refs;

   -----------------------
   -- Member references --
   -----------------------

   package Member_Refs is
      % for i, m in enumerate(generic_api.all_members, 1):
         ${generic_api.member_name(m)} : constant G.Struct_Member_Ref :=
           G.From_Index (Self_Id, ${i});
      % endfor
   end Member_Refs;

end ${ada_lib_name}.Generic_API.Introspection;
