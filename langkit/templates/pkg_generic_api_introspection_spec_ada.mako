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

   Kind_To_Type : constant array (${T.node_kind}) of G.Type_Ref :=
   ${ada_block_with_parens(
       [
            f"{t.ada_kind_name} => Type_Refs.{t.entity.api_name}"
            for t in ctx.node_types
            if not t.abstract
       ],
       3
   )};

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
