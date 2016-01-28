## vim: filetype=makoada

with GNATCOLL.Iconv;

package body ${_self.ada_api_settings.lib_name}.Init is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Nowadays, on the platforms we support, everything has already been
      --  automatically initialized. Still, this is a convenient place to check
      --  that we actually have full Libiconv support: as nothing works without
      --  it, we explicitely check support here instead of letting
      --  user-unfriendly errors happen during lexing.

      if not GNATCOLL.Iconv.Has_Iconv then
         raise Program_Error with "Libiconv is not available";
      end if;
   end Initialize;

end ${_self.ada_api_settings.lib_name}.Init;
