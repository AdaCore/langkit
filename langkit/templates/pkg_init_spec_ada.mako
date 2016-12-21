## vim: filetype=makoada

--  This package provides the library initialization routines

package ${_self.ada_api_settings.lib_name}.Init is

   procedure Initialize
     with Export        => True,
          Convention    => C,
          External_Name =>
             "${ctx.ada_api_settings.lib_name.lower()}_initialize";
   ${ada_doc('langkit.initialize', 3)}

end ${_self.ada_api_settings.lib_name}.Init;
