## vim: filetype=makoada

--  This package provides the library initialization routines

package ${ada_lib_name}.Init is

   procedure Initialize
     with Export        => True,
          Convention    => C,
          External_Name =>
             "${ada_lib_name.lower()}_initialize";
   ${ada_doc('langkit.initialize', 3)}

end ${ada_lib_name}.Init;
