## vim: filetype=makoada

--  Internal package used to provide conversion routines for analysis units.
--  This is a hack we should remove once we untangle the circular dependency
--  between the Analysis package and the AST one.

package ${_self.ada_api_settings.lib_name}.Analysis.Internal is

   function Convert (Unit : Analysis_Unit_Interface) return Analysis_Unit;
   --  There is only one implementation of Analysis_Unit_Interface, so this
   --  conversion is safe.

private

   function Convert (Unit : Analysis_Unit_Interface) return Analysis_Unit is
     (Analysis_Unit_Type (Unit.all)'Unrestricted_Access);

end ${_self.ada_api_settings.lib_name}.Analysis.Internal;
