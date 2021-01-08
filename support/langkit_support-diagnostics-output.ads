------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2020-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--  This package supports outputting ``Langkit_Support.Diagnostics.Diagnostic``
--  for a source buffer, in a pretty colorized format akin to GCC's verbose
--  diagnostic format.
--
--  .. note::
--
--      For the moment, this package has a few limitations, namely:
--
--      - Only source buffers with LF line endings are handled.
--
--      - Only error messages spanning one line are gracefully handled.
--        eventually we want to have something more powerful that has a syntax
--        for error spanning multiple lines.
--
--  .. todo::
--
--      For the moment this is only used in the ``lkt_toolbox`` executable.
--      Eventually, this should be exposed to generated library users in some
--      fashion.

package Langkit_Support.Diagnostics.Output is

   procedure Print_Diagnostic
     (Self   : Diagnostic;
      Buffer : Text_Buffer_Ifc'Class;
      Path   : String);
   --  Pretty-print given diagnostic. Outputs the diagnostic on stdout
   --  in a form that is developper friendly when used in a command line
   --  application.
   --
   --  Error messages format supports backticked parts that should correspond
   --  to references to source entities, and that will be highlighted in the
   --  resulting source.
   --
   --  Here is an example of output::
   --
   --      foo.bar:1:9: Incorrect call to `Foolize`
   --      1 | Foo.Bar.Foolize
   --                  ^^^^^^^
   --
   --  TODO: All the information necessary to print a diagnostic should
   --  eventually be self contained.

end Langkit_Support.Diagnostics.Output;
