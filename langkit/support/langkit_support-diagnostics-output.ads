--
--  Copyright (C) 2020-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package supports outputting
--  :ada:ref:`Langkit_Support.Diagnostics.Diagnostic` for a source buffer, in a
--  pretty colorized format akin to GCC's verbose diagnostic format.
--
--  .. note:: For the moment, this package has a few limitations, namely:
--
--      - Only source buffers with ``LF`` line endings are handled.
--
--      - Only error messages spanning one line are gracefully handled.
--        eventually we want to have something more powerful that has a syntax
--        for error spanning multiple lines.
--
--  .. todo:
--
--      For the moment this is only used in the ``lkt_toolbox`` executable.
--      Eventually, this should be exposed to generated library users in some
--      fashion.

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Terminal; use GNATCOLL.Terminal;

package Langkit_Support.Diagnostics.Output is

   type Diagnostic_Style is record
      Label : Unbounded_Text_Type;
      --  Label for the diagnostic

      Color : ANSI_Color;
      --  Color for the diagnostic
   end record;
   --  Style for a diagnostic

   Default_Diagnostic_Style : constant Diagnostic_Style :=
     (To_Unbounded_Text ("error"), Red);
   --  Default style

   procedure Print_Diagnostic
     (Self        : Diagnostic;
      Buffer      : Text_Buffer_Ifc'Class;
      Path        : String;
      Style       : Diagnostic_Style := Default_Diagnostic_Style;
      Output_File : File_Type := Standard_Output);
   --  Pretty-print given diagnostic. Outputs the diagnostic on ``stdout`` in a
   --  form that is developer friendly when used in a command line
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
   --  ``Output_File`` is set by default to ``Standard_Output``, you can use
   --  ``Standard_Error`` alternatively.
   --
   --  You can also customize the style, which allows you to change the label
   --  of the diagnostic (by default it's "error"), and the color of the
   --  message (red by default).
   --
   --  TODO: All the information necessary to print a diagnostic should
   --  eventually be self contained.

end Langkit_Support.Diagnostics.Output;
