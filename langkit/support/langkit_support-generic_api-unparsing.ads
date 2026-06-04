--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: At this stage, this package is considered as internal.
--
--  This package provides a generic API so that programs can unparse trees for
--  any Langkit-generated library, i.e. turn parse trees back to text sources,
--  with custom formatting. Formatting features are based on the Prettier_Ada
--  library.
--
--  Here is a simplistic pretty-printing program using this API:
--
--  .. code-block:: ada
--
--     with Ada.Command_Line;
--     with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
--     with Ada.Text_IO;              use Ada.Text_IO;
--     with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
--
--     with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
--     with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
--     with Langkit_Support.Generic_API.Analysis;
--     use Langkit_Support.Generic_API.Analysis;
--     with Langkit_Support.Generic_API.Unparsing;
--     use Langkit_Support.Generic_API.Unparsing;
--
--     with Libfoolang.Generic_API; use Libfoolang.Generic_API;
--
--     with Prettier_Ada.Documents;
--     use Prettier_Ada.Documents;
--
--     procedure Unparse is
--
--        --  Load the unparsing configuration file, that determines how to
--        --  format source code for the "foo" language: see the
--        --  Load_Unparsing_Config function for more information about this
--        --  file.
--
--        Diagnostics : Diagnostics_Vectors.Vector;
--        Config      : constant Unparsing_Configuration :=
--           Load_Unparsing_Config (Self_Id, "config.json", Diagnostics);
--
--        --  Parse the source file to reformat
--
--        Ctx : constant Lk_Context :=
--          Create_Context (Libfoolang.Generic_API.Self_Id);
--        U   : constant Lk_Unit :=
--          Ctx.Get_From_File (Ada.Command_Line.Argument (1));
--     begin
--        --  If we were unable to load the unparsing configuration, bail out
--
--        if Config = No_Unparsing_Configuration then
--           Put_Line ("Error when loading the unparsing configuration:");
--           Print (Diagnostics);
--           raise Program_Error;
--        end if;
--
--        --  If the source file to reformat has parsing errors, bail out
--
--        if U.Has_Diagnostics then
--           Put_Line ("Parsing errors:");
--           for D of U.Diagnostics loop
--              Put_Line (U.Format_GNU_Diagnostic (D));
--           end loop;
--           raise Program_Error;
--        end if;
--
--        --  Otherwise, print the reformatted source to the standard output
--
--        declare
--           Doc       : constant Document_Type :=
--             Unparse_To_Prettier (U.Root, Config);
--           Formatted : constant Unbounded_String :=
--             Format (Doc, Default_Format_Options);
--        begin
--           Put_Line (Formatted);
--        end;
--     end Unparse;
--
--  How to write unparsing configuration files is documented in
--  :ref:`unparsing_configuration_file_format`.

private with Ada.Finalization;

with GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;
with Prettier_Ada.Documents;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
private with Langkit_Support.Unparsing_Config;

package Langkit_Support.Generic_API.Unparsing is

   type Unparsing_Configuration is private;
   --  Configuration that customizes how source fragments are turned into a
   --  prettier document.

   No_Unparsing_Configuration : constant Unparsing_Configuration;
   --  Special value to mean the absence of an unparsing configuration

   function Default_Unparsing_Configuration
     (Language    : Language_Id) return Unparsing_Configuration;
   --  Return the default unparsing configuration for the given language

   function Load_Unparsing_Config
     (Language        : Language_Id;
      Filename        : String;
      Diagnostics     : in out Diagnostics_Vectors.Vector;
      Check_All_Nodes : Boolean := False;
      Overridings     : File_Array := Empty_File_Array)
      return Unparsing_Configuration;
   --  Read and parse the unparsing configuration for the given ``Language``
   --  from ``Filename``, with overriding loaded from the files at
   --  ``Overridings``. Append error messages to ``Diagnostics`` and return
   --  ``No_Unparsing_Configuration`` if an error occurs while reading the
   --  configurations.
   --
   --  If ``Check_All_Nodes`` is true, ensure that the configuration covers all
   --  possible parse nodes (creating an error if this is not the case).
   --
   --  ``Overridings`` must be a (possibly empty) list of filenames that
   --  contain node configurations to complete/replace node configurations
   --  found in the original unparsing configuration. It is also formatted in
   --  JSON: it must be an object with a single "node_configs" mapping,
   --  following the same format as in the main configuration file. If multiple
   --  overriding files are provided, they are all applied in the same order.

   function Unparse_To_Prettier
     (Node          : Lk_Node;
      Config        : Unparsing_Configuration;
      Process_Error : access procedure
                        (Node : Lk_Node; Message : String) := null)
      return Prettier_Ada.Documents.Document_Type;
   --  Unparse ``Node`` into a prettier document according to the given
   --  configuration.
   --
   --  ``Process_Error`` is called for each non-critical error (i.e. errors
   --  that do not prevent the unparsing). For each error, ``Node`` is the node
   --  that was processed when the error occured, and ``Message`` gives a short
   --  description of the error. If ``Process_Error`` is null, errors are
   --  printed on the standard output.

   function Default_Format_Options
     (Language : Language_Id)
      return Prettier_Ada.Documents.Format_Options_Type;
   --  Return the default formating options to use for this language

   procedure Pretty_Print_Main (Language : Language_Id);
   --  Implementation of the ``*_unparse`` test program for the given Language.
   --
   --  Load the configuration file, the source to pretty-print and formatting
   --  options from the command line, then dump the pretty-printed source on
   --  the standard output.

   ----------------------
   -- Unparsing traces --
   ----------------------

   Current_Token_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LANGKIT.UNPARSING.CURRENT_TOKEN",
        Default => GNATCOLL.Traces.From_Config);
   --  Trace to print the evolution of the "current token" information

   Trivias_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LANGKIT.UNPARSING.TRIVIAS",
        Default => GNATCOLL.Traces.From_Config);
   --  Trace to log information about the handling of trivias

   Expansion_Errors_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LANGKIT.UNPARSING.EXPANSION_ERRORS",
        Default => GNATCOLL.Traces.From_Config);
   --  Trace to log errors happening during expansion from parse tree and
   --  templates.

   Expanded_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LANGKIT.UNPARSING.EXPANDED",
        Default => GNATCOLL.Traces.From_Config);
   --  Trace to dump the internal document right after expansion from parse
   --  tree and templates.

   Bubble_Up_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LANGKIT.UNPARSING.BUBBLE_UP",
        Default => GNATCOLL.Traces.From_Config);
   --  Trace to dump the internal document right after the pass that bubbles
   --  up trivias.

   Broken_Groups_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LANGKIT.UNPARSING.BROKEN_GROUPS",
        Default => GNATCOLL.Traces.From_Config);
   --  Trace to dump the internal document just after the broken groups
   --  detection.

   Final_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.UNPARSING.FINAL", Default => GNATCOLL.Traces.From_Config);
   --  Trace to dump the final internal document, just before the conversion to
   --  a Prettier document.

private

   use Langkit_Support.Unparsing_Config;

   --  Unparsing_Configuration is a shared pointer to the actual configuration
   --  data record.

   type Unparsing_Configuration is new Ada.Finalization.Controlled with record
      Value : Unparsing_Configuration_Access;
   end record;

   overriding procedure Adjust (Self : in out Unparsing_Configuration);
   overriding procedure Finalize (Self : in out Unparsing_Configuration);

   No_Unparsing_Configuration : constant Unparsing_Configuration :=
     (Ada.Finalization.Controlled with Value => null);

end Langkit_Support.Generic_API.Unparsing;
