--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides a generic API so that programs can unparse trees for
--  any Langkit-generated library, i.e. turn parse trees back to text sources,
--  with custom formatting. Formatting features are based on the Prettier_Ada
--  library.
--
--  Note that it is experimental at this stage, and thus not officially
--  supported.
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
--        Config : constant Unparsing_Configuration :=
--           Load_Unparsing_Config (Self_Id, "config.json");
--
--        --  Parse the source file to reformat
--
--        Ctx : constant Lk_Context :=
--          Create_Context (Libfoolang.Generic_API.Self_Id);
--        U   : constant Lk_Unit :=
--          Ctx.Get_From_File (Ada.Command_Line.Argument (1));
--     begin
--        --  If it has parsing errors, bail out
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

private with Ada.Finalization;

with Prettier_Ada.Documents;

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;

package Langkit_Support.Generic_API.Unparsing is

   type Unparsing_Configuration is private;
   --  Configuration that customizes how source fragments are turned into a
   --  prettier document.

   function Load_Unparsing_Config
     (Language : Language_Id;
      Filename : String) return Unparsing_Configuration;
   --  Read and parse the unparsing configuration for the given Language from
   --  Filename. Raise a Langkit_Support.Errors.Invalid_Input exception if an
   --  error occurs while reading the configuration file.
   --
   --  The configuration is a JSON file that provides "document templates":
   --  patterns to generate Prettier documents:
   --
   --    * The default template to unparse nodes or node fields is the
   --      "recurse" one. Instantiating it just yields the default unparsing
   --      for that node/field::
   --
   --        "recurse"
   --
   --    * The "breakParent" template yields a "breakParent" Prettier
   --      Document::
   --
   --        "breakParent"
   --
   --    * The "line"/"hardline"/"softline"/"literalline" templates yield the
   --      corresponding Prettier documents::
   --
   --        "line"
   --        "hardlist"
   --        "softlist"
   --        "literalline"
   --
   --    * The "trim" template yields a "trim" Prettier Document::
   --
   --        "trim"
   --
   --    * The "whitespace" template yields a "text" document with the
   --      specified amount of spaces::
   --
   --        {"kind": "whitespace", "length": 2}
   --
   --        /* or just, for length = 1 */
   --
   --        "whitespace"
   --
   --    * The "align" template yields a "align" Prettier document::
   --
   --        {
   --          "kind": "align",
   --          "width": <number or string>,
   --          "contents": <sub-template>
   --        }
   --
   --    * The "dedent" template yields a "dedent" Prettier document::
   --
   --        {"kind": "dedent", "document": <sub-template>}
   --
   --    * The "dedentToRoot" template yields a "dedentToRoot" Prettier
   --      document::
   --
   --        {"kind": "dedentToRoot", "document": <sub-template>}
   --
   --    * The "fill" template yields a "fill" Prettier document::
   --
   --        {"kind": "fill", "document": <sub-template>}
   --
   --    * The "group" template yields a "group" Prettier document::
   --
   --        {"kind": "group", "document": <sub-template>}
   --        {"kind": "group", "document": <sub-template>, "shouldBreak": true}
   --
   --    * The "ifBreak" template yields an "ifBreak" Prettier document::
   --
   --        {"kind": "ifBreak", "breakContents": <sub-template>}
   --        {
   --          "kind": "ifBreak",
   --          "breakContents": <sub-template>,
   --          "flatContents": <sub-template>
   --        }
   --
   --    * The "indent" template yields an "indent" Prettier document::
   --
   --        {"kind": "indent", "contents": <sub-template>}
   --
   --    * The "markAsRoot" template yields a "markAsRoot" Prettier document::
   --
   --        {"kind": "markAsRoot", "contents": <sub-template>}
   --
   --    * A JSON list yields the corresponding "list" Prettier document::
   --
   --        [{"kind": "whitespace"}, {"kind": "recurse"}]
   --
   --  The configuration file has the following format::
   --
   --    {"node_configs": {<node-name>: <node-config>}}
   --
   --  For each node to configure, the inner mapping associates the name of the
   --  node (as a string key) to another mapping with the following format::
   --
   --    {
   --      "node": <template>,
   --      "fields": {<field-name>: <template>},
   --      "sep": <template>
   --    }
   --
   --  The "node" component is optional. If present, it contains a document
   --  template to wrap the basic unparsing of the node.
   --
   --  The "fields" component is optional. If present, it contains a mapping
   --  from field names to document templates.
   --
   --  The "sep" component is also optional, valid for list nodes only. If
   --  present, it contains a document template to unparse the list separator.
   --
   --  Standard node derivation rules apply to configurations: if node B
   --  derives from node A, and if node B does not specify a configuration for
   --  its field F, then the configuration of field F for node A applies (same
   --  for the list separator).

   function Unparse_To_Prettier
     (Node   : Lk_Node;
      Config : Unparsing_Configuration)
      return Prettier_Ada.Documents.Document_Type;
   --  Unparse ``Node`` into a prettier document according to the given
   --  configuration.

   procedure Pretty_Print_Main (Language : Language_Id);
   --  Implementation of the "*_unparse" test program for the given Language.
   --
   --  Load the configuration file, the source to pretty-print and formatting
   --  options from the command line, then dump the pretty-printed source on
   --  the standard output.

private

   --  Unparsing_Configuration is a shared pointer to the actual configuration
   --  data record.
   --
   --  Note that we want Unparsing_Configuration_Access to be a Taft amendment
   --  type so that there is no need to rebuild all units that depend on this
   --  when the configuration data structures change. For this reason, we
   --  cannot use GNATCOLL.Refcount to implement the shared pointer.

   type Unparsing_Configuration_Record;
   type Unparsing_Configuration_Access is
     access all Unparsing_Configuration_Record;

   type Unparsing_Configuration is new Ada.Finalization.Controlled with record
      Value : Unparsing_Configuration_Access;
   end record;

   overriding procedure Adjust (Self : in out Unparsing_Configuration);
   overriding procedure Finalize (Self : in out Unparsing_Configuration);

end Langkit_Support.Generic_API.Unparsing;
