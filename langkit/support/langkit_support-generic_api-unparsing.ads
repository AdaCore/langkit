--
--  Copyright (C) 2014-2022, AdaCore
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

private with Ada.Finalization;

with GNATCOLL.Traces;
with Prettier_Ada.Documents;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;

package Langkit_Support.Generic_API.Unparsing is

   type Unparsing_Configuration is private;
   --  Configuration that customizes how source fragments are turned into a
   --  prettier document.

   No_Unparsing_Configuration : constant Unparsing_Configuration;
   --  Special value to mean the absence of an unparsing configuration

   function Default_Unparsing_Configuration
     (Language : Language_Id) return Unparsing_Configuration;
   --  Return the default unparsing configuration for the given language

   function Load_Unparsing_Config
     (Language    : Language_Id;
      Filename    : String;
      Diagnostics : in out Diagnostics_Vectors.Vector)
      return Unparsing_Configuration;
   --  Read and parse the unparsing configuration for the given Language from
   --  Filename. Append error messages to ``Diagnostics`` and return
   --  ``No_Unparsing_Configuration`` if an error occurs while reading the
   --  configuration file.
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
   --    * The "line"/"hardline"/"hardLineWithoutBreakPararent"/"softline"/
   --      "literalline" templates yield the corresponding Prettier documents::
   --
   --        "line"
   --        "hardline"
   --        "hardlineWithoutBreakParent"
   --        "softline"
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
   --      An optional "id" field makes it define a symbol to reference in the
   --      same template:
   --
   --        {"kind": "group", "document": <sub-template>, "id": "mySymbol"}
   --
   --    * The "ifBreak" template yields an "ifBreak" Prettier document::
   --
   --        {"kind": "ifBreak", "breakContents": <sub-template>}
   --        {
   --          "kind": "ifBreak",
   --          "breakContents": <sub-template>,
   --          "flatContents": <sub-template>
   --        }
   --        {
   --          "kind": "ifBreak",
   --          "breakContents": <sub-template>,
   --          "flatContents": <sub-template>,
   --          "groupId": <symbol>
   --        }
   --
   --    * The "ifEmpty" template is valid only inside a fields configuration.
   --      It yields its "then" alternative if the field is an empty list, and
   --      its "else" alternative otherwise::
   --
   --        {
   --          "kind": "ifEmpty",
   --          "then": <sub-template>,
   --          "else": <sub-template>
   --        }
   --
   --    * The "ifKind" template is valid only inside a node configuration.
   --      The "null" entry is optional. If "field" is a null node, "null"
   --      is yielded if defined, otherwise fallsback to "default". If "field"
   --      is not a null node, it yields the match from its "matchers" field
   --      whose key matches with the kind of its "field" key, or "default" if
   --      none matches::
   --
   --        {
   --          "kind": "ifKind",
   --          "field": "<field-name>",
   --          "matchers": [
   --            {"kind": <node-name>, "document": <sub-template>},
   --            ...
   --          ],
   --          "default": <sub-template>
   --          "null": <sub-template>
   --        }
   --
   --      A variant is available in field templates. It has no "field" entry:
   --      the alternative is picked depending on the field that owns this
   --      template.
   --
   --    * The "indent" template yields an "indent" Prettier document::
   --
   --        {"kind": "indent", "contents": <sub-template>}
   --
   --    * The "markAsRoot" template yields a "markAsRoot" Prettier document::
   --
   --        {"kind": "markAsRoot", "contents": <sub-template>}
   --
   --    * The "innerRoot" template yields a "innerRoot" Prettier document::
   --
   --        {"kind": "innerRoot", "contents": <sub-template>}
   --
   --    * The "recurse_field" template is valid only in "node" templates for
   --      concrete nodes that are neither abstract, token nor list nodes. When
   --      used, the whole template cannot contain any
   --      "recurse"/"recurse_flatten" template, and the template, once
   --      linearized, must reflect how the node is unparsed.
   --
   --      For example, let's consider that the ``VarDecl`` node is created
   --      parsing the following chunks::
   --
   --        "var" [f_name] ":" [f_type] ";"
   --
   --      Then its "node" template must contain two "recurse_field" templates
   --      for the two fields, in the same order, and with the same tokens in
   --      between. For instance::
   --
   --        [
   --          {"kind": "text", "text": "var"},
   --          {"kind": "recurse_field", "text": "f_name"},
   --          {
   --            "kind": "group",
   --            "document": [
   --              {"kind": "text", "text": ":"},
   --              {"kind": "recurse_field", "text": "f_type"}
   --            ]
   --          },
   --          {"kind": "text", "text": ";"},
   --        ]
   --
   --    * The "recurse_flatten" template acts like "recurse" but refines its
   --      result so that the document nested in "align", "fill", "group",
   --      "indent" templates and in 1-item document lists is returned
   --      instead (recursively)::
   --
   --        {"kind": "recurse_flatten", "if": ["Node1" ,"Node2", ...]}
   --
   --      The "if" entry is optional. If provided, it must contain a list of
   --      node type names; in this case the flattening is applied only for
   --      templates that were instantiated for nodes that match at least one
   --      of the node types.
   --
   --    * The "text" template yields a "text" Prettier document::
   --
   --        {"kind": "text", "text": "some_text_to_unparse"}
   --
   --      Using this template is valid in specific contexts only:
   --
   --      * For "node" templates, when used with "recurse_field" template: see
   --        the documentation for "recurse_field";
   --
   --      * For "fields" templates: in this case, the linearized template must
   --        reflect how the field is unparsed. See the documentation for
   --        "recurse_field" to have more information about linearization.
   --
   --    * A JSON list yields the corresponding "list" Prettier document::
   --
   --        [{"kind": "whitespace"}, {"kind": "recurse"}]
   --
   --  The configuration file has the following format::
   --
   --    {
   --      "node_configs": {<node-name>: <node-config>},
   --      "max_empty_lines": <natural-number>
   --    }
   --
   --  For each node to configure, the inner "node_configs" mapping associates
   --  the name of the node (as a string key) to another mapping with the
   --  following format::
   --
   --    {
   --      "node": <template>,
   --      "fields": {<field-name>: <template>},
   --      "sep": <template>,
   --      "leading_sep": <template>,
   --      "trailing_sep": <template>
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
   --  The "leading_sep" and "trailing_sep" components are optional, and valid
   --  only for list nodes that accept respectively leading and trailing
   --  separators. If present, they contain document templates to unparse
   --  leading/trailing separators.
   --
   --  Standard node derivation rules apply to configurations: if node B
   --  derives from node A, and if node B does not specify a configuration for
   --  its field F, then the configuration of field F for node A applies (same
   --  for the list separator).
   --
   --  The "max_empty_lines" entry is optional. If provided, it must be a
   --  natural number that indicates the maximum number of consecutive empty
   --  lines to preserve during the source code reformatting. If omitted, all
   --  empty lines are preserved.

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

   Before_Spacing_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LANGKIT.UNPARSING.BEFORE_SPACING",
        Default => GNATCOLL.Traces.From_Config);
   --  Trace to dump the internal document just before required spacing is
   --  inserted.

   Broken_Groups_Trace : GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LANGKIT.UNPARSING.BROKEN_GROUPS",
        Default => GNATCOLL.Traces.From_Config);
   --  Trace to dump the internal document just after the broken groups
   --  detection.

   Final_Doc_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LANGKIT.UNPARSING.FINAL_DOC", Default => GNATCOLL.Traces.From_Config);
   --  Trace to dump the final internal document, just before the conversion to
   --  a Prettier document.

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

   No_Unparsing_Configuration : constant Unparsing_Configuration :=
     (Ada.Finalization.Controlled with Value => null);

end Langkit_Support.Generic_API.Unparsing;
