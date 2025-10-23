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

private with Ada.Finalization;

with GNATCOLL.Traces;
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
     (Language : Language_Id) return Unparsing_Configuration;
   --  Return the default unparsing configuration for the given language

   function Load_Unparsing_Config
     (Language        : Language_Id;
      Filename        : String;
      Diagnostics     : in out Diagnostics_Vectors.Vector;
      Check_All_Nodes : Boolean := False)
      return Unparsing_Configuration;
   --  Read and parse the unparsing configuration for the given Language from
   --  Filename. Append error messages to ``Diagnostics`` and return
   --  ``No_Unparsing_Configuration`` if an error occurs while reading the
   --  configuration file.
   --
   --  If ``Check_All_Nodes`` is true, ensure that the configuration covers all
   --  possible parse nodes (creating an error if this is not the case).
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
   --    * The "line"/"hardline"/"hardLineWithoutBreakParent"/"softline"/
   --      "literalline" templates yield the corresponding Prettier documents::
   --
   --        "line"
   --        "hardline"
   --        "hardlineWithoutBreakParent"
   --        "softline"
   --        "literalline"
   --
   --    * The "flushLineBreaks" template is used as a placeholder to emit
   --      potential line breaks that come from the source code to reformat::
   --
   --        "flushLineBreaks"
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
   --      The "absent" entry is optional. If "field" is not present, "absent"
   --      is yielded if defined, otherwise fallback to "default".
   --
   --      If "field" is present, the entry in "matchers" that corresponds to
   --      the field's kind is looked up ("kind" is either the name of a node,
   --      or a list of node names): the "document" template for the first
   --      entry that matches is used. The "default" template is used if there
   --      is no match::
   --
   --        {
   --          "kind": "ifKind",
   --          "field": "<field-name>",
   --          "matchers": [
   --            {"kind": <node-name>, "document": <sub-template>},
   --            ...
   --          ],
   --          "default": <sub-template>
   --          "absent": <sub-template>
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
   --      It also accepts the optional "bubbleUpLeadingTrivias" and
   --      "bubbleUpTrailingTrivias" boolean entries to override the default
   --      behavior for trivias bubbling up.
   --
   --    * The "markAsRoot" template yields a "markAsRoot" Prettier document::
   --
   --        {"kind": "markAsRoot", "contents": <sub-template>}
   --
   --    * The "innerRoot" template yields a "innerRoot" Prettier document::
   --
   --        {"kind": "innerRoot", "contents": <sub-template>}
   --
   --    * The "continuationLineIndent" template yields a
   --      "continuationLineIndent" Prettier document::
   --
   --        {"kind": "continuationLineIndent", "contents": <sub-template>}
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
   --    * The "tableSeparator" template yields the corresponding Prettier
   --      document::
   --
   --        {"kind": "tableSeparator", "text": "some_text_to_unparse"}
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
   --  The following commands also accept the optional "bubbleUpLeadingTrivias"
   --  and "bubbleUpTrailingTrivias" boolean entries to override the default
   --  behavior for trivias bubbling up.
   --
   --  * align,
   --  * continuationLineIndent,
   --  * dedent,
   --  * dedentToRoot,
   --  * fill,
   --  * group,
   --  * indent,
   --  * innerRoot,
   --  * markAsRoot.
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
   --      "trailing_sep": <template>,
   --      "flush_before_children": <boolean>
   --      "independent_lines": <boolean>
   --    }
   --
   --  The "node" component is optional. If present, it contains a document
   --  template to wrap the basic unparsing of the node.
   --
   --  The "fields" component is optional. If present, it contains a mapping
   --  from field names to document templates.
   --
   --  The "sep" component is optional, valid for list nodes only. If present,
   --  it contains a document template to unparse the list separator.
   --
   --  The "table" component is optional, valid for list nodes only. If
   --  present, unparsing such lists yield a list of table documents, each list
   --  child being unparsing to a list row. If present, it must contain an
   --  object that accepts the following entries:
   --
   --  * "sep_before": Whether list separators must be inserted at the end of
   --    the previous row (``"sep_before": true``) or at the beginning of the
   --    next row (``"sep_before": false``). This is optional, and defaults to
   --    ``true``.
   --
   --  * "split": Determine which kind of trivia found between two list
   --    children trigger a table split (i.e. the presence of such trivias end
   --    the current table, and trigger the creation of a new table for the
   --    next children). This field is optional (by default: nothing splits
   --    table), and when present, must be an array of strings, with the
   --    following possible values: ``"empty_line"``, ``"line_comment"``.
   --
   --  * "must_break": Whether each row for this table must go on its own line.
   --    If false (the default), rows go on each line only when a break occurs
   --    in the table.
   --
   --  * "join": Determine whether a list child must be put on the previous
   --    table row, i.e. whether to join what would instead be two rows.
   --
   --    If present, this must be an object with a mandatory "predicate" entry,
   --    that must be a reference to a predicate property  i.e. a property that
   --    each list child has and that returns whether to join rows, as a
   --    boolean.
   --
   --    The optional "template" entry must be a template that describes how to
   --    join two rows: the first row is substituted to the "recurse_left"
   --    template and the second row is substituted to the "recurse_right"
   --    template. For example::
   --
   --      "join": {
   --        "predicate": "p_my_predicate",
   --        "sep": [
   --          "recurse_left",
   --          {"kind": "tableSeparator", "text": ""},
   --          {"kind": "group", "document": ["line", "recurse_right"]}
   --        ]
   --      }
   --
   --  The "leading_sep" and "trailing_sep" components are optional, and valid
   --  only for list nodes that accept respectively leading and trailing
   --  separators. If present, they contain document templates to unparse
   --  leading/trailing separators.
   --
   --  The "flush_before_children" component is optional, and valid for list
   --  nodes only. It must be a boolean (true by default), that controls
   --  whether line breaks recovered from the source to reformat are flushed
   --  before each list element.
   --
   --  The "independent_lines" component is optional, and valid for list nodes
   --  only. It must be a boolean (false by default), that controls whether
   --  each list item is formatted on its own line. When true, the formatting
   --  of rewritten trees can stop reformating at the boundary of such nodes.
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
