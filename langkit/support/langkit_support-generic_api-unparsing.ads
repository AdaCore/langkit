--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides a generic API so that programs can unparse trees for
--  any Langkit-generated library, i.e. turn parse trees back to a stream of
--  tokens, without formatting. Unparsing features are typically useful for
--  programs that modify source code, such as pretty printers or instrumenters.
--
--  Note that it is experimental at this stage, and thus not officially
--  supported.
--
--  Here is a simplistic unparsing program using this API:
--
--  .. code-block:: ada
--
--     with Ada.Command_Line;
--     with Ada.Text_IO;
--
--     with Langkit_Support.Generic_API; use Langkit_Support.Generic_API;
--     with Langkit_Support.Generic_API.Analysis;
--     use Langkit_Support.Generic_API.Analysis;
--     with Langkit_Support.Generic_API.Introspection;
--     use Langkit_Support.Generic_API.Introspection;
--     with Langkit_Support.Generic_API.Unparsing;
--     use Langkit_Support.Generic_API.Unparsing;
--     with Langkit_Support.Names; use Langkit_Support.Names;
--     with Langkit_Support.Text;  use Langkit_Support.Text;
--
--     with Libfoolang.Generic_API; use Libfoolang.Generic_API;
--
--     procedure Unparse is
--        Last_Token : Token_Kind_Ref := No_Token_Kind_Ref;
--        --  Kind of the last token that was written to the standard output
--
--        procedure Process_Node (N : Lk_Node);
--        --  Unparse ``N`` to fragments and write them to the standard output,
--        --  recursively unparsing fragments for ``N``'s child nodes.
--
--        ------------------
--        -- Process_Node --
--        ------------------
--
--        procedure Process_Node (N : Lk_Node) is
--
--           --  Get the unparsing fragments corresponding to ``N``
--
--           Fragments : Unparsing_Fragment_Vectors.Vector;
--        begin
--           Unparse_To_Fragments (N, Fragments);
--
--           --  Then write each fragment to the standard output (recursively
--           --  for node children). Each time we output a token, possibly
--           --  append a newline/whitespace according to the spacing
--           --  requirements.
--
--           for F of Fragments loop
--              case F.Kind is
--                 when Token_Fragment =>
--                    case Required_Spacing (Last_Token, F.Token_Kind) is
--                       when None =>
--                          null;
--                       when Newline =>
--                          New_Line;
--                       when Whitespace =>
--                          Put (" ");
--                    end case;
--                    Last_Token := F.Token_Kind;
--                    Put (To_UTF8 (To_Text (F.Token_Text)));
--
--                 when Field_Fragment | List_Child_Fragment =>
--                    Process_Node (F.Node);
--              end case;
--           end loop;
--        end Process_Node;
--
--        --  Parse the source file to get the tree to unparse
--
--        Ctx : constant Lk_Context :=
--          Create_Context (Libfoolang.Generic_API.Self_Id);
--        U   : constant Lk_Unit :=
--          Ctx.Get_From_File (Ada.Command_Line.Argument (1));
--     begin
--
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
--        --  Otherwise, unparse it to the standard output
--
--        Process_Node (U.Root);
--     end Unparse;

with Ada.Containers.Vectors;
with Ada.Finalization;

with Prettier_Ada.Documents;

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;

package Langkit_Support.Generic_API.Unparsing is

   ---------------
   -- Fragments --
   ---------------

   type Spacing_Kind is (None, Whitespace, Newline);
   --  Spacing required between two tokens:
   --
   --  * ``Node``: no spacing required, the two tokens can be unparsed next to
   --    each other in the source buffer (spacing is permitted, but not
   --    necessary).
   --
   --  * ``Whitespace``: at least one whitespace (line break, space, horizontal
   --    tabulation: whatever the language accepts as a whitespace) is required
   --    after the first token.
   --
   --  * ``Newline``: a line break is required right after the first token.
   --    Extra spacing is permitted after that line break.

   function Required_Spacing
     (Left, Right : Token_Kind_Ref) return Spacing_Kind;
   --  Return the spacing that is required when unparsing a token of kind
   --  ``Right`` just after a token of kind ``Left`` to a source buffer.
   --
   --  For convenience, ``Required_Spacing`` is allowed to be
   --  ``No_Token_Kind_Ref``: the result is always ``None`` in this case. The
   --  intended use case for this is when processing the first token to unparse
   --  to a source buffer: ``Left`` is ``No_Token_Kind_Ref`` (no token were
   --  unprase in the source buffer yet) and ``Right`` is the first token to
   --  unparse to the source buffer.

   type Unparsing_Fragment_Kind is
     (Token_Fragment,
      List_Separator_Fragment,
      Field_Fragment,
      List_Child_Fragment);
   type Unparsing_Fragment
     (Kind : Unparsing_Fragment_Kind := Unparsing_Fragment_Kind'First)
   is record
      case Kind is
         when Token_Fragment | List_Separator_Fragment =>
            Token_Kind : Token_Kind_Ref;
            --  Token kind corresponding to this token fragment. This component
            --  is used to determine the spacing required in the source buffer
            --  between two consecutive tokens.

            Token_Text : Unbounded_Text_Type;
            --  Text to emit when unparsing this token fragment

         when Field_Fragment | List_Child_Fragment =>
            Node : Lk_Node;
            --  Node for this fragment

            case Kind is
               when Token_Fragment | List_Separator_Fragment =>
                  null;

               when Field_Fragment =>
                  Field : Struct_Member_Ref;
                  --  Syntax field for this fragment (the parent node is a
                  --  regular node).

               when List_Child_Fragment =>
                  Child_Index : Positive;
                  --  Index of this field for this fragment (the parent node is
                  --  a list node).

            end case;
      end case;
   end record;
   --  Source code unparsing fragment used to unparse source code. Fragments
   --  are either tokens or nodes (that must be decomposed themselves into
   --  fragments).

   package Unparsing_Fragment_Vectors is new Ada.Containers.Vectors
     (Positive, Unparsing_Fragment);

   procedure Unparse_To_Fragments
     (Node : Lk_Node; Fragments : out Unparsing_Fragment_Vectors.Vector);
   --  Decompose ``Node`` into a list of unparsing fragments

   --------------
   -- Prettier --
   --------------

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
   --        "list"
   --        "hardlist"
   --        "softlist"
   --        "literalline"
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
