## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

--  To facilitate use from a -gnatX project, since we don't use the [] syntax
pragma Warnings (Off, "obsolescent");

with GNATCOLL.GMP.Integers;

with Langkit_Support.Errors;
private with Langkit_Support.Internal.Analysis;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;
with Langkit_Support.Types;   use Langkit_Support.Types;


--  This package provides types and functions used in the whole ${ada_lib_name}
--  package tree.

package ${ada_lib_name}.Common is

   use Support.Slocs, Support.Text;

   subtype Big_Integer is GNATCOLL.GMP.Integers.Big_Integer;
   --  Shortcut for ``GNATCOLL.GMP.Integers.Big_Integer``

   <%
      lexer = ctx.lexer
      tokens = lexer.sorted_tokens
   %>

   Default_Charset : constant String :=
     ${ascii_repr(cfg.library.defaults.charset)};
   --  Default charset to use when creating analysis contexts

   ----------------
   -- Exceptions --
   ----------------

   ## Emit declarations for all exceptions
   % for section_name, exceptions in ctx.exceptions_by_section:
   % if section_name:
   ${comment_box(section_name, 3)}

   % endif
   % for e in exceptions:
   ${e.name} : exception renames ${e.qualname};
   ${ada_doc(e.doc_entity, 3)}

   % endfor
   % endfor

   ----------------------------
   -- Misc enumeration types --
   ----------------------------

   % for enum_type in ctx.enum_types:
      ${ada_enum_type_decl(
         enum_type.api_name,
         [v.name for v in enum_type.values],
         6,
         convention_c=True,
      )}
      ${ada_doc(enum_type, 6)}

      % if ctx.properties_logging:
      function Trace_Image (Self : ${enum_type.api_name}) return String
      is (Self'Image);
      % endif

   % endfor

   -----------
   -- Nodes --
   -----------

   ## Output enumerators so that all concrete AST_Node subclasses get their own
   ## kind. Nothing can be an instance of an abstract subclass, so these do not
   ## need their own kind.
   ${ada_enum_type_decl(
      T.node_kind,
      [t.ada_kind_name for t in ctx.node_types if not t.abstract],
      3,
   )}
   --  Type for concrete nodes

   for ${T.node_kind} use
   ${ada_block_with_parens(
       [
           f"{cls.ada_kind_name} => {ctx.node_kind_constants[cls]}"
           for cls in ctx.node_types
           if not cls.abstract
       ],
       3
   )};

   ## Output subranges to materialize abstract classes as sets of their
   ## concrete subclasses.
   % for cls in ctx.node_types:
      subtype ${cls.ada_kind_range_name} is ${T.node_kind}
         % if cls.concrete_subclasses:
            range ${'{} .. {}'.format(*cls.ada_kind_range_bounds)};
         % else:
            with Static_Predicate => False;
      --  This abstract node has no concrete derivations
         % endif
      --% no-document: True
   % endfor

   ## Output a subtype to materialize the set of kinds for synthetic nodes
   subtype Synthetic_Nodes is ${T.node_kind}
      with Static_Predicate =>
      % if ctx.synthetic_nodes:
         Synthetic_Nodes in
         ${ada_pipe_list([t.ada_kind_name for t in ctx.synthetic_nodes], 9)}
      % else:
         False
      % endif
   ;
   ${ada_doc('langkit.synthetic_nodes', 6)}

   Default_Grammar_Rule : constant Grammar_Rule := ${ctx.main_rule_api_name};
   --  Default grammar rule to use when parsing analysis units

   ------------------
   -- Lexer inputs --
   ------------------

   type Lexer_Input_Kind is
     (File,
      --  Readable source file

      Bytes_Buffer,
      --  Buffer of undecoded bytes

      Text_Buffer
      --  Buffer of decoded bytes
   );
   --  Kind of lexer input

   subtype Undecoded_Lexer_Input is
      Lexer_Input_Kind range File ..  Bytes_Buffer;

   ------------
   -- Tokens --
   ------------

   ${ada_enum_type_decl(
      "Token_Kind",
      [t.ada_name for t in tokens],
      3,
   )}
   --  Kind of token: indentifier, string literal, ...

   ${ada_enum_type_decl(
      "Token_Family",
      [tf.ada_name for tf in lexer.tokens.token_families],
      3,
   )}
   --  Groups of token kinds, to make the processing of some groups of token
   --  uniform.

   % if lexer.track_indent:
   type Indent_Kind is (Indent, Dedent, Nodent, None);
   --  Change of indentation
   % endif

   Token_Kind_To_Family : array (Token_Kind) of Token_Family :=
   ${ada_block_with_parens(
       [
           f"{t.ada_name} => {lexer.tokens.token_to_family[t].ada_name}"
           for t in tokens
       ],
       3
   )};
   --  Associate a token family to all token kinds
   --
   --% document-value: False

   function Token_Kind_Name (Token_Id : Token_Kind) return String;
   ${ada_doc('langkit.token_kind_name', 3)}

   function Token_Kind_Literal (Token_Id : Token_Kind) return Text_Type;
   --  Return the canonical literal corresponding to this token kind, or an
   --  empty string if this token has no literal.

   function Token_Error_Image (Token_Id : Token_Kind) return String;
   --  Return a string representation of ``Token_Id`` that is suitable in error
   --  messages.

   function To_Token_Kind (Raw : Raw_Token_Kind) return Token_Kind
      with Inline;
   function From_Token_Kind (Kind : Token_Kind) return Raw_Token_Kind
      with Inline;

   function Is_Token_Node (Kind : ${T.node_kind}) return Boolean;
   --  Return whether Kind corresponds to a token node

   function Is_List_Node (Kind : ${T.node_kind}) return Boolean;
   --  Return whether Kind corresponds to a list node

   function Is_Error_Node (Kind : ${T.node_kind}) return Boolean;
   --  Return whether Kind corresponds to an error node

   type Visit_Status is (Into, Over, Stop);
   --  Helper type to control the node traversal process. See the
   --  ``${ada_lib_name}.Analysis.Traverse`` function.

   -----------------------
   -- Lexical utilities --
   -----------------------

   type Token_Reference is private;
   ${ada_doc('langkit.token_reference_type', 3)}

   No_Token : constant Token_Reference;

   type Token_Data_Type is private;

   function "<" (Left, Right : Token_Reference) return Boolean;
   --  Assuming ``Left`` and ``Right`` belong to the same analysis unit, return
   --  whether ``Left`` came before ``Right`` in the source file.

   function Next
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference;
   ${ada_doc('langkit.token_next', 3)}

   function Previous
     (Token          : Token_Reference;
      Exclude_Trivia : Boolean := False) return Token_Reference;
   ${ada_doc('langkit.token_previous', 3)}

   function Data (Token : Token_Reference) return Token_Data_Type;
   --  Return the data associated to ``Token``

   function Is_Equivalent (L, R : Token_Reference) return Boolean;
   ${ada_doc('langkit.token_is_equivalent', 3)}

   function Image (Token : Token_Reference) return String;
   --  Debug helper: return a human-readable text to represent a token

   function Text (Token : Token_Reference) return Text_Type;
   --  Return the text of the token as ``Text_Type``

   function Text (First, Last : Token_Reference) return Text_Type;
   ${ada_doc('langkit.token_range_text', 3)}

   function Get_Symbol (Token : Token_Reference) return Symbol_Type;
   --  Assuming that ``Token`` refers to a token that contains a symbol, return
   --  the corresponding symbol.

   function Kind (Token_Data : Token_Data_Type) return Token_Kind;
   ${ada_doc('langkit.token_kind', 3)}

   function Is_Trivia (Token : Token_Reference) return Boolean;
   ${ada_doc('langkit.token_is_trivia', 3)}

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean;
   ${ada_doc('langkit.token_is_trivia', 3)}

   function Index (Token : Token_Reference) return Token_Index;
   ${ada_doc('langkit.token_index', 3)}

   function Index (Token_Data : Token_Data_Type) return Token_Index;
   ${ada_doc('langkit.token_index', 3)}

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range;
   --  Source location range for this token. Note that the end bound is
   --  exclusive.

   function Origin_Filename (Token : Token_Reference) return String;
   --  Return the name of the file whose content was scanned to create Token.
   --  Return an empty string if the source comes from a memory buffer instead
   --  of a file.

   function Origin_Charset (Token : Token_Reference) return String;
   --  Return the charset used to decode the source that was scanned to create
   --  Token. Return an empty string if the source was already decoded during
   --  the scan.

   function Convert
     (TDH      : Token_Data_Handler;
      Token    : Token_Reference;
      Raw_Data : Stored_Token_Data) return Token_Data_Type;
   --  Turn data from ``TDH`` and ``Raw_Data`` into a user-ready token data
   --  record.

   type Child_Or_Trivia is (Child, Trivia);
   --  Discriminator for the ``Child_Record`` type

   function Raw_Data (T : Token_Reference) return Stored_Token_Data;
   --  Return the raw token data for ``T``

   function Token_Node_Kind (Kind : ${T.node_kind}) return Token_Kind
      with Pre => Is_Token_Node (Kind);
   --  Return the token kind corresponding to the given token node kind
   % if not ctx.generate_unparsers:
   --
   --  As unparser are not generated, this always raises a ``Program_Error``
   --  exception.
   % endif

   ${exts.include_extension(ctx.ext('common', 'public_decls'))}

private

   type Token_Safety_Net is record
      Context         : Langkit_Support.Internal.Analysis.Internal_Context;
      Context_Version : Version_Number;
      --  Analysis context and version number at the time this safety net was
      --  produced.
      --
      --  TODO: it is not possible to refer to
      --  $.Implementation.Internal_Context from this spec (otherwise we get a
      --  circular dependency). For now, use the generic pointer from
      --  Langkit_Support (hack), but in the future the Token_Reference type
      --  (and this this safety net type) will go to the generic API, so we
      --  will get rid of this hack.

      TDH_Version : Version_Number;
      --  Version of the token data handler at the time this safety net was
      --  produced.
   end record;
   --  Information to embed in public APIs with token references, used to check
   --  before using the references that they are not stale.

   No_Token_Safety_Net : constant Token_Safety_Net :=
     (Langkit_Support.Internal.Analysis.No_Internal_Context, 0, 0);

   type Token_Reference is record
      TDH : Token_Data_Handler_Access;
      --  Token data handler that owns this token

      Index : Token_Or_Trivia_Index;
      --  Identifier for the trivia or the token this refers to

      Safety_Net : Token_Safety_Net;
   end record;

   procedure Check_Safety_Net (Self : Token_Reference);
   --  If ``Self`` is a stale token reference, raise a
   --  ``Stale_Reference_Error`` error.

   No_Token : constant Token_Reference :=
     (null, No_Token_Or_Trivia_Index, No_Token_Safety_Net);

   type Token_Data_Type is record
      Kind : Token_Kind;
      --  See documentation for the Kind accessor

      Is_Trivia : Boolean;
      --  See documentation for the Is_Trivia accessor

      Index : Token_Index;
      --  See documentation for the Index accessor

      Source_Buffer : Text_Cst_Access;
      --  Text for the original source file

      Source_First : Positive;
      Source_Last  : Natural;
      --  Bounds in Source_Buffer for the text corresponding to this token

      Sloc_Range : Source_Location_Range;
      --  See documenation for the Sloc_Range accessor
   end record;

end ${ada_lib_name}.Common;
