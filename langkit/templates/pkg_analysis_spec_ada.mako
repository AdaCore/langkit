## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="entities"      file="entities_ada.mako" />
<%namespace name="enum_types"    file="enum_types_ada.mako" />
<%namespace name="exts"          file="extensions.mako" />
<%namespace name="list_types"    file="list_types_ada.mako" />

<% no_builtins = lambda ts: filter(lambda t: not t.is_builtin(), ts) %>

with Ada.Unchecked_Deallocation;

with System;

with GNATCOLL.Traces;

with Langkit_Support.Bump_Ptr;    use Langkit_Support.Bump_Ptr;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Slocs;       use Langkit_Support.Slocs;
with Langkit_Support.Symbols;     use Langkit_Support.Symbols;
with Langkit_Support.Text;        use Langkit_Support.Text;

limited private with ${ada_lib_name}.Analysis.Implementation;

with ${ada_lib_name}.Lexer; use ${ada_lib_name}.Lexer;
use ${ada_lib_name}.Lexer.Token_Data_Handlers;

${exts.with_clauses(with_clauses)}

--  This package provides types and primitives to analyze source files as
--  analysis units.
--
--  This is the entry point to parse and process a unit: first create an
--  analysis context with Create, then get analysis units out of it using
--  Get_From_File and/or Get_From_Buffer.

package ${ada_lib_name}.Analysis is

   package Traces renames GNATCOLL.Traces;

   Main_Trace : constant Traces.Trace_Handle :=
     Traces.Create ("Main_Trace", Traces.From_Config);

   type Analysis_Context is private;
   ${ada_doc('langkit.analysis_context_type', 3)}

   type Analysis_Unit is private;
   ${ada_doc('langkit.analysis_unit_type', 3)}

   No_Analysis_Unit : constant Analysis_Unit;
   --  Special value to mean the absence of analysis unit. No analysis units
   --  can be passed this value.

   No_Analysis_Context : constant Analysis_Context;
   --  Special value to mean the absence of analysis unit. No analysis units
   --  can be passed this value.

   type Grammar_Rule is (
      % for i, name in enumerate(ctx.user_rule_names):
         % if i > 0:
            ,
         % endif
         ${Name.from_lower(name)}_Rule
      % endfor
   );
   ${ada_doc('langkit.grammar_rule_type', 3)}

   Property_Error : exception;
   ${ada_doc('langkit.property_error', 3)}

   type Token_Type is private;
   ${ada_doc('langkit.token_type', 3)}

   No_Token : constant Token_Type;

   Default_Charset : constant String := ${string_repr(ctx.default_charset)};

   ---------------
   -- AST nodes --
   ---------------

   % for e in ctx.entity_types:
      % if e.is_root_type:
         type ${e.api_name} is tagged private;
      % else:
         type ${e.api_name} is new ${e.base.api_name} with private;
      % endif
   % endfor

   % for e in ctx.entity_types:
      No_${e.api_name} : constant ${e.api_name};
   % endfor

   % if not ctx.separate_properties:
      ${entities.decls1()}
   % endif

   --------------------
   -- Unit providers --
   --------------------

   type Unit_Kind is (Unit_Specification, Unit_Body);
   ${ada_doc('langkit.unit_kind_type', 3)}

   Invalid_Unit_Name_Error : exception;
   ${ada_doc('langkit.invalid_unit_name_error', 3)}

   type Unit_Provider_Interface is limited interface;
   type Unit_Provider_Access is
      access Unit_Provider_Interface'Class;
   type Unit_Provider_Access_Cst is
      access constant Unit_Provider_Interface'Class;
   ${ada_doc('langkit.unit_provider_type', 3)}

   function Get_Unit_Filename
     (Provider : Unit_Provider_Interface;
      Name     : Text_Type;
      Kind     : Unit_Kind) return String is abstract;
   ${ada_doc('langkit.unit_provider_get_unit_filename', 3)}

   function Get_Unit
     (Provider    : Unit_Provider_Interface;
      Context     : Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit is abstract;
   ${ada_doc('langkit.unit_provider_get_unit_from_name', 3)}

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Unit_Provider_Interface'Class, Unit_Provider_Access);

   ---------------------------------
   -- Analysis context primitives --
   ---------------------------------

   function Create
     (Charset     : String := Default_Charset;
      With_Trivia : Boolean := False
      % if ctx.default_unit_provider:
         ; Unit_Provider : Unit_Provider_Access_Cst := null
      % endif
     ) return Analysis_Context;
   ${ada_doc('langkit.create_context', 3)}

   procedure Inc_Ref (Context : Analysis_Context);
   ${ada_doc('langkit.context_incref', 3)}

   procedure Dec_Ref (Context : in out Analysis_Context);
   ${ada_doc('langkit.context_decref', 3)}

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context; Discard : Boolean);
   ${ada_doc('langkit.context_discard_errors_in_populate_lexical_env', 3)}

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context; Timeout : Natural);
   ${ada_doc('langkit.context_set_logic_resolution_timeout', 3)}

   function Get_From_File
     (Context  : Analysis_Context;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule :=
         ${Name.from_lower(ctx.main_rule_name)}_Rule)
      return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_file', 3)}

   function Get_From_Buffer
     (Context     : Analysis_Context;
      Filename    : String;
      Charset     : String := "";
      Buffer      : String;
      Rule        : Grammar_Rule :=
         ${Name.from_lower(ctx.main_rule_name)}_Rule)
      return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_buffer', 3)}

   function Has_Unit
     (Context       : Analysis_Context;
      Unit_Filename : String) return Boolean;
   --  Returns whether Context contains a unit correponding to Unit_Filename

   % if ctx.default_unit_provider:

   function Get_From_Provider
     (Context     : Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False)
      return Analysis_Unit;
   ${ada_doc('langkit.get_unit_from_provider', 3)}

   function Get_With_Error
     (Context     : Analysis_Context;
      Filename    : String;
      Error       : String;
      Charset     : String := "";
      Rule        : Grammar_Rule :=
         ${Name.from_lower(ctx.main_rule_name)}_Rule)
      return Analysis_Unit;
   --  If a Unit for Filename already exists, return it unchanged. Otherwise,
   --  create an empty analysis unit for Filename with a diagnostic that
   --  contains the Error message.

   function Unit_Provider
     (Context : Analysis_Context) return Unit_Provider_Access_Cst;
   --  Object to translate unit names to file names
   % endif

   procedure Remove
     (Context : Analysis_Context; File_Name : String);
   ${ada_doc('langkit.remove_unit', 3)}

   procedure Destroy (Context : in out Analysis_Context);
   ${ada_doc('langkit.destroy_context', 3)}

   procedure Inc_Ref (Unit : Analysis_Unit);
   ${ada_doc('langkit.unit_incref', 3)}

   procedure Dec_Ref (Unit : Analysis_Unit);
   ${ada_doc('langkit.unit_decref', 3)}

   function Get_Context (Unit : Analysis_Unit) return Analysis_Context;
   ${ada_doc('langkit.unit_context', 3)}

   procedure Reparse (Unit : Analysis_Unit; Charset : String := "");
   ${ada_doc('langkit.unit_reparse_file', 3)}

   procedure Reparse
     (Unit    : Analysis_Unit;
      Charset : String := "";
      Buffer  : String);
   ${ada_doc('langkit.unit_reparse_buffer', 3)}

   procedure Populate_Lexical_Env (Unit : Analysis_Unit);
   ${ada_doc('langkit.unit_populate_lexical_env', 3)}

   function Get_Filename (Unit : Analysis_Unit) return String;
   ${ada_doc('langkit.unit_filename', 3)}

   function Get_Charset (Unit : Analysis_Unit) return String;
   --  Return the charset that was used to parse Unit

   function Has_Diagnostics (Unit : Analysis_Unit) return Boolean;
   ${ada_doc('langkit.unit_has_diagnostics', 3)}

   function Diagnostics (Unit : Analysis_Unit) return Diagnostics_Array;
   ${ada_doc('langkit.unit_diagnostics', 3)}

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit; D : Diagnostic) return String;
   --  Format a diagnostic in a GNU fashion. See
   --  <https://www.gnu.org/prep/standards/html_node/Errors.html>.

   pragma Warnings (Off, "defined after private extension");
   function Root (Unit : Analysis_Unit) return ${root_entity.api_name};
   ${ada_doc('langkit.unit_root', 3)}
   pragma Warnings (On, "defined after private extension");

   function First_Token (Unit : Analysis_Unit) return Token_Type;
   ${ada_doc('langkit.unit_first_token', 3)}

   function Last_Token (Unit : Analysis_Unit) return Token_Type;
   ${ada_doc('langkit.unit_last_token', 3)}

   function Token_Count (Unit : Analysis_Unit) return Natural;
   ${ada_doc('langkit.unit_token_count', 3)}

   function Trivia_Count (Unit : Analysis_Unit) return Natural;
   ${ada_doc('langkit.unit_trivia_count', 3)}

   procedure Dump_Lexical_Env (Unit : Analysis_Unit);
   --  Debug helper: output the lexical envs for given analysis unit

   procedure Trigger_Envs_Debug (Is_Active : Boolean);
   --  Activate debug traces for lexical envs lookups

   procedure Print
     (Unit       : Analysis_Unit;
      Show_Slocs : Boolean := True);
   --  Debug helper: output the AST and eventual diagnostic for this unit on
   --  standard output.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.

   procedure PP_Trivia (Unit : Analysis_Unit);
   --  Debug helper: output a minimal AST with mixed trivias

   function Version (Unit : Analysis_Unit) return Natural;
   --  Return the version for Unit. Version is a number that is incremented
   --  every time Unit changes.

   -----------------------------
   -- Miscellanous operations --
   -----------------------------

   ## Output enumerators so that all concrete AST_Node subclasses get their own
   ## kind. Nothing can be an instance of an abstract subclass, so these do not
   ## need their own kind.
   type ${root_node_kind_name} is
     (${', '.join(cls.ada_kind_name
                  for cls in ctx.astnode_types
                  if not cls.abstract)});
   --  AST node concrete types

   for ${root_node_kind_name} use
     (${', '.join('{} => {}'.format(cls.ada_kind_name,
                                    ctx.node_kind_constants[cls])
                  for cls in ctx.astnode_types
                  if not cls.abstract)});

   ## Output subranges to materialize abstract classes as sets of their
   ## concrete subclasses.
   % for cls in ctx.astnode_types:
      <% subclasses = cls.concrete_subclasses %>
      % if subclasses:
         subtype ${cls.ada_kind_range_name} is
            ${root_node_kind_name} range
               ${subclasses[0].ada_kind_name}
               .. ${subclasses[-1].ada_kind_name};
      % endif
   % endfor

   -----------------------
   -- Lexical utilities --
   -----------------------

   type Token_Data_Type is private;

   function "<" (Left, Right : Token_Type) return Boolean;
   --  Assuming Left and Right belong to the same analysis unit, return whether
   --  Left came before Right in the source file.

   function Next (Token : Token_Type) return Token_Type;
   ${ada_doc('langkit.token_next', 3)}

   function Previous (Token : Token_Type) return Token_Type;
   ${ada_doc('langkit.token_previous', 3)}

   function Data (Token : Token_Type) return Token_Data_Type;
   --  Return the data associated to T

   function Is_Equivalent (L, R : Token_Type) return Boolean;
   ${ada_doc('langkit.token_is_equivalent', 3)}

   function Image (Token : Token_Type) return String;
   --  Debug helper: return a human-readable text to represent a token

   function Text (Token : Token_Type) return Text_Type;
   --  Return the text of the token as Text_Type

   function Text (Token : Token_Type) return String;
   --  Return the text of the token as String

   function Text (First, Last : Token_Type) return Text_Type;
   ${ada_doc('langkit.token_range_text', 3)}

   function Get_Symbol (Token : Token_Type) return Symbol_Type;
   --  Assuming that Token refers to a token that contains a symbol, return the
   --  corresponding symbol.

   function Kind (Token_Data : Token_Data_Type) return Token_Kind;
   ${ada_doc('langkit.token_kind', 3)}

   function Is_Trivia (Token_Data : Token_Data_Type) return Boolean;
   ${ada_doc('langkit.token_is_trivia', 3)}

   function Index (Token : Token_Type) return Token_Index;
   function Index (Token_Data : Token_Data_Type) return Token_Index;
   ${ada_doc('langkit.token_index', 3)}

   function Sloc_Range
     (Token_Data : Token_Data_Type) return Source_Location_Range;
   --  Source location range for this token. Note that the end bound is
   --  exclusive.

   % if not ctx.separate_properties:
      ${entities.decls2()}
   % endif

   --------------------
   -- Token Iterator --
   --------------------

   type Token_Iterator is private
      with Iterable => (First       => First_Token,
                        Next        => Next_Token,
                        Has_Element => Has_Element,
                        Element     => Element);
   --  This type allows iteration on a range of tokens

   function First_Token (Self : Token_Iterator) return Token_Type;
   function Next_Token
     (Self : Token_Iterator; Tok : Token_Type) return Token_Type;
   function Has_Element
     (Self : Token_Iterator; Tok : Token_Type) return Boolean;
   function Element (Self : Token_Iterator; Tok : Token_Type) return Token_Type;

   -----------------------
   -- Enumeration types --
   -----------------------

   function Image (Value : Boolean) return String;

   % for cls in ctx.sorted_types(ctx.enum_types):
   ${enum_types.public_decl(cls)}
   % endfor

   -----------------
   -- Array types --
   -----------------

   % for array_type in ctx.sorted_types(ctx.array_types):
      % if array_type._exposed:
         ${array_types.public_api_decl(array_type)}
      % endif
   % endfor

   % if not ctx.separate_properties:
      ${entities.decls3()}
   % endif

private

   type Analysis_Context is access all Implementation.Analysis_Context_Type;
   type Analysis_Unit is access all Implementation.Analysis_Unit_Type;

   No_Analysis_Unit    : constant Analysis_Unit := null;
   No_Analysis_Context : constant Analysis_Context := null;

   --------------------------
   -- AST nodes (internal) --
   --------------------------

   <% md_fields = T.env_md.get_fields() %>

   type Public_Metadata is
      % if md_fields:
         record
            % for f in md_fields:
               % if f.type.is_bool_type:
                  ${f.name} : Boolean := False;
               % elif f.type.is_ast_node:
                  ${f.name} : System.Address := System.Null_Address;
               % else:
                  <% assert False %>
               % endif
            % endfor
         end record
            with Convention => C;
      % else:
         null record
            with Convention => C;
      % endif;

   No_Public_Metadata : constant Public_Metadata :=
      % if md_fields:
         (others => <>);
      % else:
         (null record);
      % endif

   type Public_Entity_Info is record
      MD         : Public_Metadata;
      Rebindings : System.Address;
   end record;

   No_Public_Entity_Info : constant Public_Entity_Info :=
     (No_Public_Metadata, System.Null_Address);

   % for e in ctx.entity_types:
      % if e.is_root_type:
         type ${e.api_name} is tagged record
            Node   : access Implementation.${root_node_value_type}'Class;
            E_Info : Public_Entity_Info;
         end record;
      % else:
         type ${e.api_name} is new ${e.base.api_name} with null record;
      % endif
      No_${e.api_name} : constant ${e.api_name} :=
        (null, No_Public_Entity_Info);
   % endfor

   -----------------------------------
   -- Lexical utilities (internals) --
   -----------------------------------

   type Token_Type is record
      TDH           : Token_Data_Handler_Access;
      --  Token data handler that owns this token

      Token, Trivia : Token_Index;
      --  Indices that identify what this token refers to.
      --
      --  * If this references a token, then Token is the corresponding index
      --    in TDH.Tokens and Trivia is No_Token_Index.
      --
      --  * If this references a trivia that comes before the first token,
      --    Token is No_Token_Index while Trivia is the corresponding index in
      --    TDH.Trivias.
      --
      --  * If this references a trivia that comes after some token, Token is
      --    the index for this token and Trivia is the corresponding index for
      --    this trivia.
      --
      --  * If this references no token, both Token and Trivia are
      --    No_Token_Index.
   end record;

   No_Token : constant Token_Type := (null, No_Token_Index, No_Token_Index);

   type Token_Data_Type is record
      Kind          : Token_Kind;
      --  See documentation for the Kind accessor

      Is_Trivia     : Boolean;
      --  See documentation for the Is_Trivia accessor

      Index         : Token_Index;
      --  See documentation for the Index accessor

      Source_Buffer : Text_Cst_Access;
      --  Text for the original source file

      Source_First  : Positive;
      Source_Last   : Natural;
      --  Bounds in Source_Buffer for the text corresponding to this token

      Sloc_Range    : Source_Location_Range;
      --  See documenation for the Sloc_Range accessor
   end record;

   function First_Token (TDH : Token_Data_Handler_Access) return Token_Type;
   --  Internal helper. Return a reference to the first token in TDH.

   function Last_Token (TDH : Token_Data_Handler_Access) return Token_Type;
   --  Internal helper. Return a reference to the last token in TDH.

   --------------------------------
   -- Token Iterator (internals) --
   --------------------------------

   type Token_Iterator is record
      Node : ${root_entity.api_name};
      Last : Token_Index;
   end record;

   function Raw_Data (T : Token_Type) return Lexer.Token_Data_Type;
   --  Return the raw token data for T

end ${ada_lib_name}.Analysis;
