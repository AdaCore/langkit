## vim: filetype=makoada

<%namespace name="array_types"    file="array_types_ada.mako" />
<%namespace name="iterator_types" file="iterator_types_ada.mako" />
<%namespace name="struct_types"   file="struct_types_ada.mako" />
<%namespace name="astnode_types"  file="astnode_types_ada.mako" />
<%namespace name="exts"           file="../extensions.mako" />

<% entity_type = root_entity.c_type(capi).name %>

with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;

with System;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Text;  use Langkit_Support.Text;

with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;

${exts.with_clauses(with_clauses)}

--  Internal package: defines data types and subprograms to provide the
--  implementation of the exported C API (see the corresponding C header file).

package ${ada_lib_name}.Implementation.C is

   subtype ${analysis_context_type} is Internal_Context;
   ${ada_c_doc('langkit.analysis_context_type', 3)}

   subtype ${analysis_unit_type} is Internal_Unit;
   ${ada_c_doc('langkit.analysis_unit_type', 3)}

   type ${node_type} is new System.Address;
   ${ada_c_doc('langkit.node_type', 3)}

   type ${node_kind_type} is new int;
   ${ada_c_doc('langkit.node_kind_type', 3)}

   ${struct_types.decl(root_entity)}

   type ${symbol_type} is record
      Thin_Sym : Unsigned_32;
      Table    : System.Address;
   end record
      with Convention => C;
   ${ada_c_doc('langkit.symbol_type', 3)}

   subtype ${string_type} is String_Type;

   --  Helper data structures for source location handling

   type ${sloc_type} is record
      Line   : Unsigned_32;
      Column : Unsigned_16;
   end record
     with Convention => C;

   type ${sloc_range_type} is record
      Start_S, End_S : ${sloc_type};
   end record
     with Convention => C;

   type ${text_type} is record
      Chars  : System.Address;
      ${ada_c_doc('langkit.text_type.chars', 6)}

      Length : size_t;
      ${ada_c_doc('langkit.text_type.length', 6)}

      Is_Allocated : int;
   end record
     with Convention => C;
   ${ada_c_doc('langkit.text_type', 3)}

   type ${big_integer_type} is new System.Address;
   ${ada_c_doc('langkit.big_integer_type', 3)}

   type ${token_type} is record
      Context                   : ${analysis_context_type};
      Token_Data                : Token_Data_Handler_Access;
      Token_Index, Trivia_Index : int;
   end record
     with Convention => C;
   ${ada_c_doc('langkit.token_reference_type', 3)}

   type ${diagnostic_type} is record
      Sloc_Range : ${sloc_range_type};
      Message    : ${text_type};
      --  When the API returns a diagnostic, it is up to the caller to free the
      --  message string.
   end record
     with Convention => C;
   ${ada_c_doc('langkit.diagnostic_type', 3)}

   ${ada_enum_type_decl(
      exception_kind_type,
      [str(e.kind_name) for e in ctx.sorted_exception_types],
      3,
      convention_c=True,
   )}
   ${ada_c_doc('langkit.exception_kind_type', 3)}

   type ${exception_type} is record
      Kind : ${exception_kind_type};
      ${ada_c_doc('langkit.exception_type.kind', 6)}

      Information : chars_ptr;
      ${ada_c_doc('langkit.exception_type.information', 6)}
   end record;
   ${ada_c_doc('langkit.exception_type', 3)}

   type ${exception_type}_Ptr is access ${exception_type};

   type ${bool_type} is new Unsigned_8;
   subtype uint32_t is Unsigned_32;

   % for enum_type in ctx.enum_types:
      subtype ${enum_type.c_type(capi).name} is ${enum_type.name};
   % endfor

   procedure Free (Address : System.Address)
     with Export        => True,
          Convention    => C,
          External_Name => "${capi.get_name('free')}";
   ${ada_c_doc('langkit.free', 3)}
   --  Helper to free objects in dynamic languages

   procedure ${capi.get_name('destroy_text')} (T : access ${text_type})
     with Export        => True,
          Convention    => C,
          External_Name => "${capi.get_name('destroy_text')}";
   ${ada_c_doc('langkit.destroy_text', 3)}

   procedure ${capi.get_name('symbol_text')}
     (Symbol : access ${symbol_type}; Text : access ${text_type})
      with Export, Convention => C,
           External_Name => "${capi.get_name('symbol_text')}";
   ${ada_c_doc('langkit.symbol_text', 3)}

   function ${capi.get_name("create_big_integer")}
     (Text : access ${text_type}) return ${big_integer_type}
      with Export, Convention => C,
           External_Name => "${capi.get_name('create_big_integer')}";
   ${ada_c_doc('langkit.create_big_integer', 3)}

   procedure ${capi.get_name("big_integer_text")}
     (Bigint : ${big_integer_type}; Text : access ${text_type})
      with Export, Convention => C,
           External_Name => "${capi.get_name('big_integer_text')}";
   ${ada_c_doc('langkit.big_integer_text', 3)}

   procedure ${capi.get_name("big_integer_decref")}
     (Bigint : ${big_integer_type})
      with Export, Convention => C,
           External_Name => "${capi.get_name('big_integer_decref')}";
   ${ada_c_doc('langkit.big_integer_decref', 3)}

   procedure ${capi.get_name("get_versions")}
     (Version, Build_Date : access chars_ptr)
      with Export, Convention => C,
           External_Name => "${capi.get_name('get_versions')}";
   ${ada_c_doc('langkit.get_versions', 3)}

   function ${capi.get_name("create_string")}
     (Content : System.Address; Length : int) return ${string_type}
      with Export, Convention => C,
           External_Name => "${capi.get_name('create_string')}";
   ${ada_c_doc('langkit.create_string', 3)}

   procedure ${capi.get_name("string_dec_ref")} (Self : ${string_type})
      with Export, Convention => C,
           External_Name => "${capi.get_name('string_dec_ref')}";
   ${ada_c_doc('langkit.string_dec_ref', 3)}

   ------------------
   -- File readers --
   ------------------

   type ${file_reader_type} is new System.Address;
   ${ada_c_doc('langkit.file_reader_type', 3)}

   type ${file_reader_destroy_type} is access procedure
     (Data : System.Address)
      with Convention => C;
   ${ada_c_doc('langkit.file_reader_destroy_type', 3)}

   type ${file_reader_read_type} is access procedure
     (Data       : System.Address;
      Filename   : chars_ptr;
      Charset    : chars_ptr;
      Read_BOM   : int;
      Buffer     : access ${text_type};
      Diagnostic : access ${diagnostic_type})
      with Convention => C;
   ${ada_c_doc('langkit.file_reader_read_type', 3)}

   --------------------
   -- Event handlers --
   --------------------

   type ${event_handler_type} is new System.Address;
   ${ada_c_doc('langkit.event_handler_type', 3)}

   type ${event_handler_unit_requested_type} is access procedure
     (Data               : System.Address;
      Context            : ${analysis_context_type};
      Name               : access constant ${text_type};
      From               : ${analysis_unit_type};
      Found              : ${bool_type};
      Is_Not_Found_Error : ${bool_type})
      with Convention => C;
   ${ada_c_doc('langkit.event_handler_unit_requested_callback', 3)}

   type ${event_handler_unit_parsed_type} is access procedure
     (Data     : System.Address;
      Context  : ${analysis_context_type};
      Unit     : ${analysis_unit_type};
      Reparsed : ${bool_type})
      with Convention => C;
   ${ada_c_doc('langkit.event_handler_unit_parsed_callback', 3)}

   type ${event_handler_destroy_type} is access procedure
     (Data : System.Address)
      with Convention => C;
   ${ada_c_doc('langkit.event_handler_destroy_callback', 3)}

   --------------------
   -- Unit providers --
   --------------------

   type ${unit_provider_type} is new System.Address;
   ${ada_c_doc('langkit.unit_provider_type', 3)}

   -------------------------
   -- Analysis primitives --
   -------------------------

   function ${capi.get_name("allocate_analysis_context")}
     return ${analysis_context_type}
     with Export,
          Convention    => C,
          External_name => "${capi.get_name('allocate_analysis_context')}";
   ${ada_c_doc('langkit.allocate_context', 3)}

   procedure ${capi.get_name("initialize_analysis_context")}
     (Context       : ${analysis_context_type};
      Charset       : chars_ptr;
      File_Reader   : ${file_reader_type};
      Unit_Provider : ${unit_provider_type};
      Event_Handler : ${event_handler_type};
      With_Trivia   : int;
      Tab_Stop      : int)
      with Export,
           Convention    => C,
           External_name => "${capi.get_name('initialize_analysis_context')}";
   ${ada_c_doc('langkit.initialize_context', 3)}

   function ${capi.get_name('context_incref')}
     (Context : ${analysis_context_type})
      return ${analysis_context_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('context_incref')}";
   ${ada_c_doc('langkit.context_incref', 3)}

   procedure ${capi.get_name('context_decref')}
     (Context : ${analysis_context_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('context_decref')}";
   ${ada_c_doc('langkit.context_decref', 3)}

   function ${capi.get_name('context_symbol')}
     (Context : ${analysis_context_type};
      Text    : access ${text_type};
      Symbol  : access ${symbol_type}) return int
      with Export, Convention => C,
           External_name => "${capi.get_name('context_symbol')}";
   ${ada_c_doc('langkit.context_symbol', 3)}

   procedure ${capi.get_name("context_discard_errors_in_populate_lexical_env")}
     (Context : ${analysis_context_type};
      Discard : int)
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name(
              'context_discard_errors_in_populate_lexical_env')}";
   ${ada_c_doc('langkit.context_discard_errors_in_populate_lexical_env', 3)}

   function ${capi.get_name('get_analysis_unit_from_file')}
     (Context           : ${analysis_context_type};
      Filename, Charset : chars_ptr;
      Reparse           : int;
      Rule              : ${grammar_rule_type})
      return ${analysis_unit_type}
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name('get_analysis_unit_from_file')}";
   ${ada_c_doc('langkit.get_unit_from_file', 3)}

   function ${capi.get_name('get_analysis_unit_from_buffer')}
     (Context           : ${analysis_context_type};
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t;
      Rule              : ${grammar_rule_type})
      return ${analysis_unit_type}
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name('get_analysis_unit_from_buffer')}";
   ${ada_c_doc('langkit.get_unit_from_buffer', 3)}

   % if ctx.default_unit_provider:
   function ${capi.get_name('get_analysis_unit_from_provider')}
     (Context : ${analysis_context_type};
      Name    : ${text_type};
      Kind    : ${unit_kind_type};
      Charset : chars_ptr;
      Reparse : int) return ${analysis_unit_type}
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name('get_analysis_unit_from_provider')}";
   ${ada_c_doc('langkit.get_unit_from_provider', 6)}
   % endif

   procedure ${capi.get_name('unit_root')}
     (Unit     : ${analysis_unit_type};
      Result_P : ${entity_type}_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_root')}";
   ${ada_c_doc('langkit.unit_root', 3)}

   procedure ${capi.get_name('unit_first_token')}
     (Unit  : ${analysis_unit_type};
      Token : access ${token_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_first_token')}";
   ${ada_c_doc('langkit.unit_first_token', 3)}

   procedure ${capi.get_name('unit_last_token')}
     (Unit  : ${analysis_unit_type};
      Token : access ${token_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_last_token')}";
   ${ada_c_doc('langkit.unit_last_token', 3)}

   function ${capi.get_name('unit_token_count')}
     (Unit : ${analysis_unit_type}) return int
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('unit_token_count')}";
   ${ada_c_doc('langkit.unit_token_count', 3)}

   function ${capi.get_name('unit_trivia_count')}
     (Unit : ${analysis_unit_type}) return int
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('unit_trivia_count')}";
   ${ada_c_doc('langkit.unit_trivia_count', 3)}

   procedure ${capi.get_name('unit_lookup_token')}
     (Unit   : ${analysis_unit_type};
      Sloc   : access ${sloc_type};
      Result : access ${token_type})
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('unit_lookup_token')}";
   ${ada_c_doc('langkit.unit_lookup_token', 3)}

   procedure ${capi.get_name('unit_dump_lexical_env')}
     (Unit : ${analysis_unit_type})
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('unit_dump_lexical_env')}";

   function ${capi.get_name('unit_filename')}
     (Unit : ${analysis_unit_type})
      return chars_ptr
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_filename')}";
   ${ada_c_doc('langkit.unit_filename', 3)}

   function ${capi.get_name('unit_diagnostic_count')}
     (Unit : ${analysis_unit_type}) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_diagnostic_count')}";
   ${ada_c_doc('langkit.unit_diagnostic_count', 3)}

   function ${capi.get_name('unit_diagnostic')}
     (Unit         : ${analysis_unit_type};
      N            : unsigned;
      Diagnostic_P : access ${diagnostic_type}) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_diagnostic')}";
   ${ada_c_doc('langkit.unit_diagnostic', 3)}

   function ${capi.get_name('unit_context')}
     (Unit : ${analysis_unit_type})
      return ${analysis_context_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_context')}";
   ${ada_c_doc('langkit.unit_context', 3)}

   procedure ${capi.get_name('unit_reparse_from_file')}
     (Unit : ${analysis_unit_type}; Charset : chars_ptr)
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_reparse_from_file')}";
   ${ada_c_doc('langkit.unit_reparse_file', 3)}

   procedure ${capi.get_name('unit_reparse_from_buffer')}
     (Unit        : ${analysis_unit_type};
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t)
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_reparse_from_buffer')}";
   ${ada_c_doc('langkit.unit_reparse_buffer', 3)}

   function ${capi.get_name('unit_populate_lexical_env')}
     (Unit : ${analysis_unit_type}
      % if ctx.ple_unit_root:
          ; PLE_Root_Index : int
      % endif
   ) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_populate_lexical_env')}";
   ${ada_c_doc('langkit.unit_populate_lexical_env', 3)}

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   procedure ${capi.get_name('create_bare_entity')}
     (Node   : ${node_type};
      Entity : access ${entity_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('create_bare_entity')}";
   ${ada_c_doc('langkit.create_bare_entity', 3)}

   function ${capi.get_name('is_equivalent')}
     (L, R : ${entity_type}_Ptr) return ${bool_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('node_is_equivalent')}";
   ${ada_c_doc('langkit.node_is_equivalent', 3)}

   function ${capi.get_name('hash')}
     (Node : ${entity_type}_Ptr) return uint32_t
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('node_hash')}";
   ${ada_c_doc('langkit.node_hash', 3)}

   function ${capi.get_name('node_kind')}
     (Node : ${entity_type}_Ptr) return ${node_kind_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('node_kind')}";
   ${ada_c_doc('langkit.node_kind', 3)}

   procedure ${capi.get_name('kind_name')}
     (Kind : ${node_kind_type}; Result : access ${text_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('kind_name')}";
   ${ada_c_doc('langkit.kind_name', 3)}

   function ${capi.get_name('node_unit')}
     (Node : ${entity_type}_Ptr) return ${analysis_unit_type}
      with Export => True,
           Convention => C,
           External_Name => "${capi.get_name('node_unit')}";
   ${ada_c_doc('langkit.node_unit', 3)}

   function ${capi.get_name('is_token_node')}
     (Node : ${entity_type}_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('node_is_token_node')}";
   ${ada_c_doc('langkit.node_is_token_node', 3)}

   function ${capi.get_name('is_synthetic')}
     (Node : ${entity_type}_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('node_is_synthetic')}";
   ${ada_c_doc('langkit.node_is_synthetic', 3)}

   procedure ${capi.get_name('node_image')}
     (Node : ${entity_type}_Ptr; Result : access ${text_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('node_image')}";
   ${ada_c_doc('langkit.node_image', 3)}

   procedure ${capi.get_name('node_text')}
     (Node : ${entity_type}_Ptr;
      Text : access ${text_type})
      with Export, Convention => C,
           External_Name      => "${capi.get_name('node_text')}";
   ${ada_c_doc('langkit.node_text', 3)}

   procedure ${capi.get_name('node_sloc_range')}
     (Node         : ${entity_type}_Ptr;
      Sloc_Range_P : access ${sloc_range_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('node_sloc_range')}";
   ${ada_c_doc('langkit.node_sloc_range', 3)}

   procedure ${capi.get_name('lookup_in_node')}
     (Node   : ${entity_type}_Ptr;
      Sloc   : ${sloc_type};
      Result : ${entity_type}_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('lookup_in_node')}";
   ${ada_c_doc('langkit.lookup_in_node', 3)}

   function ${capi.get_name('node_children_count')}
     (Node : ${entity_type}_Ptr) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('node_children_count')}";
   ${ada_c_doc('langkit.node_children_count', 3)}

   function ${capi.get_name('node_child')}
     (Node    : ${entity_type}_Ptr;
      N       : unsigned;
      Child_P : ${entity_type}_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('node_child')}";
   ${ada_c_doc('langkit.node_child', 3)}

   function ${capi.get_name('text_to_locale_string')}
     (Text : ${text_type}) return System.Address
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('text_to_locale_string')}";
   ${ada_c_doc('langkit.text_to_locale_string', 3)}

   ------------------
   -- File readers --
   ------------------

   function ${capi.get_name('create_file_reader')}
     (Data         : System.Address;
      Destroy_Func : ${file_reader_destroy_type};
      Read_Func    : ${file_reader_read_type}) return ${file_reader_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('create_file_reader')}";
   ${ada_c_doc('langkit.create_file_reader', 3)}

   procedure ${capi.get_name('dec_ref_file_reader')}
     (File_Reader : ${file_reader_type})
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name('dec_ref_file_reader')}";
   ${ada_c_doc('langkit.file_reader_dec_ref', 3)}

   ${exts.include_extension(
      ctx.ext('analysis', 'c_api', 'file_readers', 'spec')
   )}

   --------------------
   -- Event handlers --
   --------------------

   function ${capi.get_name('create_event_handler')}
     (Data                : System.Address;
      Destroy_Func        : ${event_handler_destroy_type};
      Unit_Requested_Func : ${event_handler_unit_requested_type};
      Unit_Parsed_Func    : ${event_handler_unit_parsed_type})
      return ${event_handler_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('create_event_handler')}";
   ${ada_c_doc('langkit.create_event_handler', 3)}

   procedure ${capi.get_name('dec_ref_event_handler')}
     (Handler : ${event_handler_type})
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name('dec_ref_event_handler')}";
   ${ada_c_doc('langkit.event_handler_dec_ref', 3)}

   ${exts.include_extension(
      ctx.ext('analysis', 'c_api', 'event_handlers', 'spec')
   )}

   --------------------
   -- Unit providers --
   --------------------

   procedure ${capi.get_name('dec_ref_unit_provider')}
     (Provider : ${unit_provider_type})
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name('dec_ref_unit_provider')}";
   ${ada_c_doc('langkit.unit_provider_dec_ref', 3)}

   ${exts.include_extension(
      ctx.ext('analysis', 'c_api', 'unit_providers', 'spec')
   )}

   ------------------
   -- Struct types --
   ------------------

   % for struct_type in ctx.struct_types:
      % if struct_type.exposed and \
            struct_type.emit_c_type and \
            struct_type != root_entity:
         ${struct_types.decl(struct_type)}
      % endif
   % endfor

   -----------------
   -- Array types --
   -----------------

   % for array_type in ctx.array_types:
      % if array_type.exposed and array_type.emit_c_type:
         ${array_types.decl(array_type)}
      % endif
   % endfor

   --------------------
   -- Iterator types --
   --------------------

   % for iterator_type in ctx.iterator_types:
       % if iterator_type.exposed and iterator_type.emit_c_type:
           ${iterator_types.decl(iterator_type)}
       % endif
   % endfor

   ----------
   -- Misc --
   ----------

   function ${capi.get_name('get_last_exception')} return ${exception_type}_Ptr
     with Export        => True,
          Convention    => C,
          External_Name => "${capi.get_name('get_last_exception')}";
   ${ada_c_doc('langkit.get_last_exception', 3)}

   function ${capi.get_name('exception_name')}
     (Kind : ${exception_kind_type}) return chars_ptr
      with Export, Convention => C;
   ${ada_c_doc('langkit.exception_name', 3)}

   procedure Clear_Last_Exception;
   --  Free the information contained in Last_Exception

   procedure Set_Last_Exception (Exc : Exception_Occurrence);
   --  Free the information contained in Last_Exception and replace it with
   --  newly allocated information from Exc.

   procedure Set_Last_Exception (Id : Exception_Id; Message : String);
   --  Likewise, but put destructured exception information. This is useful to
   --  pass messages that are longer than what the Ada runtime accepts (i.e.
   --  allows to avoid truncated error messages).

   function ${capi.get_name('token_get_kind')}
     (Token : ${token_type}) return int
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('token_get_kind')}";
   ${ada_c_doc('langkit.token_kind', 3)}

   function ${capi.get_name('token_kind_name')} (Kind : int) return chars_ptr
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('token_kind_name')}";
   ${ada_c_doc('langkit.token_kind_name', 3)}

   procedure ${capi.get_name('token_sloc_range')}
     (Token : ${token_type}; Result : access ${sloc_range_type})
      with Export        => True,
           Convention    => C,
           External_Name => "${capi.get_name('token_sloc_range')}";
   ${ada_c_doc('langkit.token_sloc_range', 3)}

   procedure ${capi.get_name('token_next')}
     (Token      : ${token_type};
      Next_Token : access ${token_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('token_next')}";
   ${ada_c_doc('langkit.token_next', 3)}

   procedure ${capi.get_name('token_previous')}
     (Token          : ${token_type};
      Previous_Token : access ${token_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('token_previous')}";
   ${ada_c_doc('langkit.token_previous', 3)}

   function ${capi.get_name('token_range_text')}
     (First, Last : ${token_type};
      Text        : access ${text_type}) return int
      with Export => True,
           Convention => C,
           External_Name => "${capi.get_name('token_range_text')}";
   ${ada_c_doc('langkit.token_range_text', 3)}

   function ${capi.get_name('token_is_equivalent')}
     (Left  : ${token_type};
      Right : ${token_type}) return ${bool_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('token_is_equivalent')}";
   ${ada_c_doc('langkit.token_is_equivalent', 3)}

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   --  All these primitives return their result through an OUT parameter. They
   --  return a boolean telling whether the operation was successful (it can
   --  fail if the node does not have the proper type, for instance). When an
   --  AST node is returned, its ref-count is left as-is.

   % for astnode in ctx.astnode_types:
       % for field in astnode.fields_with_accessors():
           ${astnode_types.accessor_decl(field)}
       % endfor
   % endfor

   ------------------------
   -- Conversion helpers --
   ------------------------

   --  The following conversion helpers are use by the various C bindings

   function Wrap (S : Source_Location) return ${sloc_type} is
     ((Unsigned_32 (S.Line), Unsigned_16 (S.Column)));
   function Unwrap (S : ${sloc_type}) return Source_Location is
     ((Line_Number (S.Line), Column_Number (S.Column)));

   function Wrap (S : Source_Location_Range) return ${sloc_range_type} is
     ((Start_S => (Unsigned_32 (S.Start_Line), Unsigned_16 (S.Start_Column)),
       End_S   => (Unsigned_32 (S.End_Line),   Unsigned_16 (S.End_Column))));
   function Unwrap (S : ${sloc_range_type}) return Source_Location_Range is
     ((Line_Number (S.Start_S.Line),
       Line_Number (S.End_S.Line),
       Column_Number (S.Start_S.Column),
       Column_Number (S.End_S.Column)));

   function Wrap (S : Unbounded_Wide_Wide_String) return ${text_type};

   function Wrap_Alloc (S : Text_Type) return ${text_type};
   function Wrap_Alloc (S : Unbounded_Wide_Wide_String) return ${text_type};
   function Wrap
     (S     : Text_Cst_Access;
      First : Positive;
      Last  : Natural) return ${text_type};

   function Wrap (T : Text_Cst_Access) return ${text_type} is
     (if T = null
      then (Chars => System.Null_Address, Length => 0, Is_Allocated => 0)
      else (Chars => T.all'Address, Length => T.all'Length, Is_Allocated => 0));
   function Wrap (T : Text_Access) return ${text_type} is
     (Wrap (Text_Cst_Access (T)));

   function Wrap_Big_Integer is new Ada.Unchecked_Conversion
     (Big_Integer_Type, ${big_integer_type});
   function Unwrap_Big_Integer is new Ada.Unchecked_Conversion
     (${big_integer_type}, Big_Integer_Type);

   --  Probably because the following conversions involve fat pointers, using
   --  the No_Strict_Aliasing pragma here has no effect. Silence the warning,
   --  since all read/writes for the pointed values are made through the "real"
   --  fat pointer (Symbol_Type) and not the fake one (${symbol_type}): strict
   --  aliasing issues should not happen.

   pragma Warnings (Off, "possible aliasing problem for type");
   function Wrap_Symbol is new Ada.Unchecked_Conversion
     (Symbol_Type, ${symbol_type});
   function Unwrap_Symbol is new Ada.Unchecked_Conversion
     (${symbol_type}, Symbol_Type);
   pragma Warnings (On, "possible aliasing problem for type");

   function Wrap is new Ada.Unchecked_Conversion
     (${T.root_node.name}, ${node_type});
   function Unwrap is new Ada.Unchecked_Conversion
     (${node_type}, ${T.root_node.name});

   function Wrap (Token : Token_Reference) return ${token_type};
   function Unwrap (Token : ${token_type}) return Token_Reference;

   function Wrap_Private_File_Reader is new Ada.Unchecked_Conversion
     (Internal_File_Reader_Access, ${file_reader_type});
   function Unwrap_Private_File_Reader is new Ada.Unchecked_Conversion
     (${file_reader_type}, Internal_File_Reader_Access);

   function Wrap_Private_Event_Handler is new Ada.Unchecked_Conversion
     (Internal_Event_Handler_Access, ${event_handler_type});
   function Unwrap_Private_Event_Handler is new Ada.Unchecked_Conversion
     (${event_handler_type}, Internal_Event_Handler_Access);

   function Wrap_Private_Provider is new Ada.Unchecked_Conversion
     (Internal_Unit_Provider_Access, ${unit_provider_type});
   function Unwrap_Private_Provider is new Ada.Unchecked_Conversion
     (${unit_provider_type}, Internal_Unit_Provider_Access);

   function Convert is new Ada.Unchecked_Conversion
     (chars_ptr, System.Address);

   % for array_type in ctx.array_types:
      % if array_type.element_type.is_entity_type and \
            array_type.element_type != T.entity:
         function Convert is new Ada.Unchecked_Conversion
           (${array_type.name}, ${T.entity.array.name});
         function Convert is new Ada.Unchecked_Conversion
           (${T.entity.array.name}, ${array_type.name});
      % endif
   % endfor

   % for iterator_type in ctx.iterator_types:
      % if iterator_type.element_type.is_entity_type and \
            iterator_type.element_type != T.entity and \
            iterator_type.is_used:
         function Convert is new Ada.Unchecked_Conversion
           (${iterator_type.name}, ${T.entity.iterator.name});
      % endif
   % endfor

end ${ada_lib_name}.Implementation.C;
