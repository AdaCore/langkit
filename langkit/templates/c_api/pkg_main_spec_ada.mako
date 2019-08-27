## vim: filetype=makoada

<%namespace name="array_types"   file="array_types_ada.mako" />
<%namespace name="struct_types"  file="struct_types_ada.mako" />
<%namespace name="astnode_types" file="astnode_types_ada.mako" />
<%namespace name="exts"          file="../extensions.mako" />

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

with ${ada_lib_name}.Analysis; use ${ada_lib_name}.Analysis;
with ${ada_lib_name}.Common;   use ${ada_lib_name}.Common;

${exts.with_clauses(with_clauses)}

--  Internal package: defines data types and subprograms to provide the
--  implementation of the exported C API (see the corresponding C header file).

private package ${ada_lib_name}.Implementation.C is

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
      Data, Bounds : System.Address;
   end record
      with Convention => C;
   ${ada_c_doc('langkit.symbol_type', 3)}

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
      Token_Data                : System.Address;
      Token_Index, Trivia_Index : int;

      Kind       : int;
      Text       : ${text_type};
      Sloc_Range : ${sloc_range_type};
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

   type ${exception_kind_type} is (
      ${', '.join(str(ctx.exception_kind_name(exc))
                  for _, exc in ctx.sorted_exception_types)}
   ) with Convention => C;
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

   --  Types for unit providers

   type ${unit_provider_type} is new System.Address;
   ${ada_c_doc('langkit.unit_provider_type', 3)}

   type Unit_Provider_Access is access all Unit_Provider_Reference;
   procedure Free is new Ada.Unchecked_Deallocation
     (Unit_Provider_Reference, Unit_Provider_Access);

   function Create_Unit_Provider_C_Reference
     (Provider : Unit_Provider_Interface'Class) return ${unit_provider_type};

   type ${unit_provider_destroy_type} is access procedure
     (Data : System.Address)
      with Convention => C;
   ${ada_c_doc('langkit.unit_provider_destroy_type', 3)}

   type ${unit_provider_get_unit_filename_type} is access function
     (Data        : System.Address;
      Name        : ${text_type};
      Kind        : ${unit_kind_type}) return chars_ptr
      with Convention => C;
   ${ada_c_doc('langkit.unit_provider_get_unit_filename_type', 3)}

   type ${unit_provider_get_unit_from_name_type} is access function
     (Data        : System.Address;
      Context     : ${analysis_context_type};
      Name        : ${text_type};
      Kind        : ${unit_kind_type};
      Charset     : chars_ptr;
      Reparse     : int) return ${analysis_unit_type}
      with Convention => C;
   ${ada_c_doc('langkit.unit_provider_get_unit_from_name_type', 3)}

   -------------------------
   -- Analysis primitives --
   -------------------------

   function ${capi.get_name("create_analysis_context")}
     (Charset       : chars_ptr;
      Unit_Provider : ${unit_provider_type};
      With_Trivia   : int;
      Tab_Stop      : int) return ${analysis_context_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('create_analysis_context')}";
   ${ada_c_doc('langkit.create_context', 3)}

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
     (Unit : ${analysis_unit_type})
      return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('unit_populate_lexical_env')}";
   ${ada_c_doc('langkit.unit_populate_lexical_env', 3)}

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

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

   procedure ${capi.get_name('node_short_image')}
     (Node : ${entity_type}_Ptr; Result : access ${text_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('node_short_image')}";
   ${ada_c_doc('langkit.node_short_image', 3)}

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

   ${array_types.decl(T.root_node.array)}
   ${array_types.decl(T.root_node.entity.array)}

   --------------------
   -- Unit providers --
   --------------------

   function ${capi.get_name('create_unit_provider')}
     (Data                    : System.Address;
      Destroy_Func            : ${unit_provider_destroy_type};
      Get_Unit_Filename_Func  : ${unit_provider_get_unit_filename_type};
      Get_Unit_From_Name_Func : ${unit_provider_get_unit_from_name_type})
      return ${unit_provider_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('create_unit_provider')}";
   ${ada_c_doc('langkit.create_unit_provider', 3)}

   procedure ${capi.get_name('destroy_unit_provider')}
     (Provider : ${unit_provider_type})
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name('destroy_unit_provider')}";
   ${ada_c_doc('langkit.destroy_unit_provider', 3)}

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
      % if array_type.element_type.should_emit_array_type and \
            array_type.exposed and \
            array_type.emit_c_type:
         ${array_types.decl(array_type)}
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

   procedure Clear_Last_Exception;
   --  Free the information contained in Last_Exception

   procedure Set_Last_Exception (Exc : Exception_Occurrence);
   --  Free the information contained in Last_Exception and replace it with
   --  newly allocated information from Exc.

   function ${capi.get_name('token_kind_name')} (Kind : int) return chars_ptr
      with Export => True,
           Convention => C,
           External_Name => "${capi.get_name('token_kind_name')}";
   ${ada_c_doc('langkit.token_kind_name', 3)}

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

   procedure ${capi.get_name('entity_image')}
     (Ent : ${entity_type}_Ptr; Result : access ${text_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name('entity_image')}";
   ${ada_c_doc('langkit.entity_image', 3)}

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

   --  The following conversions are used only at the interface between Ada and
   --  C (i.e. as parameters and return types for C entry points) for access
   --  types.  All read/writes for the pointed values are made through the
   --  access values and never through the System.Address values.  Thus, strict
   --  aliasing issues should not arise for these.
   --
   --  See <https://gcc.gnu.org/onlinedocs/gnat_ugn/
   --       Optimization-and-Strict-Aliasing.html>.

   pragma Warnings (Off, "possible aliasing problem for type");

   function Wrap_Big_Integer is new Ada.Unchecked_Conversion
     (Big_Integer_Type, ${big_integer_type});
   function Unwrap_Big_Integer is new Ada.Unchecked_Conversion
     (${big_integer_type}, Big_Integer_Type);

   function Wrap_Symbol is new Ada.Unchecked_Conversion
     (Symbol_Type, ${symbol_type});
   function Unwrap_Symbol is new Ada.Unchecked_Conversion
     (${symbol_type}, Symbol_Type);

   function Wrap is new Ada.Unchecked_Conversion
     (${T.root_node.name}, ${node_type});
   function Unwrap is new Ada.Unchecked_Conversion
     (${node_type}, ${T.root_node.name});

   function Wrap (Token : Token_Reference) return ${token_type};
   function Unwrap (Token : ${token_type}) return Token_Reference;

   function Wrap is new Ada.Unchecked_Conversion
     (Unit_Provider_Access, ${unit_provider_type});
   function Unwrap is new Ada.Unchecked_Conversion
     (${unit_provider_type}, Unit_Provider_Access);

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

   pragma Warnings (On, "possible aliasing problem for type");

end ${ada_lib_name}.Implementation.C;
