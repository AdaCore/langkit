








with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with GNAT.Traceback;

with System;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Liblktlang_Support.Slocs; use Liblktlang_Support.Slocs;
with Liblktlang_Support.Text;  use Liblktlang_Support.Text;

with Liblktlang.Common;   use Liblktlang.Common;




--  Internal package: defines data types and subprograms to provide the
--  implementation of the exported C API (see the corresponding C header file).

package Liblktlang.Implementation.C is

   subtype lkt_analysis_context is Internal_Context;
   --  This type represents a context for all source analysis. This is the
   --  first type you need to create to use Liblktlang. It will contain the
   --  results of all analysis, and is the main holder for all the data.
   --
   --  You can create several analysis contexts if you need to, which enables
   --  you, for example to:
   --
   --  * analyze several different projects at the same time;
   --
   --  * analyze different parts of the same projects in parallel.
   --
   --  In the current design, contexts always keep all of their analysis units
   --  allocated. If you need to get this memory released, the only option at
   --  your disposal is to destroy your analysis context instance.
   --
   --  This structure is partially opaque: some fields are exposed to allow
   --  direct access, for performance concerns.

   subtype lkt_analysis_unit is Internal_Unit;
   --  This type represents the analysis of a single file.
   --
   --  This type has strong-reference semantics and is ref-counted.
   --  Furthermore, a reference to a unit contains an implicit reference to the
   --  context that owns it. This means that keeping a reference to a unit will
   --  keep the context and all the unit it contains allocated.
   --
   --  This structure is partially opaque: some fields are exposed to allow
   --  direct access, for performance concerns.

   type lkt_base_node is new System.Address;
   --  Data type for all nodes. Nodes are assembled to make up a tree.  See the
   --  node primitives below to inspect such trees.
   --
   --  Unlike for contexts and units, this type has weak-reference semantics:
   --  keeping a reference to a node has no effect on the decision to keep the
   --  unit that it owns allocated. This means that once all references to the
   --  context and units related to a node are dropped, the context and its
   --  units are deallocated and the node becomes a stale reference: most
   --  operations on it will raise a ``Stale_Reference_Error``.
   --
   --  Note that since reparsing an analysis unit deallocates all the nodes it
   --  contains, this operation makes all reference to these nodes stale as
   --  well.

   type lkt_node_kind_enum is new int;
   --  Kind of AST nodes in parse trees.

   



subtype lkt_node is Internal_Entity;
type lkt_node_Ptr is access Internal_Entity;




   type lkt_symbol_type is record
      Thin_Sym : Unsigned_32;
      Table    : System.Address;
   end record
      with Convention => C;
   --  Reference to a symbol. Symbols are owned by analysis contexts, so they
   --  must not outlive them. This type exists only in the C API, and roughly
   --  wraps the corresponding Ada type (an array fat pointer).

   subtype lkt_string_type is String_Type;

   --  Helper data structures for source location handling

   type lkt_source_location is record
      Line   : Unsigned_32;
      Column : Unsigned_16;
   end record
     with Convention => C;

   type lkt_source_location_range is record
      Start_S, End_S : lkt_source_location;
   end record
     with Convention => C;

   type lkt_text is record
      Chars  : System.Address;
      --  Address for the content of the string.

      Length : size_t;
      --  Size of the string (in characters).

      Is_Allocated : int;
   end record
     with Convention => C;
   --  String encoded in UTF-32 (native endianness).

   type lkt_big_integer is new System.Address;
   --  Arbitrarily large integer.

   type lkt_token is record
      Context                   : lkt_analysis_context;
      Token_Data                : Token_Data_Handler_Access;
      Token_Index, Trivia_Index : int;
   end record
     with Convention => C;
   --  Reference to a token in an analysis unit.

   type lkt_diagnostic is record
      Sloc_Range : lkt_source_location_range;
      Message    : lkt_text;
      --  When the API returns a diagnostic, it is up to the caller to free the
      --  message string.
   end record
     with Convention => C;
   --  Diagnostic for an analysis unit: cannot open the source file, parsing
   --  error, ...

   type lkt_exception_kind is
     (Exception_File_Read_Error,
      Exception_Bad_Type_Error,
      Exception_Out_Of_Bounds_Error,
      Exception_Invalid_Input,
      Exception_Invalid_Symbol_Error,
      Exception_Invalid_Unit_Name_Error,
      Exception_Native_Exception,
      Exception_Precondition_Failure,
      Exception_Property_Error,
      Exception_Template_Args_Error,
      Exception_Template_Format_Error,
      Exception_Template_Instantiation_Error,
      Exception_Stale_Reference_Error,
      Exception_Syntax_Error,
      Exception_Unknown_Charset,
      Exception_Malformed_Tree_Error)
   with Convention => C;
   --  Enumerated type describing all possible exceptions that need to be
   --  handled in the C bindings.

   type Stack_Trace_Record (Capacity : Natural) is record
      Size : Natural;
      --  Number of elements in ``Items`` actually part of the stack trace,
      --  i.e. the actual stack trace is in ``Items (Capacity .. Size)``.

      Items : GNAT.Traceback.Tracebacks_Array (1 .. Capacity);
   end record;

   type lkt_stack_trace is access Stack_Trace_Record;
   --  Native stack trace (i.e. call chain).

   procedure Free is new Ada.Unchecked_Deallocation
     (Stack_Trace_Record, lkt_stack_trace);

   type lkt_exception is record
      Kind : lkt_exception_kind;
      --  The kind of this exception.

      Information : chars_ptr;
      --  Message and context information associated with this exception.

      Stack_Trace : lkt_stack_trace;
      --  Native stack trace associated to the exception.
   end record;
   --  Holder for native exceptions-related information.  Memory management for
   --  this and all the fields is handled by the library: one just has to make
   --  sure not to keep references to it.
   --
   --  .. TODO: For the moment, this structure contains already formatted
   --     information, but depending on possible future Ada runtime
   --     improvements, this might change.

   type lkt_exception_Ptr is access lkt_exception;

   type lkt_bool is new Unsigned_8;
   subtype uint32_t is Unsigned_32;

      subtype lkt_analysis_unit_kind is Analysis_Unit_Kind;
      subtype lkt_completion_item_kind is Completion_Item_Kind;
      subtype lkt_designated_env_kind is Designated_Env_Kind;
      subtype lkt_grammar_rule is Grammar_Rule;
      subtype lkt_lookup_kind is Lookup_Kind;

   procedure Free (Address : System.Address)
     with Export        => True,
          Convention    => C,
          External_Name => "lkt_free";
   --  Free dynamically allocated memory.
   --
   --  This is a helper to free objects from dynamic languages.
   --  Helper to free objects in dynamic languages

   procedure lkt_destroy_text (T : access lkt_text)
     with Export        => True,
          Convention    => C,
          External_Name => "lkt_destroy_text";
   --  If this text object owns the buffer it references, free this buffer.
   --
   --  Note that even though this accepts a pointer to a text object, it does
   --  not deallocates the text object itself but rather the buffer it
   --  references.

   procedure lkt_symbol_text
     (Symbol : access lkt_symbol_type; Text : access lkt_text)
      with Export, Convention => C,
           External_Name => "lkt_symbol_text";
   --  Return the text associated to this symbol.

   function lkt_create_big_integer
     (Text : access lkt_text) return lkt_big_integer
      with Export, Convention => C,
           External_Name => "lkt_create_big_integer";
   --  Create a big integer from its string representation (in base 10).

   procedure lkt_big_integer_text
     (Bigint : lkt_big_integer; Text : access lkt_text)
      with Export, Convention => C,
           External_Name => "lkt_big_integer_text";
   --  Return the string representation (in base 10) of this big integer.

   procedure lkt_big_integer_decref
     (Bigint : lkt_big_integer)
      with Export, Convention => C,
           External_Name => "lkt_big_integer_decref";
   --  Decrease the reference count for this big integer.

   procedure lkt_get_versions
     (Version, Build_Date : access chars_ptr)
      with Export, Convention => C,
           External_Name => "lkt_get_versions";
   --  Allocate strings to represent the library version number and build date
   --  and put them in Version/Build_Date. Callers are expected to call free()
   --  on the returned string once done.

   function lkt_create_string
     (Content : System.Address; Length : int) return lkt_string_type
      with Export, Convention => C,
           External_Name => "lkt_create_string";
   --  Create a string value from its content (UTF32 with native endianity).
   --
   --  Note that the CONTENT buffer argument is copied: the returned value does
   --  not contain a reference to it.

   procedure lkt_string_dec_ref (Self : lkt_string_type)
      with Export, Convention => C,
           External_Name => "lkt_string_dec_ref";
   --  Decrease the reference count for this string.

   ------------------
   -- File readers --
   ------------------

   type lkt_file_reader is new System.Address;
   --  Interface to override how source files are fetched and decoded.

   type lkt_file_reader_destroy_callback is access procedure
     (Data : System.Address)
      with Convention => C;
   --  Callback type for functions that are called when destroying a file
   --  reader.

   type lkt_file_reader_read_callback is access procedure
     (Data       : System.Address;
      Filename   : chars_ptr;
      Charset    : chars_ptr;
      Read_BOM   : int;
      Buffer     : access lkt_text;
      Diagnostic : access lkt_diagnostic)
      with Convention => C;
   --  Callback type for functions that are called to fetch the decoded source
   --  buffer for a requested filename.

   --------------------
   -- Event handlers --
   --------------------

   type lkt_event_handler is new System.Address;
   --  Interface to handle events sent by the analysis context.

   type lkt_event_handler_unit_requested_callback is access procedure
     (Data               : System.Address;
      Context            : lkt_analysis_context;
      Name               : access constant lkt_text;
      From               : lkt_analysis_unit;
      Found              : lkt_bool;
      Is_Not_Found_Error : lkt_bool)
      with Convention => C;
   --  Callback that will be called when a unit is requested from the context
   --  ``Context``.
   --
   --  ``Name`` is the name of the requested unit.
   --
   --  ``From`` is the unit from which the unit was requested.
   --
   --  ``Found`` indicates whether the requested unit was found or not.
   --
   --  ``Is_Not_Found_Error`` indicates whether the fact that the unit was not
   --  found is an error or not.
   --
   --  .. warning:: The interface of this callback is probably subject to
   --     change, so should be treated as experimental.

   type lkt_event_handler_unit_parsed_callback is access procedure
     (Data     : System.Address;
      Context  : lkt_analysis_context;
      Unit     : lkt_analysis_unit;
      Reparsed : lkt_bool)
      with Convention => C;
   --  Callback that will be called when any unit is parsed from the context
   --  ``Context``.
   --
   --  ``Unit`` is the resulting unit.
   --
   --  ``Reparsed`` indicates whether the unit was reparsed, or whether it was
   --  the first parse.

   type lkt_event_handler_destroy_callback is access procedure
     (Data : System.Address)
      with Convention => C;
   --  Callback type for functions that are called when destroying an event
   --  handler.

   --------------------
   -- Unit providers --
   --------------------

   type lkt_unit_provider is new System.Address;
   --  Interface to fetch analysis units from a name and a unit kind.
   --
   --  The unit provider mechanism provides an abstraction which assumes that
   --  to any couple (unit name, unit kind) we can associate at most one source
   --  file. This means that several couples can be associated to the same
   --  source file, but on the other hand, only one one source file can be
   --  associated to a couple.
   --
   --  This is used to make the semantic analysis able to switch from one
   --  analysis units to another.
   --
   --  See the documentation of each unit provider for the exact semantics of
   --  the unit name/kind information.

   -------------------------
   -- Analysis primitives --
   -------------------------

   function lkt_allocate_analysis_context
     return lkt_analysis_context
     with Export,
          Convention    => C,
          External_name => "lkt_allocate_analysis_context";
   --  Allocate a new analysis context.

   procedure lkt_initialize_analysis_context
     (Context       : lkt_analysis_context;
      Charset       : chars_ptr;
      File_Reader   : lkt_file_reader;
      Unit_Provider : lkt_unit_provider;
      Event_Handler : lkt_event_handler;
      With_Trivia   : int;
      Tab_Stop      : int)
      with Export,
           Convention    => C,
           External_name => "lkt_initialize_analysis_context";
   --  Initialize an analysis context. Must be called right after
   --  ``Allocate_Context`` on its result.
   --
   --  Having separate primitives for allocation/initialization allows library
   --  bindings to have a context wrapper (created between the two calls) ready
   --  when callbacks that happen during context initialization (for instance
   --  "unit parsed" events).

   function lkt_context_incref
     (Context : lkt_analysis_context)
      return lkt_analysis_context
      with Export        => True,
           Convention    => C,
           External_name => "lkt_context_incref";
   --  Increase the reference count to an analysis context. Return the
   --  reference for convenience.

   procedure lkt_context_decref
     (Context : lkt_analysis_context)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_context_decref";
   --  Decrease the reference count to an analysis context. Destruction happens
   --  when the ref-count reaches 0.

   function lkt_context_symbol
     (Context : lkt_analysis_context;
      Text    : access lkt_text;
      Symbol  : access lkt_symbol_type) return int
      with Export, Convention => C,
           External_name => "lkt_context_symbol";
   --  If the given string is a valid symbol, yield it as a symbol and return
   --  true. Otherwise, return false.

   procedure lkt_context_discard_errors_in_populate_lexical_env
     (Context : lkt_analysis_context;
      Discard : int)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_context_discard_errors_in_populate_lexical_env";
   --  Debug helper. Set whether ``Property_Error`` exceptions raised in
   --  ``Populate_Lexical_Env`` should be discarded. They are by default.

   function lkt_get_analysis_unit_from_file
     (Context           : lkt_analysis_context;
      Filename, Charset : chars_ptr;
      Reparse           : int;
      Rule              : lkt_grammar_rule)
      return lkt_analysis_unit
      with Export        => True,
           Convention    => C,
           External_name =>
              "lkt_get_analysis_unit_from_file";
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. If ``Reparse`` is true and the analysis unit already exists,
   --  reparse it from ``Filename``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.

   function lkt_get_analysis_unit_from_buffer
     (Context           : lkt_analysis_context;
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t;
      Rule              : lkt_grammar_rule)
      return lkt_analysis_unit
      with Export        => True,
           Convention    => C,
           External_name =>
              "lkt_get_analysis_unit_from_buffer";
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. Whether the analysis unit already exists or not, (re)parse it
   --  from the source code in ``Buffer``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.

   function lkt_get_analysis_unit_from_provider
     (Context : lkt_analysis_context;
      Name    : lkt_text;
      Kind    : lkt_analysis_unit_kind;
      Charset : chars_ptr;
      Reparse : int) return lkt_analysis_unit
      with Export        => True,
           Convention    => C,
           External_name =>
              "lkt_get_analysis_unit_from_provider";
   --  Create a new analysis unit for ``Name``/``Kind`` or return the existing
   --  one if any. If ``Reparse`` is true and the analysis unit already exists,
   --  reparse it from the on-disk source file.
   --
   --  The ``Name`` and ``Kind`` arguments are forwarded directly to query the
   --  context's unit provider and get the filename for the returned unit. See
   --  the documentation of the relevant unit provider for their exact
   --  semantics.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If the unit name cannot be tuned into a file name, return ``NULL``. If
   --  any other failure occurs, such as file opening, decoding, lexing or
   --  parsing failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.

   procedure lkt_unit_root
     (Unit     : lkt_analysis_unit;
      Result_P : lkt_node_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_unit_root";
   --  Return the root node for this unit, or ``NULL`` if there is none.

   procedure lkt_unit_first_token
     (Unit  : lkt_analysis_unit;
      Token : access lkt_token)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_unit_first_token";
   --  Return a reference to the first token scanned in this unit.

   procedure lkt_unit_last_token
     (Unit  : lkt_analysis_unit;
      Token : access lkt_token)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_unit_last_token";
   --  Return a reference to the last token scanned in this unit.

   function lkt_unit_token_count
     (Unit : lkt_analysis_unit) return int
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_unit_token_count";
   --  Return the number of tokens in this unit.

   function lkt_unit_trivia_count
     (Unit : lkt_analysis_unit) return int
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_unit_trivia_count";
   --  Return the number of trivias in this unit. This is 0 for units that were
   --  parsed with trivia analysis disabled.

   procedure lkt_unit_lookup_token
     (Unit   : lkt_analysis_unit;
      Sloc   : access lkt_source_location;
      Result : access lkt_token)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_unit_lookup_token";
   --  Look for a token in this unit that contains the given source location.
   --  If this falls before the first token, return the first token. If this
   --  falls between two tokens, return the token that appears before. If this
   --  falls after the last token, return the last token. If there is no token
   --  in this unit, return no token.

   procedure lkt_unit_dump_lexical_env
     (Unit : lkt_analysis_unit)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_unit_dump_lexical_env";

   function lkt_unit_filename
     (Unit : lkt_analysis_unit)
      return chars_ptr
      with Export        => True,
           Convention    => C,
           External_name => "lkt_unit_filename";
   --  Return the filename this unit is associated to.
   --
   --  The returned string is dynamically allocated and the caller must free it
   --  when done with it.

   function lkt_unit_diagnostic_count
     (Unit : lkt_analysis_unit) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "lkt_unit_diagnostic_count";
   --  Return the number of diagnostics associated to this unit.

   function lkt_unit_diagnostic
     (Unit         : lkt_analysis_unit;
      N            : unsigned;
      Diagnostic_P : access lkt_diagnostic) return int
      with Export        => True,
           Convention    => C,
           External_name => "lkt_unit_diagnostic";
   --  Get the Nth diagnostic in this unit and store it into ``*diagnostic_p``.
   --  Return zero on failure (when N is too big).

   function lkt_unit_context
     (Unit : lkt_analysis_unit)
      return lkt_analysis_context
      with Export        => True,
           Convention    => C,
           External_name => "lkt_unit_context";
   --  Return the context that owns this unit.

   procedure lkt_unit_reparse_from_file
     (Unit : lkt_analysis_unit; Charset : chars_ptr)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_unit_reparse_from_file";
   --  Reparse an analysis unit from the associated file.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure lkt_unit_reparse_from_buffer
     (Unit        : lkt_analysis_unit;
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_unit_reparse_from_buffer";
   --  Reparse an analysis unit from a buffer.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   function lkt_unit_populate_lexical_env
     (Unit : lkt_analysis_unit
   ) return int
      with Export        => True,
           Convention    => C,
           External_name => "lkt_unit_populate_lexical_env";
   --  Create lexical environments for this analysis unit, according to the
   --  specifications given in the language spec.
   --
   --  If not done before, it will be automatically called during semantic
   --  analysis. Calling it before enables one to control where the latency
   --  occurs.
   --
   --  Depending on whether errors are discarded (see
   --  ``Discard_Errors_In_Populate_Lexical_Env``), return ``0`` on failure and
   --  ``1`` on success.

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   procedure lkt_create_bare_entity
     (Node   : lkt_base_node;
      Entity : access lkt_node)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_create_bare_entity";
   --  Create an entity with null entity info for a given node.

   function lkt_is_equivalent
     (L, R : lkt_node_Ptr) return lkt_bool
      with Export        => True,
           Convention    => C,
           External_name => "lkt_node_is_equivalent";
   --  Return whether the two nodes are equivalent.

   function lkt_hash
     (Node : lkt_node_Ptr) return uint32_t
      with Export        => True,
           Convention    => C,
           External_name => "lkt_node_hash";
   --  Return a hash for the given node.

   function lkt_node_kind
     (Node : lkt_node_Ptr) return lkt_node_kind_enum
      with Export        => True,
           Convention    => C,
           External_name => "lkt_node_kind";
   --  Return the kind of this node.

   procedure lkt_kind_name
     (Kind : lkt_node_kind_enum; Result : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_kind_name";
   --  Helper for textual dump: return the kind name for this node. The
   --  returned string is a copy and thus must be free'd by the caller.

   function lkt_node_unit
     (Node : lkt_node_Ptr) return lkt_analysis_unit
      with Export => True,
           Convention => C,
           External_Name => "lkt_node_unit";
   --  Return the analysis unit that owns this node.

   function lkt_is_token_node
     (Node : lkt_node_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "lkt_node_is_token_node";
   --  Return whether this node is a node that contains only a single token.

   function lkt_is_synthetic
     (Node : lkt_node_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "lkt_node_is_synthetic";
   --  Return whether this node is synthetic.

   procedure lkt_node_image
     (Node : lkt_node_Ptr; Result : access lkt_text)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_node_image";
   --  Return a representation of this node as a string.

   procedure lkt_node_text
     (Node : lkt_node_Ptr;
      Text : access lkt_text)
      with Export, Convention => C,
           External_Name      => "lkt_node_text";
   --  Return the source buffer slice corresponding to the text that spans
   --  between the first and the last tokens of this node.
   --
   --  Note that this returns the empty string for synthetic nodes.

   procedure lkt_node_sloc_range
     (Node         : lkt_node_Ptr;
      Sloc_Range_P : access lkt_source_location_range)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_node_sloc_range";
   --  Return the spanning source location range for this node.
   --
   --  Note that this returns the sloc of the parent for synthetic nodes.

   procedure lkt_lookup_in_node
     (Node   : lkt_node_Ptr;
      Sloc   : lkt_source_location;
      Result : lkt_node_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_lookup_in_node";
   --  Return the bottom-most node from in ``Node`` and its children which
   --  contains ``Sloc``, or ``NULL`` if there is none.

   function lkt_node_children_count
     (Node : lkt_node_Ptr) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "lkt_node_children_count";
   --  Return the number of children in this node.

   function lkt_node_child
     (Node    : lkt_node_Ptr;
      N       : unsigned;
      Child_P : lkt_node_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "lkt_node_child";
   --  Return the Nth child for in this node's fields and store it into
   --  ``*child_p``.  Return zero on failure (when ``N`` is too big).

   function lkt_text_to_locale_string
     (Text : lkt_text) return System.Address
      with Export        => True,
           Convention    => C,
           External_name => "lkt_text_to_locale_string";
   --  Encode some text using the current locale. The result is dynamically
   --  allocated: it is up to the caller to free it when done with it.
   --
   --  This is a development helper to make it quick and easy to print token
   --  and diagnostic text: it ignores errors (when the locale does not support
   --  some characters). Production code should use real conversion routines
   --  such as libiconv's in order to deal with UTF-32 texts.

   procedure lkt_text_to_utf8
     (Text   : lkt_text;
      Bytes  : out chars_ptr;
      Length : out size_t)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_text_to_utf8";
   --  Encode some text to a newly allocated UTF-8 buffer (``bytes``). The size
   --  of this buffer is stored in ``length``, and the actual allocated buffer
   --  has one extra NUL byte (note that it is valid for the first ``length``
   --  bytes in ``bytes`` to contain NUL bytes).

   procedure lkt_text_from_utf8
     (Bytes  : chars_ptr;
      Length : size_t;
      Text   : out lkt_text)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_text_from_utf8";
   --  Decode a UTF-8 buffer (``bytes``, of size ``length``) to a text buffer.

   procedure lkt_char_to_utf8
     (Char   : Unsigned_32;
      Bytes  : out chars_ptr;
      Length : out size_t)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_char_to_utf8";
   --  Encode the given character to a newly allocated UTF-8 buffer
   --  (``bytes``). The size of this buffer is stored in ``length``.

   procedure lkt_char_from_utf8
     (Bytes  : chars_ptr;
      Length : size_t;
      Char   : out Unsigned_32)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_char_from_utf8";
   --  Decode a UTF-8 buffer (``bytes``, of size ``length``) to a text buffer.
   --  Note that the UTF-8 buffer is supposed to contain only one codepoint.

   procedure lkt_string_to_utf8
     (Str    : lkt_string_type;
      Bytes  : out chars_ptr;
      Length : out size_t)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_string_to_utf8";
   --  Encode some string to a newly allocated UTF-8 buffer (``bytes``). The
   --  size of this buffer is stored in ``length``, and the actual allocated
   --  buffer has one extra NUL byte (note that it is valid for the first
   --  ``length`` bytes in ``bytes`` to contain NUL bytes).

   procedure lkt_string_from_utf8
     (Bytes  : chars_ptr;
      Length : size_t;
      Str    : out lkt_string_type)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_string_from_utf8";
   --  Decode a UTF-8 buffer (``bytes``, of size ``length``) to a string
   --  buffer.

   ------------------
   -- File readers --
   ------------------

   function lkt_create_file_reader
     (Data         : System.Address;
      Destroy_Func : lkt_file_reader_destroy_callback;
      Read_Func    : lkt_file_reader_read_callback) return lkt_file_reader
      with Export        => True,
           Convention    => C,
           External_name => "lkt_create_file_reader";
   --  Create a file reader. When done with it, the result must be passed to
   --  ``lkt_dec_ref_file_reader``.
   --
   --  Pass as ``data`` a pointer to hold your private data: it will be passed
   --  to all callbacks below.
   --
   --  ``destroy`` is a callback that is called by ``lkt_dec_ref_file_reader``
   --  to leave a chance to free resources that ``data`` may hold.
   --
   --  ``read`` is a callback. For a given filename/charset and whether to read
   --  the BOM (Byte Order Mark), it tries to fetch the contents of the source
   --  file, returned in ``Contents``. If there is an error, it must return it
   --  in ``Diagnostic`` instead.

   procedure lkt_dec_ref_file_reader
     (File_Reader : lkt_file_reader)
      with Export        => True,
           Convention    => C,
           External_name =>
              "lkt_dec_ref_file_reader";
   --  Release an ownership share for this file reader. This destroys the file
   --  reader if there are no shares left.

   


   --------------------
   -- Event handlers --
   --------------------

   function lkt_create_event_handler
     (Data                : System.Address;
      Destroy_Func        : lkt_event_handler_destroy_callback;
      Unit_Requested_Func : lkt_event_handler_unit_requested_callback;
      Unit_Parsed_Func    : lkt_event_handler_unit_parsed_callback)
      return lkt_event_handler
      with Export        => True,
           Convention    => C,
           External_name => "lkt_create_event_handler";
   --  Create an event handler. When done with it, the result must be passed to
   --  ``lkt_dec_ref_event_handler``.
   --
   --  Pass as ``data`` a pointer to hold your private data: it will be passed
   --  to all callbacks below.
   --
   --  ``destroy`` is a callback that is called by
   --  ``lkt_dec_ref_event_handler`` to leave a chance to free resources that
   --  ``data`` may hold. ``NULL`` can be passed if nothing needs to be done.
   --
   --  ``unit_requested`` is a callback that will be called when a unit is
   --  requested.
   --
   --  .. warning:: Please note that the unit requested callback can be called
   --     *many* times for the same unit, so in all likeliness, those events
   --     should be filtered if they're used to forward diagnostics to the
   --     user.
   --
   --  ``unit_parsed`` is a callback that will be called when a unit is parsed.

   procedure lkt_dec_ref_event_handler
     (Handler : lkt_event_handler)
      with Export        => True,
           Convention    => C,
           External_name =>
              "lkt_dec_ref_event_handler";
   --  Release an ownership share for this event handler. This destroys the
   --  event handler if there are no shares left.

   


   --------------------
   -- Unit providers --
   --------------------

   procedure lkt_dec_ref_unit_provider
     (Provider : lkt_unit_provider)
      with Export        => True,
           Convention    => C,
           External_name =>
              "lkt_dec_ref_unit_provider";
   --  Release an ownership share for this unit provider. This destroys the
   --  unit provider if there are no shares left.

   


   ------------------
   -- Struct types --
   ------------------

         



subtype lkt_internal_decoded_char_value is Internal_Decoded_Char_Value;
type lkt_internal_decoded_char_value_Ptr is access Internal_Decoded_Char_Value;

procedure lkt_internal_decoded_char_value_inc_ref (R : lkt_internal_decoded_char_value_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_decoded_char_value_inc_ref";
procedure lkt_internal_decoded_char_value_dec_ref (R : lkt_internal_decoded_char_value_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_decoded_char_value_dec_ref";


         



subtype lkt_internal_decoded_string_value is Internal_Decoded_String_Value;
type lkt_internal_decoded_string_value_Ptr is access Internal_Decoded_String_Value;

procedure lkt_internal_decoded_string_value_inc_ref (R : lkt_internal_decoded_string_value_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_decoded_string_value_inc_ref";
procedure lkt_internal_decoded_string_value_dec_ref (R : lkt_internal_decoded_string_value_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_decoded_string_value_dec_ref";


         



subtype lkt_internal_logic_context is Internal_Logic_Context;
type lkt_internal_logic_context_Ptr is access Internal_Logic_Context;



         



subtype lkt_internal_solver_diagnostic is Internal_Solver_Diagnostic;
type lkt_internal_solver_diagnostic_Ptr is access Internal_Solver_Diagnostic;

procedure lkt_internal_solver_diagnostic_inc_ref (R : lkt_internal_solver_diagnostic_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_solver_diagnostic_inc_ref";
procedure lkt_internal_solver_diagnostic_dec_ref (R : lkt_internal_solver_diagnostic_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_solver_diagnostic_dec_ref";


         



subtype lkt_internal_solver_result is Internal_Solver_Result;
type lkt_internal_solver_result_Ptr is access Internal_Solver_Result;

procedure lkt_internal_solver_result_inc_ref (R : lkt_internal_solver_result_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_solver_result_inc_ref";
procedure lkt_internal_solver_result_dec_ref (R : lkt_internal_solver_result_Ptr)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_solver_result_dec_ref";



   -----------------
   -- Array types --
   -----------------

         



subtype lkt_node_array is Internal_Entity_Array_Access;
type lkt_node_array_Ptr is access Internal_Entity_Array_Access;

function lkt_node_array_create (Length : int) return Internal_Entity_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "lkt_node_array_create";

procedure lkt_node_array_inc_ref (A : Internal_Entity_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_node_array_inc_ref";

procedure lkt_node_array_dec_ref (A : Internal_Entity_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_node_array_dec_ref";


         



subtype lkt_internal_logic_context_array is Internal_Logic_Context_Array_Access;
type lkt_internal_logic_context_array_Ptr is access Internal_Logic_Context_Array_Access;

function lkt_internal_logic_context_array_create (Length : int) return Internal_Logic_Context_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_logic_context_array_create";

procedure lkt_internal_logic_context_array_inc_ref (A : Internal_Logic_Context_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_logic_context_array_inc_ref";

procedure lkt_internal_logic_context_array_dec_ref (A : Internal_Logic_Context_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_logic_context_array_dec_ref";


         



subtype lkt_internal_solver_diagnostic_array is Internal_Solver_Diagnostic_Array_Access;
type lkt_internal_solver_diagnostic_array_Ptr is access Internal_Solver_Diagnostic_Array_Access;

function lkt_internal_solver_diagnostic_array_create (Length : int) return Internal_Solver_Diagnostic_Array_Access
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_solver_diagnostic_array_create";

procedure lkt_internal_solver_diagnostic_array_inc_ref (A : Internal_Solver_Diagnostic_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_solver_diagnostic_array_inc_ref";

procedure lkt_internal_solver_diagnostic_array_dec_ref (A : Internal_Solver_Diagnostic_Array_Access)
   with Export        => True,
        Convention    => C,
        External_name => "lkt_internal_solver_diagnostic_array_dec_ref";



   --------------------
   -- Iterator types --
   --------------------


   ------------------
   -- Stack traces --
   ------------------

   function lkt_stack_trace_size
     (Stack_Trace : lkt_stack_trace) return int
      with Export, Convention => C;
   --  Return the number of entries in the given stack trace.

   function lkt_stack_trace_element
     (Stack_Trace : lkt_stack_trace; Index : int) return System.Address
      with Export, Convention => C;
   --  Return the stack trace item at the given index. The given index must be
   --  non-negative and lower than the stack trace size.

   function lkt_create_stack_trace
     (Size : int; Elements : System.Address) return lkt_stack_trace
      with Export, Convention => C;
   --  Allocate and return a stack trace for the given entries.
   --
   --  The result must be deallocated with the ``destroy_stack_trace`` function
   --  when done with it.

   procedure lkt_destroy_stack_trace
     (Stack_Trace : lkt_stack_trace) with Export, Convention => C;
   --  Deallocate a stack trace that was created with ``create_stack_trace``.

   function lkt_symbolize_stack_trace
     (Stack_Trace : lkt_stack_trace) return chars_ptr
      with Export, Convention => C;
   --  Convert a stack trace to a multi-line human readable trace.
   --
   --  The returned string is dynamically allocated and the caller must free it
   --  when done with it.

   ----------
   -- Misc --
   ----------

   function lkt_get_last_exception return lkt_exception_Ptr
     with Export        => True,
          Convention    => C,
          External_Name => "lkt_get_last_exception";
   --  Return exception information for the last error that happened in the
   --  current thread. Will be automatically allocated on error and free'd on
   --  the next error.

   function lkt_exception_name
     (Kind : lkt_exception_kind) return chars_ptr
      with Export, Convention => C;
   --  Return the name of the given exception kind. Callers are responsible for
   --  free'ing the result.

   procedure Clear_Last_Exception;
   --  Free the information contained in Last_Exception

   procedure Set_Last_Exception (Exc : Exception_Occurrence);
   --  Free the information contained in Last_Exception and replace it with
   --  newly allocated information from Exc.

   procedure Set_Last_Exception
     (Id          : Exception_Id;
      Message     : String;
      Stack_Trace : GNAT.Traceback.Tracebacks_Array);
   --  Likewise, but using exception information as independent components.
   --  This is useful to pass messages that are longer than what the Ada
   --  runtime accepts (i.e. allows to avoid truncated error messages).

   function lkt_token_get_kind
     (Token : lkt_token) return int
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_token_get_kind";
   --  Kind for this token.

   function lkt_token_kind_name (Kind : int) return chars_ptr
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_token_kind_name";
   --  Return a human-readable name for a token kind.
   --
   --  The returned string is dynamically allocated and the caller must free it
   --  when done with it.
   --
   --  If the given kind is invalid, return ``NULL`` and set the last exception
   --  accordingly.

   procedure lkt_token_sloc_range
     (Token : lkt_token; Result : access lkt_source_location_range)
      with Export        => True,
           Convention    => C,
           External_Name => "lkt_token_sloc_range";
   --  Return the source location range of the given token.

   procedure lkt_token_next
     (Token      : lkt_token;
      Next_Token : access lkt_token)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_token_next";
   --  Return a reference to the next token in the corresponding analysis unit.

   procedure lkt_token_previous
     (Token          : lkt_token;
      Previous_Token : access lkt_token)
      with Export        => True,
           Convention    => C,
           External_name => "lkt_token_previous";
   --  Return a reference to the previous token in the corresponding analysis
   --  unit.

   function lkt_token_range_text
     (First, Last : lkt_token;
      Text        : access lkt_text) return int
      with Export => True,
           Convention => C,
           External_Name => "lkt_token_range_text";
   --  Compute the source buffer slice corresponding to the text that spans
   --  between the ``First`` and ``Last`` tokens (both included). This yields
   --  an empty slice if ``Last`` actually appears before ``First``. Put the
   --  result in ``RESULT``.
   --
   --  This returns ``0`` if ``First`` and ``Last`` don't belong to the same
   --  analysis unit. Return ``1`` if successful.

   function lkt_token_is_equivalent
     (Left  : lkt_token;
      Right : lkt_token) return lkt_bool
      with Export        => True,
           Convention    => C,
           External_name => "lkt_token_is_equivalent";
   --  Return whether ``L`` and ``R`` are structurally equivalent tokens. This
   --  means that their position in the stream won't be taken into account,
   --  only the kind and text of the token.

   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   --  All these primitives return their result through an OUT parameter. They
   --  return a boolean telling whether the operation was successful (it can
   --  fail if the node does not have the proper type, for instance). When an
   --  AST node is returned, its ref-count is left as-is.

           
   

   
   

   function lkt_lkt_node_parent
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_parent";
   --  Return the syntactic parent for this node. Return null for the root
   --  node.

           
   

   
   

   function lkt_lkt_node_parents
     (Node : lkt_node_Ptr;

         With_Self :
            
            lkt_bool;

      Value_P : access lkt_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_parents";
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.

           
   

   
   

   function lkt_lkt_node_children
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_children";
   --  Return an array that contains the direct lexical children.
   --
   --  .. warning:: This constructs a whole array every-time you call it, and
   --     as such is less efficient than calling the ``Child`` built-in.

           
   

   
   

   function lkt_lkt_node_token_start
     (Node : lkt_node_Ptr;


      Value_P : access lkt_token) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_token_start";
   --  Return the first token used to parse this node.

           
   

   
   

   function lkt_lkt_node_token_end
     (Node : lkt_node_Ptr;


      Value_P : access lkt_token) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_token_end";
   --  Return the last token used to parse this node.

           
   

   
   

   function lkt_lkt_node_child_index
     (Node : lkt_node_Ptr;


      Value_P : access int) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_child_index";
   --  Return the 0-based index for Node in its parent's children.

           
   

   
   

   function lkt_lkt_node_previous_sibling
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_previous_sibling";
   --  Return the node's previous sibling, or null if there is no such sibling.

           
   

   
   

   function lkt_lkt_node_next_sibling
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_next_sibling";
   --  Return the node's next sibling, or null if there is no such sibling.

           
   

   
   

   function lkt_lkt_node_unit
     (Node : lkt_node_Ptr;


      Value_P : access lkt_analysis_unit) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_unit";
   --  Return the analysis unit owning this node.

           
   

   
   

   function lkt_lkt_node_is_ghost
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_is_ghost";
   --  Return whether the node is a ghost.
   --
   --  Unlike regular nodes, ghost nodes cover no token in the input source:
   --  they are logically located instead between two tokens. Both the
   --  ``token_start`` and the ``token_end`` of all ghost nodes is the token
   --  right after this logical position.

           
   

   
   

   function lkt_lkt_node_full_sloc_image
     (Node : lkt_node_Ptr;


      Value_P : access lkt_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_full_sloc_image";
   --  Return a string containing the filename + the sloc in GNU conformant
   --  format. Useful to create diagnostics from a node.

           
   

   
   

   function lkt_lkt_node_completion_item_kind_to_int
     (Node : lkt_node_Ptr;

         Kind :
            
            lkt_completion_item_kind;

      Value_P : access int) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_completion_item_kind_to_int";
   --  Convert a CompletionItemKind enum to its corresponding integer value.

           
   

   
   

   function lkt_lkt_node_p_set_solver_debug_mode
     (Node : lkt_node_Ptr;

         Enable :
            
            lkt_bool;

      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_set_solver_debug_mode";
   --  Enable or disable the solver traces for debugging purposes.

           
   

   
   

   function lkt_lkt_node_p_basic_trait_gen
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_basic_trait_gen";
   --  Unit method. Return the ``BasicTrait`` builtin generic trait.

           
   

   
   

   function lkt_lkt_node_p_basic_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_basic_trait";
   --  Unit method. Return the ``BasicTrait`` builtin trait.

           
   

   
   

   function lkt_lkt_node_p_node_gen_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_node_gen_trait";
   --  Unit method. Return the ``Node`` builtin generic trait.

           
   

   
   

   function lkt_lkt_node_p_node_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_node_trait";
   --  Unit method. Return the ``Node`` builtin trait.

           
   

   
   

   function lkt_lkt_node_p_indexable_gen_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_indexable_gen_trait";
   --  Unit method. Return the ``Node`` builtin generic trait.

           
   

   
   

   function lkt_lkt_node_p_indexable_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_indexable_trait";
   --  Unit method. Return the ``Node`` builtin trait.

           
   

   
   

   function lkt_lkt_node_p_token_node_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_token_node_trait";
   --  Unit method. Return the ``TokenNode`` builtin trait.

           
   

   
   

   function lkt_lkt_node_p_error_node_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_error_node_trait";
   --  Unit method. Return the ``ErrorNode`` builtin trait.

           
   

   
   

   function lkt_lkt_node_p_char_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_char_type";
   --  Unit method. Return the character builtin type.

           
   

   
   

   function lkt_lkt_node_p_int_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_int_type";
   --  Unit method. Return the integer builtin type.

           
   

   
   

   function lkt_lkt_node_p_bool_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_bool_type";
   --  Unit method. Return the boolean builtin type.

           
   

   
   

   function lkt_lkt_node_p_bigint_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_bigint_type";
   --  Unit method. Return the big integer builtin type.

           
   

   
   

   function lkt_lkt_node_p_string_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_string_type";
   --  Unit method. Return the string builtin type.

           
   

   
   

   function lkt_lkt_node_p_symbol_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_symbol_type";
   --  Unit method. Return the string builtin type.

           
   

   
   

   function lkt_lkt_node_p_property_error_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_property_error_type";
   --  Unit method. Return the property error builtin type.

           
   

   
   

   function lkt_lkt_node_p_regexp_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_regexp_type";
   --  Unit method. Return the regexp builtin type.

           
   

   
   

   function lkt_lkt_node_p_entity_gen_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_entity_gen_type";
   --  Unit method. Return the logicvar builtin type.

           
   

   
   

   function lkt_lkt_node_p_entity_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_entity_type";
   --  Unit method. Return the logicvar builtin type.

           
   

   
   

   function lkt_lkt_node_p_logicvar_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_logicvar_type";
   --  Unit method. Return the logicvar builtin type.

           
   

   
   

   function lkt_lkt_node_p_equation_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_equation_type";
   --  Unit method. Return the logicvar builtin type.

           
   

   
   

   function lkt_lkt_node_p_array_gen_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_array_gen_type";
   --  Unit method. Return the array builtin generic type.

           
   

   
   

   function lkt_lkt_node_p_array_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_array_type";
   --  Unit method. Return the array builtin type.

           
   

   
   

   function lkt_lkt_node_p_astlist_gen_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_astlist_gen_type";
   --  Unit method. Return the ASTList builtin generic type.

           
   

   
   

   function lkt_lkt_node_p_astlist_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_astlist_type";
   --  Unit method. Return the ASTList builtin type.

           
   

   
   

   function lkt_lkt_node_p_node_builder_gen_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_node_builder_gen_type";
   --  Unit method. Return the NodeBuilder builtin generic type.

           
   

   
   

   function lkt_lkt_node_p_node_builder_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_node_builder_type";
   --  Unit method. Return the NodeBuilder builtin type.

           
   

   
   

   function lkt_lkt_node_p_iterator_gen_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_iterator_gen_trait";
   --  Unit method. Return the Iterator builtin generic trait.

           
   

   
   

   function lkt_lkt_node_p_iterator_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_iterator_trait";
   --  Unit method. Return the Iterator builtin trait.

           
   

   
   

   function lkt_lkt_node_p_analysis_unit_gen_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_analysis_unit_gen_trait";
   --  Unit method. Return the ``AnalysisUnit`` builtin generic trait.

           
   

   
   

   function lkt_lkt_node_p_analysis_unit_trait
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_analysis_unit_trait";
   --  Unit method. Return the ``AnalysisUnit`` builtin trait.

           
   

   
   

   function lkt_lkt_node_p_topmost_invalid_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_topmost_invalid_decl";
   --  Return the topmost (from ``Self`` to the root node) FullDecl annotated
   --  with ``@invalid``, null otherwise.

           
   

   
   

   function lkt_lkt_node_p_nameres_diagnostics
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_solver_diagnostic_array) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_nameres_diagnostics";
   --  If name resolution on this lkt compilation unit fails, this returns all
   --  the diagnostics that were produced while resolving it.

           
   

   
   

   function lkt_lkt_node_p_solve_enclosing_context
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_solver_result) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_solve_enclosing_context";
   --  Finds the nearest parent that is an xref_entry_point and solve its
   --  equation.

           
   

   
   

   function lkt_lkt_node_p_xref_entry_point
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lkt_node_p_xref_entry_point";
   --  Designates entities that are entry point for the xref solving
   --  infrastructure. If this returns true, then nameres_diagnostics can be
   --  called on it.

           
   

   
   

   function lkt_argument_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_argument_f_name";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_argument_f_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_argument_f_value";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_base_lexer_case_rule_alt_f_send
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_base_lexer_case_rule_alt_f_send";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_lexer_case_rule_cond_alt_f_cond_exprs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lexer_case_rule_cond_alt_f_cond_exprs";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_base_match_branch_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_base_match_branch_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_base_match_branch_p_match_part
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_base_match_branch_p_match_part";
   --  Return the "match" part of the branch, either a pattern branch or a
   --  legacy match branch with variable declaration.

           
   

   
   

   function lkt_match_branch_f_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_match_branch_f_decl";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_pattern_match_branch_f_pattern
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_pattern_match_branch_f_pattern";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Filtered_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_block_expr_clause_f_clause
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_block_expr_clause_f_clause";
   --  This field can contain one of the following nodes: :ada:ref:`Val_Decl`,
   --  :ada:ref:`Var_Bind`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_class_qualifier_p_as_bool
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_class_qualifier_p_as_bool";
   --  Return whether this node is present

           
   

   
   

   function lkt_decl_f_syn_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_f_syn_name";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_decl_p_custom_image
     (Node : lkt_node_Ptr;


      Value_P : access lkt_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_custom_image";
   --  Return the image string using entity information.

           
   

   
   

   function lkt_decl_p_decl_type_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_decl_type_name";
   --  Return the name of the declaration type, as it should be seen by
   --  users/shown in diagnostics.

           
   

   
   

   function lkt_decl_p_as_bare_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_as_bare_decl";
   --  Get this declaration without rebindings information.

           
   

   
   

   function lkt_decl_p_get_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_get_type";
   --  Return the type of the Decl.

           
   

   
   

   function lkt_decl_p_get_cast_type
     (Node : lkt_node_Ptr;

         Cast_To :
            access constant
            lkt_node;

      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_get_cast_type";
   --  If we are casting an entity (Self) to something that is not an entity,
   --  make it an entity.

           
   

   
   

   function lkt_decl_p_get_keep_type
     (Node : lkt_node_Ptr;

         Keep_Type :
            access constant
            lkt_node;

      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_get_keep_type";
   --  Return the type of Entity when we only keep elements of type keep_type.
   --  If we are casting an entity (Self) to something that is not an entity,
   --  make it an entity.

           
   

   
   

   function lkt_decl_p_get_suffix_type
     (Node : lkt_node_Ptr;

         Prefix_Type :
            access constant
            lkt_node;

      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_get_suffix_type";
   --  If we are accessing a ParseField of an entity, then that field's type
   --  also needs to be an entity.

           
   

   
   

   function lkt_decl_p_is_generic
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_is_generic";
   --  Returns whether the Decl is generic.

           
   

   
   

   function lkt_decl_p_return_type_is_instantiated
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_return_type_is_instantiated";
   --  Return True if the return type of this function is instantiated.

           
   

   
   

   function lkt_decl_p_is_instantiated
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_is_instantiated";
   --  Return True if Self is an instantiated declaration, meaning that it does
   --  not use any of its declared generic types.

           
   

   
   

   function lkt_decl_p_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_symbol_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_name";
   --  Return the symbol corresponding to the name of this declaration.

           
   

   
   

   function lkt_decl_p_full_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_p_full_name";
   --  Return the full name of this decl, as it should be seen by users/shown
   --  in diagnostics.

           
   

   
   

   function lkt_base_grammar_rule_decl_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_base_grammar_rule_decl_f_expr";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_explicitly_typed_decl_f_decl_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_explicitly_typed_decl_f_decl_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_component_decl_f_default_val
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_component_decl_f_default_val";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_field_decl_f_trait_ref
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_field_decl_f_trait_ref";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_fun_param_decl_f_decl_annotations
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_fun_param_decl_f_decl_annotations";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_val_decl_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_val_decl_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_fun_decl_f_params
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_fun_decl_f_params";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_fun_decl_f_return_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_fun_decl_f_return_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_fun_decl_f_trait_ref
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_fun_decl_f_trait_ref";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_fun_decl_f_body
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_fun_decl_f_body";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_fun_decl_p_is_dynamic_combiner
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_fun_decl_p_is_dynamic_combiner";
   --  When this property is used as a a combinder inside an NPropagate
   --  equation, return whether it expects a dynamic number of arguments.

           
   

   
   

   function lkt_env_spec_decl_f_actions
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_env_spec_decl_f_actions";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_generic_decl_f_generic_param_decls
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_generic_decl_f_generic_param_decls";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_generic_decl_f_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_generic_decl_f_decl";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Dyn_Var_Decl`, :ada:ref:`Env_Spec_Decl`,
   --  :ada:ref:`Field_Decl`, :ada:ref:`Fun_Decl`, :ada:ref:`Generic_Decl`,
   --  :ada:ref:`Grammar_Decl`, :ada:ref:`Grammar_Rule_Decl`,
   --  :ada:ref:`Lexer_Decl`, :ada:ref:`Named_Type_Decl`, :ada:ref:`Val_Decl`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_decl_f_rules
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_decl_f_rules";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_lexer_decl_f_rules
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lexer_decl_f_rules";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Full_Decl`, :ada:ref:`Lexer_Case_Rule`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_lexer_family_decl_f_rules
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lexer_family_decl_f_rules";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_type_decl_f_traits
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_type_decl_f_traits";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_type_decl_f_syn_base_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_type_decl_f_syn_base_type";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_type_decl_p_base_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_type_decl_p_base_type";
   --  Return the base type for this node, if any.

           
   

   
   

   function lkt_type_decl_p_base_type_if_entity
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_type_decl_p_base_type_if_entity";
   --  Return the base type for this node, if any.

           
   

   
   

   function lkt_generic_param_type_decl_f_has_class
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_generic_param_type_decl_f_has_class";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_named_type_decl_f_decls
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_named_type_decl_f_decls";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_enum_class_decl_f_branches
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_enum_class_decl_f_branches";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_enum_type_decl_f_literals
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_enum_type_decl_f_literals";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_decl_annotation_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_annotation_f_name";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_decl_annotation_f_args
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_annotation_f_args";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_decl_annotation_args_f_args
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_decl_annotation_args_f_args";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_elsif_branch_f_cond_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_elsif_branch_f_cond_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_elsif_branch_f_then_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_elsif_branch_f_then_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_enum_class_case_f_decls
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_enum_class_case_f_decls";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_excludes_null_p_as_bool
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_excludes_null_p_as_bool";
   --  Return whether this node is present

           
   

   
   

   function lkt_expr_p_get_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_expr_p_get_type";
   --  Return the type of this expression.

           
   

   
   

   function lkt_expr_p_get_generic_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_expr_p_get_generic_type";
   --  Return the expected type of this expression.

           
   

   
   

   function lkt_expr_p_get_expected_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_expr_p_get_expected_type";
   --  Return the expected type of this expression.

           
   

   
   

   function lkt_expr_p_referenced_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_expr_p_referenced_decl";
   --  Return the declaration referenced by this expression, if applicable,
   --  else null.
   --
   --  The property is memoized in order to avoid use the value inside logic
   --  variables on every redundent call, causing faulty behavior when used
   --  with rebindings. TODO: Do like LAL to avoid memoization for more safety.

           
   

   
   

   function lkt_any_of_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_any_of_f_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`If_Expr`,
   --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`, :ada:ref:`Try_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_any_of_f_values
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_any_of_f_values";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`,
   --  :ada:ref:`Lit`, :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`, :ada:ref:`Try_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_array_literal_f_exprs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_array_literal_f_exprs";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Any_Of`, :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`,
   --  :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`If_Expr`, :ada:ref:`Isa`,
   --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Assign`, :ada:ref:`Logic_Expr`,
   --  :ada:ref:`Logic_Predicate`, :ada:ref:`Logic_Propagate`,
   --  :ada:ref:`Logic_Unify`, :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`, :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_array_literal_f_element_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_array_literal_f_element_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_base_call_expr_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_base_call_expr_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_base_call_expr_f_args
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_base_call_expr_f_args";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_bin_op_f_left
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_bin_op_f_left";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_bin_op_f_op
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_bin_op_f_op";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_bin_op_f_right
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_bin_op_f_right";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_block_expr_f_clauses
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_block_expr_f_clauses";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Any_Of`, :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`,
   --  :ada:ref:`Block_Expr_Clause`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_Decl`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`If_Expr`, :ada:ref:`Isa`,
   --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Assign`, :ada:ref:`Logic_Expr`,
   --  :ada:ref:`Logic_Predicate`, :ada:ref:`Logic_Propagate`,
   --  :ada:ref:`Logic_Unify`, :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`,
   --  :ada:ref:`Paren_Expr`, :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`, :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_cast_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_cast_expr_f_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_cast_expr_f_null_cond
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_cast_expr_f_null_cond";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_cast_expr_f_excludes_null
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_cast_expr_f_excludes_null";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_cast_expr_f_dest_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_cast_expr_f_dest_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_dot_expr_f_prefix
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_dot_expr_f_prefix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_dot_expr_f_null_cond
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_dot_expr_f_null_cond";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_dot_expr_f_suffix
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_dot_expr_f_suffix";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_error_on_null_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_error_on_null_f_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_generic_instantiation_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_generic_instantiation_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_generic_instantiation_f_args
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_generic_instantiation_f_args";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_discard_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_discard_f_expr";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_dont_skip_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_dont_skip_f_expr";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_dont_skip_f_dont_skip
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_dont_skip_f_dont_skip";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_list_f_list_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_list_f_list_type";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_list_f_kind
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_list_f_kind";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_list_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_list_f_expr";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_list_f_sep
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_list_f_sep";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_grammar_null_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_null_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_opt_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_opt_f_expr";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_opt_error_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_opt_error_f_expr";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_opt_error_group_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_opt_error_group_f_expr";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_opt_group_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_opt_group_f_expr";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_or_expr_f_sub_exprs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_or_expr_f_sub_exprs";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_pick_f_exprs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_pick_f_exprs";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_predicate_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_predicate_f_expr";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_predicate_f_prop_ref
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_predicate_f_prop_ref";
   --  This field can contain one of the following nodes: :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Ref_Id`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_rule_ref_f_node_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_rule_ref_f_node_name";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_skip_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_skip_f_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_stop_cut_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_stop_cut_f_expr";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_parse_node_expr_f_node_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_parse_node_expr_f_node_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_parse_node_expr_f_sub_exprs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_parse_node_expr_f_sub_exprs";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_token_lit_p_denoted_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_decoded_string_value) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_token_lit_p_denoted_value";
   --  Return the content of the given token literal node.

           
   

   
   

   function lkt_token_no_case_lit_f_lit
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_token_no_case_lit_f_lit";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_token_pattern_concat_f_left
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_token_pattern_concat_f_left";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Token_Pattern_Concat`, :ada:ref:`Token_Pattern_Lit`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_token_pattern_concat_f_right
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_token_pattern_concat_f_right";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_token_pattern_lit_p_denoted_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_decoded_string_value) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_token_pattern_lit_p_denoted_value";
   --  Return the content of the given token pattern literal node.

           
   

   
   

   function lkt_token_ref_f_token_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_token_ref_f_token_name";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_token_ref_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_token_ref_f_expr";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_id_p_custom_image
     (Node : lkt_node_Ptr;


      Value_P : access lkt_string_type) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_id_p_custom_image";
   --  Returns the image of this RefId using entity information.

           
   

   
   

   function lkt_if_expr_f_cond_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_if_expr_f_cond_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_if_expr_f_then_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_if_expr_f_then_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_if_expr_f_alternatives
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_if_expr_f_alternatives";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_if_expr_f_else_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_if_expr_f_else_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_isa_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_isa_f_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`If_Expr`,
   --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`, :ada:ref:`Try_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_isa_f_pattern
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_isa_f_pattern";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Filtered_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_keep_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_keep_expr_f_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_keep_expr_f_null_cond
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_keep_expr_f_null_cond";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_keep_expr_f_keep_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_keep_expr_f_keep_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_lambda_expr_f_params
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lambda_expr_f_params";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_lambda_expr_f_return_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lambda_expr_f_return_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_lambda_expr_f_body
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lambda_expr_f_body";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_char_lit_p_denoted_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_decoded_char_value) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_char_lit_p_denoted_value";
   --  Return the content of the given character literal node.

           
   

   
   

   function lkt_null_lit_f_dest_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_null_lit_f_dest_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_string_lit_p_denoted_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_internal_decoded_string_value) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_string_lit_p_denoted_value";
   --  Return the content of the given string literal node.

           
   

   
   

   function lkt_string_lit_p_is_prefixed_string
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_string_lit_p_is_prefixed_string";
   --  Return whether this string is prefixed or not.

           
   

   
   

   function lkt_string_lit_p_prefix
     (Node : lkt_node_Ptr;


      Value_P : access uint32_t) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_string_lit_p_prefix";
   --  Return the prefix of this string, or the null character if there is no
   --  prefix.

           
   

   
   

   function lkt_string_lit_p_is_regexp_literal
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_string_lit_p_is_regexp_literal";
   --  Return whether this string literal is actually a regexp literal, by
   --  checking that this string is prefixed by 'p'.

           
   

   
   

   function lkt_block_string_lit_f_lines
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_block_string_lit_f_lines";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_logic_assign_f_dest_var
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_logic_assign_f_dest_var";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`If_Expr`, :ada:ref:`Isa`,
   --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Assign`, :ada:ref:`Logic_Expr`,
   --  :ada:ref:`Logic_Predicate`, :ada:ref:`Logic_Propagate`,
   --  :ada:ref:`Logic_Unify`, :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_logic_assign_f_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_logic_assign_f_value";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`If_Expr`,
   --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`, :ada:ref:`Try_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_logic_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_logic_expr_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Call_Expr`,
   --  :ada:ref:`Ref_Id`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_logic_propagate_f_dest_var
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_logic_propagate_f_dest_var";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`If_Expr`, :ada:ref:`Isa`,
   --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Assign`, :ada:ref:`Logic_Expr`,
   --  :ada:ref:`Logic_Predicate`, :ada:ref:`Logic_Propagate`,
   --  :ada:ref:`Logic_Unify`, :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_logic_propagate_f_call
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_logic_propagate_f_call";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_logic_unify_f_lhs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_logic_unify_f_lhs";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`If_Expr`, :ada:ref:`Isa`,
   --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Assign`, :ada:ref:`Logic_Expr`,
   --  :ada:ref:`Logic_Predicate`, :ada:ref:`Logic_Propagate`,
   --  :ada:ref:`Logic_Unify`, :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_logic_unify_f_rhs
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_logic_unify_f_rhs";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`If_Expr`,
   --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`, :ada:ref:`Try_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_match_expr_f_match_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_match_expr_f_match_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_match_expr_f_branches
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_match_expr_f_branches";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_not_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_not_expr_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Raise_Expr`,
   --  :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`, :ada:ref:`Try_Expr`,
   --  :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_paren_expr_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_paren_expr_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_raise_expr_f_dest_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_raise_expr_f_dest_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_raise_expr_f_except_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_raise_expr_f_except_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_subscript_expr_f_prefix
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_subscript_expr_f_prefix";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_subscript_expr_f_null_cond
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_subscript_expr_f_null_cond";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_subscript_expr_f_index
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_subscript_expr_f_index";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_try_expr_f_try_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_try_expr_f_try_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_try_expr_f_or_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_try_expr_f_or_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_un_op_f_op
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_un_op_f_op";
   --  This field can contain one of the following nodes: :ada:ref:`Op_Minus`,
   --  :ada:ref:`Op_Plus`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_un_op_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_un_op_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`If_Expr`, :ada:ref:`Isa`,
   --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Assign`, :ada:ref:`Logic_Expr`,
   --  :ada:ref:`Logic_Predicate`, :ada:ref:`Logic_Propagate`,
   --  :ada:ref:`Logic_Unify`, :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_full_decl_f_doc
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_full_decl_f_doc";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_full_decl_f_decl_annotations
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_full_decl_f_decl_annotations";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_full_decl_f_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_full_decl_f_decl";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Dyn_Var_Decl`, :ada:ref:`Env_Spec_Decl`,
   --  :ada:ref:`Field_Decl`, :ada:ref:`Fun_Decl`, :ada:ref:`Generic_Decl`,
   --  :ada:ref:`Generic_Param_Type_Decl`, :ada:ref:`Grammar_Decl`,
   --  :ada:ref:`Grammar_Rule_Decl`, :ada:ref:`Lexer_Decl`,
   --  :ada:ref:`Lexer_Family_Decl`, :ada:ref:`Named_Type_Decl`,
   --  :ada:ref:`Val_Decl`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_full_decl_p_has_annotation
     (Node : lkt_node_Ptr;

         Name :
            access constant
            lkt_symbol_type;

      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_full_decl_p_has_annotation";
   --  Return whether this node has an annotation with name ``name``.

           
   

   
   

   function lkt_grammar_list_sep_f_token
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_list_sep_f_token";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_grammar_list_sep_f_extra
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_grammar_list_sep_f_extra";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_import_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_import_f_name";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_import_p_referenced_unit
     (Node : lkt_node_Ptr;


      Value_P : access lkt_analysis_unit) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_import_p_referenced_unit";
   --  Return the unit that this import statements designates. Load it if
   --  needed.

           
   

   
   

   function lkt_langkit_root_f_imports
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_langkit_root_f_imports";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_langkit_root_f_decls
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_langkit_root_f_decls";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_langkit_root_p_fetch_prelude
     (Node : lkt_node_Ptr;


      Value_P : access lkt_analysis_unit) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_langkit_root_p_fetch_prelude";
   --  External property that will fetch the prelude unit, containing
   --  predefined types and values.

           
   

   
   

   function lkt_lexer_case_rule_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lexer_case_rule_f_expr";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Grammar_Cut`, :ada:ref:`Grammar_Discard`,
   --  :ada:ref:`Grammar_List`, :ada:ref:`Grammar_Null`,
   --  :ada:ref:`Grammar_Opt_Error_Group`, :ada:ref:`Grammar_Opt_Error`,
   --  :ada:ref:`Grammar_Opt_Group`, :ada:ref:`Grammar_Opt`,
   --  :ada:ref:`Grammar_Or_Expr`, :ada:ref:`Grammar_Pick`,
   --  :ada:ref:`Grammar_Rule_Ref`, :ada:ref:`Grammar_Skip`,
   --  :ada:ref:`Grammar_Stop_Cut`, :ada:ref:`Parse_Node_Expr`,
   --  :ada:ref:`Token_Lit`, :ada:ref:`Token_No_Case_Lit`,
   --  :ada:ref:`Token_Pattern_Concat`, :ada:ref:`Token_Pattern_Lit`,
   --  :ada:ref:`Token_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_lexer_case_rule_f_alts
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lexer_case_rule_f_alts";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_lexer_case_rule_send_f_sent
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lexer_case_rule_send_f_sent";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_lexer_case_rule_send_f_match_size
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_lexer_case_rule_send_f_match_size";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_null_cond_qualifier_p_as_bool
     (Node : lkt_node_Ptr;


      Value_P : access lkt_bool) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_null_cond_qualifier_p_as_bool";
   --  Return whether this node is present

           
   

   
   

   function lkt_binding_pattern_f_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_binding_pattern_f_decl";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_binding_pattern_f_sub_pattern
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_binding_pattern_f_sub_pattern";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Not_Pattern`,
   --  :ada:ref:`Null_Pattern`, :ada:ref:`Paren_Pattern`,
   --  :ada:ref:`Regex_Pattern`, :ada:ref:`Tuple_Pattern`,
   --  :ada:ref:`Type_Pattern`
   --
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_ellipsis_pattern_f_binding
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_ellipsis_pattern_f_binding";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_extended_pattern_f_sub_pattern
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_extended_pattern_f_sub_pattern";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Any_Type_Pattern`, :ada:ref:`Paren_Pattern`,
   --  :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_extended_pattern_f_details
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_extended_pattern_f_details";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_filtered_pattern_f_sub_pattern
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_filtered_pattern_f_sub_pattern";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Not_Pattern`,
   --  :ada:ref:`Null_Pattern`, :ada:ref:`Paren_Pattern`,
   --  :ada:ref:`Regex_Pattern`, :ada:ref:`Tuple_Pattern`,
   --  :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_filtered_pattern_f_predicate
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_filtered_pattern_f_predicate";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_list_pattern_f_sub_patterns
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_list_pattern_f_sub_patterns";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Ellipsis_Pattern`, :ada:ref:`Extended_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_not_pattern_f_sub_pattern
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_not_pattern_f_sub_pattern";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Not_Pattern`,
   --  :ada:ref:`Null_Pattern`, :ada:ref:`Paren_Pattern`,
   --  :ada:ref:`Regex_Pattern`, :ada:ref:`Tuple_Pattern`,
   --  :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_or_pattern_f_left_sub_pattern
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_or_pattern_f_left_sub_pattern";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Filtered_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_or_pattern_f_right_sub_pattern
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_or_pattern_f_right_sub_pattern";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Filtered_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_paren_pattern_f_sub_pattern
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_paren_pattern_f_sub_pattern";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Filtered_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_tuple_pattern_f_sub_patterns
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_tuple_pattern_f_sub_patterns";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Not_Pattern`,
   --  :ada:ref:`Null_Pattern`, :ada:ref:`Paren_Pattern`,
   --  :ada:ref:`Regex_Pattern`, :ada:ref:`Tuple_Pattern`,
   --  :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_type_pattern_f_type_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_type_pattern_f_type_name";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_field_pattern_detail_f_id
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_field_pattern_detail_f_id";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_field_pattern_detail_f_expected_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_field_pattern_detail_f_expected_value";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Filtered_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_property_pattern_detail_f_call
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_property_pattern_detail_f_call";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_property_pattern_detail_f_expected_value
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_property_pattern_detail_f_expected_value";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Filtered_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_selector_pattern_detail_f_call
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_selector_pattern_detail_f_call";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_selector_pattern_detail_f_sub_pattern
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_selector_pattern_detail_f_sub_pattern";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Extended_Pattern`, :ada:ref:`Filtered_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Type_Pattern`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_selector_call_f_quantifier
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_selector_call_f_quantifier";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_selector_call_f_binding
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_selector_call_f_binding";
   --  This field may be null even when there are no parsing errors.

           
   

   
   

   function lkt_selector_call_f_selector_call
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_selector_call_f_selector_call";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_type_ref_p_referenced_decl
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_type_ref_p_referenced_decl";
   --  Returns the referenced type declaration.

           
   

   
   

   function lkt_function_type_ref_f_param_types
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_function_type_ref_f_param_types";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_function_type_ref_f_return_type
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_function_type_ref_f_return_type";
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_generic_type_ref_f_type_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_generic_type_ref_f_type_name";
   --  This field can contain one of the following nodes: :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Ref_Id`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_generic_type_ref_f_args
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_generic_type_ref_f_args";
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_simple_type_ref_f_type_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_simple_type_ref_f_type_name";
   --  This field can contain one of the following nodes: :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Ref_Id`
   --
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_var_bind_f_name
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_var_bind_f_name";
   --  When there are no parsing errors, this field is never null.

           
   

   
   

   function lkt_var_bind_f_expr
     (Node : lkt_node_Ptr;


      Value_P : access lkt_node) return int

      with Export        => True,
           Convention    => C,
           External_name => "lkt_var_bind_f_expr";
   --  This field can contain one of the following nodes: :ada:ref:`Any_Of`,
   --  :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
   --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
   --  :ada:ref:`If_Expr`, :ada:ref:`Isa`, :ada:ref:`Keep_Expr`,
   --  :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`, :ada:ref:`Logic_Assign`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
   --  :ada:ref:`Raise_Expr`, :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`,
   --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`
   --
   --  When there are no parsing errors, this field is never null.


   ------------------------
   -- Conversion helpers --
   ------------------------

   --  The following conversion helpers are use by the various C bindings

   function Wrap (S : Source_Location) return lkt_source_location is
     ((Unsigned_32 (S.Line), Unsigned_16 (S.Column)));
   function Unwrap (S : lkt_source_location) return Source_Location is
     ((Line_Number (S.Line), Column_Number (S.Column)));

   function Wrap (S : Source_Location_Range) return lkt_source_location_range is
     ((Start_S => (Unsigned_32 (S.Start_Line), Unsigned_16 (S.Start_Column)),
       End_S   => (Unsigned_32 (S.End_Line),   Unsigned_16 (S.End_Column))));
   function Unwrap (S : lkt_source_location_range) return Source_Location_Range is
     ((Line_Number (S.Start_S.Line),
       Line_Number (S.End_S.Line),
       Column_Number (S.Start_S.Column),
       Column_Number (S.End_S.Column)));

   function Wrap (S : Unbounded_Wide_Wide_String) return lkt_text;

   function Wrap_Alloc (S : Text_Type) return lkt_text;
   function Wrap_Alloc (S : Unbounded_Wide_Wide_String) return lkt_text;
   function Wrap
     (S     : Text_Cst_Access;
      First : Positive;
      Last  : Natural) return lkt_text;

   function Wrap (T : Text_Cst_Access) return lkt_text is
     (if T = null
      then (Chars => System.Null_Address, Length => 0, Is_Allocated => 0)
      else (Chars => T.all'Address, Length => T.all'Length, Is_Allocated => 0));
   function Wrap (T : Text_Access) return lkt_text is
     (Wrap (Text_Cst_Access (T)));

   function Wrap_Big_Integer is new Ada.Unchecked_Conversion
     (Big_Integer_Type, lkt_big_integer);
   function Unwrap_Big_Integer is new Ada.Unchecked_Conversion
     (lkt_big_integer, Big_Integer_Type);

   --  Probably because the following conversions involve fat pointers, using
   --  the No_Strict_Aliasing pragma here has no effect. Silence the warning,
   --  since all read/writes for the pointed values are made through the "real"
   --  fat pointer (Symbol_Type) and not the fake one (lkt_symbol_type): strict
   --  aliasing issues should not happen.

   pragma Warnings (Off, "possible aliasing problem for type");
   function Wrap_Symbol is new Ada.Unchecked_Conversion
     (Symbol_Type, lkt_symbol_type);
   function Unwrap_Symbol is new Ada.Unchecked_Conversion
     (lkt_symbol_type, Symbol_Type);
   pragma Warnings (On, "possible aliasing problem for type");

   function Wrap is new Ada.Unchecked_Conversion
     (Bare_Lkt_Node, lkt_base_node);
   function Unwrap is new Ada.Unchecked_Conversion
     (lkt_base_node, Bare_Lkt_Node);

   function Wrap (Token : Token_Reference) return lkt_token;
   function Unwrap (Token : lkt_token) return Token_Reference;

   function Wrap_Private_File_Reader is new Ada.Unchecked_Conversion
     (Internal_File_Reader_Access, lkt_file_reader);
   function Unwrap_Private_File_Reader is new Ada.Unchecked_Conversion
     (lkt_file_reader, Internal_File_Reader_Access);

   function Wrap_Private_Event_Handler is new Ada.Unchecked_Conversion
     (Internal_Event_Handler_Access, lkt_event_handler);
   function Unwrap_Private_Event_Handler is new Ada.Unchecked_Conversion
     (lkt_event_handler, Internal_Event_Handler_Access);

   function Wrap_Private_Provider is new Ada.Unchecked_Conversion
     (Internal_Unit_Provider_Access, lkt_unit_provider);
   function Unwrap_Private_Provider is new Ada.Unchecked_Conversion
     (lkt_unit_provider, Internal_Unit_Provider_Access);

   function Convert is new Ada.Unchecked_Conversion
     (chars_ptr, System.Address);

         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Argument_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Argument_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Enum_Class_Alt_Decl_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Enum_Class_Alt_Decl_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Expr_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Expr_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Field_Decl_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Field_Decl_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Full_Decl_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Full_Decl_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Generic_Param_Type_Decl_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Generic_Param_Type_Decl_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Type_Decl_Array_Access, Internal_Entity_Array_Access);
         function Convert is new Ada.Unchecked_Conversion
           (Internal_Entity_Array_Access, Internal_Entity_Type_Decl_Array_Access);


end Liblktlang.Implementation.C;
