## vim: filetype=makoada

with System;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package ${_self.ada_api_settings.lib_name}.C is

## The following must not output extra line breaks, hence the odd layout
<%def name='ada_c_doc(entity, column=0, **kwargs)'><%
   # Generate an Ada comment to document an entity in the C binding
   kwargs['lang'] = 'c' %>${ada_doc(entity, column, **kwargs)}</%def>

   type ${analysis_context_type} is new System.Address;
   ${ada_c_doc('langkit.analysis_context_type', 3)}

   type ${analysis_unit_type} is new System.Address;
   ${ada_c_doc('langkit.analysis_unit_type', 3)}

   type ${node_type} is new System.Address;
   ${ada_c_doc('langkit.node_type', 3)}

   type ${node_kind_type} is new int;
   ${ada_c_doc('langkit.node_kind_type', 3)}

   type ${token_type} is new System.Address;
   ${ada_c_doc('langkit.node_kind_type', 3)}

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
   end record
     with Convention => C_Pass_By_Copy;
   ${ada_c_doc('langkit.text_type', 3)}

   type ${diagnostic_type} is record
      Sloc_Range : ${sloc_range_type};
      Message    : ${text_type};
      --  When the API returns a diagnostic, it is up to the caller to free the
      --  message string.
   end record
     with Convention => C;
   ${ada_c_doc('langkit.diagnostic_type', 3)}

   % for type_name in (node_type, token_type, sloc_type, sloc_range_type, diagnostic_type):
      type ${type_name}_Ptr is access ${type_name};
   % endfor

   type int_Ptr is access int;

   % for chunk in _self.c_astnode_field_types_ada.values():
       ${chunk}
   % endfor


   -------------------------
   -- Analysis primitives --
   -------------------------

   function ${capi.get_name("create_analysis_context")}
     (Charset : chars_ptr)
      return ${analysis_context_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("create_analysis_context")}";
   ${ada_c_doc('langkit.create_context', 3)}

   procedure ${capi.get_name("destroy_analysis_context")}
     (Context : ${analysis_context_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("destroy_analysis_context")}";
   ${ada_c_doc('langkit.destroy_context', 3)}

   function ${capi.get_name("get_analysis_unit_from_file")}
     (Context           : ${analysis_context_type};
      Filename, Charset : chars_ptr;
      Reparse           : int)
      return ${analysis_unit_type}
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name("get_analysis_unit_from_file")}";
   ${ada_c_doc('langkit.get_unit_from_file', 3)}

   function ${capi.get_name("get_analysis_unit_from_buffer")}
     (Context           : ${analysis_context_type};
      Filename, Charset : chars_ptr;
      Buffer            : chars_ptr;
      Buffer_Size       : size_t)
      return ${analysis_unit_type}
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name("get_analysis_unit_from_buffer")}";
   ${ada_c_doc('langkit.get_unit_from_buffer', 3)}

   function ${capi.get_name("remove_analysis_unit")}
     (Context  : ${analysis_context_type};
      Filename : chars_ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("remove_analysis_unit")}";
   ${ada_c_doc('langkit.remove_unit', 3)}

   function ${capi.get_name("unit_root")} (Unit : ${analysis_unit_type})
                                           return ${node_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_root")}";
   ${ada_c_doc('langkit.unit_root', 3)}

   function ${capi.get_name("unit_diagnostic_count")}
     (Unit : ${analysis_unit_type}) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_diagnostic_count")}";
   ${ada_c_doc('langkit.unit_diagnostic_count', 3)}

   function ${capi.get_name("unit_diagnostic")}
     (Unit         : ${analysis_unit_type};
      N            : unsigned;
      Diagnostic_P : ${diagnostic_type}_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_diagnostic")}";
   ${ada_c_doc('langkit.unit_diagnostic', 3)}

   function ${capi.get_name("unit_incref")}
     (Unit : ${analysis_unit_type}) return ${analysis_unit_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_incref")}";
   ${ada_c_doc('langkit.unit_incref', 3)}

   procedure ${capi.get_name("unit_decref")} (Unit : ${analysis_unit_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_decref")}";
   ${ada_c_doc('langkit.unit_decref', 3)}

   procedure ${capi.get_name("unit_reparse_from_file")}
     (Unit : ${analysis_unit_type}; Charset : chars_ptr)
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_reparse_from_file")}";
   ${ada_c_doc('langkit.unit_reparse_file', 3)}

   procedure ${capi.get_name("unit_reparse_from_buffer")}
     (Unit        : ${analysis_unit_type};
      Charset     : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t)
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_reparse_from_buffer")}";
   ${ada_c_doc('langkit.unit_reparse_buffer', 3)}

   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   function ${capi.get_name("node_kind")} (Node : ${node_type})
      return ${node_kind_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_kind")}";
   ${ada_c_doc('langkit.node_kind', 3)}

   function ${capi.get_name("kind_name")} (Kind : ${node_kind_type})
                                           return ${text_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("kind_name")}";
   ${ada_c_doc('langkit.kind_name', 3)}

   procedure ${capi.get_name("node_sloc_range")}
     (Node         : ${node_type};
      Sloc_Range_P : ${sloc_range_type}_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_sloc_range")}";
   ${ada_c_doc('langkit.node_sloc_range', 3)}

   function ${capi.get_name("lookup_in_node")}
     (Node : ${node_type};
      Sloc : ${sloc_type}_Ptr) return ${node_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("lookup_in_node")}";
   ${ada_c_doc('langkit.lookup_in_node', 3)}

   function ${capi.get_name("node_parent")} (Node : ${node_type})
                                             return ${node_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_parent")}";
   ${ada_c_doc('langkit.node_parent', 3)}

   function ${capi.get_name("node_child_count")} (Node : ${node_type})
                                                  return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_child_count")}";
   ${ada_c_doc('langkit.node_child_count', 3)}

   function ${capi.get_name("node_child")}
     (Node    : ${node_type};
      N       : unsigned;
      Child_P : ${node_type}_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_child")}";
   ${ada_c_doc('langkit.node_child', 3)}

   function ${capi.get_name("token_text")} (Token : ${token_type})
                                            return ${text_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("token_text")}";
   ${ada_c_doc('langkit.token_text', 3)}

   function ${capi.get_name("text_to_locale_string")}
     (Text : ${text_type}) return System.Address
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("text_to_locale_string")}";
   ${ada_c_doc('langkit.text_to_locale_string', 3)}


   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   --  All these primitives return their result through an OUT parameter. They
   --  return a boolean telling whether the operation was successful (it can
   --  fail if the node does not have the proper type, for instance). When an
   --  AST node is returned, its ref-count is left as-is.

   % for astnode in _self.astnode_types:
       % for primitive in _self.c_astnode_primitives[astnode]:
           ${primitive.declaration}
       % endfor
   % endfor


   -------------------------
   -- Extensions handling --
   -------------------------

   ${ada_c_doc('langkit.extensions_handling', 3)}

   type ${capi.get_name("node_extension_destructor")} is
      access procedure (Node      : ${node_type};
                        Extension : System.Address)
      with Convention => C;
   ${ada_c_doc('langkit.node_extension_destructor', 3)}

   function ${capi.get_name("register_extension")} (Name : chars_ptr)
      return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("register_extension")}";
   ${ada_c_doc('langkit.register_extension', 3)}

   function ${capi.get_name("node_extension")}
     (Node   : ${node_type};
      Ext_Id : unsigned;
      Dtor   : ${capi.get_name("node_extension_destructor")})
      return System.Address
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_extension")}";
   ${ada_c_doc('langkit.node_extension', 3)}

end ${_self.ada_api_settings.lib_name}.C;
