<%namespace name="struct_types"  file="struct_types_ocaml.mako"/>
<%namespace name="astnode_types" file="astnode_types_ocaml.mako"/>
<%namespace name="exts"          file="/extensions.mako" />

<%
  root_entity_type = ocaml_api.type_public_name(root_entity)
%>
(**
 * OCaml binding of the ${ctx.lib_name.camel} API.
 *)

% for name, exc in ctx.sorted_exception_types:
${ocaml_doc(name, 0)}
exception ${exc} of string

% endfor

${exts.include_extension(
   ctx.ext('ocaml_api', 'exceptions')
)}

(**
 * Raised if a field of a node is null due to a syntax error
 *)
exception SyntaxError

% for enum_type in ctx.enum_types:
module ${ocaml_api.module_name(enum_type)} : sig
  ${ocaml_doc(enum_type, 1)}

  type t =
   % for v in enum_type.values:
  | ${v.name.camel}
   % endfor

  val name : unit -> string
end

% endfor
val default_grammar_rule : GrammarRule.t

module Sloc : sig
  ${ocaml_doc('langkit.sloc_type', 1)}

  type t = {
    line : int;
    column : int;
  }
end

module SlocRange : sig
  ${ocaml_doc('langkit.sloc_range_type', 1)}

  type t = {
    loc_start : Sloc.t;
    loc_end : Sloc.t;
  }

  val pp : Format.formatter -> t -> unit
end

module Diagnostic : sig
  ${ocaml_doc('langkit.diagnostic_type', 1)}

  type t = {
    sloc_range : SlocRange.t;
    message : string;
  }
end

module TokenData : sig
  type t
end

module Token : sig
  ${ocaml_doc('langkit.token_reference_type', 1)}

  type t = {
    token_data : TokenData.t;
    token_index : int;
    trivia_index : int;
    kind : int;
    text : string;
    sloc_range : SlocRange.t;
  }

  val kind_name : t -> string
  ${ocaml_doc('langkit.token_kind_name', 1)}

  val text_range : t -> t -> string
  ${ocaml_doc('langkit.token_range_text', 1)}

  val is_trivia : t -> bool
  ${ocaml_doc('langkit.token_is_trivia', 1)}

  val pp : Format.formatter -> t -> unit
end

module BigInteger : sig
  type t = int
end

module BareNode : sig
  type t
end

module Rebindings : sig
  type t
end

module UnitProvider : sig
  ${ocaml_doc('langkit.unit_provider_type', 1)}

  type t

  ${exts.include_extension(
     ctx.ext('ocaml_api', 'unit_providers', 'module_sig')
  )}
end

module rec Entity : sig
  type t

  val info : t -> ${ocaml_api.type_public_name(T.entity_info)}
end

% for astnode in ctx.astnode_types:
and ${ocaml_api.module_name(astnode)} : sig

  ${astnode_types.ast_type(astnode)}

end
% endfor

and AnalysisUnit : sig
  ${ocaml_doc('langkit.analysis_unit_type', 1)}

  type t

  val root : t -> ${root_entity_type} option
  ${ocaml_doc('langkit.unit_root', 1)}

  val diagnostics : t -> Diagnostic.t list
  (**
   * Diagnostics for this unit.
   *)
end

and AnalysisContext : sig
  ${ocaml_doc('langkit.analysis_context_type', 1)}

  type t

  val create :
    ?charset:string
    -> ?with_trivia:bool
    -> ?tab_stop:int
    -> ?unit_provider:UnitProvider.t
    -> unit
    -> t
  ${ocaml_doc('langkit.create_context', 1)}

  val get_from_file :
    ?charset:string
    -> ?reparse:bool
    -> ?grammar_rule:GrammarRule.t
    -> t
    -> string
    -> AnalysisUnit.t
  ${ocaml_doc('langkit.get_unit_from_file', 1)}

  val get_from_buffer :
    ?charset:string
    -> ?grammar_rule:GrammarRule.t
    -> t
    -> string
    -> string
    -> AnalysisUnit.t
  ${ocaml_doc('langkit.get_unit_from_buffer', 1)}
end

% for struct_type in ctx.struct_types:
   % if not struct_type.is_entity_type:
      % if struct_type is T.entity_info:
   ${struct_types.public_sig(struct_type)}
      % elif struct_type is T.env_md:
   ${struct_types.public_sig(struct_type)}
      % elif struct_type.exposed:
   ${struct_types.public_sig(struct_type)}
      % endif
   % endif
% endfor

type _ node =
% for astnode in ctx.astnode_types:
  | ${ocaml_api.node_name(astnode)} :
      ${ocaml_api.type_public_name(astnode)} node
% endfor

% for astnode in ctx.astnode_types:
module ${ocaml_api.node_name(astnode)} : sig
  ${ocaml_doc(astnode, 1)}

  type t = ${ocaml_api.type_public_name(astnode)}

   % if not astnode.abstract:
  type fields = ${ocaml_api.fields_name(astnode)}
   % endif

  val equal : [< t] -> [< t] -> bool

  val hash : [< t] -> int

  val compare : [< t] -> [< t] -> int

   % if astnode == T.root_node:
  val kind_name : [< ${root_entity_type}] -> string
  ${ocaml_doc('langkit.node_kind', 1)}

  val text : [< ${root_entity_type} ] -> string
  (**
   * Return the source buffer slice corresponding to the text that spans
   * between the first and the last tokens of the given node.
   *)

  val short_image : [< ${root_entity_type} ] -> string
  ${ocaml_doc('langkit.node_short_image', 1)}

  val sloc_range : [< ${root_entity_type} ] -> SlocRange.t
  ${ocaml_doc('langkit.node_sloc_range', 1)}

  val fold_tokens : ('a -> Token.t -> 'a) -> 'a -> [< ${root_entity_type} ] -> 'a
  (**
   * Fold all the token this node contains by calling f on each token.
   *)

  val iter_tokens : (Token.t -> unit) -> [< ${root_entity_type} ] -> unit
  (**
   * Iterate over all token this node contains by calling f on each token.
   *)

  val map_tokens : (Token.t -> 'a) -> [< ${root_entity_type} ] -> 'a list
  (**
   * Map each token calling the given function
   *)

  val children_opt : [< ${root_entity_type} ] -> ${root_entity_type} option list
  (**
   * Return an optional list of nodes which are the children of the given node.
   * Each child is optional because it can either be because of a syntax error,
   * or an optional field evaluated to null.
   *)

  val fold_fields :
    ('a -> ${root_entity_type} -> 'a) -> 'a -> [< ${root_entity_type} ] -> 'a
  (**
   * Fold all fields of the given node. This skips any child that is None
   * because of a syntax error or because the field is optional
   *)

  val iter_fields :
    (${root_entity_type} -> unit) -> [< ${root_entity_type} ] -> unit
  (**
   * Iter all fields of the given node. This skips any child that is None
   * because of a syntax error or because the field is optional
   *)

  val exists_fields :
    (${root_entity_type} -> bool) -> [< ${root_entity_type} ] -> bool
  (**
   * Fold all fields of the given node. Return true if the given predicate is
   * evaluated to true for at least one node.
   *)

  val for_all_fields :
    (${root_entity_type} -> bool) -> [< ${root_entity_type} ] -> bool
  (**
   * Fold all fields of the given node. Return true if the given predicate is
   * evaluated to true for all nodes.
   *)

  val fold :
    ('a -> ${root_entity_type} -> 'a) -> 'a -> [< ${root_entity_type} ] -> 'a
  (**
   * Fold the entire AST, below the given node, and call the given function on
   * each node in prefix order.
   *)

  val iter :
    (${root_entity_type} -> unit) -> [< ${root_entity_type} ] -> unit
  (**
   * Iterate over the entire AST, below the given node, and call the given
   * function on each node in prefix order.
   *)

  val filter :
    (${root_entity_type} -> bool)
    -> [< ${root_entity_type} ]
    -> ${root_entity_type} list
  (**
   * Fold the entire AST, below the given node, and return the list of node
   * evaluated to true by the given function
   *)

  val exists :
    (${root_entity_type} -> bool) -> [< ${root_entity_type} ] -> bool
  (**
   * Fold the entire AST, below the given node, and return true if the given
   * predicate is evaluated to true by at least one node.
   *)

  val for_all :
    (${root_entity_type} -> bool) -> [< ${root_entity_type} ] -> bool
  (**
   * Fold the entire AST, below the given node, and return true if the given
   * predicate is evaluated to true for all nodes.
   *)

  val find : 'a node -> [< ${root_entity_type} ] -> 'a
  (**
   * Given the kind of node, return the first node found by walking the given
   * node. The type of the resulting node depends on the desired kind
   *)


  val findall : 'a node -> [< ${root_entity_type} ] -> 'a list
  (**
   * Given the kind of node, return the all nodes of this kind found by walking
   * the given node. The type of the resulting nodes depends on the desired
   * kind
   *)

   % endif

   % for field in ocaml_api.get_properties(astnode):
      <%
         default_fields = [
            arg for arg in field.arguments if arg.default_value is not None
         ]
      %>
  val ${field.name.lower} :
      % for i, arg in enumerate(default_fields):
    ${'-> ' if i != 0 else ''}?${arg.name.lower}:
    ${ocaml_api.type_public_name(arg.public_type)}
      % endfor
    ${'-> ' if len(default_fields) > 0 else ''}[< ${ocaml_api.type_public_name(
                                                      astnode)} ]
      % for arg in field.arguments:
         % if arg.default_value is None:
    -> ${ocaml_api.type_public_name(arg.public_type)}
         % endif
      % endfor
      % if field.public_type.is_entity_type:
    -> ${ocaml_api.type_public_name(field.public_type)} option
      % else:
    -> ${ocaml_api.type_public_name(field.public_type)}
      % endif
  ${ocaml_doc(field, 1)}

   % endfor

   % for field in ocaml_api.get_parse_fields(astnode):
      <%
         precise_types = field.precise_types.minimal_matched_types

         if len(precise_types) == 1:
            return_typ = ocaml_api.type_public_name(precise_types.pop())
         else:
            return_typ = '[{}]'.format(' | '.join([
               ocaml_api.type_public_name(typ)
               for typ in precise_types]))

         if field.is_optional:
            return_typ += ' option'
      %>
  val ${field.name.lower} :
    [< ${ocaml_api.type_public_name(astnode)}]
    -> ${return_typ}

   % endfor

   % if astnode.is_list:
  val f_list :
    [< ${ocaml_api.type_public_name(astnode)}]
    -> ${ocaml_api.type_public_name(astnode.element_type)} list

   % endif
end

% endfor

