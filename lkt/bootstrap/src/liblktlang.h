








#ifndef LIBLKTLANG
#define LIBLKTLANG

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * This type represents a context for all source analysis. This is the first
 * type you need to create to use liblktlang. It will contain the results of
 * all analysis, and is the main holder for all the data.
 *
 * You can create several analysis contexts if you need to, which enables you,
 * for example to:
 *
 * * analyze several different projects at the same time;
 *
 * * analyze different parts of the same projects in parallel.
 *
 * In the current design, contexts always keep all of their analysis units
 * allocated. If you need to get this memory released, the only option at your
 * disposal is to destroy your analysis context instance.
 *
 * This structure is partially opaque: some fields are exposed to allow direct
 * access, for performance concerns.
 */
typedef struct
{
   uint64_t serial_number;
} *lkt_analysis_context;

/*
 * This type represents the analysis of a single file.
 *
 * This type has strong-reference semantics and is ref-counted. Furthermore, a
 * reference to a unit contains an implicit reference to the context that owns
 * it. This means that keeping a reference to a unit will keep the context and
 * all the unit it contains allocated.
 *
 * This structure is partially opaque: some fields are exposed to allow direct
 * access, for performance concerns.
 */
typedef struct
{
   uint64_t version_number;
} *lkt_analysis_unit;

/*
 * Data type for all nodes. Nodes are assembled to make up a tree.  See the
 * node primitives below to inspect such trees.
 *
 * Unlike for contexts and units, this type has weak-reference semantics:
 * keeping a reference to a node has no effect on the decision to keep the unit
 * that it owns allocated. This means that once all references to the context
 * and units related to a node are dropped, the context and its units are
 * deallocated and the node becomes a stale reference: most operations on it
 * will raise a ``Stale_Reference_Error``.
 *
 * Note that since reparsing an analysis unit deallocates all the nodes it
 * contains, this operation makes all reference to these nodes stale as well.
 */
typedef struct lkt_base_node__struct *lkt_base_node;

/*
 * Kind of AST nodes in parse trees.
 */
typedef enum {
    

        /* lkt_node (abstract)  */
        /*
         * Root node class for lkt AST nodes.
         *
         * Derived nodes: ``lkt_base_lexer_case_rule_alt``,
         * ``lkt_block_string_line``, ``lkt_class_qualifier``,
         * ``lkt_decl_annotation_params``, ``lkt_decl_annotation``,
         * ``lkt_decl``, ``lkt_dyn_env_wrapper``, ``lkt_elsif_branch``,
         * ``lkt_enum_class_case``, ``lkt_excludes_null``, ``lkt_expr``,
         * ``lkt_full_decl``, ``lkt_grammar_list_sep``, ``lkt_import``,
         * ``lkt_langkit_root``, ``lkt_lexer_case_rule_send``,
         * ``lkt_lexer_case_rule``, ``lkt_list_kind``,
         * ``lkt_lkt_node_base_list``, ``lkt_match_branch``, ``lkt_op``,
         * ``lkt_param``, ``lkt_type_ref``, ``lkt_var_bind``
         */
    

        /* base_lexer_case_rule_alt (abstract)  */
        /*
         * Base class for the different kind of alternatives allowed in a case
         * rule.
         *
         * Derived nodes: ``lkt_lexer_case_rule_cond_alt``,
         * ``lkt_lexer_case_rule_default_alt``
         */
    

        /*
         * Alternative of a case rule which sends the token only if the kind of
         * the previous token is among a given set.
         *
         * This node type has no derivation.
         */
        lkt_lexer_case_rule_cond_alt = 1,
    

        /*
         * Default alternative of a case rule which sends the token if all the
         * previous alternatives failed.
         *
         * This node type has no derivation.
         */
        lkt_lexer_case_rule_default_alt = 2,
    

        /*
         * A single line in a block string literal.
         *
         * This node type has no derivation.
         */
        lkt_block_string_line = 3,
    

        /* class_qualifier (abstract)  */
        /*
         * Whether this generic formal type must be a class.
         *
         * Derived nodes: ``lkt_class_qualifier_absent``,
         * ``lkt_class_qualifier_present``
         */
    

        /*
         * This node type has no derivation.
         */
        lkt_class_qualifier_absent = 4,
    

        /*
         * This node type has no derivation.
         */
        lkt_class_qualifier_present = 5,
    

        /* decl (abstract)  */
        /*
         * Base class for declarations. Encompasses regular declarations as
         * well as special declarations such as grammars, grammar rules, etc.
         *
         * Derived nodes: ``lkt_base_grammar_rule_decl``,
         * ``lkt_base_val_decl``, ``lkt_env_spec_decl``, ``lkt_generic_decl``,
         * ``lkt_grammar_decl``, ``lkt_lexer_decl``, ``lkt_lexer_family_decl``,
         * ``lkt_synth_arg_decl``, ``lkt_synth_fun_decl``, ``lkt_type_decl``
         */
    

        /* base_grammar_rule_decl (abstract)  */
        /*
         * Base class for grammar rules inside of grammars/lexers.
         *
         * Derived nodes: ``lkt_grammar_rule_decl``,
         * ``lkt_synthetic_lexer_decl``
         */
    

        /*
         * Declaration of a grammar rule inside of a grammar.
         *
         * This node type has no derivation.
         */
        lkt_grammar_rule_decl = 6,
    

        /*
         * This node type has no derivation.
         */
        lkt_synthetic_lexer_decl = 7,
    

        /* base_val_decl (abstract)  */
        /*
         * Abstract class for named values declarations, such as arguments,
         * local value bindings, fields, etc.
         *
         * Derived nodes: ``lkt_node_decl``, ``lkt_self_decl``,
         * ``lkt_user_val_decl``
         */
    

        /*
         * Synthetic declaration for the implicit "node" variable available in
         * properties.
         *
         * This node type has no derivation.
         */
        lkt_node_decl = 8,
    

        /*
         * Synthetic declaration for the implicit "self" variable available in
         * properties.
         *
         * This node type has no derivation.
         */
        lkt_self_decl = 9,
    

        /* user_val_decl (abstract)  */
        /*
         * Class for user declared val declarations (not synthetic).
         *
         * Derived nodes: ``lkt_enum_lit_decl``, ``lkt_explicitly_typed_decl``,
         * ``lkt_fun_decl``
         */
    

        /*
         * Enum literal declaration.
         *
         * This node type has no derivation.
         */
        lkt_enum_lit_decl = 10,
    

        /* explicitly_typed_decl (abstract)  */
        /*
         * Subset of user declared value declarations for values that have a
         * type that can be syntactically annotated by the user.
         *
         * Derived nodes: ``lkt_component_decl``, ``lkt_dyn_var_decl``,
         * ``lkt_match_val_decl``, ``lkt_val_decl``
         */
    

        /* component_decl (abstract)  */
        /*
         * Subset of explicitly typed declarations for value declarations that:
         *
         * 1. Have an optional default value.
         *
         * 2. Are part of a bigger declaration that can be referred to via a
         *    call expression (either a type or a function).
         *
         * Derived nodes: ``lkt_field_decl``, ``lkt_fun_arg_decl``,
         * ``lkt_lambda_arg_decl``
         */
    

        /*
         * Field declaration.
         *
         * This node type has no derivation.
         */
        lkt_field_decl = 11,
    

        /*
         * Function argument declaration.
         *
         * This node type has no derivation.
         */
        lkt_fun_arg_decl = 12,
    

        /*
         * Function argument declaration.
         *
         * This node type has no derivation.
         */
        lkt_lambda_arg_decl = 13,
    

        /*
         * Dynamic variable declaration.
         *
         * This node type has no derivation.
         */
        lkt_dyn_var_decl = 14,
    

        /*
         * Value declaration in a match branch.
         *
         * This node type has no derivation.
         */
        lkt_match_val_decl = 15,
    

        /*
         * Value declaration.
         *
         * This node type has no derivation.
         */
        lkt_val_decl = 16,
    

        /*
         * Function declaration.
         *
         * This node type has no derivation.
         */
        lkt_fun_decl = 17,
    

        /*
         * Env spec declaration.
         *
         * Each node type can have one or no env spec. Env specs contains only
         * a list of env actions.
         *
         * This node type has no derivation.
         */
        lkt_env_spec_decl = 18,
    

        /*
         * Generic entity declaration.
         *
         * This node type has no derivation.
         */
        lkt_generic_decl = 19,
    

        /*
         * Declaration of a language's grammar.
         *
         * This node type has no derivation.
         */
        lkt_grammar_decl = 20,
    

        /*
         * Declaration of a language's lexer.
         *
         * This node type has no derivation.
         */
        lkt_lexer_decl = 21,
    

        /*
         * Declaration of a token family.
         *
         * This node type has no derivation.
         */
        lkt_lexer_family_decl = 22,
    

        /*
         * Logic argument function declaration.
         *
         * This node type has no derivation.
         */
        lkt_synth_arg_decl = 23,
    

        /*
         * Logic function declaration.
         *
         * This node type has no derivation.
         */
        lkt_synth_fun_decl = 24,
    

        /* type_decl (abstract)  */
        /*
         * Abstract base class for type declarations.
         *
         * Derived nodes: ``lkt_any_type_decl``, ``lkt_enum_class_alt_decl``,
         * ``lkt_function_type``, ``lkt_generic_formal_type_decl``,
         * ``lkt_named_type_decl``
         */
    

        /*
         * Internal type to represent a type that can be matched with anything.
         *
         * This node type has no derivation.
         */
        lkt_any_type_decl = 25,
    

        /*
         * Alternative for an enum class decl.
         *
         * This node type has no derivation.
         */
        lkt_enum_class_alt_decl = 26,
    

        /*
         * Function type.
         *
         * This node type has no derivation.
         */
        lkt_function_type = 27,
    

        /*
         * Declaration of a generic formal type in a generic declaration.
         *
         * This node type has no derivation.
         */
        lkt_generic_formal_type_decl = 28,
    

        /* named_type_decl (abstract)  */
        /*
         * Explicit named type declaration.
         *
         * Derived nodes: ``lkt_basic_class_decl``, ``lkt_enum_type_decl``,
         * ``lkt_struct_decl``, ``lkt_trait_decl``
         */
    

        /* basic_class_decl (abstract)  */
        /*
         * Common ancestor for declarations of regular classes and enum
         * classes.
         *
         * Derived nodes: ``lkt_class_decl``, ``lkt_enum_class_decl``
         */
    

        /*
         * Declaration for a LK class. This only cover node classes for the
         * moment, but might be extended to support regular classes in the
         * future.
         *
         * This node type has no derivation.
         */
        lkt_class_decl = 29,
    

        /*
         * Declaration for a LK class. This only cover node classes for the
         * moment, but might be extended to support regular classes in the
         * future.
         *
         * This node type has no derivation.
         */
        lkt_enum_class_decl = 30,
    

        /*
         * Enum type declaration.
         *
         * This node type has no derivation.
         */
        lkt_enum_type_decl = 31,
    

        /*
         * Declaration for a LK struct.
         *
         * This node type has no derivation.
         */
        lkt_struct_decl = 32,
    

        /*
         * Trait declaration. For the moment, a Trait can just be used to group
         * behavior for built-in types. It's not usable as a type-bound since
         * we don't have generics, and you cannot implement one either.
         *
         * The reason they're added is to lay down the basics of what we want
         * the Lkt type system to be.
         *
         * TODO: Traits are *not* types. They're treated as such in the grammar
         * for convenience for now, but it's probably not a good idea. Migrate
         * away from this.
         *
         * This node type has no derivation.
         */
        lkt_trait_decl = 33,
    

        /*
         * Compile time annotation attached to a declaration.
         *
         * This node type has no derivation.
         */
        lkt_decl_annotation = 34,
    

        /*
         * List of arguments for an annotation with a call syntax. This
         * intermediate node is necessary in order to determine after parsing
         * whether there is no param list, or if the list is empty.
         *
         * This node type has no derivation.
         */
        lkt_decl_annotation_params = 35,
    

        /*
         * Synthetic node to instantiate a DynamicEnvironment for generics.
         *
         * This node type has no derivation.
         */
        lkt_dyn_env_wrapper = 36,
    

        /*
         * Elsif branch of an if expression.
         *
         * This node type has no derivation.
         */
        lkt_elsif_branch = 37,
    

        /*
         * Case branch for an enum class declaration.
         *
         * This node type has no derivation.
         */
        lkt_enum_class_case = 38,
    

        /* excludes_null (abstract)  */
        /*
         * Whether the containing cast expression will raise on null cast
         * result or not.
         *
         * Derived nodes: ``lkt_excludes_null_absent``,
         * ``lkt_excludes_null_present``
         */
    

        /*
         * This node type has no derivation.
         */
        lkt_excludes_null_absent = 39,
    

        /*
         * This node type has no derivation.
         */
        lkt_excludes_null_present = 40,
    

        /* expr (abstract)  */
        /*
         * Base class for expressions. Encompasses regular expressions as well
         * as special expressions (grammar expressions, etc).
         *
         * Derived nodes: ``lkt_any_of``, ``lkt_array_literal``,
         * ``lkt_base_call_expr``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
         * ``lkt_block_expr``, ``lkt_cast_expr``, ``lkt_error_on_null``,
         * ``lkt_generic_instantiation``, ``lkt_grammar_expr``, ``lkt_id``,
         * ``lkt_if_expr``, ``lkt_isa``, ``lkt_keep_expr``,
         * ``lkt_lambda_expr``, ``lkt_lit``, ``lkt_logic_assign``,
         * ``lkt_logic_expr``, ``lkt_logic_propagate``, ``lkt_logic_unify``,
         * ``lkt_match_expr``, ``lkt_not_expr``, ``lkt_paren_expr``,
         * ``lkt_raise_expr``, ``lkt_subscript_expr``, ``lkt_try_expr``,
         * ``lkt_un_op``
         */
    

        /*
         * "Any of" expression.
         *
         * This node type has no derivation.
         */
        lkt_any_of = 41,
    

        /*
         * Literal for an array value.
         *
         * This node type has no derivation.
         */
        lkt_array_literal = 42,
    

        /* base_call_expr (abstract)  */
        /*
         * Base class for expressions that are syntactically call-like.
         *
         * Derived nodes: ``lkt_call_expr``, ``lkt_logic_call_expr``
         */
    

        /*
         * Call expression.
         *
         * This node type has no derivation.
         */
        lkt_call_expr = 43,
    

        /* logic_call_expr (abstract)  */
        /*
         * Base class for logic call expresions, of the form:
         *
         * .. code::
         *
         *    name%(args)
         *
         * Derived nodes: ``lkt_logic_predicate``, ``lkt_logic_propagate_call``
         */
    

        /*
         * Class for "predicate" equations.
         *
         * This node type has no derivation.
         */
        lkt_logic_predicate = 44,
    

        /*
         * Class for the call inside "propagate" equations.
         *
         * This node type has no derivation.
         */
        lkt_logic_propagate_call = 45,
    

        /* base_dot_expr (abstract)  */
        /*
         * Base class for regular dotted expressions and null-conditional ones.
         *
         * Derived nodes: ``lkt_dot_expr``, ``lkt_null_cond_dotted_name``
         */
    

        /*
         * Dotted expression.
         *
         * This node type has no derivation.
         */
        lkt_dot_expr = 46,
    

        /*
         * Null conditional dotted expression.
         *
         * This node type has no derivation.
         */
        lkt_null_cond_dotted_name = 47,
    

        /*
         * Binary operator expression.
         *
         * This node type has no derivation.
         */
        lkt_bin_op = 48,
    

        /*
         * Block expression.
         *
         * This node type has no derivation.
         */
        lkt_block_expr = 49,
    

        /*
         * Cast expression.
         *
         * This node type has no derivation.
         */
        lkt_cast_expr = 50,
    

        /*
         * Expression that throws an error if LHS is null.
         *
         * This node type has no derivation.
         */
        lkt_error_on_null = 51,
    

        /*
         * Generic instantiation.
         *
         * This node type has no derivation.
         */
        lkt_generic_instantiation = 52,
    

        /* grammar_expr (abstract)  */
        /*
         * Base class for expressions related to grammars.
         *
         * Derived nodes: ``lkt_grammar_cut``, ``lkt_grammar_discard``,
         * ``lkt_grammar_dont_skip``, ``lkt_grammar_list``,
         * ``lkt_grammar_null``, ``lkt_grammar_opt_error_group``,
         * ``lkt_grammar_opt_error``, ``lkt_grammar_opt_group``,
         * ``lkt_grammar_opt``, ``lkt_grammar_or_expr``, ``lkt_grammar_pick``,
         * ``lkt_grammar_predicate``, ``lkt_grammar_rule_ref``,
         * ``lkt_grammar_skip``, ``lkt_grammar_stop_cut``,
         * ``lkt_parse_node_expr``, ``lkt_token_lit``,
         * ``lkt_token_no_case_lit``, ``lkt_token_pattern_concat``,
         * ``lkt_token_pattern_lit``, ``lkt_token_ref``
         */
    

        /*
         * Grammar expression for a cut.
         *
         * This node type has no derivation.
         */
        lkt_grammar_cut = 53,
    

        /*
         * Grammar expression to discard the match.
         *
         * This node type has no derivation.
         */
        lkt_grammar_discard = 54,
    

        /*
         * Grammar expression (error recovery) to ensure that any nested skip
         * parser calls won't skip certain parse results.
         *
         * This node type has no derivation.
         */
        lkt_grammar_dont_skip = 55,
    

        /*
         * Grammar expression to parse lists of results. Results can be
         * separated by a separator. List can be empty ('*') or not ('+').
         *
         * This node type has no derivation.
         */
        lkt_grammar_list = 56,
    

        /*
         * Grammar expression to parse a null node.
         *
         * This node type has no derivation.
         */
        lkt_grammar_null = 57,
    

        /*
         * Grammar expression for an optional parsing result.
         *
         * This node type has no derivation.
         */
        lkt_grammar_opt = 58,
    

        /*
         * Grammar expression for an optional parsing result. Missing result
         * creates an error, but parsing continues.
         *
         * This node type has no derivation.
         */
        lkt_grammar_opt_error = 59,
    

        /*
         * Grammar expression for a group of optional parsing results. Failure
         * to parse an optional result creates an error, but parsing continues.
         *
         * This node type has no derivation.
         */
        lkt_grammar_opt_error_group = 60,
    

        /*
         * Grammar expression for a group of optional parsing results.
         *
         * This node type has no derivation.
         */
        lkt_grammar_opt_group = 61,
    

        /*
         * Grammar ``Or`` expression (disjunctive choice between several
         * grammar options).
         *
         * This node type has no derivation.
         */
        lkt_grammar_or_expr = 62,
    

        /*
         * Grammar expression to pick the significant parse out of a list of
         * parses (will automatically discard token results).
         *
         * Derived nodes: ``lkt_grammar_implicit_pick``
         */
        lkt_grammar_pick = 63,
    

        /*
         * Implicit pick operation.
         *
         * This node type has no derivation.
         */
        lkt_grammar_implicit_pick = 64,
    

        /*
         * Grammar expression for a predicate: Only parse something if the
         * predicate (that is a reference to a node property) returns True.
         *
         * This node type has no derivation.
         */
        lkt_grammar_predicate = 65,
    

        /*
         * Grammar expression for a reference to another grammar rule.
         *
         * This node type has no derivation.
         */
        lkt_grammar_rule_ref = 66,
    

        /*
         * Grammar expression (error recovery) to skip a parsing result.
         *
         * This node type has no derivation.
         */
        lkt_grammar_skip = 67,
    

        /*
         * Grammar expression for a StopCut.
         *
         * This node type has no derivation.
         */
        lkt_grammar_stop_cut = 68,
    

        /*
         * Expression for the parsing of a Node.
         *
         * This node type has no derivation.
         */
        lkt_parse_node_expr = 69,
    

        /*
         * Grammar expression for a token literal.
         *
         * This node type has no derivation.
         */
        lkt_token_lit = 70,
    

        /*
         * Grammar expression for a case insensitive token literal.
         *
         * This node type has no derivation.
         */
        lkt_token_no_case_lit = 71,
    

        /*
         * Grammar expression for the concatenation of two patterns.
         *
         * This node type has no derivation.
         */
        lkt_token_pattern_concat = 72,
    

        /*
         * Grammar expression for a pattern literal.
         *
         * This node type has no derivation.
         */
        lkt_token_pattern_lit = 73,
    

        /*
         * Grammar expression for a token reference.
         *
         * This node type has no derivation.
         */
        lkt_token_ref = 74,
    

        /*
         * Identifier.
         *
         * Derived nodes: ``lkt_def_id``, ``lkt_module_ref_id``, ``lkt_ref_id``
         */
        lkt_id = 75,
    

        /*
         * Defining identifier.
         *
         * This node type has no derivation.
         */
        lkt_def_id = 76,
    

        /*
         * Id referencing a langkit module.
         *
         * This node type has no derivation.
         */
        lkt_module_ref_id = 77,
    

        /*
         * Reference identifier.
         *
         * This node type has no derivation.
         */
        lkt_ref_id = 78,
    

        /*
         * If expression.
         *
         * This node type has no derivation.
         */
        lkt_if_expr = 79,
    

        /*
         * Isa expression.
         *
         * This node type has no derivation.
         */
        lkt_isa = 80,
    

        /*
         * Keep expression.
         *
         * This node type has no derivation.
         */
        lkt_keep_expr = 81,
    

        /*
         * Lambda expression.
         *
         * This node type has no derivation.
         */
        lkt_lambda_expr = 82,
    

        /* lit (abstract)  */
        /*
         * Base class for literals.
         *
         * Derived nodes: ``lkt_big_num_lit``, ``lkt_char_lit``,
         * ``lkt_null_lit``, ``lkt_num_lit``, ``lkt_string_lit``
         */
    

        /*
         * Big number literal expression.
         *
         * This node type has no derivation.
         */
        lkt_big_num_lit = 83,
    

        /*
         * Character literal expression.
         *
         * This node type has no derivation.
         */
        lkt_char_lit = 84,
    

        /*
         * Null literal expression.
         *
         * This node type has no derivation.
         */
        lkt_null_lit = 85,
    

        /*
         * Number literal expression.
         *
         * This node type has no derivation.
         */
        lkt_num_lit = 86,
    

        /* string_lit (abstract)  */
        /*
         * Base node type for string literals.
         *
         * Derived nodes: ``lkt_block_string_lit``,
         * ``lkt_single_line_string_lit``
         */
    

        /*
         * String literal expression, made of multiple line strings.
         *
         * The denoted string value is the concatenation of all line string
         * items. Each line string item must be either:
         *
         * * The empty string designator (``|"``), to denote an empty line
         *   (``\n``).
         *
         * * ``|" <content>``, to designate a non-empty line. The space before
         *   ``<content>`` is mandatory, and is not included in the denoted
         *   string value. ``<content>`` can be anything that appear in a
         *   regular string literal: escape sequences are interpreted the same
         *   way.
         *
         * This node type has no derivation.
         */
        lkt_block_string_lit = 87,
    

        /*
         * Single line string literal expression.
         *
         * Note that in order to reduce the size of the node type hierarchy, we
         * define only one node (StringLit) for all our string literals (only
         * regular strings and pattern string literals at the moment). This
         * will also make it easy to add new string prefixes in the future.
         *
         * Derived nodes: ``lkt_pattern_single_line_string_lit``
         */
        lkt_single_line_string_lit = 88,
    

        /*
         * Pattern single line string literal expression.
         *
         * This node type has no derivation.
         */
        lkt_pattern_single_line_string_lit = 89,
    

        /*
         * Class for "assign to logic var" equations.
         *
         * This node type has no derivation.
         */
        lkt_logic_assign = 90,
    

        /*
         * Class for logic expressions (any ``basic_expr`` starting with %).
         *
         * This node type has no derivation.
         */
        lkt_logic_expr = 91,
    

        /*
         * Class for "propagate" equations.
         *
         * This node type has no derivation.
         */
        lkt_logic_propagate = 92,
    

        /*
         * Class for "unify" equations.
         *
         * This node type has no derivation.
         */
        lkt_logic_unify = 93,
    

        /*
         * Binary operator expression.
         *
         * This node type has no derivation.
         */
        lkt_match_expr = 94,
    

        /*
         * Boolean negation expression.
         *
         * This node type has no derivation.
         */
        lkt_not_expr = 95,
    

        /*
         * Parenthesized expression.
         *
         * This node type has no derivation.
         */
        lkt_paren_expr = 96,
    

        /*
         * Raise expression.
         *
         * This node type has no derivation.
         */
        lkt_raise_expr = 97,
    

        /*
         * Array subscript expression.
         *
         * Derived nodes: ``lkt_null_cond_subscript_expr``
         */
        lkt_subscript_expr = 98,
    

        /*
         * Null conditional subscript expression.
         *
         * This node type has no derivation.
         */
        lkt_null_cond_subscript_expr = 99,
    

        /*
         * Try expression.
         *
         * This node type has no derivation.
         */
        lkt_try_expr = 100,
    

        /*
         * Unary operator expression.
         *
         * This node type has no derivation.
         */
        lkt_un_op = 101,
    

        /*
         * Container for an lkt declaration. Contains the decl node plus the
         * documentation and annotations.
         *
         * This node type has no derivation.
         */
        lkt_full_decl = 102,
    

        /*
         * Specification for the separator of a list parser.
         *
         * This node type has no derivation.
         */
        lkt_grammar_list_sep = 103,
    

        /*
         * Statement to import another source file.
         *
         * This node type has no derivation.
         */
        lkt_import = 104,
    

        /*
         * For the moment, root node of a lkt compilation unit.
         *
         * This node type has no derivation.
         */
        lkt_langkit_root = 105,
    

        /*
         * Lexer construct to introduce a conditional lexing action.
         *
         * This node type has no derivation.
         */
        lkt_lexer_case_rule = 106,
    

        /*
         * Lexer construction used by case alternatives to represent the token
         * to send if that alternative was chosen.
         *
         * This node type has no derivation.
         */
        lkt_lexer_case_rule_send = 107,
    

        /* list_kind (abstract)  */
        /*
         * Kind for list parser expressions.
         *
         * Derived nodes: ``lkt_list_kind_one``, ``lkt_list_kind_zero``
         */
    

        /*
         * This node type has no derivation.
         */
        lkt_list_kind_one = 108,
    

        /*
         * This node type has no derivation.
         */
        lkt_list_kind_zero = 109,
    

        /* lkt_node_base_list (abstract)  */
        /*
         * Derived nodes: ``lkt_base_lexer_case_rule_alt_list``,
         * ``lkt_block_string_line_list``, ``lkt_call_expr_list``,
         * ``lkt_decl_annotation_list``, ``lkt_elsif_branch_list``,
         * ``lkt_enum_class_alt_decl_list``, ``lkt_enum_class_case_list``,
         * ``lkt_enum_lit_decl_list``, ``lkt_expr_list``,
         * ``lkt_full_decl_list``, ``lkt_fun_arg_decl_list``,
         * ``lkt_grammar_expr_list_list``, ``lkt_grammar_expr_list``,
         * ``lkt_import_list``, ``lkt_lambda_arg_decl_list``,
         * ``lkt_lkt_node_list``, ``lkt_match_branch_list``,
         * ``lkt_param_list``, ``lkt_ref_id_list``, ``lkt_type_ref_list``
         */
    

        /*
         * List of BaseLexerCaseRuleAlt.
         *
         * This node type has no derivation.
         */
        lkt_base_lexer_case_rule_alt_list = 110,
    

        /*
         * List of BlockStringLine.
         *
         * This node type has no derivation.
         */
        lkt_block_string_line_list = 111,
    

        /*
         * List of CallExpr.
         *
         * This node type has no derivation.
         */
        lkt_call_expr_list = 112,
    

        /*
         * List of DeclAnnotation.
         *
         * This node type has no derivation.
         */
        lkt_decl_annotation_list = 113,
    

        /*
         * List of ElsifBranch.
         *
         * This node type has no derivation.
         */
        lkt_elsif_branch_list = 114,
    

        /*
         * List of EnumClassAltDecl.
         *
         * This node type has no derivation.
         */
        lkt_enum_class_alt_decl_list = 115,
    

        /*
         * List of EnumClassCase.
         *
         * This node type has no derivation.
         */
        lkt_enum_class_case_list = 116,
    

        /*
         * List of EnumLitDecl.
         *
         * This node type has no derivation.
         */
        lkt_enum_lit_decl_list = 117,
    

        /*
         * List of Expr.
         *
         * This list node can contain one of the following nodes:
         * ``lkt_any_of``, ``lkt_array_literal``, ``lkt_base_dot_expr``,
         * ``lkt_bin_op``, ``lkt_block_expr``, ``lkt_call_expr``,
         * ``lkt_cast_expr``, ``lkt_error_on_null``,
         * ``lkt_generic_instantiation``, ``lkt_if_expr``, ``lkt_isa``,
         * ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
         * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
         * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
         * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``,
         * ``lkt_ref_id``, ``lkt_subscript_expr``, ``lkt_try_expr``,
         * ``lkt_un_op``
         *
         * Derived nodes: ``lkt_any_of_list``
         */
        lkt_expr_list = 118,
    

        /*
         * Pipe-separated list of expressions.
         *
         * This is used to represent the "values" operand of an ``AnyOf``
         * expression.
         *
         * This list node can contain one of the following nodes:
         * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_block_expr``,
         * ``lkt_call_expr``, ``lkt_cast_expr``, ``lkt_error_on_null``,
         * ``lkt_generic_instantiation``, ``lkt_if_expr``, ``lkt_keep_expr``,
         * ``lkt_lambda_expr``, ``lkt_lit``, ``lkt_logic_expr``,
         * ``lkt_logic_predicate``, ``lkt_match_expr``, ``lkt_paren_expr``,
         * ``lkt_raise_expr``, ``lkt_ref_id``, ``lkt_subscript_expr``,
         * ``lkt_try_expr``
         *
         * This node type has no derivation.
         */
        lkt_any_of_list = 119,
    

        /*
         * List of FullDecl.
         *
         * Derived nodes: ``lkt_decl_block``, ``lkt_generic_formal_decl_list``
         */
        lkt_full_decl_list = 120,
    

        /*
         * List of declarations that also introduces a containing lexical
         * scope.
         *
         * This node type has no derivation.
         */
        lkt_decl_block = 121,
    

        /*
         * Comma-separated list of generic formal types.
         *
         * This node type has no derivation.
         */
        lkt_generic_formal_decl_list = 122,
    

        /*
         * List of FunArgDecl.
         *
         * This node type has no derivation.
         */
        lkt_fun_arg_decl_list = 123,
    

        /*
         * List of GrammarExpr.
         *
         * This node type has no derivation.
         */
        lkt_grammar_expr_list = 124,
    

        /*
         * List of GrammarExpr.list.
         *
         * This node type has no derivation.
         */
        lkt_grammar_expr_list_list = 125,
    

        /*
         * List of Import.
         *
         * This node type has no derivation.
         */
        lkt_import_list = 126,
    

        /*
         * List of LambdaArgDecl.
         *
         * This node type has no derivation.
         */
        lkt_lambda_arg_decl_list = 127,
    

        /*
         * List of LktNode.
         *
         * This list node can contain one of the following nodes:
         * ``lkt_full_decl``, ``lkt_lexer_case_rule``, ``lkt_val_decl``,
         * ``lkt_var_bind``
         *
         * Derived nodes: ``lkt_block_decl_list``
         */
        lkt_lkt_node_list = 128,
    

        /*
         * Semicolon-separated list of declarations.
         *
         * This is used to represent declarations in a block expression.
         *
         * This list node can contain one of the following nodes:
         * ``lkt_val_decl``, ``lkt_var_bind``
         *
         * This node type has no derivation.
         */
        lkt_block_decl_list = 129,
    

        /*
         * List of MatchBranch.
         *
         * This node type has no derivation.
         */
        lkt_match_branch_list = 130,
    

        /*
         * List of Param.
         *
         * This node type has no derivation.
         */
        lkt_param_list = 131,
    

        /*
         * List of RefId.
         *
         * This node type has no derivation.
         */
        lkt_ref_id_list = 132,
    

        /*
         * List of TypeRef.
         *
         * This list node can contain one of the following nodes:
         * ``lkt_function_type_ref``, ``lkt_generic_type_ref``,
         * ``lkt_simple_type_ref``
         *
         * Derived nodes: ``lkt_isa_list``
         */
        lkt_type_ref_list = 133,
    

        /*
         * Pipe-separated list of type references.
         *
         * This is used to represent the accepted types in an ``Isa``
         * expression.
         *
         * This list node can contain one of the following nodes:
         * ``lkt_function_type_ref``, ``lkt_generic_type_ref``,
         * ``lkt_simple_type_ref``
         *
         * This node type has no derivation.
         */
        lkt_isa_list = 134,
    

        /*
         * Branch inside a match expression.
         *
         * This node type has no derivation.
         */
        lkt_match_branch = 135,
    

        /* op (abstract)  */
        /*
         * Operator in a binary operator expression.
         *
         * Derived nodes: ``lkt_op_amp``, ``lkt_op_and``, ``lkt_op_div``,
         * ``lkt_op_eq``, ``lkt_op_gt``, ``lkt_op_gte``, ``lkt_op_logic_and``,
         * ``lkt_op_logic_or``, ``lkt_op_lt``, ``lkt_op_lte``,
         * ``lkt_op_minus``, ``lkt_op_mult``, ``lkt_op_ne``, ``lkt_op_or_int``,
         * ``lkt_op_or``, ``lkt_op_plus``
         */
    

        /*
         * This node type has no derivation.
         */
        lkt_op_amp = 136,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_and = 137,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_div = 138,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_eq = 139,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_gt = 140,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_gte = 141,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_logic_and = 142,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_logic_or = 143,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_lt = 144,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_lte = 145,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_minus = 146,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_mult = 147,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_ne = 148,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_or = 149,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_or_int = 150,
    

        /*
         * This node type has no derivation.
         */
        lkt_op_plus = 151,
    

        /*
         * Parameter for function calls or for annotations.
         *
         * This node type has no derivation.
         */
        lkt_param = 152,
    

        /* type_ref (abstract)  */
        /*
         * Base class for a reference to a type.
         *
         * Derived nodes: ``lkt_default_list_type_ref``,
         * ``lkt_function_type_ref``, ``lkt_generic_type_ref``,
         * ``lkt_simple_type_ref``
         */
    

        /*
         * "list" type reference in parsers.
         *
         * This node type has no derivation.
         */
        lkt_default_list_type_ref = 153,
    

        /*
         * Reference to a function type.
         *
         * This node type has no derivation.
         */
        lkt_function_type_ref = 154,
    

        /*
         * Reference to a generic type.
         *
         * This node type has no derivation.
         */
        lkt_generic_type_ref = 155,
    

        /*
         * Simple reference to a type.
         *
         * This node type has no derivation.
         */
        lkt_simple_type_ref = 156,
    

        /*
         * Dynamic var bind expression.
         *
         * This node type has no derivation.
         */
        lkt_var_bind = 157,
} lkt_node_kind_enum;

/*
 * Reference to a symbol. Symbols are owned by analysis contexts, so they must
 * not outlive them. This type exists only in the C API, and roughly wraps the
 * corresponding Ada type (an array fat pointer).
 */
typedef struct {
   uint32_t thin_sym;
   void *table;
} lkt_symbol_type;

/*
 * Type to contain Unicode text data.
 */
typedef struct {
   int length;
   int ref_count;
   uint32_t content[1];
} *lkt_string_type;

/*
 * Data type for env rebindings. For internal use only.
 */
typedef struct lkt_env_rebindings_type__struct *lkt_env_rebindings_type;

typedef uint8_t lkt_bool;

/* Helper data structures for source location handling.  */

/*
 * Location in a source file. Line and column numbers are one-based.
 */
typedef struct {
    uint32_t line;
    uint16_t column;
} lkt_source_location;

/*
 * Location of a span of text in a source file.
 */
typedef struct {
    lkt_source_location start;
    lkt_source_location end;
} lkt_source_location_range;


/*
 * String encoded in UTF-32 (native endianness).
 */
typedef struct {
   /*
 * Address for the content of the string.
 */
    uint32_t *chars;
   /*
 * Size of the string (in characters).
 */
    size_t length;

    int is_allocated;
} lkt_text;

/*
 * Arbitrarily large integer.
 */
typedef struct lkt_big_integer__struct *lkt_big_integer;

/*
 * Kind for this token.
 */
typedef enum {
   
      
      LKT_AMP = 0
      ,
      LKT_AND_KW = 1
      ,
      LKT_AT = 2
      ,
      LKT_BIG_NUMBER = 3
      ,
      LKT_BIND_KW = 4
      ,
      LKT_BLOCK_STRING_LINE = 5
      ,
      LKT_CASE_KW = 6
      ,
      LKT_CHAR = 7
      ,
      LKT_CLASS_KW = 8
      ,
      LKT_COLON = 9
      ,
      LKT_COMB = 10
      ,
      LKT_COMMA = 11
      ,
      LKT_COMMENT = 12
      ,
      LKT_DISCARD_KW = 13
      ,
      LKT_DIV = 14
      ,
      LKT_DOC_COMMENT = 15
      ,
      LKT_DOT = 16
      ,
      LKT_DYN_VAR_KW = 17
      ,
      LKT_E_Q = 18
      ,
      LKT_ELIF_KW = 19
      ,
      LKT_ELSE_KW = 20
      ,
      LKT_ENUM_KW = 21
      ,
      LKT_EQUAL = 22
      ,
      LKT_EXCL_MARK = 23
      ,
      LKT_FAT_RIGHT_ARROW = 24
      ,
      LKT_FUN_KW = 25
      ,
      LKT_G_T = 26
      ,
      LKT_G_T_E = 27
      ,
      LKT_GENERIC_KW = 28
      ,
      LKT_GRAMMAR_KW = 29
      ,
      LKT_IDENTIFIER = 30
      ,
      LKT_IF_KW = 31
      ,
      LKT_IMPLEMENTS_KW = 32
      ,
      LKT_IMPORT_KW = 33
      ,
      LKT_IN_KW = 34
      ,
      LKT_INT_MARK = 35
      ,
      LKT_IS_KW = 36
      ,
      LKT_L_BRACE = 37
      ,
      LKT_L_BRACK = 38
      ,
      LKT_L_PAR = 39
      ,
      LKT_L_T = 40
      ,
      LKT_L_T_E = 41
      ,
      LKT_LEFT_ARROW = 42
      ,
      LKT_LEXER_KW = 43
      ,
      LKT_LEXING_FAILURE = 44
      ,
      LKT_MATCH_KW = 45
      ,
      LKT_MINUS = 46
      ,
      LKT_N_E = 47
      ,
      LKT_NOT_KW = 48
      ,
      LKT_NULL_KW = 49
      ,
      LKT_NUMBER = 50
      ,
      LKT_OR_KW = 51
      ,
      LKT_P_STRING = 52
      ,
      LKT_PERCENT = 53
      ,
      LKT_PIPE = 54
      ,
      LKT_PLUS = 55
      ,
      LKT_PRIVATE_KW = 56
      ,
      LKT_PUBLIC_KW = 57
      ,
      LKT_R_BRACE = 58
      ,
      LKT_R_BRACK = 59
      ,
      LKT_R_PAR = 60
      ,
      LKT_RAISE_KW = 61
      ,
      LKT_RIGHT_ARROW = 62
      ,
      LKT_SEMICOLON = 63
      ,
      LKT_STRING = 64
      ,
      LKT_STRUCT_KW = 65
      ,
      LKT_TERMINATION = 66
      ,
      LKT_THEN_KW = 67
      ,
      LKT_TIMES = 68
      ,
      LKT_TRAIT_KW = 69
      ,
      LKT_TRY_KW = 70
      ,
      LKT_TWO_SIDED_ARROW = 71
      ,
      LKT_VAL_KW = 72
      ,
      LKT_WHITESPACE = 73
} lkt_token_kind;

typedef struct
{
   uint64_t version;
} *lkt_token_data_handler;

/*
 * Reference to a token in an analysis unit.
 */
typedef struct {
    /* Private data associated to this token, including stale reference
       checking data, or NULL if this designates no token.  */
    lkt_analysis_context context;
    lkt_token_data_handler token_data;

    /* Internal identifiers for this token.  */
    int token_index, trivia_index;
} lkt_token;


/*
 * Diagnostic for an analysis unit: cannot open the source file, parsing error,
 * ...
 */
typedef struct {
    lkt_source_location_range sloc_range;
    lkt_text message;
} lkt_diagnostic;

   typedef enum {
      LKT_ANALYSIS_UNIT_KIND_UNIT_SPECIFICATION, LKT_ANALYSIS_UNIT_KIND_UNIT_BODY
   } lkt_analysis_unit_kind;
   /*
    * Specify a kind of analysis unit. Specification units provide an interface
    * to the outer world while body units provide an implementation for the
    * corresponding interface.
    */
   typedef enum {
      LKT_LOOKUP_KIND_RECURSIVE, LKT_LOOKUP_KIND_FLAT, LKT_LOOKUP_KIND_MINIMAL
   } lkt_lookup_kind;
   /*

    */
   typedef enum {
      LKT_DESIGNATED_ENV_KIND_NONE, LKT_DESIGNATED_ENV_KIND_CURRENT_ENV, LKT_DESIGNATED_ENV_KIND_NAMED_ENV, LKT_DESIGNATED_ENV_KIND_DIRECT_ENV
   } lkt_designated_env_kind;
   /*
    * Discriminant for DesignatedEnv structures.
    */
   typedef enum {
      LKT_GRAMMAR_RULE_MAIN_RULE_RULE, LKT_GRAMMAR_RULE_ID_RULE, LKT_GRAMMAR_RULE_REF_ID_RULE, LKT_GRAMMAR_RULE_TYPE_REF_ID_RULE, LKT_GRAMMAR_RULE_DEF_ID_RULE, LKT_GRAMMAR_RULE_DOC_RULE, LKT_GRAMMAR_RULE_IMPORT_STMT_RULE, LKT_GRAMMAR_RULE_IMPORTS_RULE, LKT_GRAMMAR_RULE_LEXER_DECL_RULE, LKT_GRAMMAR_RULE_GRAMMAR_DECL_RULE, LKT_GRAMMAR_RULE_GRAMMAR_RULE_RULE, LKT_GRAMMAR_RULE_LEXER_RULE_RULE, LKT_GRAMMAR_RULE_LEXER_FAMILY_DECL_RULE, LKT_GRAMMAR_RULE_LEXER_CASE_RULE_RULE, LKT_GRAMMAR_RULE_LEXER_CASE_ALT_RULE, LKT_GRAMMAR_RULE_LEXER_CASE_SEND_RULE, LKT_GRAMMAR_RULE_GRAMMAR_PRIMARY_RULE, LKT_GRAMMAR_RULE_GRAMMAR_EXPR_RULE, LKT_GRAMMAR_RULE_GRAMMAR_PICK_RULE, LKT_GRAMMAR_RULE_GRAMMAR_IMPLICIT_PICK_RULE, LKT_GRAMMAR_RULE_GRAMMAR_OPT_RULE, LKT_GRAMMAR_RULE_GRAMMAR_OPT_ERROR_RULE, LKT_GRAMMAR_RULE_GRAMMAR_CUT_RULE, LKT_GRAMMAR_RULE_GRAMMAR_STOPCUT_RULE, LKT_GRAMMAR_RULE_GRAMMAR_OR_EXPR_RULE, LKT_GRAMMAR_RULE_GRAMMAR_DISCARD_EXPR_RULE, LKT_GRAMMAR_RULE_TOKEN_LITERAL_RULE, LKT_GRAMMAR_RULE_TOKEN_NO_CASE_LITERAL_RULE, LKT_GRAMMAR_RULE_TOKEN_PATTERN_RULE, LKT_GRAMMAR_RULE_TOKEN_PATTERN_LITERAL_RULE, LKT_GRAMMAR_RULE_PARSE_NODE_EXPR_RULE, LKT_GRAMMAR_RULE_GRAMMAR_RULE_REF_RULE, LKT_GRAMMAR_RULE_GRAMMAR_LIST_EXPR_RULE, LKT_GRAMMAR_RULE_GRAMMAR_LIST_SEP_RULE, LKT_GRAMMAR_RULE_GRAMMAR_SKIP_RULE, LKT_GRAMMAR_RULE_GRAMMAR_NULL_RULE, LKT_GRAMMAR_RULE_GRAMMAR_TOKEN_RULE, LKT_GRAMMAR_RULE_TYPE_DECL_RULE, LKT_GRAMMAR_RULE_GENERIC_DECL_RULE, LKT_GRAMMAR_RULE_GENERIC_FORMAL_TYPE_RULE, LKT_GRAMMAR_RULE_ENUM_LIT_DECL_RULE, LKT_GRAMMAR_RULE_FUN_DECL_RULE, LKT_GRAMMAR_RULE_LAMBDA_ARG_DECL_RULE, LKT_GRAMMAR_RULE_FUN_ARG_DECL_RULE, LKT_GRAMMAR_RULE_FUN_ARG_LIST_RULE, LKT_GRAMMAR_RULE_LAMBDA_ARG_LIST_RULE, LKT_GRAMMAR_RULE_FIELD_DECL_RULE, LKT_GRAMMAR_RULE_BARE_DECL_RULE, LKT_GRAMMAR_RULE_DECL_RULE, LKT_GRAMMAR_RULE_TYPE_EXPR_RULE, LKT_GRAMMAR_RULE_TYPE_REF_RULE, LKT_GRAMMAR_RULE_TYPE_LIST_RULE, LKT_GRAMMAR_RULE_DECLS_RULE, LKT_GRAMMAR_RULE_DECL_BLOCK_RULE, LKT_GRAMMAR_RULE_VAL_DECL_RULE, LKT_GRAMMAR_RULE_DYNVAR_DECL_RULE, LKT_GRAMMAR_RULE_VAR_BIND_RULE, LKT_GRAMMAR_RULE_ENV_SPEC_ACTION_RULE, LKT_GRAMMAR_RULE_ENV_SPEC_DECL_RULE, LKT_GRAMMAR_RULE_BLOCK_RULE, LKT_GRAMMAR_RULE_EXPR_RULE, LKT_GRAMMAR_RULE_REL_RULE, LKT_GRAMMAR_RULE_EQ_RULE, LKT_GRAMMAR_RULE_ARITH_1_RULE, LKT_GRAMMAR_RULE_ARITH_2_RULE, LKT_GRAMMAR_RULE_ARITH_3_RULE, LKT_GRAMMAR_RULE_ISA_OR_PRIMARY_RULE, LKT_GRAMMAR_RULE_LOGIC_PROPAGATE_CALL_RULE, LKT_GRAMMAR_RULE_PRIMARY_RULE, LKT_GRAMMAR_RULE_MATCH_EXPR_RULE, LKT_GRAMMAR_RULE_NUM_LIT_RULE, LKT_GRAMMAR_RULE_BIG_NUM_LIT_RULE, LKT_GRAMMAR_RULE_STRING_LIT_RULE, LKT_GRAMMAR_RULE_BLOCK_STRING_LIT_RULE, LKT_GRAMMAR_RULE_CHAR_LIT_RULE, LKT_GRAMMAR_RULE_IF_EXPR_RULE, LKT_GRAMMAR_RULE_RAISE_EXPR_RULE, LKT_GRAMMAR_RULE_TRY_EXPR_RULE, LKT_GRAMMAR_RULE_ARRAY_LITERAL_RULE, LKT_GRAMMAR_RULE_CALLABLE_REF_RULE, LKT_GRAMMAR_RULE_BASIC_EXPR_RULE, LKT_GRAMMAR_RULE_TERM_RULE, LKT_GRAMMAR_RULE_BASIC_NAME_RULE, LKT_GRAMMAR_RULE_LAMBDA_EXPR_RULE, LKT_GRAMMAR_RULE_NULL_LIT_RULE, LKT_GRAMMAR_RULE_PARAM_RULE, LKT_GRAMMAR_RULE_PARAMS_RULE, LKT_GRAMMAR_RULE_DECL_ANNOTATION_PARAMS_RULE, LKT_GRAMMAR_RULE_DECL_ANNOTATION_RULE
   } lkt_grammar_rule;
   /*
    * Gramar rule to use for parsing.
    */

#define lkt_default_grammar_rule LKT_GRAMMAR_RULE_MAIN_RULE_RULE

/*
 * Enumerated type describing all possible exceptions that need to be handled
 * in the C bindings.
 */
typedef enum {
      EXCEPTION_FILE_READ_ERROR,
      EXCEPTION_BAD_TYPE_ERROR,
      EXCEPTION_OUT_OF_BOUNDS_ERROR,
      EXCEPTION_INVALID_INPUT,
      EXCEPTION_INVALID_SYMBOL_ERROR,
      EXCEPTION_INVALID_UNIT_NAME_ERROR,
      EXCEPTION_NATIVE_EXCEPTION,
      EXCEPTION_PRECONDITION_FAILURE,
      EXCEPTION_PROPERTY_ERROR,
      EXCEPTION_TEMPLATE_ARGS_ERROR,
      EXCEPTION_TEMPLATE_FORMAT_ERROR,
      EXCEPTION_TEMPLATE_INSTANTIATION_ERROR,
      EXCEPTION_STALE_REFERENCE_ERROR,
      EXCEPTION_SYNTAX_ERROR,
      EXCEPTION_UNKNOWN_CHARSET,
      EXCEPTION_MALFORMED_TREE_ERROR,
} lkt_exception_kind;

/*
 * Holder for native exceptions-related information.  Memory management for
 * this and all the fields is handled by the library: one just has to make sure
 * not to keep references to it.
 *
 * .. TODO: For the moment, this structure contains already formatted
 *    information, but depending on possible future Ada runtime improvements,
 *    this might change.
 */
typedef struct {
   /*
 * The kind of this exception.
 */
   lkt_exception_kind kind;

   /*
 * Message and context information associated with this exception.
 */
   const char *information;

   /*
 * Native stack trace associated to the exception as a multi-line human
 * readable trace. This string can be null if no trace is available.
 */
   const char *stack_trace;
} lkt_exception;

/*
 * Array types incomplete declarations
 */

        

typedef struct lkt_node_array_record *lkt_node_array;

        

typedef struct lkt_internal_logic_context_array_record *lkt_internal_logic_context_array;

        

typedef struct lkt_internal_solver_diagnostic_array_record *lkt_internal_solver_diagnostic_array;


/*
 * Iterator types incomplete declarations
 */

/*
 * An iterator provides a mean to retrieve values one-at-a-time.
 *
 * Currently, each iterator is bound to the analysis context used to create it.
 * Iterators are invalidated as soon as any unit of that analysis is reparsed.
 * Due to the nature of iterators (lazy computations), this invalidation is
 * necessary to avoid use of inconsistent state, such as an iterator trying to
 * use analysis context data that is stale.
 */



typedef void* lkt_node_iterator;



/*
 * Struct types declarations
 */

        



    typedef struct {
            uint32_t value;
            lkt_bool has_error;
            lkt_source_location error_sloc;
            lkt_string_type error_message;
    } lkt_internal_decoded_char_value;

    /* Increment the ref-count of all components in "r".  */
    extern void
    lkt_internal_decoded_char_value_inc_ref(lkt_internal_decoded_char_value *r);

    /* Decrement the ref-count of all components in "r".  */
    extern void
    lkt_internal_decoded_char_value_dec_ref(lkt_internal_decoded_char_value *r);


        



    typedef struct {
            lkt_string_type value;
            lkt_bool has_error;
            lkt_source_location error_sloc;
            lkt_string_type error_message;
    } lkt_internal_decoded_string_value;

    /* Increment the ref-count of all components in "r".  */
    extern void
    lkt_internal_decoded_string_value_inc_ref(lkt_internal_decoded_string_value *r);

    /* Decrement the ref-count of all components in "r".  */
    extern void
    lkt_internal_decoded_string_value_dec_ref(lkt_internal_decoded_string_value *r);


        



    typedef struct {char dummy;} lkt_internal_metadata;



        



    typedef struct {
            lkt_internal_metadata md;
            lkt_env_rebindings_type rebindings;
            lkt_bool from_rebound;
    } lkt_internal_entity_info;



        



    typedef struct {
            lkt_base_node node;
            lkt_internal_entity_info info;
    } lkt_node;



        



    typedef struct {
            lkt_node ref_node;
            lkt_node decl_node;
    } lkt_internal_logic_context;



        



    typedef struct {
            lkt_string_type message_template;
            lkt_node_array args;
            lkt_base_node location;
            lkt_internal_logic_context_array contexts;
            int round;
    } lkt_internal_solver_diagnostic;

    /* Increment the ref-count of all components in "r".  */
    extern void
    lkt_internal_solver_diagnostic_inc_ref(lkt_internal_solver_diagnostic *r);

    /* Decrement the ref-count of all components in "r".  */
    extern void
    lkt_internal_solver_diagnostic_dec_ref(lkt_internal_solver_diagnostic *r);


        



    typedef struct {
            lkt_bool success;
            lkt_internal_solver_diagnostic_array diagnostics;
    } lkt_internal_solver_result;

    /* Increment the ref-count of all components in "r".  */
    extern void
    lkt_internal_solver_result_inc_ref(lkt_internal_solver_result *r);

    /* Decrement the ref-count of all components in "r".  */
    extern void
    lkt_internal_solver_result_dec_ref(lkt_internal_solver_result *r);



/*
 * Types for event handler
 */

/*
 * Interface to handle events sent by the analysis context.
 */
typedef struct lkt_event_handler__struct *lkt_event_handler;

/*
 * Callback that will be called when a unit is requested from the context
 * ``Context``.
 *
 * ``Name`` is the name of the requested unit.
 *
 * ``From`` is the unit from which the unit was requested.
 *
 * ``Found`` indicates whether the requested unit was found or not.
 *
 * ``Is_Not_Found_Error`` indicates whether the fact that the unit was not
 * found is an error or not.
 *
 * .. warning:: The interface of this callback is probably subject to change,
 *    so should be treated as experimental.
 */
typedef void (*lkt_event_handler_unit_requested_callback)(
   void *data,
   lkt_analysis_context context,
   lkt_text *name,
   lkt_analysis_unit from,
   lkt_bool found,
   lkt_bool is_not_found_error
);

/*
 * Callback type for functions that are called when destroying an event
 * handler.
 */
typedef void (*lkt_event_handler_destroy_callback)(void *data);

/*
 * Callback that will be called when any unit is parsed from the context
 * ``Context``.
 *
 * ``Unit`` is the resulting unit.
 *
 * ``Reparsed`` indicates whether the unit was reparsed, or whether it was the
 * first parse.
 */
typedef void (*lkt_event_handler_unit_parsed_callback)(
   void *data,
   lkt_analysis_context context,
   lkt_analysis_unit unit,
   lkt_bool reparsed
);

/*
 * Types for file readers
 */

/*
 * Interface to override how source files are fetched and decoded.
 */
typedef struct lkt_file_reader__struct *lkt_file_reader;

/*
 * Callback type for functions that are called when destroying a file reader.
 */
typedef void (*lkt_file_reader_destroy_callback)(void *data);

/*
 * Callback type for functions that are called to fetch the decoded source
 * buffer for a requested filename.
 */
typedef void (*lkt_file_reader_read_callback)(
   void *data,
   const char *filename,
   const char *charset,
   int read_bom,
   lkt_text *buffer,
   lkt_diagnostic *diagnostic
);

/*
 * Types for unit providers
 */

/*
 * Interface to fetch analysis units from a name and a unit kind.
 *
 * The unit provider mechanism provides an abstraction which assumes that to
 * any couple (unit name, unit kind) we can associate at most one source file.
 * This means that several couples can be associated to the same source file,
 * but on the other hand, only one one source file can be associated to a
 * couple.
 *
 * This is used to make the semantic analysis able to switch from one analysis
 * units to another.
 *
 * See the documentation of each unit provider for the exact semantics of the
 * unit name/kind information.
 */
typedef struct lkt_unit_provider__struct *lkt_unit_provider;

/*
 * Types for introspection
 */

/* References to struct/node members.  */
typedef enum {
      lkt_member_ref_decoded_char_value_value
        = 1,
      lkt_member_ref_decoded_char_value_has_error
        = 2,
      lkt_member_ref_decoded_char_value_error_sloc
        = 3,
      lkt_member_ref_decoded_char_value_error_message
        = 4,
      lkt_member_ref_decoded_string_value_value
        = 5,
      lkt_member_ref_decoded_string_value_has_error
        = 6,
      lkt_member_ref_decoded_string_value_error_sloc
        = 7,
      lkt_member_ref_decoded_string_value_error_message
        = 8,
      lkt_member_ref_logic_context_ref_node
        = 9,
      lkt_member_ref_logic_context_decl_node
        = 10,
      lkt_member_ref_solver_diagnostic_message_template
        = 11,
      lkt_member_ref_solver_diagnostic_args
        = 12,
      lkt_member_ref_solver_diagnostic_location
        = 13,
      lkt_member_ref_solver_diagnostic_contexts
        = 14,
      lkt_member_ref_solver_diagnostic_round
        = 15,
      lkt_member_ref_solver_result_success
        = 16,
      lkt_member_ref_solver_result_diagnostics
        = 17,
      lkt_member_ref_base_lexer_case_rule_alt_f_send
        = 18,
      lkt_member_ref_lexer_case_rule_cond_alt_f_cond_exprs
        = 19,
      lkt_member_ref_decl_f_syn_name
        = 20,
      lkt_member_ref_base_grammar_rule_decl_f_expr
        = 21,
      lkt_member_ref_explicitly_typed_decl_f_decl_type
        = 22,
      lkt_member_ref_component_decl_f_default_val
        = 23,
      lkt_member_ref_fun_arg_decl_f_decl_annotations
        = 24,
      lkt_member_ref_val_decl_f_expr
        = 25,
      lkt_member_ref_fun_decl_f_args
        = 26,
      lkt_member_ref_fun_decl_f_return_type
        = 27,
      lkt_member_ref_fun_decl_f_body
        = 28,
      lkt_member_ref_env_spec_decl_f_actions
        = 29,
      lkt_member_ref_generic_decl_f_generic_formal_decls
        = 30,
      lkt_member_ref_generic_decl_f_decl
        = 31,
      lkt_member_ref_grammar_decl_f_rules
        = 32,
      lkt_member_ref_lexer_decl_f_rules
        = 33,
      lkt_member_ref_lexer_family_decl_f_rules
        = 34,
      lkt_member_ref_type_decl_f_traits
        = 35,
      lkt_member_ref_type_decl_f_syn_base_type
        = 36,
      lkt_member_ref_generic_formal_type_decl_f_has_class
        = 37,
      lkt_member_ref_named_type_decl_f_decls
        = 38,
      lkt_member_ref_enum_class_decl_f_branches
        = 39,
      lkt_member_ref_enum_type_decl_f_literals
        = 40,
      lkt_member_ref_decl_annotation_f_name
        = 41,
      lkt_member_ref_decl_annotation_f_params
        = 42,
      lkt_member_ref_decl_annotation_params_f_params
        = 43,
      lkt_member_ref_elsif_branch_f_cond_expr
        = 44,
      lkt_member_ref_elsif_branch_f_then_expr
        = 45,
      lkt_member_ref_enum_class_case_f_decls
        = 46,
      lkt_member_ref_any_of_f_expr
        = 47,
      lkt_member_ref_any_of_f_values
        = 48,
      lkt_member_ref_array_literal_f_exprs
        = 49,
      lkt_member_ref_array_literal_f_element_type
        = 50,
      lkt_member_ref_base_call_expr_f_name
        = 51,
      lkt_member_ref_base_call_expr_f_args
        = 52,
      lkt_member_ref_base_dot_expr_f_prefix
        = 53,
      lkt_member_ref_base_dot_expr_f_suffix
        = 54,
      lkt_member_ref_bin_op_f_left
        = 55,
      lkt_member_ref_bin_op_f_op
        = 56,
      lkt_member_ref_bin_op_f_right
        = 57,
      lkt_member_ref_block_expr_f_val_defs
        = 58,
      lkt_member_ref_block_expr_f_expr
        = 59,
      lkt_member_ref_cast_expr_f_expr
        = 60,
      lkt_member_ref_cast_expr_f_excludes_null
        = 61,
      lkt_member_ref_cast_expr_f_dest_type
        = 62,
      lkt_member_ref_error_on_null_f_expr
        = 63,
      lkt_member_ref_generic_instantiation_f_name
        = 64,
      lkt_member_ref_generic_instantiation_f_args
        = 65,
      lkt_member_ref_grammar_discard_f_expr
        = 66,
      lkt_member_ref_grammar_dont_skip_f_expr
        = 67,
      lkt_member_ref_grammar_dont_skip_f_dont_skip
        = 68,
      lkt_member_ref_grammar_list_f_list_type
        = 69,
      lkt_member_ref_grammar_list_f_kind
        = 70,
      lkt_member_ref_grammar_list_f_expr
        = 71,
      lkt_member_ref_grammar_list_f_sep
        = 72,
      lkt_member_ref_grammar_null_f_name
        = 73,
      lkt_member_ref_grammar_opt_f_expr
        = 74,
      lkt_member_ref_grammar_opt_error_f_expr
        = 75,
      lkt_member_ref_grammar_opt_error_group_f_expr
        = 76,
      lkt_member_ref_grammar_opt_group_f_expr
        = 77,
      lkt_member_ref_grammar_or_expr_f_sub_exprs
        = 78,
      lkt_member_ref_grammar_pick_f_exprs
        = 79,
      lkt_member_ref_grammar_predicate_f_expr
        = 80,
      lkt_member_ref_grammar_predicate_f_prop_ref
        = 81,
      lkt_member_ref_grammar_rule_ref_f_node_name
        = 82,
      lkt_member_ref_grammar_skip_f_name
        = 83,
      lkt_member_ref_grammar_stop_cut_f_expr
        = 84,
      lkt_member_ref_parse_node_expr_f_node_name
        = 85,
      lkt_member_ref_parse_node_expr_f_sub_exprs
        = 86,
      lkt_member_ref_token_no_case_lit_f_lit
        = 87,
      lkt_member_ref_token_pattern_concat_f_left
        = 88,
      lkt_member_ref_token_pattern_concat_f_right
        = 89,
      lkt_member_ref_token_ref_f_token_name
        = 90,
      lkt_member_ref_token_ref_f_expr
        = 91,
      lkt_member_ref_if_expr_f_cond_expr
        = 92,
      lkt_member_ref_if_expr_f_then_expr
        = 93,
      lkt_member_ref_if_expr_f_alternatives
        = 94,
      lkt_member_ref_if_expr_f_else_expr
        = 95,
      lkt_member_ref_isa_f_expr
        = 96,
      lkt_member_ref_isa_f_dest_type
        = 97,
      lkt_member_ref_keep_expr_f_expr
        = 98,
      lkt_member_ref_keep_expr_f_keep_type
        = 99,
      lkt_member_ref_lambda_expr_f_params
        = 100,
      lkt_member_ref_lambda_expr_f_return_type
        = 101,
      lkt_member_ref_lambda_expr_f_body
        = 102,
      lkt_member_ref_null_lit_f_dest_type
        = 103,
      lkt_member_ref_block_string_lit_f_lines
        = 104,
      lkt_member_ref_logic_assign_f_dest_var
        = 105,
      lkt_member_ref_logic_assign_f_value
        = 106,
      lkt_member_ref_logic_expr_f_expr
        = 107,
      lkt_member_ref_logic_propagate_f_dest_var
        = 108,
      lkt_member_ref_logic_propagate_f_call
        = 109,
      lkt_member_ref_logic_unify_f_lhs
        = 110,
      lkt_member_ref_logic_unify_f_rhs
        = 111,
      lkt_member_ref_match_expr_f_match_expr
        = 112,
      lkt_member_ref_match_expr_f_branches
        = 113,
      lkt_member_ref_not_expr_f_expr
        = 114,
      lkt_member_ref_paren_expr_f_expr
        = 115,
      lkt_member_ref_raise_expr_f_dest_type
        = 116,
      lkt_member_ref_raise_expr_f_except_expr
        = 117,
      lkt_member_ref_subscript_expr_f_prefix
        = 118,
      lkt_member_ref_subscript_expr_f_index
        = 119,
      lkt_member_ref_try_expr_f_try_expr
        = 120,
      lkt_member_ref_try_expr_f_or_expr
        = 121,
      lkt_member_ref_un_op_f_op
        = 122,
      lkt_member_ref_un_op_f_expr
        = 123,
      lkt_member_ref_full_decl_f_doc
        = 124,
      lkt_member_ref_full_decl_f_decl_annotations
        = 125,
      lkt_member_ref_full_decl_f_decl
        = 126,
      lkt_member_ref_grammar_list_sep_f_token
        = 127,
      lkt_member_ref_grammar_list_sep_f_extra
        = 128,
      lkt_member_ref_import_f_name
        = 129,
      lkt_member_ref_langkit_root_f_imports
        = 130,
      lkt_member_ref_langkit_root_f_decls
        = 131,
      lkt_member_ref_lexer_case_rule_f_expr
        = 132,
      lkt_member_ref_lexer_case_rule_f_alts
        = 133,
      lkt_member_ref_lexer_case_rule_send_f_sent
        = 134,
      lkt_member_ref_lexer_case_rule_send_f_match_size
        = 135,
      lkt_member_ref_match_branch_f_decl
        = 136,
      lkt_member_ref_match_branch_f_expr
        = 137,
      lkt_member_ref_param_f_name
        = 138,
      lkt_member_ref_param_f_value
        = 139,
      lkt_member_ref_function_type_ref_f_args_types
        = 140,
      lkt_member_ref_function_type_ref_f_return_type
        = 141,
      lkt_member_ref_generic_type_ref_f_type_name
        = 142,
      lkt_member_ref_generic_type_ref_f_params
        = 143,
      lkt_member_ref_simple_type_ref_f_type_name
        = 144,
      lkt_member_ref_var_bind_f_name
        = 145,
      lkt_member_ref_var_bind_f_expr
        = 146,
      lkt_member_ref_lkt_node_p_set_solver_debug_mode
        = 147,
      lkt_member_ref_lkt_node_p_basic_trait_gen
        = 148,
      lkt_member_ref_lkt_node_p_basic_trait
        = 149,
      lkt_member_ref_lkt_node_p_node_gen_trait
        = 150,
      lkt_member_ref_lkt_node_p_node_trait
        = 151,
      lkt_member_ref_lkt_node_p_indexable_gen_trait
        = 152,
      lkt_member_ref_lkt_node_p_indexable_trait
        = 153,
      lkt_member_ref_lkt_node_p_token_node_trait
        = 154,
      lkt_member_ref_lkt_node_p_error_node_trait
        = 155,
      lkt_member_ref_lkt_node_p_char_type
        = 156,
      lkt_member_ref_lkt_node_p_int_type
        = 157,
      lkt_member_ref_lkt_node_p_bool_type
        = 158,
      lkt_member_ref_lkt_node_p_bigint_type
        = 159,
      lkt_member_ref_lkt_node_p_string_type
        = 160,
      lkt_member_ref_lkt_node_p_symbol_type
        = 161,
      lkt_member_ref_lkt_node_p_property_error_type
        = 162,
      lkt_member_ref_lkt_node_p_regexp_type
        = 163,
      lkt_member_ref_lkt_node_p_entity_gen_type
        = 164,
      lkt_member_ref_lkt_node_p_entity_type
        = 165,
      lkt_member_ref_lkt_node_p_logicvar_type
        = 166,
      lkt_member_ref_lkt_node_p_equation_type
        = 167,
      lkt_member_ref_lkt_node_p_array_gen_type
        = 168,
      lkt_member_ref_lkt_node_p_array_type
        = 169,
      lkt_member_ref_lkt_node_p_astlist_gen_type
        = 170,
      lkt_member_ref_lkt_node_p_astlist_type
        = 171,
      lkt_member_ref_lkt_node_p_node_builder_gen_type
        = 172,
      lkt_member_ref_lkt_node_p_node_builder_type
        = 173,
      lkt_member_ref_lkt_node_p_iterator_gen_trait
        = 174,
      lkt_member_ref_lkt_node_p_iterator_trait
        = 175,
      lkt_member_ref_lkt_node_p_analysis_unit_gen_trait
        = 176,
      lkt_member_ref_lkt_node_p_analysis_unit_trait
        = 177,
      lkt_member_ref_lkt_node_p_topmost_invalid_decl
        = 178,
      lkt_member_ref_lkt_node_p_nameres_diagnostics
        = 179,
      lkt_member_ref_lkt_node_p_solve_enclosing_context
        = 180,
      lkt_member_ref_lkt_node_p_xref_entry_point
        = 181,
      lkt_member_ref_parent
        = 182,
      lkt_member_ref_parents
        = 183,
      lkt_member_ref_children
        = 184,
      lkt_member_ref_token_start
        = 185,
      lkt_member_ref_token_end
        = 186,
      lkt_member_ref_child_index
        = 187,
      lkt_member_ref_previous_sibling
        = 188,
      lkt_member_ref_next_sibling
        = 189,
      lkt_member_ref_unit
        = 190,
      lkt_member_ref_is_ghost
        = 191,
      lkt_member_ref_full_sloc_image
        = 192,
      lkt_member_ref_class_qualifier_p_as_bool
        = 193,
      lkt_member_ref_decl_p_custom_image
        = 194,
      lkt_member_ref_decl_p_decl_type_name
        = 195,
      lkt_member_ref_decl_p_as_bare_decl
        = 196,
      lkt_member_ref_decl_p_get_type
        = 197,
      lkt_member_ref_decl_p_get_cast_type
        = 198,
      lkt_member_ref_decl_p_get_keep_type
        = 199,
      lkt_member_ref_decl_p_get_suffix_type
        = 200,
      lkt_member_ref_decl_p_is_generic
        = 201,
      lkt_member_ref_decl_p_return_type_is_instantiated
        = 202,
      lkt_member_ref_decl_p_is_instantiated
        = 203,
      lkt_member_ref_decl_p_name
        = 204,
      lkt_member_ref_decl_p_full_name
        = 205,
      lkt_member_ref_fun_decl_p_is_dynamic_combiner
        = 206,
      lkt_member_ref_type_decl_p_base_type
        = 207,
      lkt_member_ref_type_decl_p_base_type_if_entity
        = 208,
      lkt_member_ref_excludes_null_p_as_bool
        = 209,
      lkt_member_ref_expr_p_get_type
        = 210,
      lkt_member_ref_expr_p_get_generic_type
        = 211,
      lkt_member_ref_expr_p_get_expected_type
        = 212,
      lkt_member_ref_expr_p_referenced_decl
        = 213,
      lkt_member_ref_token_lit_p_denoted_value
        = 214,
      lkt_member_ref_token_pattern_lit_p_denoted_value
        = 215,
      lkt_member_ref_id_p_custom_image
        = 216,
      lkt_member_ref_char_lit_p_denoted_value
        = 217,
      lkt_member_ref_string_lit_p_denoted_value
        = 218,
      lkt_member_ref_string_lit_p_is_prefixed_string
        = 219,
      lkt_member_ref_string_lit_p_prefix
        = 220,
      lkt_member_ref_string_lit_p_is_regexp_literal
        = 221,
      lkt_member_ref_full_decl_p_has_annotation
        = 222,
      lkt_member_ref_import_p_referenced_unit
        = 223,
      lkt_member_ref_type_ref_p_referenced_decl
        = 224,
} lkt_introspection_member_ref;

/*
 * Types for tree rewriting
 */

/*
 * Handle for an analysis context rewriting session
 */
typedef struct lkt_rewriting_handle__struct *lkt_rewriting_handle;

/*
 * Handle for the process of rewriting an analysis unit. Such handles are owned
 * by a Rewriting_Handle instance.
 */
typedef struct lkt_unit_rewriting_handle__struct *lkt_unit_rewriting_handle;

/*
 * Handle for the process of rewriting an AST node. Such handles are owned by a
 * Rewriting_Handle instance.
 */
typedef struct lkt_node_rewriting_handle__struct *lkt_node_rewriting_handle;

/*
 * Result of applying a rewriting session.
 *
 * On success, ``Success`` is true.
 *
 * On failure, ``Success`` is false, ``Unit`` is set to the unit on which
 * rewriting failed, and ``Diagnostics`` is set to related rewriting errors.
 */
typedef struct {
    int success;
    lkt_analysis_unit unit;
    int diagnostics_count;
    lkt_diagnostic *diagnostics;
} lkt_rewriting_apply_result;

/* All the functions below can potentially raise an exception, so
   lkt_get_last_exception must be checked after them even
   before trying to use the returned value.  */


/*
 * Array types declarations
 */

        



/*

 */
struct lkt_node_array_record {
   int n;
   int ref_count;
   lkt_node items[1];
};

/* Create a length-sized array.  */
extern lkt_node_array
lkt_node_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
lkt_node_array_inc_ref(lkt_node_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
lkt_node_array_dec_ref(lkt_node_array a);


        



/*

 */
struct lkt_internal_logic_context_array_record {
   int n;
   int ref_count;
   lkt_internal_logic_context items[1];
};

/* Create a length-sized array.  */
extern lkt_internal_logic_context_array
lkt_internal_logic_context_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
lkt_internal_logic_context_array_inc_ref(lkt_internal_logic_context_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
lkt_internal_logic_context_array_dec_ref(lkt_internal_logic_context_array a);


        



/*

 */
struct lkt_internal_solver_diagnostic_array_record {
   int n;
   int ref_count;
   lkt_internal_solver_diagnostic items[1];
};

/* Create a length-sized array.  */
extern lkt_internal_solver_diagnostic_array
lkt_internal_solver_diagnostic_array_create(int length);

/* Increment the ref-count for "a".  */
extern void
lkt_internal_solver_diagnostic_array_inc_ref(lkt_internal_solver_diagnostic_array a);

/* Decrement the ref-count for "a". This deallocates it if the ref-count drops
   to 0.  */
extern void
lkt_internal_solver_diagnostic_array_dec_ref(lkt_internal_solver_diagnostic_array a);



/*
 * Iterator types declarations
 */





/*
 * Set the next value from the iterator in the given element pointer. Return
 * ``1`` if successful, otherwise ``0``.
 *
 * This raises a ``Stale_Reference_Error`` exception if the iterator is
 * invalidated.
 */
extern int
lkt_node_iterator_next(lkt_node_iterator i, lkt_node* e);

/* Increment the ref-count for "i".  */
extern void
lkt_node_iterator_inc_ref(lkt_node_iterator i);

/* Decrement the ref-count for "i". This deallocates it if the ref-count drops
   to 0.  */
extern void
lkt_node_iterator_dec_ref(lkt_node_iterator i);




/*
 * Analysis primitives
 */

/*
 * Allocate a new analysis context.
 */
extern lkt_analysis_context
lkt_allocate_analysis_context (void);

/*
 * Initialize an analysis context. Must be called right after
 * ``Allocate_Context`` on its result.
 *
 * Having separate primitives for allocation/initialization allows library
 * bindings to have a context wrapper (created between the two calls) ready
 * when callbacks that happen during context initialization (for instance "unit
 * parsed" events).
 */
extern void
lkt_initialize_analysis_context(
   lkt_analysis_context context,
   const char *charset,
   lkt_file_reader file_reader,
   lkt_unit_provider unit_provider,
   lkt_event_handler event_handler,
   int with_trivia,
   int tab_stop
);

/*
 * Increase the reference count to an analysis context. Return the reference
 * for convenience.
 */
extern lkt_analysis_context
lkt_context_incref(lkt_analysis_context context);

/*
 * Decrease the reference count to an analysis context. Destruction happens
 * when the ref-count reaches 0.
 */
extern void
lkt_context_decref(lkt_analysis_context context);

/*
 * If the given string is a valid symbol, yield it as a symbol and return true.
 * Otherwise, return false.
 */
extern int
lkt_context_symbol(lkt_analysis_context context,
                                   lkt_text *text,
                                   lkt_symbol_type *symbol);

/*
 * Debug helper. Set whether ``Property_Error`` exceptions raised in
 * ``Populate_Lexical_Env`` should be discarded. They are by default.
 */
extern void
lkt_context_discard_errors_in_populate_lexical_env(
        lkt_analysis_context context,
        int discard);

/*
 * Create a new analysis unit for ``Filename`` or return the existing one if
 * any. If ``Reparse`` is true and the analysis unit already exists, reparse it
 * from ``Filename``.
 *
 * ``Rule`` controls which grammar rule is used to parse the unit.
 *
 * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
 * use the context's default charset.
 *
 * If any failure occurs, such as file opening, decoding, lexing or parsing
 * failure, return an analysis unit anyway: errors are described as diagnostics
 * of the returned analysis unit.
 */
extern lkt_analysis_unit
lkt_get_analysis_unit_from_file(
        lkt_analysis_context context,
        const char *filename,
        const char *charset,
        int reparse,
        lkt_grammar_rule rule);

/*
 * Create a new analysis unit for ``Filename`` or return the existing one if
 * any. Whether the analysis unit already exists or not, (re)parse it from the
 * source code in ``Buffer``.
 *
 * ``Rule`` controls which grammar rule is used to parse the unit.
 *
 * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
 * use the context's default charset.
 *
 * If any failure occurs, such as file opening, decoding, lexing or parsing
 * failure, return an analysis unit anyway: errors are described as diagnostics
 * of the returned analysis unit.
 */
extern lkt_analysis_unit
lkt_get_analysis_unit_from_buffer(
        lkt_analysis_context context,
        const char *filename,
        const char *charset,
        const char *buffer,
        size_t buffer_size,
        lkt_grammar_rule rule);

/*
 * Create a new analysis unit for ``Name``/``Kind`` or return the existing one
 * if any. If ``Reparse`` is true and the analysis unit already exists, reparse
 * it from the on-disk source file.
 *
 * The ``Name`` and ``Kind`` arguments are forwarded directly to query the
 * context's unit provider and get the filename for the returned unit. See the
 * documentation of the relevant unit provider for their exact semantics.
 *
 * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
 * use the context's default charset.
 *
 * If the unit name cannot be tuned into a file name, return ``NULL``. If any
 * other failure occurs, such as file opening, decoding, lexing or parsing
 * failure, return an analysis unit anyway: errors are described as diagnostics
 * of the returned analysis unit.
 */
extern lkt_analysis_unit
lkt_get_analysis_unit_from_provider(
        lkt_analysis_context context,
        lkt_text *name,
        lkt_analysis_unit_kind kind,
        const char *charset,
        int reparse);

/*
 * Return the root node for this unit, or ``NULL`` if there is none.
 */
extern void
lkt_unit_root(lkt_analysis_unit unit,
                              lkt_node *result_p);

/*
 * Return a reference to the first token scanned in this unit.
 */
extern void
lkt_unit_first_token(lkt_analysis_unit unit,
                                     lkt_token *token);

/*
 * Return a reference to the last token scanned in this unit.
 */
extern void
lkt_unit_last_token(lkt_analysis_unit unit,
                                    lkt_token *token);

/*
 * Return the number of tokens in this unit.
 */
extern int
lkt_unit_token_count(lkt_analysis_unit unit);

/*
 * Return the number of trivias in this unit. This is 0 for units that were
 * parsed with trivia analysis disabled.
 */
extern int
lkt_unit_trivia_count(lkt_analysis_unit unit);

/*
 * Debug helper: output the lexical envs for the given analysis unit.
 */
extern void
lkt_unit_dump_lexical_env(lkt_analysis_unit unit);

/*
 * Return the filename this unit is associated to.
 *
 * The returned string is dynamically allocated and the caller must free it
 * when done with it.
 */
extern char *
lkt_unit_filename(lkt_analysis_unit unit);

/*
 * Return the number of diagnostics associated to this unit.
 */
extern unsigned
lkt_unit_diagnostic_count(lkt_analysis_unit unit);

/*
 * Get the Nth diagnostic in this unit and store it into ``*diagnostic_p``.
 * Return zero on failure (when N is too big).
 */
extern int
lkt_unit_diagnostic(lkt_analysis_unit unit,
                                    unsigned n,
                                    lkt_diagnostic *diagnostic_p);

/*
 * Return the context that owns this unit.
 */
extern lkt_analysis_context
lkt_unit_context(lkt_analysis_unit context);

/*
 * Reparse an analysis unit from the associated file.
 *
 * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
 * use the context's default charset.
 *
 * If any failure occurs, such as decoding, lexing or parsing failure,
 * diagnostic are emitted to explain what happened.
 */
extern void
lkt_unit_reparse_from_file(lkt_analysis_unit unit,
                                           const char *charset);

/*
 * Reparse an analysis unit from a buffer.
 *
 * Use ``Charset`` in order to decode the source. If ``Charset`` is empty then
 * use the context's default charset.
 *
 * If any failure occurs, such as decoding, lexing or parsing failure,
 * diagnostic are emitted to explain what happened.
 */
extern void
lkt_unit_reparse_from_buffer (lkt_analysis_unit unit,
                                              const char *charset,
                                              const char *buffer,
                                              size_t buffer_size);

/*
 * Create lexical environments for this analysis unit, according to the
 * specifications given in the language spec.
 *
 * If not done before, it will be automatically called during semantic
 * analysis. Calling it before enables one to control where the latency occurs.
 *
 * Depending on whether errors are discarded (see
 * ``Discard_Errors_In_Populate_Lexical_Env``), return ``0`` on failure and
 * ``1`` on success.
 */
extern int
lkt_unit_populate_lexical_env(
    lkt_analysis_unit unit
);

/*
 * General AST node primitives
 */

/*
 * Create an entity with null entity info for a given node.
 */
extern void
lkt_create_bare_entity(
    lkt_base_node node,
    lkt_node *entity
);

/*
 * Return whether this node is a null node reference.
 */
static inline int
lkt_node_is_null(lkt_node *node) {
    return node->node == NULL;
}

/*
 * Return the kind of this node.
 */
extern lkt_node_kind_enum
lkt_node_kind(lkt_node *node);

/*
 * Helper for textual dump: return the kind name for this node. The returned
 * string is a copy and thus must be free'd by the caller.
 */
extern void
lkt_kind_name(lkt_node_kind_enum kind, lkt_text *result);

/*
 * Return the analysis unit that owns this node.
 */
extern lkt_analysis_unit
lkt_node_unit(lkt_node *node);

/*
 * Return a hash for the given node.
 */
extern uint32_t
lkt_node_hash(lkt_node *node);

/*
 * Return whether the two nodes are equivalent.
 */
extern lkt_bool
lkt_node_is_equivalent(lkt_node *l, lkt_node *r);

/*
 * Return whether this node is a node that contains only a single token.
 */
extern int
lkt_node_is_token_node(lkt_node *node);

/*
 * Return whether this node is synthetic.
 */
extern int
lkt_node_is_synthetic(lkt_node *node);

/*
 * Return a representation of this node as a string.
 */
extern void
lkt_node_image(lkt_node *node,
                               lkt_text *result);

/*
 * Return the source buffer slice corresponding to the text that spans between
 * the first and the last tokens of this node.
 *
 * Note that this returns the empty string for synthetic nodes.
 */
extern void
lkt_node_text(lkt_node *node,
                              lkt_text *text);

/*
 * Return the spanning source location range for this node.
 *
 * Note that this returns the sloc of the parent for synthetic nodes.
 */
extern void
lkt_node_sloc_range(lkt_node *node,
                                    lkt_source_location_range *sloc_range);

/*
 * Return the bottom-most node from in ``Node`` and its children which contains
 * ``Sloc``, or ``NULL`` if there is none.
 */
extern void
lkt_lookup_in_node(lkt_node *node,
                                   const lkt_source_location *sloc,
                                   lkt_node *result_p);

/*
 * Return the number of children in this node.
 */
extern unsigned
lkt_node_children_count(lkt_node *node);

/*
 * Return the Nth child for in this node's fields and store it into
 * ``*child_p``.  Return zero on failure (when ``N`` is too big).
 */
extern int
lkt_node_child(lkt_node *node,
                               unsigned n,
                               lkt_node* child_p);

/*
 * Encode some text using the current locale. The result is dynamically
 * allocated: it is up to the caller to free it when done with it.
 *
 * This is a development helper to make it quick and easy to print token and
 * diagnostic text: it ignores errors (when the locale does not support some
 * characters). Production code should use real conversion routines such as
 * libiconv's in order to deal with UTF-32 texts.
 */
extern char *
lkt_text_to_locale_string(lkt_text *text);

/*
 * Encode some text to a newly allocated UTF-8 buffer (``bytes``). The size of
 * this buffer is stored in ``length``, and the actual allocated buffer has one
 * extra NUL byte (note that it is valid for the first ``length`` bytes in
 * ``bytes`` to contain NUL bytes).
 */
extern void
lkt_text_to_utf8(lkt_text *text,
                                 char **bytes,
                                 size_t *length);

/*
 * Decode a UTF-8 buffer (``bytes``, of size ``length``) to a text buffer.
 */
extern void
lkt_text_from_utf8(const char *bytes,
                                   size_t length,
                                   lkt_text *text);

/*
 * Encode the given character to a newly allocated UTF-8 buffer (``bytes``).
 * The size of this buffer is stored in ``length``.
 */
extern void
lkt_char_to_utf8(uint32_t codepoint,
                                 char **bytes,
                                 size_t *length);

/*
 * Decode a UTF-8 buffer (``bytes``, of size ``length``) to a text buffer. Note
 * that the UTF-8 buffer is supposed to contain only one codepoint.
 */
extern void
lkt_char_from_utf8(const char *bytes,
                                   size_t length,
                                   uint32_t *codepoint);

/*
 * Encode some string to a newly allocated UTF-8 buffer (``bytes``). The size
 * of this buffer is stored in ``length``, and the actual allocated buffer has
 * one extra NUL byte (note that it is valid for the first ``length`` bytes in
 * ``bytes`` to contain NUL bytes).
 */
extern void
lkt_string_to_utf8(lkt_string_type string,
                                   char **bytes,
                                   size_t *length);

/*
 * Decode a UTF-8 buffer (``bytes``, of size ``length``) to a string buffer.
 */
extern void
lkt_string_from_utf8(const char *bytes,
                                     size_t length,
                                     lkt_string_type *string);

/*
 * Free dynamically allocated memory.
 *
 * This is a helper to free objects from dynamic languages.
 */
extern void
lkt_free(void *address);

/*
 * If this text object owns the buffer it references, free this buffer.
 *
 * Note that even though this accepts a pointer to a text object, it does not
 * deallocates the text object itself but rather the buffer it references.
 */
extern void
lkt_destroy_text(lkt_text *text);

/*
 * Return the text associated to this symbol.
 */
extern void
lkt_symbol_text(lkt_symbol_type *symbol,
                                lkt_text *text);

/*
 * Create a big integer from its string representation (in base 10).
 */
extern lkt_big_integer
lkt_create_big_integer(lkt_text *text);

/*
 * Return the string representation (in base 10) of this big integer.
 */
extern void
lkt_big_integer_text(lkt_big_integer bigint,
                                     lkt_text *text);

/*
 * Decrease the reference count for this big integer.
 */
extern void
lkt_big_integer_decref(lkt_big_integer bigint);

/*
 * Allocate strings to represent the library version number and build date and
 * put them in Version/Build_Date. Callers are expected to call free() on the
 * returned string once done.
 */
extern void
lkt_get_versions(char **version, char **build_date);

/*
 * Create a string value from its content (UTF32 with native endianity).
 *
 * Note that the CONTENT buffer argument is copied: the returned value does not
 * contain a reference to it.
 */
extern lkt_string_type
lkt_create_string(uint32_t *content, int length);

/*
 * Decrease the reference count for this string.
 */
extern void
lkt_string_dec_ref(lkt_string_type self);

/*
 * Kind-specific AST node primitives
 */

/* All these primitives return their result through an OUT parameter.  They
   return a boolean telling whether the operation was successful (it can fail
   if the node does not have the proper type, for instance).  When an AST node
   is returned, its ref-count is left as-is.  */

        



/*
 * Enable or disable the solver traces for debugging purposes.
 */
extern int lkt_lkt_node_p_set_solver_debug_mode(
    lkt_node *node,

        
        lkt_bool
        enable,

    lkt_bool *value_p
);


        



/*
 * Unit method. Return the ``BasicTrait`` builtin generic trait.
 */
extern int lkt_lkt_node_p_basic_trait_gen(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ``BasicTrait`` builtin trait.
 */
extern int lkt_lkt_node_p_basic_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ``Node`` builtin generic trait.
 */
extern int lkt_lkt_node_p_node_gen_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ``Node`` builtin trait.
 */
extern int lkt_lkt_node_p_node_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ``Node`` builtin generic trait.
 */
extern int lkt_lkt_node_p_indexable_gen_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ``Node`` builtin trait.
 */
extern int lkt_lkt_node_p_indexable_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ``TokenNode`` builtin trait.
 */
extern int lkt_lkt_node_p_token_node_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ``ErrorNode`` builtin trait.
 */
extern int lkt_lkt_node_p_error_node_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the character builtin type.
 */
extern int lkt_lkt_node_p_char_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the integer builtin type.
 */
extern int lkt_lkt_node_p_int_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the boolean builtin type.
 */
extern int lkt_lkt_node_p_bool_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the big integer builtin type.
 */
extern int lkt_lkt_node_p_bigint_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the string builtin type.
 */
extern int lkt_lkt_node_p_string_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the string builtin type.
 */
extern int lkt_lkt_node_p_symbol_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the property error builtin type.
 */
extern int lkt_lkt_node_p_property_error_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the regexp builtin type.
 */
extern int lkt_lkt_node_p_regexp_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the logicvar builtin type.
 */
extern int lkt_lkt_node_p_entity_gen_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the logicvar builtin type.
 */
extern int lkt_lkt_node_p_entity_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the logicvar builtin type.
 */
extern int lkt_lkt_node_p_logicvar_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the logicvar builtin type.
 */
extern int lkt_lkt_node_p_equation_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the array builtin generic type.
 */
extern int lkt_lkt_node_p_array_gen_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the array builtin type.
 */
extern int lkt_lkt_node_p_array_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ASTList builtin generic type.
 */
extern int lkt_lkt_node_p_astlist_gen_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ASTList builtin type.
 */
extern int lkt_lkt_node_p_astlist_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the NodeBuilder builtin generic type.
 */
extern int lkt_lkt_node_p_node_builder_gen_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the NodeBuilder builtin type.
 */
extern int lkt_lkt_node_p_node_builder_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the Iterator builtin generic trait.
 */
extern int lkt_lkt_node_p_iterator_gen_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the Iterator builtin trait.
 */
extern int lkt_lkt_node_p_iterator_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ``AnalysisUnit`` builtin generic trait.
 */
extern int lkt_lkt_node_p_analysis_unit_gen_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Unit method. Return the ``AnalysisUnit`` builtin trait.
 */
extern int lkt_lkt_node_p_analysis_unit_trait(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the topmost (from ``Self`` to the root node) FullDecl annotated with
 * ``@invalid``, null otherwise.
 */
extern int lkt_lkt_node_p_topmost_invalid_decl(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * If name resolution on this lkt compilation unit fails, this returns all the
 * diagnostics that were produced while resolving it.
 */
extern int lkt_lkt_node_p_nameres_diagnostics(
    lkt_node *node,


    lkt_internal_solver_diagnostic_array *value_p
);


        



/*
 * Finds the nearest parent that is an xref_entry_point and solve its equation.
 */
extern int lkt_lkt_node_p_solve_enclosing_context(
    lkt_node *node,


    lkt_internal_solver_result *value_p
);


        



/*
 * Designates entities that are entry point for the xref solving
 * infrastructure. If this returns true, then nameres_diagnostics can be called
 * on it.
 */
extern int lkt_lkt_node_p_xref_entry_point(
    lkt_node *node,


    lkt_bool *value_p
);


        



/*
 * Return the syntactic parent for this node. Return null for the root node.
 */
extern int lkt_lkt_node_parent(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return an array that contains the lexical parents, this node included iff
 * ``with_self`` is True. Nearer parents are first in the list.
 */
extern int lkt_lkt_node_parents(
    lkt_node *node,

        
        lkt_bool
        with_self,

    lkt_node_array *value_p
);


        



/*
 * Return an array that contains the direct lexical children.
 *
 * .. warning:: This constructs a whole array every-time you call it, and as
 *    such is less efficient than calling the ``Child`` built-in.
 */
extern int lkt_lkt_node_children(
    lkt_node *node,


    lkt_node_array *value_p
);


        



/*
 * Return the first token used to parse this node.
 */
extern int lkt_lkt_node_token_start(
    lkt_node *node,


    lkt_token *value_p
);


        



/*
 * Return the last token used to parse this node.
 */
extern int lkt_lkt_node_token_end(
    lkt_node *node,


    lkt_token *value_p
);


        



/*
 * Return the 0-based index for Node in its parent's children.
 */
extern int lkt_lkt_node_child_index(
    lkt_node *node,


    int *value_p
);


        



/*
 * Return the node's previous sibling, or null if there is no such sibling.
 */
extern int lkt_lkt_node_previous_sibling(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the node's next sibling, or null if there is no such sibling.
 */
extern int lkt_lkt_node_next_sibling(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the analysis unit owning this node.
 */
extern int lkt_lkt_node_unit(
    lkt_node *node,


    lkt_analysis_unit *value_p
);


        



/*
 * Return whether the node is a ghost.
 *
 * Unlike regular nodes, ghost nodes cover no token in the input source: they
 * are logically located instead between two tokens. Both the ``token_start``
 * and the ``token_end`` of all ghost nodes is the token right after this
 * logical position.
 */
extern int lkt_lkt_node_is_ghost(
    lkt_node *node,


    lkt_bool *value_p
);


        



/*
 * Return a string containing the filename + the sloc in GNU conformant format.
 * Useful to create diagnostics from a node.
 */
extern int lkt_lkt_node_full_sloc_image(
    lkt_node *node,


    lkt_string_type *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_base_lexer_case_rule_alt_f_send(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_lexer_case_rule_cond_alt_f_cond_exprs(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return whether this node is present
 */
extern int lkt_class_qualifier_p_as_bool(
    lkt_node *node,


    lkt_bool *value_p
);


        



/*
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_decl_f_syn_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the image string using entity information.
 */
extern int lkt_decl_p_custom_image(
    lkt_node *node,


    lkt_string_type *value_p
);


        



/*
 * Return the name of the declaration type, as it should be seen by users/shown
 * in diagnostics.
 */
extern int lkt_decl_p_decl_type_name(
    lkt_node *node,


    lkt_string_type *value_p
);


        



/*
 * Get this declaration without rebindings information.
 */
extern int lkt_decl_p_as_bare_decl(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the type of the Decl.
 */
extern int lkt_decl_p_get_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * If we are casting an entity (Self) to something that is not an entity, make
 * it an entity.
 */
extern int lkt_decl_p_get_cast_type(
    lkt_node *node,

        
        const lkt_node*
        cast_to,

    lkt_node *value_p
);


        



/*
 * Return the type of Entity when we only keep elements of type keep_type. If
 * we are casting an entity (Self) to something that is not an entity, make it
 * an entity.
 */
extern int lkt_decl_p_get_keep_type(
    lkt_node *node,

        
        const lkt_node*
        keep_type,

    lkt_node *value_p
);


        



/*
 * If we are accessing a ParseField of an entity, then that field's type also
 * needs to be an entity.
 */
extern int lkt_decl_p_get_suffix_type(
    lkt_node *node,

        
        const lkt_node*
        prefix_type,

    lkt_node *value_p
);


        



/*
 * Returns wether the Decl is generic.
 */
extern int lkt_decl_p_is_generic(
    lkt_node *node,


    lkt_bool *value_p
);


        



/*
 * Return True if the return type of this function is instantiated.
 */
extern int lkt_decl_p_return_type_is_instantiated(
    lkt_node *node,


    lkt_bool *value_p
);


        



/*
 * Return True if Self is an instantiated declaration, meaning that it does not
 * use any of its declared generic types.
 */
extern int lkt_decl_p_is_instantiated(
    lkt_node *node,


    lkt_bool *value_p
);


        



/*
 * Return the symbol corresponding to the name of this declaration.
 */
extern int lkt_decl_p_name(
    lkt_node *node,


    lkt_symbol_type *value_p
);


        



/*
 * Return the full name of this decl, as it should be seen by users/shown in
 * diagnostics.
 */
extern int lkt_decl_p_full_name(
    lkt_node *node,


    lkt_string_type *value_p
);


        



/*
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_base_grammar_rule_decl_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_explicitly_typed_decl_f_decl_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_component_decl_f_default_val(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_fun_arg_decl_f_decl_annotations(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_val_decl_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_fun_decl_f_args(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_fun_decl_f_return_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_fun_decl_f_body(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When this property is used as a a combinder inside an NPropagate equation,
 * return wether it expects a dynamic number of arguments.
 */
extern int lkt_fun_decl_p_is_dynamic_combiner(
    lkt_node *node,


    lkt_bool *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_env_spec_decl_f_actions(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_generic_decl_f_generic_formal_decls(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_dyn_var_decl``,
 * ``lkt_env_spec_decl``, ``lkt_field_decl``, ``lkt_fun_decl``,
 * ``lkt_generic_decl``, ``lkt_grammar_decl``, ``lkt_grammar_rule_decl``,
 * ``lkt_lexer_decl``, ``lkt_named_type_decl``, ``lkt_val_decl``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_generic_decl_f_decl(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_decl_f_rules(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``lkt_full_decl``, ``lkt_lexer_case_rule``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_lexer_decl_f_rules(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_lexer_family_decl_f_rules(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_type_decl_f_traits(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_type_decl_f_syn_base_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the base type for this node, if any.
 */
extern int lkt_type_decl_p_base_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the base type for this node, if any.
 */
extern int lkt_type_decl_p_base_type_if_entity(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_generic_formal_type_decl_f_has_class(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_named_type_decl_f_decls(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_enum_class_decl_f_branches(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_enum_type_decl_f_literals(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_decl_annotation_f_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_decl_annotation_f_params(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_decl_annotation_params_f_params(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_elsif_branch_f_cond_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_elsif_branch_f_then_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_enum_class_case_f_decls(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return whether this node is present
 */
extern int lkt_excludes_null_p_as_bool(
    lkt_node *node,


    lkt_bool *value_p
);


        



/*
 * Return the type of this expression.
 */
extern int lkt_expr_p_get_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the expected type of this expression.
 */
extern int lkt_expr_p_get_generic_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the expected type of this expression.
 */
extern int lkt_expr_p_get_expected_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the declaration referenced by this expression, if applicable, else
 * null.
 *
 * The property is memoized in order to avoid use the value inside logic
 * variables on every redundent call, causing faulty behavior when used with
 * rebindings. TODO: Do like LAL to avoid memoization for more safety.
 */
extern int lkt_expr_p_referenced_decl(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_if_expr``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_expr``, ``lkt_logic_predicate``, ``lkt_match_expr``,
 * ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_any_of_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_block_expr``,
 * ``lkt_call_expr``, ``lkt_cast_expr``, ``lkt_error_on_null``,
 * ``lkt_generic_instantiation``, ``lkt_if_expr``, ``lkt_keep_expr``,
 * ``lkt_lambda_expr``, ``lkt_lit``, ``lkt_logic_expr``,
 * ``lkt_logic_predicate``, ``lkt_match_expr``, ``lkt_paren_expr``,
 * ``lkt_raise_expr``, ``lkt_ref_id``, ``lkt_subscript_expr``, ``lkt_try_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_any_of_f_values(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``lkt_any_of``, ``lkt_array_literal``, ``lkt_base_dot_expr``,
 * ``lkt_bin_op``, ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_array_literal_f_exprs(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_array_literal_f_element_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_keep_expr``, ``lkt_lit``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_match_expr``, ``lkt_paren_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_base_call_expr_f_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_base_call_expr_f_args(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_keep_expr``, ``lkt_lit``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_match_expr``, ``lkt_paren_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_base_dot_expr_f_prefix(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_base_dot_expr_f_suffix(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_bin_op_f_left(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_bin_op_f_op(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_bin_op_f_right(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``lkt_val_decl``, ``lkt_var_bind``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_block_expr_f_val_defs(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_block_expr_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_keep_expr``, ``lkt_lit``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_match_expr``, ``lkt_paren_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_cast_expr_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_cast_expr_f_excludes_null(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_cast_expr_f_dest_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_keep_expr``, ``lkt_lit``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_match_expr``, ``lkt_paren_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_error_on_null_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_keep_expr``, ``lkt_lit``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_match_expr``, ``lkt_paren_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_generic_instantiation_f_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_generic_instantiation_f_args(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_discard_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_dont_skip_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_dont_skip_f_dont_skip(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_list_f_list_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_list_f_kind(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_list_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_grammar_list_f_sep(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_null_f_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_opt_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_opt_error_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_opt_error_group_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_opt_group_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_or_expr_f_sub_exprs(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_pick_f_exprs(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_predicate_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_dot_expr``,
 * ``lkt_ref_id``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_predicate_f_prop_ref(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_rule_ref_f_node_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_skip_f_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_stop_cut_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_parse_node_expr_f_node_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_parse_node_expr_f_sub_exprs(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the content of the given token literal node.
 */
extern int lkt_token_lit_p_denoted_value(
    lkt_node *node,


    lkt_internal_decoded_string_value *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_token_no_case_lit_f_lit(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_token_pattern_concat``, ``lkt_token_pattern_lit``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_token_pattern_concat_f_left(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_token_pattern_concat_f_right(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the content of the given token pattern literal node.
 */
extern int lkt_token_pattern_lit_p_denoted_value(
    lkt_node *node,


    lkt_internal_decoded_string_value *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_token_ref_f_token_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_token_ref_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Returns the image of this RefId using entity information.
 */
extern int lkt_id_p_custom_image(
    lkt_node *node,


    lkt_string_type *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_if_expr_f_cond_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_if_expr_f_then_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_if_expr_f_alternatives(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_if_expr_f_else_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_if_expr``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_expr``, ``lkt_logic_predicate``, ``lkt_match_expr``,
 * ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_isa_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_isa_f_dest_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_keep_expr``, ``lkt_lit``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_match_expr``, ``lkt_paren_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_keep_expr_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_keep_expr_f_keep_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_lambda_expr_f_params(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_lambda_expr_f_return_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_lambda_expr_f_body(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the content of the given character literal node.
 */
extern int lkt_char_lit_p_denoted_value(
    lkt_node *node,


    lkt_internal_decoded_char_value *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_null_lit_f_dest_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the content of the given string literal node.
 */
extern int lkt_string_lit_p_denoted_value(
    lkt_node *node,


    lkt_internal_decoded_string_value *value_p
);


        



/*
 * Return whether this string is prefixed or not.
 */
extern int lkt_string_lit_p_is_prefixed_string(
    lkt_node *node,


    lkt_bool *value_p
);


        



/*
 * Return the prefix of this string, or the null character if there is no
 * prefix.
 */
extern int lkt_string_lit_p_prefix(
    lkt_node *node,


    uint32_t *value_p
);


        



/*
 * Return whether this string literal is actually a regexp literal, by checking
 * that this string is prefixed by 'p'.
 */
extern int lkt_string_lit_p_is_regexp_literal(
    lkt_node *node,


    lkt_bool *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_block_string_lit_f_lines(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_block_expr``,
 * ``lkt_call_expr``, ``lkt_cast_expr``, ``lkt_error_on_null``,
 * ``lkt_generic_instantiation``, ``lkt_if_expr``, ``lkt_isa``,
 * ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``, ``lkt_logic_assign``,
 * ``lkt_logic_expr``, ``lkt_logic_predicate``, ``lkt_logic_propagate``,
 * ``lkt_logic_unify``, ``lkt_match_expr``, ``lkt_paren_expr``,
 * ``lkt_raise_expr``, ``lkt_ref_id``, ``lkt_subscript_expr``, ``lkt_try_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_logic_assign_f_dest_var(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_if_expr``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_expr``, ``lkt_logic_predicate``, ``lkt_match_expr``,
 * ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_logic_assign_f_value(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_call_expr``,
 * ``lkt_ref_id``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_logic_expr_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_block_expr``,
 * ``lkt_call_expr``, ``lkt_cast_expr``, ``lkt_error_on_null``,
 * ``lkt_generic_instantiation``, ``lkt_if_expr``, ``lkt_isa``,
 * ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``, ``lkt_logic_assign``,
 * ``lkt_logic_expr``, ``lkt_logic_predicate``, ``lkt_logic_propagate``,
 * ``lkt_logic_unify``, ``lkt_match_expr``, ``lkt_paren_expr``,
 * ``lkt_raise_expr``, ``lkt_ref_id``, ``lkt_subscript_expr``, ``lkt_try_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_logic_propagate_f_dest_var(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_logic_propagate_f_call(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_block_expr``,
 * ``lkt_call_expr``, ``lkt_cast_expr``, ``lkt_error_on_null``,
 * ``lkt_generic_instantiation``, ``lkt_if_expr``, ``lkt_isa``,
 * ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``, ``lkt_logic_assign``,
 * ``lkt_logic_expr``, ``lkt_logic_predicate``, ``lkt_logic_propagate``,
 * ``lkt_logic_unify``, ``lkt_match_expr``, ``lkt_paren_expr``,
 * ``lkt_raise_expr``, ``lkt_ref_id``, ``lkt_subscript_expr``, ``lkt_try_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_logic_unify_f_lhs(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_if_expr``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_expr``, ``lkt_logic_predicate``, ``lkt_match_expr``,
 * ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_logic_unify_f_rhs(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_match_expr_f_match_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_match_expr_f_branches(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_not_expr_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_paren_expr_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_raise_expr_f_dest_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_raise_expr_f_except_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_array_literal``,
 * ``lkt_base_dot_expr``, ``lkt_block_expr``, ``lkt_call_expr``,
 * ``lkt_cast_expr``, ``lkt_error_on_null``, ``lkt_generic_instantiation``,
 * ``lkt_keep_expr``, ``lkt_lit``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_match_expr``, ``lkt_paren_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_subscript_expr_f_prefix(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_subscript_expr_f_index(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_try_expr_f_try_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_try_expr_f_or_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_op_minus``,
 * ``lkt_op_plus``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_un_op_f_op(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_block_expr``,
 * ``lkt_call_expr``, ``lkt_cast_expr``, ``lkt_error_on_null``,
 * ``lkt_generic_instantiation``, ``lkt_if_expr``, ``lkt_isa``,
 * ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``, ``lkt_logic_assign``,
 * ``lkt_logic_expr``, ``lkt_logic_predicate``, ``lkt_logic_propagate``,
 * ``lkt_logic_unify``, ``lkt_match_expr``, ``lkt_paren_expr``,
 * ``lkt_raise_expr``, ``lkt_ref_id``, ``lkt_subscript_expr``, ``lkt_try_expr``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_un_op_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_full_decl_f_doc(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_full_decl_f_decl_annotations(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_dyn_var_decl``,
 * ``lkt_env_spec_decl``, ``lkt_field_decl``, ``lkt_fun_decl``,
 * ``lkt_generic_decl``, ``lkt_generic_formal_type_decl``,
 * ``lkt_grammar_decl``, ``lkt_grammar_rule_decl``, ``lkt_lexer_decl``,
 * ``lkt_lexer_family_decl``, ``lkt_named_type_decl``, ``lkt_val_decl``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_full_decl_f_decl(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return whether this node has an annotation with name ``name``.
 */
extern int lkt_full_decl_p_has_annotation(
    lkt_node *node,

        
        const lkt_symbol_type*
        name,

    lkt_bool *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_grammar_list_sep_f_token(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_grammar_list_sep_f_extra(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_import_f_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Return the unit that this import statements designates. Load it if needed.
 */
extern int lkt_import_p_referenced_unit(
    lkt_node *node,


    lkt_analysis_unit *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_langkit_root_f_imports(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_langkit_root_f_decls(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_grammar_cut``,
 * ``lkt_grammar_discard``, ``lkt_grammar_list``, ``lkt_grammar_null``,
 * ``lkt_grammar_opt_error_group``, ``lkt_grammar_opt_error``,
 * ``lkt_grammar_opt_group``, ``lkt_grammar_opt``, ``lkt_grammar_or_expr``,
 * ``lkt_grammar_pick``, ``lkt_grammar_rule_ref``, ``lkt_grammar_skip``,
 * ``lkt_grammar_stop_cut``, ``lkt_parse_node_expr``, ``lkt_token_lit``,
 * ``lkt_token_no_case_lit``, ``lkt_token_pattern_concat``,
 * ``lkt_token_pattern_lit``, ``lkt_token_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_lexer_case_rule_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_lexer_case_rule_f_alts(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_lexer_case_rule_send_f_sent(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_lexer_case_rule_send_f_match_size(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_match_branch_f_decl(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_match_branch_f_expr(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field may be null even when there are no parsing errors.
 */
extern int lkt_param_f_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_param_f_value(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * Returns the referenced type declaration.
 */
extern int lkt_type_ref_p_referenced_decl(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_function_type_ref_f_args_types(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_function_type_ref_f_return_type(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_dot_expr``,
 * ``lkt_ref_id``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_generic_type_ref_f_type_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field contains a list that itself contains one of the following nodes:
 * ``lkt_function_type_ref``, ``lkt_generic_type_ref``, ``lkt_simple_type_ref``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_generic_type_ref_f_params(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_dot_expr``,
 * ``lkt_ref_id``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_simple_type_ref_f_type_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_var_bind_f_name(
    lkt_node *node,


    lkt_node *value_p
);


        



/*
 * This field can contain one of the following nodes: ``lkt_any_of``,
 * ``lkt_array_literal``, ``lkt_base_dot_expr``, ``lkt_bin_op``,
 * ``lkt_block_expr``, ``lkt_call_expr``, ``lkt_cast_expr``,
 * ``lkt_error_on_null``, ``lkt_generic_instantiation``, ``lkt_if_expr``,
 * ``lkt_isa``, ``lkt_keep_expr``, ``lkt_lambda_expr``, ``lkt_lit``,
 * ``lkt_logic_assign``, ``lkt_logic_expr``, ``lkt_logic_predicate``,
 * ``lkt_logic_propagate``, ``lkt_logic_unify``, ``lkt_match_expr``,
 * ``lkt_not_expr``, ``lkt_paren_expr``, ``lkt_raise_expr``, ``lkt_ref_id``,
 * ``lkt_subscript_expr``, ``lkt_try_expr``, ``lkt_un_op``
 *
 * When there are no parsing errors, this field is never null.
 */
extern int lkt_var_bind_f_expr(
    lkt_node *node,


    lkt_node *value_p
);



/*
 * Event handlers
 */

/*
 * Create an event handler. When done with it, the result must be passed to
 * ``lkt_dec_ref_event_handler``.
 *
 * Pass as ``data`` a pointer to hold your private data: it will be passed to
 * all callbacks below.
 *
 * ``destroy`` is a callback that is called by ``lkt_dec_ref_event_handler`` to
 * leave a chance to free resources that ``data`` may hold. ``NULL`` can be
 * passed if nothing needs to be done.
 *
 * ``unit_requested`` is a callback that will be called when a unit is
 * requested.
 *
 * .. warning:: Please note that the unit requested callback can be called
 *    *many* times for the same unit, so in all likeliness, those events should
 *    be filtered if they're used to forward diagnostics to the user.
 *
 * ``unit_parsed`` is a callback that will be called when a unit is parsed.
 */
extern lkt_event_handler
lkt_create_event_handler(
   void *data,
   lkt_event_handler_destroy_callback destroy_func,
   lkt_event_handler_unit_requested_callback unit_requested_func,
   lkt_event_handler_unit_parsed_callback unit_parsed_func
);

/*
 * Release an ownership share for this event handler. This destroys the event
 * handler if there are no shares left.
 */
extern void
lkt_dec_ref_event_handler(lkt_event_handler self);

/*
 * File readers
 */

/*
 * Create a file reader. When done with it, the result must be passed to
 * ``lkt_dec_ref_file_reader``.
 *
 * Pass as ``data`` a pointer to hold your private data: it will be passed to
 * all callbacks below.
 *
 * ``destroy`` is a callback that is called by ``lkt_dec_ref_file_reader`` to
 * leave a chance to free resources that ``data`` may hold.
 *
 * ``read`` is a callback. For a given filename/charset and whether to read the
 * BOM (Byte Order Mark), it tries to fetch the contents of the source file,
 * returned in ``Contents``. If there is an error, it must return it in
 * ``Diagnostic`` instead.
 */
extern lkt_file_reader
lkt_create_file_reader(
   void *data,
   lkt_file_reader_destroy_callback destroy_func,
   lkt_file_reader_read_callback read_func
);

/*
 * Release an ownership share for this file reader. This destroys the file
 * reader if there are no shares left.
 */
extern void
lkt_dec_ref_file_reader(lkt_file_reader self);




/*
 * Unit providers
 */

/*
 * Release an ownership share for this unit provider. This destroys the unit
 * provider if there are no shares left.
 */
extern void
lkt_dec_ref_unit_provider(void *data);




/*
 * Misc
 */

/*
 * Return exception information for the last error that happened in the current
 * thread. Will be automatically allocated on error and free'd on the next
 * error.
 */
extern const lkt_exception *
lkt_get_last_exception(void);

/*
 * Return the name of the given exception kind. Callers are responsible for
 * free'ing the result.
 */
extern char *
lkt_exception_name(lkt_exception_kind kind);

/*
 * Kind for this token.
 */
extern int
lkt_token_get_kind(lkt_token *token);

/*
 * Return a human-readable name for a token kind.
 *
 * The returned string is dynamically allocated and the caller must free it
 * when done with it.
 *
 * If the given kind is invalid, return ``NULL`` and set the last exception
 * accordingly.
 */
extern char *
lkt_token_kind_name(lkt_token_kind kind);

/*
 * Return the source location range of the given token.
 */
extern void
lkt_token_sloc_range(lkt_token *token,
                                     lkt_source_location_range *result);

/*
 * Return a reference to the next token in the corresponding analysis unit.
 */
extern void
lkt_token_next(lkt_token *token,
                               lkt_token *next_token);

/*
 * Return a reference to the previous token in the corresponding analysis unit.
 */
extern void
lkt_token_previous(lkt_token *token,
                                   lkt_token *previous_token);

/*
 * Compute the source buffer slice corresponding to the text that spans between
 * the ``First`` and ``Last`` tokens (both included). This yields an empty
 * slice if ``Last`` actually appears before ``First``. Put the result in
 * ``RESULT``.
 *
 * This returns ``0`` if ``First`` and ``Last`` don't belong to the same
 * analysis unit. Return ``1`` if successful.
 */
extern int
lkt_token_range_text(lkt_token *first,
                                     lkt_token *last,
                                     lkt_text *result);

/*
 * Return whether ``L`` and ``R`` are structurally equivalent tokens. This
 * means that their position in the stream won't be taken into account, only
 * the kind and text of the token.
 */
extern lkt_bool
lkt_token_is_equivalent(lkt_token *left,
                                        lkt_token *right);

/*
 * Tree rewriting
 */

/* ... context rewriting... */

/*
 * Return the rewriting handle associated to Context, or No_Rewriting_Handle if
 * Context is not being rewritten.
 */
extern lkt_rewriting_handle
lkt_rewriting_context_to_handle(
    lkt_analysis_context context
);

/*
 * Return the analysis context associated to Handle
 */
extern lkt_analysis_context
lkt_rewriting_handle_to_context(
    lkt_rewriting_handle handle
);

/*
 * Start a rewriting session for Context.
 *
 * This handle will keep track of all changes to do on Context's analysis
 * units. Once the set of changes is complete, call the Apply procedure to
 * actually update Context. This makes it possible to inspect the "old" Context
 * state while creating the list of changes.
 *
 * There can be only one rewriting session per analysis context, so this will
 * raise an Existing_Rewriting_Handle_Error exception if Context already has a
 * living rewriting session.
 */
extern lkt_rewriting_handle
lkt_rewriting_start_rewriting(
    lkt_analysis_context context
);

/*
 * Discard all modifications registered in Handle and close Handle. This
 * invalidates all related unit/node handles.
 */
extern void
lkt_rewriting_abort_rewriting(
    lkt_rewriting_handle context
);

/*
 * Apply all modifications to Handle's analysis context. If that worked, close
 * Handle and return (Success => True). Otherwise, reparsing did not work, so
 * keep Handle and its Context unchanged and return details about the error
 * that happened.
 *
 * Note that on success, this invalidates all related unit/node handles.
 */
extern void
lkt_rewriting_apply(
    lkt_rewriting_handle context,
    lkt_rewriting_apply_result *result
);

/*
 * Free the result of the ``Apply`` operation.
 */
extern void
lkt_rewriting_free_apply_result(
    lkt_rewriting_apply_result *result
);

/*
 * Return the list of unit rewriting handles in the given context handle for
 * units that the Apply primitive will modify.
 *
 * This returns the list as a dynamically allocated NULL-terminated array, that
 * the caller must free when done with it.
 */
extern lkt_unit_rewriting_handle *
lkt_rewriting_unit_handles(
    lkt_rewriting_handle handle
);

/* ... unit rewriting... */

/*
 * Return the rewriting handle corresponding to Unit
 */
extern lkt_unit_rewriting_handle
lkt_rewriting_unit_to_handle(lkt_analysis_unit context);

/*
 * Return the unit corresponding to Handle
 */
extern lkt_analysis_unit
lkt_rewriting_handle_to_unit(
    lkt_unit_rewriting_handle handle
);

/*
 * Return the node handle corresponding to the root of the unit which Handle
 * designates.
 */
extern lkt_node_rewriting_handle
lkt_rewriting_unit_root(
    lkt_unit_rewriting_handle handle
);

/*
 * Set the root node for the unit Handle to Root. This unties the previous root
 * handle. If Root is not No_Node_Rewriting_Handle, this also ties Root to
 * Handle.
 *
 * Root must not already be tied to another analysis unit handle.
 */
extern void
lkt_rewriting_unit_set_root(
    lkt_unit_rewriting_handle handle,
    lkt_node_rewriting_handle root
);

/*
 * Return the text associated to the given unit.
 */
extern void
lkt_rewriting_unit_unparse(
    lkt_unit_rewriting_handle handle,
    lkt_text *result
);

/* ... node rewriting... */

/*
 * Return the rewriting handle corresponding to Node.
 *
 * The owning unit of Node must be free of diagnostics.
 */
extern lkt_node_rewriting_handle
lkt_rewriting_node_to_handle(lkt_base_node context);

/*
 * Return the node which the given rewriting Handle relates to. This can be the
 * null entity if this handle designates a new node.
 */
extern lkt_base_node
lkt_rewriting_handle_to_node(
    lkt_node_rewriting_handle handle
);

/*
 * Return a handle for the rewriting context to which Handle belongs
 */
extern lkt_rewriting_handle
lkt_rewriting_node_to_context(
    lkt_node_rewriting_handle handle
);

/*
 * Turn the given rewritten node Handles designates into text. This is the text
 * that is used in Apply in order to re-create an analysis unit.
 */
extern void
lkt_rewriting_node_unparse(
    lkt_node_rewriting_handle handle,
    lkt_text *result
);

/*
 * Return the kind corresponding to Handle's node
 */
extern lkt_node_kind_enum
lkt_rewriting_kind(lkt_node_rewriting_handle handle);

/*
 * Return a representation of ``Handle`` as a string.
 */
extern void
lkt_rewriting_node_image(
    lkt_node_rewriting_handle handle,
    lkt_text *result
);

/*
 * Return whether this node handle is tied to an analysis unit. If it is not,
 * it can be passed as the Child parameter to Set_Child.
 */
extern int
lkt_rewriting_tied(lkt_node_rewriting_handle handle);

/*
 * Return a handle for the node that is the parent of Handle's node. This is
 * ``No_Rewriting_Handle`` for a node that is not tied to any tree yet.
 */
extern lkt_node_rewriting_handle
lkt_rewriting_parent(lkt_node_rewriting_handle handle);

/*
 * Return the number of children the node represented by Handle has
 */
extern int
lkt_rewriting_children_count(
    lkt_node_rewriting_handle handle
);

/*
 * Return the node that is in the syntax ``Field`` for ``Handle``
 */
extern lkt_node_rewriting_handle
lkt_rewriting_child(
    lkt_node_rewriting_handle handle,
    lkt_introspection_member_ref field
);

/*
 * Return the list of children for ``Handle``.
 *
 * This returns the list as a dynamically allocated array with ``count``
 * elements.  The caller must free it when done with it.
 */
extern void
lkt_rewriting_children(
    lkt_node_rewriting_handle handle,
    lkt_node_rewriting_handle **children,
    int *count
);

/*
 * If ``Child`` is ``No_Rewriting_Node``, untie the syntax field in ``Handle``
 * corresponding to ``Field``, so it can be attached to another one. Otherwise,
 * ``Child`` must have no parent as it will be tied to ``Handle``'s tree.
 */
extern void
lkt_rewriting_set_child(
    lkt_node_rewriting_handle handle,
    lkt_introspection_member_ref field,
    lkt_node_rewriting_handle child
);

/*
 * Return the text associated to the given token node.
 */
extern void
lkt_rewriting_text(
    lkt_node_rewriting_handle handle,
    lkt_text *result
);

/*
 * Override text associated to the given token node.
 */
extern void
lkt_rewriting_set_text(
    lkt_node_rewriting_handle handle,
    lkt_text *text
);

/*
 * If Handle is the root of an analysis unit, untie it and set New_Node as its
 * new root. Otherwise, replace Handle with New_Node in Handle's parent node.
 *
 * Note that: * Handle must be tied to an existing analysis unit handle. *
 * New_Node must not already be tied to another analysis unit handle.
 */
extern void
lkt_rewriting_replace(
    lkt_node_rewriting_handle handle,
    lkt_node_rewriting_handle new_node
);

/*
 * Given a list of node rewriting handles ``H1``, ``H2``, ... ``HN``, replace
 * ``H1`` by ``H2`` in the rewritten tree, replace ``H2`` by ``H3``, etc. and
 * replace ``HN`` by ``H1``.
 *
 * Note that this operation is atomic: if it fails, no replacement is actually
 * performed.
 */
extern void
lkt_rewriting_rotate(
    lkt_node_rewriting_handle *handles,
    int count
);

/* ... list node rewriting... */

/*
 * Assuming ``Handle`` refers to a list node, return a handle to its first
 * child, or ``No_Node_Rewriting_Handle``` if it has no child node.
 */
extern lkt_node_rewriting_handle
lkt_rewriting_first_child(
    lkt_node_rewriting_handle handle
);

/*
 * Assuming ``Handle`` refers to a list node, return a handle to its last
 * child, or ``No_Node_Rewriting_Handle``` if it has no child node.
 */
extern lkt_node_rewriting_handle
lkt_rewriting_last_child(
    lkt_node_rewriting_handle handle
);

/*
 * Assuming ``Handle`` refers to the child of a list node, return a handle to
 * its next sibling, or ``No_Node_Rewriting_Handle``` if it is the last
 * sibling.
 */
extern lkt_node_rewriting_handle
lkt_rewriting_next_child(
    lkt_node_rewriting_handle handle
);

/*
 * Assuming ``Handle`` refers to the child of a list node, return a handle to
 * its previous sibling, or ``No_Node_Rewriting_Handle``` if it is the first
 * sibling.
 */
extern lkt_node_rewriting_handle
lkt_rewriting_previous_child(
    lkt_node_rewriting_handle handle
);

/*
 * Assuming ``Handle`` refers to the child of a list node, insert
 * ``New_Sibling`` as a new child in this list, right before ``Handle``.
 */
extern void
lkt_rewriting_insert_before(
    lkt_node_rewriting_handle handle,
    lkt_node_rewriting_handle new_sibling
);

/*
 * Assuming ``Handle`` refers to the child of a list node, insert
 * ``New_Sibling`` as a new child in this list, right before ``Handle``.
 */
extern void
lkt_rewriting_insert_after(
    lkt_node_rewriting_handle handle,
    lkt_node_rewriting_handle new_sibling
);

/*
 * Assuming ``Handle`` refers to a list node, insert ``New_Child`` to be the
 * first child in this list.
 */
extern void
lkt_rewriting_insert_first(
    lkt_node_rewriting_handle handle,
    lkt_node_rewriting_handle new_sibling
);

/*
 * Assuming ``Handle`` refers to a list node, insert ``New_Child`` to be the
 * last child in this list.
 */
extern void
lkt_rewriting_insert_last(
    lkt_node_rewriting_handle handle,
    lkt_node_rewriting_handle new_sibling
);

/*
 * Assuming Handle refers to the child of a list node, remove it from that
 * list.
 */
extern void
lkt_rewriting_remove_child(
    lkt_node_rewriting_handle handle
);

/* ... node creation... */

/*
 * Create a clone of the Handle node tree. The result is not tied to any
 * analysis unit tree.
 */
extern lkt_node_rewriting_handle
lkt_rewriting_clone(lkt_node_rewriting_handle handle);

/*
 * Create a new node of the given Kind, with empty text (for token nodes) or
 * children (for regular nodes).
 */
extern lkt_node_rewriting_handle
lkt_rewriting_create_node(
    lkt_rewriting_handle handle,
    lkt_node_kind_enum kind
);

/*
 * Create a new token node with the given Kind and Text
 */
extern lkt_node_rewriting_handle
lkt_rewriting_create_token_node(
    lkt_rewriting_handle handle,
    lkt_node_kind_enum kind,
    lkt_text *text
);

/*
 * Create a new regular node of the given Kind and assign it the given
 * Children.
 *
 * Except for lists, which can have any number of children, the size of
 * Children must match the number of children associated to the given Kind.
 * Besides, all given children must not be tied.
 */
extern lkt_node_rewriting_handle
lkt_rewriting_create_regular_node(
    lkt_rewriting_handle handle,
    lkt_node_kind_enum kind,
    lkt_node_rewriting_handle *children,
    int count
);

/*
 * Create a tree of new nodes from the given Template string, replacing
 * placeholders with nodes in Arguments and parsed according to the given
 * grammar Rule.
 */
extern lkt_node_rewriting_handle
lkt_rewriting_create_from_template(
    lkt_rewriting_handle handle,
    lkt_text *src_template,
    lkt_node_rewriting_handle *arguments,
    int count,
    lkt_grammar_rule rule
);




#ifdef __cplusplus
}
#endif

#endif
