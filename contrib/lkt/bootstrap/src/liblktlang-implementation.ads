









--  To facilitate use from a -gnatX project, since we don't use the [] syntax
pragma Warnings (Off, "obsolescent");

with Ada.Containers;              use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with System; use System;

with GNATCOLL.GMP.Integers;
with GNATCOLL.Traces;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Liblktlang_Support.Adalog.Logic_Var;
with Liblktlang_Support.Adalog.Solver;
with Liblktlang_Support.Adalog.Solver_Interface;

with Liblktlang_Support.Bump_Ptr;     use Liblktlang_Support.Bump_Ptr;
with Liblktlang_Support.Cheap_Sets;
with Liblktlang_Support.File_Readers; use Liblktlang_Support.File_Readers;
with Liblktlang_Support.Lexical_Envs; use Liblktlang_Support.Lexical_Envs;
with Liblktlang_Support.Lexical_Envs_Impl;
with Liblktlang_Support.Symbols;      use Liblktlang_Support.Symbols;
with Liblktlang_Support.Symbols.Precomputed;
with Liblktlang_Support.Token_Data_Handlers;
use Liblktlang_Support.Token_Data_Handlers;
with Liblktlang_Support.Types;        use Liblktlang_Support.Types;
with Liblktlang_Support.Vectors;

with Liblktlang.Parsers; use Liblktlang.Parsers;
with Liblktlang.Common;  use Liblktlang.Common;
with Liblktlang.Lexer_Implementation;
use Liblktlang.Lexer_Implementation;




--  Internal package: low-level primitives to implement public types and
--  operations in Liblktlang.Analysis.

private package Liblktlang.Implementation is

   pragma Suppress (Container_Checks);

   use Support.Diagnostics, Support.Slocs, Support.Text;

   ------------
   -- Traces --
   ------------

   Main_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LIBLKTLANG.MAIN_TRACE", GNATCOLL.Traces.From_Config);

   PLE_Errors_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LIBLKTLANG.PLE_ERRORS", GNATCOLL.Traces.From_Config);

   Cache_Invalidation_Trace : constant GNATCOLL.Traces.Trace_Handle :=
     GNATCOLL.Traces.Create
       ("LIBLKTLANG.CACHE_INVALIDATION",
        GNATCOLL.Traces.From_Config);

   -------------------------------------
   -- Symbols and token data handlers --
   -------------------------------------

   type Precomputed_Symbol_Index is
         (
            
               Precomputed_Sym_Abstract, --  abstract
               Precomputed_Sym_All, --  all
               Precomputed_Sym_Analysisunit, --  AnalysisUnit
               Precomputed_Sym_Any, --  any
               Precomputed_Sym_Array, --  Array
               Precomputed_Sym_As, --  as
               Precomputed_Sym_As_Bool, --  as_bool
               Precomputed_Sym_Astlist, --  ASTList
               Precomputed_Sym_Basictrait, --  BasicTrait
               Precomputed_Sym_Bigint, --  BigInt
               Precomputed_Sym_Bool, --  Bool
               Precomputed_Sym_Builder, --  builder
               Precomputed_Sym_Call, --  __call__
               Precomputed_Sym_Char, --  Char
               Precomputed_Sym_Dedent, --  dedent
               Precomputed_Sym_Domain, --  domain
               Precomputed_Sym_Dont_Skip, --  dont_skip
               Precomputed_Sym_Entity, --  Entity
               Precomputed_Sym_Env_Spec, --  env_spec
               Precomputed_Sym_Envaction, --  EnvAction
               Precomputed_Sym_Equation, --  Equation
               Precomputed_Sym_Errornode, --  ErrorNode
               Precomputed_Sym_Family, --  family
               Precomputed_Sym_Ignore_Constructor_Arg, --  ignore_constructor_arg
               Precomputed_Sym_Indent, --  indent
               Precomputed_Sym_Indexable, --  Indexable
               Precomputed_Sym_Int, --  Int
               Precomputed_Sym_Internal, --  __internal
               Precomputed_Sym_Invalid, --  invalid
               Precomputed_Sym_Iterator, --  Iterator
               Precomputed_Sym_Keep, --  keep
               Precomputed_Sym_Lazy, --  lazy
               Precomputed_Sym_List, --  list
               Precomputed_Sym_Logicvar, --  LogicVar
               Precomputed_Sym_Metadata, --  Metadata
               Precomputed_Sym_Metadata_44, --  metadata
               Precomputed_Sym_Newline, --  newline
               Precomputed_Sym_No_Case, --  no_case
               Precomputed_Sym_Node, --  Node
               Precomputed_Sym_Node_47, --  node
               Precomputed_Sym_Nodebuilder, --  NodeBuilder
               Precomputed_Sym_Null_Field, --  null_field
               Precomputed_Sym_Nullable, --  nullable
               Precomputed_Sym_Open, --  open
               Precomputed_Sym_Parse_Field, --  parse_field
               Precomputed_Sym_Pick, --  pick
               Precomputed_Sym_Previous_Token, --  previous_token
               Precomputed_Sym_Property, --  property
               Precomputed_Sym_Propertyerror, --  PropertyError
               Precomputed_Sym_Qualifier, --  qualifier
               Precomputed_Sym_Regexp, --  Regexp
               Precomputed_Sym_Root_Node, --  root_node
               Precomputed_Sym_Rootnode, --  RootNode__
               Precomputed_Sym_Self, --  self
               Precomputed_Sym_Send, --  send
               Precomputed_Sym_Skip, --  skip
               Precomputed_Sym_Stop_Cut, --  stop_cut
               Precomputed_Sym_String, --  String
               Precomputed_Sym_Super, --  super
               Precomputed_Sym_Symbol, --  Symbol
               Precomputed_Sym_Synthetic, --  synthetic
               Precomputed_Sym_T, --  T
               Precomputed_Sym_Tokennode, --  TokenNode
               Precomputed_Sym_Update, --  update
               Precomputed_Sym_Var, --  var
               Precomputed_Sym_When, --  when
               Precomputed_Sym_With_Dynvars --  with_dynvars
         )
   ;

   function Precomputed_Symbol
     (Index : Precomputed_Symbol_Index) return Text_Type;

   --  GNAT emits an incorrect value not in range in instantiation warning...
   --  So deactivate them at the instantiation point.
   pragma Warnings (Off, "value not in range");
   package Precomputed_Symbols is new Liblktlang_Support.Symbols.Precomputed
     (Precomputed_Symbol_Index, Precomputed_Symbol);
   pragma Warnings (On, "value not in range");

   --------------------
   -- Analysis types --
   --------------------

   type Analysis_Context_Type;
   type Internal_Context is access all Analysis_Context_Type;

   type Analysis_Unit_Type;
   type Internal_Unit is access all Analysis_Unit_Type;

   type Root_Node_Record;
   type Bare_Lkt_Node is access all Root_Node_Record;
   No_Bare_Lkt_Node : constant Bare_Lkt_Node := null;
   --  Most generic AST node type

   pragma No_Strict_Aliasing (Internal_Context);
   pragma No_Strict_Aliasing (Internal_Unit);
   pragma No_Strict_Aliasing (Bare_Lkt_Node);

   function "<" (Left, Right : Bare_Lkt_Node) return Boolean;
   --  Abritrary but deterministic ordering criteria for parsing nodes. This
   --  handles null nodes as well. Raise a Property_Error for synthetic nodes.

   function Is_Null (Node : Bare_Lkt_Node) return Boolean;
   function Kind (Node : Bare_Lkt_Node) return Lkt_Node_Kind_Type;

         subtype Bare_Base_Lexer_Case_Rule_Alt is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Base_Lexer_Case_Rule_Alt)
               or else Kind (Bare_Base_Lexer_Case_Rule_Alt) in Lkt_Base_Lexer_Case_Rule_Alt;
         subtype Bare_Lexer_Case_Rule_Cond_Alt is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lexer_Case_Rule_Cond_Alt)
               or else Kind (Bare_Lexer_Case_Rule_Cond_Alt) in Lkt_Lexer_Case_Rule_Cond_Alt_Range;
         subtype Bare_Lexer_Case_Rule_Default_Alt is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lexer_Case_Rule_Default_Alt)
               or else Kind (Bare_Lexer_Case_Rule_Default_Alt) in Lkt_Lexer_Case_Rule_Default_Alt_Range;
         subtype Bare_Block_String_Line is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Block_String_Line)
               or else Kind (Bare_Block_String_Line) in Lkt_Block_String_Line_Range;
         subtype Bare_Class_Qualifier is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Class_Qualifier)
               or else Kind (Bare_Class_Qualifier) in Lkt_Class_Qualifier;
         subtype Bare_Class_Qualifier_Absent is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Class_Qualifier_Absent)
               or else Kind (Bare_Class_Qualifier_Absent) in Lkt_Class_Qualifier_Absent_Range;
         subtype Bare_Class_Qualifier_Present is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Class_Qualifier_Present)
               or else Kind (Bare_Class_Qualifier_Present) in Lkt_Class_Qualifier_Present_Range;
         subtype Bare_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Decl)
               or else Kind (Bare_Decl) in Lkt_Decl;
         subtype Bare_Base_Grammar_Rule_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Base_Grammar_Rule_Decl)
               or else Kind (Bare_Base_Grammar_Rule_Decl) in Lkt_Base_Grammar_Rule_Decl;
         subtype Bare_Grammar_Rule_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Rule_Decl)
               or else Kind (Bare_Grammar_Rule_Decl) in Lkt_Grammar_Rule_Decl_Range;
         subtype Bare_Synthetic_Lexer_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Synthetic_Lexer_Decl)
               or else Kind (Bare_Synthetic_Lexer_Decl) in Lkt_Synthetic_Lexer_Decl_Range;
         subtype Bare_Base_Val_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Base_Val_Decl)
               or else Kind (Bare_Base_Val_Decl) in Lkt_Base_Val_Decl;
         subtype Bare_Node_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Node_Decl)
               or else Kind (Bare_Node_Decl) in Lkt_Node_Decl_Range;
         subtype Bare_Self_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Self_Decl)
               or else Kind (Bare_Self_Decl) in Lkt_Self_Decl_Range;
         subtype Bare_User_Val_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_User_Val_Decl)
               or else Kind (Bare_User_Val_Decl) in Lkt_User_Val_Decl;
         subtype Bare_Enum_Lit_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Enum_Lit_Decl)
               or else Kind (Bare_Enum_Lit_Decl) in Lkt_Enum_Lit_Decl_Range;
         subtype Bare_Explicitly_Typed_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Explicitly_Typed_Decl)
               or else Kind (Bare_Explicitly_Typed_Decl) in Lkt_Explicitly_Typed_Decl;
         subtype Bare_Component_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Component_Decl)
               or else Kind (Bare_Component_Decl) in Lkt_Component_Decl;
         subtype Bare_Field_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Field_Decl)
               or else Kind (Bare_Field_Decl) in Lkt_Field_Decl_Range;
         subtype Bare_Fun_Arg_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Fun_Arg_Decl)
               or else Kind (Bare_Fun_Arg_Decl) in Lkt_Fun_Arg_Decl_Range;
         subtype Bare_Lambda_Arg_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lambda_Arg_Decl)
               or else Kind (Bare_Lambda_Arg_Decl) in Lkt_Lambda_Arg_Decl_Range;
         subtype Bare_Dyn_Var_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Dyn_Var_Decl)
               or else Kind (Bare_Dyn_Var_Decl) in Lkt_Dyn_Var_Decl_Range;
         subtype Bare_Match_Val_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Match_Val_Decl)
               or else Kind (Bare_Match_Val_Decl) in Lkt_Match_Val_Decl_Range;
         subtype Bare_Val_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Val_Decl)
               or else Kind (Bare_Val_Decl) in Lkt_Val_Decl_Range;
         subtype Bare_Fun_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Fun_Decl)
               or else Kind (Bare_Fun_Decl) in Lkt_Fun_Decl_Range;
         subtype Bare_Env_Spec_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Env_Spec_Decl)
               or else Kind (Bare_Env_Spec_Decl) in Lkt_Env_Spec_Decl_Range;
         subtype Bare_Generic_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Generic_Decl)
               or else Kind (Bare_Generic_Decl) in Lkt_Generic_Decl_Range;
         subtype Bare_Grammar_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Decl)
               or else Kind (Bare_Grammar_Decl) in Lkt_Grammar_Decl_Range;
         subtype Bare_Lexer_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lexer_Decl)
               or else Kind (Bare_Lexer_Decl) in Lkt_Lexer_Decl_Range;
         subtype Bare_Lexer_Family_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lexer_Family_Decl)
               or else Kind (Bare_Lexer_Family_Decl) in Lkt_Lexer_Family_Decl_Range;
         subtype Bare_Synth_Arg_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Synth_Arg_Decl)
               or else Kind (Bare_Synth_Arg_Decl) in Lkt_Synth_Arg_Decl_Range;
         subtype Bare_Synth_Fun_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Synth_Fun_Decl)
               or else Kind (Bare_Synth_Fun_Decl) in Lkt_Synth_Fun_Decl_Range;
         subtype Bare_Type_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Type_Decl)
               or else Kind (Bare_Type_Decl) in Lkt_Type_Decl;
         subtype Bare_Any_Type_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Any_Type_Decl)
               or else Kind (Bare_Any_Type_Decl) in Lkt_Any_Type_Decl_Range;
         subtype Bare_Enum_Class_Alt_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Enum_Class_Alt_Decl)
               or else Kind (Bare_Enum_Class_Alt_Decl) in Lkt_Enum_Class_Alt_Decl_Range;
         subtype Bare_Function_Type is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Function_Type)
               or else Kind (Bare_Function_Type) in Lkt_Function_Type_Range;
         subtype Bare_Generic_Formal_Type_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Generic_Formal_Type_Decl)
               or else Kind (Bare_Generic_Formal_Type_Decl) in Lkt_Generic_Formal_Type_Decl_Range;
         subtype Bare_Named_Type_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Named_Type_Decl)
               or else Kind (Bare_Named_Type_Decl) in Lkt_Named_Type_Decl;
         subtype Bare_Basic_Class_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Basic_Class_Decl)
               or else Kind (Bare_Basic_Class_Decl) in Lkt_Basic_Class_Decl;
         subtype Bare_Class_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Class_Decl)
               or else Kind (Bare_Class_Decl) in Lkt_Class_Decl_Range;
         subtype Bare_Enum_Class_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Enum_Class_Decl)
               or else Kind (Bare_Enum_Class_Decl) in Lkt_Enum_Class_Decl_Range;
         subtype Bare_Enum_Type_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Enum_Type_Decl)
               or else Kind (Bare_Enum_Type_Decl) in Lkt_Enum_Type_Decl_Range;
         subtype Bare_Struct_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Struct_Decl)
               or else Kind (Bare_Struct_Decl) in Lkt_Struct_Decl_Range;
         subtype Bare_Trait_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Trait_Decl)
               or else Kind (Bare_Trait_Decl) in Lkt_Trait_Decl_Range;
         subtype Bare_Decl_Annotation is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Decl_Annotation)
               or else Kind (Bare_Decl_Annotation) in Lkt_Decl_Annotation_Range;
         subtype Bare_Decl_Annotation_Params is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Decl_Annotation_Params)
               or else Kind (Bare_Decl_Annotation_Params) in Lkt_Decl_Annotation_Params_Range;
         subtype Bare_Dyn_Env_Wrapper is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Dyn_Env_Wrapper)
               or else Kind (Bare_Dyn_Env_Wrapper) in Lkt_Dyn_Env_Wrapper_Range;
         subtype Bare_Elsif_Branch is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Elsif_Branch)
               or else Kind (Bare_Elsif_Branch) in Lkt_Elsif_Branch_Range;
         subtype Bare_Enum_Class_Case is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Enum_Class_Case)
               or else Kind (Bare_Enum_Class_Case) in Lkt_Enum_Class_Case_Range;
         subtype Bare_Excludes_Null is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Excludes_Null)
               or else Kind (Bare_Excludes_Null) in Lkt_Excludes_Null;
         subtype Bare_Excludes_Null_Absent is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Excludes_Null_Absent)
               or else Kind (Bare_Excludes_Null_Absent) in Lkt_Excludes_Null_Absent_Range;
         subtype Bare_Excludes_Null_Present is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Excludes_Null_Present)
               or else Kind (Bare_Excludes_Null_Present) in Lkt_Excludes_Null_Present_Range;
         subtype Bare_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Expr)
               or else Kind (Bare_Expr) in Lkt_Expr;
         subtype Bare_Any_Of is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Any_Of)
               or else Kind (Bare_Any_Of) in Lkt_Any_Of_Range;
         subtype Bare_Array_Literal is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Array_Literal)
               or else Kind (Bare_Array_Literal) in Lkt_Array_Literal_Range;
         subtype Bare_Base_Call_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Base_Call_Expr)
               or else Kind (Bare_Base_Call_Expr) in Lkt_Base_Call_Expr;
         subtype Bare_Call_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Call_Expr)
               or else Kind (Bare_Call_Expr) in Lkt_Call_Expr_Range;
         subtype Bare_Logic_Call_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Logic_Call_Expr)
               or else Kind (Bare_Logic_Call_Expr) in Lkt_Logic_Call_Expr;
         subtype Bare_Logic_Predicate is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Logic_Predicate)
               or else Kind (Bare_Logic_Predicate) in Lkt_Logic_Predicate_Range;
         subtype Bare_Logic_Propagate_Call is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Logic_Propagate_Call)
               or else Kind (Bare_Logic_Propagate_Call) in Lkt_Logic_Propagate_Call_Range;
         subtype Bare_Base_Dot_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Base_Dot_Expr)
               or else Kind (Bare_Base_Dot_Expr) in Lkt_Base_Dot_Expr;
         subtype Bare_Dot_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Dot_Expr)
               or else Kind (Bare_Dot_Expr) in Lkt_Dot_Expr_Range;
         subtype Bare_Null_Cond_Dotted_Name is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Null_Cond_Dotted_Name)
               or else Kind (Bare_Null_Cond_Dotted_Name) in Lkt_Null_Cond_Dotted_Name_Range;
         subtype Bare_Bin_Op is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Bin_Op)
               or else Kind (Bare_Bin_Op) in Lkt_Bin_Op_Range;
         subtype Bare_Block_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Block_Expr)
               or else Kind (Bare_Block_Expr) in Lkt_Block_Expr_Range;
         subtype Bare_Cast_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Cast_Expr)
               or else Kind (Bare_Cast_Expr) in Lkt_Cast_Expr_Range;
         subtype Bare_Error_On_Null is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Error_On_Null)
               or else Kind (Bare_Error_On_Null) in Lkt_Error_On_Null_Range;
         subtype Bare_Generic_Instantiation is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Generic_Instantiation)
               or else Kind (Bare_Generic_Instantiation) in Lkt_Generic_Instantiation_Range;
         subtype Bare_Grammar_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Expr)
               or else Kind (Bare_Grammar_Expr) in Lkt_Grammar_Expr;
         subtype Bare_Grammar_Cut is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Cut)
               or else Kind (Bare_Grammar_Cut) in Lkt_Grammar_Cut_Range;
         subtype Bare_Grammar_Discard is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Discard)
               or else Kind (Bare_Grammar_Discard) in Lkt_Grammar_Discard_Range;
         subtype Bare_Grammar_Dont_Skip is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Dont_Skip)
               or else Kind (Bare_Grammar_Dont_Skip) in Lkt_Grammar_Dont_Skip_Range;
         subtype Bare_Grammar_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_List)
               or else Kind (Bare_Grammar_List) in Lkt_Grammar_List_Range;
         subtype Bare_Grammar_Null is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Null)
               or else Kind (Bare_Grammar_Null) in Lkt_Grammar_Null_Range;
         subtype Bare_Grammar_Opt is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Opt)
               or else Kind (Bare_Grammar_Opt) in Lkt_Grammar_Opt_Range;
         subtype Bare_Grammar_Opt_Error is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Opt_Error)
               or else Kind (Bare_Grammar_Opt_Error) in Lkt_Grammar_Opt_Error_Range;
         subtype Bare_Grammar_Opt_Error_Group is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Opt_Error_Group)
               or else Kind (Bare_Grammar_Opt_Error_Group) in Lkt_Grammar_Opt_Error_Group_Range;
         subtype Bare_Grammar_Opt_Group is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Opt_Group)
               or else Kind (Bare_Grammar_Opt_Group) in Lkt_Grammar_Opt_Group_Range;
         subtype Bare_Grammar_Or_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Or_Expr)
               or else Kind (Bare_Grammar_Or_Expr) in Lkt_Grammar_Or_Expr_Range;
         subtype Bare_Grammar_Pick is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Pick)
               or else Kind (Bare_Grammar_Pick) in Lkt_Grammar_Pick_Range;
         subtype Bare_Grammar_Implicit_Pick is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Implicit_Pick)
               or else Kind (Bare_Grammar_Implicit_Pick) in Lkt_Grammar_Implicit_Pick_Range;
         subtype Bare_Grammar_Predicate is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Predicate)
               or else Kind (Bare_Grammar_Predicate) in Lkt_Grammar_Predicate_Range;
         subtype Bare_Grammar_Rule_Ref is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Rule_Ref)
               or else Kind (Bare_Grammar_Rule_Ref) in Lkt_Grammar_Rule_Ref_Range;
         subtype Bare_Grammar_Skip is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Skip)
               or else Kind (Bare_Grammar_Skip) in Lkt_Grammar_Skip_Range;
         subtype Bare_Grammar_Stop_Cut is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Stop_Cut)
               or else Kind (Bare_Grammar_Stop_Cut) in Lkt_Grammar_Stop_Cut_Range;
         subtype Bare_Parse_Node_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Parse_Node_Expr)
               or else Kind (Bare_Parse_Node_Expr) in Lkt_Parse_Node_Expr_Range;
         subtype Bare_Token_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Token_Lit)
               or else Kind (Bare_Token_Lit) in Lkt_Token_Lit_Range;
         subtype Bare_Token_No_Case_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Token_No_Case_Lit)
               or else Kind (Bare_Token_No_Case_Lit) in Lkt_Token_No_Case_Lit_Range;
         subtype Bare_Token_Pattern_Concat is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Token_Pattern_Concat)
               or else Kind (Bare_Token_Pattern_Concat) in Lkt_Token_Pattern_Concat_Range;
         subtype Bare_Token_Pattern_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Token_Pattern_Lit)
               or else Kind (Bare_Token_Pattern_Lit) in Lkt_Token_Pattern_Lit_Range;
         subtype Bare_Token_Ref is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Token_Ref)
               or else Kind (Bare_Token_Ref) in Lkt_Token_Ref_Range;
         subtype Bare_Id is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Id)
               or else Kind (Bare_Id) in Lkt_Id_Range;
         subtype Bare_Def_Id is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Def_Id)
               or else Kind (Bare_Def_Id) in Lkt_Def_Id_Range;
         subtype Bare_Module_Ref_Id is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Module_Ref_Id)
               or else Kind (Bare_Module_Ref_Id) in Lkt_Module_Ref_Id_Range;
         subtype Bare_Ref_Id is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ref_Id)
               or else Kind (Bare_Ref_Id) in Lkt_Ref_Id_Range;
         subtype Bare_If_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_If_Expr)
               or else Kind (Bare_If_Expr) in Lkt_If_Expr_Range;
         subtype Bare_Isa is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Isa)
               or else Kind (Bare_Isa) in Lkt_Isa_Range;
         subtype Bare_Keep_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Keep_Expr)
               or else Kind (Bare_Keep_Expr) in Lkt_Keep_Expr_Range;
         subtype Bare_Lambda_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lambda_Expr)
               or else Kind (Bare_Lambda_Expr) in Lkt_Lambda_Expr_Range;
         subtype Bare_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lit)
               or else Kind (Bare_Lit) in Lkt_Lit;
         subtype Bare_Big_Num_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Big_Num_Lit)
               or else Kind (Bare_Big_Num_Lit) in Lkt_Big_Num_Lit_Range;
         subtype Bare_Char_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Char_Lit)
               or else Kind (Bare_Char_Lit) in Lkt_Char_Lit_Range;
         subtype Bare_Null_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Null_Lit)
               or else Kind (Bare_Null_Lit) in Lkt_Null_Lit_Range;
         subtype Bare_Num_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Num_Lit)
               or else Kind (Bare_Num_Lit) in Lkt_Num_Lit_Range;
         subtype Bare_String_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_String_Lit)
               or else Kind (Bare_String_Lit) in Lkt_String_Lit;
         subtype Bare_Block_String_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Block_String_Lit)
               or else Kind (Bare_Block_String_Lit) in Lkt_Block_String_Lit_Range;
         subtype Bare_Single_Line_String_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Single_Line_String_Lit)
               or else Kind (Bare_Single_Line_String_Lit) in Lkt_Single_Line_String_Lit_Range;
         subtype Bare_Pattern_Single_Line_String_Lit is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Pattern_Single_Line_String_Lit)
               or else Kind (Bare_Pattern_Single_Line_String_Lit) in Lkt_Pattern_Single_Line_String_Lit_Range;
         subtype Bare_Logic_Assign is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Logic_Assign)
               or else Kind (Bare_Logic_Assign) in Lkt_Logic_Assign_Range;
         subtype Bare_Logic_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Logic_Expr)
               or else Kind (Bare_Logic_Expr) in Lkt_Logic_Expr_Range;
         subtype Bare_Logic_Propagate is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Logic_Propagate)
               or else Kind (Bare_Logic_Propagate) in Lkt_Logic_Propagate_Range;
         subtype Bare_Logic_Unify is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Logic_Unify)
               or else Kind (Bare_Logic_Unify) in Lkt_Logic_Unify_Range;
         subtype Bare_Match_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Match_Expr)
               or else Kind (Bare_Match_Expr) in Lkt_Match_Expr_Range;
         subtype Bare_Not_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Not_Expr)
               or else Kind (Bare_Not_Expr) in Lkt_Not_Expr_Range;
         subtype Bare_Paren_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Paren_Expr)
               or else Kind (Bare_Paren_Expr) in Lkt_Paren_Expr_Range;
         subtype Bare_Raise_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Raise_Expr)
               or else Kind (Bare_Raise_Expr) in Lkt_Raise_Expr_Range;
         subtype Bare_Subscript_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Subscript_Expr)
               or else Kind (Bare_Subscript_Expr) in Lkt_Subscript_Expr_Range;
         subtype Bare_Null_Cond_Subscript_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Null_Cond_Subscript_Expr)
               or else Kind (Bare_Null_Cond_Subscript_Expr) in Lkt_Null_Cond_Subscript_Expr_Range;
         subtype Bare_Try_Expr is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Try_Expr)
               or else Kind (Bare_Try_Expr) in Lkt_Try_Expr_Range;
         subtype Bare_Un_Op is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Un_Op)
               or else Kind (Bare_Un_Op) in Lkt_Un_Op_Range;
         subtype Bare_Full_Decl is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Full_Decl)
               or else Kind (Bare_Full_Decl) in Lkt_Full_Decl_Range;
         subtype Bare_Grammar_List_Sep is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_List_Sep)
               or else Kind (Bare_Grammar_List_Sep) in Lkt_Grammar_List_Sep_Range;
         subtype Bare_Import is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Import)
               or else Kind (Bare_Import) in Lkt_Import_Range;
         subtype Bare_Langkit_Root is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Langkit_Root)
               or else Kind (Bare_Langkit_Root) in Lkt_Langkit_Root_Range;
         subtype Bare_Lexer_Case_Rule is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lexer_Case_Rule)
               or else Kind (Bare_Lexer_Case_Rule) in Lkt_Lexer_Case_Rule_Range;
         subtype Bare_Lexer_Case_Rule_Send is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lexer_Case_Rule_Send)
               or else Kind (Bare_Lexer_Case_Rule_Send) in Lkt_Lexer_Case_Rule_Send_Range;
         subtype Bare_List_Kind is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_List_Kind)
               or else Kind (Bare_List_Kind) in Lkt_List_Kind;
         subtype Bare_List_Kind_One is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_List_Kind_One)
               or else Kind (Bare_List_Kind_One) in Lkt_List_Kind_One_Range;
         subtype Bare_List_Kind_Zero is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_List_Kind_Zero)
               or else Kind (Bare_List_Kind_Zero) in Lkt_List_Kind_Zero_Range;
         subtype Bare_Lkt_Node_Base_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lkt_Node_Base_List)
               or else Kind (Bare_Lkt_Node_Base_List) in Lkt_Lkt_Node_Base_List;
         subtype Bare_Base_Lexer_Case_Rule_Alt_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Base_Lexer_Case_Rule_Alt_List)
               or else Kind (Bare_Base_Lexer_Case_Rule_Alt_List) in Lkt_Base_Lexer_Case_Rule_Alt_List_Range;
         subtype Bare_Block_String_Line_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Block_String_Line_List)
               or else Kind (Bare_Block_String_Line_List) in Lkt_Block_String_Line_List_Range;
         subtype Bare_Call_Expr_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Call_Expr_List)
               or else Kind (Bare_Call_Expr_List) in Lkt_Call_Expr_List_Range;
         subtype Bare_Decl_Annotation_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Decl_Annotation_List)
               or else Kind (Bare_Decl_Annotation_List) in Lkt_Decl_Annotation_List_Range;
         subtype Bare_Elsif_Branch_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Elsif_Branch_List)
               or else Kind (Bare_Elsif_Branch_List) in Lkt_Elsif_Branch_List_Range;
         subtype Bare_Enum_Class_Alt_Decl_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Enum_Class_Alt_Decl_List)
               or else Kind (Bare_Enum_Class_Alt_Decl_List) in Lkt_Enum_Class_Alt_Decl_List_Range;
         subtype Bare_Enum_Class_Case_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Enum_Class_Case_List)
               or else Kind (Bare_Enum_Class_Case_List) in Lkt_Enum_Class_Case_List_Range;
         subtype Bare_Enum_Lit_Decl_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Enum_Lit_Decl_List)
               or else Kind (Bare_Enum_Lit_Decl_List) in Lkt_Enum_Lit_Decl_List_Range;
         subtype Bare_Expr_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Expr_List)
               or else Kind (Bare_Expr_List) in Lkt_Expr_List_Range;
         subtype Bare_Any_Of_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Any_Of_List)
               or else Kind (Bare_Any_Of_List) in Lkt_Any_Of_List_Range;
         subtype Bare_Full_Decl_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Full_Decl_List)
               or else Kind (Bare_Full_Decl_List) in Lkt_Full_Decl_List_Range;
         subtype Bare_Decl_Block is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Decl_Block)
               or else Kind (Bare_Decl_Block) in Lkt_Decl_Block_Range;
         subtype Bare_Generic_Formal_Decl_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Generic_Formal_Decl_List)
               or else Kind (Bare_Generic_Formal_Decl_List) in Lkt_Generic_Formal_Decl_List_Range;
         subtype Bare_Fun_Arg_Decl_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Fun_Arg_Decl_List)
               or else Kind (Bare_Fun_Arg_Decl_List) in Lkt_Fun_Arg_Decl_List_Range;
         subtype Bare_Grammar_Expr_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Expr_List)
               or else Kind (Bare_Grammar_Expr_List) in Lkt_Grammar_Expr_List_Range;
         subtype Bare_Grammar_Expr_List_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Grammar_Expr_List_List)
               or else Kind (Bare_Grammar_Expr_List_List) in Lkt_Grammar_Expr_List_List_Range;
         subtype Bare_Import_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Import_List)
               or else Kind (Bare_Import_List) in Lkt_Import_List_Range;
         subtype Bare_Lambda_Arg_Decl_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lambda_Arg_Decl_List)
               or else Kind (Bare_Lambda_Arg_Decl_List) in Lkt_Lambda_Arg_Decl_List_Range;
         subtype Bare_Lkt_Node_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Lkt_Node_List)
               or else Kind (Bare_Lkt_Node_List) in Lkt_Lkt_Node_List_Range;
         subtype Bare_Block_Decl_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Block_Decl_List)
               or else Kind (Bare_Block_Decl_List) in Lkt_Block_Decl_List_Range;
         subtype Bare_Match_Branch_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Match_Branch_List)
               or else Kind (Bare_Match_Branch_List) in Lkt_Match_Branch_List_Range;
         subtype Bare_Param_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Param_List)
               or else Kind (Bare_Param_List) in Lkt_Param_List_Range;
         subtype Bare_Ref_Id_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Ref_Id_List)
               or else Kind (Bare_Ref_Id_List) in Lkt_Ref_Id_List_Range;
         subtype Bare_Type_Ref_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Type_Ref_List)
               or else Kind (Bare_Type_Ref_List) in Lkt_Type_Ref_List_Range;
         subtype Bare_Isa_List is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Isa_List)
               or else Kind (Bare_Isa_List) in Lkt_Isa_List_Range;
         subtype Bare_Match_Branch is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Match_Branch)
               or else Kind (Bare_Match_Branch) in Lkt_Match_Branch_Range;
         subtype Bare_Op is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op)
               or else Kind (Bare_Op) in Lkt_Op;
         subtype Bare_Op_Amp is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Amp)
               or else Kind (Bare_Op_Amp) in Lkt_Op_Amp_Range;
         subtype Bare_Op_And is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_And)
               or else Kind (Bare_Op_And) in Lkt_Op_And_Range;
         subtype Bare_Op_Div is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Div)
               or else Kind (Bare_Op_Div) in Lkt_Op_Div_Range;
         subtype Bare_Op_Eq is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Eq)
               or else Kind (Bare_Op_Eq) in Lkt_Op_Eq_Range;
         subtype Bare_Op_Gt is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Gt)
               or else Kind (Bare_Op_Gt) in Lkt_Op_Gt_Range;
         subtype Bare_Op_Gte is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Gte)
               or else Kind (Bare_Op_Gte) in Lkt_Op_Gte_Range;
         subtype Bare_Op_Logic_And is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Logic_And)
               or else Kind (Bare_Op_Logic_And) in Lkt_Op_Logic_And_Range;
         subtype Bare_Op_Logic_Or is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Logic_Or)
               or else Kind (Bare_Op_Logic_Or) in Lkt_Op_Logic_Or_Range;
         subtype Bare_Op_Lt is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Lt)
               or else Kind (Bare_Op_Lt) in Lkt_Op_Lt_Range;
         subtype Bare_Op_Lte is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Lte)
               or else Kind (Bare_Op_Lte) in Lkt_Op_Lte_Range;
         subtype Bare_Op_Minus is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Minus)
               or else Kind (Bare_Op_Minus) in Lkt_Op_Minus_Range;
         subtype Bare_Op_Mult is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Mult)
               or else Kind (Bare_Op_Mult) in Lkt_Op_Mult_Range;
         subtype Bare_Op_Ne is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Ne)
               or else Kind (Bare_Op_Ne) in Lkt_Op_Ne_Range;
         subtype Bare_Op_Or is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Or)
               or else Kind (Bare_Op_Or) in Lkt_Op_Or_Range;
         subtype Bare_Op_Or_Int is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Or_Int)
               or else Kind (Bare_Op_Or_Int) in Lkt_Op_Or_Int_Range;
         subtype Bare_Op_Plus is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Op_Plus)
               or else Kind (Bare_Op_Plus) in Lkt_Op_Plus_Range;
         subtype Bare_Param is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Param)
               or else Kind (Bare_Param) in Lkt_Param_Range;
         subtype Bare_Type_Ref is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Type_Ref)
               or else Kind (Bare_Type_Ref) in Lkt_Type_Ref;
         subtype Bare_Default_List_Type_Ref is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Default_List_Type_Ref)
               or else Kind (Bare_Default_List_Type_Ref) in Lkt_Default_List_Type_Ref_Range;
         subtype Bare_Function_Type_Ref is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Function_Type_Ref)
               or else Kind (Bare_Function_Type_Ref) in Lkt_Function_Type_Ref_Range;
         subtype Bare_Generic_Type_Ref is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Generic_Type_Ref)
               or else Kind (Bare_Generic_Type_Ref) in Lkt_Generic_Type_Ref_Range;
         subtype Bare_Simple_Type_Ref is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Simple_Type_Ref)
               or else Kind (Bare_Simple_Type_Ref) in Lkt_Simple_Type_Ref_Range;
         subtype Bare_Var_Bind is Bare_Lkt_Node
            with Dynamic_Predicate =>
               Is_Null (Bare_Var_Bind)
               or else Kind (Bare_Var_Bind) in Lkt_Var_Bind_Range;

   package Alloc_AST_List_Array is new Liblktlang_Support.Bump_Ptr.Array_Alloc
     (Element_T  => Bare_Lkt_Node,
      Index_Type => Positive);
   --  Allocator for array of nodes, used in list nodes

   type Rewriting_Handle_Pointer is new System.Address;
   No_Rewriting_Handle_Pointer : constant Rewriting_Handle_Pointer :=
      Rewriting_Handle_Pointer (System.Null_Address);

      Properties_Traces : constant GNATCOLL.Traces.Trace_Handle :=
         GNATCOLL.Traces.Create
           ("LANGKIT.PROPERTIES", GNATCOLL.Traces.On
           );

   function Node_Sloc_Image (Self : Bare_Lkt_Node) return Text_Type;
   --  Return the image of the sloc for ``Self``, to be used in
   --  ``Short_Text_Image``.

   function Short_Text_Image (Self : Bare_Lkt_Node) return Text_Type;
   --  Return a short representation of the node, containing just the kind
   --  name and the sloc, or "None" if Self is null.

   function Is_Token_Node (Node : Bare_Lkt_Node) return Boolean;
   --  Return whether this node is a node that contains only a single token.

   function Is_Synthetic (Node : Bare_Lkt_Node) return Boolean;
   --  Return whether this node is synthetic.

   procedure Raise_Property_Exception
     (Node    : Bare_Lkt_Node;
      Exc     : Ada.Exceptions.Exception_Id;
      Message : String)
     with No_Return;
   --  Raise an exception of the given type and with the given message. Prepend
   --  the sloc of the given node to the exception message.

   ---------------------------
   -- Iterators safety nets --
   ---------------------------

   type Iterator_Safety_Net is record
      Context         : Internal_Context;
      Context_Serial  : Version_Number;
      Context_Version : Version_Number;
      --  Analysis context, its serial number and version number at the time
      --  this safety net was produced.
   end record;

   No_Iterator_Safety_Net : constant Iterator_Safety_Net := (null, 0, 0);

   function Create_Safety_Net
     (Context : Internal_Context) return Iterator_Safety_Net;
   --  Create an iterator safety net from the given Context

   procedure Check_Safety_Net (Self : Iterator_Safety_Net);
   --  Check that the given iterator safety net is still valid, raising a
   --  Stale_Reference_Error if it is not.

   -----------------
   -- String type --
   -----------------

   type String_Record (Length : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Content : Text_Type (1 .. Length);
   end record;

   type String_Type is access all String_Record;

   Empty_String_Record : aliased String_Record :=
     (Length => 0, Ref_Count => -1, Content => (others => <>));
   Empty_String        : constant String_Type := Empty_String_Record'Access;

   procedure Inc_Ref (Self : String_Type);
   procedure Dec_Ref (Self : in out String_Type);
   procedure Free is new Ada.Unchecked_Deallocation
     (String_Record, String_Type);

   function Create_String (Content : Text_Type) return String_Type;
   function Create_String (Content : Unbounded_Text_Type) return String_Type;
   --  Create string values from their content. The overload for unbounded
   --  strings makes it easier for callers to avoid using the secondary stack,
   --  which can be a problem for big strings.

   function Concat_String (Left, Right : String_Type) return String_Type;
   --  Return a new string that is the concatenation of ``Left`` and ``Right``

   function Equivalent (Left, Right : String_Type) return Boolean;
   --  Return whether ``Left`` and ``Right`` contain equal strings

   ---------------------------
   -- Environments handling --
   ---------------------------

   subtype Long_Long_Natural is Long_Long_Integer
      range 0 .. Long_Long_Integer'Last;

   
      type Internal_Metadata;
      

   

      

      type Internal_Metadata is record

            Dummy : Character;
      end record
        with Convention => C;




   


      function Trace_Image (R : Internal_Metadata) return String;


   
      


      No_Metadata : constant Internal_Metadata :=
      (Dummy => Character'Val (0));


   function Hash (Self : Internal_Metadata) return Hash_Type;

   
      type Internal_Inner_Env_Assoc;
      

   

      

      type Internal_Inner_Env_Assoc is record

               Key : aliased Symbol_Type;
               
               
               Value : aliased Bare_Lkt_Node;
               
               
               Rebindings : aliased Env_Rebindings;
               
               
               Metadata : aliased Internal_Metadata;
               
               
      end record
        with Convention => C;




   


      function Trace_Image (R : Internal_Inner_Env_Assoc) return String;


   
      


      No_Inner_Env_Assoc : constant Internal_Inner_Env_Assoc :=
      (
               Key => No_Symbol, 
               Value => No_Bare_Lkt_Node, 
               Rebindings => null, 
               Metadata => No_Metadata
      );

   function Get_Key (Self : Internal_Inner_Env_Assoc) return Thin_Symbol
   is (Thin (Self.Key));
   function Get_Node
     (Self : Internal_Inner_Env_Assoc) return Bare_Lkt_Node
   is (Self.Value);
   function Get_Rebindings
     (Self : Internal_Inner_Env_Assoc) return Env_Rebindings
   is (Self.Rebindings);
   function Get_Metadata
     (Self : Internal_Inner_Env_Assoc) return Internal_Metadata
   is (Self.Metadata);

   
   type Internal_Inner_Env_Assoc_Array_Record;
   type Internal_Inner_Env_Assoc_Array_Access is access all Internal_Inner_Env_Assoc_Array_Record;

      
   type Internal_Internal_Inner_Env_Assoc_Iterator;
   type Internal_Inner_Env_Assoc_Iterator_Access is access all Internal_Internal_Inner_Env_Assoc_Iterator;


   

   

   type Internal_Internal_Inner_Env_Assoc_Array is
      array (Positive range <>) of Internal_Inner_Env_Assoc;

   type Internal_Inner_Env_Assoc_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Inner_Env_Assoc_Array (1 .. N);
   end record;

   Empty_Internal_Inner_Env_Assoc_Array_Record : aliased Internal_Inner_Env_Assoc_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Inner_Env_Assoc_Array_Type : constant Internal_Inner_Env_Assoc_Array_Access :=
      Empty_Internal_Inner_Env_Assoc_Array_Record'Access;


   function Create_Internal_Inner_Env_Assoc_Array (Items_Count : Natural) return Internal_Inner_Env_Assoc_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Inner_Env_Assoc_Array
     (Items : Internal_Internal_Inner_Env_Assoc_Array) return Internal_Inner_Env_Assoc_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Inner_Env_Assoc_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Inner_Env_Assoc;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Inner_Env_Assoc_Array_Access) return Internal_Inner_Env_Assoc_Array_Access;


   function Length (T : Internal_Inner_Env_Assoc_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Array_Access);
   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Array_Access);

   function Equivalent (L, R : Internal_Inner_Env_Assoc_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Inner_Env_Assoc_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Inner_Env_Assoc_Array_Record, Internal_Inner_Env_Assoc_Array_Access);

      

   

   type Internal_Internal_Inner_Env_Assoc_Iterator is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero.
      --  Negative values are interpreted as "always living singleton".

      Safety_Net : Iterator_Safety_Net;
      --  Safety net for the iterator. Used to check that values produced by
      --  the iterator are still valid. Unlike for other types, we put the
      --  safety net in the internal type so that it can be used in all other
      --  APIs (Python, ...).
      --
      --  While other types (except nodes) are "deeply" converted to native
      --  APIs (for instance: internal arrays are turned into native Python
      --  lists, likewise for array items, etc.), iterators are lazy, so the
      --  deep conversion is not possible.

      Elements : Internal_Inner_Env_Assoc_Array_Access;
      Index    : Positive;
   end record;

   Empty_Internal_Internal_Inner_Env_Assoc_Iterator : aliased Internal_Internal_Inner_Env_Assoc_Iterator :=
     (Ref_Count  => -1,
      Safety_Net => No_Iterator_Safety_Net,
      Elements   => No_Internal_Inner_Env_Assoc_Array_Type,
      Index      => 1);
   No_Internal_Inner_Env_Assoc_Iterator_Type : constant Internal_Inner_Env_Assoc_Iterator_Access :=
      Empty_Internal_Internal_Inner_Env_Assoc_Iterator'Access;

   function Next
     (T       : Internal_Inner_Env_Assoc_Iterator_Access;
      Element : out Internal_Inner_Env_Assoc) return Boolean;

   procedure Inc_Ref (T : Internal_Inner_Env_Assoc_Iterator_Access);
   procedure Dec_Ref (T : in out Internal_Inner_Env_Assoc_Iterator_Access);

      function Trace_Image (A : Internal_Inner_Env_Assoc_Iterator_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Internal_Inner_Env_Assoc_Iterator, Internal_Inner_Env_Assoc_Iterator_Access);


   function Inner_Env_Assoc_Get
     (Self  : Internal_Inner_Env_Assoc_Array_Access;
      Index : Positive) return Internal_Inner_Env_Assoc
   is (Self.Items (Index));

   function Combine
     (L, R : Internal_Metadata) return Internal_Metadata;
   --  The combine function on environments metadata does a boolean Or on every
   --  boolean component of the env metadata.

   function Can_Reach (El, From : Bare_Lkt_Node) return Boolean
     with Inline;
   --  Return whether El can reach From, from a sequential viewpoint. If
   --  elements are declared in different units, it will always return True,
   --  eg this does not handle general visibility issues, just sequentiality of
   --  declarations.

   function AST_Envs_Node_Text_Image
     (Node  : Bare_Lkt_Node;
      Short : Boolean := True) return Text_Type;
   --  Return a "sourcefile:lineno:columnno" corresponding to the starting sloc
   --  of Node. Used to create a human-readable representation for env.
   --  rebindings.

   function Is_Rebindable (Node : Bare_Lkt_Node) return Boolean;

   function Acquire_Rebinding
     (Node             : Bare_Lkt_Node;
      Parent           : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings;
   --  Initialize and return a fresh rebinding

   procedure Release_Rebinding (Self : in out Env_Rebindings);
   --  Mark the rebinding as unused, so that a future call to Acquire_Rebinding
   --  can return it.

   procedure Register_Rebinding
     (Node : Bare_Lkt_Node; Rebinding : Env_Rebindings);
   --  Register a rebinding to be destroyed when Node's analysis unit is
   --  destroyed or reparsed.


   function Element_Parent
     (Node : Bare_Lkt_Node) return Bare_Lkt_Node;

   function Hash (Node : Bare_Lkt_Node) return Hash_Type;
   function Node_Unit (Node : Bare_Lkt_Node) return Generic_Unit_Ptr;
   function Named_Hash (Node : Bare_Lkt_Node) return Hash_Type is
     (Hash (Node));

   No_Analysis_Unit : constant Internal_Unit := null;

   function Convert_Unit is new Ada.Unchecked_Conversion
     (Generic_Unit_Ptr, Internal_Unit);
   function Convert_Unit is new Ada.Unchecked_Conversion
     (Internal_Unit, Generic_Unit_Ptr);

   function Unit_Version (Unit : Generic_Unit_Ptr) return Version_Number;
   --  Return the version for Unit. Version is a number that is incremented
   --  every time Unit changes.

   function Get_Context_Version
     (Node : Bare_Lkt_Node) return Version_Number;
   --  Assuming that Node is not null, return the version number for Node's
   --  context, which is incremented every time a unit in this context is
   --  parsed.

   function Self_Env (Node : Bare_Lkt_Node) return Lexical_Env;

   type Ref_Category is
     (Nocat);
   type Ref_Categories is array (Ref_Category) of Boolean;
   pragma Pack (Ref_Categories);

   function Properties_May_Raise
     (Exc : Ada.Exceptions.Exception_Occurrence) return Boolean;
   --  Return if ``Exc`` is one of the exceptions that properties are allowed
   --  to raise.

   package AST_Envs is new Liblktlang_Support.Lexical_Envs_Impl
     (Get_Unit_Version         => Unit_Version,
      Node_Type                => Bare_Lkt_Node,
      Node_Metadata            => Internal_Metadata,
      No_Node                  => null,
      Empty_Metadata           => No_Metadata,
      Node_Unit                => Node_Unit,
      Node_Hash                => Named_Hash,
      Metadata_Hash            => Hash,
      Combine                  => Combine,
      Node_Text_Image          => AST_Envs_Node_Text_Image,
      Acquire_Rebinding        => Acquire_Rebinding,
      Register_Rebinding       => Register_Rebinding,
      Ref_Category             => Ref_Category,
      Ref_Categories           => Ref_Categories,
      Inner_Env_Assoc          => Internal_Inner_Env_Assoc,
      Inner_Env_Assoc_Array    => Internal_Inner_Env_Assoc_Array_Access,
      Get                      => Inner_Env_Assoc_Get);

   use AST_Envs;
   subtype Internal_Entity is AST_Envs.Entity;
   subtype Internal_Entity_Info is AST_Envs.Entity_Info;

   No_Entity_Info : constant Internal_Entity_Info :=
     (No_Metadata, null, False);
   No_Entity : constant Internal_Entity :=
     (null, No_Entity_Info);

   function Hash_Entity (Self : Internal_Entity) return Hash_Type;
   --  Hash function to use in the public API. It is like the regular one, but
   --  compares metadata according to the user specification in the DSL.

   function Compare_Entity (Left, Right : Internal_Entity) return Boolean;
   --  Equality function to use in the public API. It is like the regular one,
   --  but compares metadata according to the user specification in the DSL.

   function Compare_Metadata (L, R : Internal_Metadata) return Boolean;
   --  Compare metadata ``L`` and ``R`` for public entity comparison

   function Create_Dynamic_Lexical_Env
     (Self              : Bare_Lkt_Node;
      Assocs_Getter     : Inner_Env_Assocs_Resolver;
      Assoc_Resolver    : Entity_Resolver;
      Transitive_Parent : Boolean;
      Sym_Table         : Symbol_Table) return Lexical_Env;
   --  Helper for properties code generation: wrapper around
   --  AST_Envs.Create_Dynamic_Lexical_Env.

      function Hash (B : Boolean) return Hash_Type;





   --------------------------
   -- Big integers wrapper --
   --------------------------

   type Big_Integer_Record is limited record
      Value     : GNATCOLL.GMP.Integers.Big_Integer;
      Ref_Count : Integer;
      --  Number of owners. When it drops to 0, this record can be destroyed.
      --  If -1, this is a static big integer: Inc_Ref and Dec_Ref are no-ops.
   end record;

   type Big_Integer_Type is access all Big_Integer_Record;
   pragma No_Strict_Aliasing (Big_Integer_Type);

   function Create_Big_Integer
     (Image : String; Base : Integer := 10) return Big_Integer_Type;
   function Create_Big_Integer
     (Big_Int : GNATCOLL.GMP.Integers.Big_Integer) return Big_Integer_Type;
   function Create_Big_Integer (Int : Integer) return Big_Integer_Type;
   function Create_Public_Big_Integer
     (Big_Int : Big_Integer_Type) return GNATCOLL.GMP.Integers.Big_Integer;

   No_Big_Integer_Record : aliased Big_Integer_Record :=
     (Value => <>, Ref_Count => -1);
   No_Big_Integer : constant Big_Integer_Type := No_Big_Integer_Record'Access;

   function To_Integer
     (Self    : Bare_Lkt_Node;
      Big_Int : Big_Integer_Type) return Integer;
   --  Convert ``Big_Int`` into a regular integer, raising a ``Property_Error``
   --  if it is out of range (using ``Self`` to provide context for this
   --  error).

   procedure Inc_Ref (Big_Int : Big_Integer_Type);
   procedure Dec_Ref (Big_Int : in out Big_Integer_Type);

   function Equivalent (Left, Right : Big_Integer_Type) return Boolean;
   function "<" (Left, Right : Big_Integer_Type) return Boolean;
   function "<=" (Left, Right : Big_Integer_Type) return Boolean;
   function ">" (Left, Right : Big_Integer_Type) return Boolean;
   function ">=" (Left, Right : Big_Integer_Type) return Boolean;

   function "+" (Left, Right : Big_Integer_Type) return Big_Integer_Type;
   function "-" (Left, Right : Big_Integer_Type) return Big_Integer_Type;
   function "-" (Value : Big_Integer_Type) return Big_Integer_Type;

   function Trace_Image (I : Big_Integer_Type) return String;

      function Trace_Image
        (Node       : Bare_Lkt_Node;
         Decoration : Boolean := True) return String;

   function Is_Incomplete (Node : Bare_Lkt_Node) return Boolean;
   --  Return whether this node is incomplete or not.  Incomplete nodes are a
   --  result of the parsing of a node failing as a result of a Cut parser
   --  annotation.

   function Kind_Name (Node : Bare_Lkt_Node) return String;
   --  Return the concrete kind for Node

   -------------------
   -- Node Builders --
   -------------------

   --  A node builder is basically a functor that takes one argument (a "parent
   --  node") and that returns a node (either an existing node or a node that
   --  the node builder creates).

   type Node_Builder_Record is abstract tagged record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.
   end record;
   type Node_Builder_Type is access all Node_Builder_Record'Class;

   function Build
     (Self              : Node_Builder_Record;
      Parent, Self_Node : Bare_Lkt_Node) return Bare_Lkt_Node
   is abstract;
   --  Return the node that ``Self`` must create.
   --
   --  If actual node synthetization occurs, ``Parent`` is used to initialize
   --  the parent link of the returned node.
   --
   --  This function is meant to be called in a property: ``Self_Node`` must be
   --  the ``Self`` of the calling property.

   procedure Release (Self : in out Node_Builder_Record) is null;
   --  Free resources for this node builder

   type Copy_Node_Builder_Record is new Node_Builder_Record with record
      Value : Bare_Lkt_Node;
      --  Existing node that this builder must yield
   end record;

   overriding function Build
     (Self              : Copy_Node_Builder_Record;
      Parent, Self_Node : Bare_Lkt_Node) return Bare_Lkt_Node
   is (Self.Value);

   Null_Node_Builder_Record : aliased Copy_Node_Builder_Record :=
     (Ref_Count => -1, Value => null);
   Null_Node_Builder        : constant Node_Builder_Type :=
     Null_Node_Builder_Record'Access;

   procedure Free is new Ada.Unchecked_Deallocation
     (Node_Builder_Record'Class, Node_Builder_Type);

   procedure Inc_Ref (Self : Node_Builder_Type);
   procedure Dec_Ref (Self : in out Node_Builder_Type);

   function Create_Copy_Node_Builder
     (Value : Bare_Lkt_Node) return Node_Builder_Type;



   -----------------------------------------------
   -- Structure types (incomplete declarations) --
   -----------------------------------------------

         
      type Internal_Decoded_Char_Value;
      --  Result for ``CharLit.p_denoted_value``.
   --
   --  If that property is successful, set ``has_error`` to false and ``value``
   --  to the decoded character value. Otherwise, set ``has_error`` to true and
   --  ``error_sloc`` and ``error_message`` to give information about the
   --  decoding failure.

         
      type Internal_Decoded_String_Value;
      --  Result for ``StringLit.p_denoted_value``.
   --
   --  If that property is successful, set ``has_error`` to false and ``value``
   --  to the decoded string value. Otherwise, set ``has_error`` to true and
   --  ``error_sloc`` and ``error_message`` to give information about the
   --  decoding failure.

         
      type Internal_Designated_Env;
      --  Designate an environment for an env spec action.
   --
   --  The designated environment can be either, depending on the ``Kind``
   --  field:
   --
   --  * If ``Kind`` is ``None``, no environment is designated.
   --
   --  * If ``Kind`` is ``Current_Env``, designate the current environment at
   --    this point during PLE.
   --
   --  * If ``Kind`` is ``Named_Env``, designate the environment which has
   --    precedence for the ``Env_Name`` environment name. If ``Env_Name`` is
   --    null, this designates to environment.
   --
   --  * If ``Kind`` is ``Direct_Env``, the direct value for the designated
   --    environment. That environment must be a primary one and cannot be
   --    foreign to the node currently processed by PLE. If it is the empty
   --    environment, do nothing.

         

         

         
      type Internal_Entity_Expr;
      

         
      type Internal_Entity_Any_Of;
      

         
      type Internal_Entity_Lkt_Node_Base_List;
      

         
      type Internal_Entity_Expr_List;
      

         
      type Internal_Entity_Any_Of_List;
      

         
      type Internal_Entity_Decl;
      

         
      type Internal_Entity_Type_Decl;
      

         
      type Internal_Entity_Any_Type_Decl;
      

         
      type Internal_Entity_Array_Literal;
      

         
      type Internal_Entity_Base_Call_Expr;
      

         
      type Internal_Entity_Base_Dot_Expr;
      

         
      type Internal_Entity_Base_Grammar_Rule_Decl;
      

         
      type Internal_Entity_Base_Lexer_Case_Rule_Alt;
      

         
      type Internal_Entity_Base_Lexer_Case_Rule_Alt_List;
      

         
      type Internal_Entity_Base_Val_Decl;
      

         
      type Internal_Entity_Named_Type_Decl;
      

         
      type Internal_Entity_Basic_Class_Decl;
      

         
      type Internal_Entity_Lit;
      

         
      type Internal_Entity_Big_Num_Lit;
      

         
      type Internal_Entity_Bin_Op;
      

         
      type Internal_Entity_Lkt_Node_List;
      

         
      type Internal_Entity_Block_Decl_List;
      

         
      type Internal_Entity_Block_Expr;
      

         
      type Internal_Entity_Block_String_Line;
      

         
      type Internal_Entity_Block_String_Line_List;
      

         
      type Internal_Entity_String_Lit;
      

         
      type Internal_Entity_Block_String_Lit;
      

         
      type Internal_Entity_Call_Expr;
      

         
      type Internal_Entity_Call_Expr_List;
      

         
      type Internal_Entity_Cast_Expr;
      

         
      type Internal_Entity_Char_Lit;
      

         
      type Internal_Entity_Class_Decl;
      

         
      type Internal_Entity_Class_Qualifier;
      

         
      type Internal_Entity_Class_Qualifier_Absent;
      

         
      type Internal_Entity_Class_Qualifier_Present;
      

         
      type Internal_Entity_User_Val_Decl;
      

         
      type Internal_Entity_Explicitly_Typed_Decl;
      

         
      type Internal_Entity_Component_Decl;
      

         
      type Internal_Entity_Decl_Annotation;
      

         
      type Internal_Entity_Decl_Annotation_List;
      

         
      type Internal_Entity_Decl_Annotation_Params;
      

         
      type Internal_Entity_Full_Decl_List;
      

         
      type Internal_Entity_Decl_Block;
      

         
      type Internal_Entity_Id;
      

         
      type Internal_Entity_Def_Id;
      

         
      type Internal_Entity_Type_Ref;
      

         
      type Internal_Entity_Default_List_Type_Ref;
      

         
      type Internal_Entity_Dot_Expr;
      

         
      type Internal_Entity_Dyn_Env_Wrapper;
      

         
      type Internal_Entity_Dyn_Var_Decl;
      

         
      type Internal_Entity_Elsif_Branch;
      

         
      type Internal_Entity_Elsif_Branch_List;
      

         
      type Internal_Entity_Enum_Class_Alt_Decl;
      

         
      type Internal_Entity_Enum_Class_Alt_Decl_List;
      

         
      type Internal_Entity_Enum_Class_Case;
      

         
      type Internal_Entity_Enum_Class_Case_List;
      

         
      type Internal_Entity_Enum_Class_Decl;
      

         
      type Internal_Entity_Enum_Lit_Decl;
      

         
      type Internal_Entity_Enum_Lit_Decl_List;
      

         
      type Internal_Entity_Enum_Type_Decl;
      

         
      type Internal_Entity_Env_Spec_Decl;
      

         
      type Internal_Entity_Error_On_Null;
      

         
      type Internal_Entity_Excludes_Null;
      

         
      type Internal_Entity_Excludes_Null_Absent;
      

         
      type Internal_Entity_Excludes_Null_Present;
      

         
      type Internal_Entity_Field_Decl;
      

         
      type Internal_Entity_Full_Decl;
      

         
      type Internal_Entity_Fun_Arg_Decl;
      

         
      type Internal_Entity_Fun_Arg_Decl_List;
      

         
      type Internal_Entity_Fun_Decl;
      

         
      type Internal_Entity_Function_Type;
      

         
      type Internal_Entity_Function_Type_Ref;
      

         
      type Internal_Entity_Generic_Decl;
      

         
      type Internal_Entity_Generic_Formal_Decl_List;
      

         
      type Internal_Entity_Generic_Formal_Type_Decl;
      

         
      type Internal_Entity_Generic_Instantiation;
      

         
      type Internal_Entity_Generic_Type_Ref;
      

         
      type Internal_Entity_Grammar_Expr;
      

         
      type Internal_Entity_Grammar_Cut;
      

         
      type Internal_Entity_Grammar_Decl;
      

         
      type Internal_Entity_Grammar_Discard;
      

         
      type Internal_Entity_Grammar_Dont_Skip;
      

         
      type Internal_Entity_Grammar_Expr_List;
      

         
      type Internal_Entity_Grammar_Expr_List_List;
      

         
      type Internal_Entity_Grammar_Pick;
      

         
      type Internal_Entity_Grammar_Implicit_Pick;
      

         
      type Internal_Entity_Grammar_List;
      

         
      type Internal_Entity_Grammar_List_Sep;
      

         
      type Internal_Entity_Grammar_Null;
      

         
      type Internal_Entity_Grammar_Opt;
      

         
      type Internal_Entity_Grammar_Opt_Error;
      

         
      type Internal_Entity_Grammar_Opt_Error_Group;
      

         
      type Internal_Entity_Grammar_Opt_Group;
      

         
      type Internal_Entity_Grammar_Or_Expr;
      

         
      type Internal_Entity_Grammar_Predicate;
      

         
      type Internal_Entity_Grammar_Rule_Decl;
      

         
      type Internal_Entity_Grammar_Rule_Ref;
      

         
      type Internal_Entity_Grammar_Skip;
      

         
      type Internal_Entity_Grammar_Stop_Cut;
      

         
      type Internal_Entity_If_Expr;
      

         
      type Internal_Entity_Import;
      

         
      type Internal_Entity_Import_List;
      

         
      type Internal_Entity_Isa;
      

         
      type Internal_Entity_Type_Ref_List;
      

         
      type Internal_Entity_Isa_List;
      

         
      type Internal_Entity_Keep_Expr;
      

         
      type Internal_Entity_Lambda_Arg_Decl;
      

         
      type Internal_Entity_Lambda_Arg_Decl_List;
      

         
      type Internal_Entity_Lambda_Expr;
      

         
      type Internal_Entity_Langkit_Root;
      

         
      type Internal_Entity_Lexer_Case_Rule;
      

         
      type Internal_Entity_Lexer_Case_Rule_Cond_Alt;
      

         
      type Internal_Entity_Lexer_Case_Rule_Default_Alt;
      

         
      type Internal_Entity_Lexer_Case_Rule_Send;
      

         
      type Internal_Entity_Lexer_Decl;
      

         
      type Internal_Entity_Lexer_Family_Decl;
      

         
      type Internal_Entity_List_Kind;
      

         
      type Internal_Entity_List_Kind_One;
      

         
      type Internal_Entity_List_Kind_Zero;
      

         
      type Internal_Entity_Logic_Assign;
      

         
      type Internal_Entity_Logic_Call_Expr;
      

         
      type Internal_Entity_Logic_Expr;
      

         
      type Internal_Entity_Logic_Predicate;
      

         
      type Internal_Entity_Logic_Propagate;
      

         
      type Internal_Entity_Logic_Propagate_Call;
      

         
      type Internal_Entity_Logic_Unify;
      

         
      type Internal_Entity_Match_Branch;
      

         
      type Internal_Entity_Match_Branch_List;
      

         
      type Internal_Entity_Match_Expr;
      

         
      type Internal_Entity_Match_Val_Decl;
      

         
      type Internal_Entity_Module_Ref_Id;
      

         
      type Internal_Entity_Node_Decl;
      

         
      type Internal_Entity_Not_Expr;
      

         
      type Internal_Entity_Null_Cond_Dotted_Name;
      

         
      type Internal_Entity_Subscript_Expr;
      

         
      type Internal_Entity_Null_Cond_Subscript_Expr;
      

         
      type Internal_Entity_Null_Lit;
      

         
      type Internal_Entity_Num_Lit;
      

         
      type Internal_Entity_Op;
      

         
      type Internal_Entity_Op_Amp;
      

         
      type Internal_Entity_Op_And;
      

         
      type Internal_Entity_Op_Div;
      

         
      type Internal_Entity_Op_Eq;
      

         
      type Internal_Entity_Op_Gt;
      

         
      type Internal_Entity_Op_Gte;
      

         
      type Internal_Entity_Op_Logic_And;
      

         
      type Internal_Entity_Op_Logic_Or;
      

         
      type Internal_Entity_Op_Lt;
      

         
      type Internal_Entity_Op_Lte;
      

         
      type Internal_Entity_Op_Minus;
      

         
      type Internal_Entity_Op_Mult;
      

         
      type Internal_Entity_Op_Ne;
      

         
      type Internal_Entity_Op_Or;
      

         
      type Internal_Entity_Op_Or_Int;
      

         
      type Internal_Entity_Op_Plus;
      

         
      type Internal_Entity_Param;
      

         
      type Internal_Entity_Param_List;
      

         
      type Internal_Entity_Paren_Expr;
      

         
      type Internal_Entity_Parse_Node_Expr;
      

         
      type Internal_Entity_Single_Line_String_Lit;
      

         
      type Internal_Entity_Pattern_Single_Line_String_Lit;
      

         
      type Internal_Entity_Raise_Expr;
      

         
      type Internal_Entity_Ref_Id;
      

         
      type Internal_Entity_Ref_Id_List;
      

         
      type Internal_Entity_Self_Decl;
      

         
      type Internal_Entity_Simple_Type_Ref;
      

         
      type Internal_Entity_Struct_Decl;
      

         
      type Internal_Entity_Synth_Arg_Decl;
      

         
      type Internal_Entity_Synth_Fun_Decl;
      

         
      type Internal_Entity_Synthetic_Lexer_Decl;
      

         
      type Internal_Entity_Token_Lit;
      

         
      type Internal_Entity_Token_No_Case_Lit;
      

         
      type Internal_Entity_Token_Pattern_Concat;
      

         
      type Internal_Entity_Token_Pattern_Lit;
      

         
      type Internal_Entity_Token_Ref;
      

         
      type Internal_Entity_Trait_Decl;
      

         
      type Internal_Entity_Try_Expr;
      

         
      type Internal_Entity_Un_Op;
      

         
      type Internal_Entity_Val_Decl;
      

         
      type Internal_Entity_Var_Bind;
      

         
      type Internal_Env_Assoc;
      

         
      type Internal_Formal_Param;
      --  Represent all the information of a formal parameter. Note that
   --  formal_name can (and will) be null for formals of function types.

         
      type Internal_Logic_Context;
      --  Describes an interpretation of a reference. Can be attached to logic
   --  atoms (e.g. Binds) to indicate under which interpretation this
   --  particular atom was produced, which can in turn be used to produce
   --  informative diagnostics for resolution failures.

         
      type Internal_Param_Match;
      --  Helper data structure to implement parameter matching.

         
      type Internal_Solver_Diagnostic;
      --  A raw diagnostic produced by a solver resolution failure. This contains
   --  as much information as possible to allow formatters down the chain to
   --  filter/choose which diagnostics to show among a set of diagnostics
   --  produced for a single equation.
   --
   --  * ``Message_Template`` is a string explaining the error, which may
   --    contain holes represented by the ``{}`` characters.
   --
   --  * ``Args`` is an array of nodes, which are to be plugged in the holes of
   --    the template in the same order (i.e. the first argument goes into the
   --    first hole of the template, etc.).
   --
   --  * ``Location`` is a node which indicates the location of the error.
   --
   --  * ``Contexts`` is the array of contexts that were deemed relevant for
   --    this error.
   --
   --  * ``Round`` is the solver round during which this diagnostic was
   --    emitted.

         
      type Internal_Solver_Result;
      --  A pair returned by the ``Solve_With_Diagnostic`` primitive, consisting
   --  of:
   --
   --  * A ``Success`` field indicating whether resolution was successful or
   --    not.
   --
   --  * A ``Diagnostics`` field containing an array of diagnostics which may
   --    be non-empty if ``Success`` is ``False``.


   -------------------------------------------
   -- Array types (incomplete declarations) --
   -------------------------------------------

         
   type Bare_Lkt_Node_Array_Record;
   type Bare_Lkt_Node_Array_Access is access all Bare_Lkt_Node_Array_Record;

         
   type Integer_Array_Record;
   type Integer_Array_Access is access all Integer_Array_Record;

         
   type Internal_Entity_Array_Record;
   type Internal_Entity_Array_Access is access all Internal_Entity_Array_Record;

         
   type Internal_Entity_Enum_Class_Alt_Decl_Array_Record;
   type Internal_Entity_Enum_Class_Alt_Decl_Array_Access is access all Internal_Entity_Enum_Class_Alt_Decl_Array_Record;

         
   type Internal_Entity_Expr_Array_Record;
   type Internal_Entity_Expr_Array_Access is access all Internal_Entity_Expr_Array_Record;

         
   type Internal_Entity_Field_Decl_Array_Record;
   type Internal_Entity_Field_Decl_Array_Access is access all Internal_Entity_Field_Decl_Array_Record;

         
   type Internal_Entity_Full_Decl_Array_Record;
   type Internal_Entity_Full_Decl_Array_Access is access all Internal_Entity_Full_Decl_Array_Record;

         
   type Internal_Entity_Generic_Formal_Type_Decl_Array_Record;
   type Internal_Entity_Generic_Formal_Type_Decl_Array_Access is access all Internal_Entity_Generic_Formal_Type_Decl_Array_Record;

         
   type Internal_Entity_Param_Array_Record;
   type Internal_Entity_Param_Array_Access is access all Internal_Entity_Param_Array_Record;

         
   type Internal_Entity_Type_Decl_Array_Record;
   type Internal_Entity_Type_Decl_Array_Access is access all Internal_Entity_Type_Decl_Array_Record;

         
   type Internal_Env_Assoc_Array_Record;
   type Internal_Env_Assoc_Array_Access is access all Internal_Env_Assoc_Array_Record;

         
   type Internal_Formal_Param_Array_Record;
   type Internal_Formal_Param_Array_Access is access all Internal_Formal_Param_Array_Record;

         
   type Internal_Logic_Context_Array_Record;
   type Internal_Logic_Context_Array_Access is access all Internal_Logic_Context_Array_Record;

         
   type Internal_Param_Match_Array_Record;
   type Internal_Param_Match_Array_Access is access all Internal_Param_Match_Array_Record;

         
   type Internal_Solver_Diagnostic_Array_Record;
   type Internal_Solver_Diagnostic_Array_Access is access all Internal_Solver_Diagnostic_Array_Record;

         
   type Lexical_Env_Array_Record;
   type Lexical_Env_Array_Access is access all Lexical_Env_Array_Record;

         
   type Logic_Equation_Array_Record;
   type Logic_Equation_Array_Access is access all Logic_Equation_Array_Record;

         
   type Logic_Var_Array_Record;
   type Logic_Var_Array_Access is access all Logic_Var_Array_Record;

         
   type String_Type_Array_Record;
   type String_Type_Array_Access is access all String_Type_Array_Record;

         
   type Symbol_Type_Array_Record;
   type Symbol_Type_Array_Access is access all Symbol_Type_Array_Record;


   ----------------------------------------------
   -- Iterator types (incomplete declarations) --
   ----------------------------------------------

         
   type Internal_Bare_Lkt_Node_Iterator;
   type Bare_Lkt_Node_Iterator_Access is access all Internal_Bare_Lkt_Node_Iterator;

         
   type Internal_Internal_Entity_Iterator;
   type Internal_Entity_Iterator_Access is access all Internal_Internal_Entity_Iterator;


   ---------------------------
   -- Adalog instantiations --
   ---------------------------

   function Text_Image (Ent : Internal_Entity) return Text_Type;
   function Image (Ent : Internal_Entity) return String;
   --  Return a representation of this node as a string.

   type Internal_Logic_Context_Access is access Internal_Logic_Context;

   function Allocate_Logic_Context
     (Ctx : Internal_Logic_Context) return Internal_Logic_Context_Access;
   --  Return an access on a heap allocated copy of the given context

   function Trace_Logic_Context
     (Ctx : Internal_Logic_Context_Access) return String;
   --  Return a trace representation of the context after dereference

   function Deep_Equals
     (X, Y : Internal_Logic_Context_Access) return Boolean;
   --  Return whether the two logic contexts after dereference are equal

   procedure Free_Logic_Context
     (Ctx : in out Internal_Logic_Context_Access);
   --  Release memory allocated by ``Allocate_Logic_Context``

   package Entity_Vars is new Liblktlang_Support.Adalog.Logic_Var
     (Value_Type => Internal_Entity, Value_Image => Image);
   package Solver_Ifc is new Liblktlang_Support.Adalog.Solver_Interface
     (Entity_Vars,
      Internal_Logic_Context, Internal_Logic_Context_Access,
      Trace_Logic_Context, Deep_Equals,
      Free_Logic_Context, Internal_Solver_Diagnostic);
   package Solver is new Liblktlang_Support.Adalog.Solver (Solver_Ifc);

   subtype Logic_Var is Entity_Vars.Logic_Var;
   subtype Logic_Var_Record is Entity_Vars.Logic_Var_Record;
   Null_Var : constant Logic_Var := null;
   Null_Var_Record : constant Logic_Var_Record := (Reset => True, others => <>);

   subtype Logic_Equation is Solver.Relation;
   Null_Logic_Equation : constant Logic_Equation := Solver.No_Relation;

      function Trace_Image (K : Analysis_Unit_Kind) return String;
      function Trace_Image (B : Boolean) return String;
      function Trace_Image (I : Integer) return String;
      function Trace_Image (S : Symbol_Type) return String;
      function Trace_Image (C : Character_Type) return String;
      function Trace_Image (S : String_Type) return String;
      function Trace_Image (Env : Lexical_Env) return String;
      function Trace_Image (R : Env_Rebindings) return String;
      function Trace_Image (Unit : Internal_Unit) return String;
      function Trace_Image (Eq : Logic_Equation) return String;
      function Trace_Image (Var : Logic_Var) return String;
      function Trace_Image (T : Token_Reference) return String renames Image;
      function Trace_Image (T : Source_Location) return String renames Image;
      function Trace_Image (Self : Ref_Categories) return String;

   -----------------------------------------
   -- Structure types (full declarations) --
   -----------------------------------------

         

      

      type Internal_Decoded_Char_Value is record

               Value : aliased Character_Type;
               
               
               Has_Error : aliased Boolean;
               
               
               Error_Sloc : aliased Source_Location;
               
               
               Error_Message : aliased String_Type;
               
               
      end record
        with Convention => C;
      No_Decoded_Char_Value : constant Internal_Decoded_Char_Value;

      procedure Inc_Ref (R : Internal_Decoded_Char_Value);
      procedure Dec_Ref (R : in out Internal_Decoded_Char_Value);


      function Equivalent (L, R : Internal_Decoded_Char_Value) return Boolean;

   


      function Trace_Image (R : Internal_Decoded_Char_Value) return String;


         

      

      type Internal_Decoded_String_Value is record

               Value : aliased String_Type;
               
               
               Has_Error : aliased Boolean;
               
               
               Error_Sloc : aliased Source_Location;
               
               
               Error_Message : aliased String_Type;
               
               
      end record
        with Convention => C;
      No_Decoded_String_Value : constant Internal_Decoded_String_Value;

      procedure Inc_Ref (R : Internal_Decoded_String_Value);
      procedure Dec_Ref (R : in out Internal_Decoded_String_Value);


      function Equivalent (L, R : Internal_Decoded_String_Value) return Boolean;

   


      function Trace_Image (R : Internal_Decoded_String_Value) return String;


         

      

      type Internal_Designated_Env is record

               Kind : aliased Designated_Env_Kind;
               
               
               Env_Name : aliased Symbol_Type;
               
               
               Direct_Env : aliased Lexical_Env;
               
               
      end record
        with Convention => C;
      No_Designated_Env : constant Internal_Designated_Env;

      procedure Inc_Ref (R : Internal_Designated_Env);
      procedure Dec_Ref (R : in out Internal_Designated_Env);


      function Equivalent (L, R : Internal_Designated_Env) return Boolean;

   


      function Trace_Image (R : Internal_Designated_Env) return String;


         





   
      function Hash (R : Internal_Entity_Info) return Hash_Type;


      function Trace_Image (R : Internal_Entity_Info) return String;


         



      function Create_Internal_Entity
        (Node : Bare_Lkt_Node; Info : Internal_Entity_Info)
         return Internal_Entity;


   
      function Hash (R : Internal_Entity) return Hash_Type;


      function Trace_Image (R : Internal_Entity) return String;


         

      

      type Internal_Entity_Expr is record

               Node : aliased Bare_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Expr : constant Internal_Entity_Expr;


      function Create_Internal_Entity_Expr
        (Node : Bare_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Expr;


   


      function Trace_Image (R : Internal_Entity_Expr) return String;


         

      

      type Internal_Entity_Any_Of is record

               Node : aliased Bare_Any_Of;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Any_Of : constant Internal_Entity_Any_Of;


      function Create_Internal_Entity_Any_Of
        (Node : Bare_Any_Of; Info : Internal_Entity_Info)
         return Internal_Entity_Any_Of;


   


      function Trace_Image (R : Internal_Entity_Any_Of) return String;


         

      

      type Internal_Entity_Lkt_Node_Base_List is record

               Node : aliased Bare_Lkt_Node_Base_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lkt_Node_Base_List : constant Internal_Entity_Lkt_Node_Base_List;


      function Create_Internal_Entity_Lkt_Node_Base_List
        (Node : Bare_Lkt_Node_Base_List; Info : Internal_Entity_Info)
         return Internal_Entity_Lkt_Node_Base_List;


   


      function Trace_Image (R : Internal_Entity_Lkt_Node_Base_List) return String;


         

      

      type Internal_Entity_Expr_List is record

               Node : aliased Bare_Expr_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Expr_List : constant Internal_Entity_Expr_List;


      function Create_Internal_Entity_Expr_List
        (Node : Bare_Expr_List; Info : Internal_Entity_Info)
         return Internal_Entity_Expr_List;


   


      function Trace_Image (R : Internal_Entity_Expr_List) return String;


         

      

      type Internal_Entity_Any_Of_List is record

               Node : aliased Bare_Any_Of_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Any_Of_List : constant Internal_Entity_Any_Of_List;


      function Create_Internal_Entity_Any_Of_List
        (Node : Bare_Any_Of_List; Info : Internal_Entity_Info)
         return Internal_Entity_Any_Of_List;


   


      function Trace_Image (R : Internal_Entity_Any_Of_List) return String;


         

      

      type Internal_Entity_Decl is record

               Node : aliased Bare_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Decl : constant Internal_Entity_Decl;


      function Create_Internal_Entity_Decl
        (Node : Bare_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Decl;


   
      function Hash (R : Internal_Entity_Decl) return Hash_Type;


      function Trace_Image (R : Internal_Entity_Decl) return String;


         

      

      type Internal_Entity_Type_Decl is record

               Node : aliased Bare_Type_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Type_Decl : constant Internal_Entity_Type_Decl;


      function Create_Internal_Entity_Type_Decl
        (Node : Bare_Type_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Type_Decl;


   
      function Hash (R : Internal_Entity_Type_Decl) return Hash_Type;


      function Trace_Image (R : Internal_Entity_Type_Decl) return String;


         

      

      type Internal_Entity_Any_Type_Decl is record

               Node : aliased Bare_Any_Type_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Any_Type_Decl : constant Internal_Entity_Any_Type_Decl;


      function Create_Internal_Entity_Any_Type_Decl
        (Node : Bare_Any_Type_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Any_Type_Decl;


   


      function Trace_Image (R : Internal_Entity_Any_Type_Decl) return String;


         

      

      type Internal_Entity_Array_Literal is record

               Node : aliased Bare_Array_Literal;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Array_Literal : constant Internal_Entity_Array_Literal;


      function Create_Internal_Entity_Array_Literal
        (Node : Bare_Array_Literal; Info : Internal_Entity_Info)
         return Internal_Entity_Array_Literal;


   


      function Trace_Image (R : Internal_Entity_Array_Literal) return String;


         

      

      type Internal_Entity_Base_Call_Expr is record

               Node : aliased Bare_Base_Call_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Base_Call_Expr : constant Internal_Entity_Base_Call_Expr;


      function Create_Internal_Entity_Base_Call_Expr
        (Node : Bare_Base_Call_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Call_Expr;


   


      function Trace_Image (R : Internal_Entity_Base_Call_Expr) return String;


         

      

      type Internal_Entity_Base_Dot_Expr is record

               Node : aliased Bare_Base_Dot_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Base_Dot_Expr : constant Internal_Entity_Base_Dot_Expr;


      function Create_Internal_Entity_Base_Dot_Expr
        (Node : Bare_Base_Dot_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Dot_Expr;


   


      function Trace_Image (R : Internal_Entity_Base_Dot_Expr) return String;


         

      

      type Internal_Entity_Base_Grammar_Rule_Decl is record

               Node : aliased Bare_Base_Grammar_Rule_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Base_Grammar_Rule_Decl : constant Internal_Entity_Base_Grammar_Rule_Decl;


      function Create_Internal_Entity_Base_Grammar_Rule_Decl
        (Node : Bare_Base_Grammar_Rule_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Grammar_Rule_Decl;


   


      function Trace_Image (R : Internal_Entity_Base_Grammar_Rule_Decl) return String;


         

      

      type Internal_Entity_Base_Lexer_Case_Rule_Alt is record

               Node : aliased Bare_Base_Lexer_Case_Rule_Alt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Base_Lexer_Case_Rule_Alt : constant Internal_Entity_Base_Lexer_Case_Rule_Alt;


      function Create_Internal_Entity_Base_Lexer_Case_Rule_Alt
        (Node : Bare_Base_Lexer_Case_Rule_Alt; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Lexer_Case_Rule_Alt;


   


      function Trace_Image (R : Internal_Entity_Base_Lexer_Case_Rule_Alt) return String;


         

      

      type Internal_Entity_Base_Lexer_Case_Rule_Alt_List is record

               Node : aliased Bare_Base_Lexer_Case_Rule_Alt_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Base_Lexer_Case_Rule_Alt_List : constant Internal_Entity_Base_Lexer_Case_Rule_Alt_List;


      function Create_Internal_Entity_Base_Lexer_Case_Rule_Alt_List
        (Node : Bare_Base_Lexer_Case_Rule_Alt_List; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Lexer_Case_Rule_Alt_List;


   


      function Trace_Image (R : Internal_Entity_Base_Lexer_Case_Rule_Alt_List) return String;


         

      

      type Internal_Entity_Base_Val_Decl is record

               Node : aliased Bare_Base_Val_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Base_Val_Decl : constant Internal_Entity_Base_Val_Decl;


      function Create_Internal_Entity_Base_Val_Decl
        (Node : Bare_Base_Val_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Base_Val_Decl;


   


      function Trace_Image (R : Internal_Entity_Base_Val_Decl) return String;


         

      

      type Internal_Entity_Named_Type_Decl is record

               Node : aliased Bare_Named_Type_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Named_Type_Decl : constant Internal_Entity_Named_Type_Decl;


      function Create_Internal_Entity_Named_Type_Decl
        (Node : Bare_Named_Type_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Named_Type_Decl;


   


      function Trace_Image (R : Internal_Entity_Named_Type_Decl) return String;


         

      

      type Internal_Entity_Basic_Class_Decl is record

               Node : aliased Bare_Basic_Class_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Basic_Class_Decl : constant Internal_Entity_Basic_Class_Decl;


      function Create_Internal_Entity_Basic_Class_Decl
        (Node : Bare_Basic_Class_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Basic_Class_Decl;


   


      function Trace_Image (R : Internal_Entity_Basic_Class_Decl) return String;


         

      

      type Internal_Entity_Lit is record

               Node : aliased Bare_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lit : constant Internal_Entity_Lit;


      function Create_Internal_Entity_Lit
        (Node : Bare_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Lit;


   


      function Trace_Image (R : Internal_Entity_Lit) return String;


         

      

      type Internal_Entity_Big_Num_Lit is record

               Node : aliased Bare_Big_Num_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Big_Num_Lit : constant Internal_Entity_Big_Num_Lit;


      function Create_Internal_Entity_Big_Num_Lit
        (Node : Bare_Big_Num_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Big_Num_Lit;


   


      function Trace_Image (R : Internal_Entity_Big_Num_Lit) return String;


         

      

      type Internal_Entity_Bin_Op is record

               Node : aliased Bare_Bin_Op;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Bin_Op : constant Internal_Entity_Bin_Op;


      function Create_Internal_Entity_Bin_Op
        (Node : Bare_Bin_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Bin_Op;


   


      function Trace_Image (R : Internal_Entity_Bin_Op) return String;


         

      

      type Internal_Entity_Lkt_Node_List is record

               Node : aliased Bare_Lkt_Node_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lkt_Node_List : constant Internal_Entity_Lkt_Node_List;


      function Create_Internal_Entity_Lkt_Node_List
        (Node : Bare_Lkt_Node_List; Info : Internal_Entity_Info)
         return Internal_Entity_Lkt_Node_List;


   


      function Trace_Image (R : Internal_Entity_Lkt_Node_List) return String;


         

      

      type Internal_Entity_Block_Decl_List is record

               Node : aliased Bare_Block_Decl_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Block_Decl_List : constant Internal_Entity_Block_Decl_List;


      function Create_Internal_Entity_Block_Decl_List
        (Node : Bare_Block_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Block_Decl_List;


   


      function Trace_Image (R : Internal_Entity_Block_Decl_List) return String;


         

      

      type Internal_Entity_Block_Expr is record

               Node : aliased Bare_Block_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Block_Expr : constant Internal_Entity_Block_Expr;


      function Create_Internal_Entity_Block_Expr
        (Node : Bare_Block_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Block_Expr;


   


      function Trace_Image (R : Internal_Entity_Block_Expr) return String;


         

      

      type Internal_Entity_Block_String_Line is record

               Node : aliased Bare_Block_String_Line;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Block_String_Line : constant Internal_Entity_Block_String_Line;


      function Create_Internal_Entity_Block_String_Line
        (Node : Bare_Block_String_Line; Info : Internal_Entity_Info)
         return Internal_Entity_Block_String_Line;


   


      function Trace_Image (R : Internal_Entity_Block_String_Line) return String;


         

      

      type Internal_Entity_Block_String_Line_List is record

               Node : aliased Bare_Block_String_Line_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Block_String_Line_List : constant Internal_Entity_Block_String_Line_List;


      function Create_Internal_Entity_Block_String_Line_List
        (Node : Bare_Block_String_Line_List; Info : Internal_Entity_Info)
         return Internal_Entity_Block_String_Line_List;


   


      function Trace_Image (R : Internal_Entity_Block_String_Line_List) return String;


         

      

      type Internal_Entity_String_Lit is record

               Node : aliased Bare_String_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_String_Lit : constant Internal_Entity_String_Lit;


      function Create_Internal_Entity_String_Lit
        (Node : Bare_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_String_Lit;


   


      function Trace_Image (R : Internal_Entity_String_Lit) return String;


         

      

      type Internal_Entity_Block_String_Lit is record

               Node : aliased Bare_Block_String_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Block_String_Lit : constant Internal_Entity_Block_String_Lit;


      function Create_Internal_Entity_Block_String_Lit
        (Node : Bare_Block_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Block_String_Lit;


   


      function Trace_Image (R : Internal_Entity_Block_String_Lit) return String;


         

      

      type Internal_Entity_Call_Expr is record

               Node : aliased Bare_Call_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Call_Expr : constant Internal_Entity_Call_Expr;


      function Create_Internal_Entity_Call_Expr
        (Node : Bare_Call_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Call_Expr;


   


      function Trace_Image (R : Internal_Entity_Call_Expr) return String;


         

      

      type Internal_Entity_Call_Expr_List is record

               Node : aliased Bare_Call_Expr_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Call_Expr_List : constant Internal_Entity_Call_Expr_List;


      function Create_Internal_Entity_Call_Expr_List
        (Node : Bare_Call_Expr_List; Info : Internal_Entity_Info)
         return Internal_Entity_Call_Expr_List;


   


      function Trace_Image (R : Internal_Entity_Call_Expr_List) return String;


         

      

      type Internal_Entity_Cast_Expr is record

               Node : aliased Bare_Cast_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Cast_Expr : constant Internal_Entity_Cast_Expr;


      function Create_Internal_Entity_Cast_Expr
        (Node : Bare_Cast_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Cast_Expr;


   


      function Trace_Image (R : Internal_Entity_Cast_Expr) return String;


         

      

      type Internal_Entity_Char_Lit is record

               Node : aliased Bare_Char_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Char_Lit : constant Internal_Entity_Char_Lit;


      function Create_Internal_Entity_Char_Lit
        (Node : Bare_Char_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Char_Lit;


   


      function Trace_Image (R : Internal_Entity_Char_Lit) return String;


         

      

      type Internal_Entity_Class_Decl is record

               Node : aliased Bare_Class_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Class_Decl : constant Internal_Entity_Class_Decl;


      function Create_Internal_Entity_Class_Decl
        (Node : Bare_Class_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Class_Decl;


   


      function Trace_Image (R : Internal_Entity_Class_Decl) return String;


         

      

      type Internal_Entity_Class_Qualifier is record

               Node : aliased Bare_Class_Qualifier;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Class_Qualifier : constant Internal_Entity_Class_Qualifier;


      function Create_Internal_Entity_Class_Qualifier
        (Node : Bare_Class_Qualifier; Info : Internal_Entity_Info)
         return Internal_Entity_Class_Qualifier;


   


      function Trace_Image (R : Internal_Entity_Class_Qualifier) return String;


         

      

      type Internal_Entity_Class_Qualifier_Absent is record

               Node : aliased Bare_Class_Qualifier_Absent;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Class_Qualifier_Absent : constant Internal_Entity_Class_Qualifier_Absent;


      function Create_Internal_Entity_Class_Qualifier_Absent
        (Node : Bare_Class_Qualifier_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Class_Qualifier_Absent;


   


      function Trace_Image (R : Internal_Entity_Class_Qualifier_Absent) return String;


         

      

      type Internal_Entity_Class_Qualifier_Present is record

               Node : aliased Bare_Class_Qualifier_Present;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Class_Qualifier_Present : constant Internal_Entity_Class_Qualifier_Present;


      function Create_Internal_Entity_Class_Qualifier_Present
        (Node : Bare_Class_Qualifier_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Class_Qualifier_Present;


   


      function Trace_Image (R : Internal_Entity_Class_Qualifier_Present) return String;


         

      

      type Internal_Entity_User_Val_Decl is record

               Node : aliased Bare_User_Val_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_User_Val_Decl : constant Internal_Entity_User_Val_Decl;


      function Create_Internal_Entity_User_Val_Decl
        (Node : Bare_User_Val_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_User_Val_Decl;


   


      function Trace_Image (R : Internal_Entity_User_Val_Decl) return String;


         

      

      type Internal_Entity_Explicitly_Typed_Decl is record

               Node : aliased Bare_Explicitly_Typed_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Explicitly_Typed_Decl : constant Internal_Entity_Explicitly_Typed_Decl;


      function Create_Internal_Entity_Explicitly_Typed_Decl
        (Node : Bare_Explicitly_Typed_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Explicitly_Typed_Decl;


   


      function Trace_Image (R : Internal_Entity_Explicitly_Typed_Decl) return String;


         

      

      type Internal_Entity_Component_Decl is record

               Node : aliased Bare_Component_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Component_Decl : constant Internal_Entity_Component_Decl;


      function Create_Internal_Entity_Component_Decl
        (Node : Bare_Component_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Component_Decl;


   


      function Trace_Image (R : Internal_Entity_Component_Decl) return String;


         

      

      type Internal_Entity_Decl_Annotation is record

               Node : aliased Bare_Decl_Annotation;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Decl_Annotation : constant Internal_Entity_Decl_Annotation;


      function Create_Internal_Entity_Decl_Annotation
        (Node : Bare_Decl_Annotation; Info : Internal_Entity_Info)
         return Internal_Entity_Decl_Annotation;


   


      function Trace_Image (R : Internal_Entity_Decl_Annotation) return String;


         

      

      type Internal_Entity_Decl_Annotation_List is record

               Node : aliased Bare_Decl_Annotation_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Decl_Annotation_List : constant Internal_Entity_Decl_Annotation_List;


      function Create_Internal_Entity_Decl_Annotation_List
        (Node : Bare_Decl_Annotation_List; Info : Internal_Entity_Info)
         return Internal_Entity_Decl_Annotation_List;


   


      function Trace_Image (R : Internal_Entity_Decl_Annotation_List) return String;


         

      

      type Internal_Entity_Decl_Annotation_Params is record

               Node : aliased Bare_Decl_Annotation_Params;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Decl_Annotation_Params : constant Internal_Entity_Decl_Annotation_Params;


      function Create_Internal_Entity_Decl_Annotation_Params
        (Node : Bare_Decl_Annotation_Params; Info : Internal_Entity_Info)
         return Internal_Entity_Decl_Annotation_Params;


   


      function Trace_Image (R : Internal_Entity_Decl_Annotation_Params) return String;


         

      

      type Internal_Entity_Full_Decl_List is record

               Node : aliased Bare_Full_Decl_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Full_Decl_List : constant Internal_Entity_Full_Decl_List;


      function Create_Internal_Entity_Full_Decl_List
        (Node : Bare_Full_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Full_Decl_List;


   


      function Trace_Image (R : Internal_Entity_Full_Decl_List) return String;


         

      

      type Internal_Entity_Decl_Block is record

               Node : aliased Bare_Decl_Block;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Decl_Block : constant Internal_Entity_Decl_Block;


      function Create_Internal_Entity_Decl_Block
        (Node : Bare_Decl_Block; Info : Internal_Entity_Info)
         return Internal_Entity_Decl_Block;


   


      function Trace_Image (R : Internal_Entity_Decl_Block) return String;


         

      

      type Internal_Entity_Id is record

               Node : aliased Bare_Id;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Id : constant Internal_Entity_Id;


      function Create_Internal_Entity_Id
        (Node : Bare_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Id;


   


      function Trace_Image (R : Internal_Entity_Id) return String;


         

      

      type Internal_Entity_Def_Id is record

               Node : aliased Bare_Def_Id;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Def_Id : constant Internal_Entity_Def_Id;


      function Create_Internal_Entity_Def_Id
        (Node : Bare_Def_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Def_Id;


   


      function Trace_Image (R : Internal_Entity_Def_Id) return String;


         

      

      type Internal_Entity_Type_Ref is record

               Node : aliased Bare_Type_Ref;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Type_Ref : constant Internal_Entity_Type_Ref;


      function Create_Internal_Entity_Type_Ref
        (Node : Bare_Type_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Type_Ref;


   


      function Trace_Image (R : Internal_Entity_Type_Ref) return String;


         

      

      type Internal_Entity_Default_List_Type_Ref is record

               Node : aliased Bare_Default_List_Type_Ref;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Default_List_Type_Ref : constant Internal_Entity_Default_List_Type_Ref;


      function Create_Internal_Entity_Default_List_Type_Ref
        (Node : Bare_Default_List_Type_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Default_List_Type_Ref;


   


      function Trace_Image (R : Internal_Entity_Default_List_Type_Ref) return String;


         

      

      type Internal_Entity_Dot_Expr is record

               Node : aliased Bare_Dot_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Dot_Expr : constant Internal_Entity_Dot_Expr;


      function Create_Internal_Entity_Dot_Expr
        (Node : Bare_Dot_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Dot_Expr;


   


      function Trace_Image (R : Internal_Entity_Dot_Expr) return String;


         

      

      type Internal_Entity_Dyn_Env_Wrapper is record

               Node : aliased Bare_Dyn_Env_Wrapper;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Dyn_Env_Wrapper : constant Internal_Entity_Dyn_Env_Wrapper;


      function Create_Internal_Entity_Dyn_Env_Wrapper
        (Node : Bare_Dyn_Env_Wrapper; Info : Internal_Entity_Info)
         return Internal_Entity_Dyn_Env_Wrapper;


   


      function Trace_Image (R : Internal_Entity_Dyn_Env_Wrapper) return String;


         

      

      type Internal_Entity_Dyn_Var_Decl is record

               Node : aliased Bare_Dyn_Var_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Dyn_Var_Decl : constant Internal_Entity_Dyn_Var_Decl;


      function Create_Internal_Entity_Dyn_Var_Decl
        (Node : Bare_Dyn_Var_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Dyn_Var_Decl;


   


      function Trace_Image (R : Internal_Entity_Dyn_Var_Decl) return String;


         

      

      type Internal_Entity_Elsif_Branch is record

               Node : aliased Bare_Elsif_Branch;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Elsif_Branch : constant Internal_Entity_Elsif_Branch;


      function Create_Internal_Entity_Elsif_Branch
        (Node : Bare_Elsif_Branch; Info : Internal_Entity_Info)
         return Internal_Entity_Elsif_Branch;


   


      function Trace_Image (R : Internal_Entity_Elsif_Branch) return String;


         

      

      type Internal_Entity_Elsif_Branch_List is record

               Node : aliased Bare_Elsif_Branch_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Elsif_Branch_List : constant Internal_Entity_Elsif_Branch_List;


      function Create_Internal_Entity_Elsif_Branch_List
        (Node : Bare_Elsif_Branch_List; Info : Internal_Entity_Info)
         return Internal_Entity_Elsif_Branch_List;


   


      function Trace_Image (R : Internal_Entity_Elsif_Branch_List) return String;


         

      

      type Internal_Entity_Enum_Class_Alt_Decl is record

               Node : aliased Bare_Enum_Class_Alt_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Enum_Class_Alt_Decl : constant Internal_Entity_Enum_Class_Alt_Decl;


      function Create_Internal_Entity_Enum_Class_Alt_Decl
        (Node : Bare_Enum_Class_Alt_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Class_Alt_Decl;


   


      function Trace_Image (R : Internal_Entity_Enum_Class_Alt_Decl) return String;


         

      

      type Internal_Entity_Enum_Class_Alt_Decl_List is record

               Node : aliased Bare_Enum_Class_Alt_Decl_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Enum_Class_Alt_Decl_List : constant Internal_Entity_Enum_Class_Alt_Decl_List;


      function Create_Internal_Entity_Enum_Class_Alt_Decl_List
        (Node : Bare_Enum_Class_Alt_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Class_Alt_Decl_List;


   


      function Trace_Image (R : Internal_Entity_Enum_Class_Alt_Decl_List) return String;


         

      

      type Internal_Entity_Enum_Class_Case is record

               Node : aliased Bare_Enum_Class_Case;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Enum_Class_Case : constant Internal_Entity_Enum_Class_Case;


      function Create_Internal_Entity_Enum_Class_Case
        (Node : Bare_Enum_Class_Case; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Class_Case;


   


      function Trace_Image (R : Internal_Entity_Enum_Class_Case) return String;


         

      

      type Internal_Entity_Enum_Class_Case_List is record

               Node : aliased Bare_Enum_Class_Case_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Enum_Class_Case_List : constant Internal_Entity_Enum_Class_Case_List;


      function Create_Internal_Entity_Enum_Class_Case_List
        (Node : Bare_Enum_Class_Case_List; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Class_Case_List;


   


      function Trace_Image (R : Internal_Entity_Enum_Class_Case_List) return String;


         

      

      type Internal_Entity_Enum_Class_Decl is record

               Node : aliased Bare_Enum_Class_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Enum_Class_Decl : constant Internal_Entity_Enum_Class_Decl;


      function Create_Internal_Entity_Enum_Class_Decl
        (Node : Bare_Enum_Class_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Class_Decl;


   


      function Trace_Image (R : Internal_Entity_Enum_Class_Decl) return String;


         

      

      type Internal_Entity_Enum_Lit_Decl is record

               Node : aliased Bare_Enum_Lit_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Enum_Lit_Decl : constant Internal_Entity_Enum_Lit_Decl;


      function Create_Internal_Entity_Enum_Lit_Decl
        (Node : Bare_Enum_Lit_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Lit_Decl;


   


      function Trace_Image (R : Internal_Entity_Enum_Lit_Decl) return String;


         

      

      type Internal_Entity_Enum_Lit_Decl_List is record

               Node : aliased Bare_Enum_Lit_Decl_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Enum_Lit_Decl_List : constant Internal_Entity_Enum_Lit_Decl_List;


      function Create_Internal_Entity_Enum_Lit_Decl_List
        (Node : Bare_Enum_Lit_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Lit_Decl_List;


   


      function Trace_Image (R : Internal_Entity_Enum_Lit_Decl_List) return String;


         

      

      type Internal_Entity_Enum_Type_Decl is record

               Node : aliased Bare_Enum_Type_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Enum_Type_Decl : constant Internal_Entity_Enum_Type_Decl;


      function Create_Internal_Entity_Enum_Type_Decl
        (Node : Bare_Enum_Type_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Enum_Type_Decl;


   


      function Trace_Image (R : Internal_Entity_Enum_Type_Decl) return String;


         

      

      type Internal_Entity_Env_Spec_Decl is record

               Node : aliased Bare_Env_Spec_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Env_Spec_Decl : constant Internal_Entity_Env_Spec_Decl;


      function Create_Internal_Entity_Env_Spec_Decl
        (Node : Bare_Env_Spec_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Env_Spec_Decl;


   


      function Trace_Image (R : Internal_Entity_Env_Spec_Decl) return String;


         

      

      type Internal_Entity_Error_On_Null is record

               Node : aliased Bare_Error_On_Null;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Error_On_Null : constant Internal_Entity_Error_On_Null;


      function Create_Internal_Entity_Error_On_Null
        (Node : Bare_Error_On_Null; Info : Internal_Entity_Info)
         return Internal_Entity_Error_On_Null;


   


      function Trace_Image (R : Internal_Entity_Error_On_Null) return String;


         

      

      type Internal_Entity_Excludes_Null is record

               Node : aliased Bare_Excludes_Null;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Excludes_Null : constant Internal_Entity_Excludes_Null;


      function Create_Internal_Entity_Excludes_Null
        (Node : Bare_Excludes_Null; Info : Internal_Entity_Info)
         return Internal_Entity_Excludes_Null;


   


      function Trace_Image (R : Internal_Entity_Excludes_Null) return String;


         

      

      type Internal_Entity_Excludes_Null_Absent is record

               Node : aliased Bare_Excludes_Null_Absent;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Excludes_Null_Absent : constant Internal_Entity_Excludes_Null_Absent;


      function Create_Internal_Entity_Excludes_Null_Absent
        (Node : Bare_Excludes_Null_Absent; Info : Internal_Entity_Info)
         return Internal_Entity_Excludes_Null_Absent;


   


      function Trace_Image (R : Internal_Entity_Excludes_Null_Absent) return String;


         

      

      type Internal_Entity_Excludes_Null_Present is record

               Node : aliased Bare_Excludes_Null_Present;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Excludes_Null_Present : constant Internal_Entity_Excludes_Null_Present;


      function Create_Internal_Entity_Excludes_Null_Present
        (Node : Bare_Excludes_Null_Present; Info : Internal_Entity_Info)
         return Internal_Entity_Excludes_Null_Present;


   


      function Trace_Image (R : Internal_Entity_Excludes_Null_Present) return String;


         

      

      type Internal_Entity_Field_Decl is record

               Node : aliased Bare_Field_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Field_Decl : constant Internal_Entity_Field_Decl;


      function Create_Internal_Entity_Field_Decl
        (Node : Bare_Field_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Field_Decl;


   


      function Trace_Image (R : Internal_Entity_Field_Decl) return String;


         

      

      type Internal_Entity_Full_Decl is record

               Node : aliased Bare_Full_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Full_Decl : constant Internal_Entity_Full_Decl;


      function Create_Internal_Entity_Full_Decl
        (Node : Bare_Full_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Full_Decl;


   


      function Trace_Image (R : Internal_Entity_Full_Decl) return String;


         

      

      type Internal_Entity_Fun_Arg_Decl is record

               Node : aliased Bare_Fun_Arg_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Fun_Arg_Decl : constant Internal_Entity_Fun_Arg_Decl;


      function Create_Internal_Entity_Fun_Arg_Decl
        (Node : Bare_Fun_Arg_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Fun_Arg_Decl;


   


      function Trace_Image (R : Internal_Entity_Fun_Arg_Decl) return String;


         

      

      type Internal_Entity_Fun_Arg_Decl_List is record

               Node : aliased Bare_Fun_Arg_Decl_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Fun_Arg_Decl_List : constant Internal_Entity_Fun_Arg_Decl_List;


      function Create_Internal_Entity_Fun_Arg_Decl_List
        (Node : Bare_Fun_Arg_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Fun_Arg_Decl_List;


   


      function Trace_Image (R : Internal_Entity_Fun_Arg_Decl_List) return String;


         

      

      type Internal_Entity_Fun_Decl is record

               Node : aliased Bare_Fun_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Fun_Decl : constant Internal_Entity_Fun_Decl;


      function Create_Internal_Entity_Fun_Decl
        (Node : Bare_Fun_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Fun_Decl;


   


      function Trace_Image (R : Internal_Entity_Fun_Decl) return String;


         

      

      type Internal_Entity_Function_Type is record

               Node : aliased Bare_Function_Type;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Function_Type : constant Internal_Entity_Function_Type;


      function Create_Internal_Entity_Function_Type
        (Node : Bare_Function_Type; Info : Internal_Entity_Info)
         return Internal_Entity_Function_Type;


   


      function Trace_Image (R : Internal_Entity_Function_Type) return String;


         

      

      type Internal_Entity_Function_Type_Ref is record

               Node : aliased Bare_Function_Type_Ref;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Function_Type_Ref : constant Internal_Entity_Function_Type_Ref;


      function Create_Internal_Entity_Function_Type_Ref
        (Node : Bare_Function_Type_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Function_Type_Ref;


   


      function Trace_Image (R : Internal_Entity_Function_Type_Ref) return String;


         

      

      type Internal_Entity_Generic_Decl is record

               Node : aliased Bare_Generic_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Generic_Decl : constant Internal_Entity_Generic_Decl;


      function Create_Internal_Entity_Generic_Decl
        (Node : Bare_Generic_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Generic_Decl;


   


      function Trace_Image (R : Internal_Entity_Generic_Decl) return String;


         

      

      type Internal_Entity_Generic_Formal_Decl_List is record

               Node : aliased Bare_Generic_Formal_Decl_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Generic_Formal_Decl_List : constant Internal_Entity_Generic_Formal_Decl_List;


      function Create_Internal_Entity_Generic_Formal_Decl_List
        (Node : Bare_Generic_Formal_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Generic_Formal_Decl_List;


   


      function Trace_Image (R : Internal_Entity_Generic_Formal_Decl_List) return String;


         

      

      type Internal_Entity_Generic_Formal_Type_Decl is record

               Node : aliased Bare_Generic_Formal_Type_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Generic_Formal_Type_Decl : constant Internal_Entity_Generic_Formal_Type_Decl;


      function Create_Internal_Entity_Generic_Formal_Type_Decl
        (Node : Bare_Generic_Formal_Type_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Generic_Formal_Type_Decl;


   


      function Trace_Image (R : Internal_Entity_Generic_Formal_Type_Decl) return String;


         

      

      type Internal_Entity_Generic_Instantiation is record

               Node : aliased Bare_Generic_Instantiation;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Generic_Instantiation : constant Internal_Entity_Generic_Instantiation;


      function Create_Internal_Entity_Generic_Instantiation
        (Node : Bare_Generic_Instantiation; Info : Internal_Entity_Info)
         return Internal_Entity_Generic_Instantiation;


   


      function Trace_Image (R : Internal_Entity_Generic_Instantiation) return String;


         

      

      type Internal_Entity_Generic_Type_Ref is record

               Node : aliased Bare_Generic_Type_Ref;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Generic_Type_Ref : constant Internal_Entity_Generic_Type_Ref;


      function Create_Internal_Entity_Generic_Type_Ref
        (Node : Bare_Generic_Type_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Generic_Type_Ref;


   


      function Trace_Image (R : Internal_Entity_Generic_Type_Ref) return String;


         

      

      type Internal_Entity_Grammar_Expr is record

               Node : aliased Bare_Grammar_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Expr : constant Internal_Entity_Grammar_Expr;


      function Create_Internal_Entity_Grammar_Expr
        (Node : Bare_Grammar_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Expr;


   


      function Trace_Image (R : Internal_Entity_Grammar_Expr) return String;


         

      

      type Internal_Entity_Grammar_Cut is record

               Node : aliased Bare_Grammar_Cut;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Cut : constant Internal_Entity_Grammar_Cut;


      function Create_Internal_Entity_Grammar_Cut
        (Node : Bare_Grammar_Cut; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Cut;


   


      function Trace_Image (R : Internal_Entity_Grammar_Cut) return String;


         

      

      type Internal_Entity_Grammar_Decl is record

               Node : aliased Bare_Grammar_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Decl : constant Internal_Entity_Grammar_Decl;


      function Create_Internal_Entity_Grammar_Decl
        (Node : Bare_Grammar_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Decl;


   


      function Trace_Image (R : Internal_Entity_Grammar_Decl) return String;


         

      

      type Internal_Entity_Grammar_Discard is record

               Node : aliased Bare_Grammar_Discard;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Discard : constant Internal_Entity_Grammar_Discard;


      function Create_Internal_Entity_Grammar_Discard
        (Node : Bare_Grammar_Discard; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Discard;


   


      function Trace_Image (R : Internal_Entity_Grammar_Discard) return String;


         

      

      type Internal_Entity_Grammar_Dont_Skip is record

               Node : aliased Bare_Grammar_Dont_Skip;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Dont_Skip : constant Internal_Entity_Grammar_Dont_Skip;


      function Create_Internal_Entity_Grammar_Dont_Skip
        (Node : Bare_Grammar_Dont_Skip; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Dont_Skip;


   


      function Trace_Image (R : Internal_Entity_Grammar_Dont_Skip) return String;


         

      

      type Internal_Entity_Grammar_Expr_List is record

               Node : aliased Bare_Grammar_Expr_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Expr_List : constant Internal_Entity_Grammar_Expr_List;


      function Create_Internal_Entity_Grammar_Expr_List
        (Node : Bare_Grammar_Expr_List; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Expr_List;


   


      function Trace_Image (R : Internal_Entity_Grammar_Expr_List) return String;


         

      

      type Internal_Entity_Grammar_Expr_List_List is record

               Node : aliased Bare_Grammar_Expr_List_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Expr_List_List : constant Internal_Entity_Grammar_Expr_List_List;


      function Create_Internal_Entity_Grammar_Expr_List_List
        (Node : Bare_Grammar_Expr_List_List; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Expr_List_List;


   


      function Trace_Image (R : Internal_Entity_Grammar_Expr_List_List) return String;


         

      

      type Internal_Entity_Grammar_Pick is record

               Node : aliased Bare_Grammar_Pick;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Pick : constant Internal_Entity_Grammar_Pick;


      function Create_Internal_Entity_Grammar_Pick
        (Node : Bare_Grammar_Pick; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Pick;


   


      function Trace_Image (R : Internal_Entity_Grammar_Pick) return String;


         

      

      type Internal_Entity_Grammar_Implicit_Pick is record

               Node : aliased Bare_Grammar_Implicit_Pick;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Implicit_Pick : constant Internal_Entity_Grammar_Implicit_Pick;


      function Create_Internal_Entity_Grammar_Implicit_Pick
        (Node : Bare_Grammar_Implicit_Pick; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Implicit_Pick;


   


      function Trace_Image (R : Internal_Entity_Grammar_Implicit_Pick) return String;


         

      

      type Internal_Entity_Grammar_List is record

               Node : aliased Bare_Grammar_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_List : constant Internal_Entity_Grammar_List;


      function Create_Internal_Entity_Grammar_List
        (Node : Bare_Grammar_List; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_List;


   


      function Trace_Image (R : Internal_Entity_Grammar_List) return String;


         

      

      type Internal_Entity_Grammar_List_Sep is record

               Node : aliased Bare_Grammar_List_Sep;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_List_Sep : constant Internal_Entity_Grammar_List_Sep;


      function Create_Internal_Entity_Grammar_List_Sep
        (Node : Bare_Grammar_List_Sep; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_List_Sep;


   


      function Trace_Image (R : Internal_Entity_Grammar_List_Sep) return String;


         

      

      type Internal_Entity_Grammar_Null is record

               Node : aliased Bare_Grammar_Null;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Null : constant Internal_Entity_Grammar_Null;


      function Create_Internal_Entity_Grammar_Null
        (Node : Bare_Grammar_Null; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Null;


   


      function Trace_Image (R : Internal_Entity_Grammar_Null) return String;


         

      

      type Internal_Entity_Grammar_Opt is record

               Node : aliased Bare_Grammar_Opt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Opt : constant Internal_Entity_Grammar_Opt;


      function Create_Internal_Entity_Grammar_Opt
        (Node : Bare_Grammar_Opt; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Opt;


   


      function Trace_Image (R : Internal_Entity_Grammar_Opt) return String;


         

      

      type Internal_Entity_Grammar_Opt_Error is record

               Node : aliased Bare_Grammar_Opt_Error;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Opt_Error : constant Internal_Entity_Grammar_Opt_Error;


      function Create_Internal_Entity_Grammar_Opt_Error
        (Node : Bare_Grammar_Opt_Error; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Opt_Error;


   


      function Trace_Image (R : Internal_Entity_Grammar_Opt_Error) return String;


         

      

      type Internal_Entity_Grammar_Opt_Error_Group is record

               Node : aliased Bare_Grammar_Opt_Error_Group;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Opt_Error_Group : constant Internal_Entity_Grammar_Opt_Error_Group;


      function Create_Internal_Entity_Grammar_Opt_Error_Group
        (Node : Bare_Grammar_Opt_Error_Group; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Opt_Error_Group;


   


      function Trace_Image (R : Internal_Entity_Grammar_Opt_Error_Group) return String;


         

      

      type Internal_Entity_Grammar_Opt_Group is record

               Node : aliased Bare_Grammar_Opt_Group;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Opt_Group : constant Internal_Entity_Grammar_Opt_Group;


      function Create_Internal_Entity_Grammar_Opt_Group
        (Node : Bare_Grammar_Opt_Group; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Opt_Group;


   


      function Trace_Image (R : Internal_Entity_Grammar_Opt_Group) return String;


         

      

      type Internal_Entity_Grammar_Or_Expr is record

               Node : aliased Bare_Grammar_Or_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Or_Expr : constant Internal_Entity_Grammar_Or_Expr;


      function Create_Internal_Entity_Grammar_Or_Expr
        (Node : Bare_Grammar_Or_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Or_Expr;


   


      function Trace_Image (R : Internal_Entity_Grammar_Or_Expr) return String;


         

      

      type Internal_Entity_Grammar_Predicate is record

               Node : aliased Bare_Grammar_Predicate;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Predicate : constant Internal_Entity_Grammar_Predicate;


      function Create_Internal_Entity_Grammar_Predicate
        (Node : Bare_Grammar_Predicate; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Predicate;


   


      function Trace_Image (R : Internal_Entity_Grammar_Predicate) return String;


         

      

      type Internal_Entity_Grammar_Rule_Decl is record

               Node : aliased Bare_Grammar_Rule_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Rule_Decl : constant Internal_Entity_Grammar_Rule_Decl;


      function Create_Internal_Entity_Grammar_Rule_Decl
        (Node : Bare_Grammar_Rule_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Rule_Decl;


   


      function Trace_Image (R : Internal_Entity_Grammar_Rule_Decl) return String;


         

      

      type Internal_Entity_Grammar_Rule_Ref is record

               Node : aliased Bare_Grammar_Rule_Ref;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Rule_Ref : constant Internal_Entity_Grammar_Rule_Ref;


      function Create_Internal_Entity_Grammar_Rule_Ref
        (Node : Bare_Grammar_Rule_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Rule_Ref;


   


      function Trace_Image (R : Internal_Entity_Grammar_Rule_Ref) return String;


         

      

      type Internal_Entity_Grammar_Skip is record

               Node : aliased Bare_Grammar_Skip;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Skip : constant Internal_Entity_Grammar_Skip;


      function Create_Internal_Entity_Grammar_Skip
        (Node : Bare_Grammar_Skip; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Skip;


   


      function Trace_Image (R : Internal_Entity_Grammar_Skip) return String;


         

      

      type Internal_Entity_Grammar_Stop_Cut is record

               Node : aliased Bare_Grammar_Stop_Cut;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Grammar_Stop_Cut : constant Internal_Entity_Grammar_Stop_Cut;


      function Create_Internal_Entity_Grammar_Stop_Cut
        (Node : Bare_Grammar_Stop_Cut; Info : Internal_Entity_Info)
         return Internal_Entity_Grammar_Stop_Cut;


   


      function Trace_Image (R : Internal_Entity_Grammar_Stop_Cut) return String;


         

      

      type Internal_Entity_If_Expr is record

               Node : aliased Bare_If_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_If_Expr : constant Internal_Entity_If_Expr;


      function Create_Internal_Entity_If_Expr
        (Node : Bare_If_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_If_Expr;


   


      function Trace_Image (R : Internal_Entity_If_Expr) return String;


         

      

      type Internal_Entity_Import is record

               Node : aliased Bare_Import;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Import : constant Internal_Entity_Import;


      function Create_Internal_Entity_Import
        (Node : Bare_Import; Info : Internal_Entity_Info)
         return Internal_Entity_Import;


   


      function Trace_Image (R : Internal_Entity_Import) return String;


         

      

      type Internal_Entity_Import_List is record

               Node : aliased Bare_Import_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Import_List : constant Internal_Entity_Import_List;


      function Create_Internal_Entity_Import_List
        (Node : Bare_Import_List; Info : Internal_Entity_Info)
         return Internal_Entity_Import_List;


   


      function Trace_Image (R : Internal_Entity_Import_List) return String;


         

      

      type Internal_Entity_Isa is record

               Node : aliased Bare_Isa;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Isa : constant Internal_Entity_Isa;


      function Create_Internal_Entity_Isa
        (Node : Bare_Isa; Info : Internal_Entity_Info)
         return Internal_Entity_Isa;


   


      function Trace_Image (R : Internal_Entity_Isa) return String;


         

      

      type Internal_Entity_Type_Ref_List is record

               Node : aliased Bare_Type_Ref_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Type_Ref_List : constant Internal_Entity_Type_Ref_List;


      function Create_Internal_Entity_Type_Ref_List
        (Node : Bare_Type_Ref_List; Info : Internal_Entity_Info)
         return Internal_Entity_Type_Ref_List;


   


      function Trace_Image (R : Internal_Entity_Type_Ref_List) return String;


         

      

      type Internal_Entity_Isa_List is record

               Node : aliased Bare_Isa_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Isa_List : constant Internal_Entity_Isa_List;


      function Create_Internal_Entity_Isa_List
        (Node : Bare_Isa_List; Info : Internal_Entity_Info)
         return Internal_Entity_Isa_List;


   


      function Trace_Image (R : Internal_Entity_Isa_List) return String;


         

      

      type Internal_Entity_Keep_Expr is record

               Node : aliased Bare_Keep_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Keep_Expr : constant Internal_Entity_Keep_Expr;


      function Create_Internal_Entity_Keep_Expr
        (Node : Bare_Keep_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Keep_Expr;


   


      function Trace_Image (R : Internal_Entity_Keep_Expr) return String;


         

      

      type Internal_Entity_Lambda_Arg_Decl is record

               Node : aliased Bare_Lambda_Arg_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lambda_Arg_Decl : constant Internal_Entity_Lambda_Arg_Decl;


      function Create_Internal_Entity_Lambda_Arg_Decl
        (Node : Bare_Lambda_Arg_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Lambda_Arg_Decl;


   


      function Trace_Image (R : Internal_Entity_Lambda_Arg_Decl) return String;


         

      

      type Internal_Entity_Lambda_Arg_Decl_List is record

               Node : aliased Bare_Lambda_Arg_Decl_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lambda_Arg_Decl_List : constant Internal_Entity_Lambda_Arg_Decl_List;


      function Create_Internal_Entity_Lambda_Arg_Decl_List
        (Node : Bare_Lambda_Arg_Decl_List; Info : Internal_Entity_Info)
         return Internal_Entity_Lambda_Arg_Decl_List;


   


      function Trace_Image (R : Internal_Entity_Lambda_Arg_Decl_List) return String;


         

      

      type Internal_Entity_Lambda_Expr is record

               Node : aliased Bare_Lambda_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lambda_Expr : constant Internal_Entity_Lambda_Expr;


      function Create_Internal_Entity_Lambda_Expr
        (Node : Bare_Lambda_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Lambda_Expr;


   


      function Trace_Image (R : Internal_Entity_Lambda_Expr) return String;


         

      

      type Internal_Entity_Langkit_Root is record

               Node : aliased Bare_Langkit_Root;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Langkit_Root : constant Internal_Entity_Langkit_Root;


      function Create_Internal_Entity_Langkit_Root
        (Node : Bare_Langkit_Root; Info : Internal_Entity_Info)
         return Internal_Entity_Langkit_Root;


   


      function Trace_Image (R : Internal_Entity_Langkit_Root) return String;


         

      

      type Internal_Entity_Lexer_Case_Rule is record

               Node : aliased Bare_Lexer_Case_Rule;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lexer_Case_Rule : constant Internal_Entity_Lexer_Case_Rule;


      function Create_Internal_Entity_Lexer_Case_Rule
        (Node : Bare_Lexer_Case_Rule; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Case_Rule;


   


      function Trace_Image (R : Internal_Entity_Lexer_Case_Rule) return String;


         

      

      type Internal_Entity_Lexer_Case_Rule_Cond_Alt is record

               Node : aliased Bare_Lexer_Case_Rule_Cond_Alt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lexer_Case_Rule_Cond_Alt : constant Internal_Entity_Lexer_Case_Rule_Cond_Alt;


      function Create_Internal_Entity_Lexer_Case_Rule_Cond_Alt
        (Node : Bare_Lexer_Case_Rule_Cond_Alt; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Case_Rule_Cond_Alt;


   


      function Trace_Image (R : Internal_Entity_Lexer_Case_Rule_Cond_Alt) return String;


         

      

      type Internal_Entity_Lexer_Case_Rule_Default_Alt is record

               Node : aliased Bare_Lexer_Case_Rule_Default_Alt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lexer_Case_Rule_Default_Alt : constant Internal_Entity_Lexer_Case_Rule_Default_Alt;


      function Create_Internal_Entity_Lexer_Case_Rule_Default_Alt
        (Node : Bare_Lexer_Case_Rule_Default_Alt; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Case_Rule_Default_Alt;


   


      function Trace_Image (R : Internal_Entity_Lexer_Case_Rule_Default_Alt) return String;


         

      

      type Internal_Entity_Lexer_Case_Rule_Send is record

               Node : aliased Bare_Lexer_Case_Rule_Send;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lexer_Case_Rule_Send : constant Internal_Entity_Lexer_Case_Rule_Send;


      function Create_Internal_Entity_Lexer_Case_Rule_Send
        (Node : Bare_Lexer_Case_Rule_Send; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Case_Rule_Send;


   


      function Trace_Image (R : Internal_Entity_Lexer_Case_Rule_Send) return String;


         

      

      type Internal_Entity_Lexer_Decl is record

               Node : aliased Bare_Lexer_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lexer_Decl : constant Internal_Entity_Lexer_Decl;


      function Create_Internal_Entity_Lexer_Decl
        (Node : Bare_Lexer_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Decl;


   


      function Trace_Image (R : Internal_Entity_Lexer_Decl) return String;


         

      

      type Internal_Entity_Lexer_Family_Decl is record

               Node : aliased Bare_Lexer_Family_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Lexer_Family_Decl : constant Internal_Entity_Lexer_Family_Decl;


      function Create_Internal_Entity_Lexer_Family_Decl
        (Node : Bare_Lexer_Family_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Lexer_Family_Decl;


   


      function Trace_Image (R : Internal_Entity_Lexer_Family_Decl) return String;


         

      

      type Internal_Entity_List_Kind is record

               Node : aliased Bare_List_Kind;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_List_Kind : constant Internal_Entity_List_Kind;


      function Create_Internal_Entity_List_Kind
        (Node : Bare_List_Kind; Info : Internal_Entity_Info)
         return Internal_Entity_List_Kind;


   


      function Trace_Image (R : Internal_Entity_List_Kind) return String;


         

      

      type Internal_Entity_List_Kind_One is record

               Node : aliased Bare_List_Kind_One;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_List_Kind_One : constant Internal_Entity_List_Kind_One;


      function Create_Internal_Entity_List_Kind_One
        (Node : Bare_List_Kind_One; Info : Internal_Entity_Info)
         return Internal_Entity_List_Kind_One;


   


      function Trace_Image (R : Internal_Entity_List_Kind_One) return String;


         

      

      type Internal_Entity_List_Kind_Zero is record

               Node : aliased Bare_List_Kind_Zero;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_List_Kind_Zero : constant Internal_Entity_List_Kind_Zero;


      function Create_Internal_Entity_List_Kind_Zero
        (Node : Bare_List_Kind_Zero; Info : Internal_Entity_Info)
         return Internal_Entity_List_Kind_Zero;


   


      function Trace_Image (R : Internal_Entity_List_Kind_Zero) return String;


         

      

      type Internal_Entity_Logic_Assign is record

               Node : aliased Bare_Logic_Assign;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Logic_Assign : constant Internal_Entity_Logic_Assign;


      function Create_Internal_Entity_Logic_Assign
        (Node : Bare_Logic_Assign; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Assign;


   


      function Trace_Image (R : Internal_Entity_Logic_Assign) return String;


         

      

      type Internal_Entity_Logic_Call_Expr is record

               Node : aliased Bare_Logic_Call_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Logic_Call_Expr : constant Internal_Entity_Logic_Call_Expr;


      function Create_Internal_Entity_Logic_Call_Expr
        (Node : Bare_Logic_Call_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Call_Expr;


   


      function Trace_Image (R : Internal_Entity_Logic_Call_Expr) return String;


         

      

      type Internal_Entity_Logic_Expr is record

               Node : aliased Bare_Logic_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Logic_Expr : constant Internal_Entity_Logic_Expr;


      function Create_Internal_Entity_Logic_Expr
        (Node : Bare_Logic_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Expr;


   


      function Trace_Image (R : Internal_Entity_Logic_Expr) return String;


         

      

      type Internal_Entity_Logic_Predicate is record

               Node : aliased Bare_Logic_Predicate;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Logic_Predicate : constant Internal_Entity_Logic_Predicate;


      function Create_Internal_Entity_Logic_Predicate
        (Node : Bare_Logic_Predicate; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Predicate;


   


      function Trace_Image (R : Internal_Entity_Logic_Predicate) return String;


         

      

      type Internal_Entity_Logic_Propagate is record

               Node : aliased Bare_Logic_Propagate;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Logic_Propagate : constant Internal_Entity_Logic_Propagate;


      function Create_Internal_Entity_Logic_Propagate
        (Node : Bare_Logic_Propagate; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Propagate;


   


      function Trace_Image (R : Internal_Entity_Logic_Propagate) return String;


         

      

      type Internal_Entity_Logic_Propagate_Call is record

               Node : aliased Bare_Logic_Propagate_Call;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Logic_Propagate_Call : constant Internal_Entity_Logic_Propagate_Call;


      function Create_Internal_Entity_Logic_Propagate_Call
        (Node : Bare_Logic_Propagate_Call; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Propagate_Call;


   


      function Trace_Image (R : Internal_Entity_Logic_Propagate_Call) return String;


         

      

      type Internal_Entity_Logic_Unify is record

               Node : aliased Bare_Logic_Unify;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Logic_Unify : constant Internal_Entity_Logic_Unify;


      function Create_Internal_Entity_Logic_Unify
        (Node : Bare_Logic_Unify; Info : Internal_Entity_Info)
         return Internal_Entity_Logic_Unify;


   


      function Trace_Image (R : Internal_Entity_Logic_Unify) return String;


         

      

      type Internal_Entity_Match_Branch is record

               Node : aliased Bare_Match_Branch;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Match_Branch : constant Internal_Entity_Match_Branch;


      function Create_Internal_Entity_Match_Branch
        (Node : Bare_Match_Branch; Info : Internal_Entity_Info)
         return Internal_Entity_Match_Branch;


   


      function Trace_Image (R : Internal_Entity_Match_Branch) return String;


         

      

      type Internal_Entity_Match_Branch_List is record

               Node : aliased Bare_Match_Branch_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Match_Branch_List : constant Internal_Entity_Match_Branch_List;


      function Create_Internal_Entity_Match_Branch_List
        (Node : Bare_Match_Branch_List; Info : Internal_Entity_Info)
         return Internal_Entity_Match_Branch_List;


   


      function Trace_Image (R : Internal_Entity_Match_Branch_List) return String;


         

      

      type Internal_Entity_Match_Expr is record

               Node : aliased Bare_Match_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Match_Expr : constant Internal_Entity_Match_Expr;


      function Create_Internal_Entity_Match_Expr
        (Node : Bare_Match_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Match_Expr;


   


      function Trace_Image (R : Internal_Entity_Match_Expr) return String;


         

      

      type Internal_Entity_Match_Val_Decl is record

               Node : aliased Bare_Match_Val_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Match_Val_Decl : constant Internal_Entity_Match_Val_Decl;


      function Create_Internal_Entity_Match_Val_Decl
        (Node : Bare_Match_Val_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Match_Val_Decl;


   


      function Trace_Image (R : Internal_Entity_Match_Val_Decl) return String;


         

      

      type Internal_Entity_Module_Ref_Id is record

               Node : aliased Bare_Module_Ref_Id;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Module_Ref_Id : constant Internal_Entity_Module_Ref_Id;


      function Create_Internal_Entity_Module_Ref_Id
        (Node : Bare_Module_Ref_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Module_Ref_Id;


   


      function Trace_Image (R : Internal_Entity_Module_Ref_Id) return String;


         

      

      type Internal_Entity_Node_Decl is record

               Node : aliased Bare_Node_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Node_Decl : constant Internal_Entity_Node_Decl;


      function Create_Internal_Entity_Node_Decl
        (Node : Bare_Node_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Node_Decl;


   


      function Trace_Image (R : Internal_Entity_Node_Decl) return String;


         

      

      type Internal_Entity_Not_Expr is record

               Node : aliased Bare_Not_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Not_Expr : constant Internal_Entity_Not_Expr;


      function Create_Internal_Entity_Not_Expr
        (Node : Bare_Not_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Not_Expr;


   


      function Trace_Image (R : Internal_Entity_Not_Expr) return String;


         

      

      type Internal_Entity_Null_Cond_Dotted_Name is record

               Node : aliased Bare_Null_Cond_Dotted_Name;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Null_Cond_Dotted_Name : constant Internal_Entity_Null_Cond_Dotted_Name;


      function Create_Internal_Entity_Null_Cond_Dotted_Name
        (Node : Bare_Null_Cond_Dotted_Name; Info : Internal_Entity_Info)
         return Internal_Entity_Null_Cond_Dotted_Name;


   


      function Trace_Image (R : Internal_Entity_Null_Cond_Dotted_Name) return String;


         

      

      type Internal_Entity_Subscript_Expr is record

               Node : aliased Bare_Subscript_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Subscript_Expr : constant Internal_Entity_Subscript_Expr;


      function Create_Internal_Entity_Subscript_Expr
        (Node : Bare_Subscript_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Subscript_Expr;


   


      function Trace_Image (R : Internal_Entity_Subscript_Expr) return String;


         

      

      type Internal_Entity_Null_Cond_Subscript_Expr is record

               Node : aliased Bare_Null_Cond_Subscript_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Null_Cond_Subscript_Expr : constant Internal_Entity_Null_Cond_Subscript_Expr;


      function Create_Internal_Entity_Null_Cond_Subscript_Expr
        (Node : Bare_Null_Cond_Subscript_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Null_Cond_Subscript_Expr;


   


      function Trace_Image (R : Internal_Entity_Null_Cond_Subscript_Expr) return String;


         

      

      type Internal_Entity_Null_Lit is record

               Node : aliased Bare_Null_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Null_Lit : constant Internal_Entity_Null_Lit;


      function Create_Internal_Entity_Null_Lit
        (Node : Bare_Null_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Null_Lit;


   


      function Trace_Image (R : Internal_Entity_Null_Lit) return String;


         

      

      type Internal_Entity_Num_Lit is record

               Node : aliased Bare_Num_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Num_Lit : constant Internal_Entity_Num_Lit;


      function Create_Internal_Entity_Num_Lit
        (Node : Bare_Num_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Num_Lit;


   


      function Trace_Image (R : Internal_Entity_Num_Lit) return String;


         

      

      type Internal_Entity_Op is record

               Node : aliased Bare_Op;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op : constant Internal_Entity_Op;


      function Create_Internal_Entity_Op
        (Node : Bare_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Op;


   


      function Trace_Image (R : Internal_Entity_Op) return String;


         

      

      type Internal_Entity_Op_Amp is record

               Node : aliased Bare_Op_Amp;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Amp : constant Internal_Entity_Op_Amp;


      function Create_Internal_Entity_Op_Amp
        (Node : Bare_Op_Amp; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Amp;


   


      function Trace_Image (R : Internal_Entity_Op_Amp) return String;


         

      

      type Internal_Entity_Op_And is record

               Node : aliased Bare_Op_And;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_And : constant Internal_Entity_Op_And;


      function Create_Internal_Entity_Op_And
        (Node : Bare_Op_And; Info : Internal_Entity_Info)
         return Internal_Entity_Op_And;


   


      function Trace_Image (R : Internal_Entity_Op_And) return String;


         

      

      type Internal_Entity_Op_Div is record

               Node : aliased Bare_Op_Div;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Div : constant Internal_Entity_Op_Div;


      function Create_Internal_Entity_Op_Div
        (Node : Bare_Op_Div; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Div;


   


      function Trace_Image (R : Internal_Entity_Op_Div) return String;


         

      

      type Internal_Entity_Op_Eq is record

               Node : aliased Bare_Op_Eq;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Eq : constant Internal_Entity_Op_Eq;


      function Create_Internal_Entity_Op_Eq
        (Node : Bare_Op_Eq; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Eq;


   


      function Trace_Image (R : Internal_Entity_Op_Eq) return String;


         

      

      type Internal_Entity_Op_Gt is record

               Node : aliased Bare_Op_Gt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Gt : constant Internal_Entity_Op_Gt;


      function Create_Internal_Entity_Op_Gt
        (Node : Bare_Op_Gt; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Gt;


   


      function Trace_Image (R : Internal_Entity_Op_Gt) return String;


         

      

      type Internal_Entity_Op_Gte is record

               Node : aliased Bare_Op_Gte;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Gte : constant Internal_Entity_Op_Gte;


      function Create_Internal_Entity_Op_Gte
        (Node : Bare_Op_Gte; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Gte;


   


      function Trace_Image (R : Internal_Entity_Op_Gte) return String;


         

      

      type Internal_Entity_Op_Logic_And is record

               Node : aliased Bare_Op_Logic_And;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Logic_And : constant Internal_Entity_Op_Logic_And;


      function Create_Internal_Entity_Op_Logic_And
        (Node : Bare_Op_Logic_And; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Logic_And;


   


      function Trace_Image (R : Internal_Entity_Op_Logic_And) return String;


         

      

      type Internal_Entity_Op_Logic_Or is record

               Node : aliased Bare_Op_Logic_Or;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Logic_Or : constant Internal_Entity_Op_Logic_Or;


      function Create_Internal_Entity_Op_Logic_Or
        (Node : Bare_Op_Logic_Or; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Logic_Or;


   


      function Trace_Image (R : Internal_Entity_Op_Logic_Or) return String;


         

      

      type Internal_Entity_Op_Lt is record

               Node : aliased Bare_Op_Lt;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Lt : constant Internal_Entity_Op_Lt;


      function Create_Internal_Entity_Op_Lt
        (Node : Bare_Op_Lt; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Lt;


   


      function Trace_Image (R : Internal_Entity_Op_Lt) return String;


         

      

      type Internal_Entity_Op_Lte is record

               Node : aliased Bare_Op_Lte;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Lte : constant Internal_Entity_Op_Lte;


      function Create_Internal_Entity_Op_Lte
        (Node : Bare_Op_Lte; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Lte;


   


      function Trace_Image (R : Internal_Entity_Op_Lte) return String;


         

      

      type Internal_Entity_Op_Minus is record

               Node : aliased Bare_Op_Minus;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Minus : constant Internal_Entity_Op_Minus;


      function Create_Internal_Entity_Op_Minus
        (Node : Bare_Op_Minus; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Minus;


   


      function Trace_Image (R : Internal_Entity_Op_Minus) return String;


         

      

      type Internal_Entity_Op_Mult is record

               Node : aliased Bare_Op_Mult;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Mult : constant Internal_Entity_Op_Mult;


      function Create_Internal_Entity_Op_Mult
        (Node : Bare_Op_Mult; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Mult;


   


      function Trace_Image (R : Internal_Entity_Op_Mult) return String;


         

      

      type Internal_Entity_Op_Ne is record

               Node : aliased Bare_Op_Ne;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Ne : constant Internal_Entity_Op_Ne;


      function Create_Internal_Entity_Op_Ne
        (Node : Bare_Op_Ne; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Ne;


   


      function Trace_Image (R : Internal_Entity_Op_Ne) return String;


         

      

      type Internal_Entity_Op_Or is record

               Node : aliased Bare_Op_Or;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Or : constant Internal_Entity_Op_Or;


      function Create_Internal_Entity_Op_Or
        (Node : Bare_Op_Or; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Or;


   


      function Trace_Image (R : Internal_Entity_Op_Or) return String;


         

      

      type Internal_Entity_Op_Or_Int is record

               Node : aliased Bare_Op_Or_Int;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Or_Int : constant Internal_Entity_Op_Or_Int;


      function Create_Internal_Entity_Op_Or_Int
        (Node : Bare_Op_Or_Int; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Or_Int;


   


      function Trace_Image (R : Internal_Entity_Op_Or_Int) return String;


         

      

      type Internal_Entity_Op_Plus is record

               Node : aliased Bare_Op_Plus;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Op_Plus : constant Internal_Entity_Op_Plus;


      function Create_Internal_Entity_Op_Plus
        (Node : Bare_Op_Plus; Info : Internal_Entity_Info)
         return Internal_Entity_Op_Plus;


   


      function Trace_Image (R : Internal_Entity_Op_Plus) return String;


         

      

      type Internal_Entity_Param is record

               Node : aliased Bare_Param;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Param : constant Internal_Entity_Param;


      function Create_Internal_Entity_Param
        (Node : Bare_Param; Info : Internal_Entity_Info)
         return Internal_Entity_Param;


   


      function Trace_Image (R : Internal_Entity_Param) return String;


         

      

      type Internal_Entity_Param_List is record

               Node : aliased Bare_Param_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Param_List : constant Internal_Entity_Param_List;


      function Create_Internal_Entity_Param_List
        (Node : Bare_Param_List; Info : Internal_Entity_Info)
         return Internal_Entity_Param_List;


   
      function Hash (R : Internal_Entity_Param_List) return Hash_Type;


      function Trace_Image (R : Internal_Entity_Param_List) return String;


         

      

      type Internal_Entity_Paren_Expr is record

               Node : aliased Bare_Paren_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Paren_Expr : constant Internal_Entity_Paren_Expr;


      function Create_Internal_Entity_Paren_Expr
        (Node : Bare_Paren_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Paren_Expr;


   


      function Trace_Image (R : Internal_Entity_Paren_Expr) return String;


         

      

      type Internal_Entity_Parse_Node_Expr is record

               Node : aliased Bare_Parse_Node_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Parse_Node_Expr : constant Internal_Entity_Parse_Node_Expr;


      function Create_Internal_Entity_Parse_Node_Expr
        (Node : Bare_Parse_Node_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Parse_Node_Expr;


   


      function Trace_Image (R : Internal_Entity_Parse_Node_Expr) return String;


         

      

      type Internal_Entity_Single_Line_String_Lit is record

               Node : aliased Bare_Single_Line_String_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Single_Line_String_Lit : constant Internal_Entity_Single_Line_String_Lit;


      function Create_Internal_Entity_Single_Line_String_Lit
        (Node : Bare_Single_Line_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Single_Line_String_Lit;


   


      function Trace_Image (R : Internal_Entity_Single_Line_String_Lit) return String;


         

      

      type Internal_Entity_Pattern_Single_Line_String_Lit is record

               Node : aliased Bare_Pattern_Single_Line_String_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Pattern_Single_Line_String_Lit : constant Internal_Entity_Pattern_Single_Line_String_Lit;


      function Create_Internal_Entity_Pattern_Single_Line_String_Lit
        (Node : Bare_Pattern_Single_Line_String_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Pattern_Single_Line_String_Lit;


   


      function Trace_Image (R : Internal_Entity_Pattern_Single_Line_String_Lit) return String;


         

      

      type Internal_Entity_Raise_Expr is record

               Node : aliased Bare_Raise_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Raise_Expr : constant Internal_Entity_Raise_Expr;


      function Create_Internal_Entity_Raise_Expr
        (Node : Bare_Raise_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Raise_Expr;


   


      function Trace_Image (R : Internal_Entity_Raise_Expr) return String;


         

      

      type Internal_Entity_Ref_Id is record

               Node : aliased Bare_Ref_Id;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ref_Id : constant Internal_Entity_Ref_Id;


      function Create_Internal_Entity_Ref_Id
        (Node : Bare_Ref_Id; Info : Internal_Entity_Info)
         return Internal_Entity_Ref_Id;


   


      function Trace_Image (R : Internal_Entity_Ref_Id) return String;


         

      

      type Internal_Entity_Ref_Id_List is record

               Node : aliased Bare_Ref_Id_List;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Ref_Id_List : constant Internal_Entity_Ref_Id_List;


      function Create_Internal_Entity_Ref_Id_List
        (Node : Bare_Ref_Id_List; Info : Internal_Entity_Info)
         return Internal_Entity_Ref_Id_List;


   


      function Trace_Image (R : Internal_Entity_Ref_Id_List) return String;


         

      

      type Internal_Entity_Self_Decl is record

               Node : aliased Bare_Self_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Self_Decl : constant Internal_Entity_Self_Decl;


      function Create_Internal_Entity_Self_Decl
        (Node : Bare_Self_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Self_Decl;


   


      function Trace_Image (R : Internal_Entity_Self_Decl) return String;


         

      

      type Internal_Entity_Simple_Type_Ref is record

               Node : aliased Bare_Simple_Type_Ref;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Simple_Type_Ref : constant Internal_Entity_Simple_Type_Ref;


      function Create_Internal_Entity_Simple_Type_Ref
        (Node : Bare_Simple_Type_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Simple_Type_Ref;


   


      function Trace_Image (R : Internal_Entity_Simple_Type_Ref) return String;


         

      

      type Internal_Entity_Struct_Decl is record

               Node : aliased Bare_Struct_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Struct_Decl : constant Internal_Entity_Struct_Decl;


      function Create_Internal_Entity_Struct_Decl
        (Node : Bare_Struct_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Struct_Decl;


   


      function Trace_Image (R : Internal_Entity_Struct_Decl) return String;


         

      

      type Internal_Entity_Synth_Arg_Decl is record

               Node : aliased Bare_Synth_Arg_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Synth_Arg_Decl : constant Internal_Entity_Synth_Arg_Decl;


      function Create_Internal_Entity_Synth_Arg_Decl
        (Node : Bare_Synth_Arg_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Synth_Arg_Decl;


   


      function Trace_Image (R : Internal_Entity_Synth_Arg_Decl) return String;


         

      

      type Internal_Entity_Synth_Fun_Decl is record

               Node : aliased Bare_Synth_Fun_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Synth_Fun_Decl : constant Internal_Entity_Synth_Fun_Decl;


      function Create_Internal_Entity_Synth_Fun_Decl
        (Node : Bare_Synth_Fun_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Synth_Fun_Decl;


   


      function Trace_Image (R : Internal_Entity_Synth_Fun_Decl) return String;


         

      

      type Internal_Entity_Synthetic_Lexer_Decl is record

               Node : aliased Bare_Synthetic_Lexer_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Synthetic_Lexer_Decl : constant Internal_Entity_Synthetic_Lexer_Decl;


      function Create_Internal_Entity_Synthetic_Lexer_Decl
        (Node : Bare_Synthetic_Lexer_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Synthetic_Lexer_Decl;


   


      function Trace_Image (R : Internal_Entity_Synthetic_Lexer_Decl) return String;


         

      

      type Internal_Entity_Token_Lit is record

               Node : aliased Bare_Token_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Token_Lit : constant Internal_Entity_Token_Lit;


      function Create_Internal_Entity_Token_Lit
        (Node : Bare_Token_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Token_Lit;


   


      function Trace_Image (R : Internal_Entity_Token_Lit) return String;


         

      

      type Internal_Entity_Token_No_Case_Lit is record

               Node : aliased Bare_Token_No_Case_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Token_No_Case_Lit : constant Internal_Entity_Token_No_Case_Lit;


      function Create_Internal_Entity_Token_No_Case_Lit
        (Node : Bare_Token_No_Case_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Token_No_Case_Lit;


   


      function Trace_Image (R : Internal_Entity_Token_No_Case_Lit) return String;


         

      

      type Internal_Entity_Token_Pattern_Concat is record

               Node : aliased Bare_Token_Pattern_Concat;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Token_Pattern_Concat : constant Internal_Entity_Token_Pattern_Concat;


      function Create_Internal_Entity_Token_Pattern_Concat
        (Node : Bare_Token_Pattern_Concat; Info : Internal_Entity_Info)
         return Internal_Entity_Token_Pattern_Concat;


   


      function Trace_Image (R : Internal_Entity_Token_Pattern_Concat) return String;


         

      

      type Internal_Entity_Token_Pattern_Lit is record

               Node : aliased Bare_Token_Pattern_Lit;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Token_Pattern_Lit : constant Internal_Entity_Token_Pattern_Lit;


      function Create_Internal_Entity_Token_Pattern_Lit
        (Node : Bare_Token_Pattern_Lit; Info : Internal_Entity_Info)
         return Internal_Entity_Token_Pattern_Lit;


   


      function Trace_Image (R : Internal_Entity_Token_Pattern_Lit) return String;


         

      

      type Internal_Entity_Token_Ref is record

               Node : aliased Bare_Token_Ref;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Token_Ref : constant Internal_Entity_Token_Ref;


      function Create_Internal_Entity_Token_Ref
        (Node : Bare_Token_Ref; Info : Internal_Entity_Info)
         return Internal_Entity_Token_Ref;


   


      function Trace_Image (R : Internal_Entity_Token_Ref) return String;


         

      

      type Internal_Entity_Trait_Decl is record

               Node : aliased Bare_Trait_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Trait_Decl : constant Internal_Entity_Trait_Decl;


      function Create_Internal_Entity_Trait_Decl
        (Node : Bare_Trait_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Trait_Decl;


   


      function Trace_Image (R : Internal_Entity_Trait_Decl) return String;


         

      

      type Internal_Entity_Try_Expr is record

               Node : aliased Bare_Try_Expr;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Try_Expr : constant Internal_Entity_Try_Expr;


      function Create_Internal_Entity_Try_Expr
        (Node : Bare_Try_Expr; Info : Internal_Entity_Info)
         return Internal_Entity_Try_Expr;


   


      function Trace_Image (R : Internal_Entity_Try_Expr) return String;


         

      

      type Internal_Entity_Un_Op is record

               Node : aliased Bare_Un_Op;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Un_Op : constant Internal_Entity_Un_Op;


      function Create_Internal_Entity_Un_Op
        (Node : Bare_Un_Op; Info : Internal_Entity_Info)
         return Internal_Entity_Un_Op;


   


      function Trace_Image (R : Internal_Entity_Un_Op) return String;


         

      

      type Internal_Entity_Val_Decl is record

               Node : aliased Bare_Val_Decl;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Val_Decl : constant Internal_Entity_Val_Decl;


      function Create_Internal_Entity_Val_Decl
        (Node : Bare_Val_Decl; Info : Internal_Entity_Info)
         return Internal_Entity_Val_Decl;


   


      function Trace_Image (R : Internal_Entity_Val_Decl) return String;


         

      

      type Internal_Entity_Var_Bind is record

               Node : aliased Bare_Var_Bind;
               --  The stored AST node
               
               Info : aliased Internal_Entity_Info;
               --  Entity info for this node
               
      end record
        with Convention => C;
      No_Entity_Var_Bind : constant Internal_Entity_Var_Bind;


      function Create_Internal_Entity_Var_Bind
        (Node : Bare_Var_Bind; Info : Internal_Entity_Info)
         return Internal_Entity_Var_Bind;


   


      function Trace_Image (R : Internal_Entity_Var_Bind) return String;


         

      

      type Internal_Env_Assoc is record

               Key : aliased Symbol_Type;
               
               
               Value : aliased Bare_Lkt_Node;
               
               
               Dest_Env : aliased Internal_Designated_Env;
               
               
               Metadata : aliased Internal_Metadata;
               
               
      end record
        with Convention => C;
      No_Env_Assoc : constant Internal_Env_Assoc;

      procedure Inc_Ref (R : Internal_Env_Assoc);
      procedure Dec_Ref (R : in out Internal_Env_Assoc);


      function Equivalent (L, R : Internal_Env_Assoc) return Boolean;

   


      function Trace_Image (R : Internal_Env_Assoc) return String;


         

      

      type Internal_Formal_Param is record

               Formal_Name : aliased Symbol_Type;
               
               
               Formal_Type : aliased Internal_Entity_Type_Decl;
               
               
               Has_Default_Value : aliased Boolean;
               
               
               Accept_Logical_Var : aliased Boolean;
               
               
               Decl : aliased Internal_Entity_Decl;
               
               
      end record
        with Convention => C;
      No_Formal_Param : constant Internal_Formal_Param;




   
      function Hash (R : Internal_Formal_Param) return Hash_Type;


      function Trace_Image (R : Internal_Formal_Param) return String;


         

      

      type Internal_Logic_Context is record

               Ref_Node : aliased Internal_Entity;
               
               
               Decl_Node : aliased Internal_Entity;
               
               
      end record
        with Convention => C;
      No_Logic_Context : constant Internal_Logic_Context;




   


      function Trace_Image (R : Internal_Logic_Context) return String;


         

      

      type Internal_Param_Match is record

               Has_Matched : aliased Boolean;
               
               
               Actual : aliased Internal_Entity_Param;
               
               
               Formal : aliased Internal_Formal_Param;
               
               
      end record
        with Convention => C;
      No_Param_Match : constant Internal_Param_Match;




   


      function Trace_Image (R : Internal_Param_Match) return String;


         

      

      type Internal_Solver_Diagnostic is record

               Message_Template : aliased String_Type;
               
               
               Args : aliased Internal_Entity_Array_Access;
               
               
               Location : aliased Bare_Lkt_Node;
               
               
               Contexts : aliased Internal_Logic_Context_Array_Access;
               
               
               Round : aliased Integer;
               
               
      end record
        with Convention => C;
      No_Solver_Diagnostic : constant Internal_Solver_Diagnostic;

      procedure Inc_Ref (R : Internal_Solver_Diagnostic);
      procedure Dec_Ref (R : in out Internal_Solver_Diagnostic);


      function Equivalent (L, R : Internal_Solver_Diagnostic) return Boolean;

   


      function Trace_Image (R : Internal_Solver_Diagnostic) return String;


         

      

      type Internal_Solver_Result is record

               Success : aliased Boolean;
               
               
               Diagnostics : aliased Internal_Solver_Diagnostic_Array_Access;
               
               
      end record
        with Convention => C;
      No_Solver_Result : constant Internal_Solver_Result;

      procedure Inc_Ref (R : Internal_Solver_Result);
      procedure Dec_Ref (R : in out Internal_Solver_Result);


      function Equivalent (L, R : Internal_Solver_Result) return Boolean;

   


      function Trace_Image (R : Internal_Solver_Result) return String;



   -----------------
   -- Array types --
   -----------------

   --  We implement array types as discriminated records so that binding to C
   --  can be done without copy.

         

   

   type Internal_Bare_Lkt_Node_Array is
      array (Positive range <>) of Bare_Lkt_Node;

   type Bare_Lkt_Node_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Bare_Lkt_Node_Array (1 .. N);
   end record;

   Empty_Bare_Lkt_Node_Array_Record : aliased Bare_Lkt_Node_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Bare_Lkt_Node_Array_Type : constant Bare_Lkt_Node_Array_Access :=
      Empty_Bare_Lkt_Node_Array_Record'Access;


   function Create_Bare_Lkt_Node_Array (Items_Count : Natural) return Bare_Lkt_Node_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Bare_Lkt_Node_Array
     (Items : Internal_Bare_Lkt_Node_Array) return Bare_Lkt_Node_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Bare_Lkt_Node_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Lkt_Node;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Bare_Lkt_Node_Array_Access) return Bare_Lkt_Node_Array_Access;


   function Length (T : Bare_Lkt_Node_Array_Access) return Natural;

   procedure Inc_Ref (T : Bare_Lkt_Node_Array_Access);
   procedure Dec_Ref (T : in out Bare_Lkt_Node_Array_Access);

   function Equivalent (L, R : Bare_Lkt_Node_Array_Access) return Boolean;


      function Trace_Image (A : Bare_Lkt_Node_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Bare_Lkt_Node_Array_Record, Bare_Lkt_Node_Array_Access);

         

   

   type Internal_Integer_Array is
      array (Positive range <>) of Integer;

   type Integer_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Integer_Array (1 .. N);
   end record;

   Empty_Integer_Array_Record : aliased Integer_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Integer_Array_Type : constant Integer_Array_Access :=
      Empty_Integer_Array_Record'Access;


   function Create_Integer_Array (Items_Count : Natural) return Integer_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Integer_Array
     (Items : Internal_Integer_Array) return Integer_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Integer_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Integer;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Integer_Array_Access) return Integer_Array_Access;


   function Length (T : Integer_Array_Access) return Natural;

   procedure Inc_Ref (T : Integer_Array_Access);
   procedure Dec_Ref (T : in out Integer_Array_Access);

   function Equivalent (L, R : Integer_Array_Access) return Boolean;


      function Trace_Image (A : Integer_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Integer_Array_Record, Integer_Array_Access);

         

   

   type Internal_Internal_Entity_Array is
      array (Positive range <>) of Internal_Entity;

   type Internal_Entity_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Entity_Array (1 .. N);
   end record;

   Empty_Internal_Entity_Array_Record : aliased Internal_Entity_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Array_Type : constant Internal_Entity_Array_Access :=
      Empty_Internal_Entity_Array_Record'Access;

   function Create_Internal_Entity_Array
     (Items : AST_Envs.Entity_Array) return Internal_Entity_Array_Access;

   function Create_Internal_Entity_Array (Items_Count : Natural) return Internal_Entity_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Array
     (Items : Internal_Internal_Entity_Array) return Internal_Entity_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Entity_Array_Access) return Internal_Entity_Array_Access;


   function Length (T : Internal_Entity_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Array_Access);

   function Equivalent (L, R : Internal_Entity_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Entity_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Entity_Array_Record, Internal_Entity_Array_Access);

         

   

   type Internal_Internal_Entity_Enum_Class_Alt_Decl_Array is
      array (Positive range <>) of Internal_Entity_Enum_Class_Alt_Decl;

   type Internal_Entity_Enum_Class_Alt_Decl_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Entity_Enum_Class_Alt_Decl_Array (1 .. N);
   end record;

   Empty_Internal_Entity_Enum_Class_Alt_Decl_Array_Record : aliased Internal_Entity_Enum_Class_Alt_Decl_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Enum_Class_Alt_Decl_Array_Type : constant Internal_Entity_Enum_Class_Alt_Decl_Array_Access :=
      Empty_Internal_Entity_Enum_Class_Alt_Decl_Array_Record'Access;


   function Create_Internal_Entity_Enum_Class_Alt_Decl_Array (Items_Count : Natural) return Internal_Entity_Enum_Class_Alt_Decl_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Enum_Class_Alt_Decl_Array
     (Items : Internal_Internal_Entity_Enum_Class_Alt_Decl_Array) return Internal_Entity_Enum_Class_Alt_Decl_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Enum_Class_Alt_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Enum_Class_Alt_Decl;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Entity_Enum_Class_Alt_Decl_Array_Access) return Internal_Entity_Enum_Class_Alt_Decl_Array_Access;


   function Length (T : Internal_Entity_Enum_Class_Alt_Decl_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Enum_Class_Alt_Decl_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Enum_Class_Alt_Decl_Array_Access);

   function Equivalent (L, R : Internal_Entity_Enum_Class_Alt_Decl_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Entity_Enum_Class_Alt_Decl_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Entity_Enum_Class_Alt_Decl_Array_Record, Internal_Entity_Enum_Class_Alt_Decl_Array_Access);

         

   

   type Internal_Internal_Entity_Expr_Array is
      array (Positive range <>) of Internal_Entity_Expr;

   type Internal_Entity_Expr_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Entity_Expr_Array (1 .. N);
   end record;

   Empty_Internal_Entity_Expr_Array_Record : aliased Internal_Entity_Expr_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Expr_Array_Type : constant Internal_Entity_Expr_Array_Access :=
      Empty_Internal_Entity_Expr_Array_Record'Access;


   function Create_Internal_Entity_Expr_Array (Items_Count : Natural) return Internal_Entity_Expr_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Expr_Array
     (Items : Internal_Internal_Entity_Expr_Array) return Internal_Entity_Expr_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Expr_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Expr;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Entity_Expr_Array_Access) return Internal_Entity_Expr_Array_Access;


   function Length (T : Internal_Entity_Expr_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Expr_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Expr_Array_Access);

   function Equivalent (L, R : Internal_Entity_Expr_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Entity_Expr_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Entity_Expr_Array_Record, Internal_Entity_Expr_Array_Access);

         

   

   type Internal_Internal_Entity_Field_Decl_Array is
      array (Positive range <>) of Internal_Entity_Field_Decl;

   type Internal_Entity_Field_Decl_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Entity_Field_Decl_Array (1 .. N);
   end record;

   Empty_Internal_Entity_Field_Decl_Array_Record : aliased Internal_Entity_Field_Decl_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Field_Decl_Array_Type : constant Internal_Entity_Field_Decl_Array_Access :=
      Empty_Internal_Entity_Field_Decl_Array_Record'Access;


   function Create_Internal_Entity_Field_Decl_Array (Items_Count : Natural) return Internal_Entity_Field_Decl_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Field_Decl_Array
     (Items : Internal_Internal_Entity_Field_Decl_Array) return Internal_Entity_Field_Decl_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Field_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Field_Decl;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Entity_Field_Decl_Array_Access) return Internal_Entity_Field_Decl_Array_Access;


   function Length (T : Internal_Entity_Field_Decl_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Field_Decl_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Field_Decl_Array_Access);

   function Equivalent (L, R : Internal_Entity_Field_Decl_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Entity_Field_Decl_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Entity_Field_Decl_Array_Record, Internal_Entity_Field_Decl_Array_Access);

         

   

   type Internal_Internal_Entity_Full_Decl_Array is
      array (Positive range <>) of Internal_Entity_Full_Decl;

   type Internal_Entity_Full_Decl_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Entity_Full_Decl_Array (1 .. N);
   end record;

   Empty_Internal_Entity_Full_Decl_Array_Record : aliased Internal_Entity_Full_Decl_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Full_Decl_Array_Type : constant Internal_Entity_Full_Decl_Array_Access :=
      Empty_Internal_Entity_Full_Decl_Array_Record'Access;


   function Create_Internal_Entity_Full_Decl_Array (Items_Count : Natural) return Internal_Entity_Full_Decl_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Full_Decl_Array
     (Items : Internal_Internal_Entity_Full_Decl_Array) return Internal_Entity_Full_Decl_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Full_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Full_Decl;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Entity_Full_Decl_Array_Access) return Internal_Entity_Full_Decl_Array_Access;


   function Length (T : Internal_Entity_Full_Decl_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Full_Decl_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Full_Decl_Array_Access);

   function Equivalent (L, R : Internal_Entity_Full_Decl_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Entity_Full_Decl_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Entity_Full_Decl_Array_Record, Internal_Entity_Full_Decl_Array_Access);

         

   

   type Internal_Internal_Entity_Generic_Formal_Type_Decl_Array is
      array (Positive range <>) of Internal_Entity_Generic_Formal_Type_Decl;

   type Internal_Entity_Generic_Formal_Type_Decl_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Entity_Generic_Formal_Type_Decl_Array (1 .. N);
   end record;

   Empty_Internal_Entity_Generic_Formal_Type_Decl_Array_Record : aliased Internal_Entity_Generic_Formal_Type_Decl_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Generic_Formal_Type_Decl_Array_Type : constant Internal_Entity_Generic_Formal_Type_Decl_Array_Access :=
      Empty_Internal_Entity_Generic_Formal_Type_Decl_Array_Record'Access;


   function Create_Internal_Entity_Generic_Formal_Type_Decl_Array (Items_Count : Natural) return Internal_Entity_Generic_Formal_Type_Decl_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Generic_Formal_Type_Decl_Array
     (Items : Internal_Internal_Entity_Generic_Formal_Type_Decl_Array) return Internal_Entity_Generic_Formal_Type_Decl_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Generic_Formal_Type_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Generic_Formal_Type_Decl;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Entity_Generic_Formal_Type_Decl_Array_Access) return Internal_Entity_Generic_Formal_Type_Decl_Array_Access;


   function Length (T : Internal_Entity_Generic_Formal_Type_Decl_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Generic_Formal_Type_Decl_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Generic_Formal_Type_Decl_Array_Access);

   function Equivalent (L, R : Internal_Entity_Generic_Formal_Type_Decl_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Entity_Generic_Formal_Type_Decl_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Entity_Generic_Formal_Type_Decl_Array_Record, Internal_Entity_Generic_Formal_Type_Decl_Array_Access);

         

   

   type Internal_Internal_Entity_Param_Array is
      array (Positive range <>) of Internal_Entity_Param;

   type Internal_Entity_Param_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Entity_Param_Array (1 .. N);
   end record;

   Empty_Internal_Entity_Param_Array_Record : aliased Internal_Entity_Param_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Param_Array_Type : constant Internal_Entity_Param_Array_Access :=
      Empty_Internal_Entity_Param_Array_Record'Access;


   function Create_Internal_Entity_Param_Array (Items_Count : Natural) return Internal_Entity_Param_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Param_Array
     (Items : Internal_Internal_Entity_Param_Array) return Internal_Entity_Param_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Param_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Param;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Entity_Param_Array_Access) return Internal_Entity_Param_Array_Access;


   function Length (T : Internal_Entity_Param_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Param_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Param_Array_Access);

   function Equivalent (L, R : Internal_Entity_Param_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Entity_Param_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Entity_Param_Array_Record, Internal_Entity_Param_Array_Access);

         

   

   type Internal_Internal_Entity_Type_Decl_Array is
      array (Positive range <>) of Internal_Entity_Type_Decl;

   type Internal_Entity_Type_Decl_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Entity_Type_Decl_Array (1 .. N);
   end record;

   Empty_Internal_Entity_Type_Decl_Array_Record : aliased Internal_Entity_Type_Decl_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Entity_Type_Decl_Array_Type : constant Internal_Entity_Type_Decl_Array_Access :=
      Empty_Internal_Entity_Type_Decl_Array_Record'Access;


   function Create_Internal_Entity_Type_Decl_Array (Items_Count : Natural) return Internal_Entity_Type_Decl_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Entity_Type_Decl_Array
     (Items : Internal_Internal_Entity_Type_Decl_Array) return Internal_Entity_Type_Decl_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Entity_Type_Decl_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Entity_Type_Decl;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Entity_Type_Decl_Array_Access) return Internal_Entity_Type_Decl_Array_Access;


   function Length (T : Internal_Entity_Type_Decl_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Entity_Type_Decl_Array_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Type_Decl_Array_Access);

   function Equivalent (L, R : Internal_Entity_Type_Decl_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Entity_Type_Decl_Array_Access) return String;


      function Hash (R : Internal_Entity_Type_Decl_Array_Access) return Hash_Type;

  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Entity_Type_Decl_Array_Record, Internal_Entity_Type_Decl_Array_Access);

         

   

   type Internal_Internal_Env_Assoc_Array is
      array (Positive range <>) of Internal_Env_Assoc;

   type Internal_Env_Assoc_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Env_Assoc_Array (1 .. N);
   end record;

   Empty_Internal_Env_Assoc_Array_Record : aliased Internal_Env_Assoc_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Env_Assoc_Array_Type : constant Internal_Env_Assoc_Array_Access :=
      Empty_Internal_Env_Assoc_Array_Record'Access;


   function Create_Internal_Env_Assoc_Array (Items_Count : Natural) return Internal_Env_Assoc_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Env_Assoc_Array
     (Items : Internal_Internal_Env_Assoc_Array) return Internal_Env_Assoc_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Env_Assoc_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Env_Assoc;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Env_Assoc_Array_Access) return Internal_Env_Assoc_Array_Access;


   function Length (T : Internal_Env_Assoc_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Env_Assoc_Array_Access);
   procedure Dec_Ref (T : in out Internal_Env_Assoc_Array_Access);

   function Equivalent (L, R : Internal_Env_Assoc_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Env_Assoc_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Env_Assoc_Array_Record, Internal_Env_Assoc_Array_Access);

         

   

   type Internal_Internal_Formal_Param_Array is
      array (Positive range <>) of Internal_Formal_Param;

   type Internal_Formal_Param_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Formal_Param_Array (1 .. N);
   end record;

   Empty_Internal_Formal_Param_Array_Record : aliased Internal_Formal_Param_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Formal_Param_Array_Type : constant Internal_Formal_Param_Array_Access :=
      Empty_Internal_Formal_Param_Array_Record'Access;


   function Create_Internal_Formal_Param_Array (Items_Count : Natural) return Internal_Formal_Param_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Formal_Param_Array
     (Items : Internal_Internal_Formal_Param_Array) return Internal_Formal_Param_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Formal_Param_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Formal_Param;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Formal_Param_Array_Access) return Internal_Formal_Param_Array_Access;


   function Length (T : Internal_Formal_Param_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Formal_Param_Array_Access);
   procedure Dec_Ref (T : in out Internal_Formal_Param_Array_Access);

   function Equivalent (L, R : Internal_Formal_Param_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Formal_Param_Array_Access) return String;


      function Hash (R : Internal_Formal_Param_Array_Access) return Hash_Type;

  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Formal_Param_Array_Record, Internal_Formal_Param_Array_Access);

         

   

   type Internal_Internal_Logic_Context_Array is
      array (Positive range <>) of Internal_Logic_Context;

   type Internal_Logic_Context_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Logic_Context_Array (1 .. N);
   end record;

   Empty_Internal_Logic_Context_Array_Record : aliased Internal_Logic_Context_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Logic_Context_Array_Type : constant Internal_Logic_Context_Array_Access :=
      Empty_Internal_Logic_Context_Array_Record'Access;


   function Create_Internal_Logic_Context_Array (Items_Count : Natural) return Internal_Logic_Context_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Logic_Context_Array
     (Items : Internal_Internal_Logic_Context_Array) return Internal_Logic_Context_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Logic_Context_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Logic_Context;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Logic_Context_Array_Access) return Internal_Logic_Context_Array_Access;


   function Length (T : Internal_Logic_Context_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Logic_Context_Array_Access);
   procedure Dec_Ref (T : in out Internal_Logic_Context_Array_Access);

   function Equivalent (L, R : Internal_Logic_Context_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Logic_Context_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Logic_Context_Array_Record, Internal_Logic_Context_Array_Access);

         

   

   type Internal_Internal_Param_Match_Array is
      array (Positive range <>) of Internal_Param_Match;

   type Internal_Param_Match_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Param_Match_Array (1 .. N);
   end record;

   Empty_Internal_Param_Match_Array_Record : aliased Internal_Param_Match_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Param_Match_Array_Type : constant Internal_Param_Match_Array_Access :=
      Empty_Internal_Param_Match_Array_Record'Access;


   function Create_Internal_Param_Match_Array (Items_Count : Natural) return Internal_Param_Match_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Param_Match_Array
     (Items : Internal_Internal_Param_Match_Array) return Internal_Param_Match_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Param_Match_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Param_Match;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Param_Match_Array_Access) return Internal_Param_Match_Array_Access;


   function Length (T : Internal_Param_Match_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Param_Match_Array_Access);
   procedure Dec_Ref (T : in out Internal_Param_Match_Array_Access);

   function Equivalent (L, R : Internal_Param_Match_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Param_Match_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Param_Match_Array_Record, Internal_Param_Match_Array_Access);

         

   

   type Internal_Internal_Solver_Diagnostic_Array is
      array (Positive range <>) of Internal_Solver_Diagnostic;

   type Internal_Solver_Diagnostic_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Internal_Solver_Diagnostic_Array (1 .. N);
   end record;

   Empty_Internal_Solver_Diagnostic_Array_Record : aliased Internal_Solver_Diagnostic_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Internal_Solver_Diagnostic_Array_Type : constant Internal_Solver_Diagnostic_Array_Access :=
      Empty_Internal_Solver_Diagnostic_Array_Record'Access;


   function Create_Internal_Solver_Diagnostic_Array (Items_Count : Natural) return Internal_Solver_Diagnostic_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Internal_Solver_Diagnostic_Array
     (Items : Internal_Internal_Solver_Diagnostic_Array) return Internal_Solver_Diagnostic_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Internal_Solver_Diagnostic_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Internal_Solver_Diagnostic;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Internal_Solver_Diagnostic_Array_Access) return Internal_Solver_Diagnostic_Array_Access;


   function Length (T : Internal_Solver_Diagnostic_Array_Access) return Natural;

   procedure Inc_Ref (T : Internal_Solver_Diagnostic_Array_Access);
   procedure Dec_Ref (T : in out Internal_Solver_Diagnostic_Array_Access);

   function Equivalent (L, R : Internal_Solver_Diagnostic_Array_Access) return Boolean;


      function Trace_Image (A : Internal_Solver_Diagnostic_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Internal_Solver_Diagnostic_Array_Record, Internal_Solver_Diagnostic_Array_Access);

         

   

   type Internal_Lexical_Env_Array is
      array (Positive range <>) of Lexical_Env;

   type Lexical_Env_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Lexical_Env_Array (1 .. N);
   end record;

   Empty_Lexical_Env_Array_Record : aliased Lexical_Env_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Lexical_Env_Array_Type : constant Lexical_Env_Array_Access :=
      Empty_Lexical_Env_Array_Record'Access;


   function Create_Lexical_Env_Array (Items_Count : Natural) return Lexical_Env_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Lexical_Env_Array
     (Items : Internal_Lexical_Env_Array) return Lexical_Env_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Lexical_Env_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Lexical_Env;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Lexical_Env_Array_Access) return Lexical_Env_Array_Access;


   function Length (T : Lexical_Env_Array_Access) return Natural;

   procedure Inc_Ref (T : Lexical_Env_Array_Access);
   procedure Dec_Ref (T : in out Lexical_Env_Array_Access);

   function Equivalent (L, R : Lexical_Env_Array_Access) return Boolean;


      function Trace_Image (A : Lexical_Env_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Lexical_Env_Array_Record, Lexical_Env_Array_Access);

         

   

   type Internal_Logic_Equation_Array is
      array (Positive range <>) of Logic_Equation;

   type Logic_Equation_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Logic_Equation_Array (1 .. N);
   end record;

   Empty_Logic_Equation_Array_Record : aliased Logic_Equation_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Logic_Equation_Array_Type : constant Logic_Equation_Array_Access :=
      Empty_Logic_Equation_Array_Record'Access;


   function Create_Logic_Equation_Array (Items_Count : Natural) return Logic_Equation_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Logic_Equation_Array
     (Items : Internal_Logic_Equation_Array) return Logic_Equation_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Logic_Equation_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Logic_Equation;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Logic_Equation_Array_Access) return Logic_Equation_Array_Access;


   function Length (T : Logic_Equation_Array_Access) return Natural;

   procedure Inc_Ref (T : Logic_Equation_Array_Access);
   procedure Dec_Ref (T : in out Logic_Equation_Array_Access);

   function Equivalent (L, R : Logic_Equation_Array_Access) return Boolean;


      function Trace_Image (A : Logic_Equation_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Logic_Equation_Array_Record, Logic_Equation_Array_Access);

         

   

   type Internal_Logic_Var_Array is
      array (Positive range <>) of Logic_Var;

   type Logic_Var_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Logic_Var_Array (1 .. N);
   end record;

   Empty_Logic_Var_Array_Record : aliased Logic_Var_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Logic_Var_Array_Type : constant Logic_Var_Array_Access :=
      Empty_Logic_Var_Array_Record'Access;


   function Create_Logic_Var_Array (Items_Count : Natural) return Logic_Var_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Logic_Var_Array
     (Items : Internal_Logic_Var_Array) return Logic_Var_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Logic_Var_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Logic_Var;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Logic_Var_Array_Access) return Logic_Var_Array_Access;


   function Length (T : Logic_Var_Array_Access) return Natural;

   procedure Inc_Ref (T : Logic_Var_Array_Access);
   procedure Dec_Ref (T : in out Logic_Var_Array_Access);

   function Equivalent (L, R : Logic_Var_Array_Access) return Boolean;


      function Trace_Image (A : Logic_Var_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Logic_Var_Array_Record, Logic_Var_Array_Access);

         

   

   type Internal_String_Type_Array is
      array (Positive range <>) of String_Type;

   type String_Type_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_String_Type_Array (1 .. N);
   end record;

   Empty_String_Type_Array_Record : aliased String_Type_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_String_Type_Array_Type : constant String_Type_Array_Access :=
      Empty_String_Type_Array_Record'Access;


   function Create_String_Type_Array (Items_Count : Natural) return String_Type_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_String_Type_Array
     (Items : Internal_String_Type_Array) return String_Type_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : String_Type_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return String_Type;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : String_Type_Array_Access) return String_Type_Array_Access;

      function Join_Strings
        (Separator : String_Type;
         Strings   : String_Type_Array_Access) return String_Type;
      --  Return the concatenation of all strings in ``Strings``, separated by
      --  ``Separator``.

   function Length (T : String_Type_Array_Access) return Natural;

   procedure Inc_Ref (T : String_Type_Array_Access);
   procedure Dec_Ref (T : in out String_Type_Array_Access);

   function Equivalent (L, R : String_Type_Array_Access) return Boolean;


      function Trace_Image (A : String_Type_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (String_Type_Array_Record, String_Type_Array_Access);

         

   

   type Internal_Symbol_Type_Array is
      array (Positive range <>) of Symbol_Type;

   type Symbol_Type_Array_Record (N : Natural) is record
      Ref_Count : Integer;
      --  Negative values are interpreted as "always living singleton".
      --  Non-negative values have the usual ref-counting semantics.

      Items     : Internal_Symbol_Type_Array (1 .. N);
   end record;

   Empty_Symbol_Type_Array_Record : aliased Symbol_Type_Array_Record :=
     (N => 0, Ref_Count => -1, Items => (1 .. 0 => <>));
   No_Symbol_Type_Array_Type : constant Symbol_Type_Array_Access :=
      Empty_Symbol_Type_Array_Record'Access;


   function Create_Symbol_Type_Array (Items_Count : Natural) return Symbol_Type_Array_Access;
   --  Create a new array for N uninitialized elements and give its only
   --  ownership share to the caller.

   function Create_Symbol_Type_Array
     (Items : Internal_Symbol_Type_Array) return Symbol_Type_Array_Access;
   --  Create a new array from an existing collection of elements

   function Get
     (Node    : Bare_Lkt_Node;
      T       : Symbol_Type_Array_Access;
      Index   : Integer;
      Or_Null : Boolean := False) return Symbol_Type;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based. If
   --  the result is ref-counted, a new owning reference is returned.

   function Concat (L, R : Symbol_Type_Array_Access) return Symbol_Type_Array_Access;


   function Length (T : Symbol_Type_Array_Access) return Natural;

   procedure Inc_Ref (T : Symbol_Type_Array_Access);
   procedure Dec_Ref (T : in out Symbol_Type_Array_Access);

   function Equivalent (L, R : Symbol_Type_Array_Access) return Boolean;


      function Trace_Image (A : Symbol_Type_Array_Access) return String;



  procedure Free is new Ada.Unchecked_Deallocation
    (Symbol_Type_Array_Record, Symbol_Type_Array_Access);


   --------------------
   -- Iterator types --
   --------------------

         

   

   type Internal_Bare_Lkt_Node_Iterator is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero.
      --  Negative values are interpreted as "always living singleton".

      Safety_Net : Iterator_Safety_Net;
      --  Safety net for the iterator. Used to check that values produced by
      --  the iterator are still valid. Unlike for other types, we put the
      --  safety net in the internal type so that it can be used in all other
      --  APIs (Python, ...).
      --
      --  While other types (except nodes) are "deeply" converted to native
      --  APIs (for instance: internal arrays are turned into native Python
      --  lists, likewise for array items, etc.), iterators are lazy, so the
      --  deep conversion is not possible.

      Elements : Bare_Lkt_Node_Array_Access;
      Index    : Positive;
   end record;

   Empty_Internal_Bare_Lkt_Node_Iterator : aliased Internal_Bare_Lkt_Node_Iterator :=
     (Ref_Count  => -1,
      Safety_Net => No_Iterator_Safety_Net,
      Elements   => No_Bare_Lkt_Node_Array_Type,
      Index      => 1);
   No_Bare_Lkt_Node_Iterator_Type : constant Bare_Lkt_Node_Iterator_Access :=
      Empty_Internal_Bare_Lkt_Node_Iterator'Access;

   function Next
     (T       : Bare_Lkt_Node_Iterator_Access;
      Element : out Bare_Lkt_Node) return Boolean;

   procedure Inc_Ref (T : Bare_Lkt_Node_Iterator_Access);
   procedure Dec_Ref (T : in out Bare_Lkt_Node_Iterator_Access);

      function Trace_Image (A : Bare_Lkt_Node_Iterator_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Bare_Lkt_Node_Iterator, Bare_Lkt_Node_Iterator_Access);

         

   

   type Internal_Internal_Entity_Iterator is record
      Ref_Count : Integer;
      --  Reference count. The iterator is freed when this drops to zero.
      --  Negative values are interpreted as "always living singleton".

      Safety_Net : Iterator_Safety_Net;
      --  Safety net for the iterator. Used to check that values produced by
      --  the iterator are still valid. Unlike for other types, we put the
      --  safety net in the internal type so that it can be used in all other
      --  APIs (Python, ...).
      --
      --  While other types (except nodes) are "deeply" converted to native
      --  APIs (for instance: internal arrays are turned into native Python
      --  lists, likewise for array items, etc.), iterators are lazy, so the
      --  deep conversion is not possible.

      Elements : Internal_Entity_Array_Access;
      Index    : Positive;
   end record;

   Empty_Internal_Internal_Entity_Iterator : aliased Internal_Internal_Entity_Iterator :=
     (Ref_Count  => -1,
      Safety_Net => No_Iterator_Safety_Net,
      Elements   => No_Internal_Entity_Array_Type,
      Index      => 1);
   No_Internal_Entity_Iterator_Type : constant Internal_Entity_Iterator_Access :=
      Empty_Internal_Internal_Entity_Iterator'Access;

   function Next
     (T       : Internal_Entity_Iterator_Access;
      Element : out Internal_Entity) return Boolean;

   procedure Inc_Ref (T : Internal_Entity_Iterator_Access);
   procedure Dec_Ref (T : in out Internal_Entity_Iterator_Access);

      function Trace_Image (A : Internal_Entity_Iterator_Access) return String;

   procedure Free is new Ada.Unchecked_Deallocation
     (Internal_Internal_Entity_Iterator, Internal_Entity_Iterator_Access);


   ---------------------
   -- Extension specs --
   ---------------------

   


   ------------------------
   -- Named environments --
   ------------------------

   --  The goal of named environments is to provide a sound mechanism to
   --  associate nodes and environments across analysis units: nodes whose
   --  Self_Env comes from another unit ("foreign env"), environments whose
   --  parent comes from another unit (also foreign env), or that contain
   --  symbol/node mappings for nodes coming from other units ("foreign
   --  nodes").
   --
   --  This mechanism comes with the following requirements:
   --
   --  * Ensure that, after unit reparsing, all cross-unit associations are
   --    still valid. For instance, no node's Self_Env can refer to a lexical
   --    environment that has been deallocated.
   --
   --  * Ensure that regardless of the sequence of unit parsing/reparsing that
   --    led to a given set of units (considering only unit filename and source
   --    buffer), the node/env graph (i.e. the result of PLE) is always the
   --    same, i.e. make incremental PLE idempotent.
   --
   --  Note that even though the end goal for named envs is to replace the
   --  previous mechanism (proved to be unsound, as violating the second
   --  requirement), both still coexist during the transition period.
   --
   --  Here is how this mechanism works:
   --
   --  1. Environments can be assigned zero, one or several names (i.e. one or
   --     several symbols). Name(s) assignment happens at environment
   --     construction.
   --
   --  2. As a consequence, multiple environments can be associated to a given
   --     env name. Using a total and deterministic ordering predicate, only
   --     one of them is said to have "precedence": looking up an environment
   --     using that name will return this unique environment.
   --
   --  3. For a given env name, we keep track of all uses of the environment
   --     that is looked up by its name: environment parent link, symbol/node
   --     mapping addition, node's Self_Env assignment. This info is
   --     tracked using the Named_Env_Descriptor record type below, often
   --     abbreviated NED. Note that this tracking happens even when there is
   --     no environment associated to the env name, as we need to do such
   --     updates when an environment gets associated to that env name.
   --
   --  4. Unit reparsing can destroy existing environments and/or create new
   --     ones. This means that, depending on their "ranking" using the
   --     ordering predicate, environments can earn or lose precedence for a
   --     given name.
   --
   --  5. When the precedence changes for a given name, we use the info
   --     collected as per 3. to perform relocation: relevant environment
   --     parent links are updated, symbol/node mappings are removed from the
   --     env that lost precedence and added to the env that earned precedence,
   --     etc.

   --  Tables to populate lexical entries in named envs

   package NED_Assoc_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Internal_Map_Node_Vectors.Vector,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Internal_Map_Node_Vectors."=");
   --  Symbol/lexical env entry mappings for a given named env descriptor.
   --  Symbols are not unique in all mappings, so the lexical env entries are
   --  stored in a vector.

   procedure Add
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : AST_Envs.Internal_Map_Node);
   --  Add a symbol/lexical env entry mapping in Self

   procedure Remove
     (Self : in out NED_Assoc_Maps.Map;
      Key  : Symbol_Type;
      Node : Bare_Lkt_Node);
   --  Remove a symbol/lexical env entry mapping from Self

   --  Global table for named environments

   package Sorted_Env_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Bare_Lkt_Node,
      Element_Type => Lexical_Env);
   --  List of lexical environments, sorted by owning node. This means that the
   --  following must be true for all cursors in such maps::
   --
   --     Key (Cur) = Element (Cur).Env.Node

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Bare_Lkt_Node,
      Hash                => Hash,
      Equivalent_Elements => "=");

   type Named_Env_Descriptor is record
      Name : Symbol_Type;
      --  Name corresponding to this descriptor. Useful during debugging.

      Envs : Sorted_Env_Maps.Map;
      --  For each env name, we can have one or several environments
      --  (concurrent definitions). Just like foreign nodes in lexical
      --  environments, we keep them sorted by node to preserve determinism:
      --  given a set of loaded units, we will always have the same set of
      --  name:env associations sorted in the same order and thus always the
      --  same results at lookup time.

      Env_With_Precedence : Lexical_Env;
      --  Named environment that has precedence for this name.
      --
      --  Most of the time, if Envs is empty, this is Empty_Env and otherwise,
      --  shortcut to Envs.First_Element. However, when a change in Envs
      --  invalidates Env_With_Precedence, we reset it to Empty_Env momentarily
      --  during PLE as a way to tag the temprorary inconsistency. Later on, we
      --  recompute it and perform the needed relocations.

      Foreign_Nodes : NED_Assoc_Maps.Map;
      --  This maps symbols to lists of env entries for all the foreign nodes
      --  in Env_With_Precedence.
      --
      --  This set allows efficient relocation of env entries when
      --  Env_With_Precedence changes.

      Foreign_Envs : Sorted_Env_Maps.Map;
      --  This maps the owning node to env mapping for all lexical environments
      --  whose parent must be Env_With_Precedence. Envs are indexed by owning
      --  node for quick lookup during updates.
      --
      --  This set allows efficient env parent link updates when
      --  Env_With_Precedence changes.

      Nodes_With_Foreign_Env : Node_Sets.Set;
      --  Set of nodes whose env (Self_Env) must be Env_With_Precedence.
      --
      --  This set allows efficient Self_Env updates when Env_With_Precedence
      --  changes.

      --  Note that during the updating process of a reparsed unit
      --  (Update_After_Reparse procedure), these data structures become
      --  temporarily inconsistent: Env_With_Precedence can become Empty_Env
      --  even though Envs is not empty.  This is fine, because when it does,
      --  Update_After_Reparse keeps track of it as to be updated
      --  (Named_Envs_Needing_Update map).
   end record;
   type Named_Env_Descriptor_Access is access Named_Env_Descriptor;
   procedure Destroy is new Ada.Unchecked_Deallocation
     (Named_Env_Descriptor, Named_Env_Descriptor_Access);

   package NED_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Named_Env_Descriptor_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Context-wide table that tracks for all env names the set of lexical envs
   --  that define it.

   type Exiled_Entry_In_NED is record
      Named_Env : Named_Env_Descriptor_Access;
      --  Named env descriptor in which Node is registered

      Key : Symbol_Type;
      --  Key in that Env's internal map that leads to the env descriptor that
      --  contains Node.

      Node : Bare_Lkt_Node;
      --  Exiled node
   end record;

   package Exiled_Entry_In_NED_Vectors is new
      Liblktlang_Support.Vectors (Exiled_Entry_In_NED);

   type Exiled_Env is record
      Named_Env : Named_Env_Descriptor_Access;
      --  Named env descriptor in which Env is registered

      Env : Lexical_Env;
      --  Exiled environment
   end record;

   package Exiled_Env_Vectors is new Liblktlang_Support.Vectors (Exiled_Env);

   type Named_Env_Pair is record
      Name : Symbol_Type;
      --  Name on the lexical environment

      Env  : Lexical_Env;
      --  Named lexical environment
   end record;

   package Named_Env_Vectors is new Liblktlang_Support.Vectors (Named_Env_Pair);

   --  High-level primitives to handle the life cycle of named environment

   function Get_Named_Env_Descriptor
     (Context : Internal_Context;
      Name    : Symbol_Type) return Named_Env_Descriptor_Access;
   --  Return the named env descriptor in Context corresponding to Name. Create
   --  it first, if needed.

   procedure Register_Named_Env
     (Context                   : Internal_Context;
      Name                      : Symbol_Type;
      Env                       : Lexical_Env;
      Named_Envs_Needing_Update : in out NED_Maps.Map);
   --  Register Name as the environment name for Env. If Env takes the
   --  precedence for this name, add Name/its named env descriptor to
   --  Named_Envs_Needing_Update.

   procedure Update_Named_Envs
     (Context : Internal_Context; Named_Envs : NED_Maps.Map);
   --  For each named environment in Named_Envs, update Env_With_Precedence and
   --  do the necessary adjustments: relocate exiled entries, etc.

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   Kind_To_Node_Children_Count : constant array (Lkt_Node_Kind_Type) of Integer :=
     (Lkt_Lexer_Case_Rule_Cond_Alt => 2, 
Lkt_Lexer_Case_Rule_Default_Alt => 1, 
Lkt_Block_String_Line => 0, 
Lkt_Class_Qualifier_Absent => 0, 
Lkt_Class_Qualifier_Present => 0, 
Lkt_Grammar_Rule_Decl => 2, 
Lkt_Synthetic_Lexer_Decl => 0, 
Lkt_Node_Decl => 0, 
Lkt_Self_Decl => 0, 
Lkt_Enum_Lit_Decl => 1, 
Lkt_Field_Decl => 3, 
Lkt_Fun_Arg_Decl => 4, 
Lkt_Lambda_Arg_Decl => 3, 
Lkt_Dyn_Var_Decl => 2, 
Lkt_Match_Val_Decl => 2, 
Lkt_Val_Decl => 3, 
Lkt_Fun_Decl => 4, 
Lkt_Env_Spec_Decl => 2, 
Lkt_Generic_Decl => 2, 
Lkt_Grammar_Decl => 2, 
Lkt_Lexer_Decl => 2, 
Lkt_Lexer_Family_Decl => 2, 
Lkt_Synth_Arg_Decl => 0, 
Lkt_Synth_Fun_Decl => 0, 
Lkt_Any_Type_Decl => 0, 
Lkt_Enum_Class_Alt_Decl => 1, 
Lkt_Function_Type => 0, 
Lkt_Generic_Formal_Type_Decl => 2, 
Lkt_Class_Decl => 4, 
Lkt_Enum_Class_Decl => 5, 
Lkt_Enum_Type_Decl => 4, 
Lkt_Struct_Decl => 3, 
Lkt_Trait_Decl => 2, 
Lkt_Decl_Annotation => 2, 
Lkt_Decl_Annotation_Params => 1, 
Lkt_Dyn_Env_Wrapper => 0, 
Lkt_Elsif_Branch => 2, 
Lkt_Enum_Class_Case => 1, 
Lkt_Excludes_Null_Absent => 0, 
Lkt_Excludes_Null_Present => 0, 
Lkt_Any_Of => 2, 
Lkt_Array_Literal => 2, 
Lkt_Call_Expr => 2, 
Lkt_Logic_Predicate => 2, 
Lkt_Logic_Propagate_Call => 2, 
Lkt_Dot_Expr => 2, 
Lkt_Null_Cond_Dotted_Name => 2, 
Lkt_Bin_Op => 3, 
Lkt_Block_Expr => 2, 
Lkt_Cast_Expr => 3, 
Lkt_Error_On_Null => 1, 
Lkt_Generic_Instantiation => 2, 
Lkt_Grammar_Cut => 0, 
Lkt_Grammar_Discard => 1, 
Lkt_Grammar_Dont_Skip => 2, 
Lkt_Grammar_List => 4, 
Lkt_Grammar_Null => 1, 
Lkt_Grammar_Opt => 1, 
Lkt_Grammar_Opt_Error => 1, 
Lkt_Grammar_Opt_Error_Group => 1, 
Lkt_Grammar_Opt_Group => 1, 
Lkt_Grammar_Or_Expr => 1, 
Lkt_Grammar_Pick => 1, 
Lkt_Grammar_Implicit_Pick => 1, 
Lkt_Grammar_Predicate => 2, 
Lkt_Grammar_Rule_Ref => 1, 
Lkt_Grammar_Skip => 1, 
Lkt_Grammar_Stop_Cut => 1, 
Lkt_Parse_Node_Expr => 2, 
Lkt_Token_Lit => 0, 
Lkt_Token_No_Case_Lit => 1, 
Lkt_Token_Pattern_Concat => 2, 
Lkt_Token_Pattern_Lit => 0, 
Lkt_Token_Ref => 2, 
Lkt_Id => 0, 
Lkt_Def_Id => 0, 
Lkt_Module_Ref_Id => 0, 
Lkt_Ref_Id => 0, 
Lkt_If_Expr => 4, 
Lkt_Isa => 2, 
Lkt_Keep_Expr => 2, 
Lkt_Lambda_Expr => 3, 
Lkt_Big_Num_Lit => 0, 
Lkt_Char_Lit => 0, 
Lkt_Null_Lit => 1, 
Lkt_Num_Lit => 0, 
Lkt_Block_String_Lit => 1, 
Lkt_Single_Line_String_Lit => 0, 
Lkt_Pattern_Single_Line_String_Lit => 0, 
Lkt_Logic_Assign => 2, 
Lkt_Logic_Expr => 1, 
Lkt_Logic_Propagate => 2, 
Lkt_Logic_Unify => 2, 
Lkt_Match_Expr => 2, 
Lkt_Not_Expr => 1, 
Lkt_Paren_Expr => 1, 
Lkt_Raise_Expr => 2, 
Lkt_Subscript_Expr => 2, 
Lkt_Null_Cond_Subscript_Expr => 2, 
Lkt_Try_Expr => 2, 
Lkt_Un_Op => 2, 
Lkt_Full_Decl => 3, 
Lkt_Grammar_List_Sep => 2, 
Lkt_Import => 1, 
Lkt_Langkit_Root => 2, 
Lkt_Lexer_Case_Rule => 2, 
Lkt_Lexer_Case_Rule_Send => 2, 
Lkt_List_Kind_One => 0, 
Lkt_List_Kind_Zero => 0, 
Lkt_Base_Lexer_Case_Rule_Alt_List => -1, 
Lkt_Block_String_Line_List => -1, 
Lkt_Call_Expr_List => -1, 
Lkt_Decl_Annotation_List => -1, 
Lkt_Elsif_Branch_List => -1, 
Lkt_Enum_Class_Alt_Decl_List => -1, 
Lkt_Enum_Class_Case_List => -1, 
Lkt_Enum_Lit_Decl_List => -1, 
Lkt_Expr_List => -1, 
Lkt_Any_Of_List => -1, 
Lkt_Full_Decl_List => -1, 
Lkt_Decl_Block => -1, 
Lkt_Generic_Formal_Decl_List => -1, 
Lkt_Fun_Arg_Decl_List => -1, 
Lkt_Grammar_Expr_List => -1, 
Lkt_Grammar_Expr_List_List => -1, 
Lkt_Import_List => -1, 
Lkt_Lambda_Arg_Decl_List => -1, 
Lkt_Lkt_Node_List => -1, 
Lkt_Block_Decl_List => -1, 
Lkt_Match_Branch_List => -1, 
Lkt_Param_List => -1, 
Lkt_Ref_Id_List => -1, 
Lkt_Type_Ref_List => -1, 
Lkt_Isa_List => -1, 
Lkt_Match_Branch => 2, 
Lkt_Op_Amp => 0, 
Lkt_Op_And => 0, 
Lkt_Op_Div => 0, 
Lkt_Op_Eq => 0, 
Lkt_Op_Gt => 0, 
Lkt_Op_Gte => 0, 
Lkt_Op_Logic_And => 0, 
Lkt_Op_Logic_Or => 0, 
Lkt_Op_Lt => 0, 
Lkt_Op_Lte => 0, 
Lkt_Op_Minus => 0, 
Lkt_Op_Mult => 0, 
Lkt_Op_Ne => 0, 
Lkt_Op_Or => 0, 
Lkt_Op_Or_Int => 0, 
Lkt_Op_Plus => 0, 
Lkt_Param => 2, 
Lkt_Default_List_Type_Ref => 0, 
Lkt_Function_Type_Ref => 2, 
Lkt_Generic_Type_Ref => 2, 
Lkt_Simple_Type_Ref => 1, 
Lkt_Var_Bind => 2);
   --  For each AST node kind, this array gives the number of AST node children
   --  it has. For AST node lists, this is -1 as this number varies from one
   --  list instance to another.

   function First_Child_Index (Node : Bare_Lkt_Node) return Natural;
   --  Return the index of the first child Node has

   function Last_Child_Index (Node : Bare_Lkt_Node) return Natural;
   --  Return the index of the last child Node has, or 0 if there is no child

   function Children_Count (Node : Bare_Lkt_Node) return Natural;
   --  Return the number of children that Node has

   procedure Get_Child
     (Node            : Bare_Lkt_Node;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Bare_Lkt_Node);
   --  Return the Index'th child of node, storing it into Result.
   --
   --  Child indexing is 1-based. Store in Index_In_Bounds whether Node had
   --  such a child: if not (i.e. ``Index`` is out-of-bounds), set ``Result``
   --  to a null node.

   function Child
     (Node  : Bare_Lkt_Node;
      Index : Positive) return Bare_Lkt_Node;
   --  Return the Index'th child of Node, or null if Node has no such child

   function Children
     (Node : Bare_Lkt_Node) return Internal_Bare_Lkt_Node_Array;
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Children_Count pair, useful if you
   --  want the convenience of Ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Parents
     (Node      : Bare_Lkt_Node;
      With_Self : Boolean := True)
      return Bare_Lkt_Node_Array_Access;
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.

   function Parent (Node : Bare_Lkt_Node) return Bare_Lkt_Node;

   function Fetch_Sibling
     (Node   : Bare_Lkt_Node;
      Offset : Integer) return Bare_Lkt_Node;
   function Fetch_Sibling
     (Node   : Bare_Lkt_Node;
      E_Info : Internal_Entity_Info;
      Offset : Integer) return Internal_Entity;
   --  Assuming Node is the Nth child of its parent, return the (N + Offset)'th
   --  child of the same parent, or null/No_Entity if there is no such sibling.

   function Traverse
     (Node  : Bare_Lkt_Node;
      Visit : access function (Node : Bare_Lkt_Node) return Visit_Status)
      return Visit_Status;
   --  Given the parent node for a subtree, traverse all syntactic nodes of
   --  this tree, calling the given function on each node in prefix order (i.e.
   --  top-down). The order of traversing subtrees follows the order of
   --  declaration of the corresponding attributes in the grammar. The
   --  traversal is controlled as follows by the result returned by Visit:
   --
   --     Into   The traversal continues normally with the syntactic
   --            children of the node just processed.
   --
   --     Over   The children of the node just processed are skipped and
   --            excluded from the traversal, but otherwise processing
   --            continues elsewhere in the tree.
   --
   --     Stop   The entire traversal is immediately abandoned, and the
   --            original call to Traverse returns Stop.

   procedure Traverse
     (Node  : Bare_Lkt_Node;
      Visit : access function (Node : Bare_Lkt_Node)
                               return Visit_Status);
   --  This is the same as Traverse function except that no result is returned
   --  i.e. the Traverse function is called and the result is simply discarded.

   generic
      type Data_Type is private;
      Reset_After_Traversal : Boolean := False;
   function Traverse_With_Data
     (Node  : Bare_Lkt_Node;
      Visit : access function (Node : Bare_Lkt_Node;
                               Data : in out Data_Type)
                               return Visit_Status;
      Data  : in out Data_Type)
      return Visit_Status;
   --  This is the same as the first Traverse function except it accepts an
   --  argument that is passed to all Visit calls.
   --
   --  If Reset_After_Traversal is True, the Data formal is left unchanged when
   --  Traverse_With_Data returns no matter what Visit does. Visit can change
   --  it otherwise.

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : Bare_Lkt_Node) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : Bare_Lkt_Node;
      Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   function Lookup
     (Node : Bare_Lkt_Node;
      Sloc : Source_Location) return Bare_Lkt_Node;
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.

   function Compare
     (Self, Left, Right : Bare_Lkt_Node;
      Relation          : Comparison_Relation) return Boolean;
   --  If ``Left`` and ``Right`` don't belong to the same analysis units or if
   --  one of them is null, raise a ``Property_Error`` (use ``Self`` to provide
   --  error context). Otherwise, return the comparison of their starting
   --  source location according to Relation.

   -------------------
   -- Debug helpers --
   -------------------

   function Image (Value : Boolean) return String;
   --  Image for a Boolean, for debugging/logging purposes

   procedure Print
     (Node        : Bare_Lkt_Node;
      Show_Slocs  : Boolean;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia
     (Node        : Bare_Lkt_Node;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : Bare_Lkt_Node);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   -------------------------------
   -- Root AST node (internals) --
   -------------------------------

   type Initialization_State is
     (Uninitialized,
      Initialized,
      Raised_Property_Error);
   --  Initialization status:
   --
   --  * ``Uninitialized``: initialization still needed to get a value;
   --  * ``Initialized``: initialization completed, value is available;
   --  * ``Raise_X``: initialization raised exception ``X``, value will never
   --    be available.

   subtype Error_Initialization_State is
     Initialization_State range
       Raised_Property_Error
       .. Raised_Property_Error;

   function Initialization_Error
     (Exc : Ada.Exceptions.Exception_Occurrence)
      return Error_Initialization_State;
   --  Assuming that ``Exc`` is an exception allowed to be raised in
   --  properties, return the corresponding initialization state.

   procedure Reraise_Initialization_Error
     (Node    : Bare_Lkt_Node;
      State   : Error_Initialization_State;
      Message : String);
   --  Raise the exception that ``State`` describes. ``Node`` and ``Message``
   --  are used to add contextual information to the exception.

   type Root_Node_Record (Kind : Lkt_Node_Kind_Type) is record
      Parent : Bare_Lkt_Node;
      --  Reference to the parent node, or null if this is the root one

      Unit : Internal_Unit;
      --  Reference to the analysis unit that owns this node

      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      --  Reference to the start and end token that constitutes this node. If
      --  this node is a ghost, Token_Start_Index is the token that this AST
      --  node relates to and Token_End_Index is No_Token_Index. Otherwise,
      --  both tokens are inclusive, i.e. they both belong to this node.

      Self_Env : Lexical_Env;
      --  Hold the environment this node defines, or the parent environment
      --  otherwise.

      Last_Attempted_Child : Integer;
      --  0-based index for the last child we tried to parse for this node. -1
      --  if parsing for all children was successful.

      

      
         



         


            case Kind is
                  when Lkt_Base_Lexer_Case_Rule_Alt =>
                     
         



         


            case Kind is
                  when Lkt_Lexer_Case_Rule_Cond_Alt_Range =>
                     
         


            Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : aliased Bare_Ref_Id_List :=
               No_Bare_Lkt_Node;
            Lexer_Case_Rule_Cond_Alt_F_Send : aliased Bare_Lexer_Case_Rule_Send :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Lexer_Case_Rule_Default_Alt_Range =>
                     
         


            Lexer_Case_Rule_Default_Alt_F_Send : aliased Bare_Lexer_Case_Rule_Send :=
               No_Bare_Lkt_Node;

         



      
               when others => null;
            end case;

      
                  when Lkt_Block_String_Line_Range =>
                     
         



         



            null;
      
                  when Lkt_Class_Qualifier =>
                     
         



         


            case Kind is
                  when Lkt_Class_Qualifier_Absent_Range =>
                     
         



         



            null;
      
                  when Lkt_Class_Qualifier_Present_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Lkt_Decl =>
                     
         



         


            case Kind is
                  when Lkt_Base_Grammar_Rule_Decl =>
                     
         



         


            case Kind is
                  when Lkt_Grammar_Rule_Decl_Range =>
                     
         


            Grammar_Rule_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Grammar_Rule_Decl_F_Expr : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Synthetic_Lexer_Decl_Range =>
                     
         


            Synthetic_Lexer_Decl_Sym : aliased Symbol_Type :=
               No_Symbol;

         



      
               when others => null;
            end case;

      
                  when Lkt_Base_Val_Decl =>
                     
         



         


            case Kind is
                  when Lkt_Node_Decl_Range =>
                     
         



         



            null;
      
                  when Lkt_Self_Decl_Range =>
                     
         



         



            null;
      
                  when Lkt_User_Val_Decl =>
                     
         



         


            case Kind is
                  when Lkt_Enum_Lit_Decl_Range =>
                     
         


            Enum_Lit_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Explicitly_Typed_Decl =>
                     
         



         


            case Kind is
                  when Lkt_Component_Decl =>
                     
         



         


            case Kind is
                  when Lkt_Field_Decl_Range =>
                     
         


            Field_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Field_Decl_F_Decl_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Field_Decl_F_Default_Val : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Fun_Arg_Decl_Range =>
                     
         


            Fun_Arg_Decl_F_Decl_Annotations : aliased Bare_Decl_Annotation_List :=
               No_Bare_Lkt_Node;
            Fun_Arg_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Fun_Arg_Decl_F_Decl_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Fun_Arg_Decl_F_Default_Val : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Lambda_Arg_Decl_Range =>
                     
         


            Lambda_Arg_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Lambda_Arg_Decl_F_Decl_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Lambda_Arg_Decl_F_Default_Val : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Lambda_Arg_Decl_Type_Var : aliased Logic_Var_Record :=
               Null_Var_Record;

         



      
               when others => null;
            end case;

      
                  when Lkt_Dyn_Var_Decl_Range =>
                     
         


            Dyn_Var_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Dyn_Var_Decl_F_Decl_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Match_Val_Decl_Range =>
                     
         


            Match_Val_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Match_Val_Decl_F_Decl_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Val_Decl_Range =>
                     
         


            Val_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Val_Decl_F_Decl_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Val_Decl_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
               when others => null;
            end case;

      
                  when Lkt_Fun_Decl_Range =>
                     
         


            Fun_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Fun_Decl_F_Args : aliased Bare_Fun_Arg_Decl_List :=
               No_Bare_Lkt_Node;
            Fun_Decl_F_Return_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Fun_Decl_F_Body : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
               when others => null;
            end case;

      
               when others => null;
            end case;

      
                  when Lkt_Env_Spec_Decl_Range =>
                     
         


            Env_Spec_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Env_Spec_Decl_F_Actions : aliased Bare_Call_Expr_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Generic_Decl_Range =>
                     
         


            Generic_Decl_F_Generic_Formal_Decls : aliased Bare_Generic_Formal_Decl_List :=
               No_Bare_Lkt_Node;
            Generic_Decl_F_Decl : aliased Bare_Decl :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Decl_Range =>
                     
         


            Grammar_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Grammar_Decl_F_Rules : aliased Bare_Full_Decl_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Lexer_Decl_Range =>
                     
         


            Lexer_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Lexer_Decl_F_Rules : aliased Bare_Lkt_Node_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Lexer_Family_Decl_Range =>
                     
         


            Lexer_Family_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Lexer_Family_Decl_F_Rules : aliased Bare_Full_Decl_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Synth_Arg_Decl_Range =>
                     
         



         



            null;
      
                  when Lkt_Synth_Fun_Decl_Range =>
                     
         


            Synth_Fun_Decl_Args : aliased Internal_Formal_Param_Array_Access :=
               No_Internal_Formal_Param_Array_Type;
            Synth_Fun_Decl_Return_Type : aliased Internal_Entity_Type_Decl :=
               No_Entity_Type_Decl;

         



      
                  when Lkt_Type_Decl =>
                     
         



         


            case Kind is
                  when Lkt_Any_Type_Decl_Range =>
                     
         



         



            null;
      
                  when Lkt_Enum_Class_Alt_Decl_Range =>
                     
         


            Enum_Class_Alt_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Function_Type_Range =>
                     
         


            Function_Type_Args : aliased Internal_Entity_Type_Decl_Array_Access :=
               No_Internal_Entity_Type_Decl_Array_Type;
            Function_Type_Return_Type : aliased Internal_Entity_Type_Decl :=
               No_Entity_Type_Decl;
            Function_Type_Origin : aliased Internal_Entity_Decl :=
               No_Entity_Decl;

         



      
                  when Lkt_Generic_Formal_Type_Decl_Range =>
                     
         


            Generic_Formal_Type_Decl_F_Has_Class : aliased Bare_Class_Qualifier :=
               No_Bare_Lkt_Node;
            Generic_Formal_Type_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Named_Type_Decl =>
                     
         



         


            case Kind is
                  when Lkt_Basic_Class_Decl =>
                     
         


            Basic_Class_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Basic_Class_Decl_F_Syn_Base_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Basic_Class_Decl_F_Traits : aliased Bare_Type_Ref_List :=
               No_Bare_Lkt_Node;

         


            case Kind is
                  when Lkt_Class_Decl_Range =>
                     
         


            Class_Decl_F_Decls : aliased Bare_Decl_Block :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Enum_Class_Decl_Range =>
                     
         


            Enum_Class_Decl_F_Branches : aliased Bare_Enum_Class_Case_List :=
               No_Bare_Lkt_Node;
            Enum_Class_Decl_F_Decls : aliased Bare_Decl_Block :=
               No_Bare_Lkt_Node;

         



      
               when others => null;
            end case;

      
                  when Lkt_Enum_Type_Decl_Range =>
                     
         


            Enum_Type_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Enum_Type_Decl_F_Traits : aliased Bare_Type_Ref_List :=
               No_Bare_Lkt_Node;
            Enum_Type_Decl_F_Literals : aliased Bare_Enum_Lit_Decl_List :=
               No_Bare_Lkt_Node;
            Enum_Type_Decl_F_Decls : aliased Bare_Decl_Block :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Struct_Decl_Range =>
                     
         


            Struct_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Struct_Decl_F_Traits : aliased Bare_Type_Ref_List :=
               No_Bare_Lkt_Node;
            Struct_Decl_F_Decls : aliased Bare_Decl_Block :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Trait_Decl_Range =>
                     
         


            Trait_Decl_F_Syn_Name : aliased Bare_Def_Id :=
               No_Bare_Lkt_Node;
            Trait_Decl_F_Decls : aliased Bare_Decl_Block :=
               No_Bare_Lkt_Node;

         



      
               when others => null;
            end case;

      
               when others => null;
            end case;

      
               when others => null;
            end case;

      
                  when Lkt_Decl_Annotation_Range =>
                     
         


            Decl_Annotation_F_Name : aliased Bare_Id :=
               No_Bare_Lkt_Node;
            Decl_Annotation_F_Params : aliased Bare_Decl_Annotation_Params :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Decl_Annotation_Params_Range =>
                     
         


            Decl_Annotation_Params_F_Params : aliased Bare_Param_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Dyn_Env_Wrapper_Range =>
                     
         


            Dyn_Env_Wrapper_Names : aliased Symbol_Type_Array_Access :=
               No_Symbol_Type_Array_Type;
            Dyn_Env_Wrapper_Types : aliased Internal_Entity_Type_Decl_Array_Access :=
               No_Internal_Entity_Type_Decl_Array_Type;
            Bare_Dyn_Env_Wrapper_Lf_State_Dynenvwrapper_Instantiation_Env : aliased Initialization_State :=
               Uninitialized;
            Bare_Dyn_Env_Wrapper_Lf_Stg_Dynenvwrapper_Instantiation_Env : aliased Lexical_Env :=
               Empty_Env;

         



      
                  when Lkt_Elsif_Branch_Range =>
                     
         


            Elsif_Branch_F_Cond_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Elsif_Branch_F_Then_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Enum_Class_Case_Range =>
                     
         


            Enum_Class_Case_F_Decls : aliased Bare_Enum_Class_Alt_Decl_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Excludes_Null =>
                     
         



         


            case Kind is
                  when Lkt_Excludes_Null_Absent_Range =>
                     
         



         



            null;
      
                  when Lkt_Excludes_Null_Present_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Lkt_Expr =>
                     
         


            Expr_Expected_Type_Var : aliased Logic_Var_Record :=
               Null_Var_Record;
            Expr_Actual_Type_Var : aliased Logic_Var_Record :=
               Null_Var_Record;
            Expr_Generic_Func_Type_Var : aliased Logic_Var_Record :=
               Null_Var_Record;

         


            case Kind is
                  when Lkt_Any_Of_Range =>
                     
         


            Any_Of_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Any_Of_F_Values : aliased Bare_Any_Of_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Array_Literal_Range =>
                     
         


            Array_Literal_F_Exprs : aliased Bare_Expr_List :=
               No_Bare_Lkt_Node;
            Array_Literal_F_Element_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Array_Literal_Expected_Exprs_Type_Var : aliased Logic_Var_Record :=
               Null_Var_Record;
            Array_Literal_Actual_Element_Type : aliased Logic_Var_Record :=
               Null_Var_Record;

         



      
                  when Lkt_Base_Call_Expr =>
                     
         


            Base_Call_Expr_F_Name : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Base_Call_Expr_F_Args : aliased Bare_Param_List :=
               No_Bare_Lkt_Node;

         


            case Kind is
                  when Lkt_Call_Expr_Range =>
                     
         



         



            null;
      
                  when Lkt_Logic_Call_Expr =>
                     
         



         


            case Kind is
                  when Lkt_Logic_Predicate_Range =>
                     
         



         



            null;
      
                  when Lkt_Logic_Propagate_Call_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
               when others => null;
            end case;

      
                  when Lkt_Base_Dot_Expr =>
                     
         


            Base_Dot_Expr_F_Prefix : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Base_Dot_Expr_F_Suffix : aliased Bare_Ref_Id :=
               No_Bare_Lkt_Node;

         


            case Kind is
                  when Lkt_Dot_Expr_Range =>
                     
         



         



            null;
      
                  when Lkt_Null_Cond_Dotted_Name_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Lkt_Bin_Op_Range =>
                     
         


            Bin_Op_F_Left : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Bin_Op_F_Op : aliased Bare_Op :=
               No_Bare_Lkt_Node;
            Bin_Op_F_Right : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Block_Expr_Range =>
                     
         


            Block_Expr_F_Val_Defs : aliased Bare_Block_Decl_List :=
               No_Bare_Lkt_Node;
            Block_Expr_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Cast_Expr_Range =>
                     
         


            Cast_Expr_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Cast_Expr_F_Excludes_Null : aliased Bare_Excludes_Null :=
               No_Bare_Lkt_Node;
            Cast_Expr_F_Dest_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Error_On_Null_Range =>
                     
         


            Error_On_Null_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Generic_Instantiation_Range =>
                     
         


            Generic_Instantiation_F_Name : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Generic_Instantiation_F_Args : aliased Bare_Type_Ref_List :=
               No_Bare_Lkt_Node;
            Generic_Instantiation_Rebinded_Var : aliased Logic_Var_Record :=
               Null_Var_Record;

         



      
                  when Lkt_Grammar_Expr =>
                     
         



         


            case Kind is
                  when Lkt_Grammar_Cut_Range =>
                     
         



         



            null;
      
                  when Lkt_Grammar_Discard_Range =>
                     
         


            Grammar_Discard_F_Expr : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Dont_Skip_Range =>
                     
         


            Grammar_Dont_Skip_F_Expr : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;
            Grammar_Dont_Skip_F_Dont_Skip : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_List_Range =>
                     
         


            Grammar_List_F_List_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Grammar_List_F_Kind : aliased Bare_List_Kind :=
               No_Bare_Lkt_Node;
            Grammar_List_F_Expr : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;
            Grammar_List_F_Sep : aliased Bare_Grammar_List_Sep :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Null_Range =>
                     
         


            Grammar_Null_F_Name : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Opt_Range =>
                     
         


            Grammar_Opt_F_Expr : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Opt_Error_Range =>
                     
         


            Grammar_Opt_Error_F_Expr : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Opt_Error_Group_Range =>
                     
         


            Grammar_Opt_Error_Group_F_Expr : aliased Bare_Grammar_Expr_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Opt_Group_Range =>
                     
         


            Grammar_Opt_Group_F_Expr : aliased Bare_Grammar_Expr_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Or_Expr_Range =>
                     
         


            Grammar_Or_Expr_F_Sub_Exprs : aliased Bare_Grammar_Expr_List_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Pick_Range =>
                     
         


            Grammar_Pick_F_Exprs : aliased Bare_Grammar_Expr_List :=
               No_Bare_Lkt_Node;

         


            case Kind is
                  when Lkt_Grammar_Implicit_Pick_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Lkt_Grammar_Predicate_Range =>
                     
         


            Grammar_Predicate_F_Expr : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;
            Grammar_Predicate_F_Prop_Ref : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Rule_Ref_Range =>
                     
         


            Grammar_Rule_Ref_F_Node_Name : aliased Bare_Ref_Id :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Skip_Range =>
                     
         


            Grammar_Skip_F_Name : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_Stop_Cut_Range =>
                     
         


            Grammar_Stop_Cut_F_Expr : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Parse_Node_Expr_Range =>
                     
         


            Parse_Node_Expr_F_Node_Name : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Parse_Node_Expr_F_Sub_Exprs : aliased Bare_Grammar_Expr_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Token_Lit_Range =>
                     
         



         



            null;
      
                  when Lkt_Token_No_Case_Lit_Range =>
                     
         


            Token_No_Case_Lit_F_Lit : aliased Bare_Token_Lit :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Token_Pattern_Concat_Range =>
                     
         


            Token_Pattern_Concat_F_Left : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;
            Token_Pattern_Concat_F_Right : aliased Bare_Token_Pattern_Lit :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Token_Pattern_Lit_Range =>
                     
         



         



            null;
      
                  when Lkt_Token_Ref_Range =>
                     
         


            Token_Ref_F_Token_Name : aliased Bare_Ref_Id :=
               No_Bare_Lkt_Node;
            Token_Ref_F_Expr : aliased Bare_Token_Lit :=
               No_Bare_Lkt_Node;

         



      
               when others => null;
            end case;

      
                  when Lkt_Id_Range =>
                     
         



         


            case Kind is
                  when Lkt_Def_Id_Range =>
                     
         



         



            null;
      
                  when Lkt_Module_Ref_Id_Range =>
                     
         



         



            null;
      
                  when Lkt_Ref_Id_Range =>
                     
         


            Ref_Id_Ref_Var : aliased Logic_Var_Record :=
               Null_Var_Record;

         



      
               when others => null;
            end case;

      
                  when Lkt_If_Expr_Range =>
                     
         


            If_Expr_F_Cond_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            If_Expr_F_Then_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            If_Expr_F_Alternatives : aliased Bare_Elsif_Branch_List :=
               No_Bare_Lkt_Node;
            If_Expr_F_Else_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            If_Expr_Expected_Branch_Type_Var : aliased Logic_Var_Record :=
               Null_Var_Record;

         



      
                  when Lkt_Isa_Range =>
                     
         


            Isa_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Isa_F_Dest_Type : aliased Bare_Isa_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Keep_Expr_Range =>
                     
         


            Keep_Expr_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Keep_Expr_F_Keep_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Keep_Expr_Array_Element_Type : aliased Logic_Var_Record :=
               Null_Var_Record;

         



      
                  when Lkt_Lambda_Expr_Range =>
                     
         


            Lambda_Expr_F_Params : aliased Bare_Lambda_Arg_Decl_List :=
               No_Bare_Lkt_Node;
            Lambda_Expr_F_Return_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Lambda_Expr_F_Body : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Lit =>
                     
         



         


            case Kind is
                  when Lkt_Big_Num_Lit_Range =>
                     
         



         



            null;
      
                  when Lkt_Char_Lit_Range =>
                     
         



         



            null;
      
                  when Lkt_Null_Lit_Range =>
                     
         


            Null_Lit_F_Dest_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Num_Lit_Range =>
                     
         



         



            null;
      
                  when Lkt_String_Lit =>
                     
         



         


            case Kind is
                  when Lkt_Block_String_Lit_Range =>
                     
         


            Block_String_Lit_F_Lines : aliased Bare_Block_String_Line_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Single_Line_String_Lit_Range =>
                     
         



         


            case Kind is
                  when Lkt_Pattern_Single_Line_String_Lit_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
               when others => null;
            end case;

      
               when others => null;
            end case;

      
                  when Lkt_Logic_Assign_Range =>
                     
         


            Logic_Assign_F_Dest_Var : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Logic_Assign_F_Value : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Logic_Expr_Range =>
                     
         


            Logic_Expr_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Logic_Propagate_Range =>
                     
         


            Logic_Propagate_F_Dest_Var : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Logic_Propagate_F_Call : aliased Bare_Logic_Propagate_Call :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Logic_Unify_Range =>
                     
         


            Logic_Unify_F_Lhs : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Logic_Unify_F_Rhs : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Match_Expr_Range =>
                     
         


            Match_Expr_F_Match_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Match_Expr_F_Branches : aliased Bare_Match_Branch_List :=
               No_Bare_Lkt_Node;
            Match_Expr_Expected_Branch_Type_Var : aliased Logic_Var_Record :=
               Null_Var_Record;

         



      
                  when Lkt_Not_Expr_Range =>
                     
         


            Not_Expr_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Paren_Expr_Range =>
                     
         


            Paren_Expr_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Raise_Expr_Range =>
                     
         


            Raise_Expr_F_Dest_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;
            Raise_Expr_F_Except_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Subscript_Expr_Range =>
                     
         


            Subscript_Expr_F_Prefix : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Subscript_Expr_F_Index : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         


            case Kind is
                  when Lkt_Null_Cond_Subscript_Expr_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Lkt_Try_Expr_Range =>
                     
         


            Try_Expr_F_Try_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Try_Expr_F_Or_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Try_Expr_Expected_Expr_Type_Var : aliased Logic_Var_Record :=
               Null_Var_Record;

         



      
                  when Lkt_Un_Op_Range =>
                     
         


            Un_Op_F_Op : aliased Bare_Op :=
               No_Bare_Lkt_Node;
            Un_Op_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
               when others => null;
            end case;

      
                  when Lkt_Full_Decl_Range =>
                     
         


            Full_Decl_F_Doc : aliased Bare_String_Lit :=
               No_Bare_Lkt_Node;
            Full_Decl_F_Decl_Annotations : aliased Bare_Decl_Annotation_List :=
               No_Bare_Lkt_Node;
            Full_Decl_F_Decl : aliased Bare_Decl :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Grammar_List_Sep_Range =>
                     
         


            Grammar_List_Sep_F_Token : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;
            Grammar_List_Sep_F_Extra : aliased Bare_Id :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Import_Range =>
                     
         


            Import_F_Name : aliased Bare_Module_Ref_Id :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Langkit_Root_Range =>
                     
         


            Langkit_Root_F_Imports : aliased Bare_Import_List :=
               No_Bare_Lkt_Node;
            Langkit_Root_F_Decls : aliased Bare_Full_Decl_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Lexer_Case_Rule_Range =>
                     
         


            Lexer_Case_Rule_F_Expr : aliased Bare_Grammar_Expr :=
               No_Bare_Lkt_Node;
            Lexer_Case_Rule_F_Alts : aliased Bare_Base_Lexer_Case_Rule_Alt_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Lexer_Case_Rule_Send_Range =>
                     
         


            Lexer_Case_Rule_Send_F_Sent : aliased Bare_Ref_Id :=
               No_Bare_Lkt_Node;
            Lexer_Case_Rule_Send_F_Match_Size : aliased Bare_Num_Lit :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_List_Kind =>
                     
         



         


            case Kind is
                  when Lkt_List_Kind_One_Range =>
                     
         



         



            null;
      
                  when Lkt_List_Kind_Zero_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Lkt_Lkt_Node_Base_List =>
                     
         

            Count : Natural;
            Nodes : Alloc_AST_List_Array.Element_Array_Access;


         


            case Kind is
                  when Lkt_Base_Lexer_Case_Rule_Alt_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Block_String_Line_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Call_Expr_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Decl_Annotation_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Elsif_Branch_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Enum_Class_Alt_Decl_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Enum_Class_Case_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Enum_Lit_Decl_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Expr_List_Range =>
                     
         



         


            case Kind is
                  when Lkt_Any_Of_List_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Lkt_Full_Decl_List_Range =>
                     
         



         


            case Kind is
                  when Lkt_Decl_Block_Range =>
                     
         



         



            null;
      
                  when Lkt_Generic_Formal_Decl_List_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Lkt_Fun_Arg_Decl_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Grammar_Expr_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Grammar_Expr_List_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Import_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Lambda_Arg_Decl_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Lkt_Node_List_Range =>
                     
         



         


            case Kind is
                  when Lkt_Block_Decl_List_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Lkt_Match_Branch_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Param_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Ref_Id_List_Range =>
                     
         



         



            null;
      
                  when Lkt_Type_Ref_List_Range =>
                     
         



         


            case Kind is
                  when Lkt_Isa_List_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
               when others => null;
            end case;

      
                  when Lkt_Match_Branch_Range =>
                     
         


            Match_Branch_F_Decl : aliased Bare_Match_Val_Decl :=
               No_Bare_Lkt_Node;
            Match_Branch_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Op =>
                     
         



         


            case Kind is
                  when Lkt_Op_Amp_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_And_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Div_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Eq_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Gt_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Gte_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Logic_And_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Logic_Or_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Lt_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Lte_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Minus_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Mult_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Ne_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Or_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Or_Int_Range =>
                     
         



         



            null;
      
                  when Lkt_Op_Plus_Range =>
                     
         



         



            null;
      
               when others => null;
            end case;

      
                  when Lkt_Param_Range =>
                     
         


            Param_F_Name : aliased Bare_Ref_Id :=
               No_Bare_Lkt_Node;
            Param_F_Value : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Type_Ref =>
                     
         


            Type_Ref_Type_Var : aliased Logic_Var_Record :=
               Null_Var_Record;

         


            case Kind is
                  when Lkt_Default_List_Type_Ref_Range =>
                     
         



         



            null;
      
                  when Lkt_Function_Type_Ref_Range =>
                     
         


            Function_Type_Ref_F_Args_Types : aliased Bare_Type_Ref_List :=
               No_Bare_Lkt_Node;
            Function_Type_Ref_F_Return_Type : aliased Bare_Type_Ref :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Generic_Type_Ref_Range =>
                     
         


            Generic_Type_Ref_F_Type_Name : aliased Bare_Expr :=
               No_Bare_Lkt_Node;
            Generic_Type_Ref_F_Params : aliased Bare_Type_Ref_List :=
               No_Bare_Lkt_Node;

         



      
                  when Lkt_Simple_Type_Ref_Range =>
                     
         


            Simple_Type_Ref_F_Type_Name : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
               when others => null;
            end case;

      
                  when Lkt_Var_Bind_Range =>
                     
         


            Var_Bind_F_Name : aliased Bare_Ref_Id :=
               No_Bare_Lkt_Node;
            Var_Bind_F_Expr : aliased Bare_Expr :=
               No_Bare_Lkt_Node;

         



      
               when others => null;
            end case;

      
   end record;

   procedure Initialize
     (Self              : Bare_Lkt_Node;
      Kind              : Lkt_Node_Kind_Type;
      Unit              : Internal_Unit;
      Token_Start_Index : Token_Index;
      Token_End_Index   : Token_Index;
      Parent            : Bare_Lkt_Node := null;
      Self_Env          : Lexical_Env := AST_Envs.Empty_Env);
   --  Helper for parsers, to initialize a freshly allocated node

   type PLE_Unit_State is record
      Named_Envs_Needing_Update : NED_Maps.Map;
      --  Set of named env entries whose Env_With_Precedence needs to be
      --  updated.
   end record;
   --  State of PLE on a specific unit

   type PLE_Unit_State_Access is access all PLE_Unit_State;

   type PLE_Node_State is record
      Unit_State : PLE_Unit_State_Access;
      --  State of PLE on the unit that owns this node

      Current_Env : Lexical_Env;
      --  Current environment when processing the node: initially inheritted
      --  from the Current_Env of the parent node (or Root_Scope on the root
      --  node), SetInitialEnv actions can change this.
      --
      --  Other environment actions such as AddEnv or AddToEnv can use this.

      Current_NED : Named_Env_Descriptor_Access;
      --  If the current environment was looked up by name, reference to the
      --  named environment descriptor. Null otherwise.
   end record;
   --  State of PLE on a specific node

   procedure Use_Direct_Env (State : in out PLE_Node_State; Env : Lexical_Env);
   --  Change State so that the current environment is Env, and record that it
   --  was *not* looked up by name.

   procedure Use_Named_Env
     (State   : in out PLE_Node_State;
      Context : Internal_Context;
      Name    : Symbol_Type);
   --  Change State so that the current environment comes from the named
   --  environment looked up with Name.

   procedure Set_Initial_Env
     (Self         : Bare_Lkt_Node;
      State        : in out PLE_Node_State;
      Env          : Internal_Designated_Env;
      DSL_Location : String);
   --  Helper for ``Populate_Lexical_Env``: fetch the initial environment for
   --  ``Self`` according to ``Env`` and update ``State`` accordingly.

   procedure Add_To_Env
     (Self         : Bare_Lkt_Node;
      State        : PLE_Node_State;
      Key          : Symbol_Type;
      Value        : Bare_Lkt_Node;
      Md           : Internal_Metadata;
      Resolver     : Entity_Resolver;
      Dest_Env     : Internal_Designated_Env;
      DSL_Location : String);
   --  Helper for Populate_Lexical_Env: insert the Key/Value/MD/Resolver entry
   --  in the appropriate lexical env.
   --
   --  The destination environment is:
   --
   --  * If Dest_Env_Name is not null, this is the corresponding named
   --    environment.
   --
   --  * Otherwise, use Dest_Env_Fallback if is not the empty environment.
   --
   --  * Finally, use State's current environment.
   --
   --  If the destination environment is foreign and not fetched from its name
   --  while DSL_Location is not empty, raise a Property_Error.

   procedure Ref_Env
     (Self                : Bare_Lkt_Node;
      Dest_Env            : Lexical_Env;
      Ref_Env_Nodes       : in out Bare_Lkt_Node_Array_Access;
      Resolver            : Lexical_Env_Resolver;
      Kind                : Ref_Kind;
      Cats                : Ref_Categories;
      Shed_Rebindings     : Boolean);
   --  Helper for Populate_Lexical_Env: add referenced environments to
   --  Dest_Env. Calling this takes an ownership share for Ref_Env_Nodes.

   procedure Add_Env
     (Self              : Bare_Lkt_Node;
      State             : in out PLE_Node_State;
      No_Parent         : Boolean;
      Transitive_Parent : Boolean;
      Names             : in out Symbol_Type_Array_Access);
   --  Helper for Populate_Lexical_Env: create a new environment for Self, and
   --  update State accordingly.
   --
   --  State and No_Parent participate to the computation of the parent for
   --  this new environment. Transitive_Parent is directly forwarded to the
   --  lexical environment constructor.
   --
   --  If Names is not null, this also registers the new environment as a named
   --  env for all the given names. For PLE code brevity, Add_Env takes care of
   --  freeing Names before returning.

   procedure Pre_Env_Actions
     (Self            : Bare_Lkt_Node;
      State           : in out PLE_Node_State;
      Add_To_Env_Only : Boolean := False);
   --  Internal procedure that will execute all necessary lexical env actions
   --  for Node. This is meant to be called by Populate_Lexical_Env, and not by
   --  the user.

   procedure Post_Env_Actions
     (Self : Bare_Lkt_Node; State : in out PLE_Node_State);
   --  Internal procedure that will execute all post add to env actions for
   --  Node. This is meant to be called by Populate_Lexical_Env.

   function Get_Symbol (Node : Bare_Lkt_Node) return Symbol_Type
      with Pre => Node = null or else Is_Token_Node (Node);
   --  Assuming Node is a token node, return the corresponding symbol for the
   --  token it contains.

   function Image (Self : Symbol_Type) return String_Type;
   --  Transform a Symbol into an internal String

   function Text (Node : Bare_Lkt_Node) return Text_Type;
   --  Retun the fragment of text from which Node was parsed

   ------------------------------
   -- Root AST node properties --
   ------------------------------


   -----------------------
   -- Generic list type --
   -----------------------


   function Length (Node : Bare_Lkt_Node_Base_List) return Natural;

   function Children
     (Node : Bare_Lkt_Node) return Bare_Lkt_Node_Array_Access;
   --  Return an array containing all the children of Node.
   --  This is an alternative to the Child/Children_Count pair, useful if you
   --  want the convenience of ada arrays, and you don't care about the small
   --  performance hit of creating an array.

   function Item
     (Node  : Bare_Lkt_Node_Base_List;
      Index : Positive) return Bare_Lkt_Node renames Child;

   function Get
     (Self    : Bare_Lkt_Node;
      Node    : Bare_Lkt_Node_Base_List;
      Index   : Integer;
      Or_Null : Boolean := False) return Bare_Lkt_Node;
   --  When Index is positive, return the Index'th element in T. Otherwise,
   --  return the element at index (Size - Index - 1). Index is zero-based.
   --
   --  ``Self`` is used to provide context to the ``Property_Error`` that is
   --  raised when the index is invalid.

   procedure Free_User_Fields (Node : Bare_Lkt_Node);
   --  Free resources associated to user fields in ``Node``

   procedure Set_Parents (Node, Parent : Bare_Lkt_Node);
   --  Set Node.Parent to Parent, and initialize recursively the parent of all
   --  child nodes.

   procedure Destroy (Node : Bare_Lkt_Node);
   --  Free the resources allocated to this node and all its children

   --------------------------------------
   -- Environments handling (internal) --
   --------------------------------------

   function Create_Static_Lexical_Env
     (Parent            : Lexical_Env;
      Node              : Bare_Lkt_Node;
      Sym_Table         : Symbol_Table;
      Transitive_Parent : Boolean := False) return Lexical_Env;
   --  Wrapper around AST_Envs.Create_Lexical_Env. Create the environment and,
   --  if Node is not null, register the result for destruction in Node's
   --  analysis unit.

   function Get
     (Self  : Bare_Lkt_Node;
      A     : AST_Envs.Entity_Array;
      Index : Integer) return Internal_Entity;
   --  Simple getter that raises a ``Property_Error`` on out-of-bound accesses
   --  (using ``Self`` to provide context for this error). Useful for code
   --  generation.

   function Group
     (Envs   : Lexical_Env_Array_Access;
      Env_Md : Internal_Metadata := No_Metadata) return Lexical_Env;
   --  Convenience wrapper for uniform types handling in code generation

   package Bare_Lkt_Node_Vectors is
      new Liblktlang_Support.Vectors (Bare_Lkt_Node);

   function Is_Visible_From
     (Self                     : Bare_Lkt_Node;
      Referenced_Env, Base_Env : Lexical_Env) return Boolean;
   --  Return whether the unit that ``Referenced_Env`` belongs to is visible
   --  from the unit that Base_Env belongs to. If at least one of these two
   --  lexical environments does not belong to a particular analysis unit, this
   --  raises a ``Property_Error``.
   --
   --  ``Self`` is used to give context to the error in case of failure.

   function Populate_Lexical_Env (Node : Bare_Lkt_Node) return Boolean;
   --  Populate the lexical environment for node and all its children. Return
   --  whether a Property_Error error occurred in the process.

   -----------------------------------
   -- Lexical utilities (internals) --
   -----------------------------------

   function Token
     (Node  : Bare_Lkt_Node;
      Index : Token_Index) return Token_Reference;
   --  Helper for properties. This is used to turn token indexes as stored in
   --  AST nodes into Token_Reference values.

   function Stored_Token
     (Node  : Bare_Lkt_Node;
      Token : Token_Reference) return Token_Index;
   --  Helper for properties. This is used to turn a Token_Reference value into
   --  a Token_Index value that can be stored as a field in Node. This raises a
   --  Property_Error if Node and Token don't belong to the same analysis unit
   --  or if Token is actually a Trivia.

   type Bare_Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : Bare_Lkt_Node;
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an node or a token

   package Bare_Children_Record_Vectors is new Ada.Containers.Vectors
     (Positive, Bare_Child_Record);

   subtype Bare_Children_Vector is Bare_Children_Record_Vectors.Vector;

   function Children_And_Trivia
     (Node : Bare_Lkt_Node) return Bare_Children_Vector;
   --  Implementation for Analysis.Children_And_Trivia

      

   



         



 function Lkt_Node_P_Root_Get
   
  (Node : Bare_Lkt_Node
      ; Entity_Name : Symbol_Type
  )

   return Internal_Entity_Decl
   ;


         



 function Lkt_Node_P_Get_Builtin_Type
   
  (Node : Bare_Lkt_Node
      ; Entity_Name : Symbol_Type
  )

   return Internal_Entity_Named_Type_Decl
   ;


         



 function Lkt_Node_P_Get_Builtin_Gen_Decl
   
  (Node : Bare_Lkt_Node
      ; Entity_Name : Symbol_Type
  )

   return Internal_Entity_Generic_Decl
   ;


         



 function Lkt_Node_P_Basic_Trait_Gen
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl
   ;
--  Unit method. Return the ``BasicTrait`` builtin generic trait.

         



 function Lkt_Node_P_Basic_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Trait_Decl
   ;
--  Unit method. Return the ``BasicTrait`` builtin trait.

         



 function Lkt_Node_P_Node_Gen_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl
   ;
--  Unit method. Return the ``Node`` builtin generic trait.

         



 function Lkt_Node_P_Node_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Trait_Decl
   ;
--  Unit method. Return the ``Node`` builtin trait.

         



 function Lkt_Node_P_Indexable_Gen_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl
   ;
--  Unit method. Return the ``Node`` builtin generic trait.

         



 function Lkt_Node_P_Indexable_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Trait_Decl
   ;
--  Unit method. Return the ``Node`` builtin trait.

         



 function Lkt_Node_P_Token_Node_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the ``TokenNode`` builtin trait.

         



 function Lkt_Node_P_Error_Node_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the ``ErrorNode`` builtin trait.

         



 function Lkt_Node_P_Char_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the character builtin type.

         



 function Lkt_Node_P_Int_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the integer builtin type.

         



 function Lkt_Node_P_Bool_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the boolean builtin type.

         



 function Lkt_Node_P_Bigint_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the big integer builtin type.

         



 function Lkt_Node_P_String_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the string builtin type.

         



 function Lkt_Node_P_Symbol_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the string builtin type.

         



 function Lkt_Node_P_Property_Error_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the property error builtin type.

         



 function Lkt_Node_P_Regexp_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the regexp builtin type.

         



 function Lkt_Node_P_Entity_Gen_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl
   ;
--  Unit method. Return the logicvar builtin type.

         



 function Lkt_Node_P_Entity_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the logicvar builtin type.

         



 function Lkt_Node_P_Logicvar_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the logicvar builtin type.

         



 function Lkt_Node_P_Equation_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the logicvar builtin type.

         



 function Lkt_Node_P_Array_Gen_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl
   ;
--  Unit method. Return the array builtin generic type.

         



 function Lkt_Node_P_Array_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the array builtin type.

         



 function Lkt_Node_P_Astlist_Gen_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl
   ;
--  Unit method. Return the ASTList builtin generic type.

         



 function Lkt_Node_P_Astlist_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the ASTList builtin type.

         



 function Lkt_Node_P_Node_Builder_Gen_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl
   ;
--  Unit method. Return the NodeBuilder builtin generic type.

         



 function Lkt_Node_P_Node_Builder_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl
   ;
--  Unit method. Return the NodeBuilder builtin type.

         



 function Lkt_Node_P_Iterator_Gen_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl
   ;
--  Unit method. Return the Iterator builtin generic trait.

         



 function Lkt_Node_P_Iterator_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Trait_Decl
   ;
--  Unit method. Return the Iterator builtin trait.

         



 function Lkt_Node_P_Analysis_Unit_Gen_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl
   ;
--  Unit method. Return the ``AnalysisUnit`` builtin generic trait.

         



 function Lkt_Node_P_Analysis_Unit_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Trait_Decl
   ;
--  Unit method. Return the ``AnalysisUnit`` builtin trait.

         



 function Lkt_Node_P_Any_Type
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Create a AnyTypeDecl.

         



 function Lkt_Node_P_Topmost_Invalid_Decl
   
  (Node : Bare_Lkt_Node
  )

   return Bare_Lkt_Node
   ;
--  Return the topmost (from ``Self`` to the root node) FullDecl annotated with
--  ``@invalid``, null otherwise.

         



 function Lkt_Node_P_Nameres_Diagnostics
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Solver_Diagnostic_Array_Access
   ;
--  If name resolution on this lkt compilation unit fails, this returns all the
--  diagnostics that were produced while resolving it.

         



 function Lkt_Node_P_Solve_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Solver_Result
   ;
--  Solve the equation created by this node. This should be used on entry
--  points only.

         



 function Lkt_Node_P_Solve_Enclosing_Context
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Solver_Result
   ;
--  Finds the nearest parent that is an xref_entry_point and solve its
--  equation.

         



 function Lkt_Node_P_Expected_Type_Entry_Point
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Lkt_Node_P_Solve_Expected_Types
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Lkt_Node_P_Generic_Type_Entry_Point
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Lkt_Node_P_Solve_Generic_Types
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Lkt_Node_P_Unmatched_Argument
   
  (Node : Bare_Lkt_Node
      ; Callee_Type : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Predicate used to emit an error when an argument of a CallExpr could not be
--  matched with a formal.

         



 function Lkt_Node_P_Function_Type_Helper
   
  (Node : Bare_Lkt_Node
      ; Args_Types : Internal_Entity_Type_Decl_Array_Access
      ; Return_Type : Internal_Entity_Type_Decl
      ; Origin : Internal_Entity_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type
   ;
--  Helper function to create a memoized FunctionType.

         



 function Lkt_Node_P_Shed_Rebindings
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity
   ;
--  Return this same entity but with its rebindings shed according to its
--  children lexical environment.

         



 function Dispatcher_Lkt_Node_P_Xref_Entry_Point
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   with Inline_Always
   ;
--  Designates entities that are entry point for the xref solving
--  infrastructure. If this returns true, then nameres_diagnostics can be
--  called on it.

         



 function Dispatcher_Lkt_Node_P_Xref_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   with Inline_Always
   ;
--  Base property for constructing equations that will resolve names and types
--  when resolved for every sub expression.

         



 function Dispatcher_Lkt_Node_P_Expected_Type_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   with Inline_Always
   ;
--  Creates an equation that wil resolve expected types for children nodes.

         



 function Dispatcher_Lkt_Node_P_Generic_Type_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   with Inline_Always
   ;
--  Creates an equation that will resolve generic types for children nodes.

         



 function Lkt_Node_P_Can_Reach
   
  (Node : Bare_Lkt_Node
      ; From_Node : Bare_Lkt_Node
  )

   return Boolean
   ;


         



 function Node_Env
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  For nodes that introduce a new environment, return the parent lexical
--  environment. Return the "inherited" environment otherwise.

         



 function Children_Env
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  For nodes that introduce a new environment, return it. Return the
--  "inherited" environment otherwise.

         



 function Parent
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity
   ;
--  Return the syntactic parent for this node. Return null for the root node.

         



 function Parents
   
  (Node : Bare_Lkt_Node
      ; With_Self : Boolean
         := True
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Array_Access
   ;
--  Return an array that contains the lexical parents, this node included iff
--  ``with_self`` is True. Nearer parents are first in the list.

         



 function Children
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Array_Access
   ;
--  Return an array that contains the direct lexical children.
--
--  .. warning:: This constructs a whole array every-time you call it, and as
--     such is less efficient than calling the ``Child`` built-in.

         



 function Token_Start
   
  (Node : Bare_Lkt_Node
  )

   return Token_Reference
   ;
--  Return the first token used to parse this node.

         



 function Token_End
   
  (Node : Bare_Lkt_Node
  )

   return Token_Reference
   ;
--  Return the last token used to parse this node.

         



 function Child_Index
   
  (Node : Bare_Lkt_Node
  )

   return Integer
   ;
--  Return the 0-based index for Node in its parent's children.

         



 function Previous_Sibling
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity
   ;
--  Return the node's previous sibling, or null if there is no such sibling.

         



 function Next_Sibling
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity
   ;
--  Return the node's next sibling, or null if there is no such sibling.

         



 function Unit
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Unit
   ;
--  Return the analysis unit owning this node.

         



 function Ple_Root
   
  (Node : Bare_Lkt_Node
  )

   return Bare_Lkt_Node
   ;
--  Return the PLE root that owns this node, or the unit root node if this unit
--  has no PLE root.

         



 function Is_Ghost
   
  (Node : Bare_Lkt_Node
  )

   return Boolean
   ;
--  Return whether the node is a ghost.
--
--  Unlike regular nodes, ghost nodes cover no token in the input source: they
--  are logically located instead between two tokens. Both the ``token_start``
--  and the ``token_end`` of all ghost nodes is the token right after this
--  logical position.

         



 function Text
   
  (Node : Bare_Lkt_Node
  )

   return String_Type
   ;
--  Return the text corresponding to this node. Private property (for internal
--  DSL use).

         



 function Full_Sloc_Image
   
  (Node : Bare_Lkt_Node
  )

   return String_Type
   ;
--  Return a string containing the filename + the sloc in GNU conformant
--  format. Useful to create diagnostics from a node.

         



 function Lkt_Node_P_Xref_Entry_Point
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Designates entities that are entry point for the xref solving
--  infrastructure. If this returns true, then nameres_diagnostics can be
--  called on it.

         



 function Lkt_Node_P_Xref_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Base property for constructing equations that will resolve names and types
--  when resolved for every sub expression.

         



 function Lkt_Node_P_Expected_Type_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Creates an equation that wil resolve expected types for children nodes.

         



 function Lkt_Node_P_Generic_Type_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Creates an equation that will resolve generic types for children nodes.


   




      

   


      
   function Base_Lexer_Case_Rule_Alt_F_Send
     (Node : Bare_Base_Lexer_Case_Rule_Alt) return Bare_Lexer_Case_Rule_Send;



   




      

   

      
      procedure Initialize_Fields_For_Lexer_Case_Rule_Cond_Alt
        (Self : Bare_Lexer_Case_Rule_Cond_Alt
         ; Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs : Bare_Ref_Id_List
         ; Lexer_Case_Rule_Cond_Alt_F_Send : Bare_Lexer_Case_Rule_Send
        );

      
   function Lexer_Case_Rule_Cond_Alt_F_Cond_Exprs
     (Node : Bare_Lexer_Case_Rule_Cond_Alt) return Bare_Ref_Id_List;



   




      

   

      
      procedure Initialize_Fields_For_Lexer_Case_Rule_Default_Alt
        (Self : Bare_Lexer_Case_Rule_Default_Alt
         ; Lexer_Case_Rule_Default_Alt_F_Send : Bare_Lexer_Case_Rule_Send
        );



   




      

   




   




      

   



         



 function Dispatcher_Class_Qualifier_P_As_Bool
   
  (Node : Bare_Class_Qualifier
  )

   return Boolean
   with Inline_Always
   ;
--  Return whether this node is present


   




      

   



         



 function Class_Qualifier_Absent_P_As_Bool
   
  (Node : Bare_Class_Qualifier_Absent
  )

   return Boolean
   ;



   




      

   



         



 function Class_Qualifier_Present_P_As_Bool
   
  (Node : Bare_Class_Qualifier_Present
  )

   return Boolean
   ;



   




      

   


      
   function Decl_F_Syn_Name
     (Node : Bare_Decl) return Bare_Def_Id;


         



 function Dispatcher_Decl_P_Decl_Type_Name
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   with Inline_Always
   ;
--  Return the name of the declaration type, as it should be seen by
--  users/shown in diagnostics.

         



 function Decl_P_Full_Decl
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Full_Decl
   ;


         



 function Decl_P_Implements_Node
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Checks if this decl implements the Node trait TODO: rework this.

         



 function Decl_P_As_Bare_Decl
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Get this declaration without rebindings information.

         



 function Decl_P_Is_Type_Decl
   
  (Node : Bare_Decl
  )

   return Boolean
   ;


         



 function Decl_P_Is_Defined
   
  (Node : Bare_Decl
  )

   return Boolean
   ;


         



 function Decl_P_Infer_Function_Type
   
  (Node : Bare_Decl
      ; Expected_Call : Internal_Entity_Function_Type
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type
   ;
--  Infer the type of the function from the expected_call if Entity is a
--  generic declaration. This iterates through the generic formals of the decl
--  to find all types that try to replace it and find their common_ancestor.
--
--  ``In_Logic_Call``: Wether or not we are currently solving a LogicPropage or
--  a LogicPredicate.

         



 function Decl_P_Function_Type
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type
   ;
--  Build and return a FunctionType corresponding to the current FunDecl.

         



 function Decl_P_Logic_Function_Type
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type
   ;
--  Build and return a FunctionType corresponding to the current FunDecl with
--  an extra LogicVar at the beginning. Moreover, if the function is a dynamic
--  combiner, set its argument to an array of logic variables.

         



 function Decl_P_Get_Type
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the type of the Decl.

         



 function Decl_P_Get_Cast_Type
   
  (Node : Bare_Decl
      ; Cast_To : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  If we are casting an entity (Self) to something that is not an entity, make
--  it an entity.

         



 function Decl_P_Get_Keep_Type
   
  (Node : Bare_Decl
      ; Keep_Type : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the type of Entity when we only keep elements of type keep_type. If
--  we are casting an entity (Self) to something that is not an entity, make it
--  an entity.

         



 function Decl_P_Get_Suffix_Type
   
  (Node : Bare_Decl
      ; Prefix_Type : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  If we are accessing a ParseField of an entity, then that field's type also
--  needs to be an entity.

         



 function Decl_P_Type_Var_Suffix_Ref
   
  (Node : Bare_Decl
      ; Current_Name : Internal_Entity_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Return the declaration corresponding to current_name's name inside when
--  Self is used as an expression: .. code:
--
--  .. code::
--
--     property().name
--                ^^^^
--
--  If property() returns an Enum value, we should not be able to access the
--  enum fields.

         



 function Decl_P_Ref_Var_Suffix_Ref
   
  (Node : Bare_Decl
      ; Type_Var : Internal_Entity_Type_Decl
      ; Current_Name : Internal_Entity_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Return the declaration corresponding to current_name's name inside when
--  Self is used as a declaration.

         



 function Decl_P_Formals
   
  (Node : Bare_Decl
      ; Is_Logic : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Formal_Param_Array_Access
   ;
--  Return an array of FormalParam corresponding to the called function's
--  formals.

         



 function Decl_P_Subdecl_If_Generic
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Return the subdeclaration if Self is a GenericDecl, otherwise return
--  itself.

         



 function Decl_P_Is_Generic
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Returns wether the Decl is generic.

         



 function Decl_P_Return_Type_Is_Instantiated
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return True if the return type of this function is instantiated.

         



 function Decl_P_Is_Instantiated
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return True if Self is an instantiated declaration, meaning that it does
--  not use any of its declared generic types.

         



 function Decl_P_Has_Correct_Type_Param_Number
   
  (Node : Bare_Decl
      ; Nb_Types : Integer
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Checks that the parent GenericDecl has nb_types type formals.

         



 function Decl_P_Could_Infer
   
  (Node : Bare_Decl
      ; Generic_Type : Internal_Entity_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Predicate used to verify if we were able to find the type of the callee.

         



 function Decl_P_Instantiate_Generic_Decl
   
  (Node : Bare_Decl
      ; Type_Params : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Create a DynEnvWrapper to instantiate a DynamicEnvironment to use as
--  rebindings when creating a new Entity from the current type.

         



 function Decl_P_Get_Rebinded_Decl
   
  (Node : Bare_Decl
      ; Rebindings_Env : Lexical_Env
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Create a new Entity from Self using rebindings_env as the new environment
--  to handle generics.

         



 function Decl_P_Is_Dynvar
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Dispatcher_Decl_P_Name
   
  (Node : Bare_Decl
  )

   return Symbol_Type
   with Inline_Always
   ;
--  Return the symbol corresponding to the name of this declaration.

         



 function Dispatcher_Decl_P_Full_Name
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   with Inline_Always
   ;
--  Return the full name of this decl, as it should be seen by users/shown in
--  diagnostics.

         



 function Dispatcher_Decl_P_Defined_Scope
   
  (Node : Bare_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   with Inline_Always
   ;
--  Return the lexical environment defined by the declaration (ie. fields of a
--  StructDecl).
--
--  ``Origin``: Origin node of the request.

         



 function Dispatcher_Decl_P_Defined_Scope_As_Entity
   
  (Node : Bare_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   with Inline_Always
   ;
--  Return the lexical environment defined by the declaration as if it was
--  contained inside an entity.
--
--  Entity creates a special case for the ASTList class and Node trait:
--
--  .. code::
--
--     @builtin generic[T]
--     class ASTList : ....... Indexable[T], Iterator[T]
--
--  If the type ASTList is contained inside the Entity type (eg.
--  Entity[FooNode.list]), then the properties inherited from its traits need
--  to return entities. When this property is called on ASTList, we rebind T to
--  ``Entity[T]`` and get the defined environments of the newly rebound
--  entities instead.
--
--  ``Origin``: Origin node of the request.

         



 function Env_Mappings_0
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc_Array_Access
   ;


         



 function Decl_P_Name
   
  (Node : Bare_Decl
  )

   return Symbol_Type
   ;
--  Return the symbol corresponding to the name of this declaration.

         



 function Decl_P_Full_Name
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;
--  Return the full name of this decl, as it should be seen by users/shown in
--  diagnostics.

         



 function Decl_P_Defined_Scope
   
  (Node : Bare_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  Return the lexical environment defined by the declaration (ie. fields of a
--  StructDecl).

         



 function Decl_P_Defined_Scope_As_Entity
   
  (Node : Bare_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  Return the lexical environment defined by the declaration as if it was
--  contained inside an entity.
--
--  Entity creates a special case for the ASTList class and Node trait:
--
--  .. code::
--
--     @builtin generic[T]
--     class ASTList : ....... Indexable[T], Iterator[T]
--
--  If the type ASTList is contained inside the Entity type (eg.
--  Entity[FooNode.list]), then the properties inherited from its traits need
--  to return entities. When this property is called on ASTList, we rebind T to
--  ``Entity[T]`` and get the defined environments of the newly rebound
--  entities instead.


   



         procedure Decl_Pre_Env_Actions
           (Self            : Bare_Decl;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   


      
   function Base_Grammar_Rule_Decl_F_Expr
     (Node : Bare_Base_Grammar_Rule_Decl) return Bare_Grammar_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Rule_Decl
        (Self : Bare_Grammar_Rule_Decl
         ; Grammar_Rule_Decl_F_Syn_Name : Bare_Def_Id
         ; Grammar_Rule_Decl_F_Expr : Bare_Grammar_Expr
        );


         



 function Grammar_Rule_Decl_P_Decl_Type_Name
   
  (Node : Bare_Grammar_Rule_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;



   




      

   

      
      procedure Initialize_Fields_For_Synthetic_Lexer_Decl
        (Self : Bare_Synthetic_Lexer_Decl
        );


         



 function Synthetic_Lexer_Decl_P_Name
   
  (Node : Bare_Synthetic_Lexer_Decl
  )

   return Symbol_Type
   ;


         



 function Synthetic_Lexer_Decl_P_Decl_Type_Name
   
  (Node : Bare_Synthetic_Lexer_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;



   




      

   



         



 function Base_Val_Decl_P_Defined_Scope
   
  (Node : Bare_Base_Val_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  ``Origin``: Origin node of the request.


   




      

   



         



 function Node_Decl_P_Name
   
  (Node : Bare_Node_Decl
  )

   return Symbol_Type
   ;


         



 function Node_Decl_P_Decl_Type_Name
   
  (Node : Bare_Node_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Node_Decl_P_Owning_Type
   
  (Node : Bare_Node_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;



   




      

   



         



 function Self_Decl_P_Name
   
  (Node : Bare_Self_Decl
  )

   return Symbol_Type
   ;


         



 function Self_Decl_P_Decl_Type_Name
   
  (Node : Bare_Self_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Self_Decl_P_Owning_Type
   
  (Node : Bare_Self_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;



   




      

   



         



 function User_Val_Decl_P_Xref_Entry_Point
   
  (Node : Bare_User_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Enum_Lit_Decl
        (Self : Bare_Enum_Lit_Decl
         ; Enum_Lit_Decl_F_Syn_Name : Bare_Def_Id
        );


         



 function Enum_Lit_Decl_P_Decl_Type_Name
   
  (Node : Bare_Enum_Lit_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Enum_Lit_Decl_P_Xref_Entry_Point
   
  (Node : Bare_Enum_Lit_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Enum_Lit_Decl_P_Defined_Scope
   
  (Node : Bare_Enum_Lit_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  ``Origin``: Origin node of the request.

         



 function Enum_Lit_Decl_P_Parent_Type
   
  (Node : Bare_Enum_Lit_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the parent EnumTypeDecl.


   







      

   


      
   function Explicitly_Typed_Decl_F_Decl_Type
     (Node : Bare_Explicitly_Typed_Decl) return Bare_Type_Ref;



   




      

   


      
   function Component_Decl_F_Default_Val
     (Node : Bare_Component_Decl) return Bare_Expr;


         



 function Component_Decl_P_Xref_Equation
   
  (Node : Bare_Component_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Component_Decl_P_To_Formal_Param
   
  (Node : Bare_Component_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Formal_Param
   ;
--  Create a FormalParam from the current ComponentDecl.


   




      

   

      
      procedure Initialize_Fields_For_Field_Decl
        (Self : Bare_Field_Decl
         ; Field_Decl_F_Syn_Name : Bare_Def_Id
         ; Field_Decl_F_Decl_Type : Bare_Type_Ref
         ; Field_Decl_F_Default_Val : Bare_Expr
        );


         



 function Field_Decl_P_Decl_Type_Name
   
  (Node : Bare_Field_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Field_Decl_P_Owning_Type
   
  (Node : Bare_Field_Decl
  )

   return Bare_Type_Decl
   ;


         



 function Field_Decl_P_Lazy_Field_Function_Type
   
  (Node : Bare_Field_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type
   ;
--  Lazy fields can be seen as memoized properties that do not take arguments.
--  Return a function type corresponding to that supposed property.

         



 function Env_Mappings_1
   
  (Node : Bare_Field_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc
   ;


         



 function Env_Trans_Parent_2
   
  (Node : Bare_Field_Decl
  )

   return Boolean
   ;


         



 function Env_Mappings_3
   
  (Node : Bare_Field_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc_Array_Access
   ;



   



         procedure Field_Decl_Pre_Env_Actions
           (Self            : Bare_Field_Decl;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Fun_Arg_Decl
        (Self : Bare_Fun_Arg_Decl
         ; Fun_Arg_Decl_F_Decl_Annotations : Bare_Decl_Annotation_List
         ; Fun_Arg_Decl_F_Syn_Name : Bare_Def_Id
         ; Fun_Arg_Decl_F_Decl_Type : Bare_Type_Ref
         ; Fun_Arg_Decl_F_Default_Val : Bare_Expr
        );

      
   function Fun_Arg_Decl_F_Decl_Annotations
     (Node : Bare_Fun_Arg_Decl) return Bare_Decl_Annotation_List;


         



 function Fun_Arg_Decl_P_Decl_Type_Name
   
  (Node : Bare_Fun_Arg_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;



   




      

   

      
      procedure Initialize_Fields_For_Lambda_Arg_Decl
        (Self : Bare_Lambda_Arg_Decl
         ; Lambda_Arg_Decl_F_Syn_Name : Bare_Def_Id
         ; Lambda_Arg_Decl_F_Decl_Type : Bare_Type_Ref
         ; Lambda_Arg_Decl_F_Default_Val : Bare_Expr
        );


         



 function Lambda_Arg_Decl_P_Decl_Type_Name
   
  (Node : Bare_Lambda_Arg_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;



   




      

   

      
      procedure Initialize_Fields_For_Dyn_Var_Decl
        (Self : Bare_Dyn_Var_Decl
         ; Dyn_Var_Decl_F_Syn_Name : Bare_Def_Id
         ; Dyn_Var_Decl_F_Decl_Type : Bare_Type_Ref
        );


         



 function Dyn_Var_Decl_P_Decl_Type_Name
   
  (Node : Bare_Dyn_Var_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Dyn_Var_Decl_P_Xref_Entry_Point
   
  (Node : Bare_Dyn_Var_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Match_Val_Decl
        (Self : Bare_Match_Val_Decl
         ; Match_Val_Decl_F_Syn_Name : Bare_Def_Id
         ; Match_Val_Decl_F_Decl_Type : Bare_Type_Ref
        );


         



 function Match_Val_Decl_P_Decl_Type_Name
   
  (Node : Bare_Match_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Match_Val_Decl_P_Xref_Entry_Point
   
  (Node : Bare_Match_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Match_Val_Decl_P_Match_Expr
   
  (Node : Bare_Match_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Expr
   ;



   




      

   

      
      procedure Initialize_Fields_For_Val_Decl
        (Self : Bare_Val_Decl
         ; Val_Decl_F_Syn_Name : Bare_Def_Id
         ; Val_Decl_F_Decl_Type : Bare_Type_Ref
         ; Val_Decl_F_Expr : Bare_Expr
        );

      
   function Val_Decl_F_Expr
     (Node : Bare_Val_Decl) return Bare_Expr;


         



 function Val_Decl_P_Decl_Type_Name
   
  (Node : Bare_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Val_Decl_P_Xref_Equation
   
  (Node : Bare_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Fun_Decl
        (Self : Bare_Fun_Decl
         ; Fun_Decl_F_Syn_Name : Bare_Def_Id
         ; Fun_Decl_F_Args : Bare_Fun_Arg_Decl_List
         ; Fun_Decl_F_Return_Type : Bare_Type_Ref
         ; Fun_Decl_F_Body : Bare_Expr
        );

      
   function Fun_Decl_F_Args
     (Node : Bare_Fun_Decl) return Bare_Fun_Arg_Decl_List;

      
   function Fun_Decl_F_Return_Type
     (Node : Bare_Fun_Decl) return Bare_Type_Ref;

      
   function Fun_Decl_F_Body
     (Node : Bare_Fun_Decl) return Bare_Expr;


         



 function Fun_Decl_P_Decl_Type_Name
   
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Fun_Decl_P_Owning_Type
   
  (Node : Bare_Fun_Decl
  )

   return Bare_Type_Decl
   ;


         



 function Fun_Decl_P_Is_Dynamic_Combiner
   
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  When this property is used as a a combinder inside an NPropagate equation,
--  return wether it expects a dynamic number of arguments.

         



 function Fun_Decl_P_Xref_Equation
   
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Fun_Decl_P_Function_Type_Aux
   
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type
   ;
--  Build and return a FunctionType corresponding to the current FunDecl.

         



 function Env_Mappings_4
   
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc
   ;


         



 function Env_Trans_Parent_5
   
  (Node : Bare_Fun_Decl
  )

   return Boolean
   ;


         



 function Env_Mappings_6
   
  (Node : Bare_Fun_Decl
  )

   return Internal_Env_Assoc_Array_Access
   ;



   



         procedure Fun_Decl_Pre_Env_Actions
           (Self            : Bare_Fun_Decl;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Env_Spec_Decl
        (Self : Bare_Env_Spec_Decl
         ; Env_Spec_Decl_F_Syn_Name : Bare_Def_Id
         ; Env_Spec_Decl_F_Actions : Bare_Call_Expr_List
        );

      
   function Env_Spec_Decl_F_Actions
     (Node : Bare_Env_Spec_Decl) return Bare_Call_Expr_List;


         



 function Env_Spec_Decl_P_Owning_Type
   
  (Node : Bare_Env_Spec_Decl
  )

   return Bare_Type_Decl
   ;


         



 function Env_Spec_Decl_P_Decl_Type_Name
   
  (Node : Bare_Env_Spec_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Env_Spec_Decl_P_Xref_Entry_Point
   
  (Node : Bare_Env_Spec_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Env_Spec_Decl_P_Xref_Equation
   
  (Node : Bare_Env_Spec_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Env_Trans_Parent_7
   
  (Node : Bare_Env_Spec_Decl
  )

   return Boolean
   ;


         



 function Env_Mappings_8
   
  (Node : Bare_Env_Spec_Decl
  )

   return Internal_Env_Assoc_Array_Access
   ;



   



         procedure Env_Spec_Decl_Pre_Env_Actions
           (Self            : Bare_Env_Spec_Decl;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Generic_Decl
        (Self : Bare_Generic_Decl
         ; Generic_Decl_F_Generic_Formal_Decls : Bare_Generic_Formal_Decl_List
         ; Generic_Decl_F_Decl : Bare_Decl
        );

      
   function Generic_Decl_F_Generic_Formal_Decls
     (Node : Bare_Generic_Decl) return Bare_Generic_Formal_Decl_List;

      
   function Generic_Decl_F_Decl
     (Node : Bare_Generic_Decl) return Bare_Decl;


         



 function Generic_Decl_P_Name
   
  (Node : Bare_Generic_Decl
  )

   return Symbol_Type
   ;


         



 function Generic_Decl_P_Generic_Formals
   
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Generic_Formal_Type_Decl_Array_Access
   ;


         



 function Generic_Decl_P_Generic_Formals_Names
   
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Symbol_Type_Array_Access
   ;


         



 function Generic_Decl_P_Decl_Type_Name
   
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Generic_Decl_P_Instantiated_Generic_Formals
   
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl_Array_Access
   ;


         



 function Env_Mappings_9
   
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc
   ;


         



 function Env_Trans_Parent_10
   
  (Node : Bare_Generic_Decl
  )

   return Boolean
   ;



   



         procedure Generic_Decl_Pre_Env_Actions
           (Self            : Bare_Generic_Decl;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Grammar_Decl
        (Self : Bare_Grammar_Decl
         ; Grammar_Decl_F_Syn_Name : Bare_Def_Id
         ; Grammar_Decl_F_Rules : Bare_Full_Decl_List
        );

      
   function Grammar_Decl_F_Rules
     (Node : Bare_Grammar_Decl) return Bare_Full_Decl_List;


         



 function Grammar_Decl_P_Decl_Type_Name
   
  (Node : Bare_Grammar_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Env_Mappings_11
   
  (Node : Bare_Grammar_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc
   ;


         



 function Env_Trans_Parent_12
   
  (Node : Bare_Grammar_Decl
  )

   return Boolean
   ;



   



         procedure Grammar_Decl_Pre_Env_Actions
           (Self            : Bare_Grammar_Decl;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Lexer_Decl
        (Self : Bare_Lexer_Decl
         ; Lexer_Decl_F_Syn_Name : Bare_Def_Id
         ; Lexer_Decl_F_Rules : Bare_Lkt_Node_List
        );

      
   function Lexer_Decl_F_Rules
     (Node : Bare_Lexer_Decl) return Bare_Lkt_Node_List;


         



 function Lexer_Decl_P_Decl_Type_Name
   
  (Node : Bare_Lexer_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Lexer_Decl_P_Builtin_Decls
   
  (Node : Bare_Lexer_Decl
  )

   return Internal_Env_Assoc_Array_Access
   ;


         



 function Env_Mappings_13
   
  (Node : Bare_Lexer_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc
   ;


         



 function Env_Trans_Parent_14
   
  (Node : Bare_Lexer_Decl
  )

   return Boolean
   ;


         



 function Env_Mappings_15
   
  (Node : Bare_Lexer_Decl
  )

   return Internal_Env_Assoc_Array_Access
   ;



   



         procedure Lexer_Decl_Pre_Env_Actions
           (Self            : Bare_Lexer_Decl;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Lexer_Family_Decl
        (Self : Bare_Lexer_Family_Decl
         ; Lexer_Family_Decl_F_Syn_Name : Bare_Def_Id
         ; Lexer_Family_Decl_F_Rules : Bare_Full_Decl_List
        );

      
   function Lexer_Family_Decl_F_Rules
     (Node : Bare_Lexer_Family_Decl) return Bare_Full_Decl_List;


         



 function Lexer_Family_Decl_P_Decl_Type_Name
   
  (Node : Bare_Lexer_Family_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;



   




      

   



         



 function Synth_Arg_Decl_P_Full_Name
   
  (Node : Bare_Synth_Arg_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Synth_Arg_Decl_P_Decl_Type_Name
   
  (Node : Bare_Synth_Arg_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;



   




      

   

      
      procedure Initialize_Fields_For_Synth_Fun_Decl
        (Self : Bare_Synth_Fun_Decl
        );


         



 function Synth_Fun_Decl_P_Function_Type_Aux
   
  (Node : Bare_Synth_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type
   ;


         



 function Synth_Fun_Decl_P_Decl_Type_Name
   
  (Node : Bare_Synth_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;



   




      

   


      
   function Type_Decl_F_Traits
     (Node : Bare_Type_Decl) return Bare_Type_Ref_List;

      
   function Type_Decl_F_Syn_Base_Type
     (Node : Bare_Type_Decl) return Bare_Type_Ref;


         



 function Type_Decl_P_Self_Decl
   
  (Node : Bare_Type_Decl
  )

   return Bare_Self_Decl
   ;


         



 function Type_Decl_P_Node_Decl
   
  (Node : Bare_Type_Decl
  )

   return Bare_Node_Decl
   ;


         



 function Type_Decl_P_Base_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Ref
   ;
--  Return the base type for this node, if any.

         



 function Type_Decl_P_Base_Type_If_Entity
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the base type for this node, if any.

         



 function Type_Decl_P_Is_Equation
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Predicate used to verify that operands of equation logic operators are of
--  equation type.

         



 function Type_Decl_P_Is_Bool
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Predicate used to verify that operands of boolean logic operators are of
--  boolean type.

         



 function Type_Decl_P_Is_String_Or_Array_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Type_Decl_P_Is_Int_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Type_Decl_P_Is_Int_Or_Node
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Type_Decl_P_Get_Entity_Node_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Get the type parameter used to rebind the Entity type declaration if Self
--  is the enity type.

         



 function Type_Decl_P_Is_Subtype_Or_Eq
   
  (Node : Bare_Type_Decl
      ; Rhs : Internal_Entity_Type_Decl
      ; Allow_Entity : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return True if rhs is equal to or is a subtype of Self. If allow_entity is
--  True, Entity and/or rhs are entities, get the node type.

         



 function Type_Decl_P_Common_Ancestor_Helper
   
  (Node : Bare_Type_Decl
      ; Other_Types : Internal_Entity_Type_Decl_Array_Access
      ; Idx : Integer
      ; Imprecise : Boolean
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;


         



 function Type_Decl_P_Imprecise_Common_Ancestor_List
   
  (Node : Bare_Type_Decl
      ; Other_Types : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the nearest common ancestor of Self and other_types, ignoring any
--  type that does not share a common ancestor with the rest of the types. If
--  one of the type is Entity, the result will also be wrapped by Entity.

         



 function Type_Decl_P_Commutative_Matching_Type
   
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
      ; Allow_Common_Ancestor : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return true if Self and other are matching type that can be permutated.

         



 function Type_Decl_P_Could_Determine_Type
   
  (Node : Bare_Type_Decl
  )

   return Boolean
   ;
--  Return true if Self and other are matching types.

         



 function Type_Decl_P_Matching_Generic_Types
   
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return true if Self and other are matcing generic types.

         



 function Type_Decl_P_Matching_Type
   
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return true if Self and other are matching types.

         



 function Type_Decl_P_Matching_Logic_Type
   
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return True if the types match or we are expecting an Entity and get a
--  LogicVar.

         



 function Type_Decl_P_Is_Of_Array_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Predicate used to verify that a type is an array (implements the Indexable
--  trait).

         



 function Type_Decl_P_Is_Callable
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Predicate used to emit an error when the type of an expression is not
--  callable.

         



 function Type_Decl_P_Match_Param_Get_Type
   
  (Node : Bare_Type_Decl
      ; Current_Name : Internal_Entity_Param
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Return the formal type corresponding to current_param's declaration.
--
--  ``Current_Name``: Call parameter used to find the correspoding FunArgDecl
--  during type resolution of function calls.
--
--  ``In_Logic_Call``: Wether or not we are currently solving a LogicPropage or
--  a LogicPredicate.

         



 function Type_Decl_P_Match_Param_Get_Decl
   
  (Node : Bare_Type_Decl
      ; Current_Name : Internal_Entity_Param
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Return the formal declaration corresponding to current_param.
--
--  ``Current_Name``: Call parameter used to find the correspoding FunArgDecl
--  during type resolution of function calls.
--
--  ``In_Logic_Call``: Wether or not we are currently solving a LogicPropage or
--  a LogicPredicate.

         



 function Type_Decl_P_Lambda_Param_Get_Type
   
  (Node : Bare_Type_Decl
      ; Current_Largdecl : Internal_Entity_Lambda_Arg_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the type declaration corresponding to current_largdecl's formal
--  type.

         



 function Type_Decl_P_Is_Valid_Call
   
  (Node : Bare_Type_Decl
      ; Args : Internal_Entity_Param_List
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Check wether the call to Self is valid and all formals are paired or have a
--  default value.
--
--  ``In_Logic_Call``: Wether or not we are currently solving a LogicPropage or
--  a LogicPredicate.

         



 function Type_Decl_P_Get_Return_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the return type of the FunctionType.

         



 function Type_Decl_P_Create_Function_Type
   
  (Node : Bare_Type_Decl
      ; Args : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Create a FunctionType, using Self as the return type and args for the
--  arguments.

         



 function Type_Decl_P_Make_Array_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Create a rebinded Array type declaration, using Self as the type parameter.

         



 function Type_Decl_P_Get_Array_Content_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Get the type parameter used to rebind the Array type declaration.

         



 function Type_Decl_P_Get_Super_Of_Parent
   
  (Node : Bare_Type_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  ``Origin``: Origin node of the request.

         



 function Type_Decl_P_Basic_Trait_From_Self
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return a rebinded version of BasicTrait, in order to allow for all types to
--  have builtins (do, singleton...) in their environment.

         



 function Type_Decl_P_Find_Types_That_Replace_Ty
   
  (Node : Bare_Type_Decl
      ; Ty : Internal_Entity_Type_Decl
      ; Origin : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl_Array_Access
   ;
--  Traverse ``Entity`` and ``origin`` simultaneously and list all all types
--  found in origin that would replace ty in Entity.
--
--  .. code::
--
--     ((Entity[T], Array[Entity[T]]) => T).find_types_that_replace_ty(
--         (T),
--         ((Entity[A], Array[Entity[B[C]]]) => D)
--     ) ==> [A, B[C], D]

         



 function Type_Decl_P_As_Node_Builder_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the corresponding NodeBuilder type for Entity. If Entity is not a
--  class (hence not a Node), return Entity as is.

         



 function Dispatcher_Type_Decl_P_Base_Types
   
  (Node : Bare_Type_Decl
      ; Include_Self : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl_Array_Access
   with Inline_Always
   ;
--  Return an array containing all subclasses of the type.

         



 function Dispatcher_Type_Decl_P_Is_Subtype
   
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   with Inline_Always
   ;
--  Return true if Self is a subtype of other.

         



 function Dispatcher_Type_Decl_P_Common_Ancestor
   
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
      ; Imprecise : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   with Inline_Always
   ;
--  Return the nearest common ancestor between Self and other. If imprecise is
--  True, return either type that is non-null. If both types are the Entity
--  type, use their wrapped type instead.

         



 function Dispatcher_Type_Decl_P_Node_Builder_Scope
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   with Inline_Always
   ;
--  Create an environment with the builder() function associated to the node
--  type parameter. If the type is not a class, return an empty environment.

         



 function Type_Decl_P_Base_Types
   
  (Node : Bare_Type_Decl
      ; Include_Self : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl_Array_Access
   ;
--  Return an array containing all subclasses of the type.

         



 function Type_Decl_P_Is_Subtype
   
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return true if Self is a subtype of other.

         



 function Type_Decl_P_Common_Ancestor
   
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
      ; Imprecise : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the nearest common ancestor between Self and other. If imprecise is
--  True, return either type that is non-null. If both types are the Entity
--  type, use their wrapped type instead.

         



 function Type_Decl_P_Node_Builder_Scope
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  Create an environment with the builder() function associated to the node
--  type parameter. If the type is not a class, return an empty environment.


   




      

   



         



 function Any_Type_Decl_P_Full_Name
   
  (Node : Bare_Any_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Any_Type_Decl_P_Decl_Type_Name
   
  (Node : Bare_Any_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;



   




      

   

      
      procedure Initialize_Fields_For_Enum_Class_Alt_Decl
        (Self : Bare_Enum_Class_Alt_Decl
         ; Enum_Class_Alt_Decl_F_Syn_Name : Bare_Def_Id
        );


         



 function Enum_Class_Alt_Decl_P_Decl_Type_Name
   
  (Node : Bare_Enum_Class_Alt_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Enum_Class_Alt_Decl_P_Is_Subtype
   
  (Node : Bare_Enum_Class_Alt_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Enum_Class_Alt_Decl_P_Defined_Scope
   
  (Node : Bare_Enum_Class_Alt_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  ``Origin``: Origin node of the request.

         



 function Enum_Class_Alt_Decl_P_Parent_Type
   
  (Node : Bare_Enum_Class_Alt_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the parent EnumTypeDecl.

         



 function Enum_Class_Alt_Decl_P_Base_Types
   
  (Node : Bare_Enum_Class_Alt_Decl
      ; Include_Self : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl_Array_Access
   ;
--  Return an array containing all subclasses of the type.


   







      

   

      
      procedure Initialize_Fields_For_Function_Type
        (Self : Bare_Function_Type
        );


         



 function Function_Type_P_Full_Name
   
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Function_Type_P_Decl_Type_Name
   
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Function_Type_P_Defined_Scope
   
  (Node : Bare_Function_Type
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  ``Origin``: Origin node of the request.

         



 function Function_Type_P_Should_Ignore_Constructor_Arg
   
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return True if the arguments should be ignored.

         



 function Function_Type_P_Returns_Entity
   
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Function_Type_P_Returns_Bool
   
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Generic_Formal_Type_Decl
        (Self : Bare_Generic_Formal_Type_Decl
         ; Generic_Formal_Type_Decl_F_Has_Class : Bare_Class_Qualifier
         ; Generic_Formal_Type_Decl_F_Syn_Name : Bare_Def_Id
        );

      
   function Generic_Formal_Type_Decl_F_Has_Class
     (Node : Bare_Generic_Formal_Type_Decl) return Bare_Class_Qualifier;


         



 function Generic_Formal_Type_Decl_P_Decl_Type_Name
   
  (Node : Bare_Generic_Formal_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;



   




      

   


      
   function Named_Type_Decl_F_Decls
     (Node : Bare_Named_Type_Decl) return Bare_Decl_Block;


         



 function Named_Type_Decl_P_Defined_Scope
   
  (Node : Bare_Named_Type_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  ``Origin``: Origin node of the request.


   




      

   

      
      procedure Initialize_Fields_For_Basic_Class_Decl
        (Self : Bare_Basic_Class_Decl
         ; Basic_Class_Decl_F_Syn_Name : Bare_Def_Id
         ; Basic_Class_Decl_F_Syn_Base_Type : Bare_Type_Ref
         ; Basic_Class_Decl_F_Traits : Bare_Type_Ref_List
        );


         



 function Basic_Class_Decl_P_Is_Subtype
   
  (Node : Bare_Basic_Class_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Basic_Class_Decl_P_Defined_Scope
   
  (Node : Bare_Basic_Class_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  ``Origin``: Origin node of the request.

         



 function Basic_Class_Decl_P_Defined_Scope_As_Entity
   
  (Node : Bare_Basic_Class_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  ``Origin``: Origin node of the request.

         



 function Basic_Class_Decl_P_Common_Ancestor
   
  (Node : Bare_Basic_Class_Decl
      ; Other : Internal_Entity_Type_Decl
      ; Imprecise : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;



   




      

   

      
      procedure Initialize_Fields_For_Class_Decl
        (Self : Bare_Class_Decl
         ; Basic_Class_Decl_F_Syn_Name : Bare_Def_Id
         ; Basic_Class_Decl_F_Syn_Base_Type : Bare_Type_Ref
         ; Basic_Class_Decl_F_Traits : Bare_Type_Ref_List
         ; Class_Decl_F_Decls : Bare_Decl_Block
        );


         



 function Class_Decl_P_Decl_Type_Name
   
  (Node : Bare_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Class_Decl_P_Constructor_Fields
   
  (Node : Bare_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Field_Decl_Array_Access
   ;
--  Return a list of all fields that are necessaru in the constructor of the
--  class.

         



 function Class_Decl_P_Node_Builder_Scope
   
  (Node : Bare_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;


         



 function Class_Decl_P_Function_Type_Aux
   
  (Node : Bare_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type
   ;
--  Build and return a FunctionType corresponding to the constructor of this
--  current ClassDecl.


   




      

   

      
      procedure Initialize_Fields_For_Enum_Class_Decl
        (Self : Bare_Enum_Class_Decl
         ; Basic_Class_Decl_F_Syn_Name : Bare_Def_Id
         ; Basic_Class_Decl_F_Syn_Base_Type : Bare_Type_Ref
         ; Basic_Class_Decl_F_Traits : Bare_Type_Ref_List
         ; Enum_Class_Decl_F_Branches : Bare_Enum_Class_Case_List
         ; Enum_Class_Decl_F_Decls : Bare_Decl_Block
        );

      
   function Enum_Class_Decl_F_Branches
     (Node : Bare_Enum_Class_Decl) return Bare_Enum_Class_Case_List;


         



 function Enum_Class_Decl_P_Decl_Type_Name
   
  (Node : Bare_Enum_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Enum_Class_Decl_P_Alts
   
  (Node : Bare_Enum_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Enum_Class_Alt_Decl_Array_Access
   ;


         



 function Env_Mappings_16
   
  (Node : Bare_Enum_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc
   ;


         



 function Env_Mappings_17
   
  (Node : Bare_Enum_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc_Array_Access
   ;



   



         procedure Enum_Class_Decl_Pre_Env_Actions
           (Self            : Bare_Enum_Class_Decl;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);

         procedure Enum_Class_Decl_Post_Env_Actions
           (Self : Bare_Enum_Class_Decl; State : in out PLE_Node_State);



      

   

      
      procedure Initialize_Fields_For_Enum_Type_Decl
        (Self : Bare_Enum_Type_Decl
         ; Enum_Type_Decl_F_Syn_Name : Bare_Def_Id
         ; Enum_Type_Decl_F_Traits : Bare_Type_Ref_List
         ; Enum_Type_Decl_F_Literals : Bare_Enum_Lit_Decl_List
         ; Enum_Type_Decl_F_Decls : Bare_Decl_Block
        );

      
   function Enum_Type_Decl_F_Literals
     (Node : Bare_Enum_Type_Decl) return Bare_Enum_Lit_Decl_List;


         



 function Enum_Type_Decl_P_Decl_Type_Name
   
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Env_Mappings_18
   
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc
   ;


         



 function Env_Mappings_19
   
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc_Array_Access
   ;


         



 function Env_Mappings_20
   
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc_Array_Access
   ;



   



         procedure Enum_Type_Decl_Pre_Env_Actions
           (Self            : Bare_Enum_Type_Decl;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);

         procedure Enum_Type_Decl_Post_Env_Actions
           (Self : Bare_Enum_Type_Decl; State : in out PLE_Node_State);



      

   

      
      procedure Initialize_Fields_For_Struct_Decl
        (Self : Bare_Struct_Decl
         ; Struct_Decl_F_Syn_Name : Bare_Def_Id
         ; Struct_Decl_F_Traits : Bare_Type_Ref_List
         ; Struct_Decl_F_Decls : Bare_Decl_Block
        );


         



 function Struct_Decl_P_Decl_Type_Name
   
  (Node : Bare_Struct_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Struct_Decl_P_Function_Type_Aux
   
  (Node : Bare_Struct_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type
   ;
--  Build and return a FunctionType corresponding to the current FunDecl.

         



 function Struct_Decl_P_Entity_Scope
   
  (Node : Bare_Struct_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  If Self is the Entity struct, add the scope defined by the type parameter.

         



 function Struct_Decl_P_Update_Func_Env
   
  (Node : Bare_Struct_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  Return a LexicalEnv containing a synthetic declaration of the ``update``
--  function for the StructDecl.

         



 function Struct_Decl_P_Defined_Scope
   
  (Node : Bare_Struct_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  ``Origin``: Origin node of the request.


   




      

   

      
      procedure Initialize_Fields_For_Trait_Decl
        (Self : Bare_Trait_Decl
         ; Trait_Decl_F_Syn_Name : Bare_Def_Id
         ; Trait_Decl_F_Decls : Bare_Decl_Block
        );


         



 function Trait_Decl_P_Decl_Type_Name
   
  (Node : Bare_Trait_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type
   ;


         



 function Trait_Decl_P_Defined_Scope_As_Entity
   
  (Node : Bare_Trait_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env
   ;
--  ``Origin``: Origin node of the request.


   




      

   

      
      procedure Initialize_Fields_For_Decl_Annotation
        (Self : Bare_Decl_Annotation
         ; Decl_Annotation_F_Name : Bare_Id
         ; Decl_Annotation_F_Params : Bare_Decl_Annotation_Params
        );

      
   function Decl_Annotation_F_Name
     (Node : Bare_Decl_Annotation) return Bare_Id;

      
   function Decl_Annotation_F_Params
     (Node : Bare_Decl_Annotation) return Bare_Decl_Annotation_Params;


         



 function Decl_Annotation_P_Xref_Entry_Point
   
  (Node : Bare_Decl_Annotation
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Decl_Annotation_P_With_Dynvars_Equation
   
  (Node : Bare_Decl_Annotation
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Build an equation for solving type and name resolution of the
--  'with_dynvars' annotation.

         



 function Decl_Annotation_P_Xref_Equation
   
  (Node : Bare_Decl_Annotation
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Only create an equation for annotations that really require typing and name
--  resolution (such as @with_dynvars).


   




      

   

      
      procedure Initialize_Fields_For_Decl_Annotation_Params
        (Self : Bare_Decl_Annotation_Params
         ; Decl_Annotation_Params_F_Params : Bare_Param_List
        );

      
   function Decl_Annotation_Params_F_Params
     (Node : Bare_Decl_Annotation_Params) return Bare_Param_List;



   




      

   

      
      procedure Initialize_Fields_For_Dyn_Env_Wrapper
        (Self : Bare_Dyn_Env_Wrapper
        );


         



 function Dyn_Env_Wrapper_F_Dynenvwrapper_Instantiation_Env
   
  (Node : Bare_Dyn_Env_Wrapper
  )

   return Lexical_Env
   ;
--  Instantiate the corresponding LexicalEnv containing the declarations with
--  their corresponding name.

         



 function Dyn_Env_Wrapper_P_Instantiation_Bindings
   
  (Node : Bare_Dyn_Env_Wrapper
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Inner_Env_Assoc_Array_Access
   ;



   




      

   

      
      procedure Initialize_Fields_For_Elsif_Branch
        (Self : Bare_Elsif_Branch
         ; Elsif_Branch_F_Cond_Expr : Bare_Expr
         ; Elsif_Branch_F_Then_Expr : Bare_Expr
        );

      
   function Elsif_Branch_F_Cond_Expr
     (Node : Bare_Elsif_Branch) return Bare_Expr;

      
   function Elsif_Branch_F_Then_Expr
     (Node : Bare_Elsif_Branch) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Enum_Class_Case
        (Self : Bare_Enum_Class_Case
         ; Enum_Class_Case_F_Decls : Bare_Enum_Class_Alt_Decl_List
        );

      
   function Enum_Class_Case_F_Decls
     (Node : Bare_Enum_Class_Case) return Bare_Enum_Class_Alt_Decl_List;



   




      

   



         



 function Dispatcher_Excludes_Null_P_As_Bool
   
  (Node : Bare_Excludes_Null
  )

   return Boolean
   with Inline_Always
   ;
--  Return whether this node is present


   




      

   



         



 function Excludes_Null_Absent_P_As_Bool
   
  (Node : Bare_Excludes_Null_Absent
  )

   return Boolean
   ;



   




      

   



         



 function Excludes_Null_Present_P_As_Bool
   
  (Node : Bare_Excludes_Null_Present
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Expr
        (Self : Bare_Expr
        );


         



 function Expr_P_Xref_Entry_Point
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Expr_P_Get_Type
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the type of this expression.

         



 function Expr_P_Get_Generic_Type
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the expected type of this expression.

         



 function Expr_P_Get_Expected_Type
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Return the expected type of this expression.

         



 function Expr_P_Get_Rightmost_Refid
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Ref_Id
   ;
--  Return the right-most RefId of an expression (i.e. the expression itself if
--  it already is a RefId or the suffix if it is a DotExpr).

         



 function Expr_P_Expected_Type_Equation
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Expr_P_Call_Generic_Type_Equation
   
  (Node : Bare_Expr
      ; Name : Internal_Entity_Expr
      ; Args : Internal_Entity_Param_List
      ; In_Logic_Call : Boolean
         := False
  )

   return Logic_Equation
   ;
--  Build an equation for solving the generic types of children the children of
--  call nodes (CallExpr, LogicPropage and LogicPredicate).
--
--  ``In_Logic_Call``: Wether or not we are currently solving a LogicPropage or
--  a LogicPredicate.

         



 function Expr_P_Call_Expected_Type_Equation
   
  (Node : Bare_Expr
      ; Name : Internal_Entity_Expr
      ; Args : Internal_Entity_Param_List
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Compute the expected type of name and expressions in args.

         



 function Expr_P_Match_Params
   
  (Node : Bare_Expr
      ; Formals : Internal_Formal_Param_Array_Access
      ; Args : Internal_Entity_Param_List
  )

   return Internal_Param_Match_Array_Access
   ;
--  Match a function's formals with the arguments of the CallExpr.

         



 function Expr_P_Xref_Call_Args_Equation
   
  (Node : Bare_Expr
      ; Name : Internal_Entity_Expr
      ; Args : Internal_Entity_Param_List
      ; In_Logic_Call : Boolean
         := False
  )

   return Logic_Equation
   ;
--  Build an equation for name and type resolution of calls.
--
--  ``In_Logic_Call``: Wether or not we are currently solving a LogicPropage or
--  a LogicPredicate.

         



 function Expr_P_Xref_Call_Equation
   
  (Node : Bare_Expr
      ; Name : Internal_Entity_Expr
      ; Args : Internal_Entity_Param_List
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Build an equation to solve type and name resolution for calling ``name``
--  with ``args`` as the arguments. CallExprs, LogicPredicates and
--  LogicPropagate are all calls to a given callee, but their only common
--  ancestor is Expr, so it is necessary to build the equation here.
--
--  ``In_Logic_Call``: Wether or not we are currently solving a LogicPropage or
--  a LogicPredicate.

         



 function Dispatcher_Expr_P_Xlogic_Equation
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   with Inline_Always
   ;
--  Build an equation to solve type and name resolution for logic expressions.

         



 function Dispatcher_Expr_P_Xtype_Equation
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   with Inline_Always
   ;
--  Build an equation to solve type and name resolution for type referencement.

         



 function Dispatcher_Expr_P_Referenced_Decl
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   with Inline_Always
   ;
--  Return the declaration referenced by this expression, if applicable, else
--  null.
--
--  The property is memoized in order to avoid use the value inside logic
--  variables on every redundent call, causing faulty behavior when used with
--  rebindings. TODO: Do like LAL to avoid memoization for more safety.

         



 function Dispatcher_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   with Inline_Always
   ;
--  Returns True if the expression's actual type can be determined without
--  using its expected type.

         



 function Expr_P_Xlogic_Equation
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Build an equation to solve type and name resolution for logic expressions.

         



 function Expr_P_Xtype_Equation
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Build an equation to solve type and name resolution for type referencement.

         



 function Expr_P_Referenced_Decl
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Return the declaration referenced by this expression, if applicable, else
--  null.
--
--  The property is memoized in order to avoid use the value inside logic
--  variables on every redundent call, causing faulty behavior when used with
--  rebindings. TODO: Do like LAL to avoid memoization for more safety.

         



 function Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Returns True if the expression's actual type can be determined without
--  using its expected type.


   




      

   

      
      procedure Initialize_Fields_For_Any_Of
        (Self : Bare_Any_Of
         ; Any_Of_F_Expr : Bare_Expr
         ; Any_Of_F_Values : Bare_Any_Of_List
        );

      
   function Any_Of_F_Expr
     (Node : Bare_Any_Of) return Bare_Expr;

      
   function Any_Of_F_Values
     (Node : Bare_Any_Of) return Bare_Any_Of_List;


         



 function Any_Of_P_Xref_Equation
   
  (Node : Bare_Any_Of
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Any_Of_P_Has_Context_Free_Type
   
  (Node : Bare_Any_Of
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Array_Literal
        (Self : Bare_Array_Literal
         ; Array_Literal_F_Exprs : Bare_Expr_List
         ; Array_Literal_F_Element_Type : Bare_Type_Ref
        );

      
   function Array_Literal_F_Exprs
     (Node : Bare_Array_Literal) return Bare_Expr_List;

      
   function Array_Literal_F_Element_Type
     (Node : Bare_Array_Literal) return Bare_Type_Ref;


         



 function Array_Literal_P_Has_Context_Free_Type
   
  (Node : Bare_Array_Literal
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Array_Literal_P_Expected_Exprs_Type_Equation
   
  (Node : Bare_Array_Literal
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Array_Literal_P_Xref_Equation
   
  (Node : Bare_Array_Literal
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Base_Call_Expr
        (Self : Bare_Base_Call_Expr
         ; Base_Call_Expr_F_Name : Bare_Expr
         ; Base_Call_Expr_F_Args : Bare_Param_List
        );

      
   function Base_Call_Expr_F_Name
     (Node : Bare_Base_Call_Expr) return Bare_Expr;

      
   function Base_Call_Expr_F_Args
     (Node : Bare_Base_Call_Expr) return Bare_Param_List;


         



 function Base_Call_Expr_P_Generic_Type_Equation
   
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Base_Call_Expr_P_Expected_Type_Equation
   
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Base_Call_Expr_P_Xref_Equation
   
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Base_Call_Expr_P_Xlogic_Unknown
   
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Build an equation that emits a diagnostic for when the name of the name
--  tries to call an unknown logic function.

         



 function Base_Call_Expr_P_Xlogic_Any_All
   
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Base_Call_Expr_P_Xlogic_Equation
   
  (Node : Bare_Base_Call_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Called when a CallExpr is used inside a LogicExpr.


   




      

   

      
      procedure Initialize_Fields_For_Call_Expr
        (Self : Bare_Call_Expr
         ; Base_Call_Expr_F_Name : Bare_Expr
         ; Base_Call_Expr_F_Args : Bare_Param_List
        );



   




      

   

      
      procedure Initialize_Fields_For_Logic_Call_Expr
        (Self : Bare_Logic_Call_Expr
         ; Base_Call_Expr_F_Name : Bare_Expr
         ; Base_Call_Expr_F_Args : Bare_Param_List
        );



   




      

   

      
      procedure Initialize_Fields_For_Logic_Predicate
        (Self : Bare_Logic_Predicate
         ; Base_Call_Expr_F_Name : Bare_Expr
         ; Base_Call_Expr_F_Args : Bare_Param_List
        );


         



 function Logic_Predicate_P_Generic_Type_Equation
   
  (Node : Bare_Logic_Predicate
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Logic_Predicate_P_Expected_Type_Equation
   
  (Node : Bare_Logic_Predicate
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Logic_Predicate_P_Xref_Equation
   
  (Node : Bare_Logic_Predicate
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Logic_Propagate_Call
        (Self : Bare_Logic_Propagate_Call
         ; Base_Call_Expr_F_Name : Bare_Expr
         ; Base_Call_Expr_F_Args : Bare_Param_List
        );



   




      

   

      
      procedure Initialize_Fields_For_Base_Dot_Expr
        (Self : Bare_Base_Dot_Expr
         ; Base_Dot_Expr_F_Prefix : Bare_Expr
         ; Base_Dot_Expr_F_Suffix : Bare_Ref_Id
        );

      
   function Base_Dot_Expr_F_Prefix
     (Node : Bare_Base_Dot_Expr) return Bare_Expr;

      
   function Base_Dot_Expr_F_Suffix
     (Node : Bare_Base_Dot_Expr) return Bare_Ref_Id;


         



 function Base_Dot_Expr_P_Referenced_Decl
   
  (Node : Bare_Base_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;


         



 function Base_Dot_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Base_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Base_Dot_Expr_P_First_Var_In_Prefix_Env
   
  (Node : Bare_Base_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Bind dest's logic variables to the correct declaration depending on its
--  prefix(Self).

         



 function Base_Dot_Expr_P_Xtype_Equation
   
  (Node : Bare_Base_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Base_Dot_Expr_P_Is_Call_To_Super
   
  (Node : Bare_Base_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return True if this DotExpr is a reference to super (meaning it matches the
--  patterns ``self.super`` or ``node.super``).

         



 function Base_Dot_Expr_P_Generic_Type_Equation
   
  (Node : Bare_Base_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Base_Dot_Expr_P_Expected_Type_Equation
   
  (Node : Bare_Base_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Base_Dot_Expr_P_Xref_Typing_Equation
   
  (Node : Bare_Base_Dot_Expr
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  ``In_Logic_Call``: Wether or not we are currently solving a LogicPropage or
--  a LogicPredicate.

         



 function Base_Dot_Expr_P_Xref_Equation
   
  (Node : Bare_Base_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Base_Dot_Expr_P_Xlogic_Equation
   
  (Node : Bare_Base_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Dot_Expr
        (Self : Bare_Dot_Expr
         ; Base_Dot_Expr_F_Prefix : Bare_Expr
         ; Base_Dot_Expr_F_Suffix : Bare_Ref_Id
        );



   




      

   

      
      procedure Initialize_Fields_For_Null_Cond_Dotted_Name
        (Self : Bare_Null_Cond_Dotted_Name
         ; Base_Dot_Expr_F_Prefix : Bare_Expr
         ; Base_Dot_Expr_F_Suffix : Bare_Ref_Id
        );



   




      

   

      
      procedure Initialize_Fields_For_Bin_Op
        (Self : Bare_Bin_Op
         ; Bin_Op_F_Left : Bare_Expr
         ; Bin_Op_F_Op : Bare_Op
         ; Bin_Op_F_Right : Bare_Expr
        );

      
   function Bin_Op_F_Left
     (Node : Bare_Bin_Op) return Bare_Expr;

      
   function Bin_Op_F_Op
     (Node : Bare_Bin_Op) return Bare_Op;

      
   function Bin_Op_F_Right
     (Node : Bare_Bin_Op) return Bare_Expr;


         



 function Bin_Op_P_Xref_Equation
   
  (Node : Bare_Bin_Op
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Bin_Op_P_Has_Context_Free_Type
   
  (Node : Bare_Bin_Op
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Block_Expr
        (Self : Bare_Block_Expr
         ; Block_Expr_F_Val_Defs : Bare_Block_Decl_List
         ; Block_Expr_F_Expr : Bare_Expr
        );

      
   function Block_Expr_F_Val_Defs
     (Node : Bare_Block_Expr) return Bare_Block_Decl_List;

      
   function Block_Expr_F_Expr
     (Node : Bare_Block_Expr) return Bare_Expr;


         



 function Block_Expr_P_Xref_Equation
   
  (Node : Bare_Block_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Block_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Block_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Env_Trans_Parent_21
   
  (Node : Bare_Block_Expr
  )

   return Boolean
   ;



   



         procedure Block_Expr_Pre_Env_Actions
           (Self            : Bare_Block_Expr;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Cast_Expr
        (Self : Bare_Cast_Expr
         ; Cast_Expr_F_Expr : Bare_Expr
         ; Cast_Expr_F_Excludes_Null : Bare_Excludes_Null
         ; Cast_Expr_F_Dest_Type : Bare_Type_Ref
        );

      
   function Cast_Expr_F_Expr
     (Node : Bare_Cast_Expr) return Bare_Expr;

      
   function Cast_Expr_F_Excludes_Null
     (Node : Bare_Cast_Expr) return Bare_Excludes_Null;

      
   function Cast_Expr_F_Dest_Type
     (Node : Bare_Cast_Expr) return Bare_Type_Ref;


         



 function Cast_Expr_P_Expected_Type_Equation
   
  (Node : Bare_Cast_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Cast_Expr_P_Xref_Equation
   
  (Node : Bare_Cast_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Error_On_Null
        (Self : Bare_Error_On_Null
         ; Error_On_Null_F_Expr : Bare_Expr
        );

      
   function Error_On_Null_F_Expr
     (Node : Bare_Error_On_Null) return Bare_Expr;


         



 function Error_On_Null_P_Xref_Equation
   
  (Node : Bare_Error_On_Null
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Error_On_Null_P_Has_Context_Free_Type
   
  (Node : Bare_Error_On_Null
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Generic_Instantiation
        (Self : Bare_Generic_Instantiation
         ; Generic_Instantiation_F_Name : Bare_Expr
         ; Generic_Instantiation_F_Args : Bare_Type_Ref_List
        );

      
   function Generic_Instantiation_F_Name
     (Node : Bare_Generic_Instantiation) return Bare_Expr;

      
   function Generic_Instantiation_F_Args
     (Node : Bare_Generic_Instantiation) return Bare_Type_Ref_List;


         



 function Generic_Instantiation_P_Xref_Equation
   
  (Node : Bare_Generic_Instantiation
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Expr
        (Self : Bare_Grammar_Expr
        );



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Cut
        (Self : Bare_Grammar_Cut
        );



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Discard
        (Self : Bare_Grammar_Discard
         ; Grammar_Discard_F_Expr : Bare_Grammar_Expr
        );

      
   function Grammar_Discard_F_Expr
     (Node : Bare_Grammar_Discard) return Bare_Grammar_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Dont_Skip
        (Self : Bare_Grammar_Dont_Skip
         ; Grammar_Dont_Skip_F_Expr : Bare_Grammar_Expr
         ; Grammar_Dont_Skip_F_Dont_Skip : Bare_Grammar_Expr
        );

      
   function Grammar_Dont_Skip_F_Expr
     (Node : Bare_Grammar_Dont_Skip) return Bare_Grammar_Expr;

      
   function Grammar_Dont_Skip_F_Dont_Skip
     (Node : Bare_Grammar_Dont_Skip) return Bare_Grammar_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_List
        (Self : Bare_Grammar_List
         ; Grammar_List_F_List_Type : Bare_Type_Ref
         ; Grammar_List_F_Kind : Bare_List_Kind
         ; Grammar_List_F_Expr : Bare_Grammar_Expr
         ; Grammar_List_F_Sep : Bare_Grammar_List_Sep
        );

      
   function Grammar_List_F_List_Type
     (Node : Bare_Grammar_List) return Bare_Type_Ref;

      
   function Grammar_List_F_Kind
     (Node : Bare_Grammar_List) return Bare_List_Kind;

      
   function Grammar_List_F_Expr
     (Node : Bare_Grammar_List) return Bare_Grammar_Expr;

      
   function Grammar_List_F_Sep
     (Node : Bare_Grammar_List) return Bare_Grammar_List_Sep;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Null
        (Self : Bare_Grammar_Null
         ; Grammar_Null_F_Name : Bare_Type_Ref
        );

      
   function Grammar_Null_F_Name
     (Node : Bare_Grammar_Null) return Bare_Type_Ref;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Opt
        (Self : Bare_Grammar_Opt
         ; Grammar_Opt_F_Expr : Bare_Grammar_Expr
        );

      
   function Grammar_Opt_F_Expr
     (Node : Bare_Grammar_Opt) return Bare_Grammar_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Opt_Error
        (Self : Bare_Grammar_Opt_Error
         ; Grammar_Opt_Error_F_Expr : Bare_Grammar_Expr
        );

      
   function Grammar_Opt_Error_F_Expr
     (Node : Bare_Grammar_Opt_Error) return Bare_Grammar_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Opt_Error_Group
        (Self : Bare_Grammar_Opt_Error_Group
         ; Grammar_Opt_Error_Group_F_Expr : Bare_Grammar_Expr_List
        );

      
   function Grammar_Opt_Error_Group_F_Expr
     (Node : Bare_Grammar_Opt_Error_Group) return Bare_Grammar_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Opt_Group
        (Self : Bare_Grammar_Opt_Group
         ; Grammar_Opt_Group_F_Expr : Bare_Grammar_Expr_List
        );

      
   function Grammar_Opt_Group_F_Expr
     (Node : Bare_Grammar_Opt_Group) return Bare_Grammar_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Or_Expr
        (Self : Bare_Grammar_Or_Expr
         ; Grammar_Or_Expr_F_Sub_Exprs : Bare_Grammar_Expr_List_List
        );

      
   function Grammar_Or_Expr_F_Sub_Exprs
     (Node : Bare_Grammar_Or_Expr) return Bare_Grammar_Expr_List_List;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Pick
        (Self : Bare_Grammar_Pick
         ; Grammar_Pick_F_Exprs : Bare_Grammar_Expr_List
        );

      
   function Grammar_Pick_F_Exprs
     (Node : Bare_Grammar_Pick) return Bare_Grammar_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Implicit_Pick
        (Self : Bare_Grammar_Implicit_Pick
         ; Grammar_Pick_F_Exprs : Bare_Grammar_Expr_List
        );



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Predicate
        (Self : Bare_Grammar_Predicate
         ; Grammar_Predicate_F_Expr : Bare_Grammar_Expr
         ; Grammar_Predicate_F_Prop_Ref : Bare_Expr
        );

      
   function Grammar_Predicate_F_Expr
     (Node : Bare_Grammar_Predicate) return Bare_Grammar_Expr;

      
   function Grammar_Predicate_F_Prop_Ref
     (Node : Bare_Grammar_Predicate) return Bare_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Rule_Ref
        (Self : Bare_Grammar_Rule_Ref
         ; Grammar_Rule_Ref_F_Node_Name : Bare_Ref_Id
        );

      
   function Grammar_Rule_Ref_F_Node_Name
     (Node : Bare_Grammar_Rule_Ref) return Bare_Ref_Id;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Skip
        (Self : Bare_Grammar_Skip
         ; Grammar_Skip_F_Name : Bare_Type_Ref
        );

      
   function Grammar_Skip_F_Name
     (Node : Bare_Grammar_Skip) return Bare_Type_Ref;



   




      

   

      
      procedure Initialize_Fields_For_Grammar_Stop_Cut
        (Self : Bare_Grammar_Stop_Cut
         ; Grammar_Stop_Cut_F_Expr : Bare_Grammar_Expr
        );

      
   function Grammar_Stop_Cut_F_Expr
     (Node : Bare_Grammar_Stop_Cut) return Bare_Grammar_Expr;



   




      

   

      
      procedure Initialize_Fields_For_Parse_Node_Expr
        (Self : Bare_Parse_Node_Expr
         ; Parse_Node_Expr_F_Node_Name : Bare_Type_Ref
         ; Parse_Node_Expr_F_Sub_Exprs : Bare_Grammar_Expr_List
        );

      
   function Parse_Node_Expr_F_Node_Name
     (Node : Bare_Parse_Node_Expr) return Bare_Type_Ref;

      
   function Parse_Node_Expr_F_Sub_Exprs
     (Node : Bare_Parse_Node_Expr) return Bare_Grammar_Expr_List;



   




      

   

      
      procedure Initialize_Fields_For_Token_Lit
        (Self : Bare_Token_Lit
        );



   




      

   

      
      procedure Initialize_Fields_For_Token_No_Case_Lit
        (Self : Bare_Token_No_Case_Lit
         ; Token_No_Case_Lit_F_Lit : Bare_Token_Lit
        );

      
   function Token_No_Case_Lit_F_Lit
     (Node : Bare_Token_No_Case_Lit) return Bare_Token_Lit;



   




      

   

      
      procedure Initialize_Fields_For_Token_Pattern_Concat
        (Self : Bare_Token_Pattern_Concat
         ; Token_Pattern_Concat_F_Left : Bare_Grammar_Expr
         ; Token_Pattern_Concat_F_Right : Bare_Token_Pattern_Lit
        );

      
   function Token_Pattern_Concat_F_Left
     (Node : Bare_Token_Pattern_Concat) return Bare_Grammar_Expr;

      
   function Token_Pattern_Concat_F_Right
     (Node : Bare_Token_Pattern_Concat) return Bare_Token_Pattern_Lit;



   




      

   

      
      procedure Initialize_Fields_For_Token_Pattern_Lit
        (Self : Bare_Token_Pattern_Lit
        );



   




      

   

      
      procedure Initialize_Fields_For_Token_Ref
        (Self : Bare_Token_Ref
         ; Token_Ref_F_Token_Name : Bare_Ref_Id
         ; Token_Ref_F_Expr : Bare_Token_Lit
        );

      
   function Token_Ref_F_Token_Name
     (Node : Bare_Token_Ref) return Bare_Ref_Id;

      
   function Token_Ref_F_Expr
     (Node : Bare_Token_Ref) return Bare_Token_Lit;



   




      

   

      
      procedure Initialize_Fields_For_Id
        (Self : Bare_Id
        );



   




      

   

      
      procedure Initialize_Fields_For_Def_Id
        (Self : Bare_Def_Id
        );



   




      

   

      
      procedure Initialize_Fields_For_Module_Ref_Id
        (Self : Bare_Module_Ref_Id
        );



   




      

   

      
      procedure Initialize_Fields_For_Ref_Id
        (Self : Bare_Ref_Id
        );


         



 function Ref_Id_P_From_Node
   
  (Node : Bare_Ref_Id
  )

   return Bare_Lkt_Node
   ;
--  Find the limiting node to search in the environment to avoid variables that
--  reference themselves or future variables.

         



 function Ref_Id_P_First_Var_In_Env
   
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;
--  Get the first declaration found for this RefId. This first tries to get
--  variables declared before Self.from_node, if no variable was found, find a
--  type or function anywhere in the node environment.

         



 function Ref_Id_P_Is_Being_Called
   
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Return True if this RefId is used to refer to a function being called.

         



 function Ref_Id_P_Referenced_Decl
   
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl
   ;


         



 function Ref_Id_P_Xtype_Equation
   
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Ref_Id_P_Generic_Type_Equation
   
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Ref_Id_P_Bind_Actual_Type_Equation
   
  (Node : Bare_Ref_Id
      ; First_Var : Internal_Entity_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Bind the corresponding type of first_var to the RefId.

         



 function Ref_Id_P_Xref_Equation
   
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Ref_Id_P_Xlogic_Equation
   
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_If_Expr
        (Self : Bare_If_Expr
         ; If_Expr_F_Cond_Expr : Bare_Expr
         ; If_Expr_F_Then_Expr : Bare_Expr
         ; If_Expr_F_Alternatives : Bare_Elsif_Branch_List
         ; If_Expr_F_Else_Expr : Bare_Expr
        );

      
   function If_Expr_F_Cond_Expr
     (Node : Bare_If_Expr) return Bare_Expr;

      
   function If_Expr_F_Then_Expr
     (Node : Bare_If_Expr) return Bare_Expr;

      
   function If_Expr_F_Alternatives
     (Node : Bare_If_Expr) return Bare_Elsif_Branch_List;

      
   function If_Expr_F_Else_Expr
     (Node : Bare_If_Expr) return Bare_Expr;


         



 function If_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function If_Expr_P_Branch_Exprs
   
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Expr_Array_Access
   ;
--  Return an array containing the expression of all branches.

         



 function If_Expr_P_Expected_Branch_Type_Equation
   
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Find the expected type for all branches by computing the common ancestor of
--  the type of all context free expressions, or the expected type of the
--  IfExpr if no expression is context free.

         



 function If_Expr_P_Xref_Equation
   
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function If_Expr_P_Cond_Branches_Equation
   
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Create an equation verifying that all conditions are valid.


   




      

   

      
      procedure Initialize_Fields_For_Isa
        (Self : Bare_Isa
         ; Isa_F_Expr : Bare_Expr
         ; Isa_F_Dest_Type : Bare_Isa_List
        );

      
   function Isa_F_Expr
     (Node : Bare_Isa) return Bare_Expr;

      
   function Isa_F_Dest_Type
     (Node : Bare_Isa) return Bare_Isa_List;


         



 function Isa_P_Expected_Type_Equation
   
  (Node : Bare_Isa
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Isa_P_Xref_Equation
   
  (Node : Bare_Isa
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Isa_P_Has_Context_Free_Type
   
  (Node : Bare_Isa
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Keep_Expr
        (Self : Bare_Keep_Expr
         ; Keep_Expr_F_Expr : Bare_Expr
         ; Keep_Expr_F_Keep_Type : Bare_Type_Ref
        );

      
   function Keep_Expr_F_Expr
     (Node : Bare_Keep_Expr) return Bare_Expr;

      
   function Keep_Expr_F_Keep_Type
     (Node : Bare_Keep_Expr) return Bare_Type_Ref;


         



 function Keep_Expr_P_Xref_Equation
   
  (Node : Bare_Keep_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Lambda_Expr
        (Self : Bare_Lambda_Expr
         ; Lambda_Expr_F_Params : Bare_Lambda_Arg_Decl_List
         ; Lambda_Expr_F_Return_Type : Bare_Type_Ref
         ; Lambda_Expr_F_Body : Bare_Expr
        );

      
   function Lambda_Expr_F_Params
     (Node : Bare_Lambda_Expr) return Bare_Lambda_Arg_Decl_List;

      
   function Lambda_Expr_F_Return_Type
     (Node : Bare_Lambda_Expr) return Bare_Type_Ref;

      
   function Lambda_Expr_F_Body
     (Node : Bare_Lambda_Expr) return Bare_Expr;


         



 function Lambda_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Lambda_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Lambda_Expr_P_Expected_Type_Equation
   
  (Node : Bare_Lambda_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Lambda_Expr_P_Generic_Type_Equation
   
  (Node : Bare_Lambda_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Lambda_Expr_P_Xref_Equation
   
  (Node : Bare_Lambda_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Env_Trans_Parent_22
   
  (Node : Bare_Lambda_Expr
  )

   return Boolean
   ;



   



         procedure Lambda_Expr_Pre_Env_Actions
           (Self            : Bare_Lambda_Expr;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Lit
        (Self : Bare_Lit
        );



   




      

   

      
      procedure Initialize_Fields_For_Big_Num_Lit
        (Self : Bare_Big_Num_Lit
        );


         



 function Big_Num_Lit_P_Xref_Equation
   
  (Node : Bare_Big_Num_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Char_Lit
        (Self : Bare_Char_Lit
        );


         



 function Char_Lit_P_Xref_Equation
   
  (Node : Bare_Char_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Null_Lit
        (Self : Bare_Null_Lit
         ; Null_Lit_F_Dest_Type : Bare_Type_Ref
        );

      
   function Null_Lit_F_Dest_Type
     (Node : Bare_Null_Lit) return Bare_Type_Ref;


         



 function Null_Lit_P_Xref_Equation
   
  (Node : Bare_Null_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Null_Lit_P_Has_Context_Free_Type
   
  (Node : Bare_Null_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Num_Lit
        (Self : Bare_Num_Lit
        );


         



 function Num_Lit_P_Xref_Equation
   
  (Node : Bare_Num_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_String_Lit
        (Self : Bare_String_Lit
        );


         



 function Dispatcher_String_Lit_P_Denoted_Value
   
  (Node : Bare_String_Lit
  )

   return Internal_Decoded_String_Value
   with Inline_Always
   ;
--  Return the content of the given string literal node.

         



 function Dispatcher_String_Lit_P_Is_Prefixed_String
   
  (Node : Bare_String_Lit
  )

   return Boolean
   with Inline_Always
   ;
--  Return whether this string is prefixed or not.

         



 function Dispatcher_String_Lit_P_Prefix
   
  (Node : Bare_String_Lit
  )

   return Character_Type
   with Inline_Always
   ;
--  Return the prefix of this string, or the null character if there is no
--  prefix.

         



 function String_Lit_P_Is_Regexp_Literal
   
  (Node : Bare_String_Lit
  )

   return Boolean
   ;
--  Return whether this string literal is actually a regexp literal, by
--  checking that this string is prefixed by 'p'.

         



 function String_Lit_P_Xref_Equation
   
  (Node : Bare_String_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Block_String_Lit
        (Self : Bare_Block_String_Lit
         ; Block_String_Lit_F_Lines : Bare_Block_String_Line_List
        );

      
   function Block_String_Lit_F_Lines
     (Node : Bare_Block_String_Lit) return Bare_Block_String_Line_List;


         



 function Block_String_Lit_P_Is_Prefixed_String
   
  (Node : Bare_Block_String_Lit
  )

   return Boolean
   ;


         



 function Block_String_Lit_P_Prefix
   
  (Node : Bare_Block_String_Lit
  )

   return Character_Type
   ;



   




      

   

      
      procedure Initialize_Fields_For_Single_Line_String_Lit
        (Self : Bare_Single_Line_String_Lit
        );



   




      

   

      
      procedure Initialize_Fields_For_Pattern_Single_Line_String_Lit
        (Self : Bare_Pattern_Single_Line_String_Lit
        );



   




      

   

      
      procedure Initialize_Fields_For_Logic_Assign
        (Self : Bare_Logic_Assign
         ; Logic_Assign_F_Dest_Var : Bare_Expr
         ; Logic_Assign_F_Value : Bare_Expr
        );

      
   function Logic_Assign_F_Dest_Var
     (Node : Bare_Logic_Assign) return Bare_Expr;

      
   function Logic_Assign_F_Value
     (Node : Bare_Logic_Assign) return Bare_Expr;


         



 function Logic_Assign_P_Xref_Equation
   
  (Node : Bare_Logic_Assign
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Logic_Expr
        (Self : Bare_Logic_Expr
         ; Logic_Expr_F_Expr : Bare_Expr
        );

      
   function Logic_Expr_F_Expr
     (Node : Bare_Logic_Expr) return Bare_Expr;


         



 function Logic_Expr_P_Xref_Equation
   
  (Node : Bare_Logic_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Logic_Propagate
        (Self : Bare_Logic_Propagate
         ; Logic_Propagate_F_Dest_Var : Bare_Expr
         ; Logic_Propagate_F_Call : Bare_Logic_Propagate_Call
        );

      
   function Logic_Propagate_F_Dest_Var
     (Node : Bare_Logic_Propagate) return Bare_Expr;

      
   function Logic_Propagate_F_Call
     (Node : Bare_Logic_Propagate) return Bare_Logic_Propagate_Call;


         



 function Logic_Propagate_P_Generic_Type_Equation
   
  (Node : Bare_Logic_Propagate
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Logic_Propagate_P_Expected_Type_Equation
   
  (Node : Bare_Logic_Propagate
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Logic_Propagate_P_Xref_Equation
   
  (Node : Bare_Logic_Propagate
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Logic_Unify
        (Self : Bare_Logic_Unify
         ; Logic_Unify_F_Lhs : Bare_Expr
         ; Logic_Unify_F_Rhs : Bare_Expr
        );

      
   function Logic_Unify_F_Lhs
     (Node : Bare_Logic_Unify) return Bare_Expr;

      
   function Logic_Unify_F_Rhs
     (Node : Bare_Logic_Unify) return Bare_Expr;


         



 function Logic_Unify_P_Xref_Equation
   
  (Node : Bare_Logic_Unify
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Match_Expr
        (Self : Bare_Match_Expr
         ; Match_Expr_F_Match_Expr : Bare_Expr
         ; Match_Expr_F_Branches : Bare_Match_Branch_List
        );

      
   function Match_Expr_F_Match_Expr
     (Node : Bare_Match_Expr) return Bare_Expr;

      
   function Match_Expr_F_Branches
     (Node : Bare_Match_Expr) return Bare_Match_Branch_List;


         



 function Match_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Match_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Match_Expr_P_Branch_Exprs
   
  (Node : Bare_Match_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Expr_Array_Access
   ;
--  Return an array containing the expression of all branches.

         



 function Match_Expr_P_Expected_Branch_Type_Equation
   
  (Node : Bare_Match_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Find the expected type for all branches by computing the common ancestor of
--  the type of all context free expressions, or the expected type of the
--  MatchExpr if no expression is context free.

         



 function Match_Expr_P_Xref_Equation
   
  (Node : Bare_Match_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Not_Expr
        (Self : Bare_Not_Expr
         ; Not_Expr_F_Expr : Bare_Expr
        );

      
   function Not_Expr_F_Expr
     (Node : Bare_Not_Expr) return Bare_Expr;


         



 function Not_Expr_P_Xref_Equation
   
  (Node : Bare_Not_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Paren_Expr
        (Self : Bare_Paren_Expr
         ; Paren_Expr_F_Expr : Bare_Expr
        );

      
   function Paren_Expr_F_Expr
     (Node : Bare_Paren_Expr) return Bare_Expr;


         



 function Paren_Expr_P_Expected_Type_Equation
   
  (Node : Bare_Paren_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Paren_Expr_P_Xref_Equation
   
  (Node : Bare_Paren_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Paren_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Paren_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Raise_Expr
        (Self : Bare_Raise_Expr
         ; Raise_Expr_F_Dest_Type : Bare_Type_Ref
         ; Raise_Expr_F_Except_Expr : Bare_Expr
        );

      
   function Raise_Expr_F_Dest_Type
     (Node : Bare_Raise_Expr) return Bare_Type_Ref;

      
   function Raise_Expr_F_Except_Expr
     (Node : Bare_Raise_Expr) return Bare_Expr;


         



 function Raise_Expr_P_Xref_Equation
   
  (Node : Bare_Raise_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Raise_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Raise_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Subscript_Expr
        (Self : Bare_Subscript_Expr
         ; Subscript_Expr_F_Prefix : Bare_Expr
         ; Subscript_Expr_F_Index : Bare_Expr
        );

      
   function Subscript_Expr_F_Prefix
     (Node : Bare_Subscript_Expr) return Bare_Expr;

      
   function Subscript_Expr_F_Index
     (Node : Bare_Subscript_Expr) return Bare_Expr;


         



 function Subscript_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Subscript_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;


         



 function Subscript_Expr_P_Xref_Equation
   
  (Node : Bare_Subscript_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Null_Cond_Subscript_Expr
        (Self : Bare_Null_Cond_Subscript_Expr
         ; Subscript_Expr_F_Prefix : Bare_Expr
         ; Subscript_Expr_F_Index : Bare_Expr
        );



   




      

   

      
      procedure Initialize_Fields_For_Try_Expr
        (Self : Bare_Try_Expr
         ; Try_Expr_F_Try_Expr : Bare_Expr
         ; Try_Expr_F_Or_Expr : Bare_Expr
        );

      
   function Try_Expr_F_Try_Expr
     (Node : Bare_Try_Expr) return Bare_Expr;

      
   function Try_Expr_F_Or_Expr
     (Node : Bare_Try_Expr) return Bare_Expr;


         



 function Try_Expr_P_Exprs
   
  (Node : Bare_Try_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Expr_Array_Access
   ;
--  Return an array containing all expressions.

         



 function Try_Expr_P_Expected_Exprs_Type_Equation
   
  (Node : Bare_Try_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;
--  Find the expected type for all branches by computing the common ancestor of
--  the type of all context free expressions, or the expected type of the
--  TryExpr if no expression is context free.

         



 function Try_Expr_P_Xref_Equation
   
  (Node : Bare_Try_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Try_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Try_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;



   




      

   

      
      procedure Initialize_Fields_For_Un_Op
        (Self : Bare_Un_Op
         ; Un_Op_F_Op : Bare_Op
         ; Un_Op_F_Expr : Bare_Expr
        );

      
   function Un_Op_F_Op
     (Node : Bare_Un_Op) return Bare_Op;

      
   function Un_Op_F_Expr
     (Node : Bare_Un_Op) return Bare_Expr;


         



 function Un_Op_P_Xref_Equation
   
  (Node : Bare_Un_Op
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Full_Decl
        (Self : Bare_Full_Decl
         ; Full_Decl_F_Doc : Bare_String_Lit
         ; Full_Decl_F_Decl_Annotations : Bare_Decl_Annotation_List
         ; Full_Decl_F_Decl : Bare_Decl
        );

      
   function Full_Decl_F_Doc
     (Node : Bare_Full_Decl) return Bare_String_Lit;

      
   function Full_Decl_F_Decl_Annotations
     (Node : Bare_Full_Decl) return Bare_Decl_Annotation_List;

      
   function Full_Decl_F_Decl
     (Node : Bare_Full_Decl) return Bare_Decl;


         



 function Full_Decl_P_Has_Annotation
   
  (Node : Bare_Full_Decl
      ; Name : Symbol_Type
  )

   return Boolean
   ;
--  Return whether this node has an annotation with name ``name``.

         



 function Full_Decl_P_Get_Annotation
   
  (Node : Bare_Full_Decl
      ; Name : Symbol_Type
  )

   return Bare_Decl_Annotation
   ;
--  Return the annotation with name ``name``.


   




      

   

      
      procedure Initialize_Fields_For_Grammar_List_Sep
        (Self : Bare_Grammar_List_Sep
         ; Grammar_List_Sep_F_Token : Bare_Grammar_Expr
         ; Grammar_List_Sep_F_Extra : Bare_Id
        );

      
   function Grammar_List_Sep_F_Token
     (Node : Bare_Grammar_List_Sep) return Bare_Grammar_Expr;

      
   function Grammar_List_Sep_F_Extra
     (Node : Bare_Grammar_List_Sep) return Bare_Id;



   




      

   

      
      procedure Initialize_Fields_For_Import
        (Self : Bare_Import
         ; Import_F_Name : Bare_Module_Ref_Id
        );

      
   function Import_F_Name
     (Node : Bare_Import) return Bare_Module_Ref_Id;


         



 function Import_P_Referenced_Unit
   
  (Node : Bare_Import
  )

   return Internal_Unit
   ;
--  Return the unit that this import statements designates. Load it if needed.

         



 function Env_Do_23
   
  (Node : Bare_Import
  )

   return Internal_Unit
   ;



   



         procedure Import_Pre_Env_Actions
           (Self            : Bare_Import;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   

      
      procedure Initialize_Fields_For_Langkit_Root
        (Self : Bare_Langkit_Root
         ; Langkit_Root_F_Imports : Bare_Import_List
         ; Langkit_Root_F_Decls : Bare_Full_Decl_List
        );

      
   function Langkit_Root_F_Imports
     (Node : Bare_Langkit_Root) return Bare_Import_List;

      
   function Langkit_Root_F_Decls
     (Node : Bare_Langkit_Root) return Bare_Full_Decl_List;


         



 function Langkit_Root_P_Internal_Env
   
  (Node : Bare_Langkit_Root
  )

   return Lexical_Env
   ;
--  Get the hidden environment in the prelude containing a default declaration
--  of the Metadata type, for when it is not defined by the specification.

         



 function Env_Do_24
   
  (Node : Bare_Langkit_Root
  )

   return Internal_Unit
   ;


         



 function Ref_Env_Nodes_25
   
  (Node : Bare_Langkit_Root
  )

   return Bare_Lkt_Node_Array_Access
   ;


         



 function Ref_Cond_26
   
  (Node : Bare_Langkit_Root
  )

   return Boolean
   ;



   



         procedure Langkit_Root_Pre_Env_Actions
           (Self            : Bare_Langkit_Root;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);

         procedure Langkit_Root_Post_Env_Actions
           (Self : Bare_Langkit_Root; State : in out PLE_Node_State);



      

   

      
      procedure Initialize_Fields_For_Lexer_Case_Rule
        (Self : Bare_Lexer_Case_Rule
         ; Lexer_Case_Rule_F_Expr : Bare_Grammar_Expr
         ; Lexer_Case_Rule_F_Alts : Bare_Base_Lexer_Case_Rule_Alt_List
        );

      
   function Lexer_Case_Rule_F_Expr
     (Node : Bare_Lexer_Case_Rule) return Bare_Grammar_Expr;

      
   function Lexer_Case_Rule_F_Alts
     (Node : Bare_Lexer_Case_Rule) return Bare_Base_Lexer_Case_Rule_Alt_List;



   




      

   

      
      procedure Initialize_Fields_For_Lexer_Case_Rule_Send
        (Self : Bare_Lexer_Case_Rule_Send
         ; Lexer_Case_Rule_Send_F_Sent : Bare_Ref_Id
         ; Lexer_Case_Rule_Send_F_Match_Size : Bare_Num_Lit
        );

      
   function Lexer_Case_Rule_Send_F_Sent
     (Node : Bare_Lexer_Case_Rule_Send) return Bare_Ref_Id;

      
   function Lexer_Case_Rule_Send_F_Match_Size
     (Node : Bare_Lexer_Case_Rule_Send) return Bare_Num_Lit;



   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   



         



 function Env_Trans_Parent_27
   
  (Node : Bare_Decl_Block
  )

   return Boolean
   ;



   



         procedure Decl_Block_Pre_Env_Actions
           (Self            : Bare_Decl_Block;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Match_Branch
        (Self : Bare_Match_Branch
         ; Match_Branch_F_Decl : Bare_Match_Val_Decl
         ; Match_Branch_F_Expr : Bare_Expr
        );

      
   function Match_Branch_F_Decl
     (Node : Bare_Match_Branch) return Bare_Match_Val_Decl;

      
   function Match_Branch_F_Expr
     (Node : Bare_Match_Branch) return Bare_Expr;


         



 function Env_Trans_Parent_28
   
  (Node : Bare_Match_Branch
  )

   return Boolean
   ;



   



         procedure Match_Branch_Pre_Env_Actions
           (Self            : Bare_Match_Branch;
            State           : in out PLE_Node_State;
            Add_To_Env_Only : Boolean := False);




      

   



         



 function Op_P_Is_Equation_Op
   
  (Node : Bare_Op
  )

   return Boolean
   ;


         



 function Op_P_Is_Bool_Op
   
  (Node : Bare_Op
  )

   return Boolean
   ;


         



 function Op_P_Is_Arith_Op
   
  (Node : Bare_Op
  )

   return Boolean
   ;


         



 function Op_P_Is_Order_Op
   
  (Node : Bare_Op
  )

   return Boolean
   ;



   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   




   




      

   

      
      procedure Initialize_Fields_For_Param
        (Self : Bare_Param
         ; Param_F_Name : Bare_Ref_Id
         ; Param_F_Value : Bare_Expr
        );

      
   function Param_F_Name
     (Node : Bare_Param) return Bare_Ref_Id;

      
   function Param_F_Value
     (Node : Bare_Param) return Bare_Expr;


         



 function Param_P_Expected_Type_Equation
   
  (Node : Bare_Param
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;


         



 function Param_P_Xref_Equation
   
  (Node : Bare_Param
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Type_Ref
        (Self : Bare_Type_Ref
        );


         



 function Type_Ref_P_Xref_Entry_Point
   
  (Node : Bare_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean
   ;
--  Designates entities that are entry point for the xref solving
--  infrastructure. If this returns true, then nameres_diagnostics can be
--  called on it.

         



 function Type_Ref_P_Referenced_Decl
   
  (Node : Bare_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl
   ;
--  Returns the referenced type declaration.


   




      

   

      
      procedure Initialize_Fields_For_Default_List_Type_Ref
        (Self : Bare_Default_List_Type_Ref
        );



   




      

   

      
      procedure Initialize_Fields_For_Function_Type_Ref
        (Self : Bare_Function_Type_Ref
         ; Function_Type_Ref_F_Args_Types : Bare_Type_Ref_List
         ; Function_Type_Ref_F_Return_Type : Bare_Type_Ref
        );

      
   function Function_Type_Ref_F_Args_Types
     (Node : Bare_Function_Type_Ref) return Bare_Type_Ref_List;

      
   function Function_Type_Ref_F_Return_Type
     (Node : Bare_Function_Type_Ref) return Bare_Type_Ref;


         



 function Function_Type_Ref_P_Xref_Equation
   
  (Node : Bare_Function_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Generic_Type_Ref
        (Self : Bare_Generic_Type_Ref
         ; Generic_Type_Ref_F_Type_Name : Bare_Expr
         ; Generic_Type_Ref_F_Params : Bare_Type_Ref_List
        );

      
   function Generic_Type_Ref_F_Type_Name
     (Node : Bare_Generic_Type_Ref) return Bare_Expr;

      
   function Generic_Type_Ref_F_Params
     (Node : Bare_Generic_Type_Ref) return Bare_Type_Ref_List;


         



 function Generic_Type_Ref_P_Xref_Equation
   
  (Node : Bare_Generic_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Simple_Type_Ref
        (Self : Bare_Simple_Type_Ref
         ; Simple_Type_Ref_F_Type_Name : Bare_Expr
        );

      
   function Simple_Type_Ref_F_Type_Name
     (Node : Bare_Simple_Type_Ref) return Bare_Expr;


         



 function Simple_Type_Ref_P_Xref_Equation
   
  (Node : Bare_Simple_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   




      

   

      
      procedure Initialize_Fields_For_Var_Bind
        (Self : Bare_Var_Bind
         ; Var_Bind_F_Name : Bare_Ref_Id
         ; Var_Bind_F_Expr : Bare_Expr
        );

      
   function Var_Bind_F_Name
     (Node : Bare_Var_Bind) return Bare_Ref_Id;

      
   function Var_Bind_F_Expr
     (Node : Bare_Var_Bind) return Bare_Expr;


         



 function Var_Bind_P_Xref_Equation
   
  (Node : Bare_Var_Bind
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation
   ;



   





   function "<" (Left, Right : Internal_Unit) return Boolean;

   type Exiled_Entry is record
      Env  : Lexical_Env;
      Key  : Symbol_Type;
      Node : Bare_Lkt_Node;
   end record;
   --  Tuple of values passed to AST_Envs.Add. Used in the lexical
   --  environment rerooting machinery: see Remove_Exiled_Entries and
   --  Reroot_Foreign_Nodes.

   package Exiled_Entry_Vectors is new Liblktlang_Support.Vectors (Exiled_Entry);

   type Foreign_Node_Entry is record
      Node : Bare_Lkt_Node;
      --  The foreign node that has been added to an analysis unit's lexical
      --  environment.

      Unit : Internal_Unit;
      --  Analysis unit that owns Node
   end record;

   package Foreign_Node_Entry_Vectors is new Liblktlang_Support.Vectors
     (Foreign_Node_Entry);

   procedure Register_Destroyable
     (Unit : Internal_Unit; Node : Bare_Lkt_Node);
   --  Register Node to be destroyed when Unit is deallocated/reparsed

   procedure Register_Destroyable
     (Unit : Internal_Unit; Env : AST_Envs.Lexical_Env_Access);
   --  Register Env to be destroyed when Unit is deallocated/reparsed

   ------------------------
   --  Memoization state --
   ------------------------

   


type Mmz_Property is (
      Mmz_Bare_Base_Dot_Expr_Base_Dot_Expr_P_First_Var_In_Prefix_Env
     ,  Mmz_Bare_Basic_Class_Decl_Basic_Class_Decl_P_Defined_Scope
     ,  Mmz_Bare_Class_Decl_Class_Decl_P_Node_Builder_Scope
     ,  Mmz_Bare_Decl_Decl_P_Formals
     ,  Mmz_Bare_Decl_Decl_P_Get_Type
     ,  Mmz_Bare_Decl_Decl_P_Instantiate_Generic_Decl
     ,  Mmz_Bare_Decl_Decl_P_Logic_Function_Type
     ,  Mmz_Bare_Expr_Expr_P_Get_Expected_Type
     ,  Mmz_Bare_Expr_Expr_P_Get_Generic_Type
     ,  Mmz_Bare_Expr_Expr_P_Get_Type
     ,  Mmz_Bare_Expr_Expr_P_Match_Params
     ,  Mmz_Bare_Expr_Expr_P_Referenced_Decl
     ,  Mmz_Bare_Lexer_Decl_Lexer_Decl_P_Builtin_Decls
     ,  Mmz_Bare_Lkt_Node_Lkt_Node_P_Any_Type
     ,  Mmz_Bare_Lkt_Node_Lkt_Node_P_Function_Type_Helper
     ,  Mmz_Bare_Lkt_Node_Lkt_Node_P_Get_Builtin_Type
     ,  Mmz_Bare_Lkt_Node_Lkt_Node_P_Solve_Enclosing_Context
     ,  Mmz_Bare_Lkt_Node_Lkt_Node_P_Solve_Equation
     ,  Mmz_Bare_Lkt_Node_Lkt_Node_P_Solve_Expected_Types
     ,  Mmz_Bare_Lkt_Node_Lkt_Node_P_Solve_Generic_Types
     ,  Mmz_Bare_Ref_Id_Ref_Id_P_First_Var_In_Env
     ,  Mmz_Bare_Ref_Id_Ref_Id_P_Referenced_Decl
     ,  Mmz_Bare_Struct_Decl_Struct_Decl_P_Update_Func_Env
     ,  Mmz_Bare_Type_Decl_Type_Decl_P_Make_Array_Type
     ,  Mmz_Bare_Type_Decl_Type_Decl_P_Node_Builder_Scope
     ,  Mmz_Bare_Type_Decl_Type_Decl_P_Node_Decl
     ,  Mmz_Bare_Type_Decl_Type_Decl_P_Self_Decl
     ,  Mmz_Bare_Type_Ref_Type_Ref_P_Referenced_Decl
);
type Mmz_Key_Kind is (
      Mmz_Bare_Base_Dot_Expr
     ,  Mmz_Bare_Basic_Class_Decl
     ,  Mmz_Bare_Class_Decl
     ,  Mmz_Bare_Decl
     ,  Mmz_Bare_Expr
     ,  Mmz_Bare_Lexer_Decl
     ,  Mmz_Bare_Lkt_Node
     ,  Mmz_Bare_Param_List
     ,  Mmz_Bare_Ref_Id
     ,  Mmz_Bare_Struct_Decl
     ,  Mmz_Bare_Type_Decl
     ,  Mmz_Bare_Type_Ref
     ,  Mmz_Boolean
     ,  Mmz_Env_Rebindings
     ,  Mmz_Internal_Entity
     ,  Mmz_Internal_Entity_Decl
     ,  Mmz_Internal_Entity_Info
     ,  Mmz_Internal_Entity_Param_List
     ,  Mmz_Internal_Entity_Type_Decl
     ,  Mmz_Internal_Entity_Type_Decl_Array_Access
     ,  Mmz_Internal_Formal_Param_Array_Access
     ,  Mmz_Internal_Metadata
     ,  Mmz_Symbol_Type
);
type Mmz_Value_Kind is (
   Mmz_Evaluating,
   Mmz_Error
      , Mmz_Bare_Node_Decl
      , Mmz_Bare_Self_Decl
      , Mmz_Boolean
      , Mmz_Internal_Entity_Decl
      , Mmz_Internal_Entity_Function_Type
      , Mmz_Internal_Entity_Named_Type_Decl
      , Mmz_Internal_Entity_Type_Decl
      , Mmz_Internal_Env_Assoc_Array_Access
      , Mmz_Internal_Formal_Param_Array_Access
      , Mmz_Internal_Param_Match_Array_Access
      , Mmz_Internal_Solver_Result
      , Mmz_Lexical_Env
);

type Mmz_Key_Item (Kind : Mmz_Key_Kind := Mmz_Bare_Base_Dot_Expr) is record
   case Kind is
         when Mmz_Bare_Base_Dot_Expr =>
            As_Bare_Base_Dot_Expr : Bare_Base_Dot_Expr;
         when Mmz_Bare_Basic_Class_Decl =>
            As_Bare_Basic_Class_Decl : Bare_Basic_Class_Decl;
         when Mmz_Bare_Class_Decl =>
            As_Bare_Class_Decl : Bare_Class_Decl;
         when Mmz_Bare_Decl =>
            As_Bare_Decl : Bare_Decl;
         when Mmz_Bare_Expr =>
            As_Bare_Expr : Bare_Expr;
         when Mmz_Bare_Lexer_Decl =>
            As_Bare_Lexer_Decl : Bare_Lexer_Decl;
         when Mmz_Bare_Lkt_Node =>
            As_Bare_Lkt_Node : Bare_Lkt_Node;
         when Mmz_Bare_Param_List =>
            As_Bare_Param_List : Bare_Param_List;
         when Mmz_Bare_Ref_Id =>
            As_Bare_Ref_Id : Bare_Ref_Id;
         when Mmz_Bare_Struct_Decl =>
            As_Bare_Struct_Decl : Bare_Struct_Decl;
         when Mmz_Bare_Type_Decl =>
            As_Bare_Type_Decl : Bare_Type_Decl;
         when Mmz_Bare_Type_Ref =>
            As_Bare_Type_Ref : Bare_Type_Ref;
         when Mmz_Boolean =>
            As_Boolean : Boolean;
         when Mmz_Env_Rebindings =>
            As_Env_Rebindings : Env_Rebindings;
         when Mmz_Internal_Entity =>
            As_Internal_Entity : Internal_Entity;
         when Mmz_Internal_Entity_Decl =>
            As_Internal_Entity_Decl : Internal_Entity_Decl;
         when Mmz_Internal_Entity_Info =>
            As_Internal_Entity_Info : Internal_Entity_Info;
         when Mmz_Internal_Entity_Param_List =>
            As_Internal_Entity_Param_List : Internal_Entity_Param_List;
         when Mmz_Internal_Entity_Type_Decl =>
            As_Internal_Entity_Type_Decl : Internal_Entity_Type_Decl;
         when Mmz_Internal_Entity_Type_Decl_Array_Access =>
            As_Internal_Entity_Type_Decl_Array_Access : Internal_Entity_Type_Decl_Array_Access;
         when Mmz_Internal_Formal_Param_Array_Access =>
            As_Internal_Formal_Param_Array_Access : Internal_Formal_Param_Array_Access;
         when Mmz_Internal_Metadata =>
            As_Internal_Metadata : Internal_Metadata;
         when Mmz_Symbol_Type =>
            As_Symbol_Type : Symbol_Type;
   end case;
end record;

type Mmz_Key_Array is array (Positive range <>) of Mmz_Key_Item;
type Mmz_Key_Array_Access is access all Mmz_Key_Array;
type Mmz_Key is record
   Property : Mmz_Property;
   Items    : Mmz_Key_Array_Access;
end record;

type Mmz_Value (Kind : Mmz_Value_Kind := Mmz_Evaluating) is record
   case Kind is
      when Mmz_Evaluating =>
         null;

      when Mmz_Error =>
         Exc_Id  : Ada.Exceptions.Exception_Id;
         Exc_Msg : String_Access;

         when Mmz_Bare_Node_Decl =>
            As_Bare_Node_Decl : Bare_Node_Decl;
         when Mmz_Bare_Self_Decl =>
            As_Bare_Self_Decl : Bare_Self_Decl;
         when Mmz_Boolean =>
            As_Boolean : Boolean;
         when Mmz_Internal_Entity_Decl =>
            As_Internal_Entity_Decl : Internal_Entity_Decl;
         when Mmz_Internal_Entity_Function_Type =>
            As_Internal_Entity_Function_Type : Internal_Entity_Function_Type;
         when Mmz_Internal_Entity_Named_Type_Decl =>
            As_Internal_Entity_Named_Type_Decl : Internal_Entity_Named_Type_Decl;
         when Mmz_Internal_Entity_Type_Decl =>
            As_Internal_Entity_Type_Decl : Internal_Entity_Type_Decl;
         when Mmz_Internal_Env_Assoc_Array_Access =>
            As_Internal_Env_Assoc_Array_Access : Internal_Env_Assoc_Array_Access;
         when Mmz_Internal_Formal_Param_Array_Access =>
            As_Internal_Formal_Param_Array_Access : Internal_Formal_Param_Array_Access;
         when Mmz_Internal_Param_Match_Array_Access =>
            As_Internal_Param_Match_Array_Access : Internal_Param_Match_Array_Access;
         when Mmz_Internal_Solver_Result =>
            As_Internal_Solver_Result : Internal_Solver_Result;
         when Mmz_Lexical_Env =>
            As_Lexical_Env : Lexical_Env;
   end case;
end record;

function Hash (Key : Mmz_Key) return Hash_Type;
function Equivalent (L, R : Mmz_Key) return Boolean;

package Memoization_Maps is new Ada.Containers.Hashed_Maps
  (Mmz_Key, Mmz_Value, Hash, Equivalent_Keys => Equivalent);

procedure Destroy (Map : in out Memoization_Maps.Map);
--  Free all resources stored in a memoization map. This includes destroying
--  ref-count shares the map owns.

type Memoization_Handle is record
   Key : Mmz_Key;
   --  Key for the memoization

   Cur : Memoization_Maps.Cursor;
   --  If the unit memoization table has an entry for Key, this holds a cursor
   --  to it.

   Cache_Version : Version_Number := 0;
   --  Version of the unit memoization table at the time Key/Cur were created.
   --  When using this record, if the version has changed, both Key and Cur are
   --  invalid and must be recomputed.
end record;
--  Wrapper for memoization state, to be used in memoized properties.
--  Please use high-level functions below instead of accessing fields
--  directly.

function Find_Memoized_Value
  (Unit       : Internal_Unit;
   Handle     : out Memoization_Handle;
   Value      : out Mmz_Value;
   Create_Key : access function return Mmz_Key) return Boolean;
--  Initialize Handle and look for a memoization entry in Unit.Memoization_Map
--  that corresponds to the key in Handle/Create_Key. If one is found, put it
--  in Value and return True. Create such an entry and return False otherwise.

procedure Add_Memoized_Value
  (Unit   : Internal_Unit;
   Handle : in out Memoization_Handle;
   Value  : Mmz_Value;
   Stored : out Boolean);
--  Insert the Handle.Key/Value entry in Unit.Memoization_Map (replacing the
--  previous entry, if present). Set Stored to whether the key/value entry was
--  actually stored: it's not when Handle is stale, i.e. caches where reset
--  since Handle was created).

procedure Add_Memoized_Error
  (Unit   : Internal_Unit;
   Handle : in out Memoization_Handle;
   Exc    : Ada.Exceptions.Exception_Occurrence;
   Stored : out Boolean);
--  Wrapper for ``Add_Memoized_Value`` specifically for memoizing an exception

procedure Reraise_Memoized_Error (Value : Mmz_Value)
with No_Return,
     Pre => Value.Kind = Mmz_Error;
--  Re-raise the exception memoized in ``Value``

procedure Store_Memoized_Error
  (Exc     : Ada.Exceptions.Exception_Occurrence;
   Exc_Id  : out Ada.Exceptions.Exception_Id;
   Exc_Msg : out String_Access);
procedure Free_Memoized_Error
  (Exc_Id  : in out Ada.Exceptions.Exception_Id;
   Exc_Msg : in out String_Access);
procedure Reraise_Memoized_Error
  (Exc_Id  : Ada.Exceptions.Exception_Id;
   Exc_Msg : String_Access)
with No_Return;
--  Lower level routines that language spec extensions can use when dealing
--  with memoization manually.



   -----------------------------
   -- Miscellanous operations --
   -----------------------------

   type Destroy_Procedure is access procedure (Object : System.Address);

   type Destroyable_Type is record
      Object  : System.Address;
      --  Object to destroy

      Destroy : Destroy_Procedure;
      --  Procedure to destroy Object
   end record;
   --  Simple holder to associate an object to destroy and the procedure to
   --  perform the destruction.

   package Destroyable_Vectors is new Liblktlang_Support.Vectors
     (Destroyable_Type);

   package Analysis_Unit_Sets is new Liblktlang_Support.Cheap_Sets
     (Internal_Unit, null);

   package Units_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => GNATCOLL.VFS.Virtual_File,
      Element_Type    => Internal_Unit,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => GNATCOLL.VFS."=");

   function Token_Data (Unit : Internal_Unit) return Token_Data_Handler_Access;

   function Lookup_Symbol
     (Context : Internal_Context; Symbol : Text_Type) return Symbol_Type;
   --  Return the given symbol text as a symbol for this context. Raise an
   --  Invalid_Symbol_Error if it is invalid.

   function Create_Special_Unit
     (Context             : Internal_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit;
   --  Create a new special analysis unit, i.e. a unit that is not registered
   --  in Context's unit map.

   function Templates_Unit (Context : Internal_Context) return Internal_Unit;
   --  Return the analysis unit to be used to parse tree rewriting templates.
   --  This creates it if it does not exists yet.

   procedure Set_Rule (Unit : Internal_Unit; Rule : Grammar_Rule);

   package Virtual_File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => GNATCOLL.VFS.Virtual_File,
      Equivalent_Keys => "=",
      "="             => GNATCOLL.VFS."=",
      Hash            => Ada.Strings.Unbounded.Hash);

   function Normalized_Unit_Filename
     (Context : Internal_Context; Filename : String)
      return GNATCOLL.VFS.Virtual_File;
   --  Try to return a canonical filename. This is used to have an
   --  as-unique-as-possible analysis unit identifier.

   ------------------------------------
   -- File reader internal interface --
   ------------------------------------

   type Internal_File_Reader is limited interface;
   type Internal_File_Reader_Access is access all Internal_File_Reader'Class;
   pragma No_Strict_Aliasing (Internal_File_Reader_Access);

   procedure Inc_Ref (Self : in out Internal_File_Reader) is abstract;
   --  Create an ownership share for this file reader.

   function Dec_Ref (Self : in out Internal_File_Reader) return Boolean
   is abstract;
   --  Release an ownership share for this file reader. This destroys the file
   --  reader if there are no shares left.
   --
   --  Return whether there are no ownership shares left.

   procedure Read
     (Self        : Internal_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector) is abstract;
   --  Read the content of the source at the given filename, decoding it using
   --  the given charset and decoding the byte order mark if ``Read_BOM`` is
   --  true.
   --
   --  If there is an error during this process, append an error message to
   --  Diagnostics. In that case, Contents is considered uninitialized.
   --
   --  Otherwise, allocate a Text_Type buffer, fill it and initialize Contents
   --  to refer to it.

   procedure Dec_Ref (File_Reader : in out Internal_File_Reader_Access);
   --  Call Dec_Ref on File_Reader.all and, if the ref-count reaches 0,
   --  dealloacte it.

   --------------------------------------
   -- Unit provider internal interface --
   --------------------------------------

   type Internal_Unit_Provider is limited interface;
   type Internal_Unit_Provider_Access is
      access all Internal_Unit_Provider'Class;
   pragma No_Strict_Aliasing (Internal_Unit_Provider_Access);

   procedure Inc_Ref (Provider : in out Internal_Unit_Provider) is abstract;
   --  Create an ownership share for this unit provider.

   function Dec_Ref (Provider : in out Internal_Unit_Provider) return Boolean
   is abstract;
   --  Release an ownership share for this unit provider. This destroys the
   --  unit provider if there are no shares left.
   --
   --  Return whether there are no ownership shares left.

   procedure Get_Unit_Location
     (Provider       : Internal_Unit_Provider;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : out Unbounded_String;
      PLE_Root_Index : out Positive) is abstract;
   --  See the public ``Get_Unit_Location`` procedure

   procedure Get_Unit_And_PLE_Root
     (Provider       : Internal_Unit_Provider;
      Context        : Internal_Context;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Charset        : String := "";
      Reparse        : Boolean := False;
      Unit           : out Internal_Unit;
      PLE_Root_Index : out Positive) is abstract;
   --  See the public ``Get_Unit_And_PLE_Root`` procedure

   procedure Dec_Ref (Provider : in out Internal_Unit_Provider_Access);

   type Resolved_Unit is record
      Unit           : Internal_Unit;
      Filename       : String_Access;
      PLE_Root_Index : Positive;
   end record;
   --  Cache entry for requests to unit providers

   type Resolved_Unit_Array is array (Analysis_Unit_Kind) of Resolved_Unit;
   --  One cache entry per unit kind, i.e. all cache entries needed for a given
   --  unit name.

   package Unit_Provider_Cache_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Resolved_Unit_Array,
      Equivalent_Keys => "=",
      Hash            => Hash);

   --------------------------------------
   -- Event handler internal interface --
   --------------------------------------

   type Internal_Event_Handler is limited interface;
   type Internal_Event_Handler_Access is
      access all Internal_Event_Handler'Class;
   pragma No_Strict_Aliasing (Internal_Event_Handler_Access);

   procedure Inc_Ref (Self : in out Internal_Event_Handler) is abstract;
   --  Create an ownership share for this event handler.

   function Dec_Ref (Self : in out Internal_Event_Handler) return Boolean
   is abstract;
   --  Release an ownership share for this event handler. This destroys the
   --  event handler if there are no shares left.
   --
   --  Return whether there are no ownership shares left.

   procedure Unit_Requested_Callback
     (Self               : in out Internal_Event_Handler;
      Context            : Internal_Context;
      Name               : Text_Type;
      From               : Internal_Unit;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is null;

   procedure Unit_Parsed_Callback
     (Self     : in out Internal_Event_Handler;
      Context  : Internal_Context;
      Unit     : Internal_Unit;
      Reparsed : Boolean) is null;

   procedure Dec_Ref (Self : in out Internal_Event_Handler_Access);

   -----------------------------
   -- Lexical env cache stats --
   -----------------------------


   ---------------------------------
   -- Analysis context definition --
   ---------------------------------

   type Analysis_Context_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Serial_Number : Version_Number;
      --  Serial number that is incremented each time this context allocation
      --  is released.

      --  End of ABI area

      Initialized : Boolean;
      Ref_Count   : Natural;
      --  Whether this context is fully initialized, and when it is allocated,
      --  its number of ownership shares. Allocated contexts have 3 possible
      --  states:
      --
      --  * Acquired (not yet initialized, Ref_Count => 1, Initialized =>
      --    False): it can be either initialized or released.
      --
      --  * Initialized (Ref_Count > 0, Initialized => True): it can only be
      --    destroyed and released.
      --
      --  * Released (Ref_Count = 0, Initialized => False): it can only be
      --    acquired again.

      Units : Units_Maps.Map;
      --  Collection of analysis units loaded in this context

      Filenames : Virtual_File_Maps.Map;
      --  Cache for GNATCOLL.VFS.Virtual_File we create for String filenames.
      --  Re-using older Virtual_File values is useful as this reduces the need
      --  to normalize paths, which is a costly operation.

      Symbols : Symbol_Table;
      --  Symbol table used in this whole context

      Charset : Unbounded_String;
      --  Default charset to use in analysis units

      Tab_Stop : aliased Positive;
      --  Tab stop for the lexer to correctly interpret ASCII.HT input
      --  characters.

      With_Trivia : Boolean;
      --  Whether Trivia nodes were parsed and included in analysis units

      Root_Scope : Lexical_Env;
      --  The lexical scope that is shared amongst every compilation unit. Used
      --  to resolve cross file references.

      Named_Envs : NED_Maps.Map;
      --  Map env names to the corresponding named environment descriptors

      File_Reader : Internal_File_Reader_Access;
      --  Object to override the reading and decoding of source files

      Event_Handler : Internal_Event_Handler_Access;
      --  Object to provide event callbacks

      Unit_Provider : Internal_Unit_Provider_Access;
      --  Object to translate unit names to file names

      Unit_Provider_Cache : Unit_Provider_Cache_Maps.Map;
      --  Cache for the Unit_Provider.Get_Unit_And_PLE_Root primitive

      Parser : Parser_Type;
      --  Main parser type. TODO: If we want to parse in several tasks, we'll
      --  replace that by an array of parsers.

      Discard_Errors_In_Populate_Lexical_Env : Boolean;
      --  See the eponym procedure

      In_Populate_Lexical_Env : Boolean;
      --  Flag to tell whether we are running the Populate_Lexical_Env pass.
      --  When it's on, we must not use the memoization map as the hash of
      --  lexical environment changes when their content changes.

      Logic_Resolution_Timeout : Natural;
      --  If zero, inefficient. Otherwise, designates the maximal number of
      --  steps allowed in the resolution of logic equations before
      --  interrupting the resolution because of timeout. See the
      --  Set_Logic_Resolution_Timeout procedure.

      Cache_Version : Version_Number;
      --  Version number used to invalidate memoization caches in a lazy
      --  fashion. If an analysis unit's version number is strictly inferior to
      --  this, its memoization map should be cleared.

      Reparse_Cache_Version : Version_Number;
      --  Version number used to invalidate referenced envs caches. It is
      --  incremented only when a unit is reparsed in the context.

      Rewriting_Handle : Rewriting_Handle_Pointer :=
         No_Rewriting_Handle_Pointer;
      --  Rewriting handle for this context's current rewriting session.
      --  No_Rewriting_Handle_Pointer if there is no such session currently.

      Templates_Unit : Internal_Unit := No_Analysis_Unit;
      --  Special analysis unit used only as a containing unit to parse
      --  templates in the context of tree rewriting.

      Available_Rebindings : Env_Rebindings_Vectors.Vector;
      --  List of allocated-but-unused Env_Rebinding_Type records.
      --
      --  Each rebinding we allocate for an analysis context is deallocated
      --  only when the whole context is released, so when this list is not
      --  empty, we pick one of its element instead of allocating another
      --  rebinding (see the Acquire_Rebindings and Release_Rebindings
      --  subprograms).
      --
      --  Thanks to this mechanism, we have a very simple way to implement
      --  rebindings validity checking for nodes: once we have established that
      --  the node reference is valid regarding its context, we know that the
      --  rebindings pointer is valid, and thus we can just check the rebinding
      --  version number.


      

   end record;

   package Node_To_Named_Env_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Bare_Lkt_Node,
      Element_Type    => Named_Env_Descriptor_Access,
      Hash            => Hash,
      Equivalent_Keys => "=");

   package Boolean_Vectors is new Liblktlang_Support.Vectors (Boolean);

   type Analysis_Unit_Type is limited record
      --  Start of ABI area. In order to perform fast checks from foreign
      --  languages, we maintain minimal ABI for analysis context: this allows
      --  us in language bindings to directly peek in this record rather than
      --  rely on (slow) calls to getters.

      Unit_Version : Version_Number := 0;
      --  Version for this particular unit. This will be incremented every time
      --  a reparse occurs.

      --  End of ABI area

      Context : Internal_Context;
      --  The owning context for this analysis unit

      Is_Internal : Boolean;
      --  Whether this unit is internal.
      --
      --  The use of file readers for parsing is disabled for internal units,
      --  which allows in-memory parsing for them even when a file reader is
      --  active.
      --
      --  It is illegal for users of public APIs to reparse an internal unit.
      --  Setting this flag allows generated libraries to create internal units
      --  to implement language internals and forbid library users to mess with
      --  this unit.

      Ast_Root : Bare_Lkt_Node;

      Filename : GNATCOLL.VFS.Virtual_File;
      --  The originating name for this analysis unit. This should be set even
      --  if the analysis unit was parsed from a buffer.

      Charset : Unbounded_String;
      --  The parsing charset for this analysis unit, as a string. If the
      --  charset used actually came from a byte order mark, this is
      --  nevertheless set to the one the user requested.

      TDH : aliased Token_Data_Handler;
      --  The token data handler that handles all token data during parsing and
      --  owns it afterwards.

      Diagnostics : Diagnostics_Vectors.Vector;
      --  The list of diagnostics produced for this analysis unit

      Rule : Grammar_Rule;
      --  The grammar rule used to parse this unit

      Ast_Mem_Pool : Bump_Ptr_Pool;
      --  This memory pool shall only be used for AST parsing. Stored here
      --  because it is more convenient, but one shall not allocate from it.

      Destroyables : Destroyable_Vectors.Vector;
      --  Collection of objects to destroy when destroying the analysis unit

      Referenced_Units : Analysis_Unit_Sets.Set;
      --  Units that are referenced from this one. Useful for
      --  visibility/computation of the reference graph.

      PLE_Roots_Starting_Token : Token_Index_Vectors.Vector;
      --  If this unit contains a list of PLE roots, then for each PLE root,
      --  this vector contains a reference to the first token that is part of
      --  it. Otherwise, this vector is empty.
      --
      --  This table is initialized after each parsing and allows to quickly
      --  look for the PLE root corresponding to some token, and thus to some
      --  node in this unit (see the ``Lookup_PLE_Root`` function).

      Env_Populated_Roots : Boolean_Vectors.Vector;
      --  For each PLE root in this unit, indicates whether
      --  Populate_Lexical_Env was called on it.
      --
      --  Note that this vector may contain less or more elements than the
      --  number of PLE roots in this unit: this allows not to run PLE twice on
      --  each root, and to keep track on which roots PLE should be run after a
      --  reparse. "Missing" elements in this vector are considered False.

      Exiled_Entries : Exiled_Entry_Vectors.Vector;
      --  Lexical env population for this unit may have added AST nodes it owns
      --  to the lexical environments that belong to other units ("exiled"
      --  entries). For each of these AST nodes, this vector contains an entry
      --  that records the target environment, the AST node and the
      --  corresponding symbol.

      Foreign_Nodes : Foreign_Node_Entry_Vectors.Vector;
      --  This unit owns a set of lexical environments. This vector contains
      --  the list of AST nodes that were added to these environments and that
      --  come from other units.

      Exiled_Entries_In_NED : Exiled_Entry_In_NED_Vectors.Vector;
      --  Like Exiled_Entries, but for symbol/node associations exclusively
      --  handled by the named environments mechanism.
      --
      --  This list allows efficient removal of these entries from
      --  Named_Env_Descriptor.Foreign_Nodes components when unloading this
      --  unit.

      Exiled_Envs : Exiled_Env_Vectors.Vector;
      --  List of lexical environments created in this unit and whose parent is
      --  a named environment.
      --
      --  This list allows efficient removal for these envs from
      --  Named_Env_Descriptor.Foreign_Envs components when unloading this
      --  unit.

      Named_Envs : Named_Env_Vectors.Vector;
      --  List of named environment created in this unit.
      --
      --  This list allows efficient removal for these envs from the
      --  Named_Env_Descriptor.Envs components when unloading this unit.

      Nodes_With_Foreign_Env : Node_To_Named_Env_Maps.Map;
      --  Mapping from a node to its Self_Env's named env descriptor, for each
      --  node in this unit whose Self_Env is a named environment.
      --
      --  This mapping allows efficient removal for these nodes from the
      --  Named_Env_Descriptor.Nodes_With_Foreign_Env components when unloading
      --  this unit.

      Rebindings : aliased Env_Rebindings_Vectors.Vector;
      --  List of rebindings for which Old_Env and/or New_Env belong to this
      --  unit. When this unit gets destroyed or reparsed, these rebindings
      --  need to be destroyed too (see Destroy_Rebindings).

         Memoization_Map : Memoization_Maps.Map;
         --  Mapping of arguments tuple to property result for memoization

      Cache_Version : Version_Number := 0;
      --  See the eponym field in Analysis_Context_Type


      

   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Context_Type, Internal_Context);

   procedure Free is new Ada.Unchecked_Deallocation
     (Analysis_Unit_Type, Internal_Unit);

   type Reparsed_Unit (Present : Boolean := False) is record
      case Present is
         when False => null;
         when True =>
            TDH          : Token_Data_Handler;
            Diagnostics  : Diagnostics_Vectors.Vector;
            Ast_Mem_Pool : Bump_Ptr_Pool;
            Ast_Root     : Bare_Lkt_Node;
      end case;
   end record;
   --  Holder for fields affected by an analysis unit reparse. This makes it
   --  possible to separate the "reparsing" and the "replace" steps.

   procedure Destroy (Reparsed : in out Reparsed_Unit);
   --  Free all resources in Reparsed

   function Basename (Filename : String) return String;
   --  Return the base filename for String

   ----------------------------------------------------
   -- Implementation for analysis context primitives --
   ----------------------------------------------------

   function Allocate_Context return Internal_Context;
   --  Allocate a new analysis context.

   procedure Initialize_Context
     (Context        : Internal_Context;
      Charset        : String;
      File_Reader    : Internal_File_Reader_Access;
      Unit_Provider  : Internal_Unit_Provider_Access;
      Event_Handler  : Internal_Event_Handler_Access;
      With_Trivia    : Boolean;
      Tab_Stop       : Positive);
   --  Initialize an analysis context. Must be called right after
   --  ``Allocate_Context`` on its result.
   --
   --  Having separate primitives for allocation/initialization allows library
   --  bindings to have a context wrapper (created between the two calls) ready
   --  when callbacks that happen during context initialization (for instance
   --  "unit parsed" events).
   --  Implementation for ``Analysis.Create_Context``: call
   --  ``Allocate_Context`` to allocate an ``Internal_Context`` value, then
   --  call ``Initialize_Context`` to initialize it.
   --
   --  Having separate primitives for allocation/initialization allows library
   --  bindings to have a context wrapper (created between the two calls) ready
   --  when callbacks that happen during context initialization (for instance
   --  "unit parsed" events).

   function Create_Unit
     (Context             : Internal_Context;
      Normalized_Filename : GNATCOLL.VFS.Virtual_File;
      Charset             : String;
      Rule                : Grammar_Rule) return Internal_Unit
      with Pre => not Has_Unit (Context, +Normalized_Filename.Full_Name);
   --  Create a new analysis unit and register it in Context

   function Get_Unit
     (Context           : Internal_Context;
      Filename, Charset : String;
      Reparse           : Boolean;
      Input             : Internal_Lexer_Input;
      Rule              : Grammar_Rule;
      Is_Internal       : Boolean := False) return Internal_Unit;
   --  Helper for Get_From_File and Get_From_Buffer. Return the resulting
   --  analysis unit.
   --
   --  If ``Is_Internal`` is True, allow parsing from buffer even if
   --  ``Context`` has a file reader, and forbid later calls to
   --  Get_From_File/Get_From_Buffer/Reparse on the returned unit.

   function Has_Unit
     (Context : Internal_Context; Unit_Filename : String) return Boolean;
   --  Implementation for Analysis.Has_Unit

   function Get_From_File
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Reparse  : Boolean;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_From_File

   function Get_From_Buffer
     (Context  : Internal_Context;
      Filename : String;
      Charset  : String;
      Buffer   : String;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_From_Buffer

   function Get_With_Error
     (Context  : Internal_Context;
      Filename : String;
      Error    : Text_Type;
      Charset  : String;
      Rule     : Grammar_Rule) return Internal_Unit;
   --  Implementation for Analysis.Get_With_Error


   function Get_From_Provider
     (Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String;
      Reparse : Boolean) return Internal_Unit;
   --  Implementation for Analysis.Get_From_Provider


   function Unit_Provider
     (Context : Internal_Context) return Internal_Unit_Provider_Access;
   --  Implementation for Analysis.Unit_Provider

   procedure Resolve_Unit
     (Context : Internal_Context;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Unit    : out Resolved_Unit);
   --  Completely resolve the requested unit. The result is cached: later calls
   --  for the same name/kind will have constant complexity.

   procedure Get_Unit_Location
     (Context        : Internal_Context;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : out String_Access;
      PLE_Root_Index : out Positive);
   --  Caching wrapper around Context.Unit_Provider.Get_Unit_Location

   procedure Get_Unit_And_PLE_Root
     (Context        : Internal_Context;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Unit           : out Internal_Unit;
      PLE_Root_Index : out Positive);
   --  Caching wrapper around Context.Unit_Provider.Get_Unit_And_PLE_Root

   function Hash (Context : Internal_Context) return Hash_Type;
   --  Implementation for Analysis.Hash

   function Has_With_Trivia (Context : Internal_Context) return Boolean;
   --  Implementation for Analysis.Has_With_Trivia

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Internal_Context; Discard : Boolean);
   --  Implementation for Analysis.Discard_Errors_In_Populate_Lexical_Env

   procedure Set_Logic_Resolution_Timeout
     (Context : Internal_Context; Timeout : Natural);
   --  Implementation for Analysis.Set_Logic_Resolution_Timeout

   function Has_Rewriting_Handle (Context : Internal_Context) return Boolean;
   --  Implementation for Analysis.Has_Rewriting_Handle

   procedure Inc_Ref (Context : Internal_Context);
   --  Increment the ref-count of Context. This does nothing if Context is
   --  null.

   procedure Dec_Ref (Context : in out Internal_Context);
   --  Decrement the ref-count of Context, destroying it if the ref-count
   --  reaches zero. This does nothing if Context is null.

   procedure Destroy (Context : Internal_Context)
      with Pre => not Has_Rewriting_Handle (Context);
   --  Free all resources allocated for Context

   -------------------------------------------------
   -- Implementation for analysis unit primitives --
   -------------------------------------------------

   function Context (Unit : Internal_Unit) return Internal_Context;
   --  Implementation for Analysis.Context

   function Hash (Unit : Internal_Unit) return Hash_Type;
   --  Implementation for Analysis.Hash

   procedure Reparse (Unit : Internal_Unit; Charset : String);
   --  Implementation for Analysis.Reparse

   procedure Reparse
     (Unit : Internal_Unit; Charset : String; Buffer  : String);
   --  Implementation for Analysis.Reparse

   procedure Populate_Lexical_Env
     (Unit           : Internal_Unit;
      PLE_Root_Index : Positive
         := 1
      );
   --  Implementation for Analysis.Populate_Lexical_Env

   procedure Populate_Lexical_Env_For_Unit (Node : Bare_Lkt_Node);
   --  Populate the lexical environment for the PLE root that owns ``Node``, or
   --  for the whole unit if there is no PLE root.

   function Get_Filename (Unit : Internal_Unit) return String;
   --  Implementation for Analysis.Get_Filename

   function Get_Charset (Unit : Internal_Unit) return String;
   --  Implementation for Analysis.Get_Charset

   function Has_Diagnostics (Unit : Internal_Unit) return Boolean;
   --  Implementation for Analysis.Has_Diagnostics

   function Diagnostics (Unit : Internal_Unit) return Diagnostics_Array;
   --  Implementation for Analysis.Diagnostics

   function Format_GNU_Diagnostic
     (Unit : Internal_Unit; D : Diagnostic) return String;
   --  Implementation for Analysis.Format_GNU_Diagnostic

   function Root (Unit : Internal_Unit) return Bare_Lkt_Node;
   --  Implementation for Analysis.Root

   function First_Token (Unit : Internal_Unit) return Token_Reference;
   --  Implementation for Analysis.First_Token

   function Last_Token (Unit : Internal_Unit) return Token_Reference;
   --  Implementation for Analysis.Last_Token

   function Token_Count (Unit : Internal_Unit) return Natural;
   --  Implementation for Analysis.Token_Count

   function Trivia_Count (Unit : Internal_Unit) return Natural;
   --  Implementation for Analysis.Trivia_Count

   function Text (Unit : Internal_Unit) return Text_Type;
   --  Implementation for Analysis.Text

   function Lookup_Token
     (Unit : Internal_Unit; Sloc : Source_Location) return Token_Reference;
   --  Implementation for Analysis.Lookup_Token

   procedure Lookup_PLE_Root
     (Node  : Bare_Lkt_Node;
      Root  : out Bare_Lkt_Node;
      Index : out Natural);
   --  Look for the PLE root that owns this node. If there is one, assign it to
   --  ``Root`` and assign its index in the list of PLE roots to ``Index``. If
   --  there is none, set ``Root`` to the unit root node and ``Index`` to 0.

   procedure Dump_Lexical_Env (Unit : Internal_Unit);
   --  Implementation for Analysis.Dump_Lexical_Env

   procedure Print (Unit : Internal_Unit; Show_Slocs : Boolean);
   --  Implementation for Analysis.Print

   procedure PP_Trivia (Unit : Internal_Unit);
   --  Implementation for Analysis.PP_Trivia

   procedure Destroy (Unit : in out Internal_Unit);
   --  TODO???

   function Basename (Unit : Internal_Unit) return String;
   --  Return the base filename for Unit

   procedure Invalidate_Caches
     (Context : Internal_Context; Invalidate_Envs : Boolean);
   --  Invalidate memoization caches. If Invalidate_Envs is true, also
   --  invalidate referenced envs caches.

   procedure Reset_Caches (Unit : Internal_Unit);
   --  Destroy Unit's memoization cache. This resets Unit's version number to
   --  Unit.Context.Cache_Version.

   procedure Reference_Unit (From, Referenced : Internal_Unit);
   --  Set the Referenced unit as being referenced from the From unit. This is
   --  useful for visibility purposes, and is mainly meant to be used in the
   --  env hooks.

   function Get_Line
     (Unit : Internal_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   function Is_Referenced_From
     (Self, Unit : Internal_Unit) return Boolean;

   procedure Do_Parsing
     (Unit   : Internal_Unit;
      Input  : Internal_Lexer_Input;
      Result : out Reparsed_Unit);
   --  Parse text for Unit using Input and store the result in Result. This
   --  leaves Unit unchanged.

   procedure Update_After_Reparse
     (Unit : Internal_Unit; Reparsed : in out Reparsed_Unit);
   --  Update Unit's AST from Reparsed and update stale lexical environment
   --  data after the reparsing of Unit.

   procedure Destroy_Unit_Destroyables (Unit : Internal_Unit);
   --  Destroy all destroyables objects in Unit and clear this list in Unit

   procedure Remove_Exiled_Entries (Unit : Internal_Unit);
   --  Remove lexical environment entries referencing nodes in Unit from
   --  lexical environments Unit does not own. Remove foreign node entries in
   --  foreign units that correspond to these exiled entries. Clear
   --  Unit.Exiled_Entries afterwards.

   procedure Remove_Named_Envs
     (Unit                      : Internal_Unit;
      Named_Envs_Needing_Update : in out NED_Maps.Map);
   --  Remove envs that belong to Unit from all relevant NEDs, and keep track
   --  in Named_Env_Needing_Update of the env names whose env with precedence
   --  must change because of this.

   procedure Extract_Foreign_Nodes
     (Unit          : Internal_Unit;
      Foreign_Nodes : in out Bare_Lkt_Node_Vectors.Vector);
   --  Collect in Foreign_Nodes all foreign nodes in Unit's lexical
   --  environments (i.e. lexical env entries that refer to nodes which belongs
   --  to other analysis units). Remove the exiled entries in foreign units
   --  that correspond to these foreign nodes. Clear Unit.Foreign_Nodes
   --  afterwards.

   procedure Reroot_Foreign_Node (Node : Bare_Lkt_Node);
   --  Re-create the lexical env entry for Node. This is to be used in
   --  Flush_Populate_Lexical_Env_Queue, after reparsing removed the target
   --  lexical environment.

   procedure Destroy_Rebindings
     (Rebindings : access Env_Rebindings_Vectors.Vector);
   --  Destroy all rebindings in Rebindings, plus their child rebindings. Note
   --  that children can belong to various analysis units, so this takes care
   --  of removing the destroyed rebindings from each concerned analysis unit's
   --  Rebindings vector.
   --
   --  This require an access parameter in order to avoid aliasing issues in
   --  the body.

   function Get_Rewriting_Handle
     (Context : Internal_Context) return Rewriting_Handle_Pointer;
   --  Return the Rewriting_Handle component of Context

   procedure Set_Rewriting_Handle
     (Context : Internal_Context; Handle : Rewriting_Handle_Pointer);
   --  Set the Rewriting_Handle component of Context

   type Node_Safety_Net is record
      Context        : Internal_Context;
      Context_Serial : Version_Number;
      --  Analysis context and serial number at the time this safety net was
      --  produced.

      Unit         : Internal_Unit;
      Unit_Version : Version_Number;
      --  Analysis unit and unit version at the time this safety net was
      --  produced.

      Rebindings_Version : Version_Number;
      --  Version of the associated rebinding at the time this safety net was
      --  procuded.
   end record;
   --  Information to embed in public APIs, used to check before accessing data
   --  that the said-data is still valid.

   No_Node_Safety_Net : constant Node_Safety_Net := (null, 0, null, 0, 0);

   function String_To_Symbol
     (Self    : Bare_Lkt_Node;
      Context : Internal_Context;
      S       : String_Type) return Symbol_Type;
   --  Convert ``S`` into the corresponding symbol, raising a
   --  ``Property_Error`` if symbol canonicalization fails (using ``Self`` to
   --  provide context for this error). If ``S`` is empty, just return
   --  ``null``.

   function Solve_Wrapper
     (R            : Solver.Relation;
      Context_Node : Bare_Lkt_Node) return Boolean;
   --  Wrapper for Liblktlang_Support.Adalog.Solve; will handle setting the debug
   --  strings in the equation if in debug mode.

   function Solve_With_Diagnostics
     (R            : Solver.Relation;
      Context_Node : Bare_Lkt_Node) return Internal_Solver_Result;
   --  Like ``Solve_Wrapper``, but returns a ``Internal_Solver_Result`` which
   --  contains solver diagnostics in case of resolution failure.

   generic
      type T (<>) is limited private;
      type T_Access is access all T;
      with procedure Destroy (Object : in out T_Access);
   procedure Register_Destroyable_Gen
     (Unit : Internal_Unit; Object : T_Access);
   --  Generic procedure to register an object so that it is automatically
   --  destroyed when Unit is destroyed.

   function New_Unit_String
     (Unit : Internal_Unit; Str : String) return String_Access;
   --  This function allocates a string whose lifetime will be associated with
   --  ``Unit``.

private
   --  We only have a private part to defer the initialization of struct
   --  constants. This allows us to circumvent circularity problems between
   --  arrays and structs.

         
      


      No_Decoded_Char_Value : constant Internal_Decoded_Char_Value :=
      (
               Value => Chars.NUL, 
               Has_Error => False, 
               Error_Sloc => No_Source_Location, 
               Error_Message => Empty_String
      );

         
      


      No_Decoded_String_Value : constant Internal_Decoded_String_Value :=
      (
               Value => Empty_String, 
               Has_Error => False, 
               Error_Sloc => No_Source_Location, 
               Error_Message => Empty_String
      );

         
      


      No_Designated_Env : constant Internal_Designated_Env :=
      (
               Kind => None, 
               Env_Name => No_Symbol, 
               Direct_Env => Empty_Env
      );

         

         

         
      


      No_Entity_Expr : constant Internal_Entity_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Any_Of : constant Internal_Entity_Any_Of :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lkt_Node_Base_List : constant Internal_Entity_Lkt_Node_Base_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Expr_List : constant Internal_Entity_Expr_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Any_Of_List : constant Internal_Entity_Any_Of_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Decl : constant Internal_Entity_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Type_Decl : constant Internal_Entity_Type_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Any_Type_Decl : constant Internal_Entity_Any_Type_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Array_Literal : constant Internal_Entity_Array_Literal :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Base_Call_Expr : constant Internal_Entity_Base_Call_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Base_Dot_Expr : constant Internal_Entity_Base_Dot_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Base_Grammar_Rule_Decl : constant Internal_Entity_Base_Grammar_Rule_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Base_Lexer_Case_Rule_Alt : constant Internal_Entity_Base_Lexer_Case_Rule_Alt :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Base_Lexer_Case_Rule_Alt_List : constant Internal_Entity_Base_Lexer_Case_Rule_Alt_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Base_Val_Decl : constant Internal_Entity_Base_Val_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Named_Type_Decl : constant Internal_Entity_Named_Type_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Basic_Class_Decl : constant Internal_Entity_Basic_Class_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lit : constant Internal_Entity_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Big_Num_Lit : constant Internal_Entity_Big_Num_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Bin_Op : constant Internal_Entity_Bin_Op :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lkt_Node_List : constant Internal_Entity_Lkt_Node_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Block_Decl_List : constant Internal_Entity_Block_Decl_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Block_Expr : constant Internal_Entity_Block_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Block_String_Line : constant Internal_Entity_Block_String_Line :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Block_String_Line_List : constant Internal_Entity_Block_String_Line_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_String_Lit : constant Internal_Entity_String_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Block_String_Lit : constant Internal_Entity_Block_String_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Call_Expr : constant Internal_Entity_Call_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Call_Expr_List : constant Internal_Entity_Call_Expr_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Cast_Expr : constant Internal_Entity_Cast_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Char_Lit : constant Internal_Entity_Char_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Class_Decl : constant Internal_Entity_Class_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Class_Qualifier : constant Internal_Entity_Class_Qualifier :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Class_Qualifier_Absent : constant Internal_Entity_Class_Qualifier_Absent :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Class_Qualifier_Present : constant Internal_Entity_Class_Qualifier_Present :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_User_Val_Decl : constant Internal_Entity_User_Val_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Explicitly_Typed_Decl : constant Internal_Entity_Explicitly_Typed_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Component_Decl : constant Internal_Entity_Component_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Decl_Annotation : constant Internal_Entity_Decl_Annotation :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Decl_Annotation_List : constant Internal_Entity_Decl_Annotation_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Decl_Annotation_Params : constant Internal_Entity_Decl_Annotation_Params :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Full_Decl_List : constant Internal_Entity_Full_Decl_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Decl_Block : constant Internal_Entity_Decl_Block :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Id : constant Internal_Entity_Id :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Def_Id : constant Internal_Entity_Def_Id :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Type_Ref : constant Internal_Entity_Type_Ref :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Default_List_Type_Ref : constant Internal_Entity_Default_List_Type_Ref :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Dot_Expr : constant Internal_Entity_Dot_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Dyn_Env_Wrapper : constant Internal_Entity_Dyn_Env_Wrapper :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Dyn_Var_Decl : constant Internal_Entity_Dyn_Var_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Elsif_Branch : constant Internal_Entity_Elsif_Branch :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Elsif_Branch_List : constant Internal_Entity_Elsif_Branch_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Enum_Class_Alt_Decl : constant Internal_Entity_Enum_Class_Alt_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Enum_Class_Alt_Decl_List : constant Internal_Entity_Enum_Class_Alt_Decl_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Enum_Class_Case : constant Internal_Entity_Enum_Class_Case :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Enum_Class_Case_List : constant Internal_Entity_Enum_Class_Case_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Enum_Class_Decl : constant Internal_Entity_Enum_Class_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Enum_Lit_Decl : constant Internal_Entity_Enum_Lit_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Enum_Lit_Decl_List : constant Internal_Entity_Enum_Lit_Decl_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Enum_Type_Decl : constant Internal_Entity_Enum_Type_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Env_Spec_Decl : constant Internal_Entity_Env_Spec_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Error_On_Null : constant Internal_Entity_Error_On_Null :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Excludes_Null : constant Internal_Entity_Excludes_Null :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Excludes_Null_Absent : constant Internal_Entity_Excludes_Null_Absent :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Excludes_Null_Present : constant Internal_Entity_Excludes_Null_Present :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Field_Decl : constant Internal_Entity_Field_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Full_Decl : constant Internal_Entity_Full_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Fun_Arg_Decl : constant Internal_Entity_Fun_Arg_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Fun_Arg_Decl_List : constant Internal_Entity_Fun_Arg_Decl_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Fun_Decl : constant Internal_Entity_Fun_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Function_Type : constant Internal_Entity_Function_Type :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Function_Type_Ref : constant Internal_Entity_Function_Type_Ref :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Generic_Decl : constant Internal_Entity_Generic_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Generic_Formal_Decl_List : constant Internal_Entity_Generic_Formal_Decl_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Generic_Formal_Type_Decl : constant Internal_Entity_Generic_Formal_Type_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Generic_Instantiation : constant Internal_Entity_Generic_Instantiation :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Generic_Type_Ref : constant Internal_Entity_Generic_Type_Ref :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Expr : constant Internal_Entity_Grammar_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Cut : constant Internal_Entity_Grammar_Cut :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Decl : constant Internal_Entity_Grammar_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Discard : constant Internal_Entity_Grammar_Discard :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Dont_Skip : constant Internal_Entity_Grammar_Dont_Skip :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Expr_List : constant Internal_Entity_Grammar_Expr_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Expr_List_List : constant Internal_Entity_Grammar_Expr_List_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Pick : constant Internal_Entity_Grammar_Pick :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Implicit_Pick : constant Internal_Entity_Grammar_Implicit_Pick :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_List : constant Internal_Entity_Grammar_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_List_Sep : constant Internal_Entity_Grammar_List_Sep :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Null : constant Internal_Entity_Grammar_Null :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Opt : constant Internal_Entity_Grammar_Opt :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Opt_Error : constant Internal_Entity_Grammar_Opt_Error :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Opt_Error_Group : constant Internal_Entity_Grammar_Opt_Error_Group :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Opt_Group : constant Internal_Entity_Grammar_Opt_Group :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Or_Expr : constant Internal_Entity_Grammar_Or_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Predicate : constant Internal_Entity_Grammar_Predicate :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Rule_Decl : constant Internal_Entity_Grammar_Rule_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Rule_Ref : constant Internal_Entity_Grammar_Rule_Ref :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Skip : constant Internal_Entity_Grammar_Skip :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Grammar_Stop_Cut : constant Internal_Entity_Grammar_Stop_Cut :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_If_Expr : constant Internal_Entity_If_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Import : constant Internal_Entity_Import :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Import_List : constant Internal_Entity_Import_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Isa : constant Internal_Entity_Isa :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Type_Ref_List : constant Internal_Entity_Type_Ref_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Isa_List : constant Internal_Entity_Isa_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Keep_Expr : constant Internal_Entity_Keep_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lambda_Arg_Decl : constant Internal_Entity_Lambda_Arg_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lambda_Arg_Decl_List : constant Internal_Entity_Lambda_Arg_Decl_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lambda_Expr : constant Internal_Entity_Lambda_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Langkit_Root : constant Internal_Entity_Langkit_Root :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lexer_Case_Rule : constant Internal_Entity_Lexer_Case_Rule :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lexer_Case_Rule_Cond_Alt : constant Internal_Entity_Lexer_Case_Rule_Cond_Alt :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lexer_Case_Rule_Default_Alt : constant Internal_Entity_Lexer_Case_Rule_Default_Alt :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lexer_Case_Rule_Send : constant Internal_Entity_Lexer_Case_Rule_Send :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lexer_Decl : constant Internal_Entity_Lexer_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Lexer_Family_Decl : constant Internal_Entity_Lexer_Family_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_List_Kind : constant Internal_Entity_List_Kind :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_List_Kind_One : constant Internal_Entity_List_Kind_One :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_List_Kind_Zero : constant Internal_Entity_List_Kind_Zero :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Logic_Assign : constant Internal_Entity_Logic_Assign :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Logic_Call_Expr : constant Internal_Entity_Logic_Call_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Logic_Expr : constant Internal_Entity_Logic_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Logic_Predicate : constant Internal_Entity_Logic_Predicate :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Logic_Propagate : constant Internal_Entity_Logic_Propagate :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Logic_Propagate_Call : constant Internal_Entity_Logic_Propagate_Call :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Logic_Unify : constant Internal_Entity_Logic_Unify :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Match_Branch : constant Internal_Entity_Match_Branch :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Match_Branch_List : constant Internal_Entity_Match_Branch_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Match_Expr : constant Internal_Entity_Match_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Match_Val_Decl : constant Internal_Entity_Match_Val_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Module_Ref_Id : constant Internal_Entity_Module_Ref_Id :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Node_Decl : constant Internal_Entity_Node_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Not_Expr : constant Internal_Entity_Not_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Null_Cond_Dotted_Name : constant Internal_Entity_Null_Cond_Dotted_Name :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Subscript_Expr : constant Internal_Entity_Subscript_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Null_Cond_Subscript_Expr : constant Internal_Entity_Null_Cond_Subscript_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Null_Lit : constant Internal_Entity_Null_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Num_Lit : constant Internal_Entity_Num_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op : constant Internal_Entity_Op :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Amp : constant Internal_Entity_Op_Amp :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_And : constant Internal_Entity_Op_And :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Div : constant Internal_Entity_Op_Div :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Eq : constant Internal_Entity_Op_Eq :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Gt : constant Internal_Entity_Op_Gt :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Gte : constant Internal_Entity_Op_Gte :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Logic_And : constant Internal_Entity_Op_Logic_And :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Logic_Or : constant Internal_Entity_Op_Logic_Or :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Lt : constant Internal_Entity_Op_Lt :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Lte : constant Internal_Entity_Op_Lte :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Minus : constant Internal_Entity_Op_Minus :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Mult : constant Internal_Entity_Op_Mult :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Ne : constant Internal_Entity_Op_Ne :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Or : constant Internal_Entity_Op_Or :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Or_Int : constant Internal_Entity_Op_Or_Int :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Op_Plus : constant Internal_Entity_Op_Plus :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Param : constant Internal_Entity_Param :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Param_List : constant Internal_Entity_Param_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Paren_Expr : constant Internal_Entity_Paren_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Parse_Node_Expr : constant Internal_Entity_Parse_Node_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Single_Line_String_Lit : constant Internal_Entity_Single_Line_String_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Pattern_Single_Line_String_Lit : constant Internal_Entity_Pattern_Single_Line_String_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Raise_Expr : constant Internal_Entity_Raise_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ref_Id : constant Internal_Entity_Ref_Id :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Ref_Id_List : constant Internal_Entity_Ref_Id_List :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Self_Decl : constant Internal_Entity_Self_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Simple_Type_Ref : constant Internal_Entity_Simple_Type_Ref :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Struct_Decl : constant Internal_Entity_Struct_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Synth_Arg_Decl : constant Internal_Entity_Synth_Arg_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Synth_Fun_Decl : constant Internal_Entity_Synth_Fun_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Synthetic_Lexer_Decl : constant Internal_Entity_Synthetic_Lexer_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Token_Lit : constant Internal_Entity_Token_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Token_No_Case_Lit : constant Internal_Entity_Token_No_Case_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Token_Pattern_Concat : constant Internal_Entity_Token_Pattern_Concat :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Token_Pattern_Lit : constant Internal_Entity_Token_Pattern_Lit :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Token_Ref : constant Internal_Entity_Token_Ref :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Trait_Decl : constant Internal_Entity_Trait_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Try_Expr : constant Internal_Entity_Try_Expr :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Un_Op : constant Internal_Entity_Un_Op :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Val_Decl : constant Internal_Entity_Val_Decl :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Entity_Var_Bind : constant Internal_Entity_Var_Bind :=
      (
               Node => No_Bare_Lkt_Node, 
               Info => No_Entity_Info
      );

         
      


      No_Env_Assoc : constant Internal_Env_Assoc :=
      (
               Key => No_Symbol, 
               Value => No_Bare_Lkt_Node, 
               Dest_Env => No_Designated_Env, 
               Metadata => No_Metadata
      );

         
      


      No_Formal_Param : constant Internal_Formal_Param :=
      (
               Formal_Name => No_Symbol, 
               Formal_Type => No_Entity_Type_Decl, 
               Has_Default_Value => False, 
               Accept_Logical_Var => False, 
               Decl => No_Entity_Decl
      );

         
      


      No_Logic_Context : constant Internal_Logic_Context :=
      (
               Ref_Node => No_Entity, 
               Decl_Node => No_Entity
      );

         
      


      No_Param_Match : constant Internal_Param_Match :=
      (
               Has_Matched => False, 
               Actual => No_Entity_Param, 
               Formal => No_Formal_Param
      );

         
      


      No_Solver_Diagnostic : constant Internal_Solver_Diagnostic :=
      (
               Message_Template => Empty_String, 
               Args => No_Internal_Entity_Array_Type, 
               Location => No_Bare_Lkt_Node, 
               Contexts => No_Internal_Logic_Context_Array_Type, 
               Round => 0
      );

         
      


      No_Solver_Result : constant Internal_Solver_Result :=
      (
               Success => False, 
               Diagnostics => No_Internal_Solver_Diagnostic_Array_Type
      );


end Liblktlang.Implementation;
