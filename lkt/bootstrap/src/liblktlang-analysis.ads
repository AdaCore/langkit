







with Ada.Containers;
private with Ada.Containers.Vectors;
private with Ada.Finalization;
with Ada.Strings.Unbounded;
   private with Ada.Unchecked_Deallocation;

with GNATCOLL.Refcount;

   private with Liblktlang_Support.Boxes;

with Liblktlang_Support.File_Readers; use Liblktlang_Support.File_Readers;
with Liblktlang_Support.Lexical_Envs; use Liblktlang_Support.Lexical_Envs;

with Liblktlang_Support.Token_Data_Handlers;
use Liblktlang_Support.Token_Data_Handlers;

with Liblktlang.Common; use Liblktlang.Common;
private with Liblktlang.Implementation;
private with Liblktlang.Debug;




--  This package provides types and primitives to analyze source files as
--  analysis units.
--
--  This is the entry point to parse and process a unit:
--
--  * First create an analysis context with
--    :ada:ref:`Liblktlang.Analysis.Create_Context`.
--
--  * Then get analysis units out of it using the ``Get_From_*`` functions. The
--    most used of them is :ada:ref:`Liblktlang.Analysis.Get_From_File`,
--    which allows you to get an analysis unit out of a file path.
--
--  .. code-block:: ada
--
--      with Libadalang.Analysis;
--
--      procedure Main is
--         package Lib renames Liblktlang.Analysis;
--
--         Context : constant Lib.Analysis_Context := Lib.Create_Context;
--         Unit    : constant Lib.Analysis_Unit :=
--           Context.Get_From_File ("/path/to/source/file");
--      begin
--         Unit.Print;
--      end Main;


package Liblktlang.Analysis is

   pragma Extensions_Allowed (On);

   use Support.Diagnostics, Support.Slocs, Support.Text;

   type Analysis_Context is tagged private;
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

   type Analysis_Unit is new Liblktlang_Support.Text.Text_Buffer_Ifc with private;
   --  This type represents the analysis of a single file.
   --
   --  This type has strong-reference semantics and is ref-counted.
   --  Furthermore, a reference to a unit contains an implicit reference to the
   --  context that owns it. This means that keeping a reference to a unit will
   --  keep the context and all the unit it contains allocated.

   No_Analysis_Context : constant Analysis_Context;
   --  Special value to mean the absence of analysis context

   No_Analysis_Unit : constant Analysis_Unit;
   --  Special value to mean the absence of analysis unit. No analysis units
   --  can be passed this value.

   ---------------
   -- AST nodes --
   ---------------

   pragma Warnings
     (Off, """First_Controlling_Parameter"" is not a valid aspect identifier");
   --  TODO???  (eng/libadalang/libadalang#1374) Remove this pragma once all
   --  supported GNAT versions have support for the First_Controlling_Parameter
   --  aspect.

      type Lkt_Node is tagged private with First_Controlling_Parameter;
      --  Data type for all nodes. Nodes are assembled to make up a tree.  See
      --  the node primitives below to inspect such trees.
      --
      --  Unlike for contexts and units, this type has weak-reference
      --  semantics: keeping a reference to a node has no effect on the
      --  decision to keep the unit that it owns allocated. This means that
      --  once all references to the context and units related to a node are
      --  dropped, the context and its units are deallocated and the node
      --  becomes a stale reference: most operations on it will raise a
      --  ``Stale_Reference_Error``.
      --
      --  Note that since reparsing an analysis unit deallocates all the nodes
      --  it contains, this operation makes all reference to these nodes stale
      --  as well.
      --
      --  Root node class for lkt AST nodes.
      --
      --  Derived nodes: :ada:ref:`Argument`,
      --  :ada:ref:`Base_Lexer_Case_Rule_Alt`, :ada:ref:`Base_Pattern`,
      --  :ada:ref:`Block_String_Line`, :ada:ref:`Class_Qualifier`,
      --  :ada:ref:`Decl_Annotation_Args`, :ada:ref:`Decl_Annotation`,
      --  :ada:ref:`Decl`, :ada:ref:`Dyn_Env_Wrapper`, :ada:ref:`Elsif_Branch`,
      --  :ada:ref:`Enum_Class_Case`, :ada:ref:`Excludes_Null`,
      --  :ada:ref:`Expr`, :ada:ref:`Full_Decl`, :ada:ref:`Grammar_List_Sep`,
      --  :ada:ref:`Import`, :ada:ref:`Langkit_Root`,
      --  :ada:ref:`Lexer_Case_Rule_Send`, :ada:ref:`Lexer_Case_Rule`,
      --  :ada:ref:`List_Kind`, :ada:ref:`Lkt_Node_Base_List`,
      --  :ada:ref:`Match_Branch`, :ada:ref:`Node_Pattern_Detail`,
      --  :ada:ref:`Null_Cond_Qualifier`, :ada:ref:`Op`,
      --  :ada:ref:`Selector_Call`, :ada:ref:`Type_Ref`, :ada:ref:`Var_Bind`

      function Equals (L, R : Lkt_Node) return Boolean;
      --  Comparison function, meant to compare two nodes.
      --
      --  .. note: For complex reasons, we cannot expose this function as the
      --     ``"="`` operator. This is the function you need to use as the
      --     equality function for containers instantiations.
      type Expr is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Base class for expressions. Encompasses regular expressions as well
      --  as special expressions (grammar expressions, etc).
      --
      --  Derived nodes: :ada:ref:`Any_Of`, :ada:ref:`Array_Literal`,
      --  :ada:ref:`Base_Call_Expr`, :ada:ref:`Bin_Op`, :ada:ref:`Block_Expr`,
      --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
      --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Grammar_Expr`,
      --  :ada:ref:`Id`, :ada:ref:`If_Expr`, :ada:ref:`Isa`,
      --  :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`, :ada:ref:`Lit`,
      --  :ada:ref:`Logic_Assign`, :ada:ref:`Logic_Expr`,
      --  :ada:ref:`Logic_Propagate`, :ada:ref:`Logic_Unify`,
      --  :ada:ref:`Match_Expr`, :ada:ref:`Not_Expr`, :ada:ref:`Paren_Expr`,
      --  :ada:ref:`Raise_Expr`, :ada:ref:`Subscript_Expr`,
      --  :ada:ref:`Try_Expr`, :ada:ref:`Un_Op`

      type Any_Of is new Expr with private
         with First_Controlling_Parameter
      ;
      --  "Any of" expression.
      --
      --  This node type has no derivation.

      type Lkt_Node_Base_List is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Derived nodes: :ada:ref:`Argument_List`,
      --  :ada:ref:`Base_Lexer_Case_Rule_Alt_List`,
      --  :ada:ref:`Base_Pattern_List`, :ada:ref:`Block_String_Line_List`,
      --  :ada:ref:`Call_Expr_List`, :ada:ref:`Decl_Annotation_List`,
      --  :ada:ref:`Elsif_Branch_List`, :ada:ref:`Enum_Class_Alt_Decl_List`,
      --  :ada:ref:`Enum_Class_Case_List`, :ada:ref:`Enum_Lit_Decl_List`,
      --  :ada:ref:`Expr_List`, :ada:ref:`Full_Decl_List`,
      --  :ada:ref:`Fun_Param_Decl_List`, :ada:ref:`Grammar_Expr_List_List`,
      --  :ada:ref:`Grammar_Expr_List`, :ada:ref:`Import_List`,
      --  :ada:ref:`Lambda_Param_Decl_List`, :ada:ref:`Lkt_Node_List`,
      --  :ada:ref:`Match_Branch_List`, :ada:ref:`Node_Pattern_Detail_List`,
      --  :ada:ref:`Ref_Id_List`, :ada:ref:`Type_Ref_List`

      type Expr_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Expr_List_First,
                           Next        => Expr_List_Next,
                           Has_Element => Expr_List_Has_Element,
                           Element     => Expr_List_Element)
      ;
      --  List of Expr.
      --
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Any_Of`, :ada:ref:`Array_Literal`, :ada:ref:`Bin_Op`,
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
      --  Derived nodes: :ada:ref:`Any_Of_List`

      type Any_Of_List is new Expr_List with private
         with First_Controlling_Parameter
      ;
      --  Pipe-separated list of expressions.
      --
      --  This is used to represent the "values" operand of an ``AnyOf``
      --  expression.
      --
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`,
      --  :ada:ref:`Call_Expr`, :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`,
      --  :ada:ref:`Error_On_Null`, :ada:ref:`Generic_Instantiation`,
      --  :ada:ref:`If_Expr`, :ada:ref:`Keep_Expr`, :ada:ref:`Lambda_Expr`,
      --  :ada:ref:`Lit`, :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
      --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Raise_Expr`,
      --  :ada:ref:`Ref_Id`, :ada:ref:`Subscript_Expr`, :ada:ref:`Try_Expr`
      --
      --  This node type has no derivation.

      type Decl is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Base class for declarations. Encompasses regular declarations as well
      --  as special declarations such as grammars, grammar rules, etc.
      --
      --  Derived nodes: :ada:ref:`Base_Grammar_Rule_Decl`,
      --  :ada:ref:`Base_Val_Decl`, :ada:ref:`Env_Spec_Decl`,
      --  :ada:ref:`Generic_Decl`, :ada:ref:`Grammar_Decl`,
      --  :ada:ref:`Lexer_Decl`, :ada:ref:`Lexer_Family_Decl`,
      --  :ada:ref:`Synth_Fun_Decl`, :ada:ref:`Synth_Param_Decl`,
      --  :ada:ref:`Type_Decl`

      type Type_Decl is new Decl with private
         with First_Controlling_Parameter
      ;
      --  Abstract base class for type declarations.
      --
      --  Derived nodes: :ada:ref:`Any_Type_Decl`,
      --  :ada:ref:`Enum_Class_Alt_Decl`, :ada:ref:`Function_Type`,
      --  :ada:ref:`Generic_Param_Type_Decl`, :ada:ref:`Named_Type_Decl`

      type Any_Type_Decl is new Type_Decl with private
         with First_Controlling_Parameter
      ;
      --  Internal type to represent a type that can be matched with anything.
      --
      --  This node type has no derivation.

      type Argument is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Argument for function calls or for annotations.
      --
      --  This node type has no derivation.

      type Argument_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Argument_List_First,
                           Next        => Argument_List_Next,
                           Has_Element => Argument_List_Has_Element,
                           Element     => Argument_List_Element)
      ;
      --  List of Argument.
      --
      --  This node type has no derivation.

      type Array_Literal is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Literal for an array value.
      --
      --  This node type has no derivation.

      type Base_Call_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Base class for expressions that are syntactically call-like.
      --
      --  Derived nodes: :ada:ref:`Call_Expr`, :ada:ref:`Logic_Call_Expr`

      type Base_Grammar_Rule_Decl is new Decl with private
         with First_Controlling_Parameter
      ;
      --  Base class for grammar rules inside of grammars/lexers.
      --
      --  Derived nodes: :ada:ref:`Grammar_Rule_Decl`,
      --  :ada:ref:`Synthetic_Lexer_Decl`

      type Base_Lexer_Case_Rule_Alt is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Base class for the different kind of alternatives allowed in a case
      --  rule.
      --
      --  Derived nodes: :ada:ref:`Lexer_Case_Rule_Cond_Alt`,
      --  :ada:ref:`Lexer_Case_Rule_Default_Alt`

      type Base_Lexer_Case_Rule_Alt_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Base_Lexer_Case_Rule_Alt_List_First,
                           Next        => Base_Lexer_Case_Rule_Alt_List_Next,
                           Has_Element => Base_Lexer_Case_Rule_Alt_List_Has_Element,
                           Element     => Base_Lexer_Case_Rule_Alt_List_Element)
      ;
      --  List of BaseLexerCaseRuleAlt.
      --
      --  This node type has no derivation.

      type Base_Pattern is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Root node class for patterns.
      --
      --  This is a mostly LKQL specific node for the moment, as are every node
      --  derived from it.
      --
      --  Derived nodes: :ada:ref:`Binding_Pattern`,
      --  :ada:ref:`Filtered_Pattern`, :ada:ref:`Value_Pattern`

      type Base_Pattern_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Base_Pattern_List_First,
                           Next        => Base_Pattern_List_Next,
                           Has_Element => Base_Pattern_List_Has_Element,
                           Element     => Base_Pattern_List_Element)
      ;
      --  List of BasePattern.
      --
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
      --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
      --  :ada:ref:`Node_Pattern`, :ada:ref:`Not_Pattern`,
      --  :ada:ref:`Null_Pattern`, :ada:ref:`Paren_Pattern`,
      --  :ada:ref:`Regex_Pattern`, :ada:ref:`Splat_Pattern`,
      --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Universal_Pattern`
      --
      --  This node type has no derivation.

      type Base_Val_Decl is new Decl with private
         with First_Controlling_Parameter
      ;
      --  Abstract class for named values declarations, such as parameters,
      --  local value bindings, fields, etc.
      --
      --  Derived nodes: :ada:ref:`Node_Decl`, :ada:ref:`Self_Decl`,
      --  :ada:ref:`User_Val_Decl`

      type Named_Type_Decl is new Type_Decl with private
         with First_Controlling_Parameter
      ;
      --  Explicit named type declaration.
      --
      --  Derived nodes: :ada:ref:`Basic_Class_Decl`,
      --  :ada:ref:`Enum_Type_Decl`, :ada:ref:`Struct_Decl`,
      --  :ada:ref:`Trait_Decl`

      type Basic_Class_Decl is new Named_Type_Decl with private
         with First_Controlling_Parameter
      ;
      --  Common ancestor for declarations of regular classes and enum classes.
      --
      --  Derived nodes: :ada:ref:`Class_Decl`, :ada:ref:`Enum_Class_Decl`

      type Lit is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Base class for literals.
      --
      --  Derived nodes: :ada:ref:`Big_Num_Lit`, :ada:ref:`Char_Lit`,
      --  :ada:ref:`Null_Lit`, :ada:ref:`Num_Lit`, :ada:ref:`String_Lit`

      type Big_Num_Lit is new Lit with private
         with First_Controlling_Parameter
      ;
      --  Big number literal expression.
      --
      --  This node type has no derivation.

      type Bin_Op is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Binary operator expression.
      --
      --  This node type has no derivation.

      type Binding_Pattern is new Base_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern comprising a binding name and a value pattern.
      --
      --  For instance:
      --
      --  .. code::
      --
      --     o@ObjectDecl
      --
      --  This node type has no derivation.

      type Lkt_Node_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Lkt_Node_List_First,
                           Next        => Lkt_Node_List_Next,
                           Has_Element => Lkt_Node_List_Has_Element,
                           Element     => Lkt_Node_List_Element)
      ;
      --  List of LktNode.
      --
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Full_Decl`, :ada:ref:`Lexer_Case_Rule`,
      --  :ada:ref:`Val_Decl`, :ada:ref:`Var_Bind`
      --
      --  Derived nodes: :ada:ref:`Block_Decl_List`

      type Block_Decl_List is new Lkt_Node_List with private
         with First_Controlling_Parameter
      ;
      --  Semicolon-separated list of declarations.
      --
      --  This is used to represent declarations in a block expression.
      --
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Val_Decl`, :ada:ref:`Var_Bind`
      --
      --  This node type has no derivation.

      type Block_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Block expression.
      --
      --  This node type has no derivation.

      type Block_String_Line is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  A single line in a block string literal.
      --
      --  This node type has no derivation.

      type Block_String_Line_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Block_String_Line_List_First,
                           Next        => Block_String_Line_List_Next,
                           Has_Element => Block_String_Line_List_Has_Element,
                           Element     => Block_String_Line_List_Element)
      ;
      --  List of BlockStringLine.
      --
      --  This node type has no derivation.

      type String_Lit is new Lit with private
         with First_Controlling_Parameter
      ;
      --  Base node type for string literals.
      --
      --  Derived nodes: :ada:ref:`Block_String_Lit`,
      --  :ada:ref:`Single_Line_String_Lit`

      type Block_String_Lit is new String_Lit with private
         with First_Controlling_Parameter
      ;
      --  String literal expression, made of multiple line strings.
      --
      --  The denoted string value is the concatenation of all line string
      --  items. Each line string item must be either:
      --
      --  * The empty string designator (``|"``), to denote an empty line
      --    (``\n``).
      --
      --  * ``|" <content>``, to designate a non-empty line. The space before
      --    ``<content>`` is mandatory, and is not included in the denoted
      --    string value. ``<content>`` can be anything that appear in a
      --    regular string literal: escape sequences are interpreted the same
      --    way.
      --
      --  This node type has no derivation.

      type Value_Pattern is new Base_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Root node class for patterns that filter values. (As opposed to
      --  patterns that only bind values to a given name without doing any kind
      --  of filtering)
      --
      --  Derived nodes: :ada:ref:`Bool_Pattern`, :ada:ref:`Integer_Pattern`,
      --  :ada:ref:`List_Pattern`, :ada:ref:`Node_Pattern`,
      --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`,
      --  :ada:ref:`Or_Pattern`, :ada:ref:`Paren_Pattern`,
      --  :ada:ref:`Regex_Pattern`, :ada:ref:`Splat_Pattern`,
      --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Universal_Pattern`

      type Bool_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern to match on booleans.
      --
      --  Derived nodes: :ada:ref:`Bool_Pattern_False`,
      --  :ada:ref:`Bool_Pattern_True`

      type Bool_Pattern_False is new Bool_Pattern with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Bool_Pattern_True is new Bool_Pattern with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Call_Expr is new Base_Call_Expr with private
         with First_Controlling_Parameter
      ;
      --  Call expression.
      --
      --  This node type has no derivation.

      type Call_Expr_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Call_Expr_List_First,
                           Next        => Call_Expr_List_Next,
                           Has_Element => Call_Expr_List_Has_Element,
                           Element     => Call_Expr_List_Element)
      ;
      --  List of CallExpr.
      --
      --  This node type has no derivation.

      type Cast_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Cast expression.
      --
      --  This node type has no derivation.

      type Char_Lit is new Lit with private
         with First_Controlling_Parameter
      ;
      --  Character literal expression.
      --
      --  This node type has no derivation.

      type Class_Decl is new Basic_Class_Decl with private
         with First_Controlling_Parameter
      ;
      --  Declaration for a LK class. This only cover node classes for the
      --  moment, but might be extended to support regular classes in the
      --  future.
      --
      --  This node type has no derivation.

      type Class_Qualifier is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Whether this generic parameter type must be a class.
      --
      --  Derived nodes: :ada:ref:`Class_Qualifier_Absent`,
      --  :ada:ref:`Class_Qualifier_Present`

      type Class_Qualifier_Absent is new Class_Qualifier with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Class_Qualifier_Present is new Class_Qualifier with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type User_Val_Decl is new Base_Val_Decl with private
         with First_Controlling_Parameter
      ;
      --  Class for user declared val declarations (not synthetic).
      --
      --  Derived nodes: :ada:ref:`Enum_Lit_Decl`,
      --  :ada:ref:`Explicitly_Typed_Decl`, :ada:ref:`Fun_Decl`

      type Explicitly_Typed_Decl is new User_Val_Decl with private
         with First_Controlling_Parameter
      ;
      --  Subset of user declared value declarations for values that have a
      --  type that can be syntactically annotated by the user.
      --
      --  Derived nodes: :ada:ref:`Component_Decl`, :ada:ref:`Dyn_Var_Decl`,
      --  :ada:ref:`Match_Val_Decl`, :ada:ref:`Val_Decl`

      type Component_Decl is new Explicitly_Typed_Decl with private
         with First_Controlling_Parameter
      ;
      --  Subset of explicitly typed declarations for value declarations that:
      --
      --  1. Have an optional default value.
      --
      --  2. Are part of a bigger declaration that can be referred to via a
      --     call expression (either a type or a function).
      --
      --  Derived nodes: :ada:ref:`Field_Decl`, :ada:ref:`Fun_Param_Decl`,
      --  :ada:ref:`Lambda_Param_Decl`

      type Decl_Annotation is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Compile time annotation attached to a declaration.
      --
      --  This node type has no derivation.

      type Decl_Annotation_Args is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  List of arguments for an annotation with a call syntax. This
      --  intermediate node is necessary in order to determine after parsing
      --  whether there is no argument list, or if the list is empty.
      --
      --  This node type has no derivation.

      type Decl_Annotation_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Decl_Annotation_List_First,
                           Next        => Decl_Annotation_List_Next,
                           Has_Element => Decl_Annotation_List_Has_Element,
                           Element     => Decl_Annotation_List_Element)
      ;
      --  List of DeclAnnotation.
      --
      --  This node type has no derivation.

      type Full_Decl_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Full_Decl_List_First,
                           Next        => Full_Decl_List_Next,
                           Has_Element => Full_Decl_List_Has_Element,
                           Element     => Full_Decl_List_Element)
      ;
      --  List of FullDecl.
      --
      --  Derived nodes: :ada:ref:`Decl_Block`,
      --  :ada:ref:`Generic_Param_Decl_List`

      type Decl_Block is new Full_Decl_List with private
         with First_Controlling_Parameter
      ;
      --  List of declarations that also introduces a containing lexical scope.
      --
      --  This node type has no derivation.

      type Id is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Identifier.
      --
      --  Derived nodes: :ada:ref:`Def_Id`, :ada:ref:`Module_Ref_Id`,
      --  :ada:ref:`Ref_Id`

      type Def_Id is new Id with private
         with First_Controlling_Parameter
      ;
      --  Defining identifier.
      --
      --  This node type has no derivation.

      type Type_Ref is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Base class for a reference to a type.
      --
      --  Derived nodes: :ada:ref:`Default_List_Type_Ref`,
      --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
      --  :ada:ref:`Simple_Type_Ref`

      type Default_List_Type_Ref is new Type_Ref with private
         with First_Controlling_Parameter
      ;
      --  "list" type reference in parsers.
      --
      --  This node type has no derivation.

      type Dot_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Dotted expression.
      --
      --  This node type has no derivation.

      type Dyn_Env_Wrapper is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Synthetic node to instantiate a DynamicEnvironment for generics.
      --
      --  This node type has no derivation.

      type Dyn_Var_Decl is new Explicitly_Typed_Decl with private
         with First_Controlling_Parameter
      ;
      --  Dynamic variable declaration.
      --
      --  This node type has no derivation.

      type Elsif_Branch is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Elsif branch of an if expression.
      --
      --  This node type has no derivation.

      type Elsif_Branch_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Elsif_Branch_List_First,
                           Next        => Elsif_Branch_List_Next,
                           Has_Element => Elsif_Branch_List_Has_Element,
                           Element     => Elsif_Branch_List_Element)
      ;
      --  List of ElsifBranch.
      --
      --  This node type has no derivation.

      type Enum_Class_Alt_Decl is new Type_Decl with private
         with First_Controlling_Parameter
      ;
      --  Alternative for an enum class decl.
      --
      --  This node type has no derivation.

      type Enum_Class_Alt_Decl_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Enum_Class_Alt_Decl_List_First,
                           Next        => Enum_Class_Alt_Decl_List_Next,
                           Has_Element => Enum_Class_Alt_Decl_List_Has_Element,
                           Element     => Enum_Class_Alt_Decl_List_Element)
      ;
      --  List of EnumClassAltDecl.
      --
      --  This node type has no derivation.

      type Enum_Class_Case is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Case branch for an enum class declaration.
      --
      --  This node type has no derivation.

      type Enum_Class_Case_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Enum_Class_Case_List_First,
                           Next        => Enum_Class_Case_List_Next,
                           Has_Element => Enum_Class_Case_List_Has_Element,
                           Element     => Enum_Class_Case_List_Element)
      ;
      --  List of EnumClassCase.
      --
      --  This node type has no derivation.

      type Enum_Class_Decl is new Basic_Class_Decl with private
         with First_Controlling_Parameter
      ;
      --  Declaration for a LK class. This only cover node classes for the
      --  moment, but might be extended to support regular classes in the
      --  future.
      --
      --  This node type has no derivation.

      type Enum_Lit_Decl is new User_Val_Decl with private
         with First_Controlling_Parameter
      ;
      --  Enum literal declaration.
      --
      --  This node type has no derivation.

      type Enum_Lit_Decl_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Enum_Lit_Decl_List_First,
                           Next        => Enum_Lit_Decl_List_Next,
                           Has_Element => Enum_Lit_Decl_List_Has_Element,
                           Element     => Enum_Lit_Decl_List_Element)
      ;
      --  List of EnumLitDecl.
      --
      --  This node type has no derivation.

      type Enum_Type_Decl is new Named_Type_Decl with private
         with First_Controlling_Parameter
      ;
      --  Enum type declaration.
      --
      --  This node type has no derivation.

      type Env_Spec_Decl is new Decl with private
         with First_Controlling_Parameter
      ;
      --  Env spec declaration.
      --
      --  Each node type can have one or no env spec. Env specs contains only a
      --  list of env actions.
      --
      --  This node type has no derivation.

      type Error_On_Null is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Expression that throws an error if LHS is null.
      --
      --  This node type has no derivation.

      type Excludes_Null is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Whether the containing cast expression will raise on null cast result
      --  or not.
      --
      --  Derived nodes: :ada:ref:`Excludes_Null_Absent`,
      --  :ada:ref:`Excludes_Null_Present`

      type Excludes_Null_Absent is new Excludes_Null with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Excludes_Null_Present is new Excludes_Null with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Node_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Root node class for node patterns
      --
      --  Derived nodes: :ada:ref:`Extended_Node_Pattern`,
      --  :ada:ref:`Type_Pattern`

      type Extended_Node_Pattern is new Node_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Node pattern of the form:
      --
      --  ``KindName(field: Pattern, prop(): Pattern, any selector: Pattern)``
      --
      --  For instance:
      --
      --  .. code::
      --
      --     ObjectDecl(any children: AspectAssoc)
      --
      --  This node type has no derivation.

      type Field_Decl is new Component_Decl with private
         with First_Controlling_Parameter
      ;
      --  Field declaration.
      --
      --  This node type has no derivation.

      type Filtered_Pattern is new Base_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern with a filtering predicate, of the form: ``<pattern> when
      --  <predicate>``
      --
      --  For instance:
      --
      --  .. code::
      --
      --     o@ObjectDecl when o.children.length == 3
      --
      --  This node type has no derivation.

      type Full_Decl is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Container for an lkt declaration. Contains the decl node plus the
      --  documentation and annotations.
      --
      --  This node type has no derivation.

      type Fun_Decl is new User_Val_Decl with private
         with First_Controlling_Parameter
      ;
      --  Function declaration.
      --
      --  This node type has no derivation.

      type Fun_Param_Decl is new Component_Decl with private
         with First_Controlling_Parameter
      ;
      --  Function parameter declaration.
      --
      --  This node type has no derivation.

      type Fun_Param_Decl_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Fun_Param_Decl_List_First,
                           Next        => Fun_Param_Decl_List_Next,
                           Has_Element => Fun_Param_Decl_List_Has_Element,
                           Element     => Fun_Param_Decl_List_Element)
      ;
      --  List of FunParamDecl.
      --
      --  This node type has no derivation.

      type Function_Type is new Type_Decl with private
         with First_Controlling_Parameter
      ;
      --  Function type.
      --
      --  This node type has no derivation.

      type Function_Type_Ref is new Type_Ref with private
         with First_Controlling_Parameter
      ;
      --  Reference to a function type.
      --
      --  This node type has no derivation.

      type Generic_Decl is new Decl with private
         with First_Controlling_Parameter
      ;
      --  Generic entity declaration.
      --
      --  This node type has no derivation.

      type Generic_Instantiation is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Generic instantiation.
      --
      --  This node type has no derivation.

      type Generic_Param_Decl_List is new Full_Decl_List with private
         with First_Controlling_Parameter
      ;
      --  Comma-separated list of generic parameter types.
      --
      --  This node type has no derivation.

      type Generic_Param_Type_Decl is new Type_Decl with private
         with First_Controlling_Parameter
      ;
      --  Declaration of a parameter type in a generic declaration.
      --
      --  This node type has no derivation.

      type Generic_Type_Ref is new Type_Ref with private
         with First_Controlling_Parameter
      ;
      --  Reference to a generic type.
      --
      --  This node type has no derivation.

      type Grammar_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Base class for expressions related to grammars.
      --
      --  Derived nodes: :ada:ref:`Grammar_Cut`, :ada:ref:`Grammar_Discard`,
      --  :ada:ref:`Grammar_Dont_Skip`, :ada:ref:`Grammar_List`,
      --  :ada:ref:`Grammar_Null`, :ada:ref:`Grammar_Opt_Error_Group`,
      --  :ada:ref:`Grammar_Opt_Error`, :ada:ref:`Grammar_Opt_Group`,
      --  :ada:ref:`Grammar_Opt`, :ada:ref:`Grammar_Or_Expr`,
      --  :ada:ref:`Grammar_Pick`, :ada:ref:`Grammar_Predicate`,
      --  :ada:ref:`Grammar_Rule_Ref`, :ada:ref:`Grammar_Skip`,
      --  :ada:ref:`Grammar_Stop_Cut`, :ada:ref:`Parse_Node_Expr`,
      --  :ada:ref:`Token_Lit`, :ada:ref:`Token_No_Case_Lit`,
      --  :ada:ref:`Token_Pattern_Concat`, :ada:ref:`Token_Pattern_Lit`,
      --  :ada:ref:`Token_Ref`

      type Grammar_Cut is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for a cut.
      --
      --  This node type has no derivation.

      type Grammar_Decl is new Decl with private
         with First_Controlling_Parameter
      ;
      --  Declaration of a language's grammar.
      --
      --  This node type has no derivation.

      type Grammar_Discard is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression to discard the match.
      --
      --  This node type has no derivation.

      type Grammar_Dont_Skip is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression (error recovery) to ensure that any nested skip
      --  parser calls won't skip certain parse results.
      --
      --  This node type has no derivation.

      type Grammar_Expr_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Grammar_Expr_List_First,
                           Next        => Grammar_Expr_List_Next,
                           Has_Element => Grammar_Expr_List_Has_Element,
                           Element     => Grammar_Expr_List_Element)
      ;
      --  List of GrammarExpr.
      --
      --  This node type has no derivation.

      type Grammar_Expr_List_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Grammar_Expr_List_List_First,
                           Next        => Grammar_Expr_List_List_Next,
                           Has_Element => Grammar_Expr_List_List_Has_Element,
                           Element     => Grammar_Expr_List_List_Element)
      ;
      --  List of ASTList[GrammarExpr].
      --
      --  This node type has no derivation.

      type Grammar_Pick is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression to pick the significant parse out of a list of
      --  parses (will automatically discard token results).
      --
      --  Derived nodes: :ada:ref:`Grammar_Implicit_Pick`

      type Grammar_Implicit_Pick is new Grammar_Pick with private
         with First_Controlling_Parameter
      ;
      --  Implicit pick operation.
      --
      --  This node type has no derivation.

      type Grammar_List is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression to parse lists of results. Results can be
      --  separated by a separator. List can be empty ('*') or not ('+').
      --
      --  This node type has no derivation.

      type Grammar_List_Sep is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Specification for the separator of a list parser.
      --
      --  This node type has no derivation.

      type Grammar_Null is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression to parse a null node.
      --
      --  This node type has no derivation.

      type Grammar_Opt is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for an optional parsing result.
      --
      --  This node type has no derivation.

      type Grammar_Opt_Error is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for an optional parsing result. Missing result
      --  creates an error, but parsing continues.
      --
      --  This node type has no derivation.

      type Grammar_Opt_Error_Group is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for a group of optional parsing results. Failure
      --  to parse an optional result creates an error, but parsing continues.
      --
      --  This node type has no derivation.

      type Grammar_Opt_Group is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for a group of optional parsing results.
      --
      --  This node type has no derivation.

      type Grammar_Or_Expr is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar ``Or`` expression (disjunctive choice between several grammar
      --  options).
      --
      --  This node type has no derivation.

      type Grammar_Predicate is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for a predicate: Only parse something if the
      --  predicate (that is a reference to a node property) returns True.
      --
      --  This node type has no derivation.

      type Grammar_Rule_Decl is new Base_Grammar_Rule_Decl with private
         with First_Controlling_Parameter
      ;
      --  Declaration of a grammar rule inside of a grammar.
      --
      --  This node type has no derivation.

      type Grammar_Rule_Ref is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for a reference to another grammar rule.
      --
      --  This node type has no derivation.

      type Grammar_Skip is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression (error recovery) to skip a parsing result.
      --
      --  This node type has no derivation.

      type Grammar_Stop_Cut is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for a StopCut.
      --
      --  This node type has no derivation.

      type If_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  If expression.
      --
      --  This node type has no derivation.

      type Import is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Statement to import another source file.
      --
      --  This node type has no derivation.

      type Import_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Import_List_First,
                           Next        => Import_List_Next,
                           Has_Element => Import_List_Has_Element,
                           Element     => Import_List_Element)
      ;
      --  List of Import.
      --
      --  This node type has no derivation.

      type Integer_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern to match on integers.
      --
      --  This node type has no derivation.

      type Isa is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Isa expression.
      --
      --  This node type has no derivation.

      type Keep_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Keep expression.
      --
      --  This node type has no derivation.

      type Lambda_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Lambda expression.
      --
      --  This node type has no derivation.

      type Lambda_Param_Decl is new Component_Decl with private
         with First_Controlling_Parameter
      ;
      --  Function parameter declaration.
      --
      --  This node type has no derivation.

      type Lambda_Param_Decl_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Lambda_Param_Decl_List_First,
                           Next        => Lambda_Param_Decl_List_Next,
                           Has_Element => Lambda_Param_Decl_List_Has_Element,
                           Element     => Lambda_Param_Decl_List_Element)
      ;
      --  List of LambdaParamDecl.
      --
      --  This node type has no derivation.

      type Langkit_Root is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  For the moment, root node of a lkt compilation unit.
      --
      --  This node type has no derivation.

      type Lexer_Case_Rule is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Lexer construct to introduce a conditional lexing action.
      --
      --  This node type has no derivation.

      type Lexer_Case_Rule_Cond_Alt is new Base_Lexer_Case_Rule_Alt with private
         with First_Controlling_Parameter
      ;
      --  Alternative of a case rule which sends the token only if the kind of
      --  the previous token is among a given set.
      --
      --  This node type has no derivation.

      type Lexer_Case_Rule_Default_Alt is new Base_Lexer_Case_Rule_Alt with private
         with First_Controlling_Parameter
      ;
      --  Default alternative of a case rule which sends the token if all the
      --  previous alternatives failed.
      --
      --  This node type has no derivation.

      type Lexer_Case_Rule_Send is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Lexer construction used by case alternatives to represent the token
      --  to send if that alternative was chosen.
      --
      --  This node type has no derivation.

      type Lexer_Decl is new Decl with private
         with First_Controlling_Parameter
      ;
      --  Declaration of a language's lexer.
      --
      --  This node type has no derivation.

      type Lexer_Family_Decl is new Decl with private
         with First_Controlling_Parameter
      ;
      --  Declaration of a token family.
      --
      --  This node type has no derivation.

      type List_Kind is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Kind for list parser expressions.
      --
      --  Derived nodes: :ada:ref:`List_Kind_One`, :ada:ref:`List_Kind_Zero`

      type List_Kind_One is new List_Kind with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type List_Kind_Zero is new List_Kind with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type List_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern to match on lists.
      --
      --  This node type has no derivation.

      type Logic_Assign is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Class for "assign to logic var" equations.
      --
      --  This node type has no derivation.

      type Logic_Call_Expr is new Base_Call_Expr with private
         with First_Controlling_Parameter
      ;
      --  Base class for logic call expresions, of the form:
      --
      --  .. code::
      --
      --     name%(args)
      --
      --  Derived nodes: :ada:ref:`Logic_Predicate`,
      --  :ada:ref:`Logic_Propagate_Call`

      type Logic_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Class for logic expressions (any ``basic_expr`` starting with %).
      --
      --  This node type has no derivation.

      type Logic_Predicate is new Logic_Call_Expr with private
         with First_Controlling_Parameter
      ;
      --  Class for "predicate" equations.
      --
      --  This node type has no derivation.

      type Logic_Propagate is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Class for "propagate" equations.
      --
      --  This node type has no derivation.

      type Logic_Propagate_Call is new Logic_Call_Expr with private
         with First_Controlling_Parameter
      ;
      --  Class for the call inside "propagate" equations.
      --
      --  This node type has no derivation.

      type Logic_Unify is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Class for "unify" equations.
      --
      --  This node type has no derivation.

      type Match_Branch is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Branch inside a match expression.
      --
      --  This node type has no derivation.

      type Match_Branch_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Match_Branch_List_First,
                           Next        => Match_Branch_List_Next,
                           Has_Element => Match_Branch_List_Has_Element,
                           Element     => Match_Branch_List_Element)
      ;
      --  List of MatchBranch.
      --
      --  This node type has no derivation.

      type Match_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Binary operator expression.
      --
      --  This node type has no derivation.

      type Match_Val_Decl is new Explicitly_Typed_Decl with private
         with First_Controlling_Parameter
      ;
      --  Value declaration in a match branch.
      --
      --  This node type has no derivation.

      type Module_Ref_Id is new Id with private
         with First_Controlling_Parameter
      ;
      --  Id referencing a langkit module.
      --
      --  This node type has no derivation.

      type Node_Decl is new Base_Val_Decl with private
         with First_Controlling_Parameter
      ;
      --  Synthetic declaration for the implicit "node" variable available in
      --  properties.
      --
      --  This node type has no derivation.

      type Node_Pattern_Detail is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Base class for parts of an extended node pattern.
      --
      --  Derived nodes: :ada:ref:`Node_Pattern_Field`,
      --  :ada:ref:`Node_Pattern_Property`, :ada:ref:`Node_Pattern_Selector`

      type Node_Pattern_Detail_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Node_Pattern_Detail_List_First,
                           Next        => Node_Pattern_Detail_List_Next,
                           Has_Element => Node_Pattern_Detail_List_Has_Element,
                           Element     => Node_Pattern_Detail_List_Element)
      ;
      --  List of NodePatternDetail.
      --
      --  This node type has no derivation.

      type Node_Pattern_Field is new Node_Pattern_Detail with private
         with First_Controlling_Parameter
      ;
      --  Access to a field in a node pattern.
      --
      --  This node type has no derivation.

      type Node_Pattern_Property is new Node_Pattern_Detail with private
         with First_Controlling_Parameter
      ;
      --  Access to a property in a node pattern.
      --
      --  This node type has no derivation.

      type Node_Pattern_Selector is new Node_Pattern_Detail with private
         with First_Controlling_Parameter
      ;
      --  Use of a selector in a node pattern
      --
      --  This node type has no derivation.

      type Not_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Boolean negation expression.
      --
      --  This node type has no derivation.

      type Not_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern that matches if its inner pattern doesn't match.
      --
      --  For instance:
      --
      --  .. code::
      --
      --     let non_objects = select not ObjectDecl
      --
      --  This node type has no derivation.

      type Null_Cond_Qualifier is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Whether the "?" operation qualifier (to denote the null-conditional
      --  behavior) is present.
      --
      --  Derived nodes: :ada:ref:`Null_Cond_Qualifier_Absent`,
      --  :ada:ref:`Null_Cond_Qualifier_Present`

      type Null_Cond_Qualifier_Absent is new Null_Cond_Qualifier with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Null_Cond_Qualifier_Present is new Null_Cond_Qualifier with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Null_Lit is new Lit with private
         with First_Controlling_Parameter
      ;
      --  Null literal expression.
      --
      --  This node type has no derivation.

      type Null_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Null pattern. Will only match the null node.
      --
      --  This node type has no derivation.

      type Num_Lit is new Lit with private
         with First_Controlling_Parameter
      ;
      --  Number literal expression.
      --
      --  This node type has no derivation.

      type Op is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Operator in a binary operator expression.
      --
      --  Derived nodes: :ada:ref:`Op_Amp`, :ada:ref:`Op_And`,
      --  :ada:ref:`Op_Div`, :ada:ref:`Op_Eq`, :ada:ref:`Op_Gt`,
      --  :ada:ref:`Op_Gte`, :ada:ref:`Op_Logic_And`, :ada:ref:`Op_Logic_Or`,
      --  :ada:ref:`Op_Lt`, :ada:ref:`Op_Lte`, :ada:ref:`Op_Minus`,
      --  :ada:ref:`Op_Mult`, :ada:ref:`Op_Ne`, :ada:ref:`Op_Or_Int`,
      --  :ada:ref:`Op_Or`, :ada:ref:`Op_Plus`

      type Op_Amp is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_And is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Div is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Eq is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Gt is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Gte is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Logic_And is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Logic_Or is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Lt is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Lte is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Minus is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Mult is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Ne is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Or is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Or_Int is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Op_Plus is new Op with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Or_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern that matches if any of its subpatterns matches.
      --
      --  For instance:
      --
      --  .. code::
      --
      --     let value_decls = select ObjectDecl or ParamSpec
      --
      --  This node type has no derivation.

      type Paren_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Parenthesized expression.
      --
      --  This node type has no derivation.

      type Paren_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  A parenthesized pattern.
      --
      --  This node type has no derivation.

      type Parse_Node_Expr is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Expression for the parsing of a Node.
      --
      --  This node type has no derivation.

      type Single_Line_String_Lit is new String_Lit with private
         with First_Controlling_Parameter
      ;
      --  Single line string literal expression.
      --
      --  Note that in order to reduce the size of the node type hierarchy, we
      --  define only one node (StringLit) for all our string literals (only
      --  regular strings and pattern string literals at the moment). This will
      --  also make it easy to add new string prefixes in the future.
      --
      --  Derived nodes: :ada:ref:`Pattern_Single_Line_String_Lit`

      type Pattern_Single_Line_String_Lit is new Single_Line_String_Lit with private
         with First_Controlling_Parameter
      ;
      --  Pattern single line string literal expression.
      --
      --  This node type has no derivation.

      type Raise_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Raise expression.
      --
      --  This node type has no derivation.

      type Ref_Id is new Id with private
         with First_Controlling_Parameter
      ;
      --  Reference identifier.
      --
      --  This node type has no derivation.

      type Ref_Id_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Ref_Id_List_First,
                           Next        => Ref_Id_List_Next,
                           Has_Element => Ref_Id_List_Has_Element,
                           Element     => Ref_Id_List_Element)
      ;
      --  List of RefId.
      --
      --  This node type has no derivation.

      type Regex_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern that considers the value as text and matches it against the
      --  given regular expression.
      --
      --  This node type has no derivation.

      type Selector_Call is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Root node for selector patterns
      --
      --  This node type has no derivation.

      type Self_Decl is new Base_Val_Decl with private
         with First_Controlling_Parameter
      ;
      --  Synthetic declaration for the implicit "self" variable available in
      --  properties.
      --
      --  This node type has no derivation.

      type Simple_Type_Ref is new Type_Ref with private
         with First_Controlling_Parameter
      ;
      --  Simple reference to a type.
      --
      --  This node type has no derivation.

      type Splat_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern to match any remaining number of elements in a list pattern.
      --
      --  This node type has no derivation.

      type Struct_Decl is new Named_Type_Decl with private
         with First_Controlling_Parameter
      ;
      --  Declaration for a LK struct.
      --
      --  This node type has no derivation.

      type Subscript_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Array subscript expression.
      --
      --  This node type has no derivation.

      type Synth_Fun_Decl is new Decl with private
         with First_Controlling_Parameter
      ;
      --  Logic function declaration.
      --
      --  This node type has no derivation.

      type Synth_Param_Decl is new Decl with private
         with First_Controlling_Parameter
      ;
      --  Logic function parameter declaration.
      --
      --  This node type has no derivation.

      type Synthetic_Lexer_Decl is new Base_Grammar_Rule_Decl with private
         with First_Controlling_Parameter
      ;
      --  This node type has no derivation.

      type Type_Ref_List is new Lkt_Node_Base_List with private
         with First_Controlling_Parameter
            , Iterable => (First       => Type_Ref_List_First,
                           Next        => Type_Ref_List_Next,
                           Has_Element => Type_Ref_List_Has_Element,
                           Element     => Type_Ref_List_Element)
      ;
      --  List of TypeRef.
      --
      --  This list node can contain one of the following nodes:
      --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
      --  :ada:ref:`Simple_Type_Ref`
      --
      --  Derived nodes: :ada:ref:`Synthetic_Type_Ref_List`

      type Synthetic_Type_Ref_List is new Type_Ref_List with private
         with First_Controlling_Parameter
      ;
      --  Synthetic list of type references, used to create synthetic type
      --  declarations.
      --
      --  This node type has no derivation.

      type Token_Lit is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for a token literal.
      --
      --  This node type has no derivation.

      type Token_No_Case_Lit is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for a case insensitive token literal.
      --
      --  This node type has no derivation.

      type Token_Pattern_Concat is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for the concatenation of two patterns.
      --
      --  This node type has no derivation.

      type Token_Pattern_Lit is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for a pattern literal.
      --
      --  This node type has no derivation.

      type Token_Ref is new Grammar_Expr with private
         with First_Controlling_Parameter
      ;
      --  Grammar expression for a token reference.
      --
      --  This node type has no derivation.

      type Trait_Decl is new Named_Type_Decl with private
         with First_Controlling_Parameter
      ;
      --  Trait declaration. For the moment, a Trait can just be used to group
      --  behavior for built-in types. It's not usable as a type-bound since we
      --  don't have generics, and you cannot implement one either.
      --
      --  The reason they're added is to lay down the basics of what we want
      --  the Lkt type system to be.
      --
      --  TODO: Traits are *not* types. They're treated as such in the grammar
      --  for convenience for now, but it's probably not a good idea. Migrate
      --  away from this.
      --
      --  This node type has no derivation.

      type Try_Expr is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Try expression.
      --
      --  This node type has no derivation.

      type Tuple_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern to match on tuples.
      --
      --  This node type has no derivation.

      type Type_Pattern is new Node_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Pattern of the form: KindName
      --
      --  This node type has no derivation.

      type Un_Op is new Expr with private
         with First_Controlling_Parameter
      ;
      --  Unary operator expression.
      --
      --  This node type has no derivation.

      type Universal_Pattern is new Value_Pattern with private
         with First_Controlling_Parameter
      ;
      --  Universal pattern that matches any value.
      --
      --  For instance:
      --
      --  .. code::
      --
      --     let declParent = query * [children(depth==1)] BasicDecl
      --
      --  This node type has no derivation.

      type Val_Decl is new Explicitly_Typed_Decl with private
         with First_Controlling_Parameter
      ;
      --  Value declaration.
      --
      --  This node type has no derivation.

      type Var_Bind is new Lkt_Node with private
         with First_Controlling_Parameter
      ;
      --  Dynamic var bind expression.
      --
      --  This node type has no derivation.


      No_Lkt_Node : constant Lkt_Node;
      --  Special value to represent the absence of a node. Note that every
      --  node type derived from the root type has a similar ``No_Node``
      --  constant.
      No_Expr : constant Expr;
      --% no-document: True
      No_Any_Of : constant Any_Of;
      --% no-document: True
      No_Lkt_Node_Base_List : constant Lkt_Node_Base_List;
      --% no-document: True
      No_Expr_List : constant Expr_List;
      --% no-document: True
      No_Any_Of_List : constant Any_Of_List;
      --% no-document: True
      No_Decl : constant Decl;
      --% no-document: True
      No_Type_Decl : constant Type_Decl;
      --% no-document: True
      No_Any_Type_Decl : constant Any_Type_Decl;
      --% no-document: True
      No_Argument : constant Argument;
      --% no-document: True
      No_Argument_List : constant Argument_List;
      --% no-document: True
      No_Array_Literal : constant Array_Literal;
      --% no-document: True
      No_Base_Call_Expr : constant Base_Call_Expr;
      --% no-document: True
      No_Base_Grammar_Rule_Decl : constant Base_Grammar_Rule_Decl;
      --% no-document: True
      No_Base_Lexer_Case_Rule_Alt : constant Base_Lexer_Case_Rule_Alt;
      --% no-document: True
      No_Base_Lexer_Case_Rule_Alt_List : constant Base_Lexer_Case_Rule_Alt_List;
      --% no-document: True
      No_Base_Pattern : constant Base_Pattern;
      --% no-document: True
      No_Base_Pattern_List : constant Base_Pattern_List;
      --% no-document: True
      No_Base_Val_Decl : constant Base_Val_Decl;
      --% no-document: True
      No_Named_Type_Decl : constant Named_Type_Decl;
      --% no-document: True
      No_Basic_Class_Decl : constant Basic_Class_Decl;
      --% no-document: True
      No_Lit : constant Lit;
      --% no-document: True
      No_Big_Num_Lit : constant Big_Num_Lit;
      --% no-document: True
      No_Bin_Op : constant Bin_Op;
      --% no-document: True
      No_Binding_Pattern : constant Binding_Pattern;
      --% no-document: True
      No_Lkt_Node_List : constant Lkt_Node_List;
      --% no-document: True
      No_Block_Decl_List : constant Block_Decl_List;
      --% no-document: True
      No_Block_Expr : constant Block_Expr;
      --% no-document: True
      No_Block_String_Line : constant Block_String_Line;
      --% no-document: True
      No_Block_String_Line_List : constant Block_String_Line_List;
      --% no-document: True
      No_String_Lit : constant String_Lit;
      --% no-document: True
      No_Block_String_Lit : constant Block_String_Lit;
      --% no-document: True
      No_Value_Pattern : constant Value_Pattern;
      --% no-document: True
      No_Bool_Pattern : constant Bool_Pattern;
      --% no-document: True
      No_Bool_Pattern_False : constant Bool_Pattern_False;
      --% no-document: True
      No_Bool_Pattern_True : constant Bool_Pattern_True;
      --% no-document: True
      No_Call_Expr : constant Call_Expr;
      --% no-document: True
      No_Call_Expr_List : constant Call_Expr_List;
      --% no-document: True
      No_Cast_Expr : constant Cast_Expr;
      --% no-document: True
      No_Char_Lit : constant Char_Lit;
      --% no-document: True
      No_Class_Decl : constant Class_Decl;
      --% no-document: True
      No_Class_Qualifier : constant Class_Qualifier;
      --% no-document: True
      No_Class_Qualifier_Absent : constant Class_Qualifier_Absent;
      --% no-document: True
      No_Class_Qualifier_Present : constant Class_Qualifier_Present;
      --% no-document: True
      No_User_Val_Decl : constant User_Val_Decl;
      --% no-document: True
      No_Explicitly_Typed_Decl : constant Explicitly_Typed_Decl;
      --% no-document: True
      No_Component_Decl : constant Component_Decl;
      --% no-document: True
      No_Decl_Annotation : constant Decl_Annotation;
      --% no-document: True
      No_Decl_Annotation_Args : constant Decl_Annotation_Args;
      --% no-document: True
      No_Decl_Annotation_List : constant Decl_Annotation_List;
      --% no-document: True
      No_Full_Decl_List : constant Full_Decl_List;
      --% no-document: True
      No_Decl_Block : constant Decl_Block;
      --% no-document: True
      No_Id : constant Id;
      --% no-document: True
      No_Def_Id : constant Def_Id;
      --% no-document: True
      No_Type_Ref : constant Type_Ref;
      --% no-document: True
      No_Default_List_Type_Ref : constant Default_List_Type_Ref;
      --% no-document: True
      No_Dot_Expr : constant Dot_Expr;
      --% no-document: True
      No_Dyn_Env_Wrapper : constant Dyn_Env_Wrapper;
      --% no-document: True
      No_Dyn_Var_Decl : constant Dyn_Var_Decl;
      --% no-document: True
      No_Elsif_Branch : constant Elsif_Branch;
      --% no-document: True
      No_Elsif_Branch_List : constant Elsif_Branch_List;
      --% no-document: True
      No_Enum_Class_Alt_Decl : constant Enum_Class_Alt_Decl;
      --% no-document: True
      No_Enum_Class_Alt_Decl_List : constant Enum_Class_Alt_Decl_List;
      --% no-document: True
      No_Enum_Class_Case : constant Enum_Class_Case;
      --% no-document: True
      No_Enum_Class_Case_List : constant Enum_Class_Case_List;
      --% no-document: True
      No_Enum_Class_Decl : constant Enum_Class_Decl;
      --% no-document: True
      No_Enum_Lit_Decl : constant Enum_Lit_Decl;
      --% no-document: True
      No_Enum_Lit_Decl_List : constant Enum_Lit_Decl_List;
      --% no-document: True
      No_Enum_Type_Decl : constant Enum_Type_Decl;
      --% no-document: True
      No_Env_Spec_Decl : constant Env_Spec_Decl;
      --% no-document: True
      No_Error_On_Null : constant Error_On_Null;
      --% no-document: True
      No_Excludes_Null : constant Excludes_Null;
      --% no-document: True
      No_Excludes_Null_Absent : constant Excludes_Null_Absent;
      --% no-document: True
      No_Excludes_Null_Present : constant Excludes_Null_Present;
      --% no-document: True
      No_Node_Pattern : constant Node_Pattern;
      --% no-document: True
      No_Extended_Node_Pattern : constant Extended_Node_Pattern;
      --% no-document: True
      No_Field_Decl : constant Field_Decl;
      --% no-document: True
      No_Filtered_Pattern : constant Filtered_Pattern;
      --% no-document: True
      No_Full_Decl : constant Full_Decl;
      --% no-document: True
      No_Fun_Decl : constant Fun_Decl;
      --% no-document: True
      No_Fun_Param_Decl : constant Fun_Param_Decl;
      --% no-document: True
      No_Fun_Param_Decl_List : constant Fun_Param_Decl_List;
      --% no-document: True
      No_Function_Type : constant Function_Type;
      --% no-document: True
      No_Function_Type_Ref : constant Function_Type_Ref;
      --% no-document: True
      No_Generic_Decl : constant Generic_Decl;
      --% no-document: True
      No_Generic_Instantiation : constant Generic_Instantiation;
      --% no-document: True
      No_Generic_Param_Decl_List : constant Generic_Param_Decl_List;
      --% no-document: True
      No_Generic_Param_Type_Decl : constant Generic_Param_Type_Decl;
      --% no-document: True
      No_Generic_Type_Ref : constant Generic_Type_Ref;
      --% no-document: True
      No_Grammar_Expr : constant Grammar_Expr;
      --% no-document: True
      No_Grammar_Cut : constant Grammar_Cut;
      --% no-document: True
      No_Grammar_Decl : constant Grammar_Decl;
      --% no-document: True
      No_Grammar_Discard : constant Grammar_Discard;
      --% no-document: True
      No_Grammar_Dont_Skip : constant Grammar_Dont_Skip;
      --% no-document: True
      No_Grammar_Expr_List : constant Grammar_Expr_List;
      --% no-document: True
      No_Grammar_Expr_List_List : constant Grammar_Expr_List_List;
      --% no-document: True
      No_Grammar_Pick : constant Grammar_Pick;
      --% no-document: True
      No_Grammar_Implicit_Pick : constant Grammar_Implicit_Pick;
      --% no-document: True
      No_Grammar_List : constant Grammar_List;
      --% no-document: True
      No_Grammar_List_Sep : constant Grammar_List_Sep;
      --% no-document: True
      No_Grammar_Null : constant Grammar_Null;
      --% no-document: True
      No_Grammar_Opt : constant Grammar_Opt;
      --% no-document: True
      No_Grammar_Opt_Error : constant Grammar_Opt_Error;
      --% no-document: True
      No_Grammar_Opt_Error_Group : constant Grammar_Opt_Error_Group;
      --% no-document: True
      No_Grammar_Opt_Group : constant Grammar_Opt_Group;
      --% no-document: True
      No_Grammar_Or_Expr : constant Grammar_Or_Expr;
      --% no-document: True
      No_Grammar_Predicate : constant Grammar_Predicate;
      --% no-document: True
      No_Grammar_Rule_Decl : constant Grammar_Rule_Decl;
      --% no-document: True
      No_Grammar_Rule_Ref : constant Grammar_Rule_Ref;
      --% no-document: True
      No_Grammar_Skip : constant Grammar_Skip;
      --% no-document: True
      No_Grammar_Stop_Cut : constant Grammar_Stop_Cut;
      --% no-document: True
      No_If_Expr : constant If_Expr;
      --% no-document: True
      No_Import : constant Import;
      --% no-document: True
      No_Import_List : constant Import_List;
      --% no-document: True
      No_Integer_Pattern : constant Integer_Pattern;
      --% no-document: True
      No_Isa : constant Isa;
      --% no-document: True
      No_Keep_Expr : constant Keep_Expr;
      --% no-document: True
      No_Lambda_Expr : constant Lambda_Expr;
      --% no-document: True
      No_Lambda_Param_Decl : constant Lambda_Param_Decl;
      --% no-document: True
      No_Lambda_Param_Decl_List : constant Lambda_Param_Decl_List;
      --% no-document: True
      No_Langkit_Root : constant Langkit_Root;
      --% no-document: True
      No_Lexer_Case_Rule : constant Lexer_Case_Rule;
      --% no-document: True
      No_Lexer_Case_Rule_Cond_Alt : constant Lexer_Case_Rule_Cond_Alt;
      --% no-document: True
      No_Lexer_Case_Rule_Default_Alt : constant Lexer_Case_Rule_Default_Alt;
      --% no-document: True
      No_Lexer_Case_Rule_Send : constant Lexer_Case_Rule_Send;
      --% no-document: True
      No_Lexer_Decl : constant Lexer_Decl;
      --% no-document: True
      No_Lexer_Family_Decl : constant Lexer_Family_Decl;
      --% no-document: True
      No_List_Kind : constant List_Kind;
      --% no-document: True
      No_List_Kind_One : constant List_Kind_One;
      --% no-document: True
      No_List_Kind_Zero : constant List_Kind_Zero;
      --% no-document: True
      No_List_Pattern : constant List_Pattern;
      --% no-document: True
      No_Logic_Assign : constant Logic_Assign;
      --% no-document: True
      No_Logic_Call_Expr : constant Logic_Call_Expr;
      --% no-document: True
      No_Logic_Expr : constant Logic_Expr;
      --% no-document: True
      No_Logic_Predicate : constant Logic_Predicate;
      --% no-document: True
      No_Logic_Propagate : constant Logic_Propagate;
      --% no-document: True
      No_Logic_Propagate_Call : constant Logic_Propagate_Call;
      --% no-document: True
      No_Logic_Unify : constant Logic_Unify;
      --% no-document: True
      No_Match_Branch : constant Match_Branch;
      --% no-document: True
      No_Match_Branch_List : constant Match_Branch_List;
      --% no-document: True
      No_Match_Expr : constant Match_Expr;
      --% no-document: True
      No_Match_Val_Decl : constant Match_Val_Decl;
      --% no-document: True
      No_Module_Ref_Id : constant Module_Ref_Id;
      --% no-document: True
      No_Node_Decl : constant Node_Decl;
      --% no-document: True
      No_Node_Pattern_Detail : constant Node_Pattern_Detail;
      --% no-document: True
      No_Node_Pattern_Detail_List : constant Node_Pattern_Detail_List;
      --% no-document: True
      No_Node_Pattern_Field : constant Node_Pattern_Field;
      --% no-document: True
      No_Node_Pattern_Property : constant Node_Pattern_Property;
      --% no-document: True
      No_Node_Pattern_Selector : constant Node_Pattern_Selector;
      --% no-document: True
      No_Not_Expr : constant Not_Expr;
      --% no-document: True
      No_Not_Pattern : constant Not_Pattern;
      --% no-document: True
      No_Null_Cond_Qualifier : constant Null_Cond_Qualifier;
      --% no-document: True
      No_Null_Cond_Qualifier_Absent : constant Null_Cond_Qualifier_Absent;
      --% no-document: True
      No_Null_Cond_Qualifier_Present : constant Null_Cond_Qualifier_Present;
      --% no-document: True
      No_Null_Lit : constant Null_Lit;
      --% no-document: True
      No_Null_Pattern : constant Null_Pattern;
      --% no-document: True
      No_Num_Lit : constant Num_Lit;
      --% no-document: True
      No_Op : constant Op;
      --% no-document: True
      No_Op_Amp : constant Op_Amp;
      --% no-document: True
      No_Op_And : constant Op_And;
      --% no-document: True
      No_Op_Div : constant Op_Div;
      --% no-document: True
      No_Op_Eq : constant Op_Eq;
      --% no-document: True
      No_Op_Gt : constant Op_Gt;
      --% no-document: True
      No_Op_Gte : constant Op_Gte;
      --% no-document: True
      No_Op_Logic_And : constant Op_Logic_And;
      --% no-document: True
      No_Op_Logic_Or : constant Op_Logic_Or;
      --% no-document: True
      No_Op_Lt : constant Op_Lt;
      --% no-document: True
      No_Op_Lte : constant Op_Lte;
      --% no-document: True
      No_Op_Minus : constant Op_Minus;
      --% no-document: True
      No_Op_Mult : constant Op_Mult;
      --% no-document: True
      No_Op_Ne : constant Op_Ne;
      --% no-document: True
      No_Op_Or : constant Op_Or;
      --% no-document: True
      No_Op_Or_Int : constant Op_Or_Int;
      --% no-document: True
      No_Op_Plus : constant Op_Plus;
      --% no-document: True
      No_Or_Pattern : constant Or_Pattern;
      --% no-document: True
      No_Paren_Expr : constant Paren_Expr;
      --% no-document: True
      No_Paren_Pattern : constant Paren_Pattern;
      --% no-document: True
      No_Parse_Node_Expr : constant Parse_Node_Expr;
      --% no-document: True
      No_Single_Line_String_Lit : constant Single_Line_String_Lit;
      --% no-document: True
      No_Pattern_Single_Line_String_Lit : constant Pattern_Single_Line_String_Lit;
      --% no-document: True
      No_Raise_Expr : constant Raise_Expr;
      --% no-document: True
      No_Ref_Id : constant Ref_Id;
      --% no-document: True
      No_Ref_Id_List : constant Ref_Id_List;
      --% no-document: True
      No_Regex_Pattern : constant Regex_Pattern;
      --% no-document: True
      No_Selector_Call : constant Selector_Call;
      --% no-document: True
      No_Self_Decl : constant Self_Decl;
      --% no-document: True
      No_Simple_Type_Ref : constant Simple_Type_Ref;
      --% no-document: True
      No_Splat_Pattern : constant Splat_Pattern;
      --% no-document: True
      No_Struct_Decl : constant Struct_Decl;
      --% no-document: True
      No_Subscript_Expr : constant Subscript_Expr;
      --% no-document: True
      No_Synth_Fun_Decl : constant Synth_Fun_Decl;
      --% no-document: True
      No_Synth_Param_Decl : constant Synth_Param_Decl;
      --% no-document: True
      No_Synthetic_Lexer_Decl : constant Synthetic_Lexer_Decl;
      --% no-document: True
      No_Type_Ref_List : constant Type_Ref_List;
      --% no-document: True
      No_Synthetic_Type_Ref_List : constant Synthetic_Type_Ref_List;
      --% no-document: True
      No_Token_Lit : constant Token_Lit;
      --% no-document: True
      No_Token_No_Case_Lit : constant Token_No_Case_Lit;
      --% no-document: True
      No_Token_Pattern_Concat : constant Token_Pattern_Concat;
      --% no-document: True
      No_Token_Pattern_Lit : constant Token_Pattern_Lit;
      --% no-document: True
      No_Token_Ref : constant Token_Ref;
      --% no-document: True
      No_Trait_Decl : constant Trait_Decl;
      --% no-document: True
      No_Try_Expr : constant Try_Expr;
      --% no-document: True
      No_Tuple_Pattern : constant Tuple_Pattern;
      --% no-document: True
      No_Type_Pattern : constant Type_Pattern;
      --% no-document: True
      No_Un_Op : constant Un_Op;
      --% no-document: True
      No_Universal_Pattern : constant Universal_Pattern;
      --% no-document: True
      No_Val_Decl : constant Val_Decl;
      --% no-document: True
      No_Var_Bind : constant Var_Bind;
      --% no-document: True

   function Is_Null (Node : Lkt_Node'Class) return Boolean;
   --  Return whether this node is a null node reference.

   function Is_Token_Node
     (Node : Lkt_Node'Class) return Boolean;
   --  Return whether this node is a node that contains only a single token.

   function Is_Synthetic
     (Node : Lkt_Node'Class) return Boolean;
   --  Return whether this node is synthetic.

   function "=" (L, R : Lkt_Node'Class) return Boolean;
   --  Return whether ``L`` and ``R`` designate the same node

   function Image (Node : Lkt_Node'Class) return String;
   --  Return a short string describing ``Node``, or None" if ``Node.Is_Null``
   --  is true.

   -------------------
   -- Event handler --
   -------------------

   type Event_Handler_Interface is interface;
   --  Interface to handle events sent by the analysis context.

   procedure Unit_Requested_Callback
     (Self               : in out Event_Handler_Interface;
      Context            : Analysis_Context'Class;
      Name               : Text_Type;
      From               : Analysis_Unit'Class;
      Found              : Boolean;
      Is_Not_Found_Error : Boolean) is null;
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

   procedure Unit_Parsed_Callback
     (Self     : in out Event_Handler_Interface;
      Context  : Analysis_Context'Class;
      Unit     : Analysis_Unit'Class;
      Reparsed : Boolean) is null;
   --  Callback that will be called when any unit is parsed from the context
   --  ``Context``.
   --
   --  ``Unit`` is the resulting unit.
   --
   --  ``Reparsed`` indicates whether the unit was reparsed, or whether it was
   --  the first parse.

   procedure Release (Self : in out Event_Handler_Interface) is abstract;
   --  Actions to perform when releasing resources associated to Self

   procedure Do_Release (Self : in out Event_Handler_Interface'Class);
   --  Helper for the instantiation below

   package Event_Handler_References is new GNATCOLL.Refcount.Shared_Pointers
     (Event_Handler_Interface'Class, Do_Release);

   subtype Event_Handler_Reference is Event_Handler_References.Ref;
   No_Event_Handler_Ref : Event_Handler_Reference renames
      Event_Handler_References.Null_Ref;

   function Create_Event_Handler_Reference
     (Handler : Event_Handler_Interface'Class) return Event_Handler_Reference;
   --  Simple wrapper around the GNATCOLL.Refcount API to create event handler
   --  references.

   --------------------
   -- Unit providers --
   --------------------

   type Unit_Provider_Interface is interface;
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

   function Get_Unit_Filename
     (Provider : Unit_Provider_Interface;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is abstract;
   --  Return the filename corresponding to the given unit name/unit kind.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   procedure Get_Unit_Location
     (Provider       : Unit_Provider_Interface;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : in out Ada.Strings.Unbounded.Unbounded_String;
      PLE_Root_Index : in out Natural) is null;
   --  Like ``Get_Unit_Filename``, but return both the source file that
   --  ``Name``/``Kind`` designate (in ``Filename``) and the index of the PLE
   --  root inside that unit (in ``PLE_Root_Index``). If ``PLE_Root_Index`` is
   --  left to 0 upon return, discard the result and switch to the PLE root
   --  unaware ``Get_Unit_Filename`` function.

   function Get_Unit
     (Provider : Unit_Provider_Interface;
      Context  : Analysis_Context'Class;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Charset  : String := "";
      Reparse  : Boolean := False) return Analysis_Unit'Class is abstract;
   --  Fetch and return the analysis unit referenced by the given unit name.
   --  Raise a ``Property_Error`` if the given unit name is not valid.

   procedure Get_Unit_And_PLE_Root
     (Provider       : Unit_Provider_Interface;
      Context        : Analysis_Context'Class;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Charset        : String := "";
      Reparse        : Boolean := False;
      Unit           : in out Analysis_Unit'Class;
      PLE_Root_Index : in out Natural) is null;
   --  Like ``Get_Unit``, but return both the analysis unit that
   --  ``Name``/``Kind`` designate (in ``Unit``) and the index of the PLE root
   --  inside that unit (in ``PLE_Root_Index``). If ``PLE_Root_Index`` is left
   --  to 0 upon return, discard the result and switch to the PLE root unaware
   --  ``Get_Unit`` function.

   procedure Release (Provider : in out Unit_Provider_Interface) is abstract;
   --  Actions to perform when releasing resources associated to Provider

   procedure Do_Release (Provider : in out Unit_Provider_Interface'Class);
   --  Helper for the instantiation below

   package Unit_Provider_References is new GNATCOLL.Refcount.Shared_Pointers
     (Unit_Provider_Interface'Class, Do_Release);

   subtype Unit_Provider_Reference is Unit_Provider_References.Ref;
   No_Unit_Provider_Reference : Unit_Provider_Reference renames
      Unit_Provider_References.Null_Ref;

   function Create_Unit_Provider_Reference
     (Provider : Unit_Provider_Interface'Class) return Unit_Provider_Reference;
   --  Simple wrapper around the GNATCOLL.Refcount API to create unit provider
   --  references.

   ---------------------------------
   -- Analysis context primitives --
   ---------------------------------

   function Create_Context
     (Charset       : String := Default_Charset;
      File_Reader   : File_Reader_Reference := No_File_Reader_Reference;
      Unit_Provider : Unit_Provider_Reference := No_Unit_Provider_Reference;
      Event_Handler : Event_Handler_Reference := No_Event_Handler_Ref;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Positive := 8)
      return Analysis_Context;
   --  Create a new analysis context.
   --
   --  ``Charset`` will be used as a default charset to decode input sources in
   --  analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   --  charsets. Be careful: passing an unsupported charset is not guaranteed
   --  to raise an error here. If no charset is provided, ``"utf-8"`` is the
   --  default.
   --
   --  .. TODO: Passing an unsupported charset here is not guaranteed to raise
   --     an error right here, but this would be really helpful for users.
   --
   --  When ``With_Trivia`` is true, the parsed analysis units will contain
   --  trivias.
   --
   --  If provided, ``File_Reader`` will be used to fetch the contents of
   --  source files instead of the default, which is to just read it from the
   --  filesystem and decode it using the regular charset rules. Note that if
   --  provided, all parsing APIs that provide a buffer are forbidden, and any
   --  use of the rewriting API with the returned context is rejected.
   --
   --  If provided, ``Unit_Provider`` will be used to query the file name that
   --  corresponds to a unit reference during semantic analysis. If it is
   --  ``null``, the default one is used instead.
   --
   --  If provided, ``Event_Handler`` will be notified when various events
   --  happen.
   --
   --  ``Tab_Stop`` is a positive number to describe the effect of tabulation
   --  characters on the column number in source files.
   --% belongs-to: Analysis_Context

   function Has_Unit
     (Context       : Analysis_Context'Class;
      Unit_Filename : String) return Boolean;
   --  Return whether ``Context`` contains a unit correponding to
   --  ``Unit_Filename``.

   function Get_From_File
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
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
   --
   --  It is invalid to pass ``True`` to ``Reparse`` if a rewriting context is
   --  active.

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
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
   --
   --  Calling this is invalid if a rewriting context is active.

   function Get_From_Buffer
     (Context  : Analysis_Context'Class;
      Filename : String;
      Charset  : String := "";
      Buffer   : Ada.Strings.Unbounded.Unbounded_String;
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   --  Likewise, but working on an unbounded string

   function Get_With_Error
     (Context  : Analysis_Context'Class;
      Filename : String;
      Error    : Text_Type;
      Charset  : String := "";
      Rule     : Grammar_Rule := Default_Grammar_Rule) return Analysis_Unit;
   --  If a Unit for ``Filename`` already exists, return it unchanged.
   --  Otherwise, create an empty analysis unit for ``Filename`` with a
   --  diagnostic that contains the ``Error`` message.


   function Get_From_Provider
     (Context : Analysis_Context'Class;
      Name    : Text_Type;
      Kind    : Analysis_Unit_Kind;
      Charset : String := "";
      Reparse : Boolean := False) return Analysis_Unit;
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
   --  If the unit name cannot be tuned into a file name, raise an
   --  ``Invalid_Unit_Name_Error`` exception. If any other failure occurs, such
   --  as file opening, decoding, lexing or parsing failure, return an analysis
   --  unit anyway: errors are described as diagnostics of the returned
   --  analysis unit.
   --
   --  It is invalid to pass ``True`` to ``Reparse`` if a rewriting context is
   --  active.


   function Unit_Provider
     (Context : Analysis_Context'Class) return Unit_Provider_Reference;
   --  Return the unit provider for ``Context``
   --
   --% belongs-to: Analysis_Context

   function Hash (Context : Analysis_Context) return Ada.Containers.Hash_Type;
   --  Return a hash for this context, to be used in hash tables.

   function Has_With_Trivia (Context : Analysis_Context'Class) return Boolean;
   --  Return whether ``Context`` keeps trivia when parsing units

   procedure Discard_Errors_In_Populate_Lexical_Env
     (Context : Analysis_Context'Class; Discard : Boolean);
   --  Debug helper. Set whether ``Property_Error`` exceptions raised in
   --  ``Populate_Lexical_Env`` should be discarded. They are by default.

   procedure Set_Logic_Resolution_Timeout
     (Context : Analysis_Context'Class; Timeout : Natural);
   --  If ``Timeout`` is greater than zero, set a timeout for the resolution of
   --  logic equations. The unit is the number of steps in ANY/ALL relations.
   --  If ``Timeout`` is zero, disable the timeout. By default, the timeout is
   --  ``100 000`` steps.

   procedure Set_Lookup_Cache_Mode (Mode : Lookup_Cache_Kind);
   --  Set the lexical environments lookup cache mode according to ``Mode``.
   --  Note: Mainly meant for debugging the default mode.

   function Has_Rewriting_Handle
     (Context : Analysis_Context'Class) return Boolean;
   --  Return whether ``Context`` has a rewriting handler (see
   --  ``Liblktlang.Rewriting``), i.e. whether it is in the process of
   --  rewriting. If true, this means that the set of currently loaded analysis
   --  units is frozen until the rewriting process is done.

   ------------------------------
   -- Analysis unit primitives --
   ------------------------------

   function Context (Unit : Analysis_Unit'Class) return Analysis_Context;
   --  Return the context that owns this unit.

   function Hash (Unit : Analysis_Unit) return Ada.Containers.Hash_Type;
   --  Return a hash for this unit, to be used in hash tables.

   procedure Reparse (Unit : Analysis_Unit'Class; Charset : String := "");
   --  Reparse an analysis unit from the associated file.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure Reparse
     (Unit    : Analysis_Unit'Class;
      Charset : String := "";
      Buffer  : String);
   --  Reparse an analysis unit from a buffer.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as decoding, lexing or parsing failure,
   --  diagnostic are emitted to explain what happened.

   procedure Populate_Lexical_Env
     (Unit : Analysis_Unit'Class
     );
   --  Create lexical environments for this analysis unit, according to the
   --  specifications given in the language spec.
   --
   --  If not done before, it will be automatically called during semantic
   --  analysis. Calling it before enables one to control where the latency
   --  occurs.
   --
   --  Depending on whether errors are discarded (see
   --  ``Discard_Errors_In_Populate_Lexical_Env``), raise a ``Property_Error``
   --  on failure.

   function Get_Filename (Unit : Analysis_Unit'Class) return String;
   --  Return the filename this unit is associated to.

   function Get_Charset (Unit : Analysis_Unit'Class) return String;
   --  Return the charset that was used to parse Unit

   function Has_Diagnostics (Unit : Analysis_Unit'Class) return Boolean;
   --  Return whether this unit has associated diagnostics.

   function Diagnostics (Unit : Analysis_Unit'Class) return Diagnostics_Array;
   --  Return an array that contains the diagnostics associated to this unit.

   function Format_GNU_Diagnostic
     (Unit : Analysis_Unit'Class; D : Diagnostic) return String;
   --  Format a diagnostic in a GNU fashion. See
   --  <https://www.gnu.org/prep/standards/html_node/Errors.html>.

   pragma Warnings (Off, "defined after private extension");
   function Root (Unit : Analysis_Unit'Class) return Lkt_Node;
   --  Return the root node for this unit, or ``null`` if there is none.
   pragma Warnings (On, "defined after private extension");

   function First_Token (Unit : Analysis_Unit'Class) return Token_Reference;
   --  Return a reference to the first token scanned in this unit.

   function Last_Token (Unit : Analysis_Unit'Class) return Token_Reference;
   --  Return a reference to the last token scanned in this unit.

   function Token_Count (Unit : Analysis_Unit'Class) return Natural;
   --  Return the number of tokens in this unit.

   function Trivia_Count (Unit : Analysis_Unit'Class) return Natural;
   --  Return the number of trivias in this unit. This is 0 for units that were
   --  parsed with trivia analysis disabled.

   function Unit (Token : Token_Reference) return Analysis_Unit;
   --  Return the analysis unit that owns ``Token``

   function Text (Unit : Analysis_Unit'Class) return Text_Type;
   --  Return the source buffer associated to this unit.

   function Lookup_Token
     (Unit : Analysis_Unit'Class; Sloc : Source_Location)
      return Token_Reference;
   --  Look for a token in this unit that contains the given source location.
   --  If this falls before the first token, return the first token. If this
   --  falls between two tokens, return the token that appears before. If this
   --  falls after the last token, return the last token. If there is no token
   --  in this unit, return no token.

   procedure Dump_Lexical_Env (Unit : Analysis_Unit'Class);
   --  Debug helper: output the lexical envs for the given analysis unit.

   procedure Trigger_Envs_Debug (Is_Active : Boolean);
   --  Debug helper: activate debug traces for lexical envs lookups

   procedure Print (Unit : Analysis_Unit'Class; Show_Slocs : Boolean := True);
   --  Debug helper: output the AST and eventual diagnostic for this unit on
   --  standard output.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.

   procedure PP_Trivia (Unit : Analysis_Unit'Class);
   --  Debug helper: output a minimal AST with mixed trivias

   overriding function Get_Line
     (Unit : Analysis_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   type Child_Record (Kind : Child_Or_Trivia := Child) is record
      case Kind is
         when Child =>
            Node : Lkt_Node;
         when Trivia =>
            Trivia : Token_Reference;
      end case;
   end record;
   --  Variant that holds either an AST node or a token

   type Children_Array is private
      with Iterable => (First       => First,
                        Next        => Next,
                        Has_Element => Has_Element,
                        Element     => Element,
                        Last        => Last,
                        Previous    => Previous);
   --  This iterable type holds an array of ``Child`` or ``Trivia`` nodes

   function First (Self : Children_Array) return Natural;
   --  Return the first child or trivia cursor corresponding to the children
   --  array. Helper for the ``Iterable`` aspect.

   function Last (Self : Children_Array) return Natural;
   --  Return the last child or trivia cursor corresponding to the children
   --  array. Helper for the ``Iterable`` aspect.

   function Next (Self : Children_Array; Pos : Natural) return Natural;
   --  Return the child or trivia cursor that follows ``Self`` in the children
   --  array. Helper for the ``Iterable`` aspect.

   function Previous (Self : Children_Array; Pos : Natural) return Natural;
   --  Return the child or trivia cursor that follows ``Self`` in the children
   --  array. Helper for the ``Iterable`` aspect.

   function Has_Element (Self : Children_Array; Pos : Natural) return Boolean;
   --  Return if ``Pos`` is in ``Self``'s iteration range. Helper for the
   --  ``Iterable`` aspect.

   function Element (Self : Children_Array; Pos : Natural) return Child_Record;
   --  Return the child of trivia node at position ``Pos`` in ``Self``. Helper
   --  for the ``Iterable`` aspect.

   function Children_And_Trivia
     (Node : Lkt_Node'Class) return Children_Array;
   --  Return the children of this node interleaved with Trivia token nodes, so
   --  that:
   --
   --  - Every trivia contained between ``Node.Start_Token`` and
   --    ``Node.End_Token - 1`` will be part of the returned array.
   --
   --  - Nodes and trivias will be lexically ordered.

   ---------------------
   -- Composite types --
   ---------------------

            
   type Decoded_Char_Value is private;
   --  Result for ``CharLit.p_denoted_value``.
   --
   --  If that property is successful, set ``has_error`` to false and ``value``
   --  to the decoded character value. Otherwise, set ``has_error`` to true and
   --  ``error_sloc`` and ``error_message`` to give information about the
   --  decoding failure.

      
   function Value
     (Self : Decoded_Char_Value)
      return Character_Type
;
      
      
   function Has_Error
     (Self : Decoded_Char_Value)
      return Boolean
;
      
      
   function Error_Sloc
     (Self : Decoded_Char_Value)
      return Source_Location
;
      
      
   function Error_Message
     (Self : Decoded_Char_Value)
      return Text_Type
;
      

   
   
   function Create_Decoded_Char_Value
     (Value : Character_Type; Has_Error : Boolean; Error_Sloc : Source_Location; Error_Message : Text_Type)
     return Decoded_Char_Value
;

            
   type Decoded_String_Value is private;
   --  Result for ``StringLit.p_denoted_value``.
   --
   --  If that property is successful, set ``has_error`` to false and ``value``
   --  to the decoded string value. Otherwise, set ``has_error`` to true and
   --  ``error_sloc`` and ``error_message`` to give information about the
   --  decoding failure.

      
   function Value
     (Self : Decoded_String_Value)
      return Text_Type
;
      
      
   function Has_Error
     (Self : Decoded_String_Value)
      return Boolean
;
      
      
   function Error_Sloc
     (Self : Decoded_String_Value)
      return Source_Location
;
      
      
   function Error_Message
     (Self : Decoded_String_Value)
      return Text_Type
;
      

   
   
   function Create_Decoded_String_Value
     (Value : Text_Type; Has_Error : Boolean; Error_Sloc : Source_Location; Error_Message : Text_Type)
     return Decoded_String_Value
;

            
   type Lkt_Node_Array is
      array (Positive range <>) of Lkt_Node;


            
   type Logic_Context is private;
   --  Describes an interpretation of a reference. Can be attached to logic
   --  atoms (e.g. Binds) to indicate under which interpretation this
   --  particular atom was produced, which can in turn be used to produce
   --  informative diagnostics for resolution failures.

      
   function Ref_Node
     (Self : Logic_Context)
      return Lkt_Node'Class
;
      
      
   function Decl_Node
     (Self : Logic_Context)
      return Lkt_Node'Class
;
      

   
   
   function Create_Logic_Context
     (Ref_Node : Lkt_Node'Class; Decl_Node : Lkt_Node'Class)
     return Logic_Context
;

            
   type Logic_Context_Array is
      array (Positive range <>) of Logic_Context;


            
   type Solver_Diagnostic is private;
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

      
   function Message_Template
     (Self : Solver_Diagnostic)
      return Text_Type
;
      
      
   function Args
     (Self : Solver_Diagnostic)
      return Lkt_Node_Array
;
      
      
   function Location
     (Self : Solver_Diagnostic)
      return Lkt_Node'Class
;
      
      
   function Contexts
     (Self : Solver_Diagnostic)
      return Logic_Context_Array
;
      
      
   function Round
     (Self : Solver_Diagnostic)
      return Integer
;
      

   
   
   function Create_Solver_Diagnostic
     (Message_Template : Text_Type; Args : Lkt_Node_Array; Location : Lkt_Node'Class; Contexts : Logic_Context_Array; Round : Integer)
     return Solver_Diagnostic
;

            
   type Solver_Diagnostic_Array is
      array (Positive range <>) of Solver_Diagnostic;


            
   type Solver_Result is private;
   --  A pair returned by the ``Solve_With_Diagnostic`` primitive, consisting
   --  of:
   --
   --  * A ``Success`` field indicating whether resolution was successful or
   --    not.
   --
   --  * A ``Diagnostics`` field containing an array of diagnostics which may
   --    be non-empty if ``Success`` is ``False``.

      
   function Success
     (Self : Solver_Result)
      return Boolean
;
      
      
   function Diagnostics
     (Self : Solver_Result)
      return Solver_Diagnostic_Array
;
      

   
   
   function Create_Solver_Result
     (Success : Boolean; Diagnostics : Solver_Diagnostic_Array)
     return Solver_Result
;


   --------------------
   -- Token Iterator --
   --------------------

   type Token_Iterator is private
      with Iterable => (First       => First_Token,
                        Next        => Next_Token,
                        Has_Element => Has_Element,
                        Element     => Element);
   --  Allow iteration on a range of tokens corresponding to a node

   function First_Token (Self : Token_Iterator) return Token_Reference;
   --  Return the first token corresponding to the node

   function Next_Token
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference;
   --  Return the token that follows Tok in the token stream

   function Has_Element
     (Self : Token_Iterator; Tok : Token_Reference) return Boolean;
   --  Return if Tok is in Self's iteration range

   function Element
     (Self : Token_Iterator; Tok : Token_Reference) return Token_Reference;
   --  Identity function: helper for the Iterable aspect

   -------------------------
   -- AST Node primitives --
   -------------------------

   function Kind
     (Node : Lkt_Node'Class) return Lkt_Node_Kind_Type;
   function Kind_Name (Node : Lkt_Node'Class) return String;
   --  Return the concrete kind for Node

   pragma Warnings (Off, "defined after private extension");




         
   function Parent
     (Node : Lkt_Node'Class) return Lkt_Node;
   --  Return the syntactic parent for this node. Return null for the root
   --  node.
   --% belongs-to: Lkt_Node

         
   function Parents
     (Node : Lkt_Node'Class;
      With_Self : Boolean := True) return Lkt_Node_Array;
   --  Return an array that contains the lexical parents, this node included
   --  iff ``with_self`` is True. Nearer parents are first in the list.
   --% belongs-to: Lkt_Node

         
   function Children
     (Node : Lkt_Node'Class) return Lkt_Node_Array;
   --  Return an array that contains the direct lexical children.
   --
   --  .. warning:: This constructs a whole array every-time you call it, and
   --     as such is less efficient than calling the ``Child`` built-in.
   --% belongs-to: Lkt_Node

         
   function Token_Start
     (Node : Lkt_Node'Class) return Token_Reference;
   --  Return the first token used to parse this node.
   --% belongs-to: Lkt_Node

         
   function Token_End
     (Node : Lkt_Node'Class) return Token_Reference;
   --  Return the last token used to parse this node.
   --% belongs-to: Lkt_Node

         
   function Child_Index
     (Node : Lkt_Node'Class) return Integer;
   --  Return the 0-based index for Node in its parent's children.
   --% belongs-to: Lkt_Node

         
   function Previous_Sibling
     (Node : Lkt_Node'Class) return Lkt_Node;
   --  Return the node's previous sibling, or null if there is no such sibling.
   --% belongs-to: Lkt_Node

         
   function Next_Sibling
     (Node : Lkt_Node'Class) return Lkt_Node;
   --  Return the node's next sibling, or null if there is no such sibling.
   --% belongs-to: Lkt_Node

         
   function Unit
     (Node : Lkt_Node'Class) return Analysis_Unit;
   --  Return the analysis unit owning this node.
   --% belongs-to: Lkt_Node

         
   function Is_Ghost
     (Node : Lkt_Node'Class) return Boolean;
   --  Return whether the node is a ghost.
   --
   --  Unlike regular nodes, ghost nodes cover no token in the input source:
   --  they are logically located instead between two tokens. Both the
   --  ``token_start`` and the ``token_end`` of all ghost nodes is the token
   --  right after this logical position.
   --% belongs-to: Lkt_Node

         
   function Full_Sloc_Image
     (Node : Lkt_Node'Class) return Text_Type;
   --  Return a string containing the filename + the sloc in GNU conformant
   --  format. Useful to create diagnostics from a node.
   --% belongs-to: Lkt_Node

         
   function Completion_Item_Kind_To_Int
     (Node : Lkt_Node'Class;
      Kind : Completion_Item_Kind) return Integer;
   --  Convert a CompletionItemKind enum to its corresponding integer value.
   --% belongs-to: Lkt_Node

         
   function P_Set_Solver_Debug_Mode
     (Node : Lkt_Node'Class;
      Enable : Boolean) return Boolean;
   --  Enable or disable the solver traces for debugging purposes.
   --% belongs-to: Lkt_Node

         
   function P_Basic_Trait_Gen
     (Node : Lkt_Node'Class) return Generic_Decl;
   --  Unit method. Return the ``BasicTrait`` builtin generic trait.
   --% belongs-to: Lkt_Node

         
   function P_Basic_Trait
     (Node : Lkt_Node'Class) return Trait_Decl;
   --  Unit method. Return the ``BasicTrait`` builtin trait.
   --% belongs-to: Lkt_Node

         
   function P_Node_Gen_Trait
     (Node : Lkt_Node'Class) return Generic_Decl;
   --  Unit method. Return the ``Node`` builtin generic trait.
   --% belongs-to: Lkt_Node

         
   function P_Node_Trait
     (Node : Lkt_Node'Class) return Trait_Decl;
   --  Unit method. Return the ``Node`` builtin trait.
   --% belongs-to: Lkt_Node

         
   function P_Indexable_Gen_Trait
     (Node : Lkt_Node'Class) return Generic_Decl;
   --  Unit method. Return the ``Node`` builtin generic trait.
   --% belongs-to: Lkt_Node

         
   function P_Indexable_Trait
     (Node : Lkt_Node'Class) return Trait_Decl;
   --  Unit method. Return the ``Node`` builtin trait.
   --% belongs-to: Lkt_Node

         
   function P_Token_Node_Trait
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the ``TokenNode`` builtin trait.
   --% belongs-to: Lkt_Node

         
   function P_Error_Node_Trait
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the ``ErrorNode`` builtin trait.
   --% belongs-to: Lkt_Node

         
   function P_Char_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the character builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Int_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the integer builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Bool_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the boolean builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Bigint_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the big integer builtin type.
   --% belongs-to: Lkt_Node

         
   function P_String_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the string builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Symbol_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the string builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Property_Error_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the property error builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Regexp_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the regexp builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Entity_Gen_Type
     (Node : Lkt_Node'Class) return Generic_Decl;
   --  Unit method. Return the logicvar builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Entity_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the logicvar builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Logicvar_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the logicvar builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Equation_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the logicvar builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Array_Gen_Type
     (Node : Lkt_Node'Class) return Generic_Decl;
   --  Unit method. Return the array builtin generic type.
   --% belongs-to: Lkt_Node

         
   function P_Array_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the array builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Astlist_Gen_Type
     (Node : Lkt_Node'Class) return Generic_Decl;
   --  Unit method. Return the ASTList builtin generic type.
   --% belongs-to: Lkt_Node

         
   function P_Astlist_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the ASTList builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Node_Builder_Gen_Type
     (Node : Lkt_Node'Class) return Generic_Decl;
   --  Unit method. Return the NodeBuilder builtin generic type.
   --% belongs-to: Lkt_Node

         
   function P_Node_Builder_Type
     (Node : Lkt_Node'Class) return Named_Type_Decl;
   --  Unit method. Return the NodeBuilder builtin type.
   --% belongs-to: Lkt_Node

         
   function P_Iterator_Gen_Trait
     (Node : Lkt_Node'Class) return Generic_Decl;
   --  Unit method. Return the Iterator builtin generic trait.
   --% belongs-to: Lkt_Node

         
   function P_Iterator_Trait
     (Node : Lkt_Node'Class) return Trait_Decl;
   --  Unit method. Return the Iterator builtin trait.
   --% belongs-to: Lkt_Node

         
   function P_Analysis_Unit_Gen_Trait
     (Node : Lkt_Node'Class) return Generic_Decl;
   --  Unit method. Return the ``AnalysisUnit`` builtin generic trait.
   --% belongs-to: Lkt_Node

         
   function P_Analysis_Unit_Trait
     (Node : Lkt_Node'Class) return Trait_Decl;
   --  Unit method. Return the ``AnalysisUnit`` builtin trait.
   --% belongs-to: Lkt_Node

         
   function P_Topmost_Invalid_Decl
     (Node : Lkt_Node'Class) return Lkt_Node;
   --  Return the topmost (from ``Self`` to the root node) FullDecl annotated
   --  with ``@invalid``, null otherwise.
   --% belongs-to: Lkt_Node

         
   function P_Nameres_Diagnostics
     (Node : Lkt_Node'Class) return Solver_Diagnostic_Array;
   --  If name resolution on this lkt compilation unit fails, this returns all
   --  the diagnostics that were produced while resolving it.
   --% belongs-to: Lkt_Node

         
   function P_Solve_Enclosing_Context
     (Node : Lkt_Node'Class) return Solver_Result;
   --  Finds the nearest parent that is an xref_entry_point and solve its
   --  equation.
   --% belongs-to: Lkt_Node

         
   function P_Xref_Entry_Point
     (Node : Lkt_Node'Class) return Boolean;
   --  Designates entities that are entry point for the xref solving
   --  infrastructure. If this returns true, then nameres_diagnostics can be
   --  called on it.
   --% belongs-to: Lkt_Node






         
   function P_Get_Type
     (Node : Expr'Class) return Type_Decl;
   --  Return the type of this expression.
   --% belongs-to: Expr

         
   function P_Get_Generic_Type
     (Node : Expr'Class) return Type_Decl;
   --  Return the expected type of this expression.
   --% belongs-to: Expr

         
   function P_Get_Expected_Type
     (Node : Expr'Class) return Type_Decl;
   --  Return the expected type of this expression.
   --% belongs-to: Expr

         
   function P_Referenced_Decl
     (Node : Expr'Class) return Decl;
   --  Return the declaration referenced by this expression, if applicable,
   --  else null.
   --
   --  The property is memoized in order to avoid use the value inside logic
   --  variables on every redundent call, causing faulty behavior when used
   --  with rebindings. TODO: Do like LAL to avoid memoization for more safety.
   --% belongs-to: Expr





         
   

   function F_Expr
     (Node : Any_Of'Class) return Expr;
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
   --% belongs-to: Any_Of


         
   

   function F_Values
     (Node : Any_Of'Class) return Any_Of_List;
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
   --% belongs-to: Any_Of










         function List_Child
           (Node : Expr_List'Class; Index : Positive)
            return Expr;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Expr_List_First (Node : Expr_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Expr_List_Next
           (Node : Expr_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Expr_List_Has_Element
           (Node : Expr_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Expr_List_Element
           (Node : Expr_List; Cursor : Positive)
            return Expr'Class;
         --  Implementation detail for the Iterable aspect











         
   

   function F_Syn_Name
     (Node : Decl'Class) return Def_Id;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Decl



         
   function P_Custom_Image
     (Node : Decl'Class) return Text_Type;
   --  Return the image string using entity information.
   --% belongs-to: Decl

         
   function P_Decl_Type_Name
     (Node : Decl'Class) return Text_Type;
   --  Return the name of the declaration type, as it should be seen by
   --  users/shown in diagnostics.
   --% belongs-to: Decl

         
   function P_As_Bare_Decl
     (Node : Decl'Class) return Decl;
   --  Get this declaration without rebindings information.
   --% belongs-to: Decl

         
   function P_Get_Type
     (Node : Decl'Class) return Type_Decl;
   --  Return the type of the Decl.
   --% belongs-to: Decl

         
   function P_Get_Cast_Type
     (Node : Decl'Class;
      Cast_To : Type_Decl'Class) return Type_Decl;
   --  If we are casting an entity (Self) to something that is not an entity,
   --  make it an entity.
   --% belongs-to: Decl

         
   function P_Get_Keep_Type
     (Node : Decl'Class;
      Keep_Type : Type_Decl'Class) return Type_Decl;
   --  Return the type of Entity when we only keep elements of type keep_type.
   --  If we are casting an entity (Self) to something that is not an entity,
   --  make it an entity.
   --% belongs-to: Decl

         
   function P_Get_Suffix_Type
     (Node : Decl'Class;
      Prefix_Type : Type_Decl'Class) return Type_Decl;
   --  If we are accessing a ParseField of an entity, then that field's type
   --  also needs to be an entity.
   --% belongs-to: Decl

         
   function P_Is_Generic
     (Node : Decl'Class) return Boolean;
   --  Returns whether the Decl is generic.
   --% belongs-to: Decl

         
   function P_Return_Type_Is_Instantiated
     (Node : Decl'Class) return Boolean;
   --  Return True if the return type of this function is instantiated.
   --% belongs-to: Decl

         
   function P_Is_Instantiated
     (Node : Decl'Class) return Boolean;
   --  Return True if Self is an instantiated declaration, meaning that it does
   --  not use any of its declared generic types.
   --% belongs-to: Decl

         
   function P_Name
     (Node : Decl'Class) return Unbounded_Text_Type;
   --  Return the symbol corresponding to the name of this declaration.
   --% belongs-to: Decl

         
   function P_Full_Name
     (Node : Decl'Class) return Text_Type;
   --  Return the full name of this decl, as it should be seen by users/shown
   --  in diagnostics.
   --% belongs-to: Decl





         
   

   function F_Traits
     (Node : Type_Decl'Class) return Type_Ref_List;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Type_Decl


         
   

   function F_Syn_Base_Type
     (Node : Type_Decl'Class) return Type_Ref;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Type_Decl



         
   function P_Base_Type
     (Node : Type_Decl'Class) return Type_Ref;
   --  Return the base type for this node, if any.
   --% belongs-to: Type_Decl

         
   function P_Base_Type_If_Entity
     (Node : Type_Decl'Class) return Type_Decl;
   --  Return the base type for this node, if any.
   --% belongs-to: Type_Decl










         
   

   function F_Name
     (Node : Argument'Class) return Ref_Id;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Argument


         
   

   function F_Value
     (Node : Argument'Class) return Expr;
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
   --% belongs-to: Argument





         function List_Child
           (Node : Argument_List'Class; Index : Positive)
            return Argument;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Argument_List_First (Node : Argument_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Argument_List_Next
           (Node : Argument_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Argument_List_Has_Element
           (Node : Argument_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Argument_List_Element
           (Node : Argument_List; Cursor : Positive)
            return Argument'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Exprs
     (Node : Array_Literal'Class) return Expr_List;
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
   --% belongs-to: Array_Literal


         
   

   function F_Element_Type
     (Node : Array_Literal'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Array_Literal







         
   

   function F_Name
     (Node : Base_Call_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Base_Call_Expr


         
   

   function F_Args
     (Node : Base_Call_Expr'Class) return Argument_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Base_Call_Expr







         
   

   function F_Expr
     (Node : Base_Grammar_Rule_Decl'Class) return Grammar_Expr;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Base_Grammar_Rule_Decl







         
   

   function F_Send
     (Node : Base_Lexer_Case_Rule_Alt'Class) return Lexer_Case_Rule_Send;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Base_Lexer_Case_Rule_Alt





         function List_Child
           (Node : Base_Lexer_Case_Rule_Alt_List'Class; Index : Positive)
            return Base_Lexer_Case_Rule_Alt;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Base_Lexer_Case_Rule_Alt_List_First (Node : Base_Lexer_Case_Rule_Alt_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Base_Lexer_Case_Rule_Alt_List_Next
           (Node : Base_Lexer_Case_Rule_Alt_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Base_Lexer_Case_Rule_Alt_List_Has_Element
           (Node : Base_Lexer_Case_Rule_Alt_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Base_Lexer_Case_Rule_Alt_List_Element
           (Node : Base_Lexer_Case_Rule_Alt_List; Cursor : Positive)
            return Base_Lexer_Case_Rule_Alt'Class;
         --  Implementation detail for the Iterable aspect









         function List_Child
           (Node : Base_Pattern_List'Class; Index : Positive)
            return Base_Pattern;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Base_Pattern_List_First (Node : Base_Pattern_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Base_Pattern_List_Next
           (Node : Base_Pattern_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Base_Pattern_List_Has_Element
           (Node : Base_Pattern_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Base_Pattern_List_Element
           (Node : Base_Pattern_List; Cursor : Positive)
            return Base_Pattern'Class;
         --  Implementation detail for the Iterable aspect











         
   

   function F_Decls
     (Node : Named_Type_Decl'Class) return Decl_Block;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Named_Type_Decl






















         
   

   function F_Left
     (Node : Bin_Op'Class) return Expr;
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
   --% belongs-to: Bin_Op


         
   

   function F_Op
     (Node : Bin_Op'Class) return Op;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Bin_Op

      function F_Op
        (Node : Bin_Op'Class) return Lkt_Op;
      --% belongs-to: Bin_Op

         
   

   function F_Right
     (Node : Bin_Op'Class) return Expr;
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
   --% belongs-to: Bin_Op







         
   

   function F_Binding
     (Node : Binding_Pattern'Class) return Id;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Binding_Pattern


         
   

   function F_Value_Pattern
     (Node : Binding_Pattern'Class) return Base_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Node_Pattern`, :ada:ref:`Not_Pattern`,
   --  :ada:ref:`Null_Pattern`, :ada:ref:`Paren_Pattern`,
   --  :ada:ref:`Regex_Pattern`, :ada:ref:`Tuple_Pattern`,
   --  :ada:ref:`Universal_Pattern`
   --
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Binding_Pattern






         function Lkt_Node_List_First (Node : Lkt_Node_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Lkt_Node_List_Next
           (Node : Lkt_Node_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Lkt_Node_List_Has_Element
           (Node : Lkt_Node_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Lkt_Node_List_Element
           (Node : Lkt_Node_List; Cursor : Positive)
            return Lkt_Node'Class;
         --  Implementation detail for the Iterable aspect











         
   

   function F_Val_Defs
     (Node : Block_Expr'Class) return Block_Decl_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Val_Decl`, :ada:ref:`Var_Bind`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Block_Expr


         
   

   function F_Expr
     (Node : Block_Expr'Class) return Expr;
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
   --% belongs-to: Block_Expr










         function List_Child
           (Node : Block_String_Line_List'Class; Index : Positive)
            return Block_String_Line;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Block_String_Line_List_First (Node : Block_String_Line_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Block_String_Line_List_Next
           (Node : Block_String_Line_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Block_String_Line_List_Has_Element
           (Node : Block_String_Line_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Block_String_Line_List_Element
           (Node : Block_String_Line_List; Cursor : Positive)
            return Block_String_Line'Class;
         --  Implementation detail for the Iterable aspect







         
   function P_Denoted_Value
     (Node : String_Lit'Class) return Decoded_String_Value;
   --  Return the content of the given string literal node.
   --% belongs-to: String_Lit

         
   function P_Is_Prefixed_String
     (Node : String_Lit'Class) return Boolean;
   --  Return whether this string is prefixed or not.
   --% belongs-to: String_Lit

         
   function P_Prefix
     (Node : String_Lit'Class) return Character_Type;
   --  Return the prefix of this string, or the null character if there is no
   --  prefix.
   --% belongs-to: String_Lit

         
   function P_Is_Regexp_Literal
     (Node : String_Lit'Class) return Boolean;
   --  Return whether this string literal is actually a regexp literal, by
   --  checking that this string is prefixed by 'p'.
   --% belongs-to: String_Lit





         
   

   function F_Lines
     (Node : Block_String_Lit'Class) return Block_String_Line_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Block_String_Lit






























         function List_Child
           (Node : Call_Expr_List'Class; Index : Positive)
            return Call_Expr;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Call_Expr_List_First (Node : Call_Expr_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Call_Expr_List_Next
           (Node : Call_Expr_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Call_Expr_List_Has_Element
           (Node : Call_Expr_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Call_Expr_List_Element
           (Node : Call_Expr_List; Cursor : Positive)
            return Call_Expr'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Expr
     (Node : Cast_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Cast_Expr


         
   

   function F_Null_Cond
     (Node : Cast_Expr'Class) return Null_Cond_Qualifier;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Cast_Expr

      function F_Null_Cond (Node : Cast_Expr'Class) return Boolean;
      --% belongs-to: Cast_Expr


         
   

   function F_Excludes_Null
     (Node : Cast_Expr'Class) return Excludes_Null;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Cast_Expr

      function F_Excludes_Null (Node : Cast_Expr'Class) return Boolean;
      --% belongs-to: Cast_Expr


         
   

   function F_Dest_Type
     (Node : Cast_Expr'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Cast_Expr








         
   function P_Denoted_Value
     (Node : Char_Lit'Class) return Decoded_Char_Value;
   --  Return the content of the given character literal node.
   --% belongs-to: Char_Lit











         
   function P_As_Bool
     (Node : Class_Qualifier'Class) return Boolean;
   --  Return whether this node is present
   --% belongs-to: Class_Qualifier




















         
   

   function F_Decl_Type
     (Node : Explicitly_Typed_Decl'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Explicitly_Typed_Decl







         
   

   function F_Default_Val
     (Node : Component_Decl'Class) return Expr;
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
   --% belongs-to: Component_Decl







         
   

   function F_Name
     (Node : Decl_Annotation'Class) return Id;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Decl_Annotation


         
   

   function F_Args
     (Node : Decl_Annotation'Class) return Decl_Annotation_Args;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Decl_Annotation







         
   

   function F_Args
     (Node : Decl_Annotation_Args'Class) return Argument_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Decl_Annotation_Args





         function List_Child
           (Node : Decl_Annotation_List'Class; Index : Positive)
            return Decl_Annotation;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Decl_Annotation_List_First (Node : Decl_Annotation_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Decl_Annotation_List_Next
           (Node : Decl_Annotation_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Decl_Annotation_List_Has_Element
           (Node : Decl_Annotation_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Decl_Annotation_List_Element
           (Node : Decl_Annotation_List; Cursor : Positive)
            return Decl_Annotation'Class;
         --  Implementation detail for the Iterable aspect




         function List_Child
           (Node : Full_Decl_List'Class; Index : Positive)
            return Full_Decl;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Full_Decl_List_First (Node : Full_Decl_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Full_Decl_List_Next
           (Node : Full_Decl_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Full_Decl_List_Has_Element
           (Node : Full_Decl_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Full_Decl_List_Element
           (Node : Full_Decl_List; Cursor : Positive)
            return Full_Decl'Class;
         --  Implementation detail for the Iterable aspect












         
   function P_Custom_Image
     (Node : Id'Class) return Text_Type;
   --  Returns the image of this RefId using entity information.
   --% belongs-to: Id











         
   function P_Referenced_Decl
     (Node : Type_Ref'Class) return Type_Decl;
   --  Returns the referenced type declaration.
   --% belongs-to: Type_Ref










         
   

   function F_Prefix
     (Node : Dot_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Dot_Expr


         
   

   function F_Null_Cond
     (Node : Dot_Expr'Class) return Null_Cond_Qualifier;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Dot_Expr

      function F_Null_Cond (Node : Dot_Expr'Class) return Boolean;
      --% belongs-to: Dot_Expr


         
   

   function F_Suffix
     (Node : Dot_Expr'Class) return Ref_Id;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Dot_Expr

















         
   

   function F_Cond_Expr
     (Node : Elsif_Branch'Class) return Expr;
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
   --% belongs-to: Elsif_Branch


         
   

   function F_Then_Expr
     (Node : Elsif_Branch'Class) return Expr;
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
   --% belongs-to: Elsif_Branch





         function List_Child
           (Node : Elsif_Branch_List'Class; Index : Positive)
            return Elsif_Branch;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Elsif_Branch_List_First (Node : Elsif_Branch_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Elsif_Branch_List_Next
           (Node : Elsif_Branch_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Elsif_Branch_List_Has_Element
           (Node : Elsif_Branch_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Elsif_Branch_List_Element
           (Node : Elsif_Branch_List; Cursor : Positive)
            return Elsif_Branch'Class;
         --  Implementation detail for the Iterable aspect









         function List_Child
           (Node : Enum_Class_Alt_Decl_List'Class; Index : Positive)
            return Enum_Class_Alt_Decl;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Enum_Class_Alt_Decl_List_First (Node : Enum_Class_Alt_Decl_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Enum_Class_Alt_Decl_List_Next
           (Node : Enum_Class_Alt_Decl_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Enum_Class_Alt_Decl_List_Has_Element
           (Node : Enum_Class_Alt_Decl_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Enum_Class_Alt_Decl_List_Element
           (Node : Enum_Class_Alt_Decl_List; Cursor : Positive)
            return Enum_Class_Alt_Decl'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Decls
     (Node : Enum_Class_Case'Class) return Enum_Class_Alt_Decl_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Enum_Class_Case





         function List_Child
           (Node : Enum_Class_Case_List'Class; Index : Positive)
            return Enum_Class_Case;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Enum_Class_Case_List_First (Node : Enum_Class_Case_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Enum_Class_Case_List_Next
           (Node : Enum_Class_Case_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Enum_Class_Case_List_Has_Element
           (Node : Enum_Class_Case_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Enum_Class_Case_List_Element
           (Node : Enum_Class_Case_List; Cursor : Positive)
            return Enum_Class_Case'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Branches
     (Node : Enum_Class_Decl'Class) return Enum_Class_Case_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Enum_Class_Decl










         function List_Child
           (Node : Enum_Lit_Decl_List'Class; Index : Positive)
            return Enum_Lit_Decl;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Enum_Lit_Decl_List_First (Node : Enum_Lit_Decl_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Enum_Lit_Decl_List_Next
           (Node : Enum_Lit_Decl_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Enum_Lit_Decl_List_Has_Element
           (Node : Enum_Lit_Decl_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Enum_Lit_Decl_List_Element
           (Node : Enum_Lit_Decl_List; Cursor : Positive)
            return Enum_Lit_Decl'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Literals
     (Node : Enum_Type_Decl'Class) return Enum_Lit_Decl_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Enum_Type_Decl







         
   

   function F_Actions
     (Node : Env_Spec_Decl'Class) return Call_Expr_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Env_Spec_Decl







         
   

   function F_Expr
     (Node : Error_On_Null'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Error_On_Null








         
   function P_As_Bool
     (Node : Excludes_Null'Class) return Boolean;
   --  Return whether this node is present
   --% belongs-to: Excludes_Null




















         
   

   function F_Node_Pattern
     (Node : Extended_Node_Pattern'Class) return Value_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Type_Pattern`,
   --  :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Extended_Node_Pattern


         
   

   function F_Details
     (Node : Extended_Node_Pattern'Class) return Node_Pattern_Detail_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Extended_Node_Pattern







         
   

   function F_Trait_Ref
     (Node : Field_Decl'Class) return Dot_Expr;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Field_Decl







         
   

   function F_Pattern
     (Node : Filtered_Pattern'Class) return Base_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Node_Pattern`, :ada:ref:`Not_Pattern`,
   --  :ada:ref:`Null_Pattern`, :ada:ref:`Paren_Pattern`,
   --  :ada:ref:`Regex_Pattern`, :ada:ref:`Tuple_Pattern`,
   --  :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Filtered_Pattern


         
   

   function F_Predicate
     (Node : Filtered_Pattern'Class) return Expr;
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
   --% belongs-to: Filtered_Pattern







         
   

   function F_Doc
     (Node : Full_Decl'Class) return String_Lit;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Full_Decl


         
   

   function F_Decl_Annotations
     (Node : Full_Decl'Class) return Decl_Annotation_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Full_Decl


         
   

   function F_Decl
     (Node : Full_Decl'Class) return Decl;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Dyn_Var_Decl`, :ada:ref:`Env_Spec_Decl`,
   --  :ada:ref:`Field_Decl`, :ada:ref:`Fun_Decl`, :ada:ref:`Generic_Decl`,
   --  :ada:ref:`Generic_Param_Type_Decl`, :ada:ref:`Grammar_Decl`,
   --  :ada:ref:`Grammar_Rule_Decl`, :ada:ref:`Lexer_Decl`,
   --  :ada:ref:`Lexer_Family_Decl`, :ada:ref:`Named_Type_Decl`,
   --  :ada:ref:`Val_Decl`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Full_Decl



         
   function P_Has_Annotation
     (Node : Full_Decl'Class;
      Name : Unbounded_Text_Type) return Boolean;
   --  Return whether this node has an annotation with name ``name``.
   --% belongs-to: Full_Decl





         
   

   function F_Params
     (Node : Fun_Decl'Class) return Fun_Param_Decl_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Fun_Decl


         
   

   function F_Return_Type
     (Node : Fun_Decl'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Fun_Decl


         
   

   function F_Trait_Ref
     (Node : Fun_Decl'Class) return Dot_Expr;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Fun_Decl


         
   

   function F_Body
     (Node : Fun_Decl'Class) return Expr;
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
   --% belongs-to: Fun_Decl



         
   function P_Is_Dynamic_Combiner
     (Node : Fun_Decl'Class) return Boolean;
   --  When this property is used as a a combinder inside an NPropagate
   --  equation, return whether it expects a dynamic number of arguments.
   --% belongs-to: Fun_Decl





         
   

   function F_Decl_Annotations
     (Node : Fun_Param_Decl'Class) return Decl_Annotation_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Fun_Param_Decl





         function List_Child
           (Node : Fun_Param_Decl_List'Class; Index : Positive)
            return Fun_Param_Decl;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Fun_Param_Decl_List_First (Node : Fun_Param_Decl_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Fun_Param_Decl_List_Next
           (Node : Fun_Param_Decl_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Fun_Param_Decl_List_Has_Element
           (Node : Fun_Param_Decl_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Fun_Param_Decl_List_Element
           (Node : Fun_Param_Decl_List; Cursor : Positive)
            return Fun_Param_Decl'Class;
         --  Implementation detail for the Iterable aspect











         
   

   function F_Param_Types
     (Node : Function_Type_Ref'Class) return Type_Ref_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Function_Type_Ref


         
   

   function F_Return_Type
     (Node : Function_Type_Ref'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Function_Type_Ref







         
   

   function F_Generic_Param_Decls
     (Node : Generic_Decl'Class) return Generic_Param_Decl_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Generic_Decl


         
   

   function F_Decl
     (Node : Generic_Decl'Class) return Decl;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Dyn_Var_Decl`, :ada:ref:`Env_Spec_Decl`,
   --  :ada:ref:`Field_Decl`, :ada:ref:`Fun_Decl`, :ada:ref:`Generic_Decl`,
   --  :ada:ref:`Grammar_Decl`, :ada:ref:`Grammar_Rule_Decl`,
   --  :ada:ref:`Lexer_Decl`, :ada:ref:`Named_Type_Decl`, :ada:ref:`Val_Decl`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Generic_Decl







         
   

   function F_Name
     (Node : Generic_Instantiation'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Generic_Instantiation


         
   

   function F_Args
     (Node : Generic_Instantiation'Class) return Type_Ref_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Generic_Instantiation












         
   

   function F_Has_Class
     (Node : Generic_Param_Type_Decl'Class) return Class_Qualifier;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Generic_Param_Type_Decl

      function F_Has_Class (Node : Generic_Param_Type_Decl'Class) return Boolean;
      --% belongs-to: Generic_Param_Type_Decl







         
   

   function F_Type_Name
     (Node : Generic_Type_Ref'Class) return Expr;
   --  This field can contain one of the following nodes: :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Ref_Id`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Generic_Type_Ref


         
   

   function F_Args
     (Node : Generic_Type_Ref'Class) return Type_Ref_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Generic_Type_Ref

















         
   

   function F_Rules
     (Node : Grammar_Decl'Class) return Full_Decl_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Decl







         
   

   function F_Expr
     (Node : Grammar_Discard'Class) return Grammar_Expr;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Discard







         
   

   function F_Expr
     (Node : Grammar_Dont_Skip'Class) return Grammar_Expr;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Dont_Skip


         
   

   function F_Dont_Skip
     (Node : Grammar_Dont_Skip'Class) return Grammar_Expr;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Dont_Skip





         function List_Child
           (Node : Grammar_Expr_List'Class; Index : Positive)
            return Grammar_Expr;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Grammar_Expr_List_First (Node : Grammar_Expr_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Grammar_Expr_List_Next
           (Node : Grammar_Expr_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Grammar_Expr_List_Has_Element
           (Node : Grammar_Expr_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Grammar_Expr_List_Element
           (Node : Grammar_Expr_List; Cursor : Positive)
            return Grammar_Expr'Class;
         --  Implementation detail for the Iterable aspect




         function List_Child
           (Node : Grammar_Expr_List_List'Class; Index : Positive)
            return Grammar_Expr_List;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Grammar_Expr_List_List_First (Node : Grammar_Expr_List_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Grammar_Expr_List_List_Next
           (Node : Grammar_Expr_List_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Grammar_Expr_List_List_Has_Element
           (Node : Grammar_Expr_List_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Grammar_Expr_List_List_Element
           (Node : Grammar_Expr_List_List; Cursor : Positive)
            return Grammar_Expr_List'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Exprs
     (Node : Grammar_Pick'Class) return Grammar_Expr_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Pick












         
   

   function F_List_Type
     (Node : Grammar_List'Class) return Type_Ref;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_List


         
   

   function F_Kind
     (Node : Grammar_List'Class) return List_Kind;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_List

      function F_Kind
        (Node : Grammar_List'Class) return Lkt_List_Kind;
      --% belongs-to: Grammar_List

         
   

   function F_Expr
     (Node : Grammar_List'Class) return Grammar_Expr;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_List


         
   

   function F_Sep
     (Node : Grammar_List'Class) return Grammar_List_Sep;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Grammar_List







         
   

   function F_Token
     (Node : Grammar_List_Sep'Class) return Grammar_Expr;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_List_Sep


         
   

   function F_Extra
     (Node : Grammar_List_Sep'Class) return Id;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Grammar_List_Sep







         
   

   function F_Name
     (Node : Grammar_Null'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Null







         
   

   function F_Expr
     (Node : Grammar_Opt'Class) return Grammar_Expr;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Opt







         
   

   function F_Expr
     (Node : Grammar_Opt_Error'Class) return Grammar_Expr;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Opt_Error







         
   

   function F_Expr
     (Node : Grammar_Opt_Error_Group'Class) return Grammar_Expr_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Opt_Error_Group







         
   

   function F_Expr
     (Node : Grammar_Opt_Group'Class) return Grammar_Expr_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Opt_Group







         
   

   function F_Sub_Exprs
     (Node : Grammar_Or_Expr'Class) return Grammar_Expr_List_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Or_Expr







         
   

   function F_Expr
     (Node : Grammar_Predicate'Class) return Grammar_Expr;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Predicate


         
   

   function F_Prop_Ref
     (Node : Grammar_Predicate'Class) return Expr;
   --  This field can contain one of the following nodes: :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Ref_Id`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Predicate












         
   

   function F_Node_Name
     (Node : Grammar_Rule_Ref'Class) return Ref_Id;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Rule_Ref







         
   

   function F_Name
     (Node : Grammar_Skip'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Skip







         
   

   function F_Expr
     (Node : Grammar_Stop_Cut'Class) return Grammar_Expr;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Grammar_Stop_Cut







         
   

   function F_Cond_Expr
     (Node : If_Expr'Class) return Expr;
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
   --% belongs-to: If_Expr


         
   

   function F_Then_Expr
     (Node : If_Expr'Class) return Expr;
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
   --% belongs-to: If_Expr


         
   

   function F_Alternatives
     (Node : If_Expr'Class) return Elsif_Branch_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: If_Expr


         
   

   function F_Else_Expr
     (Node : If_Expr'Class) return Expr;
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
   --% belongs-to: If_Expr







         
   

   function F_Name
     (Node : Import'Class) return Module_Ref_Id;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Import



         
   function P_Referenced_Unit
     (Node : Import'Class) return Analysis_Unit;
   --  Return the unit that this import statements designates. Load it if
   --  needed.
   --% belongs-to: Import



         function List_Child
           (Node : Import_List'Class; Index : Positive)
            return Import;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Import_List_First (Node : Import_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Import_List_Next
           (Node : Import_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Import_List_Has_Element
           (Node : Import_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Import_List_Element
           (Node : Import_List; Cursor : Positive)
            return Import'Class;
         --  Implementation detail for the Iterable aspect











         
   

   function F_Expr
     (Node : Isa'Class) return Expr;
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
   --% belongs-to: Isa


         
   

   function F_Pattern
     (Node : Isa'Class) return Base_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Filtered_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Node_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Isa







         
   

   function F_Expr
     (Node : Keep_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Keep_Expr


         
   

   function F_Null_Cond
     (Node : Keep_Expr'Class) return Null_Cond_Qualifier;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Keep_Expr

      function F_Null_Cond (Node : Keep_Expr'Class) return Boolean;
      --% belongs-to: Keep_Expr


         
   

   function F_Keep_Type
     (Node : Keep_Expr'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Keep_Expr







         
   

   function F_Params
     (Node : Lambda_Expr'Class) return Lambda_Param_Decl_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Lambda_Expr


         
   

   function F_Return_Type
     (Node : Lambda_Expr'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Lambda_Expr


         
   

   function F_Body
     (Node : Lambda_Expr'Class) return Expr;
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
   --% belongs-to: Lambda_Expr










         function List_Child
           (Node : Lambda_Param_Decl_List'Class; Index : Positive)
            return Lambda_Param_Decl;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Lambda_Param_Decl_List_First (Node : Lambda_Param_Decl_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Lambda_Param_Decl_List_Next
           (Node : Lambda_Param_Decl_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Lambda_Param_Decl_List_Has_Element
           (Node : Lambda_Param_Decl_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Lambda_Param_Decl_List_Element
           (Node : Lambda_Param_Decl_List; Cursor : Positive)
            return Lambda_Param_Decl'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Imports
     (Node : Langkit_Root'Class) return Import_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Langkit_Root


         
   

   function F_Decls
     (Node : Langkit_Root'Class) return Full_Decl_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Langkit_Root



         
   function P_Fetch_Prelude
     (Node : Langkit_Root'Class) return Analysis_Unit;
   --  External property that will fetch the prelude unit, containing
   --  predefined types and values.
   --% belongs-to: Langkit_Root





         
   

   function F_Expr
     (Node : Lexer_Case_Rule'Class) return Grammar_Expr;
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
   --% belongs-to: Lexer_Case_Rule


         
   

   function F_Alts
     (Node : Lexer_Case_Rule'Class) return Base_Lexer_Case_Rule_Alt_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Lexer_Case_Rule







         
   

   function F_Cond_Exprs
     (Node : Lexer_Case_Rule_Cond_Alt'Class) return Ref_Id_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Lexer_Case_Rule_Cond_Alt












         
   

   function F_Sent
     (Node : Lexer_Case_Rule_Send'Class) return Ref_Id;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Lexer_Case_Rule_Send


         
   

   function F_Match_Size
     (Node : Lexer_Case_Rule_Send'Class) return Num_Lit;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Lexer_Case_Rule_Send







         
   

   function F_Rules
     (Node : Lexer_Decl'Class) return Lkt_Node_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Full_Decl`, :ada:ref:`Lexer_Case_Rule`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Lexer_Decl







         
   

   function F_Rules
     (Node : Lexer_Family_Decl'Class) return Full_Decl_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Lexer_Family_Decl






















         
   

   function F_Patterns
     (Node : List_Pattern'Class) return Base_Pattern_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Node_Pattern`, :ada:ref:`Not_Pattern`,
   --  :ada:ref:`Null_Pattern`, :ada:ref:`Paren_Pattern`,
   --  :ada:ref:`Regex_Pattern`, :ada:ref:`Splat_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: List_Pattern







         
   

   function F_Dest_Var
     (Node : Logic_Assign'Class) return Expr;
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
   --% belongs-to: Logic_Assign


         
   

   function F_Value
     (Node : Logic_Assign'Class) return Expr;
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
   --% belongs-to: Logic_Assign












         
   

   function F_Expr
     (Node : Logic_Expr'Class) return Expr;
   --  This field can contain one of the following nodes: :ada:ref:`Call_Expr`,
   --  :ada:ref:`Ref_Id`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Logic_Expr












         
   

   function F_Dest_Var
     (Node : Logic_Propagate'Class) return Expr;
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
   --% belongs-to: Logic_Propagate


         
   

   function F_Call
     (Node : Logic_Propagate'Class) return Logic_Propagate_Call;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Logic_Propagate












         
   

   function F_Lhs
     (Node : Logic_Unify'Class) return Expr;
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
   --% belongs-to: Logic_Unify


         
   

   function F_Rhs
     (Node : Logic_Unify'Class) return Expr;
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
   --% belongs-to: Logic_Unify







         
   

   function F_Decl
     (Node : Match_Branch'Class) return Match_Val_Decl;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Match_Branch


         
   

   function F_Expr
     (Node : Match_Branch'Class) return Expr;
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
   --% belongs-to: Match_Branch





         function List_Child
           (Node : Match_Branch_List'Class; Index : Positive)
            return Match_Branch;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Match_Branch_List_First (Node : Match_Branch_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Match_Branch_List_Next
           (Node : Match_Branch_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Match_Branch_List_Has_Element
           (Node : Match_Branch_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Match_Branch_List_Element
           (Node : Match_Branch_List; Cursor : Positive)
            return Match_Branch'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Match_Expr
     (Node : Match_Expr'Class) return Expr;
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
   --% belongs-to: Match_Expr


         
   

   function F_Branches
     (Node : Match_Expr'Class) return Match_Branch_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Match_Expr

























         function List_Child
           (Node : Node_Pattern_Detail_List'Class; Index : Positive)
            return Node_Pattern_Detail;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Node_Pattern_Detail_List_First (Node : Node_Pattern_Detail_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Node_Pattern_Detail_List_Next
           (Node : Node_Pattern_Detail_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Node_Pattern_Detail_List_Has_Element
           (Node : Node_Pattern_Detail_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Node_Pattern_Detail_List_Element
           (Node : Node_Pattern_Detail_List; Cursor : Positive)
            return Node_Pattern_Detail'Class;
         --  Implementation detail for the Iterable aspect






         
   

   function F_Id
     (Node : Node_Pattern_Field'Class) return Id;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Node_Pattern_Field


         
   

   function F_Expected_Value
     (Node : Node_Pattern_Field'Class) return Base_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Filtered_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Node_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Node_Pattern_Field







         
   

   function F_Call
     (Node : Node_Pattern_Property'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Node_Pattern_Property


         
   

   function F_Expected_Value
     (Node : Node_Pattern_Property'Class) return Base_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Filtered_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Node_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Node_Pattern_Property







         
   

   function F_Call
     (Node : Node_Pattern_Selector'Class) return Selector_Call;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Node_Pattern_Selector


         
   

   function F_Pattern
     (Node : Node_Pattern_Selector'Class) return Base_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Filtered_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Node_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Node_Pattern_Selector







         
   

   function F_Expr
     (Node : Not_Expr'Class) return Expr;
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
   --% belongs-to: Not_Expr







         
   

   function F_Pattern
     (Node : Not_Pattern'Class) return Base_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Node_Pattern`, :ada:ref:`Not_Pattern`,
   --  :ada:ref:`Null_Pattern`, :ada:ref:`Paren_Pattern`,
   --  :ada:ref:`Regex_Pattern`, :ada:ref:`Tuple_Pattern`,
   --  :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Not_Pattern








         
   function P_As_Bool
     (Node : Null_Cond_Qualifier'Class) return Boolean;
   --  Return whether this node is present
   --% belongs-to: Null_Cond_Qualifier















         
   

   function F_Dest_Type
     (Node : Null_Lit'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Null_Lit






































































































         
   

   function F_Left
     (Node : Or_Pattern'Class) return Base_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Filtered_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Node_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Or_Pattern


         
   

   function F_Right
     (Node : Or_Pattern'Class) return Base_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Filtered_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Node_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Or_Pattern







         
   

   function F_Expr
     (Node : Paren_Expr'Class) return Expr;
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
   --% belongs-to: Paren_Expr







         
   

   function F_Pattern
     (Node : Paren_Pattern'Class) return Base_Pattern;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Filtered_Pattern`, :ada:ref:`Integer_Pattern`,
   --  :ada:ref:`List_Pattern`, :ada:ref:`Node_Pattern`,
   --  :ada:ref:`Not_Pattern`, :ada:ref:`Null_Pattern`, :ada:ref:`Or_Pattern`,
   --  :ada:ref:`Paren_Pattern`, :ada:ref:`Regex_Pattern`,
   --  :ada:ref:`Tuple_Pattern`, :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Paren_Pattern







         
   

   function F_Node_Name
     (Node : Parse_Node_Expr'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Parse_Node_Expr


         
   

   function F_Sub_Exprs
     (Node : Parse_Node_Expr'Class) return Grammar_Expr_List;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Parse_Node_Expr

















         
   

   function F_Dest_Type
     (Node : Raise_Expr'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Raise_Expr


         
   

   function F_Except_Expr
     (Node : Raise_Expr'Class) return Expr;
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
   --% belongs-to: Raise_Expr










         function List_Child
           (Node : Ref_Id_List'Class; Index : Positive)
            return Ref_Id;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Ref_Id_List_First (Node : Ref_Id_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Ref_Id_List_Next
           (Node : Ref_Id_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Ref_Id_List_Has_Element
           (Node : Ref_Id_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Ref_Id_List_Element
           (Node : Ref_Id_List; Cursor : Positive)
            return Ref_Id'Class;
         --  Implementation detail for the Iterable aspect











         
   

   function F_Quantifier
     (Node : Selector_Call'Class) return Id;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Selector_Call


         
   

   function F_Binding
     (Node : Selector_Call'Class) return Id;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Selector_Call


         
   

   function F_Selector_Call
     (Node : Selector_Call'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Selector_Call












         
   

   function F_Type_Name
     (Node : Simple_Type_Ref'Class) return Expr;
   --  This field can contain one of the following nodes: :ada:ref:`Dot_Expr`,
   --  :ada:ref:`Ref_Id`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Simple_Type_Ref







         
   

   function F_Binding
     (Node : Splat_Pattern'Class) return Id;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Splat_Pattern












         
   

   function F_Prefix
     (Node : Subscript_Expr'Class) return Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Array_Literal`, :ada:ref:`Block_Expr`, :ada:ref:`Call_Expr`,
   --  :ada:ref:`Cast_Expr`, :ada:ref:`Dot_Expr`, :ada:ref:`Error_On_Null`,
   --  :ada:ref:`Generic_Instantiation`, :ada:ref:`Keep_Expr`, :ada:ref:`Lit`,
   --  :ada:ref:`Logic_Expr`, :ada:ref:`Logic_Predicate`,
   --  :ada:ref:`Match_Expr`, :ada:ref:`Paren_Expr`, :ada:ref:`Ref_Id`,
   --  :ada:ref:`Subscript_Expr`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Subscript_Expr


         
   

   function F_Null_Cond
     (Node : Subscript_Expr'Class) return Null_Cond_Qualifier;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Subscript_Expr

      function F_Null_Cond (Node : Subscript_Expr'Class) return Boolean;
      --% belongs-to: Subscript_Expr


         
   

   function F_Index
     (Node : Subscript_Expr'Class) return Expr;
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
   --% belongs-to: Subscript_Expr




















         function List_Child
           (Node : Type_Ref_List'Class; Index : Positive)
            return Type_Ref;
         --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has
         --  no such child.

         function Type_Ref_List_First (Node : Type_Ref_List) return Positive;
         --  Implementation detail for the Iterable aspect

         function Type_Ref_List_Next
           (Node : Type_Ref_List; Cursor : Positive) return Positive;
         --  Implementation detail for the Iterable aspect

         function Type_Ref_List_Has_Element
           (Node : Type_Ref_List; Cursor : Positive) return Boolean;
         --  Implementation detail for the Iterable aspect

         function Type_Ref_List_Element
           (Node : Type_Ref_List; Cursor : Positive)
            return Type_Ref'Class;
         --  Implementation detail for the Iterable aspect












         
   function P_Denoted_Value
     (Node : Token_Lit'Class) return Decoded_String_Value;
   --  Return the content of the given token literal node.
   --% belongs-to: Token_Lit





         
   

   function F_Lit
     (Node : Token_No_Case_Lit'Class) return Token_Lit;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Token_No_Case_Lit







         
   

   function F_Left
     (Node : Token_Pattern_Concat'Class) return Grammar_Expr;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Token_Pattern_Concat`, :ada:ref:`Token_Pattern_Lit`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Token_Pattern_Concat


         
   

   function F_Right
     (Node : Token_Pattern_Concat'Class) return Token_Pattern_Lit;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Token_Pattern_Concat








         
   function P_Denoted_Value
     (Node : Token_Pattern_Lit'Class) return Decoded_String_Value;
   --  Return the content of the given token pattern literal node.
   --% belongs-to: Token_Pattern_Lit





         
   

   function F_Token_Name
     (Node : Token_Ref'Class) return Ref_Id;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Token_Ref


         
   

   function F_Expr
     (Node : Token_Ref'Class) return Token_Lit;
   --  This field may be null even when there are no parsing errors.
   --% belongs-to: Token_Ref












         
   

   function F_Try_Expr
     (Node : Try_Expr'Class) return Expr;
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
   --% belongs-to: Try_Expr


         
   

   function F_Or_Expr
     (Node : Try_Expr'Class) return Expr;
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
   --% belongs-to: Try_Expr







         
   

   function F_Patterns
     (Node : Tuple_Pattern'Class) return Base_Pattern_List;
   --  This field contains a list that itself contains one of the following
   --  nodes: :ada:ref:`Binding_Pattern`, :ada:ref:`Bool_Pattern`,
   --  :ada:ref:`Integer_Pattern`, :ada:ref:`List_Pattern`,
   --  :ada:ref:`Node_Pattern`, :ada:ref:`Not_Pattern`,
   --  :ada:ref:`Null_Pattern`, :ada:ref:`Paren_Pattern`,
   --  :ada:ref:`Regex_Pattern`, :ada:ref:`Tuple_Pattern`,
   --  :ada:ref:`Universal_Pattern`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Tuple_Pattern







         
   

   function F_Type_Name
     (Node : Type_Pattern'Class) return Type_Ref;
   --  This field can contain one of the following nodes:
   --  :ada:ref:`Function_Type_Ref`, :ada:ref:`Generic_Type_Ref`,
   --  :ada:ref:`Simple_Type_Ref`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Type_Pattern







         
   

   function F_Op
     (Node : Un_Op'Class) return Op;
   --  This field can contain one of the following nodes: :ada:ref:`Op_Minus`,
   --  :ada:ref:`Op_Plus`
   --
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Un_Op

      function F_Op
        (Node : Un_Op'Class) return Lkt_Op;
      --% belongs-to: Un_Op

         
   

   function F_Expr
     (Node : Un_Op'Class) return Expr;
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
   --% belongs-to: Un_Op












         
   

   function F_Expr
     (Node : Val_Decl'Class) return Expr;
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
   --% belongs-to: Val_Decl







         
   

   function F_Name
     (Node : Var_Bind'Class) return Ref_Id;
   --  When there are no parsing errors, this field is never null.
   --% belongs-to: Var_Bind


         
   

   function F_Expr
     (Node : Var_Bind'Class) return Expr;
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
   --% belongs-to: Var_Bind




   pragma Warnings (On, "defined after private extension");

   -------------------------------
   -- Tree traversal operations --
   -------------------------------

   function Children_Count
     (Node : Lkt_Node'Class) return Natural;
   --  Return the number of children ``Node`` has

   function First_Child_Index
     (Node : Lkt_Node'Class) return Natural;
   --  Return the index of the first child ``Node`` has

   function Last_Child_Index
     (Node : Lkt_Node'Class) return Natural;
   --  Return the index of the last child ``Node`` has, or 0 if there is no
   --  child.

   pragma Warnings (Off, "defined after private extension");
   procedure Get_Child
     (Node            : Lkt_Node'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Lkt_Node);
   --  Return the ``Index``'th child of node, storing it into ``Result``.
   --
   --  Child indexing is 1-based. Store in ``Index_In_Bounds`` whether ``Node``
   --  had such a child: if not (i.e. ``Index`` is out-of-bounds), set
   --  ``Result`` to a null node.

   function Child
     (Node  : Lkt_Node'Class;
      Index : Positive)
      return Lkt_Node;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function First_Child
     (Node : Lkt_Node'Class) return Lkt_Node;
   --  Return the first child ``Node`` has, or ``No_Lkt_Node``
   --  if there is none.

   function Last_Child
     (Node : Lkt_Node'Class) return Lkt_Node;
   --  Return the last child ``Node`` has, or ``No_Lkt_Node`` if
   --  there is none.

   function Closest_Common_Parent
     (Self, Other : Lkt_Node'Class)
      return Lkt_Node;
   --  If ``Self`` and ``Other`` do not belong to the same analysis unit,
   --  return ``No_Lkt_Node``. Otherwise, return the deepest
   --  node in the tree that is a parent for both ``Self`` and ``Other``.
   pragma Warnings (On, "defined after private extension");

   function Traverse
     (Node  : Lkt_Node'Class;
      Visit : access function (Node : Lkt_Node'Class)
                               return Visit_Status)
     return Visit_Status;
   --  Call ``Visit`` on ``Node`` and all its children, transitively. Calls
   --  happen in prefix order (i.e. top-down and left first). The traversal is
   --  controlled as follows by the result returned by Visit:
   --
   --  ``Into``
   --     The traversal continues normally with the syntactic children of the
   --     node just processed.
   --
   --  ``Over``
   --     The children of the node just processed are skipped and excluded from
   --     the traversal, but otherwise processing continues elsewhere in the
   --     tree.
   --
   --  ``Stop``
   --     The entire traversal is immediately abandoned, and the original call
   --     to ``Traverse`` returns ``Stop``.

   procedure Traverse
     (Node  : Lkt_Node'Class;
      Visit : access function (Node : Lkt_Node'Class)
                               return Visit_Status);
   --  This is the same as ``Traverse`` function except that no result is
   --  returned i.e. the ``Traverse`` function is called and the result is
   --  simply discarded.

   ----------------------------------------
   -- Source location-related operations --
   ----------------------------------------

   function Sloc_Range
     (Node : Lkt_Node'Class) return Source_Location_Range;
   --  Return the source location range corresponding to the set of tokens from
   --  which Node was parsed.

   function Compare
     (Node : Lkt_Node'Class;
      Sloc : Source_Location) return Relative_Position;
   --  Compare Sloc to the sloc range of Node

   pragma Warnings (Off, "defined after private extension");
   function Lookup
     (Node : Lkt_Node'Class;
      Sloc : Source_Location) return Lkt_Node;
   --  Look for the bottom-most AST node whose sloc range contains Sloc. Return
   --  it, or null if no such node was found.
   pragma Warnings (On, "defined after private extension");

   -----------------------
   -- Lexical utilities --
   -----------------------

   function Text (Node : Lkt_Node'Class) return Text_Type;
   --  Return the source buffer slice corresponding to the text that spans
   --  between the first and the last tokens of this node.
   --
   --  Note that this returns the empty string for synthetic nodes.

   function Token_Range
     (Node : Lkt_Node'Class) return Token_Iterator;
   --  Return an iterator on the range of tokens encompassed by Node

   


   -------------------
   -- Debug helpers --
   -------------------

   procedure Print
     (Node        : Lkt_Node'Class;
      Show_Slocs  : Boolean := True;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children.
   --
   --  If Show_Slocs, include AST nodes' source locations in the output.
   --
   --  Line_Prefix is prepended to each output line.

   procedure PP_Trivia
     (Node        : Lkt_Node'Class;
      Line_Prefix : String := "");
   --  Debug helper: print to standard output Node and all its children along
   --  with the trivia associated to them. Line_Prefix is prepended to each
   --  output line.

   procedure Assign_Names_To_Logic_Vars (Node : Lkt_Node'Class);
   --  Debug helper: Assign names to every logical variable in the root node,
   --  so that we can trace logical variables.

   --  The following As_* functions convert references to nodes from one type
   --  to another (Lkt_Node can refer to any node type). They
   --  raise a Constraint_Error if the conversion is invalid.

   pragma Warnings (Off, "defined after private extension");
      function As_Lkt_Node
        (Node : Lkt_Node'Class) return Lkt_Node;
      --% no-document: True
      function As_Expr
        (Node : Lkt_Node'Class) return Expr;
      --% no-document: True
      function As_Any_Of
        (Node : Lkt_Node'Class) return Any_Of;
      --% no-document: True
      function As_Lkt_Node_Base_List
        (Node : Lkt_Node'Class) return Lkt_Node_Base_List;
      --% no-document: True
      function As_Expr_List
        (Node : Lkt_Node'Class) return Expr_List;
      --% no-document: True
      function As_Any_Of_List
        (Node : Lkt_Node'Class) return Any_Of_List;
      --% no-document: True
      function As_Decl
        (Node : Lkt_Node'Class) return Decl;
      --% no-document: True
      function As_Type_Decl
        (Node : Lkt_Node'Class) return Type_Decl;
      --% no-document: True
      function As_Any_Type_Decl
        (Node : Lkt_Node'Class) return Any_Type_Decl;
      --% no-document: True
      function As_Argument
        (Node : Lkt_Node'Class) return Argument;
      --% no-document: True
      function As_Argument_List
        (Node : Lkt_Node'Class) return Argument_List;
      --% no-document: True
      function As_Array_Literal
        (Node : Lkt_Node'Class) return Array_Literal;
      --% no-document: True
      function As_Base_Call_Expr
        (Node : Lkt_Node'Class) return Base_Call_Expr;
      --% no-document: True
      function As_Base_Grammar_Rule_Decl
        (Node : Lkt_Node'Class) return Base_Grammar_Rule_Decl;
      --% no-document: True
      function As_Base_Lexer_Case_Rule_Alt
        (Node : Lkt_Node'Class) return Base_Lexer_Case_Rule_Alt;
      --% no-document: True
      function As_Base_Lexer_Case_Rule_Alt_List
        (Node : Lkt_Node'Class) return Base_Lexer_Case_Rule_Alt_List;
      --% no-document: True
      function As_Base_Pattern
        (Node : Lkt_Node'Class) return Base_Pattern;
      --% no-document: True
      function As_Base_Pattern_List
        (Node : Lkt_Node'Class) return Base_Pattern_List;
      --% no-document: True
      function As_Base_Val_Decl
        (Node : Lkt_Node'Class) return Base_Val_Decl;
      --% no-document: True
      function As_Named_Type_Decl
        (Node : Lkt_Node'Class) return Named_Type_Decl;
      --% no-document: True
      function As_Basic_Class_Decl
        (Node : Lkt_Node'Class) return Basic_Class_Decl;
      --% no-document: True
      function As_Lit
        (Node : Lkt_Node'Class) return Lit;
      --% no-document: True
      function As_Big_Num_Lit
        (Node : Lkt_Node'Class) return Big_Num_Lit;
      --% no-document: True
      function As_Bin_Op
        (Node : Lkt_Node'Class) return Bin_Op;
      --% no-document: True
      function As_Binding_Pattern
        (Node : Lkt_Node'Class) return Binding_Pattern;
      --% no-document: True
      function As_Lkt_Node_List
        (Node : Lkt_Node'Class) return Lkt_Node_List;
      --% no-document: True
      function As_Block_Decl_List
        (Node : Lkt_Node'Class) return Block_Decl_List;
      --% no-document: True
      function As_Block_Expr
        (Node : Lkt_Node'Class) return Block_Expr;
      --% no-document: True
      function As_Block_String_Line
        (Node : Lkt_Node'Class) return Block_String_Line;
      --% no-document: True
      function As_Block_String_Line_List
        (Node : Lkt_Node'Class) return Block_String_Line_List;
      --% no-document: True
      function As_String_Lit
        (Node : Lkt_Node'Class) return String_Lit;
      --% no-document: True
      function As_Block_String_Lit
        (Node : Lkt_Node'Class) return Block_String_Lit;
      --% no-document: True
      function As_Value_Pattern
        (Node : Lkt_Node'Class) return Value_Pattern;
      --% no-document: True
      function As_Bool_Pattern
        (Node : Lkt_Node'Class) return Bool_Pattern;
      --% no-document: True
      function As_Bool_Pattern_False
        (Node : Lkt_Node'Class) return Bool_Pattern_False;
      --% no-document: True
      function As_Bool_Pattern_True
        (Node : Lkt_Node'Class) return Bool_Pattern_True;
      --% no-document: True
      function As_Call_Expr
        (Node : Lkt_Node'Class) return Call_Expr;
      --% no-document: True
      function As_Call_Expr_List
        (Node : Lkt_Node'Class) return Call_Expr_List;
      --% no-document: True
      function As_Cast_Expr
        (Node : Lkt_Node'Class) return Cast_Expr;
      --% no-document: True
      function As_Char_Lit
        (Node : Lkt_Node'Class) return Char_Lit;
      --% no-document: True
      function As_Class_Decl
        (Node : Lkt_Node'Class) return Class_Decl;
      --% no-document: True
      function As_Class_Qualifier
        (Node : Lkt_Node'Class) return Class_Qualifier;
      --% no-document: True
      function As_Class_Qualifier_Absent
        (Node : Lkt_Node'Class) return Class_Qualifier_Absent;
      --% no-document: True
      function As_Class_Qualifier_Present
        (Node : Lkt_Node'Class) return Class_Qualifier_Present;
      --% no-document: True
      function As_User_Val_Decl
        (Node : Lkt_Node'Class) return User_Val_Decl;
      --% no-document: True
      function As_Explicitly_Typed_Decl
        (Node : Lkt_Node'Class) return Explicitly_Typed_Decl;
      --% no-document: True
      function As_Component_Decl
        (Node : Lkt_Node'Class) return Component_Decl;
      --% no-document: True
      function As_Decl_Annotation
        (Node : Lkt_Node'Class) return Decl_Annotation;
      --% no-document: True
      function As_Decl_Annotation_Args
        (Node : Lkt_Node'Class) return Decl_Annotation_Args;
      --% no-document: True
      function As_Decl_Annotation_List
        (Node : Lkt_Node'Class) return Decl_Annotation_List;
      --% no-document: True
      function As_Full_Decl_List
        (Node : Lkt_Node'Class) return Full_Decl_List;
      --% no-document: True
      function As_Decl_Block
        (Node : Lkt_Node'Class) return Decl_Block;
      --% no-document: True
      function As_Id
        (Node : Lkt_Node'Class) return Id;
      --% no-document: True
      function As_Def_Id
        (Node : Lkt_Node'Class) return Def_Id;
      --% no-document: True
      function As_Type_Ref
        (Node : Lkt_Node'Class) return Type_Ref;
      --% no-document: True
      function As_Default_List_Type_Ref
        (Node : Lkt_Node'Class) return Default_List_Type_Ref;
      --% no-document: True
      function As_Dot_Expr
        (Node : Lkt_Node'Class) return Dot_Expr;
      --% no-document: True
      function As_Dyn_Env_Wrapper
        (Node : Lkt_Node'Class) return Dyn_Env_Wrapper;
      --% no-document: True
      function As_Dyn_Var_Decl
        (Node : Lkt_Node'Class) return Dyn_Var_Decl;
      --% no-document: True
      function As_Elsif_Branch
        (Node : Lkt_Node'Class) return Elsif_Branch;
      --% no-document: True
      function As_Elsif_Branch_List
        (Node : Lkt_Node'Class) return Elsif_Branch_List;
      --% no-document: True
      function As_Enum_Class_Alt_Decl
        (Node : Lkt_Node'Class) return Enum_Class_Alt_Decl;
      --% no-document: True
      function As_Enum_Class_Alt_Decl_List
        (Node : Lkt_Node'Class) return Enum_Class_Alt_Decl_List;
      --% no-document: True
      function As_Enum_Class_Case
        (Node : Lkt_Node'Class) return Enum_Class_Case;
      --% no-document: True
      function As_Enum_Class_Case_List
        (Node : Lkt_Node'Class) return Enum_Class_Case_List;
      --% no-document: True
      function As_Enum_Class_Decl
        (Node : Lkt_Node'Class) return Enum_Class_Decl;
      --% no-document: True
      function As_Enum_Lit_Decl
        (Node : Lkt_Node'Class) return Enum_Lit_Decl;
      --% no-document: True
      function As_Enum_Lit_Decl_List
        (Node : Lkt_Node'Class) return Enum_Lit_Decl_List;
      --% no-document: True
      function As_Enum_Type_Decl
        (Node : Lkt_Node'Class) return Enum_Type_Decl;
      --% no-document: True
      function As_Env_Spec_Decl
        (Node : Lkt_Node'Class) return Env_Spec_Decl;
      --% no-document: True
      function As_Error_On_Null
        (Node : Lkt_Node'Class) return Error_On_Null;
      --% no-document: True
      function As_Excludes_Null
        (Node : Lkt_Node'Class) return Excludes_Null;
      --% no-document: True
      function As_Excludes_Null_Absent
        (Node : Lkt_Node'Class) return Excludes_Null_Absent;
      --% no-document: True
      function As_Excludes_Null_Present
        (Node : Lkt_Node'Class) return Excludes_Null_Present;
      --% no-document: True
      function As_Node_Pattern
        (Node : Lkt_Node'Class) return Node_Pattern;
      --% no-document: True
      function As_Extended_Node_Pattern
        (Node : Lkt_Node'Class) return Extended_Node_Pattern;
      --% no-document: True
      function As_Field_Decl
        (Node : Lkt_Node'Class) return Field_Decl;
      --% no-document: True
      function As_Filtered_Pattern
        (Node : Lkt_Node'Class) return Filtered_Pattern;
      --% no-document: True
      function As_Full_Decl
        (Node : Lkt_Node'Class) return Full_Decl;
      --% no-document: True
      function As_Fun_Decl
        (Node : Lkt_Node'Class) return Fun_Decl;
      --% no-document: True
      function As_Fun_Param_Decl
        (Node : Lkt_Node'Class) return Fun_Param_Decl;
      --% no-document: True
      function As_Fun_Param_Decl_List
        (Node : Lkt_Node'Class) return Fun_Param_Decl_List;
      --% no-document: True
      function As_Function_Type
        (Node : Lkt_Node'Class) return Function_Type;
      --% no-document: True
      function As_Function_Type_Ref
        (Node : Lkt_Node'Class) return Function_Type_Ref;
      --% no-document: True
      function As_Generic_Decl
        (Node : Lkt_Node'Class) return Generic_Decl;
      --% no-document: True
      function As_Generic_Instantiation
        (Node : Lkt_Node'Class) return Generic_Instantiation;
      --% no-document: True
      function As_Generic_Param_Decl_List
        (Node : Lkt_Node'Class) return Generic_Param_Decl_List;
      --% no-document: True
      function As_Generic_Param_Type_Decl
        (Node : Lkt_Node'Class) return Generic_Param_Type_Decl;
      --% no-document: True
      function As_Generic_Type_Ref
        (Node : Lkt_Node'Class) return Generic_Type_Ref;
      --% no-document: True
      function As_Grammar_Expr
        (Node : Lkt_Node'Class) return Grammar_Expr;
      --% no-document: True
      function As_Grammar_Cut
        (Node : Lkt_Node'Class) return Grammar_Cut;
      --% no-document: True
      function As_Grammar_Decl
        (Node : Lkt_Node'Class) return Grammar_Decl;
      --% no-document: True
      function As_Grammar_Discard
        (Node : Lkt_Node'Class) return Grammar_Discard;
      --% no-document: True
      function As_Grammar_Dont_Skip
        (Node : Lkt_Node'Class) return Grammar_Dont_Skip;
      --% no-document: True
      function As_Grammar_Expr_List
        (Node : Lkt_Node'Class) return Grammar_Expr_List;
      --% no-document: True
      function As_Grammar_Expr_List_List
        (Node : Lkt_Node'Class) return Grammar_Expr_List_List;
      --% no-document: True
      function As_Grammar_Pick
        (Node : Lkt_Node'Class) return Grammar_Pick;
      --% no-document: True
      function As_Grammar_Implicit_Pick
        (Node : Lkt_Node'Class) return Grammar_Implicit_Pick;
      --% no-document: True
      function As_Grammar_List
        (Node : Lkt_Node'Class) return Grammar_List;
      --% no-document: True
      function As_Grammar_List_Sep
        (Node : Lkt_Node'Class) return Grammar_List_Sep;
      --% no-document: True
      function As_Grammar_Null
        (Node : Lkt_Node'Class) return Grammar_Null;
      --% no-document: True
      function As_Grammar_Opt
        (Node : Lkt_Node'Class) return Grammar_Opt;
      --% no-document: True
      function As_Grammar_Opt_Error
        (Node : Lkt_Node'Class) return Grammar_Opt_Error;
      --% no-document: True
      function As_Grammar_Opt_Error_Group
        (Node : Lkt_Node'Class) return Grammar_Opt_Error_Group;
      --% no-document: True
      function As_Grammar_Opt_Group
        (Node : Lkt_Node'Class) return Grammar_Opt_Group;
      --% no-document: True
      function As_Grammar_Or_Expr
        (Node : Lkt_Node'Class) return Grammar_Or_Expr;
      --% no-document: True
      function As_Grammar_Predicate
        (Node : Lkt_Node'Class) return Grammar_Predicate;
      --% no-document: True
      function As_Grammar_Rule_Decl
        (Node : Lkt_Node'Class) return Grammar_Rule_Decl;
      --% no-document: True
      function As_Grammar_Rule_Ref
        (Node : Lkt_Node'Class) return Grammar_Rule_Ref;
      --% no-document: True
      function As_Grammar_Skip
        (Node : Lkt_Node'Class) return Grammar_Skip;
      --% no-document: True
      function As_Grammar_Stop_Cut
        (Node : Lkt_Node'Class) return Grammar_Stop_Cut;
      --% no-document: True
      function As_If_Expr
        (Node : Lkt_Node'Class) return If_Expr;
      --% no-document: True
      function As_Import
        (Node : Lkt_Node'Class) return Import;
      --% no-document: True
      function As_Import_List
        (Node : Lkt_Node'Class) return Import_List;
      --% no-document: True
      function As_Integer_Pattern
        (Node : Lkt_Node'Class) return Integer_Pattern;
      --% no-document: True
      function As_Isa
        (Node : Lkt_Node'Class) return Isa;
      --% no-document: True
      function As_Keep_Expr
        (Node : Lkt_Node'Class) return Keep_Expr;
      --% no-document: True
      function As_Lambda_Expr
        (Node : Lkt_Node'Class) return Lambda_Expr;
      --% no-document: True
      function As_Lambda_Param_Decl
        (Node : Lkt_Node'Class) return Lambda_Param_Decl;
      --% no-document: True
      function As_Lambda_Param_Decl_List
        (Node : Lkt_Node'Class) return Lambda_Param_Decl_List;
      --% no-document: True
      function As_Langkit_Root
        (Node : Lkt_Node'Class) return Langkit_Root;
      --% no-document: True
      function As_Lexer_Case_Rule
        (Node : Lkt_Node'Class) return Lexer_Case_Rule;
      --% no-document: True
      function As_Lexer_Case_Rule_Cond_Alt
        (Node : Lkt_Node'Class) return Lexer_Case_Rule_Cond_Alt;
      --% no-document: True
      function As_Lexer_Case_Rule_Default_Alt
        (Node : Lkt_Node'Class) return Lexer_Case_Rule_Default_Alt;
      --% no-document: True
      function As_Lexer_Case_Rule_Send
        (Node : Lkt_Node'Class) return Lexer_Case_Rule_Send;
      --% no-document: True
      function As_Lexer_Decl
        (Node : Lkt_Node'Class) return Lexer_Decl;
      --% no-document: True
      function As_Lexer_Family_Decl
        (Node : Lkt_Node'Class) return Lexer_Family_Decl;
      --% no-document: True
      function As_List_Kind
        (Node : Lkt_Node'Class) return List_Kind;
      --% no-document: True
      function As_List_Kind_One
        (Node : Lkt_Node'Class) return List_Kind_One;
      --% no-document: True
      function As_List_Kind_Zero
        (Node : Lkt_Node'Class) return List_Kind_Zero;
      --% no-document: True
      function As_List_Pattern
        (Node : Lkt_Node'Class) return List_Pattern;
      --% no-document: True
      function As_Logic_Assign
        (Node : Lkt_Node'Class) return Logic_Assign;
      --% no-document: True
      function As_Logic_Call_Expr
        (Node : Lkt_Node'Class) return Logic_Call_Expr;
      --% no-document: True
      function As_Logic_Expr
        (Node : Lkt_Node'Class) return Logic_Expr;
      --% no-document: True
      function As_Logic_Predicate
        (Node : Lkt_Node'Class) return Logic_Predicate;
      --% no-document: True
      function As_Logic_Propagate
        (Node : Lkt_Node'Class) return Logic_Propagate;
      --% no-document: True
      function As_Logic_Propagate_Call
        (Node : Lkt_Node'Class) return Logic_Propagate_Call;
      --% no-document: True
      function As_Logic_Unify
        (Node : Lkt_Node'Class) return Logic_Unify;
      --% no-document: True
      function As_Match_Branch
        (Node : Lkt_Node'Class) return Match_Branch;
      --% no-document: True
      function As_Match_Branch_List
        (Node : Lkt_Node'Class) return Match_Branch_List;
      --% no-document: True
      function As_Match_Expr
        (Node : Lkt_Node'Class) return Match_Expr;
      --% no-document: True
      function As_Match_Val_Decl
        (Node : Lkt_Node'Class) return Match_Val_Decl;
      --% no-document: True
      function As_Module_Ref_Id
        (Node : Lkt_Node'Class) return Module_Ref_Id;
      --% no-document: True
      function As_Node_Decl
        (Node : Lkt_Node'Class) return Node_Decl;
      --% no-document: True
      function As_Node_Pattern_Detail
        (Node : Lkt_Node'Class) return Node_Pattern_Detail;
      --% no-document: True
      function As_Node_Pattern_Detail_List
        (Node : Lkt_Node'Class) return Node_Pattern_Detail_List;
      --% no-document: True
      function As_Node_Pattern_Field
        (Node : Lkt_Node'Class) return Node_Pattern_Field;
      --% no-document: True
      function As_Node_Pattern_Property
        (Node : Lkt_Node'Class) return Node_Pattern_Property;
      --% no-document: True
      function As_Node_Pattern_Selector
        (Node : Lkt_Node'Class) return Node_Pattern_Selector;
      --% no-document: True
      function As_Not_Expr
        (Node : Lkt_Node'Class) return Not_Expr;
      --% no-document: True
      function As_Not_Pattern
        (Node : Lkt_Node'Class) return Not_Pattern;
      --% no-document: True
      function As_Null_Cond_Qualifier
        (Node : Lkt_Node'Class) return Null_Cond_Qualifier;
      --% no-document: True
      function As_Null_Cond_Qualifier_Absent
        (Node : Lkt_Node'Class) return Null_Cond_Qualifier_Absent;
      --% no-document: True
      function As_Null_Cond_Qualifier_Present
        (Node : Lkt_Node'Class) return Null_Cond_Qualifier_Present;
      --% no-document: True
      function As_Null_Lit
        (Node : Lkt_Node'Class) return Null_Lit;
      --% no-document: True
      function As_Null_Pattern
        (Node : Lkt_Node'Class) return Null_Pattern;
      --% no-document: True
      function As_Num_Lit
        (Node : Lkt_Node'Class) return Num_Lit;
      --% no-document: True
      function As_Op
        (Node : Lkt_Node'Class) return Op;
      --% no-document: True
      function As_Op_Amp
        (Node : Lkt_Node'Class) return Op_Amp;
      --% no-document: True
      function As_Op_And
        (Node : Lkt_Node'Class) return Op_And;
      --% no-document: True
      function As_Op_Div
        (Node : Lkt_Node'Class) return Op_Div;
      --% no-document: True
      function As_Op_Eq
        (Node : Lkt_Node'Class) return Op_Eq;
      --% no-document: True
      function As_Op_Gt
        (Node : Lkt_Node'Class) return Op_Gt;
      --% no-document: True
      function As_Op_Gte
        (Node : Lkt_Node'Class) return Op_Gte;
      --% no-document: True
      function As_Op_Logic_And
        (Node : Lkt_Node'Class) return Op_Logic_And;
      --% no-document: True
      function As_Op_Logic_Or
        (Node : Lkt_Node'Class) return Op_Logic_Or;
      --% no-document: True
      function As_Op_Lt
        (Node : Lkt_Node'Class) return Op_Lt;
      --% no-document: True
      function As_Op_Lte
        (Node : Lkt_Node'Class) return Op_Lte;
      --% no-document: True
      function As_Op_Minus
        (Node : Lkt_Node'Class) return Op_Minus;
      --% no-document: True
      function As_Op_Mult
        (Node : Lkt_Node'Class) return Op_Mult;
      --% no-document: True
      function As_Op_Ne
        (Node : Lkt_Node'Class) return Op_Ne;
      --% no-document: True
      function As_Op_Or
        (Node : Lkt_Node'Class) return Op_Or;
      --% no-document: True
      function As_Op_Or_Int
        (Node : Lkt_Node'Class) return Op_Or_Int;
      --% no-document: True
      function As_Op_Plus
        (Node : Lkt_Node'Class) return Op_Plus;
      --% no-document: True
      function As_Or_Pattern
        (Node : Lkt_Node'Class) return Or_Pattern;
      --% no-document: True
      function As_Paren_Expr
        (Node : Lkt_Node'Class) return Paren_Expr;
      --% no-document: True
      function As_Paren_Pattern
        (Node : Lkt_Node'Class) return Paren_Pattern;
      --% no-document: True
      function As_Parse_Node_Expr
        (Node : Lkt_Node'Class) return Parse_Node_Expr;
      --% no-document: True
      function As_Single_Line_String_Lit
        (Node : Lkt_Node'Class) return Single_Line_String_Lit;
      --% no-document: True
      function As_Pattern_Single_Line_String_Lit
        (Node : Lkt_Node'Class) return Pattern_Single_Line_String_Lit;
      --% no-document: True
      function As_Raise_Expr
        (Node : Lkt_Node'Class) return Raise_Expr;
      --% no-document: True
      function As_Ref_Id
        (Node : Lkt_Node'Class) return Ref_Id;
      --% no-document: True
      function As_Ref_Id_List
        (Node : Lkt_Node'Class) return Ref_Id_List;
      --% no-document: True
      function As_Regex_Pattern
        (Node : Lkt_Node'Class) return Regex_Pattern;
      --% no-document: True
      function As_Selector_Call
        (Node : Lkt_Node'Class) return Selector_Call;
      --% no-document: True
      function As_Self_Decl
        (Node : Lkt_Node'Class) return Self_Decl;
      --% no-document: True
      function As_Simple_Type_Ref
        (Node : Lkt_Node'Class) return Simple_Type_Ref;
      --% no-document: True
      function As_Splat_Pattern
        (Node : Lkt_Node'Class) return Splat_Pattern;
      --% no-document: True
      function As_Struct_Decl
        (Node : Lkt_Node'Class) return Struct_Decl;
      --% no-document: True
      function As_Subscript_Expr
        (Node : Lkt_Node'Class) return Subscript_Expr;
      --% no-document: True
      function As_Synth_Fun_Decl
        (Node : Lkt_Node'Class) return Synth_Fun_Decl;
      --% no-document: True
      function As_Synth_Param_Decl
        (Node : Lkt_Node'Class) return Synth_Param_Decl;
      --% no-document: True
      function As_Synthetic_Lexer_Decl
        (Node : Lkt_Node'Class) return Synthetic_Lexer_Decl;
      --% no-document: True
      function As_Type_Ref_List
        (Node : Lkt_Node'Class) return Type_Ref_List;
      --% no-document: True
      function As_Synthetic_Type_Ref_List
        (Node : Lkt_Node'Class) return Synthetic_Type_Ref_List;
      --% no-document: True
      function As_Token_Lit
        (Node : Lkt_Node'Class) return Token_Lit;
      --% no-document: True
      function As_Token_No_Case_Lit
        (Node : Lkt_Node'Class) return Token_No_Case_Lit;
      --% no-document: True
      function As_Token_Pattern_Concat
        (Node : Lkt_Node'Class) return Token_Pattern_Concat;
      --% no-document: True
      function As_Token_Pattern_Lit
        (Node : Lkt_Node'Class) return Token_Pattern_Lit;
      --% no-document: True
      function As_Token_Ref
        (Node : Lkt_Node'Class) return Token_Ref;
      --% no-document: True
      function As_Trait_Decl
        (Node : Lkt_Node'Class) return Trait_Decl;
      --% no-document: True
      function As_Try_Expr
        (Node : Lkt_Node'Class) return Try_Expr;
      --% no-document: True
      function As_Tuple_Pattern
        (Node : Lkt_Node'Class) return Tuple_Pattern;
      --% no-document: True
      function As_Type_Pattern
        (Node : Lkt_Node'Class) return Type_Pattern;
      --% no-document: True
      function As_Un_Op
        (Node : Lkt_Node'Class) return Un_Op;
      --% no-document: True
      function As_Universal_Pattern
        (Node : Lkt_Node'Class) return Universal_Pattern;
      --% no-document: True
      function As_Val_Decl
        (Node : Lkt_Node'Class) return Val_Decl;
      --% no-document: True
      function As_Var_Bind
        (Node : Lkt_Node'Class) return Var_Bind;
      --% no-document: True

   function Hash
     (Node : Lkt_Node) return Ada.Containers.Hash_Type;
   --  Generic hash function, to be used for nodes as keys in hash tables
   pragma Warnings (On, "defined after private extension");

private

   type Internal_Context_Access is
      access all Implementation.Analysis_Context_Type;
   type Internal_Unit_Access is
      access all Implementation.Analysis_Unit_Type;

   type Analysis_Context is new Ada.Finalization.Controlled with record
      Internal : Internal_Context_Access;
   end record;

   overriding procedure Initialize (Context : in out Analysis_Context);
   overriding procedure Adjust (Context : in out Analysis_Context);
   overriding procedure Finalize (Context : in out Analysis_Context);

   type Analysis_Unit is new Liblktlang_Support.Text.Text_Buffer_Ifc with record
      Internal : Internal_Unit_Access;

      Context : Analysis_Context;
      --  Keep a reference to the owning context so that the context lives as
      --  long as there is at least one reference to one of its units.
   end record;

   No_Analysis_Context : constant Analysis_Context :=
     (Ada.Finalization.Controlled with Internal => null);
   No_Analysis_Unit    : constant Analysis_Unit :=
     (Internal => null,
      Context  => (Ada.Finalization.Controlled with Internal => null));

   --------------------------
   -- AST nodes (internal) --
   --------------------------

         type Lkt_Node is tagged record
            Internal   : Implementation.AST_Envs.Entity;
            Safety_Net : Implementation.Node_Safety_Net;
         end record;
      No_Lkt_Node : constant Lkt_Node :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Expr is new Lkt_Node with null record;
      No_Expr : constant Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Any_Of is new Expr with null record;
      No_Any_Of : constant Any_Of :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lkt_Node_Base_List is new Lkt_Node with null record;
      No_Lkt_Node_Base_List : constant Lkt_Node_Base_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Expr_List is new Lkt_Node_Base_List with null record;
      No_Expr_List : constant Expr_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Any_Of_List is new Expr_List with null record;
      No_Any_Of_List : constant Any_Of_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Decl is new Lkt_Node with null record;
      No_Decl : constant Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Type_Decl is new Decl with null record;
      No_Type_Decl : constant Type_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Any_Type_Decl is new Type_Decl with null record;
      No_Any_Type_Decl : constant Any_Type_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Argument is new Lkt_Node with null record;
      No_Argument : constant Argument :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Argument_List is new Lkt_Node_Base_List with null record;
      No_Argument_List : constant Argument_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Array_Literal is new Expr with null record;
      No_Array_Literal : constant Array_Literal :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Base_Call_Expr is new Expr with null record;
      No_Base_Call_Expr : constant Base_Call_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Base_Grammar_Rule_Decl is new Decl with null record;
      No_Base_Grammar_Rule_Decl : constant Base_Grammar_Rule_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Base_Lexer_Case_Rule_Alt is new Lkt_Node with null record;
      No_Base_Lexer_Case_Rule_Alt : constant Base_Lexer_Case_Rule_Alt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Base_Lexer_Case_Rule_Alt_List is new Lkt_Node_Base_List with null record;
      No_Base_Lexer_Case_Rule_Alt_List : constant Base_Lexer_Case_Rule_Alt_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Base_Pattern is new Lkt_Node with null record;
      No_Base_Pattern : constant Base_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Base_Pattern_List is new Lkt_Node_Base_List with null record;
      No_Base_Pattern_List : constant Base_Pattern_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Base_Val_Decl is new Decl with null record;
      No_Base_Val_Decl : constant Base_Val_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Named_Type_Decl is new Type_Decl with null record;
      No_Named_Type_Decl : constant Named_Type_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Basic_Class_Decl is new Named_Type_Decl with null record;
      No_Basic_Class_Decl : constant Basic_Class_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lit is new Expr with null record;
      No_Lit : constant Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Big_Num_Lit is new Lit with null record;
      No_Big_Num_Lit : constant Big_Num_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Bin_Op is new Expr with null record;
      No_Bin_Op : constant Bin_Op :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Binding_Pattern is new Base_Pattern with null record;
      No_Binding_Pattern : constant Binding_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lkt_Node_List is new Lkt_Node_Base_List with null record;
      No_Lkt_Node_List : constant Lkt_Node_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Block_Decl_List is new Lkt_Node_List with null record;
      No_Block_Decl_List : constant Block_Decl_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Block_Expr is new Expr with null record;
      No_Block_Expr : constant Block_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Block_String_Line is new Lkt_Node with null record;
      No_Block_String_Line : constant Block_String_Line :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Block_String_Line_List is new Lkt_Node_Base_List with null record;
      No_Block_String_Line_List : constant Block_String_Line_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type String_Lit is new Lit with null record;
      No_String_Lit : constant String_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Block_String_Lit is new String_Lit with null record;
      No_Block_String_Lit : constant Block_String_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Value_Pattern is new Base_Pattern with null record;
      No_Value_Pattern : constant Value_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Bool_Pattern is new Value_Pattern with null record;
      No_Bool_Pattern : constant Bool_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Bool_Pattern_False is new Bool_Pattern with null record;
      No_Bool_Pattern_False : constant Bool_Pattern_False :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Bool_Pattern_True is new Bool_Pattern with null record;
      No_Bool_Pattern_True : constant Bool_Pattern_True :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Call_Expr is new Base_Call_Expr with null record;
      No_Call_Expr : constant Call_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Call_Expr_List is new Lkt_Node_Base_List with null record;
      No_Call_Expr_List : constant Call_Expr_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Cast_Expr is new Expr with null record;
      No_Cast_Expr : constant Cast_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Char_Lit is new Lit with null record;
      No_Char_Lit : constant Char_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Class_Decl is new Basic_Class_Decl with null record;
      No_Class_Decl : constant Class_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Class_Qualifier is new Lkt_Node with null record;
      No_Class_Qualifier : constant Class_Qualifier :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Class_Qualifier_Absent is new Class_Qualifier with null record;
      No_Class_Qualifier_Absent : constant Class_Qualifier_Absent :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Class_Qualifier_Present is new Class_Qualifier with null record;
      No_Class_Qualifier_Present : constant Class_Qualifier_Present :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type User_Val_Decl is new Base_Val_Decl with null record;
      No_User_Val_Decl : constant User_Val_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Explicitly_Typed_Decl is new User_Val_Decl with null record;
      No_Explicitly_Typed_Decl : constant Explicitly_Typed_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Component_Decl is new Explicitly_Typed_Decl with null record;
      No_Component_Decl : constant Component_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Decl_Annotation is new Lkt_Node with null record;
      No_Decl_Annotation : constant Decl_Annotation :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Decl_Annotation_Args is new Lkt_Node with null record;
      No_Decl_Annotation_Args : constant Decl_Annotation_Args :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Decl_Annotation_List is new Lkt_Node_Base_List with null record;
      No_Decl_Annotation_List : constant Decl_Annotation_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Full_Decl_List is new Lkt_Node_Base_List with null record;
      No_Full_Decl_List : constant Full_Decl_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Decl_Block is new Full_Decl_List with null record;
      No_Decl_Block : constant Decl_Block :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Id is new Expr with null record;
      No_Id : constant Id :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Def_Id is new Id with null record;
      No_Def_Id : constant Def_Id :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Type_Ref is new Lkt_Node with null record;
      No_Type_Ref : constant Type_Ref :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Default_List_Type_Ref is new Type_Ref with null record;
      No_Default_List_Type_Ref : constant Default_List_Type_Ref :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Dot_Expr is new Expr with null record;
      No_Dot_Expr : constant Dot_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Dyn_Env_Wrapper is new Lkt_Node with null record;
      No_Dyn_Env_Wrapper : constant Dyn_Env_Wrapper :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Dyn_Var_Decl is new Explicitly_Typed_Decl with null record;
      No_Dyn_Var_Decl : constant Dyn_Var_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Elsif_Branch is new Lkt_Node with null record;
      No_Elsif_Branch : constant Elsif_Branch :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Elsif_Branch_List is new Lkt_Node_Base_List with null record;
      No_Elsif_Branch_List : constant Elsif_Branch_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Enum_Class_Alt_Decl is new Type_Decl with null record;
      No_Enum_Class_Alt_Decl : constant Enum_Class_Alt_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Enum_Class_Alt_Decl_List is new Lkt_Node_Base_List with null record;
      No_Enum_Class_Alt_Decl_List : constant Enum_Class_Alt_Decl_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Enum_Class_Case is new Lkt_Node with null record;
      No_Enum_Class_Case : constant Enum_Class_Case :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Enum_Class_Case_List is new Lkt_Node_Base_List with null record;
      No_Enum_Class_Case_List : constant Enum_Class_Case_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Enum_Class_Decl is new Basic_Class_Decl with null record;
      No_Enum_Class_Decl : constant Enum_Class_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Enum_Lit_Decl is new User_Val_Decl with null record;
      No_Enum_Lit_Decl : constant Enum_Lit_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Enum_Lit_Decl_List is new Lkt_Node_Base_List with null record;
      No_Enum_Lit_Decl_List : constant Enum_Lit_Decl_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Enum_Type_Decl is new Named_Type_Decl with null record;
      No_Enum_Type_Decl : constant Enum_Type_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Env_Spec_Decl is new Decl with null record;
      No_Env_Spec_Decl : constant Env_Spec_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Error_On_Null is new Expr with null record;
      No_Error_On_Null : constant Error_On_Null :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Excludes_Null is new Lkt_Node with null record;
      No_Excludes_Null : constant Excludes_Null :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Excludes_Null_Absent is new Excludes_Null with null record;
      No_Excludes_Null_Absent : constant Excludes_Null_Absent :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Excludes_Null_Present is new Excludes_Null with null record;
      No_Excludes_Null_Present : constant Excludes_Null_Present :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Node_Pattern is new Value_Pattern with null record;
      No_Node_Pattern : constant Node_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Extended_Node_Pattern is new Node_Pattern with null record;
      No_Extended_Node_Pattern : constant Extended_Node_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Field_Decl is new Component_Decl with null record;
      No_Field_Decl : constant Field_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Filtered_Pattern is new Base_Pattern with null record;
      No_Filtered_Pattern : constant Filtered_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Full_Decl is new Lkt_Node with null record;
      No_Full_Decl : constant Full_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Fun_Decl is new User_Val_Decl with null record;
      No_Fun_Decl : constant Fun_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Fun_Param_Decl is new Component_Decl with null record;
      No_Fun_Param_Decl : constant Fun_Param_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Fun_Param_Decl_List is new Lkt_Node_Base_List with null record;
      No_Fun_Param_Decl_List : constant Fun_Param_Decl_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Function_Type is new Type_Decl with null record;
      No_Function_Type : constant Function_Type :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Function_Type_Ref is new Type_Ref with null record;
      No_Function_Type_Ref : constant Function_Type_Ref :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Generic_Decl is new Decl with null record;
      No_Generic_Decl : constant Generic_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Generic_Instantiation is new Expr with null record;
      No_Generic_Instantiation : constant Generic_Instantiation :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Generic_Param_Decl_List is new Full_Decl_List with null record;
      No_Generic_Param_Decl_List : constant Generic_Param_Decl_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Generic_Param_Type_Decl is new Type_Decl with null record;
      No_Generic_Param_Type_Decl : constant Generic_Param_Type_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Generic_Type_Ref is new Type_Ref with null record;
      No_Generic_Type_Ref : constant Generic_Type_Ref :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Expr is new Expr with null record;
      No_Grammar_Expr : constant Grammar_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Cut is new Grammar_Expr with null record;
      No_Grammar_Cut : constant Grammar_Cut :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Decl is new Decl with null record;
      No_Grammar_Decl : constant Grammar_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Discard is new Grammar_Expr with null record;
      No_Grammar_Discard : constant Grammar_Discard :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Dont_Skip is new Grammar_Expr with null record;
      No_Grammar_Dont_Skip : constant Grammar_Dont_Skip :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Expr_List is new Lkt_Node_Base_List with null record;
      No_Grammar_Expr_List : constant Grammar_Expr_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Expr_List_List is new Lkt_Node_Base_List with null record;
      No_Grammar_Expr_List_List : constant Grammar_Expr_List_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Pick is new Grammar_Expr with null record;
      No_Grammar_Pick : constant Grammar_Pick :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Implicit_Pick is new Grammar_Pick with null record;
      No_Grammar_Implicit_Pick : constant Grammar_Implicit_Pick :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_List is new Grammar_Expr with null record;
      No_Grammar_List : constant Grammar_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_List_Sep is new Lkt_Node with null record;
      No_Grammar_List_Sep : constant Grammar_List_Sep :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Null is new Grammar_Expr with null record;
      No_Grammar_Null : constant Grammar_Null :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Opt is new Grammar_Expr with null record;
      No_Grammar_Opt : constant Grammar_Opt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Opt_Error is new Grammar_Expr with null record;
      No_Grammar_Opt_Error : constant Grammar_Opt_Error :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Opt_Error_Group is new Grammar_Expr with null record;
      No_Grammar_Opt_Error_Group : constant Grammar_Opt_Error_Group :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Opt_Group is new Grammar_Expr with null record;
      No_Grammar_Opt_Group : constant Grammar_Opt_Group :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Or_Expr is new Grammar_Expr with null record;
      No_Grammar_Or_Expr : constant Grammar_Or_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Predicate is new Grammar_Expr with null record;
      No_Grammar_Predicate : constant Grammar_Predicate :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Rule_Decl is new Base_Grammar_Rule_Decl with null record;
      No_Grammar_Rule_Decl : constant Grammar_Rule_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Rule_Ref is new Grammar_Expr with null record;
      No_Grammar_Rule_Ref : constant Grammar_Rule_Ref :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Skip is new Grammar_Expr with null record;
      No_Grammar_Skip : constant Grammar_Skip :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Grammar_Stop_Cut is new Grammar_Expr with null record;
      No_Grammar_Stop_Cut : constant Grammar_Stop_Cut :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type If_Expr is new Expr with null record;
      No_If_Expr : constant If_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Import is new Lkt_Node with null record;
      No_Import : constant Import :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Import_List is new Lkt_Node_Base_List with null record;
      No_Import_List : constant Import_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Integer_Pattern is new Value_Pattern with null record;
      No_Integer_Pattern : constant Integer_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Isa is new Expr with null record;
      No_Isa : constant Isa :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Keep_Expr is new Expr with null record;
      No_Keep_Expr : constant Keep_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lambda_Expr is new Expr with null record;
      No_Lambda_Expr : constant Lambda_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lambda_Param_Decl is new Component_Decl with null record;
      No_Lambda_Param_Decl : constant Lambda_Param_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lambda_Param_Decl_List is new Lkt_Node_Base_List with null record;
      No_Lambda_Param_Decl_List : constant Lambda_Param_Decl_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Langkit_Root is new Lkt_Node with null record;
      No_Langkit_Root : constant Langkit_Root :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lexer_Case_Rule is new Lkt_Node with null record;
      No_Lexer_Case_Rule : constant Lexer_Case_Rule :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lexer_Case_Rule_Cond_Alt is new Base_Lexer_Case_Rule_Alt with null record;
      No_Lexer_Case_Rule_Cond_Alt : constant Lexer_Case_Rule_Cond_Alt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lexer_Case_Rule_Default_Alt is new Base_Lexer_Case_Rule_Alt with null record;
      No_Lexer_Case_Rule_Default_Alt : constant Lexer_Case_Rule_Default_Alt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lexer_Case_Rule_Send is new Lkt_Node with null record;
      No_Lexer_Case_Rule_Send : constant Lexer_Case_Rule_Send :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lexer_Decl is new Decl with null record;
      No_Lexer_Decl : constant Lexer_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Lexer_Family_Decl is new Decl with null record;
      No_Lexer_Family_Decl : constant Lexer_Family_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type List_Kind is new Lkt_Node with null record;
      No_List_Kind : constant List_Kind :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type List_Kind_One is new List_Kind with null record;
      No_List_Kind_One : constant List_Kind_One :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type List_Kind_Zero is new List_Kind with null record;
      No_List_Kind_Zero : constant List_Kind_Zero :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type List_Pattern is new Value_Pattern with null record;
      No_List_Pattern : constant List_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Logic_Assign is new Expr with null record;
      No_Logic_Assign : constant Logic_Assign :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Logic_Call_Expr is new Base_Call_Expr with null record;
      No_Logic_Call_Expr : constant Logic_Call_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Logic_Expr is new Expr with null record;
      No_Logic_Expr : constant Logic_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Logic_Predicate is new Logic_Call_Expr with null record;
      No_Logic_Predicate : constant Logic_Predicate :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Logic_Propagate is new Expr with null record;
      No_Logic_Propagate : constant Logic_Propagate :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Logic_Propagate_Call is new Logic_Call_Expr with null record;
      No_Logic_Propagate_Call : constant Logic_Propagate_Call :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Logic_Unify is new Expr with null record;
      No_Logic_Unify : constant Logic_Unify :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Match_Branch is new Lkt_Node with null record;
      No_Match_Branch : constant Match_Branch :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Match_Branch_List is new Lkt_Node_Base_List with null record;
      No_Match_Branch_List : constant Match_Branch_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Match_Expr is new Expr with null record;
      No_Match_Expr : constant Match_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Match_Val_Decl is new Explicitly_Typed_Decl with null record;
      No_Match_Val_Decl : constant Match_Val_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Module_Ref_Id is new Id with null record;
      No_Module_Ref_Id : constant Module_Ref_Id :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Node_Decl is new Base_Val_Decl with null record;
      No_Node_Decl : constant Node_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Node_Pattern_Detail is new Lkt_Node with null record;
      No_Node_Pattern_Detail : constant Node_Pattern_Detail :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Node_Pattern_Detail_List is new Lkt_Node_Base_List with null record;
      No_Node_Pattern_Detail_List : constant Node_Pattern_Detail_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Node_Pattern_Field is new Node_Pattern_Detail with null record;
      No_Node_Pattern_Field : constant Node_Pattern_Field :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Node_Pattern_Property is new Node_Pattern_Detail with null record;
      No_Node_Pattern_Property : constant Node_Pattern_Property :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Node_Pattern_Selector is new Node_Pattern_Detail with null record;
      No_Node_Pattern_Selector : constant Node_Pattern_Selector :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Not_Expr is new Expr with null record;
      No_Not_Expr : constant Not_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Not_Pattern is new Value_Pattern with null record;
      No_Not_Pattern : constant Not_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Null_Cond_Qualifier is new Lkt_Node with null record;
      No_Null_Cond_Qualifier : constant Null_Cond_Qualifier :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Null_Cond_Qualifier_Absent is new Null_Cond_Qualifier with null record;
      No_Null_Cond_Qualifier_Absent : constant Null_Cond_Qualifier_Absent :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Null_Cond_Qualifier_Present is new Null_Cond_Qualifier with null record;
      No_Null_Cond_Qualifier_Present : constant Null_Cond_Qualifier_Present :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Null_Lit is new Lit with null record;
      No_Null_Lit : constant Null_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Null_Pattern is new Value_Pattern with null record;
      No_Null_Pattern : constant Null_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Num_Lit is new Lit with null record;
      No_Num_Lit : constant Num_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op is new Lkt_Node with null record;
      No_Op : constant Op :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Amp is new Op with null record;
      No_Op_Amp : constant Op_Amp :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_And is new Op with null record;
      No_Op_And : constant Op_And :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Div is new Op with null record;
      No_Op_Div : constant Op_Div :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Eq is new Op with null record;
      No_Op_Eq : constant Op_Eq :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Gt is new Op with null record;
      No_Op_Gt : constant Op_Gt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Gte is new Op with null record;
      No_Op_Gte : constant Op_Gte :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Logic_And is new Op with null record;
      No_Op_Logic_And : constant Op_Logic_And :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Logic_Or is new Op with null record;
      No_Op_Logic_Or : constant Op_Logic_Or :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Lt is new Op with null record;
      No_Op_Lt : constant Op_Lt :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Lte is new Op with null record;
      No_Op_Lte : constant Op_Lte :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Minus is new Op with null record;
      No_Op_Minus : constant Op_Minus :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Mult is new Op with null record;
      No_Op_Mult : constant Op_Mult :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Ne is new Op with null record;
      No_Op_Ne : constant Op_Ne :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Or is new Op with null record;
      No_Op_Or : constant Op_Or :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Or_Int is new Op with null record;
      No_Op_Or_Int : constant Op_Or_Int :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Op_Plus is new Op with null record;
      No_Op_Plus : constant Op_Plus :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Or_Pattern is new Value_Pattern with null record;
      No_Or_Pattern : constant Or_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Paren_Expr is new Expr with null record;
      No_Paren_Expr : constant Paren_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Paren_Pattern is new Value_Pattern with null record;
      No_Paren_Pattern : constant Paren_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Parse_Node_Expr is new Grammar_Expr with null record;
      No_Parse_Node_Expr : constant Parse_Node_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Single_Line_String_Lit is new String_Lit with null record;
      No_Single_Line_String_Lit : constant Single_Line_String_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Pattern_Single_Line_String_Lit is new Single_Line_String_Lit with null record;
      No_Pattern_Single_Line_String_Lit : constant Pattern_Single_Line_String_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Raise_Expr is new Expr with null record;
      No_Raise_Expr : constant Raise_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ref_Id is new Id with null record;
      No_Ref_Id : constant Ref_Id :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Ref_Id_List is new Lkt_Node_Base_List with null record;
      No_Ref_Id_List : constant Ref_Id_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Regex_Pattern is new Value_Pattern with null record;
      No_Regex_Pattern : constant Regex_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Selector_Call is new Lkt_Node with null record;
      No_Selector_Call : constant Selector_Call :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Self_Decl is new Base_Val_Decl with null record;
      No_Self_Decl : constant Self_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Simple_Type_Ref is new Type_Ref with null record;
      No_Simple_Type_Ref : constant Simple_Type_Ref :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Splat_Pattern is new Value_Pattern with null record;
      No_Splat_Pattern : constant Splat_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Struct_Decl is new Named_Type_Decl with null record;
      No_Struct_Decl : constant Struct_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Subscript_Expr is new Expr with null record;
      No_Subscript_Expr : constant Subscript_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Synth_Fun_Decl is new Decl with null record;
      No_Synth_Fun_Decl : constant Synth_Fun_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Synth_Param_Decl is new Decl with null record;
      No_Synth_Param_Decl : constant Synth_Param_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Synthetic_Lexer_Decl is new Base_Grammar_Rule_Decl with null record;
      No_Synthetic_Lexer_Decl : constant Synthetic_Lexer_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Type_Ref_List is new Lkt_Node_Base_List with null record;
      No_Type_Ref_List : constant Type_Ref_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Synthetic_Type_Ref_List is new Type_Ref_List with null record;
      No_Synthetic_Type_Ref_List : constant Synthetic_Type_Ref_List :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Token_Lit is new Grammar_Expr with null record;
      No_Token_Lit : constant Token_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Token_No_Case_Lit is new Grammar_Expr with null record;
      No_Token_No_Case_Lit : constant Token_No_Case_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Token_Pattern_Concat is new Grammar_Expr with null record;
      No_Token_Pattern_Concat : constant Token_Pattern_Concat :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Token_Pattern_Lit is new Grammar_Expr with null record;
      No_Token_Pattern_Lit : constant Token_Pattern_Lit :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Token_Ref is new Grammar_Expr with null record;
      No_Token_Ref : constant Token_Ref :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Trait_Decl is new Named_Type_Decl with null record;
      No_Trait_Decl : constant Trait_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Try_Expr is new Expr with null record;
      No_Try_Expr : constant Try_Expr :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Tuple_Pattern is new Value_Pattern with null record;
      No_Tuple_Pattern : constant Tuple_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Type_Pattern is new Node_Pattern with null record;
      No_Type_Pattern : constant Type_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Un_Op is new Expr with null record;
      No_Un_Op : constant Un_Op :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Universal_Pattern is new Value_Pattern with null record;
      No_Universal_Pattern : constant Universal_Pattern :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Val_Decl is new Explicitly_Typed_Decl with null record;
      No_Val_Decl : constant Val_Decl :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);
         type Var_Bind is new Lkt_Node with null record;
      No_Var_Bind : constant Var_Bind :=
        (Internal   => Implementation.No_Entity,
         Safety_Net => Implementation.No_Node_Safety_Net);

   package Child_Record_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Child_Record);

   type Children_Array is record
      Children : Child_Record_Vectors.Vector;
   end record;

   procedure Check_Safety_Net (Self : Lkt_Node'Class);
   --  Check that Self's node and rebindings are still valid, raising a
   --  Stale_Reference_Error if one is not.

   --------------------------------
   -- Token Iterator (internals) --
   --------------------------------

   type Token_Iterator is record
      Node : Lkt_Node;
      Last : Token_Index;
   end record;

   ---------------------------------
   -- Composite types (internals) --
   ---------------------------------

            
   type Internal_Decoded_Char_Value_Record is limited record
      Internal_Value : Character_Type;
      Internal_Has_Error : Boolean;
      Internal_Error_Sloc : Source_Location;
      Internal_Error_Message : Text_Access;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Decoded_Char_Value_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Decoded_Char_Value_Record; Count : Positive);
   procedure Release (Self : in out Internal_Decoded_Char_Value_Record)
   ;

   package Boxed_Decoded_Char_Value is new Liblktlang_Support.Boxes
     (Internal_Decoded_Char_Value_Record, Refcount, Set_Refcount, Release);

   type Decoded_Char_Value is new Boxed_Decoded_Char_Value.Reference;


            
   type Internal_Decoded_String_Value_Record is limited record
      Internal_Value : Text_Access;
      Internal_Has_Error : Boolean;
      Internal_Error_Sloc : Source_Location;
      Internal_Error_Message : Text_Access;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Decoded_String_Value_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Decoded_String_Value_Record; Count : Positive);
   procedure Release (Self : in out Internal_Decoded_String_Value_Record)
   ;

   package Boxed_Decoded_String_Value is new Liblktlang_Support.Boxes
     (Internal_Decoded_String_Value_Record, Refcount, Set_Refcount, Release);

   type Decoded_String_Value is new Boxed_Decoded_String_Value.Reference;


            
      type Lkt_Node_Array_Access is access all Lkt_Node_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Lkt_Node_Array, Lkt_Node_Array_Access);

            
   type Internal_Logic_Context_Record is limited record
      Internal_Ref_Node : Lkt_Node;
      Internal_Decl_Node : Lkt_Node;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Logic_Context_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Logic_Context_Record; Count : Positive);
   procedure Release (Self : in out Internal_Logic_Context_Record)
         is null
   ;

   package Boxed_Logic_Context is new Liblktlang_Support.Boxes
     (Internal_Logic_Context_Record, Refcount, Set_Refcount, Release);

   type Logic_Context is new Boxed_Logic_Context.Reference;


            
      type Logic_Context_Array_Access is access all Logic_Context_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Logic_Context_Array, Logic_Context_Array_Access);

            
   type Internal_Solver_Diagnostic_Record is limited record
      Internal_Message_Template : Text_Access;
      Internal_Args : Lkt_Node_Array_Access;
      Internal_Location : Lkt_Node;
      Internal_Contexts : Logic_Context_Array_Access;
      Internal_Round : Integer;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Solver_Diagnostic_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Solver_Diagnostic_Record; Count : Positive);
   procedure Release (Self : in out Internal_Solver_Diagnostic_Record)
   ;

   package Boxed_Solver_Diagnostic is new Liblktlang_Support.Boxes
     (Internal_Solver_Diagnostic_Record, Refcount, Set_Refcount, Release);

   type Solver_Diagnostic is new Boxed_Solver_Diagnostic.Reference;


            
      type Solver_Diagnostic_Array_Access is access all Solver_Diagnostic_Array;
      procedure Free is new Ada.Unchecked_Deallocation
        (Solver_Diagnostic_Array, Solver_Diagnostic_Array_Access);

            
   type Internal_Solver_Result_Record is limited record
      Internal_Success : Boolean;
      Internal_Diagnostics : Solver_Diagnostic_Array_Access;

      Refcount : Positive;
   end record;

   function Refcount (Self : Internal_Solver_Result_Record) return Positive;
   procedure Set_Refcount
     (Self : in out Internal_Solver_Result_Record; Count : Positive);
   procedure Release (Self : in out Internal_Solver_Result_Record)
   ;

   package Boxed_Solver_Result is new Liblktlang_Support.Boxes
     (Internal_Solver_Result_Record, Refcount, Set_Refcount, Release);

   type Solver_Result is new Boxed_Solver_Result.Reference;



   --  The dummy references to these packages forces them to be included in
   --  statically linked builds (thanks to the binder). This benefits the GDB
   --  helpers at no cost.

   Version : String renames Liblktlang.Version;
   procedure RN (Node : Liblktlang.Implementation.Bare_Lkt_Node)
      renames Liblktlang.Debug.PN;

end Liblktlang.Analysis;
