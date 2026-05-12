





--  To facilitate use from a -gnatX project, since we don't use the [] syntax
pragma Warnings (Off, "obsolescent");

pragma Warnings (Off, "referenced");
pragma Warnings (Off, "use clause for package * has no effect");

       with Liblktlang_Support.Adalog.Debug;
         use Liblktlang_Support.Adalog.Debug;
       with Liblktlang_Support.Lexical_Envs;
         use Liblktlang_Support.Lexical_Envs;
       with Liblktlang_Support.Slocs;
         use Liblktlang_Support.Slocs;
       with Liblktlang_Support.Symbols;
         use Liblktlang_Support.Symbols;
       with Liblktlang_Support.Text;
         use Liblktlang_Support.Text;
       with Liblktlang_Support.Token_Data_Handlers;
       with Liblktlang_Support.Types;
         use Liblktlang_Support.Types;
       with Liblktlang.Common;
         use Liblktlang.Common;
       with Liblktlang.Implementation;
         use Liblktlang.Implementation;
       with System;
         use System;

pragma Warnings (On, "referenced");
pragma Warnings (On, "use clause for package * has no effect");

private package Liblktlang.Impl_0 is

   pragma Extensions_Allowed (On);

      




function Lkt_Node_P_Prelude_Env
   
  (Node : Bare_Lkt_Node
  )

   return Lexical_Env

   
   ;
--  Return the lexical env that contains all definitions from the prelude.


         
function Lkt_Node_P_Prelude_Env
  (E : Internal_Entity
  ) return Lexical_Env;



      




function Lkt_Node_P_Is_From_Prelude
   
  (Node : Bare_Lkt_Node
  )

   return Boolean

   
   ;
--  Return whether this node belongs to the Lkt prelude unit.




      




function Lkt_Node_P_Root_Env
   
  (Node : Bare_Lkt_Node
  )

   return Lexical_Env

   
   ;





      




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
--  Unit method. Return the ``Char`` builtin type.




      




function Lkt_Node_P_Int_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``Int`` builtin type.




      




function Lkt_Node_P_Bool_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``Bool`` builtin type.




      




function Lkt_Node_P_Bigint_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``BigInt`` builtin type.




      




function Lkt_Node_P_String_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``String`` builtin type.




      




function Lkt_Node_P_Symbol_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``Symbol`` builtin type.




      




function Lkt_Node_P_Property_Error_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``PropertyError`` builtin type.




      




function Lkt_Node_P_Regexp_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``Regexp`` builtin type.




      




function Lkt_Node_P_Entity_Gen_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl

   
   ;
--  Unit method. Return the ``Entity`` generic builtin type.




      




function Lkt_Node_P_Entity_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``Entity`` builtin type.




      




function Lkt_Node_P_Logicvar_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``LogicVar`` builtin type.




      




function Lkt_Node_P_Equation_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``Equation`` builtin type.




      




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




      




function Lkt_Node_P_Stream_Gen_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl

   
   ;
--  Unit method. Return the stream builtin generic type.




      




function Lkt_Node_P_Stream_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the stream builtin type.




      




function Lkt_Node_P_Astlist_Gen_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl

   
   ;
--  Unit method. Return the ``ASTList`` builtin generic type.




      




function Lkt_Node_P_Astlist_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``ASTList`` builtin type.




      




function Lkt_Node_P_Node_Builder_Gen_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl

   
   ;
--  Unit method. Return the ``NodeBuilder`` builtin generic type.




      




function Lkt_Node_P_Node_Builder_Type
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Named_Type_Decl

   
   ;
--  Unit method. Return the ``NodeBuilder`` builtin type.




      




function Lkt_Node_P_Iterator_Gen_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Generic_Decl

   
   ;
--  Unit method. Return the ``Iterator`` builtin generic trait.




      




function Lkt_Node_P_Iterator_Trait
   
  (Node : Bare_Lkt_Node
  )

   return Internal_Entity_Trait_Decl

   
   ;
--  Unit method. Return the ``Iterator`` builtin trait.




      




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




      




function Lkt_Node_P_Get_Empty_Type_Ref_List
   
  (Node : Bare_Lkt_Node
  )

   return Bare_Synthetic_Type_Ref_List

   
   ;
--  Return an empty synthetic ``TypeRef`` list node. Used to generate synthetic
--  type declarations.




      




function Lkt_Node_P_Any_Type
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Create an ``AnyTypeDecl``.




      




function Lkt_Node_P_Self_Root
   
  (Node : Bare_Lkt_Node
  )

   return Bare_Langkit_Root

   
   ;
--  Return the ``LangkitRoot`` node that is the root of the current unit.




      




function Lkt_Node_P_Referenced_Module
   
  (Node : Bare_Lkt_Node
      ; Module_Name : Bare_Id
  )

   return Internal_Entity_Langkit_Root

   
   ;
--  Static method. Convenience wrapper around
--  ``internal_fetch_referenced_unit`` to get the ``LangkitRoot`` as an entity
--  for a given module. Used to write lexical env resolvers.




      




function Lkt_Node_P_Topmost_Invalid_Decl
   
  (Node : Bare_Lkt_Node
  )

   return Bare_Lkt_Node

   
   ;
--  Return the topmost (from ``Self`` to the root node) ``FullDecl`` annotated
--  with ``@invalid``, null otherwise.




      




function Lkt_Node_P_Nameres_Diagnostics
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Solver_Diagnostic_Array_Access

   
   ;
--  If name resolution on this Lkt compilation unit fails, this returns all the
--  diagnostics that were produced while resolving it.




      




function Lkt_Node_P_Solve_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Solver_Result

   
   ;
--  Solve the equation created by this node. This should be used on xref entry
--  points only.




      




function Lkt_Node_P_Solve_Enclosing_Context
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Solver_Result

   
   ;
--  Find the nearest parent that is an xref entry point and solve its equation.




      




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





      




function Lkt_Node_P_Function_Type_Helper
   
  (Node : Bare_Lkt_Node
      ; Param_Types : Internal_Entity_Type_Decl_Array_Access
      ; Return_Type : Internal_Entity_Type_Decl
      ; Origin : Internal_Entity_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type

   
   ;
--  Helper function to create a memoized ``FunctionType``.




      




function Lkt_Node_P_Shed_Rebindings
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity

   
   ;
--  Return this same entity but with its rebindings shed according to its
--  children lexical environment.


         
function Lkt_Node_P_Shed_Rebindings
  (E : Internal_Entity
  ) return Internal_Entity;



      




function Dispatcher_Lkt_Node_P_Xref_Entry_Point
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
      with
        Inline_Always
   ;
--  Return whether this node is an entry point for the xref solving
--  infrastructure. If this returns true, then ``nameres_diagnostics`` can be
--  called on it.




      




function Dispatcher_Lkt_Node_P_Xref_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
      with
        Inline_Always
   ;
--  Base property for constructing equations that will resolve names and types
--  when resolved for every sub expression.




      




function Dispatcher_Lkt_Node_P_Expected_Type_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
      with
        Inline_Always
   ;
--  Return an equation to resolve expected types for children nodes.




      




function Dispatcher_Lkt_Node_P_Generic_Type_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
      with
        Inline_Always
   ;
--  Return an equation to resolve generic types for children nodes.




      




function Dispatcher_Lkt_Node_P_Complete
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Complete_Item_Array_Access

   
      with
        Inline_Always
   ;
--  Return an array of completion item for language server clients.




      




function Lkt_Node_P_Can_Reach
   
  (Node : Bare_Lkt_Node
      ; From_Node : Bare_Lkt_Node
  )

   return Boolean

   
      with
        Export,
        External_Name => "Liblktlang__can_reach"
   ;





      




function Lkt_Node_P_Xref_Entry_Point
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Return whether this node is an entry point for the xref solving
--  infrastructure. If this returns true, then ``nameres_diagnostics`` can be
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
--  Return an equation to resolve expected types for children nodes.




      




function Lkt_Node_P_Generic_Type_Equation
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;
--  Return an equation to resolve generic types for children nodes.




      




function Lkt_Node_P_Complete
   
  (Node : Bare_Lkt_Node
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Complete_Item_Array_Access

   
   ;
--  Return an array of completion item for language server clients.




      




function Argument_P_Expected_Type_Equation
   
  (Node : Bare_Argument
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Argument_P_Xref_Equation
   
  (Node : Bare_Argument
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Base_Import_P_Referenced_Units
   
  (Node : Bare_Base_Import
  )

   return Internal_Unit_Array_Access

   
   ;
--  Return the list of units that contain the modules that this clause imports.
--  Load them if needed.




      




function Dispatcher_Base_Import_P_Referenced_Modules
   
  (Node : Bare_Base_Import
  )

   return Bare_Id_Array_Access

   
      with
        Inline_Always
   ;
--  Return identifiers for the list of modules that this clause imports.




      




function Import_P_Referenced_Modules
   
  (Node : Bare_Import
  )

   return Bare_Id_Array_Access

   
   ;





      




function Internal_Env_Do_16
   
  (Node : Bare_Import
  )

   return Internal_Unit_Array_Access

   
   ;





      




function Internal_Env_Mappings_17
   
  (Node : Bare_Import
  )

   return Internal_Env_Assoc_Array_Access

   
   ;





      




function Import_All_From_P_Referenced_Modules
   
  (Node : Bare_Import_All_From
  )

   return Bare_Id_Array_Access

   
   ;





      




function Import_All_From_P_Referenced_Scope
   
  (Node : Bare_Import_All_From
  )

   return Lexical_Env

   
   ;
--  Return the module scope for the module that this clause designates.


         
function Import_All_From_P_Referenced_Scope
  (E : Internal_Entity
  ) return Lexical_Env;



      




function Internal_Env_Do_20
   
  (Node : Bare_Import_All_From
  )

   return Internal_Unit_Array_Access

   
   ;





      




function Internal_Ref_Env_Nodes_21
   
  (Node : Bare_Import_All_From
  )

   return Bare_Lkt_Node_Array_Access

   
   ;





      




function Import_From_P_Referenced_Modules
   
  (Node : Bare_Import_From
  )

   return Bare_Id_Array_Access

   
   ;





      




function Internal_Env_Do_18
   
  (Node : Bare_Import_From
  )

   return Internal_Unit_Array_Access

   
   ;





      




function Internal_Env_Mappings_19
   
  (Node : Bare_Import_From
  )

   return Internal_Env_Assoc_Array_Access

   
   ;





      




function Dispatcher_Base_Match_Branch_P_Match_Part
   
  (Node : Bare_Base_Match_Branch
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity

   
      with
        Inline_Always
   ;
--  Return the "match" part of the branch, either a pattern branch or a legacy
--  match branch with variable declaration.




      




function Match_Branch_P_Match_Part
   
  (Node : Bare_Match_Branch
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity

   
   ;





      




function Pattern_Match_Branch_P_Match_Part
   
  (Node : Bare_Pattern_Match_Branch
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity

   
   ;





      




function Dispatcher_Class_Qualifier_P_As_Bool
   
  (Node : Bare_Class_Qualifier
  )

   return Boolean

   
      with
        Inline_Always
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





      




function Dispatcher_Decl_P_Decl_Type_Name
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
      with
        Inline_Always
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





      




function Decl_P_Def_Ids
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Def_Id_Array_Access

   
   ;
--  Return all the defining names that this declaration defines.




      




function Decl_P_Implements_Node
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Return if this declaration implements the ``Node`` trait.
--
--  TODO: rework this.




      




function Decl_P_As_Bare_Decl
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;
--  Return this declaration without rebindings information.




      




function Decl_P_Is_Type_Decl
   
  (Node : Bare_Decl
  )

   return Boolean

   
   ;




         
   type Decl_P_Is_Type_Decl_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Decl_P_Is_Type_Decl_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Decl_P_Is_Type_Decl_0_Predicate
;

   
   overriding function Call
     (Self : Decl_P_Is_Type_Decl_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Decl_P_Is_Type_Decl_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Decl_P_Is_Type_Decl_0_Predicate) return String;


      




function Decl_P_Is_Defined
   
  (Node : Bare_Decl
  )

   return Boolean

   
   ;




         
   type Decl_P_Is_Defined_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Decl_P_Is_Defined_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Decl_P_Is_Defined_0_Predicate
;

   
   overriding function Call
     (Self : Decl_P_Is_Defined_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Decl_P_Is_Defined_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Decl_P_Is_Defined_0_Predicate) return String;


      




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
--  generic declaration.
--
--  This iterates through the generic parameters of the decl to find all types
--  that try to replace it and find their common_ancestor.
--
--  ``In_Logic_Call``: Whether we are currently solving a ``LogicPropagate`` or
--  a ``LogicPredicate``.




      




function Decl_P_Function_Type
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type

   
   ;
--  Return a ``FunctionType`` node that can be used to type calls using the
--  current declaration as the call target (the function type for a function
--  declaration, the constructor type for a struct or a class declaration,
--  ...).




         
   type Decl_P_Function_Type_0_Functor is new Solver_Ifc.Converter_Type with
      null record
   with First_Controlling_Parameter;

   
   overriding function Convert (
      Self : Decl_P_Function_Type_0_Functor;
         From : Internal_Entity
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Decl_P_Function_Type_0_Functor) return String;


   
   
   function Create_Decl_P_Function_Type_0_Functor
     return Decl_P_Function_Type_0_Functor
;

      




function Decl_P_Logic_Function_Type
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type

   
   ;
--  ``FunDecl.function_type_aux`` wrapper to deal with function references in
--  the context of propagate/predicate equations: replace the first parameter
--  to be an array of logic variables if used as a dynamic combiner, prepend a
--  parameter corresponding to ``self`` otherwise.




      




function Decl_P_Get_Type
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Return the relevant type when this declaration is used to create a value
--  (the return type for a function, the type itself for structs/classes, ...).
--
--  This returns null when the type cannot be inferred.




      




function Decl_P_Get_Cast_Type
   
  (Node : Bare_Decl
      ; Cast_To : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Return the type of ``E.as[T]`` when ``cast_to`` materializes the cast
--  destination type (``T``), and ``self`` is the type of ``E``.
--
--  The result is ``T`` except when ``T`` is a bare node type whereas ``self``
--  is an entity type: in this case, the result is ``Entity[T]``.




      




function Decl_P_Get_Keep_Type
   
  (Node : Bare_Decl
      ; Keep_Type : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Return the type of ``E.keep[T]`` when ``keep_type`` materializes the input
--  type (``T``), and ``self`` is the type of ``E``.




         
   type Decl_P_Get_Keep_Type_0_Functor is new Solver_Ifc.Combiner_Type with
      null record
   with First_Controlling_Parameter;

   
   overriding function Combine (
      Self : Decl_P_Get_Keep_Type_0_Functor;
         Vals : Entity_Vars.Value_Array
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Decl_P_Get_Keep_Type_0_Functor) return String;


   
   
   function Create_Decl_P_Get_Keep_Type_0_Functor
   (
         N : Positive
   )
     return Decl_P_Get_Keep_Type_0_Functor
;

      




function Decl_P_Get_Suffix_Type
   
  (Node : Bare_Decl
      ; Prefix_Type : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Retun the type of ``E.F`` when ``self`` is the declaration of the ``F``
--  member and ``prefix_type`` is the type of ``E``.




      




function Decl_P_Type_Var_Suffix_Ref
   
  (Node : Bare_Decl
      ; Current_Name : Internal_Entity_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;
--  Return the declaration that ``E.M`` references when ``self`` is the type of
--  ``E`` and ``current_name`` is the member reference ``M``.




      




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




      




function Decl_P_Get_Params
   
  (Node : Bare_Decl
      ; Is_Logic : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Resolved_Param_Array_Access

   
   ;
--  Return an array of ResolvedParam corresponding to the called function's
--  parameters.




      




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
--  Returns whether the Decl is generic.



         
   type Decl_P_Is_Generic_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Decl_P_Is_Generic_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Decl_P_Is_Generic_0_Predicate
;

   
   overriding function Call
     (Self : Decl_P_Is_Generic_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Decl_P_Is_Generic_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Decl_P_Is_Generic_0_Predicate) return String;


      




function Decl_P_Return_Type_Is_Instantiated
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Return True if the return type of this function is instantiated.



         
   type Decl_P_Return_Type_Is_Instantiated_0_Predicate is new Solver_Ifc.Predicate_Type with record
         null;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Decl_P_Return_Type_Is_Instantiated_0_Predicate
      return Decl_P_Return_Type_Is_Instantiated_0_Predicate
;

   
   overriding function Call
     (Self : Decl_P_Return_Type_Is_Instantiated_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;


   overriding function Image (Self : Decl_P_Return_Type_Is_Instantiated_0_Predicate) return String;


      




function Decl_P_Is_Instantiated
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Return True if Self is an instantiated declaration, meaning that it does
--  not use any of its declared generic types.




      




function Decl_P_Has_Correct_Type_Arg_Number
   
  (Node : Bare_Decl
      ; Nb_Types : Integer
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Check that the parent GenericDecl has nb_types parameter types



         
   type Decl_P_Has_Correct_Type_Arg_Number_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Field_0 : Integer;
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Decl_P_Has_Correct_Type_Arg_Number_0_Predicate
   (
         Nb_Types : Integer;
         Error_Location : Bare_Lkt_Node
   )
      return Decl_P_Has_Correct_Type_Arg_Number_0_Predicate
;

   
   overriding function Call
     (Self : Decl_P_Has_Correct_Type_Arg_Number_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Decl_P_Has_Correct_Type_Arg_Number_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Decl_P_Has_Correct_Type_Arg_Number_0_Predicate) return String;


      




function Decl_P_Could_Infer
   
  (Node : Bare_Decl
      ; Generic_Type : Internal_Entity_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Predicate used to verify if we were able to find the type of the callee.



         
   type Decl_P_Could_Infer_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Field_0 : Internal_Entity_Function_Type;
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Decl_P_Could_Infer_0_Predicate
   (
         Generic_Type : Internal_Entity_Function_Type;
         Error_Location : Bare_Lkt_Node
   )
      return Decl_P_Could_Infer_0_Predicate
;

   
   overriding function Call
     (Self : Decl_P_Could_Infer_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Decl_P_Could_Infer_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Decl_P_Could_Infer_0_Predicate) return String;


      




function Decl_P_Instantiate_Generic_Decl
   
  (Node : Bare_Decl
      ; Param_Types : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;
--  Create a DynEnvWrapper to instantiate a DynamicEnvironment to use as
--  rebindings when creating a new Entity from the current type.




         
   type Decl_P_Instantiate_Generic_Decl_0_Functor is new Solver_Ifc.Combiner_Type with
      null record
   with First_Controlling_Parameter;

   
   overriding function Combine (
      Self : Decl_P_Instantiate_Generic_Decl_0_Functor;
         Vals : Entity_Vars.Value_Array
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Decl_P_Instantiate_Generic_Decl_0_Functor) return String;


   
   
   function Create_Decl_P_Instantiate_Generic_Decl_0_Functor
   (
         N : Positive
   )
     return Decl_P_Instantiate_Generic_Decl_0_Functor
;

      




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




         
   type Decl_P_Is_Dynvar_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Decl_P_Is_Dynvar_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Decl_P_Is_Dynvar_0_Predicate
;

   
   overriding function Call
     (Self : Decl_P_Is_Dynvar_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Decl_P_Is_Dynvar_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Decl_P_Is_Dynvar_0_Predicate) return String;


      




function Dispatcher_Decl_P_Name
   
  (Node : Bare_Decl
  )

   return Symbol_Type

   
      with
        Inline_Always
   ;
--  Return the symbol corresponding to the name of this declaration.




      




function Dispatcher_Decl_P_Full_Name_Internal
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
      with
        Inline_Always
   ;
--  Return the full name of this decl, as it should be seen by users/shown in
--  diagnostics. This property is not public: using it avoids calling PLE when
--  used with ``custom_image``




      




function Dispatcher_Decl_P_Full_Name
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
      with
        Inline_Always
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

   
      with
        Inline_Always
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

   
      with
        Inline_Always
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




      




function Decl_P_Is_Directly_Referenceable
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;




         
   type Decl_P_Is_Directly_Referenceable_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Decl_P_Is_Directly_Referenceable_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Decl_P_Is_Directly_Referenceable_0_Predicate
;

   
   overriding function Call
     (Self : Decl_P_Is_Directly_Referenceable_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Decl_P_Is_Directly_Referenceable_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Decl_P_Is_Directly_Referenceable_0_Predicate) return String;


      




function Decl_P_Extraneous_Parameter
   
  (Node : Bare_Decl
      ; Callee_Type : Internal_Entity_Type_Decl
      ; Callee : Internal_Entity_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Predicate used to emit an error when when a CallExpr has a named argument
--  undefined in the callee.
--
--  If ``callee`` is null, return true: an other diagnostic should have been
--  emitted to raise that issue.



         
   type Decl_P_Extraneous_Parameter_0_Predicate is new Solver_Ifc.N_Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Decl_P_Extraneous_Parameter_0_Predicate
   (
         N : Positive;
         Error_Location : Bare_Lkt_Node
   )
      return Decl_P_Extraneous_Parameter_0_Predicate
;

   
   overriding function Call
     (Self : Decl_P_Extraneous_Parameter_0_Predicate;
         Entities : Entity_Vars.Value_Array
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Decl_P_Extraneous_Parameter_0_Predicate;
         Entities : Entity_Vars.Value_Array;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Decl_P_Extraneous_Parameter_0_Predicate) return String;


      




function Decl_P_Unmatched_Argument
   
  (Node : Bare_Decl
      ; Callee_Type : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Predicate used to emit an error when a CallExpr argument could not be
--  matched with a parameter.



         
   type Decl_P_Unmatched_Argument_0_Predicate is new Solver_Ifc.N_Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Decl_P_Unmatched_Argument_0_Predicate
   (
         N : Positive;
         Error_Location : Bare_Lkt_Node
   )
      return Decl_P_Unmatched_Argument_0_Predicate
;

   
   overriding function Call
     (Self : Decl_P_Unmatched_Argument_0_Predicate;
         Entities : Entity_Vars.Value_Array
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Decl_P_Unmatched_Argument_0_Predicate;
         Entities : Entity_Vars.Value_Array;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Decl_P_Unmatched_Argument_0_Predicate) return String;


      




function Internal_Env_Mappings_1
   
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




      




function Decl_P_Full_Name_Internal
   
  (Node : Bare_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;
--  Return the full name of this decl, as it should be seen by users/shown in
--  diagnostics. This property is not public: using it avoids calling PLE when
--  used with ``custom_image``




      




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




      




function Grammar_Rule_Decl_P_Decl_Type_Name
   
  (Node : Bare_Grammar_Rule_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




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





      




function Binding_Val_Decl_P_Decl_Type_Name
   
  (Node : Bare_Binding_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




function Binding_Val_Decl_P_Xref_Entry_Point
   
  (Node : Bare_Binding_Val_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;





      




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




      




function Component_Decl_P_Xref_Equation
   
  (Node : Bare_Component_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Component_Decl_P_To_Generic_Param
   
  (Node : Bare_Component_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Resolved_Param

   
   ;
--  Create a ResolvedParam from the current ComponentDecl.




      




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




      




function Internal_Env_Mappings_2
   
  (Node : Bare_Field_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc

   
   ;





      




function Internal_Env_Mappings_3
   
  (Node : Bare_Field_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc_Array_Access

   
   ;





      




function Fun_Param_Decl_P_Decl_Type_Name
   
  (Node : Bare_Fun_Param_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




function Lambda_Param_Decl_P_Decl_Type_Name
   
  (Node : Bare_Lambda_Param_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




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
--  When this property is called by a LogicCallExpr, return whether it expects
--  a dynamic number of arguments. In other words, it expects an array of
--  entities as its first argument.




      




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





      




function Fun_Decl_P_Find_All_Overrides_Helper
   
  (Node : Bare_Fun_Decl
      ; Current_Node : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Fun_Decl_Array_Access

   
   ;





      




function Fun_Decl_P_Find_All_Overrides
   
  (Node : Bare_Fun_Decl
      ; Units : Internal_Unit_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Fun_Decl_Array_Access

   
   ;
--  Return the list of all RefId that refer to this DefId.




      




function Fun_Decl_P_Base_Fun_Decls
   
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Fun_Decl_Array_Access

   
   ;
--  Return a list of all the parent function declarations that ``self``
--  overrides, including itself.




      




function Internal_Env_Mappings_4
   
  (Node : Bare_Fun_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc

   
   ;





      




function Internal_Env_Mappings_5
   
  (Node : Bare_Fun_Decl
  )

   return Internal_Env_Assoc_Array_Access

   
   ;





      




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





      




function Internal_Env_Mappings_6
   
  (Node : Bare_Env_Spec_Decl
  )

   return Internal_Env_Assoc_Array_Access

   
   ;





      




function Error_Decl_P_Decl_Type_Name
   
  (Node : Bare_Error_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




function Generic_Decl_P_Name
   
  (Node : Bare_Generic_Decl
  )

   return Symbol_Type

   
   ;





      




function Generic_Decl_P_Image_Suffix
   
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;
--  Return the suffix of the node's image that contains the type parameters.




      




function Generic_Decl_P_Generic_Params
   
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Generic_Param_Type_Decl_Array_Access

   
   ;





      




function Generic_Decl_P_Generic_Params_Names
   
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





      




function Generic_Decl_P_Instantiated_Generic_Params
   
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl_Array_Access

   
   ;





      




function Internal_Env_Mappings_7
   
  (Node : Bare_Generic_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc

   
   ;





      




function Grammar_Decl_P_Decl_Type_Name
   
  (Node : Bare_Grammar_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




function Internal_Env_Mappings_8
   
  (Node : Bare_Grammar_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc

   
   ;





      




function Langkit_Root_P_Decl_Type_Name
   
  (Node : Bare_Langkit_Root
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




function Langkit_Root_P_Name
   
  (Node : Bare_Langkit_Root
  )

   return Symbol_Type

   
   ;





      




function Langkit_Root_P_Defined_Scope
   
  (Node : Bare_Langkit_Root
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env

   
   ;
--  ``Origin``: Origin node of the request.




      




function Langkit_Root_P_Resolve_Metadata
   
  (Node : Bare_Langkit_Root
  )

   return Internal_Entity

   
   ;
--  Return the Metadata type: see the doc for __default_metadata in the prelude
--  unit.


         
function Langkit_Root_P_Resolve_Metadata
  (E : Internal_Entity
  ) return Internal_Entity;



      




function Langkit_Root_F_Empty_Type_Ref_List
   
  (Node : Bare_Langkit_Root
  )

   return Bare_Synthetic_Type_Ref_List

   
   ;
--  An empty synthetic TypeRef list node. Used to generate synthetic type
--  declarations.
--
--  This lazy field is on ``LangkitRoot`` so that at most a single synthetic
--  node is created by Lkt unit. Use ``LktNode.get_empty_type_ref_list`` for
--  convenience.




      




function Internal_Env_Do_22
   
  (Node : Bare_Langkit_Root
  )

   return Internal_Unit

   
   ;





      




function Internal_Env_Mappings_23
   
  (Node : Bare_Langkit_Root
  )

   return Internal_Env_Assoc

   
   ;





      




function Internal_Ref_Env_Nodes_24
   
  (Node : Bare_Langkit_Root
  )

   return Bare_Lkt_Node_Array_Access

   
   ;





      




function Internal_Ref_Cond_25
   
  (Node : Bare_Langkit_Root
  )

   return Boolean

   
   ;





      




function Internal_Env_Mappings_26
   
  (Node : Bare_Langkit_Root
  )

   return Internal_Env_Assoc

   
   ;





      




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





      




function Internal_Env_Mappings_9
   
  (Node : Bare_Lexer_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc

   
   ;





      




function Internal_Env_Mappings_10
   
  (Node : Bare_Lexer_Decl
  )

   return Internal_Env_Assoc_Array_Access

   
   ;





      




function Lexer_Family_Decl_P_Decl_Type_Name
   
  (Node : Bare_Lexer_Family_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




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





      




function Synth_Param_Decl_P_Full_Name_Internal
   
  (Node : Bare_Synth_Param_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




function Synth_Param_Decl_P_Decl_Type_Name
   
  (Node : Bare_Synth_Param_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




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





      




function Type_Decl_P_Full_Name
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




function Type_Decl_P_Def_Id
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Def_Id

   
   ;
--  Return the defining name of this type declaration




      




function Dispatcher_Type_Decl_P_Base_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
      with
        Inline_Always
   ;
--  Return the base type for this node, if any.
--
--  Note that this includes "logic" base types for covariant types, like
--  ``Entity`` (i.e. ``Entity[Parent]`` is returned as a base type of
--  ``Entity[Child]``.




      




function Type_Decl_P_Base_Types
   
  (Node : Bare_Type_Decl
      ; Include_Self : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl_Array_Access

   
   ;
--  Return an array containing all base types of ``self`` (ancestors last).
--
--  If ``include_self``, also include this type.




      




function Type_Decl_P_Is_Equation
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Predicate used to verify that operands of equation logic operators are of
--  equation type.



         
   type Type_Decl_P_Is_Equation_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_Equation_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_Equation_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_Equation_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_Equation_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_Equation_0_Predicate) return String;


      




function Type_Decl_P_Is_Bool
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Predicate used to verify that operands of boolean logic operators are of
--  boolean type.



         
   type Type_Decl_P_Is_Bool_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_Bool_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_Bool_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_Bool_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_Bool_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_Bool_0_Predicate) return String;


      




function Type_Decl_P_Is_String_Or_Array_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;




         
   type Type_Decl_P_Is_String_Or_Array_Type_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_String_Or_Array_Type_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_String_Or_Array_Type_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_String_Or_Array_Type_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_String_Or_Array_Type_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_String_Or_Array_Type_0_Predicate) return String;


      




function Type_Decl_P_Is_Int_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;




         
   type Type_Decl_P_Is_Int_Type_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_Int_Type_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_Int_Type_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_Int_Type_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_Int_Type_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_Int_Type_0_Predicate) return String;


      




function Type_Decl_P_Is_Int_Or_Node
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;




         
   type Type_Decl_P_Is_Int_Or_Node_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_Int_Or_Node_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_Int_Or_Node_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_Int_Or_Node_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_Int_Or_Node_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_Int_Or_Node_0_Predicate) return String;


      




function Type_Decl_P_Is_Stream
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;




         
   type Type_Decl_P_Is_Stream_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_Stream_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_Stream_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_Stream_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_Stream_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_Stream_0_Predicate) return String;


      




function Type_Decl_P_Get_Entity_Node_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Get the type parameter used to rebind the Entity type declaration if Self
--  is the entity type.




      




function Type_Decl_P_Is_Subtype_Or_Eq
   
  (Node : Bare_Type_Decl
      ; Rhs : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Return whether ``rhs`` is equal to or is a subtype of ``self``.



         
   type Type_Decl_P_Is_Subtype_Or_Eq_0_Predicate is new Solver_Ifc.N_Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_Subtype_Or_Eq_0_Predicate
   (
         N : Positive;
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_Subtype_Or_Eq_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_Subtype_Or_Eq_0_Predicate;
         Entities : Entity_Vars.Value_Array
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_Subtype_Or_Eq_0_Predicate;
         Entities : Entity_Vars.Value_Array;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_Subtype_Or_Eq_0_Predicate) return String;


      




function Type_Decl_P_Is_Entity_Subtype_Or_Eq
   
  (Node : Bare_Type_Decl
      ; Rhs : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Return whether ``rhs`` is equal to or is a subtype of ``Entity[self]``.



         
   type Type_Decl_P_Is_Entity_Subtype_Or_Eq_0_Predicate is new Solver_Ifc.N_Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_Entity_Subtype_Or_Eq_0_Predicate
   (
         N : Positive;
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_Entity_Subtype_Or_Eq_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_Entity_Subtype_Or_Eq_0_Predicate;
         Entities : Entity_Vars.Value_Array
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_Entity_Subtype_Or_Eq_0_Predicate;
         Entities : Entity_Vars.Value_Array;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_Entity_Subtype_Or_Eq_0_Predicate) return String;


      




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
--  Wrapper around ``TypeDecl.common_ancestor`` that returns the nearest common
--  ancestor for ``self`` and all types for the ``other_types`` slice between
--  indexes ``idx`` and ``other_types.length() - 1`` (included).
--
--  ``imprecise`` is forwarded to ``TypeDecl.common_ancestor``.




      




function Type_Decl_P_Imprecise_Common_Ancestor_List
   
  (Node : Bare_Type_Decl
      ; Other_Types : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Return the nearest common ancestor of ``self`` and ``other_types``,
--  ignoring any type that does not share a common ancestor with the rest of
--  the types.




         
   type Type_Decl_P_Imprecise_Common_Ancestor_List_0_Functor is new Solver_Ifc.Combiner_Type with
      null record
   with First_Controlling_Parameter;

   
   overriding function Combine (
      Self : Type_Decl_P_Imprecise_Common_Ancestor_List_0_Functor;
         Vals : Entity_Vars.Value_Array
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Type_Decl_P_Imprecise_Common_Ancestor_List_0_Functor) return String;


   
   
   function Create_Type_Decl_P_Imprecise_Common_Ancestor_List_0_Functor
   (
         N : Positive
   )
     return Type_Decl_P_Imprecise_Common_Ancestor_List_0_Functor
;

      




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



         
   type Type_Decl_P_Commutative_Matching_Type_0_Predicate is new Solver_Ifc.N_Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Commutative_Matching_Type_0_Predicate
   (
         N : Positive;
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Commutative_Matching_Type_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Commutative_Matching_Type_0_Predicate;
         Entities : Entity_Vars.Value_Array
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Commutative_Matching_Type_0_Predicate;
         Entities : Entity_Vars.Value_Array;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Commutative_Matching_Type_0_Predicate) return String;

         
   type Type_Decl_P_Commutative_Matching_Type_1_Predicate is new Solver_Ifc.N_Predicate_Type with record
         Field_0 : Boolean;
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Commutative_Matching_Type_1_Predicate
   (
         N : Positive;
         Allow_Common_Ancestor : Boolean;
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Commutative_Matching_Type_1_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Commutative_Matching_Type_1_Predicate;
         Entities : Entity_Vars.Value_Array
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Commutative_Matching_Type_1_Predicate;
         Entities : Entity_Vars.Value_Array;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Commutative_Matching_Type_1_Predicate) return String;


      




function Type_Decl_P_Could_Determine_Type
   
  (Node : Bare_Type_Decl
  )

   return Boolean

   
   ;
--  Return true if Self and other are matching types.



         
   type Type_Decl_P_Could_Determine_Type_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Could_Determine_Type_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Could_Determine_Type_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Could_Determine_Type_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Could_Determine_Type_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Could_Determine_Type_0_Predicate) return String;


      




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



         
   type Type_Decl_P_Matching_Type_0_Predicate is new Solver_Ifc.N_Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Matching_Type_0_Predicate
   (
         N : Positive;
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Matching_Type_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Matching_Type_0_Predicate;
         Entities : Entity_Vars.Value_Array
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Matching_Type_0_Predicate;
         Entities : Entity_Vars.Value_Array;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Matching_Type_0_Predicate) return String;

         
   type Type_Decl_P_Matching_Type_1_Predicate is new Solver_Ifc.Predicate_Type with record
         Field_0 : Internal_Entity_Type_Decl;
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Matching_Type_1_Predicate
   (
         Other : Internal_Entity_Type_Decl;
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Matching_Type_1_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Matching_Type_1_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Matching_Type_1_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Matching_Type_1_Predicate) return String;


      




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



         
   type Type_Decl_P_Matching_Logic_Type_0_Predicate is new Solver_Ifc.N_Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Matching_Logic_Type_0_Predicate
   (
         N : Positive;
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Matching_Logic_Type_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Matching_Logic_Type_0_Predicate;
         Entities : Entity_Vars.Value_Array
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Matching_Logic_Type_0_Predicate;
         Entities : Entity_Vars.Value_Array;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Matching_Logic_Type_0_Predicate) return String;

         
   type Type_Decl_P_Matching_Logic_Type_1_Predicate is new Solver_Ifc.Predicate_Type with record
         Field_0 : Internal_Entity_Type_Decl;
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Matching_Logic_Type_1_Predicate
   (
         Other : Internal_Entity_Type_Decl;
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Matching_Logic_Type_1_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Matching_Logic_Type_1_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Matching_Logic_Type_1_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Matching_Logic_Type_1_Predicate) return String;


      




function Type_Decl_P_Is_Indexable_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Predicate used to verify that a type implements the Indexable trait.



         
   type Type_Decl_P_Is_Indexable_Type_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_Indexable_Type_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_Indexable_Type_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_Indexable_Type_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_Indexable_Type_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_Indexable_Type_0_Predicate) return String;


      




function Type_Decl_P_Is_Callable
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Predicate used to emit an error when the type of an expression is not
--  callable.



         
   type Type_Decl_P_Is_Callable_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_Callable_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_Callable_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_Callable_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_Callable_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_Callable_0_Predicate) return String;


      




function Type_Decl_P_Match_Param_Get_Type
   
  (Node : Bare_Type_Decl
      ; Current_Name : Internal_Entity_Argument
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;
--  Return the parameter type corresponding to current_name's declaration.
--
--  ``Current_Name``: Call argument used to find the corresponding
--  ``FunParamDecl`` during type resolution of function calls.
--
--  ``In_Logic_Call``: Whether we are currently solving a ``LogicPropagate`` or
--  a ``LogicPredicate``.




         
   type Type_Decl_P_Match_Param_Get_Type_0_Functor is new Solver_Ifc.Converter_Type with
      record
            Current_Name : Internal_Entity_Argument;
            In_Logic_Call : Boolean;
      end record
   with First_Controlling_Parameter;

   
   overriding function Convert (
      Self : Type_Decl_P_Match_Param_Get_Type_0_Functor;
         From : Internal_Entity
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Type_Decl_P_Match_Param_Get_Type_0_Functor) return String;


   
   
   function Create_Type_Decl_P_Match_Param_Get_Type_0_Functor
   (
         Current_Name : Internal_Entity_Argument;
         In_Logic_Call : Boolean
   )
     return Type_Decl_P_Match_Param_Get_Type_0_Functor
;

      




function Type_Decl_P_Match_Param_Get_Decl
   
  (Node : Bare_Type_Decl
      ; Current_Name : Internal_Entity_Argument
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;
--  Return the parameter declaration corresponding to current_name.
--
--  ``Current_Name``: Call argument used to find the corresponding
--  ``FunParamDecl`` during type resolution of function calls.
--
--  ``In_Logic_Call``: Whether we are currently solving a ``LogicPropagate`` or
--  a ``LogicPredicate``.




         
   type Type_Decl_P_Match_Param_Get_Decl_0_Functor is new Solver_Ifc.Converter_Type with
      record
            Current_Name : Internal_Entity_Argument;
            In_Logic_Call : Boolean;
      end record
   with First_Controlling_Parameter;

   
   overriding function Convert (
      Self : Type_Decl_P_Match_Param_Get_Decl_0_Functor;
         From : Internal_Entity
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Type_Decl_P_Match_Param_Get_Decl_0_Functor) return String;


   
   
   function Create_Type_Decl_P_Match_Param_Get_Decl_0_Functor
   (
         Current_Name : Internal_Entity_Argument;
         In_Logic_Call : Boolean
   )
     return Type_Decl_P_Match_Param_Get_Decl_0_Functor
;

      




function Type_Decl_P_Lambda_Param_Get_Type
   
  (Node : Bare_Type_Decl
      ; Param_Decl : Internal_Entity_Lambda_Param_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Return the type declaration corresponding to param_decl's type.




      




function Type_Decl_P_Is_Valid_Call
   
  (Node : Bare_Type_Decl
      ; Args : Internal_Entity_Argument_List
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Check whether the call to Self is valid and all parameters are paired or
--  have a default value.
--
--  ``In_Logic_Call``: Whether we are currently solving a ``LogicPropagate`` or
--  a ``LogicPredicate``.



         
   type Type_Decl_P_Is_Valid_Call_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Field_0 : Internal_Entity_Argument_List;
         Field_1 : Boolean;
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Type_Decl_P_Is_Valid_Call_0_Predicate
   (
         Args : Internal_Entity_Argument_List;
         In_Logic_Call : Boolean;
         Error_Location : Bare_Lkt_Node
   )
      return Type_Decl_P_Is_Valid_Call_0_Predicate
;

   
   overriding function Call
     (Self : Type_Decl_P_Is_Valid_Call_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Type_Decl_P_Is_Valid_Call_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Type_Decl_P_Is_Valid_Call_0_Predicate) return String;


      




function Type_Decl_P_Get_Return_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Return the return type of the FunctionType.




         
   type Type_Decl_P_Get_Return_Type_0_Functor is new Solver_Ifc.Converter_Type with
      null record
   with First_Controlling_Parameter;

   
   overriding function Convert (
      Self : Type_Decl_P_Get_Return_Type_0_Functor;
         From : Internal_Entity
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Type_Decl_P_Get_Return_Type_0_Functor) return String;


   
   
   function Create_Type_Decl_P_Get_Return_Type_0_Functor
     return Type_Decl_P_Get_Return_Type_0_Functor
;

      




function Type_Decl_P_Create_Function_Type
   
  (Node : Bare_Type_Decl
      ; Params : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Create a FunctionType, using Self as the return type and params for the
--  parameter types.




         
   type Type_Decl_P_Create_Function_Type_0_Functor is new Solver_Ifc.Combiner_Type with
      null record
   with First_Controlling_Parameter;

   
   overriding function Combine (
      Self : Type_Decl_P_Create_Function_Type_0_Functor;
         Vals : Entity_Vars.Value_Array
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Type_Decl_P_Create_Function_Type_0_Functor) return String;


   
   
   function Create_Type_Decl_P_Create_Function_Type_0_Functor
   (
         N : Positive
   )
     return Type_Decl_P_Create_Function_Type_0_Functor
;

      




function Type_Decl_P_Make_Array_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Create a rebound ``Array`` type declaration, using ``self`` as the type
--  parameter.




         
   type Type_Decl_P_Make_Array_Type_0_Functor is new Solver_Ifc.Converter_Type with
      null record
   with First_Controlling_Parameter;

   
   overriding function Convert (
      Self : Type_Decl_P_Make_Array_Type_0_Functor;
         From : Internal_Entity
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Type_Decl_P_Make_Array_Type_0_Functor) return String;


   
   
   function Create_Type_Decl_P_Make_Array_Type_0_Functor
     return Type_Decl_P_Make_Array_Type_0_Functor
;

      




function Type_Decl_P_Make_Stream_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Create a rebound ``Stream`` type declaration, using ``self`` as the type
--  parameter.




         
   type Type_Decl_P_Make_Stream_Type_0_Functor is new Solver_Ifc.Converter_Type with
      null record
   with First_Controlling_Parameter;

   
   overriding function Convert (
      Self : Type_Decl_P_Make_Stream_Type_0_Functor;
         From : Internal_Entity
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Type_Decl_P_Make_Stream_Type_0_Functor) return String;


   
   
   function Create_Type_Decl_P_Make_Stream_Type_0_Functor
     return Type_Decl_P_Make_Stream_Type_0_Functor
;

      




function Type_Decl_P_Get_Indexable_Content_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Get the type parameter used to rebind the Array type declaration.




         
   type Type_Decl_P_Get_Indexable_Content_Type_0_Functor is new Solver_Ifc.Converter_Type with
      null record
   with First_Controlling_Parameter;

   
   overriding function Convert (
      Self : Type_Decl_P_Get_Indexable_Content_Type_0_Functor;
         From : Internal_Entity
   ) return Internal_Entity
 with Inline;
   overriding function Image (Self : Type_Decl_P_Get_Indexable_Content_Type_0_Functor) return String;


   
   
   function Create_Type_Decl_P_Get_Indexable_Content_Type_0_Functor
     return Type_Decl_P_Get_Indexable_Content_Type_0_Functor
;

      




function Type_Decl_P_Get_Super_Of_Parent
   
  (Node : Bare_Type_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;
--  Return the declaration that ``origin`` overrides (i.e. the homonym
--  declaration in the base type, if any).
--
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




      




function Dispatcher_Type_Decl_P_Is_Subtype
   
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
      with
        Inline_Always
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
--  Return the nearest common ancestor between ``self`` and ``other``.
--
--  When there is no common ancestor, return the null type (if ``imprecise`` is
--  False) or either ``self`` or ``other`` that is non-null.




      




function Type_Decl_P_Node_Builder_Scope
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env

   
   ;
--  Create an environment with the builder() function associated to the node
--  type parameter. If the type is not a class, return an empty environment.




      




function Type_Decl_P_Instantiate_Generic_Type_Decl
   
  (Node : Bare_Type_Decl
      ; Param_Types : Internal_Entity_Type_Decl_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Convenience wrapper around ``Decl.instantiated_generic_decl`` when it is
--  known to return a ``TypeDecl``.




      




function Type_Decl_P_Base_Type
   
  (Node : Bare_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Return the base type for this node, if any.
--
--  Note that this includes "logic" base types for covariant types, like
--  ``Entity`` (i.e. ``Entity[Parent]`` is returned as a base type of
--  ``Entity[Child]``.




      




function Type_Decl_P_Is_Subtype
   
  (Node : Bare_Type_Decl
      ; Other : Internal_Entity_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Return true if Self is a subtype of other.




      




function Any_Type_Decl_P_Full_Name_Internal
   
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




      




function Enum_Class_Alt_Decl_P_Base_Type
   
  (Node : Bare_Enum_Class_Alt_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Return the parent EnumTypeDecl.




      




function Function_Type_P_Full_Name_Internal
   
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




         
   type Function_Type_P_Returns_Entity_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Function_Type_P_Returns_Entity_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Function_Type_P_Returns_Entity_0_Predicate
;

   
   overriding function Call
     (Self : Function_Type_P_Returns_Entity_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Function_Type_P_Returns_Entity_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Function_Type_P_Returns_Entity_0_Predicate) return String;


      




function Function_Type_P_Returns_Bool
   
  (Node : Bare_Function_Type
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;




         
   type Function_Type_P_Returns_Bool_0_Predicate is new Solver_Ifc.Predicate_Type with record
         Error_Location : Bare_Lkt_Node;
   end record
   with First_Controlling_Parameter;

   
   

   function Create_Function_Type_P_Returns_Bool_0_Predicate
   (
         Error_Location : Bare_Lkt_Node
   )
      return Function_Type_P_Returns_Bool_0_Predicate
;

   
   overriding function Call
     (Self : Function_Type_P_Returns_Bool_0_Predicate;
         Entity : Internal_Entity
     ) return Boolean
;

   
   overriding procedure Failed
     (Self : Function_Type_P_Returns_Bool_0_Predicate;
         Entity : Internal_Entity;
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
;

   overriding function Image (Self : Function_Type_P_Returns_Bool_0_Predicate) return String;


      




function Generic_Param_Type_Decl_P_Decl_Type_Name
   
  (Node : Bare_Generic_Param_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




function Named_Type_Decl_P_Defined_Scope
   
  (Node : Bare_Named_Type_Decl
      ; Origin : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Lexical_Env

   
   ;
--  ``Origin``: Origin node of the request.




      




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
--  Return the list of all fields that must have a corresponding parameter in
--  the class constructor.




      




function Class_Decl_P_Function_Type_Aux
   
  (Node : Bare_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Function_Type

   
   ;





      




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





      




function Internal_Env_Mappings_11
   
  (Node : Bare_Enum_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc

   
   ;





      




function Internal_Env_Mappings_12
   
  (Node : Bare_Enum_Class_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc_Array_Access

   
   ;





      




function Enum_Type_Decl_P_Decl_Type_Name
   
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;





      




function Internal_Env_Mappings_13
   
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc

   
   ;





      




function Internal_Env_Mappings_14
   
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc_Array_Access

   
   ;





      




function Internal_Env_Mappings_15
   
  (Node : Bare_Enum_Type_Decl
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Env_Assoc_Array_Access

   
   ;





      




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



         
function Dyn_Env_Wrapper_P_Instantiation_Bindings
  (E : Internal_Entity
  ) return Internal_Inner_Env_Assoc_Array_Access;



      




function Dispatcher_Excludes_Null_P_As_Bool
   
  (Node : Bare_Excludes_Null
  )

   return Boolean

   
      with
        Inline_Always
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





      




function Expr_P_Xref_Entry_Point
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;





      




function Dispatcher_Expr_P_Get_Type
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
      with
        Inline_Always
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





      




function Expr_P_Match_Params
   
  (Node : Bare_Expr
      ; Params : Internal_Resolved_Param_Array_Access
      ; Args : Internal_Entity_Argument_List
  )

   return Internal_Param_Match_Array_Access

   
   ;
--  Match a function's parameters with the arguments of the CallExpr.




      




function Dispatcher_Expr_P_Xlogic_Equation
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
      with
        Inline_Always
   ;
--  Build an equation to solve type and name resolution for logic expressions.




      




function Dispatcher_Expr_P_Xtype_Equation
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
      with
        Inline_Always
   ;
--  Build an equation to solve type and name resolution for type referencement.




      




function Dispatcher_Expr_P_Referenced_Decl
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
      with
        Inline_Always
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

   
      with
        Inline_Always
   ;
--  Returns True if the expression's actual type can be determined without
--  using its expected type.




      




function Expr_P_Get_Type
   
  (Node : Bare_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Return the type of this expression.




      




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





      




function Base_Call_Expr_P_Generic_Type_Equation_Helper
   
  (Node : Bare_Base_Call_Expr
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;
--  ``In_Logic_Call``: Whether we are currently solving a ``LogicPropagate`` or
--  a ``LogicPredicate``.




      




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
--  Compute the expected type of name and expressions in args.




      




function Base_Call_Expr_P_Xref_Call_Args_Equation
   
  (Node : Bare_Base_Call_Expr
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;
--  Build an equation to match arguments of the call to the parameters of the
--  called entity.
--
--  ``In_Logic_Call``: Whether we are currently solving a ``LogicPropagate`` or
--  a ``LogicPredicate``.




      




function Base_Call_Expr_P_Xref_Call_Equation
   
  (Node : Bare_Base_Call_Expr
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;
--  Build an equation to solve type and name resolution for calling this
--  BaseCallExpr
--
--  ``In_Logic_Call``: Whether we are currently solving a ``LogicPropagate`` or
--  a ``LogicPredicate``.




      




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




      




function Logic_Predicate_P_Generic_Type_Equation
   
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





      




function Block_Expr_P_Expr
   
  (Node : Bare_Block_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Expr

   
   ;





      




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





      




function Dot_Expr_P_Referenced_Decl
   
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;





      




function Dot_Expr_P_Has_Context_Free_Type
   
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;





      




function Dot_Expr_P_First_Var_In_Prefix_Env
   
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;
--  Bind dest's logic variables to the correct declaration depending on its
--  prefix(Self).




      




function Dot_Expr_P_Xtype_Equation
   
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Dot_Expr_P_Is_Call_To_Super
   
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;
--  Return True if this DotExpr is a reference to super (meaning it matches the
--  patterns ``self.super`` or ``node.super``).




      




function Dot_Expr_P_Generic_Type_Equation
   
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Dot_Expr_P_Expected_Type_Equation
   
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Dot_Expr_P_Xref_Typing_Equation
   
  (Node : Bare_Dot_Expr
      ; In_Logic_Call : Boolean
         := False
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;
--  ``In_Logic_Call``: Whether we are currently solving a ``LogicPropagate`` or
--  a ``LogicPredicate``.




      




function Dot_Expr_P_Xref_Equation
   
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Dot_Expr_P_Xlogic_Equation
   
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Dot_Expr_P_Complete
   
  (Node : Bare_Dot_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Complete_Item_Array_Access

   
   ;





      




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





      




function Generic_Instantiation_P_Xref_Equation
   
  (Node : Bare_Generic_Instantiation
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Id_P_Is_Not_Type_Name
   
  (Node : Bare_Id
  )

   return Boolean

   
   ;





      




function Def_Id_P_Decl
   
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;
--  Return the Decl that defines this identifier.




      




function Def_Id_P_Name
   
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;
--  Return the name defined by this DefId.




      




function Def_Id_P_Get_Type
   
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;
--  Return the type of the symbol defined by this DefId.




      




function Def_Id_P_Get_Implementatinons
   
  (Node : Bare_Def_Id
      ; Units : Internal_Unit_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Def_Id_Array_Access

   
   ;
--  Return the implementations of this name.




      




function Def_Id_P_Decl_Detail
   
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;
--  Return the details to display in the language server client when it
--  requests for completion or hovering information.




      




function Def_Id_P_Completion_Item_Kind
   
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Integer

   
   ;
--  Return the kind of completion item for this DefId.




      




function Def_Id_P_Doc
   
  (Node : Bare_Def_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return String_Type

   
   ;
--  Return the documentation associated to this DefId.




      




function Def_Id_P_Find_All_References_Helper
   
  (Node : Bare_Def_Id
      ; Current_Node : Internal_Entity
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Ref_Id_Array_Access

   
   ;





      




function Def_Id_P_Find_All_References
   
  (Node : Bare_Def_Id
      ; Units : Internal_Unit_Array_Access
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Ref_Result_Array_Access

   
   ;
--  Return the list of all RefId that refer to this DefId.




      




function Imported_Id_P_Referenced_Decl_From_Module
   
  (Node : Bare_Imported_Id
      ; Module_Name : Bare_Module_Id
  )

   return Internal_Entity_Decl

   
   ;
--  Helper for ``referenced_decl``. Return the declaration that corresponds to
--  this imported id, assuming that it was imported from ``module_name``.




      




function Imported_Id_P_Referenced_Decl
   
  (Node : Bare_Imported_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;





      




function Imported_Id_P_Get_Type
   
  (Node : Bare_Imported_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;





      




function Module_Id_P_Referenced_Decl
   
  (Node : Bare_Module_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Decl

   
   ;





      




function Module_Id_P_Xref_Equation
   
  (Node : Bare_Module_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Module_Id_P_Get_Type
   
  (Node : Bare_Module_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Type_Decl

   
   ;





      




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





      




function Ref_Id_P_Referenced_Defining_Name
   
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Entity_Def_Id

   
   ;
--  Return the referenced defining name.




      




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





      




function Ref_Id_P_Complete
   
  (Node : Bare_Ref_Id
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Internal_Complete_Item_Array_Access

   
   ;





      




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
--  Return an equation that assigns the expected types for all branch
--  expressions.




      




function If_Expr_P_Cond_Branches_Equation
   
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;
--  Return an equation that verifies that all condition expressions are valid
--  and assigns them their expected types.




      




function If_Expr_P_Xref_Equation
   
  (Node : Bare_If_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




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





      




function Keep_Expr_P_Xref_Equation
   
  (Node : Bare_Keep_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




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





      




function Big_Num_Lit_P_Xref_Equation
   
  (Node : Bare_Big_Num_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Char_Lit_P_Xref_Equation
   
  (Node : Bare_Char_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




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





      




function Num_Lit_P_Xref_Equation
   
  (Node : Bare_Num_Lit
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Dispatcher_String_Lit_P_Denoted_Value
   
  (Node : Bare_String_Lit
  )

   return Internal_Decoded_String_Value

   
      with
        Inline_Always
   ;
--  Return the content of the given string literal node.




      




function Dispatcher_String_Lit_P_Is_Prefixed_String
   
  (Node : Bare_String_Lit
  )

   return Boolean

   
      with
        Inline_Always
   ;
--  Return whether this string is prefixed or not.




      




function Dispatcher_String_Lit_P_Prefix
   
  (Node : Bare_String_Lit
  )

   return Character_Type

   
      with
        Inline_Always
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





      




function Module_Doc_String_Lit_P_Is_Prefixed_String
   
  (Node : Bare_Module_Doc_String_Lit
  )

   return Boolean

   
   ;





      




function Module_Doc_String_Lit_P_Prefix
   
  (Node : Bare_Module_Doc_String_Lit
  )

   return Character_Type

   
   ;





      




function Logic_Assign_P_Xref_Equation
   
  (Node : Bare_Logic_Assign
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Logic_Expr_P_Xref_Equation
   
  (Node : Bare_Logic_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Logic_Propagate_P_Generic_Type_Equation
   
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





      




function Logic_Unify_P_Xref_Equation
   
  (Node : Bare_Logic_Unify
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




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





      




function Not_Expr_P_Xref_Equation
   
  (Node : Bare_Not_Expr
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




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





      




function Un_Op_P_Xref_Equation
   
  (Node : Bare_Un_Op
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




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




      




function Imported_Name_P_Resolve_Simple_Import
   
  (Node : Bare_Imported_Name
  )

   return Internal_Entity

   
   ;
--  Lexical env resolver for imported names from ``import`` nodes.


         
function Imported_Name_P_Resolve_Simple_Import
  (E : Internal_Entity
  ) return Internal_Entity;



      




function Imported_Name_P_Resolve_Import_From
   
  (Node : Bare_Imported_Name
  )

   return Internal_Entity

   
   ;
--  Lexical env resolver for imported names from ``from M import ...`` nodes.


         
function Imported_Name_P_Resolve_Import_From
  (E : Internal_Entity
  ) return Internal_Entity;



      




function Imported_Name_P_Env_Assoc
   
  (Node : Bare_Imported_Name
  )

   return Internal_Env_Assoc

   
   ;
--  Helper for env specs: return an env association for this imported name.




      




function Dispatcher_Null_Cond_Qualifier_P_As_Bool
   
  (Node : Bare_Null_Cond_Qualifier
  )

   return Boolean

   
      with
        Inline_Always
   ;
--  Return whether this node is present




      




function Null_Cond_Qualifier_Absent_P_As_Bool
   
  (Node : Bare_Null_Cond_Qualifier_Absent
  )

   return Boolean

   
   ;





      




function Null_Cond_Qualifier_Present_P_As_Bool
   
  (Node : Bare_Null_Cond_Qualifier_Present
  )

   return Boolean

   
   ;





      




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





      




function Op_P_Is_Commutative
   
  (Node : Bare_Op
  )

   return Boolean

   
   ;





      




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




      




function Function_Type_Ref_P_Xref_Equation
   
  (Node : Bare_Function_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Generic_Type_Ref_P_Xref_Equation
   
  (Node : Bare_Generic_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Simple_Type_Ref_P_Xref_Equation
   
  (Node : Bare_Simple_Type_Ref
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;





      




function Var_Bind_P_Xref_Entry_Point
   
  (Node : Bare_Var_Bind
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Boolean

   
   ;





      




function Var_Bind_P_Xref_Equation
   
  (Node : Bare_Var_Bind
   ; E_Info : Internal_Entity_Info :=
      No_Entity_Info
  )

   return Logic_Equation

   
   ;






      
      procedure Import_Pre_Env_Actions
        (Self            : Bare_Import;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Import_All_From_Pre_Env_Actions
        (Self            : Bare_Import_All_From;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Import_From_Pre_Env_Actions
        (Self            : Bare_Import_From;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Base_Match_Branch_Pre_Env_Actions
        (Self            : Bare_Base_Match_Branch;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Decl_Pre_Env_Actions
        (Self            : Bare_Decl;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Field_Decl_Pre_Env_Actions
        (Self            : Bare_Field_Decl;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Fun_Decl_Pre_Env_Actions
        (Self            : Bare_Fun_Decl;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Env_Spec_Decl_Pre_Env_Actions
        (Self            : Bare_Env_Spec_Decl;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Generic_Decl_Pre_Env_Actions
        (Self            : Bare_Generic_Decl;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Grammar_Decl_Pre_Env_Actions
        (Self            : Bare_Grammar_Decl;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Langkit_Root_Pre_Env_Actions
        (Self            : Bare_Langkit_Root;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Lexer_Decl_Pre_Env_Actions
        (Self            : Bare_Lexer_Decl;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Enum_Class_Decl_Pre_Env_Actions
        (Self            : Bare_Enum_Class_Decl;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);

      procedure Enum_Class_Decl_Post_Env_Actions
        (Self : Bare_Enum_Class_Decl; State : in out PLE_Node_State);

      
      procedure Enum_Type_Decl_Pre_Env_Actions
        (Self            : Bare_Enum_Type_Decl;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);

      procedure Enum_Type_Decl_Post_Env_Actions
        (Self : Bare_Enum_Type_Decl; State : in out PLE_Node_State);

      
      procedure Block_Expr_Pre_Env_Actions
        (Self            : Bare_Block_Expr;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Lambda_Expr_Pre_Env_Actions
        (Self            : Bare_Lambda_Expr;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);


      
      procedure Decl_Block_Pre_Env_Actions
        (Self            : Bare_Decl_Block;
         State           : in out PLE_Node_State;
         Add_To_Env_Only : Boolean := False);



end Liblktlang.Impl_0;
