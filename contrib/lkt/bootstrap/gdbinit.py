
"""
This script is meant to be sourced inside GDB when debugging a program that
uses liblktlang. It installs Langkit's "GDB helpers": pretty-printers and
custom commands that are convenient when debugging liblktlang. Note that GDB
helpers require the Langkit Python library.
"""

import sys




try:
    import langkit.gdb
except ImportError as exc:
    print(
        f"{__file__}: Cannot import the 'langkit.gdb' Python package: langkit"
        " GDB helpers require it",
        file=sys.stderr,
    )
else:
    
    langkit.gdb.setup(
        lib_name='liblktlang',
        astnode_names=['Lkt_Node', 'Base_Lexer_Case_Rule_Alt', 'Lexer_Case_Rule_Cond_Alt', 'Lexer_Case_Rule_Default_Alt', 'Block_String_Line', 'Class_Qualifier', 'Class_Qualifier_Absent', 'Class_Qualifier_Present', 'Decl', 'Base_Grammar_Rule_Decl', 'Grammar_Rule_Decl', 'Synthetic_Lexer_Decl', 'Base_Val_Decl', 'Node_Decl', 'Self_Decl', 'User_Val_Decl', 'Enum_Lit_Decl', 'Explicitly_Typed_Decl', 'Component_Decl', 'Field_Decl', 'Fun_Arg_Decl', 'Lambda_Arg_Decl', 'Dyn_Var_Decl', 'Match_Val_Decl', 'Val_Decl', 'Fun_Decl', 'Env_Spec_Decl', 'Generic_Decl', 'Grammar_Decl', 'Lexer_Decl', 'Lexer_Family_Decl', 'Synth_Arg_Decl', 'Synth_Fun_Decl', 'Type_Decl', 'Any_Type_Decl', 'Enum_Class_Alt_Decl', 'Function_Type', 'Generic_Formal_Type_Decl', 'Named_Type_Decl', 'Basic_Class_Decl', 'Class_Decl', 'Enum_Class_Decl', 'Enum_Type_Decl', 'Struct_Decl', 'Trait_Decl', 'Decl_Annotation', 'Decl_Annotation_Params', 'Dyn_Env_Wrapper', 'Elsif_Branch', 'Enum_Class_Case', 'Excludes_Null', 'Excludes_Null_Absent', 'Excludes_Null_Present', 'Expr', 'Any_Of', 'Array_Literal', 'Base_Call_Expr', 'Call_Expr', 'Logic_Call_Expr', 'Logic_Predicate', 'Logic_Propagate_Call', 'Base_Dot_Expr', 'Dot_Expr', 'Null_Cond_Dotted_Name', 'Bin_Op', 'Block_Expr', 'Cast_Expr', 'Error_On_Null', 'Generic_Instantiation', 'Grammar_Expr', 'Grammar_Cut', 'Grammar_Discard', 'Grammar_Dont_Skip', 'Grammar_List', 'Grammar_Null', 'Grammar_Opt', 'Grammar_Opt_Error', 'Grammar_Opt_Error_Group', 'Grammar_Opt_Group', 'Grammar_Or_Expr', 'Grammar_Pick', 'Grammar_Implicit_Pick', 'Grammar_Predicate', 'Grammar_Rule_Ref', 'Grammar_Skip', 'Grammar_Stop_Cut', 'Parse_Node_Expr', 'Token_Lit', 'Token_No_Case_Lit', 'Token_Pattern_Concat', 'Token_Pattern_Lit', 'Token_Ref', 'Id', 'Def_Id', 'Module_Ref_Id', 'Ref_Id', 'If_Expr', 'Isa', 'Keep_Expr', 'Lambda_Expr', 'Lit', 'Big_Num_Lit', 'Char_Lit', 'Null_Lit', 'Num_Lit', 'String_Lit', 'Block_String_Lit', 'Single_Line_String_Lit', 'Pattern_Single_Line_String_Lit', 'Logic_Assign', 'Logic_Expr', 'Logic_Propagate', 'Logic_Unify', 'Match_Expr', 'Not_Expr', 'Paren_Expr', 'Raise_Expr', 'Subscript_Expr', 'Null_Cond_Subscript_Expr', 'Try_Expr', 'Un_Op', 'Full_Decl', 'Grammar_List_Sep', 'Import', 'Langkit_Root', 'Lexer_Case_Rule', 'Lexer_Case_Rule_Send', 'List_Kind', 'List_Kind_One', 'List_Kind_Zero', 'Lkt_Node_Base_List', 'Base_Lexer_Case_Rule_Alt_List', 'Block_String_Line_List', 'Call_Expr_List', 'Decl_Annotation_List', 'Elsif_Branch_List', 'Enum_Class_Alt_Decl_List', 'Enum_Class_Case_List', 'Enum_Lit_Decl_List', 'Expr_List', 'Any_Of_List', 'Full_Decl_List', 'Decl_Block', 'Generic_Formal_Decl_List', 'Fun_Arg_Decl_List', 'Grammar_Expr_List', 'Grammar_Expr_List_List', 'Import_List', 'Lambda_Arg_Decl_List', 'Lkt_Node_List', 'Block_Decl_List', 'Match_Branch_List', 'Param_List', 'Ref_Id_List', 'Type_Ref_List', 'Isa_List', 'Match_Branch', 'Op', 'Op_Amp', 'Op_And', 'Op_Div', 'Op_Eq', 'Op_Gt', 'Op_Gte', 'Op_Logic_And', 'Op_Logic_Or', 'Op_Lt', 'Op_Lte', 'Op_Minus', 'Op_Mult', 'Op_Ne', 'Op_Or', 'Op_Or_Int', 'Op_Plus', 'Param', 'Type_Ref', 'Default_List_Type_Ref', 'Function_Type_Ref', 'Generic_Type_Ref', 'Simple_Type_Ref', 'Var_Bind'],
        astnode_kinds={1: 'Lexer_Case_Rule_Cond_Alt', 2: 'Lexer_Case_Rule_Default_Alt', 3: 'Block_String_Line', 4: 'Class_Qualifier_Absent', 5: 'Class_Qualifier_Present', 6: 'Grammar_Rule_Decl', 7: 'Synthetic_Lexer_Decl', 8: 'Node_Decl', 9: 'Self_Decl', 10: 'Enum_Lit_Decl', 11: 'Field_Decl', 12: 'Fun_Arg_Decl', 13: 'Lambda_Arg_Decl', 14: 'Dyn_Var_Decl', 15: 'Match_Val_Decl', 16: 'Val_Decl', 17: 'Fun_Decl', 18: 'Env_Spec_Decl', 19: 'Generic_Decl', 20: 'Grammar_Decl', 21: 'Lexer_Decl', 22: 'Lexer_Family_Decl', 23: 'Synth_Arg_Decl', 24: 'Synth_Fun_Decl', 25: 'Any_Type_Decl', 26: 'Enum_Class_Alt_Decl', 27: 'Function_Type', 28: 'Generic_Formal_Type_Decl', 29: 'Class_Decl', 30: 'Enum_Class_Decl', 31: 'Enum_Type_Decl', 32: 'Struct_Decl', 33: 'Trait_Decl', 34: 'Decl_Annotation', 35: 'Decl_Annotation_Params', 36: 'Dyn_Env_Wrapper', 37: 'Elsif_Branch', 38: 'Enum_Class_Case', 39: 'Excludes_Null_Absent', 40: 'Excludes_Null_Present', 41: 'Any_Of', 42: 'Array_Literal', 43: 'Call_Expr', 44: 'Logic_Predicate', 45: 'Logic_Propagate_Call', 46: 'Dot_Expr', 47: 'Null_Cond_Dotted_Name', 48: 'Bin_Op', 49: 'Block_Expr', 50: 'Cast_Expr', 51: 'Error_On_Null', 52: 'Generic_Instantiation', 53: 'Grammar_Cut', 54: 'Grammar_Discard', 55: 'Grammar_Dont_Skip', 56: 'Grammar_List', 57: 'Grammar_Null', 58: 'Grammar_Opt', 59: 'Grammar_Opt_Error', 60: 'Grammar_Opt_Error_Group', 61: 'Grammar_Opt_Group', 62: 'Grammar_Or_Expr', 63: 'Grammar_Pick', 64: 'Grammar_Implicit_Pick', 65: 'Grammar_Predicate', 66: 'Grammar_Rule_Ref', 67: 'Grammar_Skip', 68: 'Grammar_Stop_Cut', 69: 'Parse_Node_Expr', 70: 'Token_Lit', 71: 'Token_No_Case_Lit', 72: 'Token_Pattern_Concat', 73: 'Token_Pattern_Lit', 74: 'Token_Ref', 75: 'Id', 76: 'Def_Id', 77: 'Module_Ref_Id', 78: 'Ref_Id', 79: 'If_Expr', 80: 'Isa', 81: 'Keep_Expr', 82: 'Lambda_Expr', 83: 'Big_Num_Lit', 84: 'Char_Lit', 85: 'Null_Lit', 86: 'Num_Lit', 87: 'Block_String_Lit', 88: 'Single_Line_String_Lit', 89: 'Pattern_Single_Line_String_Lit', 90: 'Logic_Assign', 91: 'Logic_Expr', 92: 'Logic_Propagate', 93: 'Logic_Unify', 94: 'Match_Expr', 95: 'Not_Expr', 96: 'Paren_Expr', 97: 'Raise_Expr', 98: 'Subscript_Expr', 99: 'Null_Cond_Subscript_Expr', 100: 'Try_Expr', 101: 'Un_Op', 102: 'Full_Decl', 103: 'Grammar_List_Sep', 104: 'Import', 105: 'Langkit_Root', 106: 'Lexer_Case_Rule', 107: 'Lexer_Case_Rule_Send', 108: 'List_Kind_One', 109: 'List_Kind_Zero', 110: 'Base_Lexer_Case_Rule_Alt_List', 111: 'Block_String_Line_List', 112: 'Call_Expr_List', 113: 'Decl_Annotation_List', 114: 'Elsif_Branch_List', 115: 'Enum_Class_Alt_Decl_List', 116: 'Enum_Class_Case_List', 117: 'Enum_Lit_Decl_List', 118: 'Expr_List', 119: 'Any_Of_List', 120: 'Full_Decl_List', 121: 'Decl_Block', 122: 'Generic_Formal_Decl_List', 123: 'Fun_Arg_Decl_List', 124: 'Grammar_Expr_List', 125: 'Grammar_Expr_List_List', 126: 'Import_List', 127: 'Lambda_Arg_Decl_List', 128: 'Lkt_Node_List', 129: 'Block_Decl_List', 130: 'Match_Branch_List', 131: 'Param_List', 132: 'Ref_Id_List', 133: 'Type_Ref_List', 134: 'Isa_List', 135: 'Match_Branch', 136: 'Op_Amp', 137: 'Op_And', 138: 'Op_Div', 139: 'Op_Eq', 140: 'Op_Gt', 141: 'Op_Gte', 142: 'Op_Logic_And', 143: 'Op_Logic_Or', 144: 'Op_Lt', 145: 'Op_Lte', 146: 'Op_Minus', 147: 'Op_Mult', 148: 'Op_Ne', 149: 'Op_Or', 150: 'Op_Or_Int', 151: 'Op_Plus', 152: 'Param', 153: 'Default_List_Type_Ref', 154: 'Function_Type_Ref', 155: 'Generic_Type_Ref', 156: 'Simple_Type_Ref', 157: 'Var_Bind'},
        prefix='lkt'
    )
