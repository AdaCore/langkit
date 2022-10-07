--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package contains definitions for exceptions common to all
--  Langkit-generated libraries.

package Langkit_Support.Errors with Preelaborate is

   Native_Exception        : exception;
   Precondition_Failure    : exception;
   Property_Error          : exception;
   Invalid_Unit_Name_Error : exception;
   Invalid_Symbol_Error    : exception;
   Stale_Reference_Error   : exception;
   Unknown_Charset         : exception;
   Invalid_Input           : exception;
   Syntax_Error            : exception;
   File_Read_Error         : exception;

   package Introspection is
      Bad_Type_Error      : exception;
      Out_Of_Bounds_Error : exception;
   end Introspection;

   package Unparsing is
      Malformed_Tree_Error : exception;
   end Unparsing;

   package Rewriting is
      Template_Format_Error        : exception;
      Template_Args_Error          : exception;
      Template_Instantiation_Error : exception;
   end Rewriting;

end Langkit_Support.Errors;
