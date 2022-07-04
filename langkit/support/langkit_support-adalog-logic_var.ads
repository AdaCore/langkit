--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  This package provides data types to create logic variables, i.e. variables
--  that logic equation must associate to set of values.

generic
   type Value_Type is private;
   --  Type of the values associated to variables, assumed to have by-reference
   --  semantics.

   with procedure Inc_Ref (E : Value_Type) is null;
   with procedure Dec_Ref (E : in out Value_Type) is null;
   --  Associated ref-counting primitives

   with function Value_Image (E : Value_Type) return String is <>;
   --  Image of values, for debugging purposes

package Langkit_Support.Adalog.Logic_Var is

   type Logic_Var_Record;
   --  Storage for a logic variable. Equations only deal with reference to them
   --  (see ``Logic_Var`` access type below).

   type Logic_Var is access all Logic_Var_Record;
   --  Reference to a logic variable

   No_Logic_Var : constant Logic_Var := null;

   type Logic_Var_Record is record
      Reset : Boolean := True;
      --  Whether this variable is reset, i.e. whether it has no value

      Value : Value_Type;
      --  The value of this logic variable, when it is set (see the ``Reset``
      --  component).

      Dbg_Name : String_Access;
      --  Access to a string representing the name of this variable, for
      --  debugging purposes.

      Id : Natural := 0;

      Aliased_To : Logic_Var := null;
   end record;

   procedure Reset (Self : Logic_Var);
   --  Reset the logic variable to an undefined state with no value

   function Is_Defined (Self : Logic_Var) return Boolean;
   --  Return whether the logic variable has a value

   procedure Set_Value (Self : Logic_Var; Data : Value_Type);
   --  Set the value of the logic variable to ``Data``. Only the solver is
   --  supposed to use this.

   function Get_Value (Self : Logic_Var) return Value_Type;
   --  Assuming this variable has an associated value, return it, along with a
   --  new ownership share: the caller must call ``Dec_Ref`` on the result when
   --  done with it.

   function Image (Self : Logic_Var) return String is
     (if Self.Dbg_Name /= null
      then "%" &  Self.Dbg_Name.all
      else "%<unnamed>");

   function Id (Self : Logic_Var) return Natural;
   --  Return the Id of this variable.
   --
   --  Variables have a null Id by default, and the solver assigns it a
   --  positive Id during resolution. Handling integers instead of pointers
   --  makes it easier to create collections of variables.

   procedure Set_Id (Self : Logic_Var; Id : Natural);
   --  Set the Id for this logic variable. If 0, reset its Id and unalias the
   --  variable, if it was aliased.

   procedure Alias (Self, To : Logic_Var);
   --  Alias this variable to another variable.
   --
   --  Note that this operation is valid only if both variables are still
   --  reset, i.e. before variable assignments. This way, aliasing two
   --  variables can never fail (we would need to perform value consistency
   --  checking otherwise).

   procedure Unalias (Self : Logic_Var);
   --  Remove alias information for this variable

   function Get_Alias (Self : Logic_Var) return Logic_Var;
   --  Get the alias for this logic variable, if there is one

   procedure Destroy (Self : in out Logic_Var_Record);
   --  Release resources associated with this logic variable

   type Logic_Var_Array is array (Positive range <>) of Logic_Var;
   type Value_Array is array (Positive range <>) of Value_Type;
   --  Array of values of this variable, for convenience. To be used in other
   --  generic packages taking a formal Logic_Var package as argument.

end Langkit_Support.Adalog.Logic_Var;
