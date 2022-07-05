--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides a standardized array type (starting at index 0) along
--  with a host of functional primitives to manipulate instances of this
--  array. Functional transformations are a more natural way to express some
--  transformations (filters for example), and, thanks to Ada's secondary stack
--  based arrays, can be much faster than the dynamic vector counterpart.
--
--  For example, given the following imperative code::
--
--     Input  : Vector;
--     Output : Vector;
--
--     for El of Input loop
--        if Predicate (El) then
--           Output.Append (El);
--        end if;
--     end loop;
--
--  You could do the same thing in a functional way with this module, like so::
--
--     Input  : Array_Type;
--     Output : Array_Type := Filter (Input, Predicate'Access)
--
--  The module generally provides two ways to use a higher order primitive:
--
--  1. The first is by using the dynamic version of the primitive, that takes
--     an access to the subprogram(s) it is going to need. For filter, it will
--     be the Filter primitive.
--
--  2. The second is to use the generic version of the primitive, that will
--     take the subprograms as generic parameters. Those versions end with the
--     _Gen suffix. For filter, it will be Filter_Gen. Those versions are
--     faster, because the front-end is able to inline the parameter
--     subprograms inside the call.

generic
   type Element_Type is private;
   type Index_Type is range <>;

   type Array_Type is array (Index_Type range <>) of Element_Type;

   with function "=" (L, R : Element_Type) return Boolean is <>;
package Langkit_Support.Array_Utils is

   subtype Extended_Index is Index_Type'Base;

   Empty_Array : constant Array_Type
     (Index_Type'Succ (Index_Type'First) .. Index_Type'First)
       := (others => <>);
   --  Constant for the empty array

   type Option_Type (Has_Element : Boolean) is record
      case Has_Element is
      when True =>
         Element : Element_Type;
      when False => null;
      end case;
   end record;
   --  Basic option type, that can either contain an element or nothing

   function Create (El : Element_Type) return Option_Type;
   --  Creates an instance of an option type, containing El

   None : constant Option_Type := (Has_Element => False);
   --  Constant for the empty Option type

   function Reverse_Array (In_Array : Array_Type) return Array_Type;

   ---------
   -- Map --
   ---------

   generic
      type Out_Type is private;
      type Out_Array_Type is array (Index_Type range <>) of Out_Type;
      with function Transform (In_Element : Element_Type) return Out_Type;
   function Map_Gen (In_Array : Array_Type) return Out_Array_Type;
   --  Applies Transform on every element of In_Array, returning an array from
   --  all the transformed elements.
   --  This version takes a formal Transform parameter, and is meant for the
   --  cases where the transformed values have a different type from the input
   --  values.

   generic
      type Out_Type is private;
      type Out_Array_Type is array (Index_Type range <>) of Out_Type;
   function Map
     (In_Array  : Array_Type;
      Transform : access function (El : Element_Type) return Out_Type)
     return Out_Array_Type;
   --  Applies Transform on every element of In_Array, returning an array from
   --  all the transformed elements.
   --  This version takes an access Transform parameter, and is meant for the
   --  cases where the transformed values have a different type from the input
   --  values.

   generic
      with function Transform (In_Element : Element_Type) return Element_Type;
   function Id_Map_Gen (In_Array : Array_Type) return Array_Type;
   --  Applies Transform on every element of In_Array, returning an array from
   --  all the transformed elements.
   --  This version takes a formal Transform parameter, and is meant for the
   --  cases where the transformed values have the same type as the input
   --  values.

   function Id_Map
     (In_Array  : Array_Type;
      Transform : access function (El : Element_Type) return Element_Type)
     return Array_Type;
   --  Applies Transform on every element of In_Array, returning an array from
   --  all the transformed elements.
   --  This version takes an access Transform parameter, and is meant for the
   --  cases where the transformed values have the same type as the input
   --  values.

   ------------
   -- Filter --
   ------------

   generic
      with function Predicate (In_Element : Element_Type) return Boolean;
   function Filter_Gen (In_Array : Array_Type) return Array_Type;
   --  Returns a new array that contains every element in In_Array for which
   --  Predicate returns true.
   --  This version takes a formal Predicate parameter.

   function Filter
     (In_Array : Array_Type;
      Pred     : access function (E : Element_Type) return Boolean)
     return Array_Type;
   --  Returns a new array that contains every element in In_Array for which
   --  Predicate returns true.
   --  This version takes an access Predicate parameter.

   ---------------
   -- Partition --
   ---------------

   generic
      with function Predicate (E : Element_Type) return Boolean;
   procedure Partition_Gen
     (In_Array       : in out Array_Type;
      Last_Satisfied : out Extended_Index);
   --  Swap elements in In_Array and set Last_Satisfied so that upon return,
   --  all elements in the slice::
   --
   --    In_Array ('First .. Last_Satisfied)
   --
   --  do satisfy the given Predicate and all the others don't.

   procedure Partition
     (In_Array       : in out Array_Type;
      Predicate      : access function (E : Element_Type) return Boolean;
      Last_Satisfied : out Extended_Index);
   --  Swap elements in In_Array and set Last_Satisfied so that upon return,
   --  all elements in the slice::
   --
   --    In_Array ('First .. Last_Satisfied)
   --
   --  do satisfy the given Predicate and all the others don't.

   generic
      with function "=" (L, R : Element_Type) return Boolean;
   function Unique_Gen
     (In_Array : Array_Type) return Array_Type;
   --  Returns a new array that contains every unique element in In_Array for
   --  which Predicate returns true.
   --  This version takes a formal "=" function in case you need to redefine
   --  equality for Element_Type.

   function Unique (In_Array : Array_Type) return Array_Type;
   --  Returns a new array that contains every unique element in In_Array for
   --  which Predicate returns true.

   function Contains (In_Array : Array_Type; El : Element_Type) return Boolean;
   --  Returns True if In_Array contains El

   generic
      with function Predicate (In_Element : Element_Type) return Boolean;
   function Find_Gen
     (In_Array : Array_Type;
      Rev      : Boolean := False) return Option_Type;
   --  Return the first element in In_Array for which Predicate returns True.
   --  If Rev is True, the search will be done from the end of the array
   --  to the start.
   --  This version takes predicate as formal subprogram parameter, and returns
   --  an option type for the element.

   function Find
     (In_Array  : Array_Type;
      Predicate : access function (El : Element_Type) return Boolean;
      Rev       : Boolean := False) return Option_Type;
   --  Return the first element in In_Array for which Predicate returns True.
   --  If Rev is True, the search will be done from the end of the array
   --  to the start.
   --  This version takes predicate as an access subprogram parameter, and
   --  returns an option type for the element.

   function Find
     (In_Array  : Array_Type;
      Predicate : access function (El : Element_Type) return Boolean;
      Rev       : Boolean := False;
      Ret       : out Element_Type) return Boolean;
   --  Return the first element in In_Array for which Predicate returns True.
   --  If Rev is True, the search will be done from the end of the array
   --  to the start.
   --  This version takes predicate as an access subprogram parameter, and
   --  returns the found element as an out parameter.

   generic
      with function Predicate (In_Element : Element_Type) return Boolean;
   function Find_Gen_Or
     (In_Array         : Array_Type;
      Val_If_Not_Found : Element_Type;
      Rev              : Boolean := False) return Element_Type;
   --  Return the first element in In_Array for which Predicate returns True.
   --  If Rev is True, the search will be done from the end of the array
   --  to the start.
   --  This version takes predicate as a formal subprogram parameter, and
   --  returns Val_If_Not_Found if no element is found.

   function Find
     (In_Array         : Array_Type;
      Predicate        : access function (El : Element_Type) return Boolean;
      Val_If_Not_Found : Element_Type;
      Rev              : Boolean := False) return Element_Type;
   --  Return the first element in In_Array for which Predicate returns True.
   --  If Rev is True, the search will be done from the end of the array
   --  to the start.
   --  This version takes predicate as an access subprogram parameter, and
   --  returns Val_If_Not_Found if no element is found.

   --------------
   -- Flat_Map --
   --------------

   generic
      type F_Type is private;
      type Fn_Ret_Array_Type is array (Index_Type range <>) of F_Type;
   function Flat_Map
     (In_Array  : Array_Type;
      Transform : access function (El : Element_Type) return Fn_Ret_Array_Type)
     return Fn_Ret_Array_Type;
   --  Given a transform function, that from an element of the array, returns a
   --  new array, this function applies the transform function to every element
   --  in the array, and returns the concatenation of every resulting array.
   --  This version takes the Transform function as an access parameter, and
   --  is generic in the element type of the arrays returned by the transform
   --  function.

   function Id_Flat_Map
     (In_Array  : Array_Type;
      Transform : access function (El : Element_Type) return Array_Type)
     return Array_Type;
   --  Given a transform function, that from an element of the array, returns a
   --  new array, this function applies the transform function to every element
   --  in the array, and returns the concatenation of every resulting array.
   --  This version takes the Transform function as an access parameter, and
   --  is for the special case in which the type of the returned arrays is the
   --  same as the type of the In_Array.

   generic
      type F_Type is private;
      type Fun_Ret_Array_Type is array (Index_Type range <>) of F_Type;
      with function Transform
        (In_Element : Element_Type) return Fun_Ret_Array_Type;
   function Flat_Map_Gen (In_Array : Array_Type) return Fun_Ret_Array_Type;
   --  Given a transform function, that from an element of the array, returns a
   --  new array, this function applies the transform function to every element
   --  in the array, and returns the concatenation of every resulting array.
   --  This version takes the Transform function as a formal parameter, and
   --  is generic in the element type of the arrays returned by the transform
   --  function.

   generic
      with function Transform
        (In_Element : Element_Type) return Array_Type;
   function Id_Flat_Map_Gen (In_Array : Array_Type) return Array_Type;
   --  Given a transform function, that from an element of the array, returns a
   --  new array, this function applies the transform function to every element
   --  in the array, and returns the concatenation of every resulting array.
   --  This version takes the Transform function as a formal parameter, and
   --  is for the special case in which the type of the returned arrays is the
   --  same as the type of the In_Array.

   generic
      type Other_Index_Type is (<>);
      type Other_Array_Type is
        array (Other_Index_Type range <>) of Element_Type;
      with function "+"
        (L, R : Other_Index_Type) return Other_Index_Type is <>;
   function Copy (In_Array : Array_Type) return Other_Array_Type;
   --  Given an array type Other_Array_Type, of compatible element type but
   --  dissimilar index type, and an array of type Array_Type, return a new
   --  array of type Other_Array_Type.

private
   type Bool_Array is array (Index_Type range <>) of Boolean;
end Langkit_Support.Array_Utils;
