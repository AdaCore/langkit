--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with GNATCOLL.Strings; use GNATCOLL.Strings;

package body Langkit_Support.Adalog.Solver_Interface is

   ----------------------------------
   --  Stateless functors wrappers --
   ----------------------------------

   --  Those types are wrappers used to provide the helper constructors that
   --  allow users of the solver to pass function converters/predicates rather
   --  than functor objects, that are more cumbersome to define.

   type Predicate_Fn is access function (V : Value_Type) return Boolean;
   type Converter_Fn is access function (V : Value_Type) return Value_Type;
   type N_Predicate_Fn is access function (Vs : Value_Array) return Boolean;
   type Combiner_Fn is access function (Vs : Value_Array) return Value_Type;

   type Predicate_Fn_Wrapper is new Predicate_Type with record
      Callback : Predicate_Fn;
      Name     : XString;
   end record;

   overriding function Call
     (Self : Predicate_Fn_Wrapper; Val : Value_Type) return Boolean
   is (Self.Callback (Val));

   overriding function Image (Self : Predicate_Fn_Wrapper) return String
   is (Self.Name.To_String);

   type N_Predicate_Fn_Wrapper is new N_Predicate_Type with record
      Callback : N_Predicate_Fn;
      Name     : XString;
   end record;

   overriding function Call
     (Self : N_Predicate_Fn_Wrapper; Vs : Value_Array) return Boolean
   is (Self.Callback (Vs));

   overriding function Image (Self : N_Predicate_Fn_Wrapper) return String
   is (Self.Name.To_String);

   type Converter_Wrapper is new Converter_Type with record
      Callback : Converter_Fn;
      Name     : XString;
   end record;

   overriding function Convert
     (Self : Converter_Wrapper; Val : Value_Type) return Value_Type
   is (Self.Callback (Val));

   overriding function Image (Self : Converter_Wrapper) return String
   is (Self.Name.To_String);

   type Combiner_Wrapper is new Combiner_Type with record
      Callback : Combiner_Fn;
      Name     : XString;
   end record;

   overriding function Combine
     (Self : Combiner_Wrapper; Vs : Value_Array) return Value_Type
   is (Self.Callback (Vs));

   overriding function Image (Self : Combiner_Wrapper) return String
   is (Self.Name.To_String);

   ------------------
   -- Call_Wrapper --
   ------------------

   function Call_Wrapper
     (Self : in out Predicate_Type'Class; Val  : Value_Type) return Boolean is
   begin
      if not Self.Cache_Set or else Self.Cache_Key /= Val then
         Self.Cache_Value := Self.Call (Val);
         Self.Cache_Set := True;
         Self.Cache_Key := Val;
      end if;
      return Self.Cache_Value;
   end Call_Wrapper;

   ------------------
   -- Call_Wrapper --
   ------------------

   function Call_Wrapper
     (Self : in out N_Predicate_Type'Class;
      Vals : Logic_Vars.Value_Array) return Boolean is
   begin
      if not Self.Cache_Set or else Self.Cache_Key /= Vals then
         Self.Cache_Value := Self.Call (Vals);
         Self.Cache_Set := True;
         Self.Cache_Key := Vals;
      end if;
      return Self.Cache_Value;
   end Call_Wrapper;

   ---------------------
   -- Convert_Wrapper --
   ---------------------

   function Convert_Wrapper
     (Self : in out Converter_Type; From : Value_Type) return Value_Type
   is
   begin
      if not Self.Cache_Set or else Self.Cache_Key /= From then
         Self.Cache_Value := Converter_Type'Class (Self).Convert (From);
         Self.Cache_Set := True;
         Self.Cache_Key := From;
      end if;
      return Self.Cache_Value;
   end Convert_Wrapper;

   ---------------------
   -- Combine_Wrapper --
   ---------------------

   function Combine_Wrapper
     (Self : in out Combiner_Type'Class;
      Vals : Logic_Vars.Value_Array) return Value_Type
   is
   begin
      if not Self.Cache_Set or else Self.Cache_Key /= Vals then
         Self.Cache_Value := Self.Combine (Vals);
         Self.Cache_Set := True;
         Self.Cache_Key := Vals;
      end if;
      return Self.Cache_Value;
   end Combine_Wrapper;

   ---------------
   -- Converter --
   ---------------

   function Converter
     (Pred      : access function (V : Value_Type) return Value_Type;
      Pred_Name : String := "Converter") return Converter_Type'Class is
   begin
      return Converter_Wrapper'(Cache_Set   => False,
                                Cache_Key   => <>,
                                Cache_Value => <>,
                                Ref_Count   => 1,
                                Callback    => Pred'Unrestricted_Access.all,
                                Name        => To_XString (Pred_Name));
   end Converter;

   --------------------------
   -- Stub implementations --
   --------------------------

   type No_Converter_Type is new Converter_Type with null record;

   overriding function Convert
     (Dummy : No_Converter_Type; Dummy_From : Value_Type) return Value_Type
   is (raise Program_Error);

   function No_Converter return Converter_Type'Class
   is (No_Converter_Type'(Cache_Set => False, Ref_Count => 1, others => <>));

   ---------------------
   -- Is_No_Converter --
   ---------------------

   function Is_No_Converter (Self : Converter_Type'Class) return Boolean is
   begin
      return Self in No_Converter_Type'Class;
   end Is_No_Converter;

   ---------------
   -- Predicate --
   ---------------

   function Predicate
     (Pred      : access function (V : Value_Type) return Boolean;
      Pred_Name : String := "Predicate") return Predicate_Type'Class is
   begin
      return Predicate_Fn_Wrapper'
        (Cache_Set   => False,
         Cache_Key   => <>,
         Cache_Value => <>,
         Ref_Count   => 1,
         Callback    => Pred'Unrestricted_Access.all,
         Name        => To_XString (Pred_Name));
   end Predicate;

   -----------------
   -- N_Predicate --
   -----------------

   function N_Predicate
     (Pred      : access function (V : Value_Array) return Boolean;
      Arity     : Positive;
      Pred_Name : String := "N_Predicate") return N_Predicate_Type'Class is
   begin
      return N_Predicate_Fn_Wrapper'
        (N           => Arity,
         Cache_Set   => False,
         Cache_Key   => <>,
         Cache_Value => <>,
         Ref_Count   => 1,
         Callback    => Pred'Unrestricted_Access.all,
         Name        => To_XString (Pred_Name));
   end N_Predicate;

   --------------
   -- Combiner --
   --------------

   function Combiner
     (Comb      : access function (V : Value_Array) return Value_Type;
      Arity     : Positive;
      Comb_Name : String := "Combiner") return Combiner_Type'Class
   is
   begin
      return Combiner_Wrapper'
        (N           => Arity,
         Cache_Set   => False,
         Cache_Key   => <>,
         Cache_Value => <>,
         Ref_Count   => 1,
         Callback    => Comb'Unrestricted_Access.all,
         Name        => To_XString (Comb_Name));
   end Combiner;

end Langkit_Support.Adalog.Solver_Interface;
