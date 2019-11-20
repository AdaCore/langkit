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
   type Comparer_Fn is access function (L, R : Value_Type) return Boolean;

   type Predicate_Fn_Wrapper is new Predicate_Type with record
      Callback : Predicate_Fn;
      Name     : XString;
   end record;

   function Call (Self : Predicate_Fn_Wrapper; Val : Value_Type) return Boolean
   is (Self.Callback (Val));

   function Image (Self : Predicate_Fn_Wrapper) return String
   is (Self.Name.To_String);

   type N_Predicate_Fn_Wrapper is new N_Predicate_Type with record
      Callback : N_Predicate_Fn;
      Name     : XString;
   end record;

   function Call
     (Self : N_Predicate_Fn_Wrapper; Vs : Value_Array) return Boolean
   is (Self.Callback (Vs));

   function Image (Self : N_Predicate_Fn_Wrapper) return String
   is (Self.Name.To_String);

   type Converter_Wrapper is new Converter_Type with record
      Callback : Converter_Fn;
      Name     : XString;
   end record;

   function Convert
     (Self : Converter_Wrapper; Val : Value_Type) return Value_Type is
     (Self.Callback (Val));

   function Image (Self : Converter_Wrapper) return String
   is (Self.Name.To_String);

   type Comparer_Wrapper is new Comparer_Type with record
      Callback : Comparer_Fn;
      Name     : XString;
   end record;

   function Compare
     (Self : Comparer_Wrapper; L, R : Value_Type) return Boolean is
     (Self.Callback (L, R));

   function Image (Self : Comparer_Wrapper) return String
   is (Self.Name.To_String);

   --------------
   -- Comparer --
   --------------

   function Comparer
     (Pred : access function (L, R : Value_Type) return Boolean;
      Pred_Name : String := "Comparer") return Comparer_Type'Class
   is
   begin
      return Comparer_Wrapper'(Pred'Unrestricted_Access.all,
                               To_XString (Pred_Name));
   end Comparer;

   ---------------
   -- Converter --
   ---------------

   function Converter
     (Pred : access function (V : Value_Type) return Value_Type;
      Pred_Name : String := "Converter") return Converter_Type'Class
   is
   begin
      return Converter_Wrapper'(Pred'Unrestricted_Access.all,
                               To_XString (Pred_Name));
   end Converter;

   ------------------
   -- Stub implems --
   ------------------

   type No_Comparer_Type is new Comparer_Type with null record;
   function Compare
     (Dummy            : No_Comparer_Type;
      Dummy_L, Dummy_R : Value_Type) return Boolean
   is (False);

   type No_Converter_Type is new Converter_Type with null record;
   function Convert
     (Dummy : No_Converter_Type; Dummy_From : Value_Type) return Value_Type
   is (raise Program_Error);

   function No_Comparer return Comparer_Type'Class
   is (No_Comparer_Type'(null record));

   function No_Converter return Converter_Type'Class
   is (No_Converter_Type'(null record));

   ---------------
   -- Predicate --
   ---------------

   function Predicate
     (Pred      : access function (V : Value_Type) return Boolean;
      Pred_Name : String := "Predicate") return Predicate_Type'Class
   is
   begin
      return Predicate_Fn_Wrapper'(Pred'Unrestricted_Access.all,
                                   To_XString (Pred_Name));
   end Predicate;

   -----------------
   -- N_Predicate --
   -----------------

   function N_Predicate
     (Pred       : access function (V : Value_Array) return Boolean;
      Pred_Name  : String := "N_Predicate")
      return N_Predicate_Type'Class
   is
   begin
      return N_Predicate_Fn_Wrapper'(Pred'Unrestricted_Access.all,
                                     To_XString (Pred_Name));
   end N_Predicate;

end Langkit_Support.Adalog.Solver_Interface;
