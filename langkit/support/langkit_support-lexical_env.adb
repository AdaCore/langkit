package body Langkit_Support.Lexical_Env is

   function Decorate
     (Els : Env_Element_Array; MD : Element_Metadata) return Env_Element_Array;
   --  From an array of Env_Elements, decorate every element with additional
   --  Metadata stored in MD.

   ------------
   -- Create --
   ------------

   function Create
      (El : Element_T; MD : Element_Metadata) return Env_Element
   is
   begin
      return Env_Element'(El => El, MD => MD);
   end Create;

   ------------
   -- Unwrap --
   ------------

   function Unwrap
     (Els : Env_Element_Array) return Element_Array
   is
      function Get (Self : Env_Element) return Element_T is (Self.El)
        with Inline_Always;

      function Internal_Unwrap is new Env_Element_Arrays.Map_Gen
        (Element_T, Element_Array, Get)
        with Inline_Always;
      --  Internal_Unwrap could be exposed directly, but in order to have a
      --  full subprogram profile available to the users, we wrap the generic
      --  instantiation.

   begin
      return Internal_Unwrap (Els);
   end Unwrap;

   --------------
   -- Decorate --
   --------------

   function Decorate
     (Els : Env_Element_Array; MD : Element_Metadata) return Env_Element_Array
   is
      function Decorate_Element (El : Env_Element) return Env_Element
      is
        (Env_Element'(El.El, Combine (El.MD, MD)));

      function Internal_Decorate
      is new Env_Element_Arrays.Id_Map_Gen (Decorate_Element)
        with Inline_Always;
   begin
      return Internal_Decorate (Els);
   end Decorate;

   ------------
   -- Create --
   ------------

   function Create
     (Parent     : Lexical_Env;
      Env        : Internal_Map := Internal_Envs.Empty_Map;
      Default_MD : Element_Metadata := Empty_Metadata) return Lexical_Env
   is
   begin
      return new Lexical_Env_Type'
        (Parent          => Parent,
         Referenced_Envs => <>,
         Env             => Env,
         Default_MD      => Default_MD);
   end Create;

   ---------
   -- Set --
   ---------

   procedure Add
     (Self  : Lexical_Env;
      Key   : Symbol_Type;
      Value : Element_T;
      MD    : Element_Metadata := Empty_Metadata)
   is
      use Internal_Envs;

      Env_El : constant Env_Element := Env_Element'(Value, MD);
      C      : Cursor;
      Dummy  : Boolean;
   begin
      Self.Env.Insert (Key, Env_Element_Vectors.Empty_Vector, C, Dummy);
      Append (Reference (Self.Env, C).Element.all, Env_El);
   end Add;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Lexical_Env; Key : Symbol_Type) return Env_Element_Array
   is
      use Internal_Envs;
      use Env_Element_Arrays;

      use Lexical_Env_Vectors.Elements_Arrays;

      function Get_Own_Elements
        (Self : Lexical_Env) return Env_Element_Array;
      --  Return the elements for Key contained by the internal map contained
      --  in this env.

      ----------------------
      -- Get_Own_Elements --
      ----------------------

      function Get_Own_Elements
        (Self : Lexical_Env) return Env_Element_Array
      is
         C : constant Cursor := Self.Env.Find (Key);
      begin
         return
           (if Has_Element (C)
            then Decorate
              (Env_Element_Vectors.To_Array (Element (C)), Self.Default_MD)
            else Env_Element_Arrays.Empty_Array);
      end Get_Own_Elements;

      function Get_Elements
      is new Lexical_Env_Vectors.Elements_Arrays.Flat_Map_Gen
        (Env_Element, Env_Element_Array, Get_Own_Elements);
      --  Return the concatenation of Get_Own_Elements for this env and every
      --  parent.

   begin
      if Self = null then
         return Env_Element_Arrays.Empty_Array;
      end if;

      return Get_Elements
        (Self & Lexical_Env_Vectors.To_Array (Self.Referenced_Envs))
        & Get (Self.Parent, Key);
   end Get;

   ---------
   -- Get --
   ---------

   function Get
     (Self : Lexical_Env; Key : Symbol_Type) return Element_Array is
   begin
      return Unwrap (Get (Self, Key));
   end Get;

end Langkit_Support.Lexical_Env;
