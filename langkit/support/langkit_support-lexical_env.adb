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
     (Parent        : Lexical_Env;
      Node          : Element_T;
      Is_Refcounted : Boolean;
      Default_MD    : Element_Metadata := Empty_Metadata) return Lexical_Env
   is
   begin
      if Parent /= null then
         Inc_Ref (Parent);
      end if;
      return new Lexical_Env_Type'
        (Parent          => Parent,
         Node            => Node,
         Referenced_Envs => <>,
         Env             => new Internal_Envs.Map,
         Default_MD      => Default_MD,
         Ref_Count       => (if Is_Refcounted then 1 else No_Refcount));
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
      --  See Empty_Env's documentation

      if Self = Empty_Env then
         return;
      end if;

      Self.Env.Insert (Key, Env_Element_Vectors.Empty_Vector, C, Dummy);
      Append (Reference (Self.Env.all, C).Element.all, Env_El);
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

   -----------
   -- Group --
   -----------

   function Group (Envs : Lexical_Env_Array) return Lexical_Env is

      procedure Duplicate_Parent_Chain
        (Env         : in out Lexical_Env;
         Last_Parent : out Lexical_Env);
      --  Create a copy of the parent-based chain of lexical environments in
      --  Env. Update Env to be the inner child of this chain and Last_Parent
      --  to be the last parent in this chain.

      ----------------------------
      -- Duplicate_Parent_Chain --
      ----------------------------

      procedure Duplicate_Parent_Chain
        (Env         : in out Lexical_Env;
         Last_Parent : out Lexical_Env)
      is
         First : Boolean := True;
         E     : Lexical_Env := Env;
      begin
         Env := null;
         Last_Parent := null;
         while E /= null loop
            declare
               New_Last_Parent : constant Lexical_Env := Orphan (E);
            begin
               if First then
                  Env := New_Last_Parent;
                  First := False;
               else
                  Last_Parent.Parent := New_Last_Parent;
               end if;
               Last_Parent := New_Last_Parent;
            end;
            E := E.Parent;
         end loop;
      end Duplicate_Parent_Chain;

      Result : Lexical_Env := null;
   begin
      case Envs'Length is

         --  In the following simple cases, there is no need to create any new
         --  environment.

         when 0 =>
            return Empty_Env;
         when 1 =>
            Result := Envs (Envs'First);
            Inc_Ref (Result);
            return Result;

         when others =>

            --  Create a chain of environents (chain link is the parent link)
            --  so that:
            --
            --  * first environments in Envs are first in the returned chain;
            --  * parent environments appear after their children in the
            --    returned chain.

            for Env of reverse Envs loop
               declare
                  SE  : Lexical_Env := Env;
                  SLP : Lexical_Env;
               begin
                  Duplicate_Parent_Chain (SE, SLP);
                  SLP.Parent := Result;
                  Result := SE;
               end;
            end loop;
            return Result;
      end case;
   end Group;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Lexical_Env) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Lexical_Env_Type, Lexical_Env);
   begin
      --  Do not free the internal map for ref-counted allocated environments
      --  as all maps are owned by analysis unit owned environments.

      if Self.Ref_Count = No_Refcount then
         for Elts of Self.Env.all loop
            Env_Element_Vectors.Destroy (Elts);
         end loop;
         Lexical_Env_Vectors.Destroy (Self.Referenced_Envs);
         Destroy (Self.Env);
      end if;

      Free (Self);
   end Destroy;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Lexical_Env) is
   begin
      if Self.Ref_Count = No_Refcount then
         return;
      end if;

      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Lexical_Env) is
   begin
      if Self = null or else Self.Ref_Count = No_Refcount then
         return;
      end if;

      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         Dec_Ref (Self.Parent);
         Destroy (Self);
      end if;
      Self := null;
   end Dec_Ref;

end Langkit_Support.Lexical_Env;
