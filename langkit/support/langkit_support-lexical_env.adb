with Langkit_Support.Array_Utils;

package body Langkit_Support.Lexical_Env is

   package Env_Element_Arrays is new Langkit_Support.Array_Utils
     (Env_Element, Positive, Env_Element_Array);
   package Internal_Map_Element_Arrays is new Langkit_Support.Array_Utils
     (Internal_Map_Element, Positive, Internal_Map_Element_Array);

   function Decorate
     (Elts       : Internal_Map_Element_Array;
      MD         : Element_Metadata;
      Rebindings : Env_Rebindings) return Env_Element_Array;
   --  From an array of Env_Elements, decorate every element with additional
   --  Metadata stored in MD.

   -----------------------
   -- Simple_Env_Getter --
   -----------------------

   function Simple_Env_Getter (E : Lexical_Env) return Env_Getter
   is
     (Env_Getter'(False, Env => E));

   --------------------
   -- Dyn_Env_Getter --
   --------------------

   function Dyn_Env_Getter
     (Fn : Getter_Fn_T; State : Getter_State_T) return Env_Getter
   is
   begin
      return Env_Getter'(True, State, Fn);
   end Dyn_Env_Getter;

   -------------
   -- Get_Env --
   -------------

   function Get_Env (Self : Env_Getter) return Lexical_Env is
   begin
      if Self.Dynamic then
         return Self.Getter_Fn (Self.Getter_State);
      else
         return Self.Env;
      end if;
   end Get_Env;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Env_Getter) is
   begin
      if not Self.Dynamic then
         Inc_Ref (Self.Env);
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Env_Getter) is
   begin
      if not Self.Dynamic then
         Dec_Ref (Self.Env);
      end if;
   end Dec_Ref;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Env_Rebinding) is
   begin
      Inc_Ref (Self.Old_Env);
      Inc_Ref (Self.New_Env);
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Env_Rebinding) is
   begin
      Dec_Ref (Self.Old_Env);
      Dec_Ref (Self.New_Env);
   end Dec_Ref;

   ------------
   -- Create --
   ------------

   function Create (Bindings : Env_Rebindings_Array) return Env_Rebindings is
      Result : constant Env_Rebindings := new Env_Rebindings_Type'
        (Size       => Bindings'Length,
         Rebindings => Bindings,
         Ref_Count  => 1);
   begin
      for R of Bindings loop
         Inc_Ref (R);
      end loop;
      return Result;
   end Create;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Env_Rebindings) is
   begin
      if Self /= null then
         Self.Ref_Count := Self.Ref_Count + 1;
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Env_Rebindings) is
      procedure Unchecked_Free
      is new Ada.Unchecked_Deallocation (Env_Rebindings_Type, Env_Rebindings);
   begin
      if Self /= null then
         Self.Ref_Count := Self.Ref_Count - 1;
         if Self.Ref_Count = 0 then
            for R of Self.Rebindings loop
               Dec_Ref (R);
            end loop;
            Unchecked_Free (Self);
         end if;
      end if;
   end Dec_Ref;

   -------------
   -- Combine --
   -------------

   function Combine (L, R : Env_Rebindings) return Env_Rebindings is
      Result : Env_Rebindings;
   begin
      if L = null and then R = null then
         return null;
      elsif L = null or else L.Size = 0 then
         Inc_Ref (R);
         return R;
      elsif R = null or else R.Size = 0 then
         Inc_Ref (L);
         return L;
      end if;

      Result := new Env_Rebindings_Type (L.Size + R.Size);
      Result.Ref_Count := 1;

      for J in 1 .. L.Size loop
         Result.Rebindings (J) := L.Rebindings (J);
         Inc_Ref (Result.Rebindings (J));
      end loop;
      for J in 1 .. R.Size loop
         Result.Rebindings (J + L.Size + 1) := R.Rebindings (J);
         Inc_Ref (Result.Rebindings (J));
      end loop;

      return Result;
   end Combine;

   ------------
   -- Append --
   ------------

   function Append
     (Self : Env_Rebindings; Binding : Env_Rebinding) return Env_Rebindings
   is
   begin
      if Binding = No_Env_Rebinding then
         Inc_Ref (Self);
         return Self;

      else
         return Create
           (if Self /= null
            then Self.Rebindings & Binding
            else (1 => Binding));
      end if;
   end Append;

   -----------------
   -- Get_New_Env --
   -----------------

   function Get_New_Env
     (Self : Env_Rebindings; Old_Env : Lexical_Env) return Lexical_Env
   is
   begin
      if Self = null then
         return Old_Env;
      end if;

      for J in 1 .. Self.Size loop
         if Old_Env = Get_Env (Self.Rebindings (J).Old_Env) then
            return Get_Env (Self.Rebindings (J).New_Env);
         end if;
      end loop;

      return Old_Env;
   end Get_New_Env;

   -------------
   -- Combine --
   -------------

   function Combine (L, R : Entity_Info) return Entity_Info is
   begin
      return (MD         => Combine (L.MD, R.MD),
              Rebindings => Combine (L.Rebindings, R.Rebindings),
              Is_Null    => False);
   end Combine;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Entity_Info) is
   begin
      Inc_Ref (Self.Rebindings);
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Entity_Info) is
   begin
      Dec_Ref (Self.Rebindings);
   end Dec_Ref;

   ------------
   -- Create --
   ------------

   function Create (El : Element_T; MD : Element_Metadata) return Env_Element
   is
   begin
      return Env_Element'
        (El      => El,
         Info    => (MD => MD, Rebindings => null, Is_Null => False),
         Is_Null => False);
   end Create;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Env_Element) is
   begin
      Inc_Ref (Self.Info.Rebindings);
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Env_Element) is
   begin
      Dec_Ref (Self.Info.Rebindings);
   end Dec_Ref;

   ------------
   -- Create --
   ------------

   function Create
     (Parent        : Env_Getter;
      Node          : Element_T;
      Is_Refcounted : Boolean;
      Default_MD    : Element_Metadata := Empty_Metadata) return Lexical_Env
   is
   begin
      if Parent /= No_Env_Getter then
         Inc_Ref (Parent);
      end if;
      return new Lexical_Env_Type'
        (Parent                     => Parent,
         Node                       => Node,
         Referenced_Envs            => <>,
         Transitive_Referenced_Envs => <>,
         Env                        => new Internal_Envs.Map,
         Default_MD                 => Default_MD,
         Rebinding                  => No_Env_Rebinding,
         Ref_Count                  => (if Is_Refcounted then 1
                                        else No_Refcount));
   end Create;

   ---------
   -- Add --
   ---------

   procedure Add
     (Self     : Lexical_Env;
      Key      : Symbol_Type;
      Value    : Element_T;
      MD       : Element_Metadata := Empty_Metadata;
      Resolver : Entity_Resolver := null)
   is
      use Internal_Envs;

      Element : constant Internal_Map_Element := (Value, MD, Resolver);
      C       : Cursor;
      Dummy   : Boolean;
   begin
      --  See Empty_Env's documentation

      if Self = Empty_Env then
         return;
      end if;

      Self.Env.Insert
        (Key, Internal_Map_Element_Vectors.Empty_Vector, C, Dummy);
      Reference (Self.Env.all, C).Element.Append (Element);
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Self  : Lexical_Env;
      Key   : Symbol_Type;
      Value : Element_T)
   is
      V : constant Internal_Envs.Reference_Type := Self.Env.Reference (Key);
   begin
      --  Get rid of element
      for I in 1 .. V.Length loop
         if V.Get (I).Element = Value then
            V.Remove_At (I);
            exit;
         end if;
      end loop;
   end Remove;

   package Referenced_Envs_Arrays is new Langkit_Support.Array_Utils
     (Referenced_Env, Positive, Referenced_Envs_Vectors.Elements_Array);

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Self            : Lexical_Env;
      To_Reference    : Lexical_Env;
      Referenced_From : Element_T := No_Element;
      Transitive      : Boolean   := False)
   is
   begin
      if Transitive then
         Referenced_Envs_Vectors.Append
           (Self.Transitive_Referenced_Envs,
            Referenced_Env'(Referenced_From, To_Reference));
      else
         Referenced_Envs_Vectors.Append
           (Self.Referenced_Envs,
            Referenced_Env'(Referenced_From, To_Reference));
      end if;
      Inc_Ref (To_Reference);
   end Reference;

   ---------
   -- Get --
   ---------

   function Get
     (Self       : Lexical_Env;
      Key        : Symbol_Type;
      From       : Element_T := No_Element;
      Recursive  : Boolean := True;
      Rebindings : Env_Rebindings := null)
      return Env_Element_Array
   is
      Current_Rebindings : Env_Rebindings;

      use Internal_Envs;
      use Env_Element_Arrays;

      use Referenced_Envs_Arrays;

      function Get_Ref_Env_Elements
        (Self : Referenced_Env) return Env_Element_Array;

      function Get_Own_Elements
        (Self : Lexical_Env) return Env_Element_Array;
      --  Return the elements for Key contained by the internal map contained
      --  in this env.

      function Get_Ref_Env_Elements
        (Self : Referenced_Env) return Env_Element_Array is
      begin

         --  If the referenced environment has an origin point, and the client
         --  passed an origin from the request, see if the environment is
         --  reachable.

         if Self.From_Node /= No_Element
           and then From /= No_Element
           and then not Can_Reach (Self.From_Node, From)
         then
            return Env_Element_Arrays.Empty_Array;
         end if;

         return Get (Self.Env, Key, From, Recursive => False,
                     Rebindings => Current_Rebindings);
      end Get_Ref_Env_Elements;

      ----------------------
      -- Get_Own_Elements --
      ----------------------

      function Get_Own_Elements
        (Self : Lexical_Env) return Env_Element_Array
      is
         C   : Cursor := Internal_Envs.No_Element;
         Env : constant Lexical_Env := Get_New_Env (Current_Rebindings, Self);
      begin
         if Env.Env /= null then
            C := Env.Env.Find (Key);
         end if;

         return
           (if Has_Element (C)

            --  We want to reverse the returned array, so that last inserted
            --  results are returned first.
            then Decorate
              (Internal_Map_Element_Arrays.Reverse_Array
                 (Internal_Map_Element_Vectors.To_Array (Element (C))),
               Env.Default_MD,
               Current_Rebindings)

            else Env_Element_Arrays.Empty_Array);
      end Get_Own_Elements;

      function Get_Refd_Elements
      is new Referenced_Envs_Arrays.Flat_Map_Gen
        (Env_Element, Env_Element_Array, Get_Ref_Env_Elements);
      --  Return the concatenation of Get_Own_Elements for this env and every
      --  parent.

      function Can_Reach_F (El : Env_Element) return Boolean is
        (Can_Reach (El.El, From));

   begin
      if Self = null then
         return Env_Element_Arrays.Empty_Array;
      end if;

      Current_Rebindings := Append (Rebindings, Self.Rebinding);

      declare
         use type Env_Element_Array;

         Parent_Env : constant Lexical_Env := Get_Env (Self.Parent);

         Own_Elts   : constant Env_Element_Array :=
            Get_Own_Elements (Self);
         Refd_Elts  : constant Env_Element_Array :=
           (if Recursive
            then Get_Refd_Elements
              (Referenced_Envs_Vectors.To_Array (Self.Referenced_Envs))
            else Env_Element_Arrays.Empty_Array);
         TRefd_Elts : constant Env_Element_Array :=
            Get_Refd_Elements
              (Referenced_Envs_Vectors.To_Array
                 (Self.Transitive_Referenced_Envs));
         Parent_Elts : constant Env_Element_Array :=
           (if Recursive
            then Get (Parent_Env, Key, Rebindings => Current_Rebindings)
            else Env_Element_Arrays.Empty_Array);

         Ret : constant Env_Element_Array :=
            Own_Elts & Refd_Elts & TRefd_Elts & Parent_Elts;
      begin
         Dec_Ref (Current_Rebindings);

         --  Only filter if a non null value was given for the From parameter
         return (if From = No_Element then Ret
                 else Env_Element_Arrays.Filter (Ret, Can_Reach_F'Access));
      end;
   end Get;

   ------------
   -- Orphan --
   ------------

   function Orphan (Self : Lexical_Env) return Lexical_Env is
   begin
      for Env of Self.Referenced_Envs loop
         Inc_Ref (Env.Env);
      end loop;

      return new Lexical_Env_Type'
        (Parent                     => No_Env_Getter,
         Node                       => Self.Node,
         Referenced_Envs            => Self.Referenced_Envs.Copy,
         Transitive_Referenced_Envs => Self.Transitive_Referenced_Envs.Copy,
         Env                        => Self.Env,
         Default_MD                 => Self.Default_MD,
         Rebinding                  => Self.Rebinding,
         Ref_Count                  => 1);
   end Orphan;

   -----------
   -- Group --
   -----------

   function Group (Envs : Lexical_Env_Array) return Lexical_Env is
      N : constant Lexical_Env :=
        new Lexical_Env_Type'
          (Parent                     => No_Env_Getter,
           Node                       => No_Element,
           Referenced_Envs            => <>,
           Transitive_Referenced_Envs => <>,
           Env                        => null,
           Default_MD                 => Empty_Metadata,
           Rebinding                  => No_Env_Rebinding,
           Ref_Count                  => 1);
   begin
      for Env of Envs loop
         Reference (N, Env, No_Element, True);
      end loop;
      return N;
   end Group;

   ----------------
   -- Rebind_Env --
   ----------------

   function Rebind_Env
     (Base_Env             : Lexical_Env;
      To_Rebind, Rebind_To : Env_Getter) return Lexical_Env
   is
      N : constant Lexical_Env :=
        new Lexical_Env_Type'
          (Parent                     => No_Env_Getter,
           Node                       => No_Element,
           Referenced_Envs            => <>,
           Transitive_Referenced_Envs => <>,
           Env                        => null,
           Default_MD                 => Empty_Metadata,
           Rebinding                  => No_Env_Rebinding,
           Ref_Count                  => 1);
   begin
      Reference (N, Base_Env, No_Element, True);
      N.Rebinding := (To_Rebind, Rebind_To);
      return N;
   end Rebind_Env;

   ----------------
   -- Rebind_Env --
   ----------------

   function Rebind_Env
     (Base_Env             : Lexical_Env;
      To_Rebind, Rebind_To : Lexical_Env) return Lexical_Env
   is
   begin
      return Rebind_Env
        (Base_Env,
         Simple_Env_Getter (To_Rebind), Simple_Env_Getter (Rebind_To));
   end Rebind_Env;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Lexical_Env) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Lexical_Env_Type, Lexical_Env);
      Refd_Env : Lexical_Env;
   begin

      --  Do not free the internal map for ref-counted allocated environments
      --  as all maps are owned by analysis unit owned environments.

      if Self.Ref_Count = No_Refcount then
         for Elts of Self.Env.all loop
            Internal_Map_Element_Vectors.Destroy (Elts);
         end loop;
         Destroy (Self.Env);
      end if;

      --  Referenced_Envs on the other hand are always owned by Self

      for Ref_Env of Self.Referenced_Envs loop
         Refd_Env := Ref_Env.Env;
         Dec_Ref (Refd_Env);
      end loop;
      Referenced_Envs_Vectors.Destroy (Self.Referenced_Envs);

      for Ref_Env of Self.Transitive_Referenced_Envs loop
         Refd_Env := Ref_Env.Env;
         Dec_Ref (Refd_Env);
      end loop;
      Referenced_Envs_Vectors.Destroy (Self.Transitive_Referenced_Envs);

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

   --------------
   -- Decorate --
   --------------

   function Decorate
     (Elts       : Internal_Map_Element_Array;
      MD         : Element_Metadata;
      Rebindings : Env_Rebindings) return Env_Element_Array
   is
      function Create_Entity (Elt : Internal_Map_Element)
         return Env_Element;
      --  Transform an element from the environment into an entity

      function Create_Entity (Elt : Internal_Map_Element) return Env_Element
      is
         Resolved : Env_Element;
         Result   : constant Env_Element :=
           (El      => Elt.Element,
            Info    => (MD         => Combine (Elt.MD, MD),
                        Rebindings => Rebindings,
                        Is_Null    => False),
            Is_Null => False);
      begin
         if Elt.Resolver = null then
            Inc_Ref (Result.Info.Rebindings);
            return Result;
         else
            Resolved := Elt.Resolver.all (Result);
            return Resolved;
         end if;
      end Create_Entity;

      function Internal_Decorate is new Internal_Map_Element_Arrays.Map_Gen
        (Out_Type       => Env_Element,
         Out_Array_Type => Env_Element_Array,
         Transform      => Create_Entity) with Inline;
   begin
      return Internal_Decorate (Elts);
   end Decorate;

end Langkit_Support.Lexical_Env;
