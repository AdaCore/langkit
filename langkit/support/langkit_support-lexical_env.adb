with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;

with System.Address_Image;

with Langkit_Support.Array_Utils;
with Langkit_Support.Images; use Langkit_Support.Images;

package body Langkit_Support.Lexical_Env is

   package Entity_Arrays is new Langkit_Support.Array_Utils
     (Entity, Positive, Entity_Array);

   package Referenced_Envs_Arrays is new Langkit_Support.Array_Utils
     (Referenced_Env, Positive, Referenced_Envs_Vectors.Elements_Array);

   package Internal_Map_Element_Arrays is new Langkit_Support.Array_Utils
     (Internal_Map_Element, Positive, Internal_Map_Element_Array);

   function Extract_Rebinding
     (Rebindings  : in out Env_Rebindings;
      Rebound_Env : Lexical_Env) return Lexical_Env;
   --  Look for a pair in Rebindings whose Old_Env field is "Rebound_Env".
   --
   --  If there is one, return the env it is associated to, and put the
   --  remaining set of rebindings in rebindings. Otherwise, return
   --  "Rebound_Env".
   --
   --  If Rebindings is bound to a new set of rebindings upon return, the
   --  ownership share of the old rebinding set will have been forfeited.

   function Shed_Rebindings
     (From_Env   : Lexical_Env;
      Rebindings : Env_Rebindings) return Env_Rebindings
     with Inline;
   --  Shed env rebindings that are not in the parent chain for From_Env

   function Decorate
     (Elts       : Internal_Map_Element_Array;
      MD         : Element_Metadata;
      Rebindings : Env_Rebindings) return Entity_Array;
   --  From an array of entities, decorate every element with additional
   --  Metadata stored in MD.

   procedure Check_Rebindings_Unicity (Self : Env_Rebindings);
   --  Perform a unicity check on the various rebindings in Self. In
   --  particular, check that there are no two identical Old_Env and no two
   --  identical New_Env in the set of rebindings.

   function Is_Parent (Candidate_Parent, Node : Element_T) return Boolean;
   --  Return whether Candidate_Parent is a parent of Node

   -----------------------
   -- Simple_Env_Getter --
   -----------------------

   function Simple_Env_Getter (E : Lexical_Env) return Env_Getter is
   begin
      Inc_Ref (E);
      return Env_Getter'(Dynamic       => False,
                         Is_Refcounted => E.Ref_Count /= No_Refcount,
                         Env           => E);
   end Simple_Env_Getter;

   --------------------
   -- Dyn_Env_Getter --
   --------------------

   function Dyn_Env_Getter
     (Resolver : Lexical_Env_Resolver; Node : Element_T) return Env_Getter is
   begin
      return Env_Getter'(True, Node, Resolver);
   end Dyn_Env_Getter;

   -------------
   -- Get_Env --
   -------------

   function Get_Env (Self : Env_Getter) return Lexical_Env is
   begin
      if Self.Dynamic then
         declare
            R : constant Lexical_Env_Resolver := Self.Resolver;
            E : constant Entity := (El => Self.Node, Info => No_Entity_Info);
         begin
            return R.all (E);
         end;
      else
         if Self.Env /= null then
            Inc_Ref (Self.Env);
         end if;
         return Self.Env;
      end if;
   end Get_Env;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (L, R : Env_Getter) return Boolean is
   begin
      if L.Dynamic or else R.Dynamic then
         raise Constraint_Error with "trying to compare dynamic env getters";
      else
         return L.Env = R.Env;
      end if;
   end Is_Equivalent;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Env_Getter) is
   begin
      if not Self.Dynamic and then Self.Is_Refcounted then
         Inc_Ref (Self.Env);
      end if;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Env_Getter) is
   begin
      if not Self.Dynamic and then Self.Is_Refcounted then
         Dec_Ref (Self.Env);
      end if;
   end Dec_Ref;

   -------------
   -- Combine --
   -------------

   function Combine (L, R : Env_Rebindings) return Env_Rebindings is
      Result, Cur : Env_Rebindings;
   begin
      if L = null then
         return R;
      elsif R = null then
         return L;
      end if;

      Result := L;
      Cur := R;
      while Cur /= null loop
         Result := Append (Result, Cur.Old_Env, Cur.New_Env);
         Cur := Cur.Parent;
      end loop;

      Check_Rebindings_Unicity (Result);
      return Result;
   end Combine;

   ------------
   -- Append --
   ------------

   function Append
     (Self             : Env_Rebindings;
      Old_Env, New_Env : Lexical_Env) return Env_Rebindings is
   begin
      --  Look for an existing rebinding for the result: in the Old_Env's pool
      --  if there is no parent, otherwise in the parent's children.
      if Self = null then
         if Old_Env.Rebindings_Pool /= null then
            declare
               use Env_Rebindings_Pools;
               Cur : constant Cursor := Old_Env.Rebindings_Pool.Find
                 (New_Env);
            begin
               if Cur /= Env_Rebindings_Pools.No_Element then
                  return Element (Cur);
               end if;
            end;
         end if;

      else
         for C of Self.Children loop
            if C.Old_Env = Old_Env and then C.New_Env = New_Env then
               return C;
            end if;
         end loop;
      end if;

      --  No luck? then create a new rebinding and register it where required
      declare
         Result : constant Env_Rebindings := new Env_Rebindings_Type'
           (Parent   => Self,
            Old_Env  => Old_Env,
            New_Env  => New_Env,
            Children => Env_Rebindings_Vectors.Empty_Vector);
      begin
         if Self /= null then
            Self.Children.Append (Result);
         else
            if Old_Env.Rebindings_Pool = null then
               Old_Env.Rebindings_Pool := new Env_Rebindings_Pools.Map;
            end if;
            Old_Env.Rebindings_Pool.Insert (New_Env, Result);
         end if;

         Register_Rebinding (Old_Env.Node, Result.all'Address);
         Register_Rebinding (New_Env.Node, Result.all'Address);
         Check_Rebindings_Unicity (Result);
         return Result;
      end;
   end Append;

   ----------------------
   -- Append_Rebinding --
   ----------------------

   function Append_Rebinding
     (Self    : Env_Rebindings;
      Old_Env : Lexical_Env;
      New_Env : Lexical_Env) return Env_Rebindings is
   begin
      if not Is_Rebindable (Old_Env.Node) then
         Raise_Property_Error ("Illegal lexical environment rebinding");
      end if;

      return Append (Self, Old_Env, New_Env);
   end Append_Rebinding;

   ------------
   -- Create --
   ------------

   function Create (El : Element_T; MD : Element_Metadata) return Entity is
   begin
      return Entity'
        (El      => El,
         Info    => (MD => MD, Rebindings => null));
   end Create;

   -------------------
   -- Is_Equivalent --
   -------------------

   function Is_Equivalent (L, R : Entity) return Boolean is
   begin
      if L.El /= R.El then
         return False;
      end if;

      --  All null element are equals, regardless of their entity info
      if L.El = No_Element then
         return True;
      end if;

      --  For all other cases, make sure the entity info is equivalent
      return L.Info = R.Info;
   end Is_Equivalent;

   ------------
   -- Create --
   ------------

   function Create
     (Parent        : Env_Getter;
      Node          : Element_T;
      Is_Refcounted : Boolean;
      Default_MD    : Element_Metadata := Empty_Metadata) return Lexical_Env is
   begin
      if Parent /= No_Env_Getter then
         Inc_Ref (Parent);
      end if;
      return new Lexical_Env_Type'
        (Parent                     => Parent,
         Node                       => Node,
         Referenced_Envs            => <>,
         Env                        => new Internal_Envs.Map,
         Default_MD                 => Default_MD,
         Rebindings                 => null,
         Rebindings_Pool            => null,
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

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Self            : Lexical_Env;
      Referenced_From : Element_T;
      Resolver        : Lexical_Env_Resolver;
      Creator         : Element_T;
      Transitive      : Boolean := False)
   is
      Getter : constant Env_Getter :=
         Dyn_Env_Getter (Resolver, Referenced_From);
   begin
      Referenced_Envs_Vectors.Append
        (Self.Referenced_Envs,
         Referenced_Env'(Transitive, Getter, Creator));
   end Reference;

   ---------------
   -- Reference --
   ---------------

   procedure Reference
     (Self         : Lexical_Env;
      To_Reference : Lexical_Env;
      Creator      : Element_T;
      Transitive   : Boolean := False)
   is
   begin
      Referenced_Envs_Vectors.Append
        (Self.Referenced_Envs,
         Referenced_Env'(Transitive,
                         Simple_Env_Getter (To_Reference),
                         Creator));
   end Reference;

   ---------
   -- Get --
   ---------

   function Get
     (Self       : Lexical_Env;
      Key        : Symbol_Type;
      From       : Element_T := No_Element;
      Recursive  : Boolean := True;
      Rebindings : Env_Rebindings := null;
      Filter     : access function (Ent : Entity; Env : Lexical_Env)
                                    return Boolean := null)
      return Entity_Array
   is
      Current_Rebindings : Env_Rebindings;

      use Internal_Envs;
      use Entity_Arrays;

      function Get_Refd_Elements (Self : Referenced_Env) return Entity_Array;
      --  If we can determine that From can reach Self.From_Node, return the
      --  recursive lookup of Key in Self. Otherwise, return an empty array.

      function Get_Own_Elements
        (Self       : Lexical_Env;
         Rebindings : Env_Rebindings) return Entity_Array;
      --  Return the elements for Key contained by the internal map contained
      --  in the Self environment. Decorate each element with its own metadata
      --  and with the given Rebindings.

      function Is_Filtered_Out return Boolean;
      --  Return whether, according to Filter, Self should be discarded during
      --  the lexical env lookup.

      -----------------------
      -- Get_Refd_Elements --
      -----------------------

      function Get_Refd_Elements (Self : Referenced_Env) return Entity_Array is
         Env : Lexical_Env;
      begin
         if not Recursive and then not Self.Is_Transitive then
            return Entity_Arrays.Empty_Array;
         end if;

         --  Don't follow the reference environment if either:
         --   * the node from which this reference starts cannot reach From;
         --   * the node that created this environment reference is a parent of
         --     From.

         if Self.Getter.Dynamic
           and then From /= No_Element
           and then (not Can_Reach (Self.Getter.Node, From)
                     or else Is_Parent (Self.Creator, From))
         then
            return Entity_Arrays.Empty_Array;
         end if;

         Env := Get_Env (Self.Getter);

         --  Make sure that whether the call to Get below suceeds or raises an
         --  exception, we always Dec_Ref the returned environment so we don't
         --  leak in case of error.
         begin
            declare
               Rebindings : constant Env_Rebindings :=
                 (if Self.Is_Transitive
                  then Current_Rebindings
                  else Shed_Rebindings (Env, Current_Rebindings));

               Result : constant Entity_Array :=
                 Get (Env, Key, From,
                      Recursive  => Recursive and Self.Is_Transitive,
                      Rebindings => Rebindings,
                      Filter     => Filter);
            begin
               Dec_Ref (Env);
               return Result;
            end;

         exception
            when others =>
               Dec_Ref (Env);
               raise;
         end;
      end Get_Refd_Elements;

      ----------------------
      -- Get_Own_Elements --
      ----------------------

      function Get_Own_Elements
        (Self       : Lexical_Env;
         Rebindings : Env_Rebindings) return Entity_Array
      is
         C : Cursor := Internal_Envs.No_Element;
      begin
         if Self.Env /= null then
            C := Self.Env.Find (Key);
         end if;

         return
           (if Has_Element (C)

            --  We want to reverse the returned array, so that last inserted
            --  results are returned first.
            then Decorate
              (Internal_Map_Element_Arrays.Reverse_Array
                 (Internal_Map_Element_Vectors.To_Array (Element (C))),
               Self.Default_MD,
               Rebindings)

            else Entity_Arrays.Empty_Array);
      end Get_Own_Elements;

      ---------------------
      -- Is_Filtered_Out --
      ---------------------

      function Is_Filtered_Out return Boolean is
         E : constant Entity := (El => From, Info => No_Entity_Info);
      begin
         --  If we are not provided a node and a property to call, just
         --  consider all environments.

         if From = No_Element or else Filter = null then
            return False;
         end if;

         return not Filter (E, Self);
      end Is_Filtered_Out;

      function Get_Refd_Elements is new Referenced_Envs_Arrays.Flat_Map_Gen
        (Entity, Entity_Array, Get_Refd_Elements);
      --  Likewise, but calling Get_Refd_Elements instead of Recurse

      function Can_Reach_F (El : Entity) return Boolean is
        (Can_Reach (El.El, From));

      Own_Lookup_Env    : Lexical_Env;
      Parent_Env        : Lexical_Env;
      Parent_Rebindings : Env_Rebindings;
   begin
      if Self = null then
         return Entity_Arrays.Empty_Array;
      end if;

      Parent_Env := Get_Env (Self.Parent);

      Current_Rebindings := Combine (Self.Rebindings, Rebindings);

      --  If there is an environment corresponding to Self in env rebindings,
      --  we'll get it here. We'll also shed it from the set of current
      --  rebindings.

      Own_Lookup_Env := Extract_Rebinding (Current_Rebindings, Self);

      Parent_Rebindings :=
        (if Own_Lookup_Env /= Self
         then Shed_Rebindings (Parent_Env, Current_Rebindings)
         else Current_Rebindings);

      declare
         use type Entity_Array;

         Empty : Entity_Array renames Entity_Arrays.Empty_Array;

         Filtered_Out : constant Boolean := Is_Filtered_Out;

         Own_Elts : constant Entity_Array :=
           (if Filtered_Out
            then Empty
            else Get_Own_Elements (Own_Lookup_Env, Current_Rebindings));

         Refd_Elts : constant Entity_Array :=
           (if Filtered_Out
            then Empty
            else Get_Refd_Elements
               (Referenced_Envs_Vectors.To_Array (Self.Referenced_Envs)));

         Parent_Elts : constant Entity_Array :=
           (if Recursive
            then Get (Parent_Env, Key, From,
                      Rebindings => Parent_Rebindings,
                      Filter     => Filter)
            else Empty);

         Ret : Entity_Array := Own_Elts & Refd_Elts & Parent_Elts;

         Last_That_Can_Reach : Integer := Ret'Last;
      begin
         --  Only filter if a non null value was given for the From parameter

         if From /= No_Element then
            Partition (Ret, Can_Reach_F'Access, Last_That_Can_Reach);
         end if;

         Dec_Ref (Own_Lookup_Env);

         return Ret (Ret'First .. Last_That_Can_Reach);
      end;
   end Get;

   ------------
   -- Orphan --
   ------------

   function Orphan (Self : Lexical_Env) return Lexical_Env is
   begin
      return new Lexical_Env_Type'
        (Parent          => No_Env_Getter,
         Node            => Self.Node,
         Referenced_Envs => Self.Referenced_Envs.Copy,
         Env             => Self.Env,
         Default_MD      => Self.Default_MD,
         Rebindings      => Self.Rebindings,
         Rebindings_Pool => null,
         Ref_Count       => <>);
   end Orphan;

   -----------
   -- Group --
   -----------

   function Group (Envs : Lexical_Env_Array) return Lexical_Env is
      N : Lexical_Env;
   begin
      case Envs'Length is
      when 0 =>
         return Empty_Env;
      when 1 =>
         N := Envs (Envs'First);
         Inc_Ref (N);
         return N;
      when others =>
         N := new Lexical_Env_Type'
           (Parent          => No_Env_Getter,
            Node            => No_Element,
            Referenced_Envs => <>,
            Env             => null,
            Default_MD      => Empty_Metadata,
            Rebindings      => null,
            Rebindings_Pool => null,
            Ref_Count       => <>);
         for Env of Envs loop
            Reference (N, Env, No_Element, Transitive => True);
         end loop;
         return N;
      end case;
   end Group;

   ----------------
   -- Rebind_Env --
   ----------------

   function Rebind_Env
     (Base_Env : Lexical_Env; Rebindings : Env_Rebindings) return Lexical_Env
   is
   begin
      --  If no rebindings were passed, just return the original base env

      if Rebindings = null then
         Inc_Ref (Base_Env);
         return Base_Env;
      end if;

      return N : constant Lexical_Env :=
        new Lexical_Env_Type'
          (Parent          => No_Env_Getter,
           Node            => No_Element,
           Referenced_Envs => <>,
           Env             => null,
           Default_MD      => Empty_Metadata,
           Rebindings      => Rebindings,

           --  This pool is only used on primary lexical environments, so there
           --  is no need to convey it to synthetic lexical envs.
           Rebindings_Pool => null,

           Ref_Count       => <>)
      do
         Reference (N, Base_Env, No_Element, Transitive => True);
      end return;
   end Rebind_Env;

   ----------------
   -- Rebind_Env --
   ----------------

   function Rebind_Env
     (Base_Env : Lexical_Env; E_Info : Entity_Info) return Lexical_Env is
   begin
      return Rebind_Env (Base_Env, E_Info.Rebindings);
   end Rebind_Env;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Lexical_Env) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Lexical_Env_Type, Lexical_Env);
      Primary : constant Boolean := Is_Primary (Self);
   begin
      --  Do not free the internal map for ref-counted allocated environments
      --  as all maps are owned by analysis unit owned environments.

      if Self.Ref_Count = No_Refcount then
         for Elts of Self.Env.all loop
            Internal_Map_Element_Vectors.Destroy (Elts);
         end loop;
         Destroy (Self.Env);
      end if;

      for Ref_Env of Self.Referenced_Envs loop
         declare
            Getter : Env_Getter := Ref_Env.Getter;
         begin
            Dec_Ref (Getter);
         end;
      end loop;
      Self.Referenced_Envs.Destroy;

      if Primary and then Self.Rebindings_Pool /= null then
         Destroy (Self.Rebindings_Pool);
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

   function Pop (Rebindings : Env_Rebindings) return Env_Rebindings is
     (if Rebindings = null then null else Rebindings.Parent);

   -----------------------
   -- Extract_Rebinding --
   -----------------------

   function Extract_Rebinding
     (Rebindings  : in out Env_Rebindings;
      Rebound_Env : Lexical_Env) return Lexical_Env
   is
      Return_Env : Lexical_Env := Rebound_Env;
   begin
      if Rebindings /= null then
         --  Look for a rebinding in the chain whose Old_Env is Rebound_Env.
         --  The correct behavior is to extract the *last* rebinding, because
         --  due to the stacked nature of rebindings, the user is never
         --  supposed to extract a rebinding other than the last one that was
         --  added.
         --
         --  TODO: We're still doing a full for-loop to check if this invariant
         --  is consistently respected. This means that the case where the env
         --  is not found is linear, which is probably the most common case.
         --  Ideally we would have the loop only in debug builds.

         declare
            R : Env_Rebindings := Rebindings;
         begin
            while R /= null loop
               declare
                  R_Old_Env : constant Lexical_Env := R.Old_Env;
               begin
                  if Rebound_Env = R_Old_Env then
                     Return_Env := R.New_Env;

                     --  Extracted rebinding *must* be the last one
                     pragma Assert (R = Rebindings);
                     exit;
                  end if;
               end;
               R := R.Parent;
            end loop;
         end;
      end if;

      Inc_Ref (Return_Env);

      --  If we found the rebinding to extract, create the new rebindings set
      if Return_Env /= Rebound_Env then
         Rebindings := Pop (Rebindings);
      end if;

      return Return_Env;
   end Extract_Rebinding;

   ---------------------
   -- Shed_Rebindings --
   ---------------------

   function Shed_Rebindings
     (From_Env   : Lexical_Env;
      Rebindings : Env_Rebindings) return Env_Rebindings
   is
      First_Rebindable_Parent : Lexical_Env;
      Result                  : Env_Rebindings := Rebindings;
   begin
      --  If there is no bindings, nothing to do here
      if Rebindings = null then
         return null;
      end if;

      --  Look for the first environment in From_Env's parent chain whose Node
      --  is rebindable. Use null if there is no such env.
      First_Rebindable_Parent := From_Env;
      while
         First_Rebindable_Parent /= null
         and then (First_Rebindable_Parent.Node = No_Element
                   or else not Is_Rebindable (First_Rebindable_Parent.Node))
      loop
         First_Rebindable_Parent := Get_Env (First_Rebindable_Parent.Parent);
      end loop;

      --  If there is no rebindable parent anywhere, it means we cannot have
      --  rebindings. In that case, shed them all, i.e. return null rebindings.
      if First_Rebindable_Parent = null then
         return null;
      end if;

      --  If we fond a rebindable parent, then we will shed all rebindings
      --  between the top of the rebinding stack and the corresponding
      --  rebinding.
      while Result /= null and then Result.Old_Env /= First_Rebindable_Parent
      loop
         Result := Result.Parent;
      end loop;

      return Result;
   end Shed_Rebindings;

   ---------------------
   -- Shed_Rebindings --
   ---------------------

   function Shed_Rebindings
     (E_Info : Entity_Info; Env : Lexical_Env) return Entity_Info is
   begin
      return (MD         => E_Info.MD,
              Rebindings => Shed_Rebindings (Env, E_Info.Rebindings));
   end Shed_Rebindings;

   --------------
   -- Decorate --
   --------------

   function Decorate
     (Elts       : Internal_Map_Element_Array;
      MD         : Element_Metadata;
      Rebindings : Env_Rebindings) return Entity_Array
   is
      function Create_Entity (Elt : Internal_Map_Element) return Entity;
      --  Transform an element from the environment into an entity

      function Create_Entity (Elt : Internal_Map_Element) return Entity is
         Result : constant Entity :=
           (El   => Elt.Element,
            Info => (MD         => Combine (Elt.MD, MD),
                     Rebindings => Rebindings));
      begin
         return
           (if Elt.Resolver = null
            then Result
            else Elt.Resolver.all (Result));
      end Create_Entity;

      function Internal_Decorate is new Internal_Map_Element_Arrays.Map_Gen
        (Out_Type       => Entity,
         Out_Array_Type => Entity_Array,
         Transform      => Create_Entity) with Inline;
   begin
      return Internal_Decorate (Elts);
   end Decorate;

   ------------------------------
   -- Check_Rebindings_Unicity --
   ------------------------------

   procedure Check_Rebindings_Unicity (Self : Env_Rebindings) is
      L, R : Env_Rebindings := Self;
   begin
      while L /= null loop
         R := L.Parent;
         while R /= null loop
            pragma Assert
              (L.Old_Env /= R.Old_Env and then L.New_Env /= R.New_Env);
            R := R.Parent;
         end loop;
         L := L.Parent;
      end loop;
   end Check_Rebindings_Unicity;

   ---------------
   -- Is_Parent --
   ---------------

   function Is_Parent (Candidate_Parent, Node : Element_T) return Boolean is
      N : Element_T;
   begin
      if Candidate_Parent = No_Element then
         return False;
      end if;

      N := Parent (Node);
      while N /= No_Element loop
         if N = Candidate_Parent then
            return True;
         end if;
         N := Parent (N);
      end loop;
      return False;
   end Is_Parent;

   -----------
   -- Image --
   -----------

   function Image (Self : Env_Rebindings) return Text_Type is

      function Image (Self : Lexical_Env) return Text_Type is
        (if Self.Ref_Count /= No_Refcount
         then "<synthetic>" else Element_Image (Self.Node));

      function Rebinding_Image (Self : Env_Rebindings) return Text_Type is
        (Image (Self.New_Env));

   begin
      if Self = null then
         return "<null>";
      end if;
      declare
         use Env_Rebindings_Vectors;
         Rebindings_Vector : Vector := Empty_Vector;
         Cur               : Env_Rebindings := Self;
         Buffer            : Unbounded_Wide_Wide_String;
      begin
         --  Get rebindings in reverse order: older first, most recent last
         while Cur /= null loop
            Rebindings_Vector.Append (Cur);
            Cur := Cur.Parent;
         end loop;

         Append (Buffer, "[");
         for I in reverse 1 .. Rebindings_Vector.Last_Index loop
            if I < Rebindings_Vector.Last_Index then
               Append (Buffer, ", ");
            end if;
            Append (Buffer, Rebinding_Image (Rebindings_Vector.Get (I)));
         end loop;
         Append (Buffer, "]");

         Rebindings_Vector.Destroy;
         return To_Wide_Wide_String (Buffer);
      end;
   end Image;

   -----------------
   -- Sorted_Envs --
   -----------------

   --  Those ordered maps are used to have a stable representation of internal
   --  lexical environments, which is not the case with hashed maps.

   function "<" (L, R : Symbol_Type) return Boolean
   is
     (L.all < R.all);

   package Sorted_Envs is new Ada.Containers.Ordered_Maps
     (Symbol_Type,
      Element_Type    => Internal_Map_Element_Vectors.Vector,
      "<"             => "<",
      "="             => Internal_Map_Element_Vectors."=");

   function To_Sorted_Env (Env : Internal_Envs.Map) return Sorted_Envs.Map;

   -------------------
   -- To_Sorted_Env --
   -------------------

   function To_Sorted_Env (Env : Internal_Envs.Map) return Sorted_Envs.Map is
      Ret_Env : Sorted_Envs.Map;
      use Internal_Envs;
   begin
      for El in Env.Iterate loop
         Ret_Env.Include (Key (El), Element (El));
      end loop;
      return Ret_Env;
   end To_Sorted_Env;

   ----------
   -- Dump --
   ----------

   function Lexical_Env_Image
     (Self           : Lexical_Env;
      Env_Id         : String := "";
      Parent_Env_Id  : String := "";
      Dump_Addresses : Boolean := False;
      Dump_Content   : Boolean := True) return String
   is

      Result : Unbounded_String;

      use Sorted_Envs;

      function Short_Image
        (N : Element_T) return String
      is (if N = No_Element then "<null>"
          else Image (Element_Image (N, False)));
      --  TODO??? This is slightly hackish, because we're converting a wide
      --  string back to string. But since we're using this solely for
      --  test/debug purposes, it should not matter. Still, would be good to
      --  have Text_Type everywhere at some point.

      function Image (El : Internal_Map_Element) return String is
        (Short_Image (El.Element));

      function Image is new Internal_Map_Element_Vectors.Image
        (Image);

      procedure Dump_Referenced
        (Name : String; Refs : Referenced_Envs_Vectors.Vector);

      First_Arg : Boolean := True;

      procedure New_Arg;
      procedure New_Arg is
      begin
         if First_Arg then
            First_Arg := False;
         else
            Append (Result, ", ");
         end if;
      end New_Arg;

      ---------------------
      -- Dump_Referenced --
      ---------------------

      procedure Dump_Referenced
        (Name : String; Refs : Referenced_Envs_Vectors.Vector)
      is
         Is_First : Boolean := True;
      begin
         for R of Refs loop
            declare
               Env : Lexical_Env := Get_Env (R.Getter);
            begin
               if Env /= Empty_Env then
                  if Is_First then
                     Append (Result, "    " & Name & ":" & ASCII.LF);
                     Is_First := False;
                  end if;
                  Append (Result, "      ");
                  if R.Getter.Dynamic then
                     Append (Result, Short_Image (R.Getter.Node) & ": ");
                  end if;

                  Append
                    (Result,
                     Lexical_Env_Image (Self           => Env,
                                        Dump_Addresses => Dump_Addresses,
                                        Dump_Content   => False));
                  Append (Result, ASCII.LF);
                  Dec_Ref (Env);
               end if;
            end;
         end loop;
      end Dump_Referenced;

   begin
      if Env_Id'Length /= 0 then
         Append (Result, Env_Id & " = ");
      end if;
      Append (Result, "LexEnv(");
      if Self = Empty_Env then
         New_Arg;
         Append (Result, "Empty");
      end if;
      if Self.Ref_Count /= No_Refcount then
         New_Arg;
         Append (Result, "Synthetic");
      end if;
      if Parent_Env_Id'Length > 0 then
         New_Arg;
         Append (Result,
                 "Parent="
                 & (if Self.Parent /= No_Env_Getter
                    then Parent_Env_Id else "null"));
      end if;
      if Self.Node /= No_Element then
         New_Arg;
         Append (Result, "Node=" & Image (Element_Image (Self.Node, False)));
      end if;
      if Dump_Addresses then
         New_Arg;
         Append (Result, "0x" & System.Address_Image (Self.all'Address));
      end if;
      Append (Result, ")");

      if not Dump_Content then
         return To_String (Result);
      end if;
      Append (Result, ":" & ASCII.LF);

      Dump_Referenced ("Referenced", Self.Referenced_Envs);

      if Self.Env = null then
         Append (Result, "    <null>" & ASCII.LF);
      elsif Self.Env.Is_Empty then
         Append (Result, "    <empty>" & ASCII.LF);
      else
         for El in To_Sorted_Env (Self.Env.all).Iterate loop
            Append (Result, "    ");
            Append
              (Result,
               Langkit_Support.Text.Image (Key (El).all) & ": "
               & Image (Element (El))
               & ASCII.LF);
         end loop;
      end if;

      return To_String (Result);
   end Lexical_Env_Image;

   -----------------------------------
   -- Dump_Lexical_Env_Parent_Chain --
   -----------------------------------

   function Lexical_Env_Parent_Chain
     (Env : Lexical_Env) return String
   is
      Id     : Positive := 1;
      E      : Lexical_Env := Env;
      Result : Unbounded_String;
   begin

      if E = null then
         Append (Result, "<null>" & ASCII.LF);
      end if;

      while E /= null loop
         declare
            Id_Str : constant String := '@' & Stripped_Image (Id);
         begin
            Append
              (Result,
               Lexical_Env_Image
                 (Self           => E,
                  Env_Id         => Id_Str,
                  Parent_Env_Id  => '@' & Stripped_Image (Id + 1),
                  Dump_Addresses => True));
         end;
         Id := Id + 1;
         E := Get_Env (E.Parent);
      end loop;
      return To_String (Result);
   end Lexical_Env_Parent_Chain;

   --------------------------
   -- Dump_One_Lexical_Env --
   --------------------------

   procedure Dump_One_Lexical_Env
     (Self           : Lexical_Env;
      Env_Id         : String := "";
      Parent_Env_Id  : String := "";
      Dump_Addresses : Boolean := False;
      Dump_Content   : Boolean := True)
   is
   begin
      Put_Line
        (Lexical_Env_Image
           (Self, Env_Id, Parent_Env_Id, Dump_Addresses, Dump_Content));
   end Dump_One_Lexical_Env;

   -----------------------------------
   -- Dump_Lexical_Env_Parent_Chain --
   -----------------------------------

   procedure Dump_Lexical_Env_Parent_Chain (Env : Lexical_Env) is
   begin
      Put_Line (Lexical_Env_Parent_Chain (Env));
   end Dump_Lexical_Env_Parent_Chain;

end Langkit_Support.Lexical_Env;
