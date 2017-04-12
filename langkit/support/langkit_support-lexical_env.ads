with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Vectors;

--  This package implements a scoped lexical environment data structure that
--  will then be used in AST nodes. Particularities:
--
--  - This data structure implements simple nesting via a Parent_Env link in
--    each env. If the parent is null you are at the topmost env.
--
--  - You can reference other envs, which are virtually treated like parent
--    envs too.
--
--  - You can annotate both whole environments and env elements with metadata,
--    giving more information about the elements. The consequence is that
--    metadata needs to be combinable, eg. you need to be able to create a
--    single metadata record from two metadata records.
--
--  TODO??? For the moment, everything is public, because it is not yet clear
--  what the interaction interface will be with the generated library. We might
--  want to make the type private at some point (or not).

generic
   type Element_T is private;
   type Element_Metadata is private;
   No_Element     : Element_T;
   Empty_Metadata : Element_Metadata;

   with function Combine (L, R : Element_Metadata) return Element_Metadata;

   with function Can_Reach (El, From : Element_T) return Boolean is <>;
   --  Function that will allow filtering nodes depending on the origin node of
   --  the request. In practice, this is used to implement sequential semantics
   --  for lexical envs, as-in, an element declared after another is not yet
   --  visible.

   type Getter_State_T is private;
   --  For dynamic env getters, the function pointer is allowed to have a state
   --  that carries needed data. This is preferred to a tagged type because the
   --  state has a fixed size here.
package Langkit_Support.Lexical_Env is

   ----------------------
   -- Lexical_Env Type --
   ----------------------

   type Lexical_Env_Type;
   --  Value type for lexical envs

   type Lexical_Env is access all Lexical_Env_Type;
   --  Pointer type for lexical environments. This is the type that shall be
   --  used.

   ----------------
   -- Env_Getter --
   ----------------

   type Getter_Fn_T is access
     function (Self : Getter_State_T) return Lexical_Env;

   type Env_Getter is private;
   --  Link to an environment. It can be either a simple link (just a pointer)
   --  or a dynamic link (a function that recomputes the link when needed). See
   --  tho two constructors below.

   No_Env_Getter : constant Env_Getter;

   function Simple_Env_Getter (E : Lexical_Env) return Env_Getter;
   --  Create a static Env_Getter (i.e. pointer to environment)

   function Dyn_Env_Getter
     (Fn : Getter_Fn_T; State : Getter_State_T) return Env_Getter;
   --  Create a dynamic Env_Getter (i.e. function and closure to compute an
   --  environment).

   function Get_Env (Self : Env_Getter) return Lexical_Env;
   --  Return the environment associated to the Self env getter

   type Env_Rebinding is record
      Old_Env, New_Env : Env_Getter;
   end record;

   No_Env_Rebinding : constant Env_Rebinding;

   type Env_Rebindings_Array is array (Positive range <>) of Env_Rebinding;

   type Env_Rebindings_Type (Size : Natural) is record
      Rebindings : Env_Rebindings_Array (1 .. Size);
      Ref_Count  : Natural := 1;
   end record;

   type Env_Rebindings is access all Env_Rebindings_Type;

   function Create (Bindings : Env_Rebindings_Array) return Env_Rebindings;
   --  Create a new Env_Rebindings from an array of binding pairs

   procedure Inc_Ref (Self : Env_Rebindings);
   --  Increment Self's reference count. Do nothing if Self is null.

   procedure Dec_Ref (Self : in out Env_Rebindings);
   --  Decrement Self's reference count. Also destroy it if the count drops to
   --  0. Do nothing in Self is null.

   function Combine (L, R : Env_Rebindings) return Env_Rebindings;
   --  Return a new Env_Rebindings that combines rebindings from both L and R

   function Append
     (Self : Env_Rebindings; Binding : Env_Rebinding) return Env_Rebindings;

   function Get_New_Env
     (Self : Env_Rebindings; Old_Env : Lexical_Env) return Lexical_Env;
   --  Return the new env corresponding to Old_Env in Self. Return
   --  No_Env_Getter if there is no association.

   ----------------------
   -- Env_Element Type --
   ----------------------

   type Entity_Info is record
      MD         : Element_Metadata;
      Rebindings : Env_Rebindings := null;
      Is_Null    : Boolean := False;
   end record
      with Convention => C_Pass_By_Copy;

   No_Entity_Info : constant Entity_Info := (Empty_Metadata, null, True);

   function Combine (L, R : Entity_Info) return Entity_Info;
   --  Return a new Entity_Info that combines info from both L and R

   procedure Inc_Ref (Self : Entity_Info);
   --  Increment the reference count of items in Self

   procedure Dec_Ref (Self : in out Entity_Info);
   --  Decrement the reference count of items in Self

   type Env_Element is record
      El      : Element_T;
      Info    : Entity_Info;
      Is_Null : Boolean := False;
   end record;
   --  Wrapper structure to contain both the 'real' env element that the user
   --  wanted to store, and its associated metadata.

   function Create
     (El : Element_T; MD : Element_Metadata) return Env_Element;
   --  Constructor that returns an Env_Element from an Element_T and an
   --  Element_Metadata instances.

   procedure Inc_Ref (Self : Env_Element);
   --  Increment the reference count of items in Self

   procedure Dec_Ref (Self : in out Env_Element);
   --  Decrement the reference count of items in Self

   ------------------------------------------
   --  Arrays of elements and env elements --
   ------------------------------------------

   package Env_Element_Vectors is new Langkit_Support.Vectors (Env_Element);
   --  Vectors used to store collections of environment elements, as values of
   --  a lexical environment map. We want to use vectors internally.

   type Element_Array is array (Positive range <>) of Element_T;

   subtype Env_Element_Array is Env_Element_Vectors.Elements_Array;
   --  Arrays of wrapped elements stored in the environment maps

   function Unwrap
     (Els : Env_Element_Array) return Element_Array;
   --  Get and array of unwrapped elements from an array of wrapped elements

   type Referenced_Env is record
      From_Node : Element_T;
      --  The node from which the environment has been referenced

      Env       : Lexical_Env;
      --  The referenced env
   end record;
   --  Represents a referenced env

   package Referenced_Envs_Vectors
   is new Langkit_Support.Vectors (Referenced_Env);
   --  Vectors of referenced envs, used to store referenced environments

   use Env_Element_Vectors;

   package Internal_Envs is new Ada.Containers.Hashed_Maps
     (Symbol_Type,
      Element_Type    => Env_Element_Vectors.Vector,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Internal_Map is access all Internal_Envs.Map;
   --  Internal maps of Symbols to vectors of elements

   procedure Destroy is new Ada.Unchecked_Deallocation
     (Internal_Envs.Map, Internal_Map);

   No_Refcount : constant Integer := -1;
   --  Special constant for the Ref_Count field below that means: this lexical
   --  environment is not ref-counted.

   type Lexical_Env_Type is record
      Parent : Env_Getter := No_Env_Getter;
      --  Parent environment for this env. Null by default.

      Node : Element_T;
      --  Node for which this environment was created

      Referenced_Envs : Referenced_Envs_Vectors.Vector;
      --  A list of environments referenced by this environment

      Transitive_Referenced_Envs : Referenced_Envs_Vectors.Vector;
      --  A list of environments referenced by this environment. Unlike
      --  Referenced_Envs, Transitive_Referenced_Envs are *always* explored
      --  when calling Get, whether the "Recursive" parameter is True or not.
      --  They're used to create new environments that are the concatenation
      --  of other environments.

      Env : Internal_Map := null;
      --  Map containing mappings from symbols to elements for this env
      --  instance. In the generated library, Elements will be AST nodes. If
      --  the lexical env is refcounted, then it does not own this env.

      Default_MD : Element_Metadata;
      --  Default metadata for this env instance

      Rebinding : Env_Rebinding := No_Env_Rebinding;

      Ref_Count : Integer;
      --  For ref-counted lexical environments, this contains the number of
      --  owners. It is initially set to 1. When it drops to 0, the env can be
      --  destroyed.
      --
      --  For envs owned by analysis units, it is always No_Refcount.
   end record;

   Empty_Env : constant Lexical_Env;
   --  Empty_Env is a magical lexical environment that will always be empty. We
   --  allow users to call Add on it anyway as a convenience, but this is a
   --  no-op. This makes sense as Empty_Env's purpose is to be used to
   --  represent missing scopes from erroneous trees.

   function Create
     (Parent        : Env_Getter;
      Node          : Element_T;
      Is_Refcounted : Boolean;
      Default_MD    : Element_Metadata := Empty_Metadata) return Lexical_Env;
   --  Constructor. Creates a new lexical env, given a parent, an internal data
   --  env, and a default metadata. If Is_Refcounted is true, the caller is the
   --  only owner of the result (ref-count is 1).

   procedure Add
     (Self  : Lexical_Env;
      Key   : Symbol_Type;
      Value : Element_T;
      MD    : Element_Metadata := Empty_Metadata);
   --  Add Value to the list of values for the key Key, with the metadata MD

   procedure Remove
     (Self  : Lexical_Env;
      Key   : Symbol_Type;
      Value : Element_T);
   --  Remove Value from the list of values for the key Key

   procedure Reference
     (Self            : Lexical_Env;
      To_Reference    : Lexical_Env;
      Referenced_From : Element_T := No_Element;
      Transitive      : Boolean   := False);
   --  Reference the env To_Reference from Self, making its content accessible
   --  from self. If Referenced_From is passed, for requests with an origin
   --  point (from parameter), the content will only be visible if Can_Reach
   --  (Referenced_From, From) is True. Practically this means that the origin
   --  point of the request needs to be *after* Referenced_From in the file.

   function Get
     (Self       : Lexical_Env;
      Key        : Symbol_Type;
      From       : Element_T := No_Element;
      Recursive  : Boolean := True;
      Rebindings : Env_Rebindings := null)
      return Env_Element_Array;
   --  Get the array of entities for this Key. If From is given, then
   --  elements will be filtered according to the Can_Reach primitive given
   --  as parameter for the generic package.
   --
   --  If Recursive, look for Key in all Self's parents as well, and in
   --  referenced envs. Otherwise, limit the search to Self.

   function Orphan (Self : Lexical_Env) return Lexical_Env;
   --  Return a dynamically allocated copy of Self that has no parent

   generic
      type Index_Type is range <>;
      type Lexical_Env_Array is array (Index_Type range <>) of Lexical_Env;
   function Group (Envs : Lexical_Env_Array) return Lexical_Env;
   --  Return a lexical environment that logically groups together multiple
   --  lexical environments. Note that this does not modify the input
   --  environments, however it returns a new owning reference.
   --
   --  If this array is empty, Empty_Env is returned. Note that if Envs'Length
   --  is greater than 1, the result is dynamically allocated.

   function Rebind_Env
     (Base_Env             : Lexical_Env;
      To_Rebind, Rebind_To : Env_Getter) return Lexical_Env;
   function Rebind_Env
     (Base_Env             : Lexical_Env;
      To_Rebind, Rebind_To : Lexical_Env) return Lexical_Env;
   --  Returns a new env based on Base_Env, where To_Rebind is rebound to
   --  Rebind_To.

   procedure Destroy (Self : in out Lexical_Env);
   --  Deallocate the resources allocated to the Self lexical environment. Must
   --  not be used directly for ref-counted envs.

   procedure Inc_Ref (Self : Lexical_Env);
   --  If Self is a ref-counted lexical env, increment this reference count. Do
   --  nothing otherwise.

   procedure Dec_Ref (Self : in out Lexical_Env);
   --  If Self is a ref-counted lexical env, decrement this reference count and
   --  set it to null. Also destroy it if the count drops to 0. Do nothing
   --  otherwise.

private

   type Env_Getter (Dynamic : Boolean := False) is record
      case Dynamic is
         when True =>
            Getter_State : Getter_State_T;
            Getter_Fn    : Getter_Fn_T;
         when False =>
            Env : Lexical_Env;
      end case;
   end record;

   No_Env_Getter : constant Env_Getter := (False, null);
   No_Env_Rebinding : constant Env_Rebinding := (No_Env_Getter, No_Env_Getter);

   Empty_Env_Map    : aliased Internal_Envs.Map := Internal_Envs.Empty_Map;
   Empty_Env_Record : aliased Lexical_Env_Type :=
     (Parent                     => No_Env_Getter,
      Node                       => No_Element,
      Referenced_Envs            => <>,
      Transitive_Referenced_Envs => <>,
      Env                        => Empty_Env_Map'Access,
      Default_MD                 => Empty_Metadata,
      Rebinding                  => No_Env_Rebinding,
      Ref_Count                  => No_Refcount);
   Empty_Env : constant Lexical_Env := Empty_Env_Record'Access;

end Langkit_Support.Lexical_Env;
