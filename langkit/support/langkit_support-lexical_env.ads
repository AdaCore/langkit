with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;
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

   with function Element_Image (El : Element_T) return Text_Type;
package Langkit_Support.Lexical_Env is

   type Env_Rebindings_Type (<>) is private;
   type Env_Rebindings is access all Env_Rebindings_Type;
   --  Set of mappings from one lexical environment to another. This is used to
   --  temporarily substitute lexical environment during symbol lookup.

   --------------
   -- Entities --
   --------------

   type Entity_Info is record
      MD         : Element_Metadata;
      Rebindings : Env_Rebindings := null;
   end record
      with Convention => C;

   No_Entity_Info : constant Entity_Info := (Empty_Metadata, null);

   function Combine (L, R : Entity_Info) return Entity_Info;
   --  Return a new Entity_Info that combines info from both L and R

   procedure Inc_Ref (Self : Entity_Info);
   --  Increment the reference count of items in Self

   procedure Dec_Ref (Self : in out Entity_Info);
   --  Decrement the reference count of items in Self

   type Entity is record
      El      : Element_T;
      Info    : Entity_Info;
   end record;
   --  Wrapper structure to contain both the 'real' env element that the user
   --  wanted to store, and its associated metadata.

   function Create (El : Element_T; MD : Element_Metadata) return Entity;
   --  Constructor that returns an Entity from an Element_T and an
   --  Element_Metadata instances.

   function Is_Equivalent (L, R : Entity) return Boolean;
   --  Return whether we can consider that L and R are equivalent entities

   procedure Inc_Ref (Self : Entity);
   --  Increment the reference count of items in Self

   procedure Dec_Ref (Self : in out Entity);
   --  Decrement the reference count of items in Self

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

   type Getter_Fn_T is access function (Elt : Element_T) return Lexical_Env;

   type Env_Getter (Dynamic : Boolean := False) is record
      case Dynamic is
         when True =>
            Elt       : Element_T;
            Getter_Fn : Getter_Fn_T;
         when False =>
            Is_Refcounted : Boolean;
            --  Whether Env is ref-counted. When it's not, we can avoid calling
            --  Dec_Ref at destruction time: This is useful because at analysis
            --  unit destruction time, this may be a dangling access to an
            --  environment from another unit.

            Env           : Lexical_Env;
      end case;
   end record;
   --  Link to an environment. It can be either a simple link (just a pointer)
   --  or a dynamic link (a function that recomputes the link when needed). See
   --  tho two constructors below.

   No_Env_Getter : constant Env_Getter;

   function Simple_Env_Getter (E : Lexical_Env) return Env_Getter;
   --  Create a static Env_Getter (i.e. pointer to environment)

   function Dyn_Env_Getter
     (Fn : Getter_Fn_T; Elt : Element_T) return Env_Getter;
   --  Create a dynamic Env_Getter (i.e. function and closure to compute an
   --  environment).

   function Get_Env (Self : Env_Getter) return Lexical_Env;
   --  Return the environment associated to the Self env getter

   function Is_Equivalent (L, R : Env_Getter) return Boolean;
   --  If at least one of L and R is a dynamic env getter, raise a
   --  Constraint_Error. Otherwise, return whether the pointed environments are
   --  equal.

   procedure Inc_Ref (Self : Env_Getter);
   --  Shortcut to run Inc_Ref of the potentially embedded lexical environment

   procedure Dec_Ref (Self : in out Env_Getter);
   --  Shortcut to run Dec_Ref of the potentially embedded lexical environment

   --------------------
   -- Env_Rebindings --
   --------------------

   type Env_Rebinding is record
      Old_Env, New_Env : Env_Getter;
   end record;
   --  Mapping from one lexical environment (the old one) to another (the new
   --  one).

   No_Env_Rebinding : constant Env_Rebinding;

   procedure Inc_Ref (Self : Env_Rebinding);
   --  Shortcut to run Inc_Ref on both embedded Env_Getter values

   procedure Dec_Ref (Self : in out Env_Rebinding);
   --  Shortcut to run Dec_Ref on both embedded Env_Getter values

   type Env_Rebindings_Array is array (Positive range <>) of Env_Rebinding;

   function Is_Equivalent (L, R : Env_Rebindings) return Boolean;
   --  Return whether we can consider L and R as being the same set of env
   --  rebindings. Raise a Constraint_Error if this involves comparing
   --  synthetic environment or dynamic env getters.

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

   function Append_Rebinding
     (Self      : Env_Rebindings;
      To_Rebind : Lexical_Env;
      Rebind_To : Lexical_Env) return Env_Rebindings;

   -------------------------------------
   -- Arrays of elements and entities --
   -------------------------------------

   package Entity_Vectors is new Langkit_Support.Vectors (Entity);
   --  Vectors used to store collections of environment elements, as values of
   --  a lexical environment map. We want to use vectors internally.

   type Element_Array is array (Positive range <>) of Element_T;

   subtype Entity_Array is Entity_Vectors.Elements_Array;
   --  Arrays of wrapped elements stored in the environment maps

   -----------------------------
   -- Referenced environments --
   -----------------------------

   type Lexical_Env_Resolver is access
     function (Ref : Entity) return Lexical_Env;
   --  Callback type for the lazy referenced env resolution mechanism

   type Referenced_Env (Is_Dynamic : Boolean := False) is record
      Is_Transitive : Boolean := False;

      case Is_Dynamic is
         when True =>
            From_Node : Element_T;
            --  The node from which the environment has been referenced

            Resolver  : Lexical_Env_Resolver;
            --  A function that takes From_Node and resolves to the environment
            --  that is referenced.

         when False =>
            Is_Refcounted : Boolean;
            --  Whether Env is ref-counted. When it's not, we can avoid calling
            --  Dec_Ref at destruction time. This is useful because at analysis
            --  unit destruction time, this may be a dangling access to an
            --  environment from another unit.

            Env           : Lexical_Env;
      end case;
   end record;
   --  Represents a referenced env

   function Get_Refd_Env (Self : Referenced_Env) return Lexical_Env;

   package Referenced_Envs_Vectors is new Langkit_Support.Vectors
     (Referenced_Env);
   --  Vectors of referenced envs, used to store referenced environments

   ----------------------------------------
   -- Lexical environment representation --
   ----------------------------------------

   type Entity_Resolver is access
     function (Ref : Entity) return Entity;
   --  Callback type for the lazy entity resolution mechanism. Such functions
   --  must take a "reference" entity (e.g. a name) and return the referenced
   --  entity.

   package Lexical_Env_Vectors is new Langkit_Support.Vectors (Lexical_Env);

   type Internal_Map_Element is record
      Element : Element_T;
      --  If Resolver is null, this is the element that lexical env lookup must
      --  return. Otherwise, it is the argument to pass to Resolver in order to
      --  get the result.

      MD      : Element_Metadata;
      --  Metadata associated to Element

      Resolver : Entity_Resolver;
   end record;

   package Internal_Map_Element_Vectors is new Langkit_Support.Vectors
     (Internal_Map_Element);

   subtype Internal_Map_Element_Array is
      Internal_Map_Element_Vectors.Elements_Array;

   package Internal_Envs is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Internal_Map_Element_Vectors.Vector,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => Internal_Map_Element_Vectors."=");

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

      Env : Internal_Map := null;
      --  Map containing mappings from symbols to elements for this env
      --  instance. In the generated library, Elements will be AST nodes. If
      --  the lexical env is refcounted, then it does not own this env.

      Default_MD : Element_Metadata;
      --  Default metadata for this env instance

      Rebindings : Env_Rebindings := null;

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
     (Self     : Lexical_Env;
      Key      : Symbol_Type;
      Value    : Element_T;
      MD       : Element_Metadata := Empty_Metadata;
      Resolver : Entity_Resolver := null);
   --  Add Value to the list of values for the key Key, with the metadata MD

   procedure Remove
     (Self  : Lexical_Env;
      Key   : Symbol_Type;
      Value : Element_T);
   --  Remove Value from the list of values for the key Key

   procedure Reference
     (Self            : Lexical_Env;
      Referenced_From : Element_T;
      Resolver        : Lexical_Env_Resolver;
      Transitive      : Boolean := False);
   --  Reference the env To_Reference from Self, making its content accessible
   --  from self. For requests with an origin point (From parameter), the
   --  content will only be visible if Can_Reach (Referenced_From, From) is
   --  True. Practically this means that the origin point of the request needs
   --  to be *after* Referenced_From in the file.

   procedure Reference
     (Self         : Lexical_Env;
      To_Reference : Lexical_Env;
      Transitive   : Boolean := False);

   function Get
     (Self       : Lexical_Env;
      Key        : Symbol_Type;
      From       : Element_T := No_Element;
      Recursive  : Boolean := True;
      Rebindings : Env_Rebindings := null)
      return Entity_Array;
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
   function Rebind_Env
      (Base_Env        : Lexical_Env;
       E_Info          : Entity_Info) return Lexical_Env;
   --  Returns a new env based on Base_Env, where To_Rebind is rebound to
   --  Rebind_To.

   function Image (Self : Env_Rebindings) return Text_Type;

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

   type Env_Rebindings_Type (Size : Natural) is record
      Ref_Count : Natural := 1;
      Bindings  : Env_Rebindings_Array (1 .. Size);
   end record;

   No_Env_Getter    : constant Env_Getter := (False, False, null);
   No_Env_Rebinding : constant Env_Rebinding := (No_Env_Getter, No_Env_Getter);

   Empty_Env_Map    : aliased Internal_Envs.Map := Internal_Envs.Empty_Map;
   Empty_Env_Record : aliased Lexical_Env_Type :=
     (Parent                     => No_Env_Getter,
      Node                       => No_Element,
      Referenced_Envs            => <>,
      Env                        => Empty_Env_Map'Access,
      Default_MD                 => Empty_Metadata,
      Rebindings                 => null,
      Ref_Count                  => No_Refcount);
   Empty_Env : constant Lexical_Env := Empty_Env_Record'Access;

end Langkit_Support.Lexical_Env;
