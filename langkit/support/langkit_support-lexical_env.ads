with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Array_Utils;
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
package Langkit_Support.Lexical_Env is

   ----------------------
   -- Env_Element Type --
   ----------------------

   type Env_Element is record
      El      : Element_T;
      MD      : Element_Metadata;
      Is_Null : Boolean := False;
   end record;
   --  Wrapper structure to contain both the 'real' env element that the user
   --  wanted to store, and its associated metadata.

   function Create
     (El : Element_T; MD : Element_Metadata) return Env_Element;
   --  Constructor that returns an Env_Element from an Element_T and an
   --  Element_Metadata instances.

   ------------------------------------------
   --  Arrays of elements and env elements --
   ------------------------------------------

   package Env_Element_Vectors is new Langkit_Support.Vectors (Env_Element);
   --  Vectors used to store collections of environment elements, as values of
   --  a lexical environment map. We want to use vectors internally.

   package Element_Arrays is new Langkit_Support.Array_Utils (Element_T);
   subtype Element_Array is Element_Arrays.Array_Type;
   --  Arrays of unwraped raw elements stored in the environment maps

   package Env_Element_Arrays renames Env_Element_Vectors.Elements_Arrays;
   subtype Env_Element_Array is Env_Element_Arrays.Array_Type;
   --  Arrays of wrapped elements stored in the environment maps

   function Unwrap
     (Els : Env_Element_Array) return Element_Array;
   --  Get and array of unwrapped elements from an array of wrapped elements

   ----------------------
   -- Lexical_Env Type --
   ----------------------

   type Lexical_Env_Type;
   --  Value type for lexical envs

   type Lexical_Env is access all Lexical_Env_Type;
   --  Pointer type for lexical environments. This is the type that shall be
   --  used.

   package Lexical_Env_Vectors is new Langkit_Support.Vectors (Lexical_Env);
   --  Vectors of lexical environments, used to store referenced environments

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
      Parent          : Lexical_Env := null;
      --  Parent environment for this env. Null by default.

      Node            : Element_T;
      --  Node for which this environment was created

      Referenced_Envs : Lexical_Env_Vectors.Vector;
      --  A list of environments referenced by this environment

      Env             : Internal_Map := null;
      --  Map containing mappings from symbols to elements for this env
      --  instance. In the generated library, Elements will be AST nodes.

      Default_MD      : Element_Metadata;
      --  Default metadata for this env instance

      Ref_Count       : Integer;
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
     (Parent        : Lexical_Env;
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

   function Get
     (Self : Lexical_Env; Key : Symbol_Type) return Element_Array;
   --  Get the array of unwrapped elements for this key

   function Get
     (Self : Lexical_Env; Key : Symbol_Type) return Env_Element_Array;
   --  Get the array of wrapped elements for this key

   function Orphan (Self : Lexical_Env) return Lexical_Env is
     (new Lexical_Env_Type'(
        Parent          => null,
        Node            => Self.Node,
        Referenced_Envs => Self.Referenced_Envs,
        Env             => Self.Env,
        Default_MD      => Self.Default_MD,
        Ref_Count       => 1));
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

   Empty_Env_Map    : aliased Internal_Envs.Map := Internal_Envs.Empty_Map;
   Empty_Env_Record : aliased Lexical_Env_Type :=
     (Parent          => null,
      Node            => No_Element,
      Referenced_Envs => <>,
      Env             => Empty_Env_Map'Access,
      Default_MD      => Empty_Metadata,
      Ref_Count       => No_Refcount);
   Empty_Env : constant Lexical_Env := Empty_Env_Record'Access;

end Langkit_Support.Lexical_Env;
