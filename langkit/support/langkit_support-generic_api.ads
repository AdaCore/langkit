--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  .. note:: At this stage, this package is considered as internal.
--
--  This package and its children provide generic APIs so that programs can
--  work with all Langkit-generated libraries.

limited private with Langkit_Support.Internal.Descriptor;
with Langkit_Support.Names; use Langkit_Support.Names;
with Langkit_Support.Text;  use Langkit_Support.Text;

package Langkit_Support.Generic_API is

   ------------------
   -- Language IDs --
   ------------------

   type Any_Language_Id is private;
   No_Language_Id : constant Any_Language_Id;
   subtype Language_Id is Any_Language_Id
     with Dynamic_Predicate => Language_Id /= No_Language_Id;
   --  Unique identifier for a Langkit-generated library.
   --
   --  All operations done using the generic API are done in the context of a
   --  Langkit-generated library. Because of this, all subprograms in the
   --  :ada:ref:`Langkit_Support.Generic_API` packages take, in one way or
   --  another, an argument that contains a reference to the language ID for
   --  that library: a :ada:ref:`Language_Id` argument directly, or for
   --  instance a :ada:ref:`Langkit_Support.Generic_API.Analysis.Lk_Node`
   --  argument (analysis contexts/units/nodes contain references to the
   --  language ID).
   --
   --  Note however that some subprograms accept null values for
   --  library-sensitive arguments, for instance they accept
   --  :ada:ref:`Langkit_Support.Generic_API.Analysis.No_Lk_Node` for their
   --  :ada:ref:`Langkit_Support.Generic_API.Analysis.Lk_Node` argument. The
   --  special :ada:ref:`Langkit_Support.Generic_API.Analysis.No_Lk_Node` value
   --  contains no reference to a particular Langkit-generated library. So even
   --  though such subprograms take a
   --  :ada:ref:`Langkit_Support.Generic_API.Analysis.Lk_Node` argument, they
   --  also need to take a :ada:ref:`Language_Id` argument to receive the
   --  language ID information in all cases.

   function Language_Name (Id : Language_Id) return Name_Type;
   --  Return the name of the language that the library corresponding to ``Id``
   --  analyzes.

   -------------------
   -- Grammar rules --
   -------------------

   type Grammar_Rule_Ref is private;
   --  Reference to a grammar rule for a given language

   No_Grammar_Rule_Ref : constant Grammar_Rule_Ref;
   --  Special value to express no grammar rule reference

   function Language (Rule : Grammar_Rule_Ref) return Language_Id;
   --  Return the language ID corresponding to the given grammar rule. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Rule`` is :ada:ref:`No_Grammar_Rule_Ref`.

   function Default_Grammar_Rule (Id : Language_Id) return Grammar_Rule_Ref;
   --  Return the default grammar rule for the given language

   function Grammar_Rule_Name (Rule : Grammar_Rule_Ref) return Name_Type;
   --  Return the name for the given grammar rule. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Rule`` is :ada:ref:`No_Grammar_Rule_Ref`.

   function Is_Public (Rule : Grammar_Rule_Ref) return Boolean;
   --  Return whether the given grammar rule is public. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Rule`` is :ada:ref:`No_Grammar_Rule_Ref`.

   function Grammar_Rule_Doc (Rule : Grammar_Rule_Ref) return Text_Type;
   --  Return the doc associated to this grammar rule. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Rule`` is :ada:ref:`No_Grammar_Rule_Ref`.

   type Any_Grammar_Rule_Index is new Natural;
   subtype Grammar_Rule_Index is
     Any_Grammar_Rule_Index range 1 ..  Any_Grammar_Rule_Index'Last;
   No_Grammar_Rule_Index : constant Any_Grammar_Rule_Index := 0;
   --  Language-specific index to designate a grammar rule.
   --
   --  A given languages accepts ``N`` grammar rules, so the only valid indexes
   --  for it are ``1 .. N``. The :ada:ref:`Last_Grammar_Rule` function below
   --  gives the actual ``N`` for a given language.

   function To_Index (Rule : Grammar_Rule_Ref) return Grammar_Rule_Index;
   --  Return the index of the given grammar rule. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Rule`` is :ada:ref:`No_Grammar_Rule_Ref`.

   function From_Index
     (Id : Language_Id; Rule : Grammar_Rule_Index) return Grammar_Rule_Ref;
   --  Return the grammar rule for the given language corresponding to the
   --  ``Rule`` index. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Rule`` is not a valid grammar rule index for the given language.
   --
   --% belongs-to: Grammar_Rule_Ref

   function Last_Grammar_Rule (Id : Language_Id) return Grammar_Rule_Index;
   --  Return the index of the last grammar rule for the given language

   -----------------
   -- Token kinds --
   -----------------

   type Token_Kind_Ref is private;
   --  Reference to a token kind for a given language

   No_Token_Kind_Ref : constant Token_Kind_Ref;
   --  Special value to express no token kind reference

   function Language (Kind : Token_Kind_Ref) return Language_Id;
   --  Return the language ID corresponding to the given token kind. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Kind`` is :ada:ref:`No_Token_Kind_Ref`.

   function Token_Kind_Name (Kind : Token_Kind_Ref) return Name_Type;
   --  Return the name for the given token kind. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Kind`` is :ada:ref:`No_Token_Kind_Ref`.

   function Is_Comment (Kind : Token_Kind_Ref) return Boolean;
   --  Return whether unparsing must treat the given token kind as a comment,
   --  i.e. a trivia to preserve in unparsed sources. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Kind`` is :ada:ref:`No_Token_Kind_Ref`.

   type Any_Token_Kind_Index is new Natural;
   subtype Token_Kind_Index is
     Any_Token_Kind_Index range 1 ..  Any_Token_Kind_Index'Last;
   No_Token_Kind_Index : constant Any_Token_Kind_Index := 0;
   --  Language-specific index to designate a token kind.
   --
   --  A given languages accepts ``N`` token kinds, so the only valid indexes
   --  for it are ``1 .. N``. The :ada:ref:`Last_Token_Kind` function below
   --  gives the actual ``N`` for a given language.

   function To_Index (Kind : Token_Kind_Ref) return Token_Kind_Index;
   --  Return the index of the given token kind. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Kind`` is :ada:ref:`No_Token_Kind_Ref`.

   function From_Index
     (Id : Language_Id; Kind : Token_Kind_Index) return Token_Kind_Ref;
   --  Return the token kind for the given language corresponding to the
   --  ``Kind`` index. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Kind`` is not a valid token kind index for the given language.
   --
   --% belongs-to: Token_Kind_Ref

   function Last_Token_Kind (Id : Language_Id) return Token_Kind_Index;
   --  Return the index of the last token kind for the given language
   --
   --% belongs-to: Token_Kind_Ref

   --------------------
   -- Token families --
   --------------------

   type Token_Family_Ref is private;
   --  Reference to a token family for a given language

   No_Token_Family_Ref : constant Token_Family_Ref;
   --  Special value to express no token family reference

   function Language (Family : Token_Family_Ref) return Language_Id;
   --  Return the language ID corresponding to the given token family. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Family`` is :ada:ref:`No_Token_Family_Ref`.

   function Token_Family_Name (Family : Token_Family_Ref) return Name_Type;
   --  Return the name for the given token family. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Family`` is :ada:ref:`No_Token_Family_Ref`.

   function Token_Family (Kind : Token_Kind_Ref) return Token_Family_Ref;
   --  Return the token family that owns tokens of the given ``Kind``. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Kind`` is :ada:ref:`No_Token_Kind`.

   type Any_Token_Family_Index is new Natural;
   subtype Token_Family_Index is
     Any_Token_Family_Index range 1 ..  Any_Token_Family_Index'Last;
   No_Token_Family_Index : constant Any_Token_Family_Index := 0;
   --  Language-specific index to designate a token family.
   --
   --  A given languages accepts ``N`` token families, so the only valid
   --  indexes for it are ``1 .. N``. The :ada:ref:`Last_Token_Family` function
   --  below gives the actual ``N`` for a given language.

   function To_Index (Family : Token_Family_Ref) return Token_Family_Index;
   --  Return the index of the given token family. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Family`` is :ada:ref:`No_Token_Family_Ref`.

   function From_Index
     (Id : Language_Id; Family : Token_Family_Index) return Token_Family_Ref;
   --  Return the token family for the given language corresponding to the
   --  ``Family`` index. Raise a
   --  :ada:ref:`Langkit_Support.Errors.Precondition_Failure` exception if
   --  ``Family`` is not a valid token family index for the given language.
   --
   --% belongs-to: Token_Family_Ref

   function Last_Token_Family (Id : Language_Id) return Token_Family_Index;
   --  Return the index of the last token family for the given language
   --
   --% belongs-to: Token_Family_Ref

private

   ------------------
   -- Language IDs --
   ------------------

   type Any_Language_Id is
     access constant Langkit_Support.Internal.Descriptor.Language_Descriptor;

   No_Language_Id : constant Any_Language_Id := null;

   -------------------
   -- Grammar rules --
   -------------------

   procedure Check_Grammar_Rule (Rule : Grammar_Rule_Ref);
   --  Raise a ``Precondition_Failure`` exception if ``Rule`` is
   --  ``No_Grammar_Rule_Ref``.

   procedure Check_Grammar_Rule (Id : Language_Id; Rule : Grammar_Rule_Index);
   --  If ``Rule`` is not a valid grammar rule for Id, raise a
   --  ``Precondition_Failure`` exception.

   type Grammar_Rule_Ref is record
      Id    : Any_Language_Id;
      Index : Any_Grammar_Rule_Index;
      --  Either this is ``No_Grammar_Rule_Ref``, and in that case both members
      --  should be null/zero, either ``Index`` designates a valid grammar rule
      --  for the language ``Id`` represents.
   end record;

   No_Grammar_Rule_Ref : constant Grammar_Rule_Ref := (null, 0);

   -----------------
   -- Token kinds --
   -----------------

   procedure Check_Token_Kind (Kind : Token_Kind_Ref);
   --  Raise a ``Precondition_Failure`` exception if ``Kind`` is
   --  ``No_Token_Kind_Ref``.

   procedure Check_Token_Kind (Id : Language_Id; Kind : Token_Kind_Index);
   --  If ``Kind`` is not a valid token kind for Id, raise a
   --  ``Precondition_Failure`` exception.

   type Token_Kind_Ref is record
      Id    : Any_Language_Id;
      Index : Any_Token_Kind_Index;
      --  Either this is ``No_Token_Kind_Ref``, and in that case both members
      --  should be null/zero, either ``Index`` designates a valid token kind
      --  for the language ``Id`` represents.
   end record;

   No_Token_Kind_Ref : constant Token_Kind_Ref := (null, 0);

   --------------------
   -- Token families --
   --------------------

   procedure Check_Token_Family (Family : Token_Family_Ref);
   --  Raise a ``Precondition_Failure`` exception if ``Family`` is
   --  ``No_Token_Family_Ref``.

   procedure Check_Token_Family
     (Id : Language_Id; Family : Token_Family_Index);
   --  If ``Family`` is not a valid token family for Id, raise a
   --  ``Precondition_Failure`` exception.

   type Token_Family_Ref is record
      Id    : Any_Language_Id;
      Index : Any_Token_Family_Index;
      --  Either this is ``No_Token_Family_Ref``, and in that case both members
      --  should be null/zero, either ``Index`` designates a valid token family
      --  for the language ``Id`` represents.
   end record;

   No_Token_Family_Ref : constant Token_Family_Ref := (null, 0);

end Langkit_Support.Generic_API;
