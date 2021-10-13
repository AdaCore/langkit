------------------------------------------------------------------------------
--                                                                          --
--                                 Langkit                                  --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Langkit is free software; you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 3,  or (at your option)  any later ver- --
-- sion.   This software  is distributed in the hope that it will be useful --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                         --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--  This package provides a generic API so that programs can work with the
--  $.Analysis packages of all Langkit-generated libraries.
--
--  Note that it is experimental at this stage, and thus not officially
--  supported.
--
--  TODO??? Create a language-agnostic documentation for all the concepts
--  referenced here (context, unit, node, token, trivia, ...).

private with Ada.Finalization;

with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
private with Langkit_Support.Internal.Analysis;
private with Langkit_Support.Internal.Descriptor;
with Langkit_Support.Text;         use Langkit_Support.Text;

package Langkit_Support.Generic_API.Analysis is

   type Lk_Context is tagged private;
   type Lk_Unit is new Langkit_Support.Text.Text_Buffer_Ifc with private;
   type Lk_Node is tagged private;
   --  Reference types to actual context/unit/node objects. Only ``Lk_Context``
   --  and ``Lk_Unit`` are strong references (designated object lives as long
   --  as the reference exists): ``Lk_Node`` is a weak reference (designated
   --  object may be destroyed even if there are still references to it, but
   --  usage of such a stale reference is properly rejected).

   No_Lk_Context : constant Lk_Context;
   --  Special value to mean the absence of analysis context

   No_Lk_Unit : constant Lk_Unit;
   --  Special value to mean the absence of analysis unit

   No_Lk_Node : constant Lk_Node;
   --  Special value to mean the absence of analysis node

   --  TODO: add unit providers handling

   ------------------------
   -- Context operations --
   ------------------------

   function Create_Context
     (Language      : Language_Id;
      Charset       : String := "";
      File_Reader   : File_Reader_Reference := No_File_Reader_Reference;
      With_Trivia   : Boolean := True;
      Tab_Stop      : Natural := 0) return Lk_Context;
   --  Create a new analysis context.
   --
   --  ``Charset`` will be used as a default charset to decode input sources
   --  in analysis units. Please see ``GNATCOLL.Iconv`` for several supported
   --  charsets. Be careful: passing an unsupported charset is not guaranteed
   --  to raise an error here. If no charset is provided, use the language
   --  default.
   --
   --  When ``With_Trivia`` is true, the parsed analysis units will contain
   --  trivias.
   --
   --  If provided, ``File_Reader`` will be used to fetch the contents of
   --  source files instead of the default, which is to just read it from the
   --  filesystem and decode it using the regular charset rules. Note that if
   --  provided, all parsing APIs that provide a buffer are forbidden, and any
   --  use of the rewriting API with the returned context is rejected.
   --
   --  ``Tab_Stop`` is a positive number to describe the effect of tabulation
   --  characters on the column number in source files.

   function Language_For (Self : Lk_Context'Class) return Language_Id;
   --  Return the unique identifier for the Langkit-generated library that
   --  implements ``Context``.

   function Has_Unit
     (Self : Lk_Context'Class; Unit_Filename : String) return Boolean;
   --  Return whether ``Context`` contains a unit correponding to
   --  ``Unit_Filename``.

   function Get_From_File
     (Self     : Lk_Context'Class;
      Filename : String;
      Charset  : String := "";
      Reparse  : Boolean := False;
      Rule     : Grammar_Rule_Ref := No_Grammar_Rule_Ref) return Lk_Unit;
   --  Create a new analysis unit for ``Filename`` or return the existing one
   --  if any. If ``Reparse`` is true and the analysis unit already exists,
   --  reparse it from ``Filename``.
   --
   --  ``Rule`` controls which grammar rule is used to parse the unit. If
   --  ``No_Grammar_Rule_Ref``, use the default grammar rule for this language.
   --
   --  Use ``Charset`` in order to decode the source. If ``Charset`` is empty
   --  then use the context's default charset.
   --
   --  If any failure occurs, such as file opening, decoding, lexing or parsing
   --  failure, return an analysis unit anyway: errors are described as
   --  diagnostics of the returned analysis unit.
   --
   --  It is invalid to pass ``True`` to ``Reparse`` if a rewriting context is
   --  active.

   --  TODO??? Bind all other analysis context primitives

   ------------------------------
   -- Analysis unit operations --
   ------------------------------

   overriding function Get_Line
     (Self : Lk_Unit; Line_Number : Positive) return Text_Type;
   --  Return the line of text at line number ``Line_Number``

   function Context (Self : Lk_Unit'Class) return Lk_Context;
   --  Return the context that owns this unit

   function Root (Self : Lk_Unit'Class) return Lk_Node;
   --  Return the root node for this unit, or ``No_Lk_Node`` if there is
   --  none.

   --  TODO??? Bind all other analysis unit primitives

   -------------------------------
   -- Analysis nodes operations --
   -------------------------------

   function Unit (Self : Lk_Node'Class) return Lk_Unit;
   --  Return the unit that owns this node

   function Is_Null (Self : Lk_Node'Class) return Boolean;
   --  Return whether ``Node`` is a null node reference

   function "=" (Left, Right : Lk_Node'Class) return Boolean;
   --  Return whether ``Left`` and ``Right`` designate the same node.
   --
   --  Note that, unlike other operations accepting multiple generic types,
   --  checking equality is allowed for nodes coming from different languages.

   function Image (Self : Lk_Node'Class) return String;
   --  Return a short string describing ``Node``, or ``"None"`` if
   --  ``Node.Is_Null`` is true.

   type Lk_Node_Array is array (Positive range <>) of Lk_Node;

   function Parent (Self : Lk_Node'Class) return Lk_Node;
   --  Return the syntactic parent for ``Node``. Return a null node for the
   --  root one.

   function Children_Count (Self : Lk_Node'Class) return Natural;
   --  Return the number of children Node has

   procedure Get_Child
     (Self            : Lk_Node'Class;
      Index           : Positive;
      Index_In_Bounds : out Boolean;
      Result          : out Lk_Node);
   --  Return the ``Index``'th child of node, storing it into ``Result``.
   --
   --  Child indexing is 1-based. Store in ``Index_In_Bounds`` whether ``Node``
   --  had such a child: if not (i.e. ``Index`` is out-of-bounds), set
   --  ``Result`` to a null node.

   function Child (Self : Lk_Node'Class; Index : Positive) return Lk_Node;
   --  Return the ``Index``'th child of ``Node``, or null if ``Node`` has no
   --  such child.

   function Children (Self : Lk_Node'Class) return Lk_Node_Array;
   --  Return the number of children in ``Node``

   function Next_Sibling (Self : Lk_Node'Class) return Lk_Node;
   --  Return the ``Node``'s next sibling, or null if there is no such sibling

   function Previous_Sibling (Self : Lk_Node'Class) return Lk_Node;
   --  Return the ``Node``'s previous sibling, or null if there is no such
   --  sibling.

   type Visit_Status is (Into, Over, Stop);
   --  Helper type to control the node traversal process. See the ``Traverse``
   --  subprograms below.

   function Traverse
     (Self  : Lk_Node'Class;
      Visit : access function (Node : Lk_Node'Class) return Visit_Status)
      return Visit_Status;
   --  Call ``Visit`` on ``Node`` and all its children, transitively. Calls
   --  happen in prefix order (i.e. top-down and left first). The traversal is
   --  controlled as follows by the result returned by Visit:
   --
   --  ``Into``
   --     The traversal continues normally with the syntactic children of the
   --     node just processed.
   --
   --  ``Over``
   --     The children of the node just processed are skipped and excluded from
   --     the traversal, but otherwise processing continues elsewhere in the
   --     tree.
   --
   --  ``Stop``
   --     The entire traversal is immediately abandoned, and the original call
   --     to ``Traverse`` returns ``Stop``.

   procedure Traverse
     (Self  : Lk_Node'Class;
      Visit : access function (Node : Lk_Node'Class) return Visit_Status);
   --  This is the same as ``Traverse`` function except that no result is
   --  returned i.e. the ``Traverse`` function is called and the result is
   --  simply discarded.

   --  TODO??? Bind all other node primitives

private

   use Langkit_Support.Internal.Analysis;
   use Langkit_Support.Internal.Descriptor;

   type Lk_Context is new Ada.Finalization.Controlled with record
      Desc     : Language_Descriptor_Access;
      Internal : Internal_Context;
   end record;

   overriding procedure Initialize (Self : in out Lk_Context);
   overriding procedure Adjust (Self : in out Lk_Context);
   overriding procedure Finalize (Self : in out Lk_Context);

   type Lk_Unit is new Langkit_Support.Text.Text_Buffer_Ifc with record
      Internal : Internal_Unit;
      --  Note that this is a System.Address derivation, not an access type, so
      --  it is not automatically initialized. We want to consider
      --  uninitialized Lk_Unit objects as null ones, so null checks must be
      --  done on the Context compoment instead.

      Context : Lk_Context;
      --  Keep a reference to the owning context so that the context lives as
      --  long as there is at least one reference to one of its units.
   end record;

   type Lk_Node is new Ada.Finalization.Controlled with record
      Desc       : Language_Descriptor_Access;
      Internal   : Internal_Entity;
      Safety_Net : Node_Safety_Net;
   end record;

   overriding procedure Initialize (Self : in out Lk_Node);
   overriding procedure Adjust (Self : in out Lk_Node);
   overriding procedure Finalize (Self : in out Lk_Node);

   No_Lk_Context : constant Lk_Context :=
     (Ada.Finalization.Controlled with
      Desc     => null,
      Internal => No_Internal_Context);
   No_Lk_Unit    : constant Lk_Unit :=
     (Internal => No_Internal_Unit,
      Context  => (Ada.Finalization.Controlled with
                   Desc     => null,
                   Internal => No_Internal_Context));
   No_Lk_Node    : constant Lk_Node :=
     (Ada.Finalization.Controlled with
      Desc       => null,
      Internal   => No_Internal_Entity,
      Safety_Net => No_Node_Safety_Net);

end Langkit_Support.Generic_API.Analysis;
