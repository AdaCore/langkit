## vim: filetype=makoada

<%def name="decl()">
<%
   key_types = ctx.sorted_types(ctx.memoization_keys)
   value_types = ctx.sorted_types(ctx.memoization_values)

   memoized_props = sorted(ctx.memoized_properties,
                           key=lambda p: p.qualname)

   # We want discrimanted types below to be constrained, so we want
   # discriminant default values.
   default_key = key_types[0].memoization_kind
%>

type Mmz_Property is
  (${', '.join(p.memoization_enum for p in memoized_props)});
type Mmz_Key_Kind is (${', '.join(t.memoization_kind for t in key_types)});
type Mmz_Value_Kind is
  (Mmz_Evaluating, Mmz_Property_Error
   ${(''.join(', ' + t.memoization_kind for t in value_types))});

type Mmz_Key_Item (Kind : Mmz_Key_Kind := ${default_key}) is record
   case Kind is
      % for t in key_types:
         when ${t.memoization_kind} =>
            As_${t.name} : ${t.name};
      % endfor
   end case;
end record;

type Mmz_Key_Array is array (Positive range <>) of Mmz_Key_Item;
type Mmz_Key_Array_Access is access all Mmz_Key_Array;
type Mmz_Key is record
   Property : Mmz_Property;
   Items    : Mmz_Key_Array_Access;
end record;

type Mmz_Value (Kind : Mmz_Value_Kind := Mmz_Evaluating) is record
   case Kind is
      when Mmz_Evaluating | Mmz_Property_Error =>
         null;

      % for t in value_types:
         when ${t.memoization_kind} =>
            As_${t.name} : ${t.name};
      % endfor
   end case;
end record;

function Hash (Key : Mmz_Key) return Hash_Type;
function Equivalent (L, R : Mmz_Key) return Boolean;

package Memoization_Maps is new Ada.Containers.Hashed_Maps
  (Mmz_Key, Mmz_Value, Hash, Equivalent_Keys => Equivalent);

procedure Destroy (Map : in out Memoization_Maps.Map);
--  Free all resources stored in a memoization map. This includes destroying
--  ref-count shares the map owns.

</%def>

<%def name="body()">

<%
   key_types = ctx.sorted_types(ctx.memoization_keys)
   value_types = ctx.sorted_types(ctx.memoization_values)
%>

function Hash (Key : Mmz_Key_Item) return Hash_Type;
function Equivalent (L, R : Mmz_Key_Item) return Boolean;
procedure Destroy (Key : in out Mmz_Key_Array_Access);

----------------
-- Equivalent --
----------------

function Equivalent (L, R : Mmz_Key_Item) return Boolean is
begin
   if L.Kind /= R.Kind then
      return False;
   end if;

   case L.Kind is
      % for t in key_types:
         when ${t.memoization_kind} =>
            <%
               l = 'L.As_{}'.format(t.name)
               r = 'R.As_{}'.format(t.name)
            %>
            % if t.has_equivalent_function:
               return Equivalent (${l}, ${r});
            % else:
               return ${l} = ${r};
            % endif
      % endfor
   end case;
end Equivalent;

----------
-- Hash --
----------

function Hash (Key : Mmz_Key_Item) return Hash_Type is
begin
   case Key.Kind is
      % for t in key_types:
         when ${t.memoization_kind} =>
            % if t.is_ast_node:
               return Hash (${root_node_type_name} (Key.As_${t.name}));
            % else:
               return Hash (Key.As_${t.name});
            % endif
      % endfor
   end case;
end Hash;

----------
-- Hash --
----------

function Hash (Key : Mmz_Key) return Hash_Type is
   Result : Hash_Type := Mmz_Property'Pos (Key.Property);
begin
   for K of Key.Items.all loop
      Result := Combine (Result, Hash (K));
   end loop;
   return Result;
end Hash;

----------------
-- Equivalent --
----------------

function Equivalent (L, R : Mmz_Key) return Boolean is
   L_Items : Mmz_Key_Array renames L.Items.all;
   R_Items : Mmz_Key_Array renames R.Items.all;
begin
   if L.Property /= R.Property or else L_Items'Length /= R_Items'Length then
      return False;
   end if;

   for I in L_Items'Range loop
      if not Equivalent (L_Items (I), R_Items (I)) then
         return False;
      end if;
   end loop;

   return True;
end Equivalent;

-------------
-- Destroy --
-------------

procedure Destroy (Map : in out Memoization_Maps.Map) is
   use Memoization_Maps;

   --  We need keys and values to be valid when clearing the memoization map,
   --  but on the other hand we need to free keys and values as well. To
   --  achieve both goals, we first copy key and values into arrays, then we
   --  clear the map, and then we free keys/values in the arrays. Allocate both
   --  arrays on the heap to avoid stack overflow, as they can be quite big.

   Length : constant Natural := Natural (Map.Length);

   type Key_Array is array (1 .. Length) of Mmz_Key_Array_Access;
   type Key_Array_Access is access Key_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Key_Array, Key_Array_Access);

   type Value_Array is array (1 .. Length) of Mmz_Value;
   type Value_Array_Access is access Value_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Value_Array, Value_Array_Access);

   Keys   : Key_Array_Access := new Key_Array;
   Values : Value_Array_Access := new Value_Array;
   I      : Positive := 1;
begin
   for Cur in Map.Iterate loop
      Keys (I) := Key (Cur).Items;
      Values (I) := Element (Cur);
      I := I + 1;
   end loop;

   Map.Clear;

   for K_Array of Keys.all loop
      Destroy (K_Array);
   end loop;

   <% refcounted_value_types = [t for t in value_types if t.is_refcounted] %>
   % if refcounted_value_types:
      for V of Values.all loop
         case V.Kind is
            % for t in refcounted_value_types:
               when ${t.memoization_kind} =>
                  Dec_Ref (V.As_${t.name});
            % endfor

            when others => null;
         end case;
      end loop;
   % endif

   Free (Keys);
   Free (Values);
end Destroy;

-------------
-- Destroy --
-------------

procedure Destroy (Key : in out Mmz_Key_Array_Access) is
   procedure Free is new Ada.Unchecked_Deallocation
     (Mmz_Key_Array, Mmz_Key_Array_Access);
begin
   <% refcounted_key_types = [t for t in key_types
                              if t.is_refcounted] %>

   % if refcounted_key_types:
      for K of Key.all loop
         case K.Kind is
            % for t in refcounted_key_types:
               when ${t.memoization_kind} =>
                  Dec_Ref (K.As_${t.name});
            % endfor

            when others => null;
         end case;
      end loop;
   % endif
   Free (Key);
end Destroy;

</%def>
