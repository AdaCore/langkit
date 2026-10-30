pragma Ada_2022;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Introspection;
with Langkit_Support.Text;   use Langkit_Support.Text;
with Libfoolang.Analysis;    use Libfoolang.Analysis;
with Libfoolang.Generic_API; use Libfoolang.Generic_API;
with Libfoolang.Generic_API.Introspection;
use Libfoolang.Generic_API.Introspection;

procedure Main is

   U  : constant Analysis_Unit :=
     Create_Context.Get_From_Buffer
       (Filename => "main.txt", Buffer => "example");
   N  : constant Foo_Node := U.Root;
   GN : constant Lk_Node := To_Generic_Node (N);

   function To_Generic_SA (S : Text_Type_Array) return Value_Ref;
   function From_Generic_SA (S : Value_Ref) return Text_Type_Array;
   --  Converters to/from generic API introspection values

   function Image (S : Text_Type_Array) return String;
   --  Return a concise human readable representation of S

   procedure Check (S1, S2 : Text_Type_Array);
   --  Print the result of P_Prop (S1, S2)

   -------------------
   -- To_Generic_SA --
   -------------------

   function To_Generic_SA (S : Text_Type_Array) return Value_Ref is
      Values : Value_Ref_Array (S'Range);
   begin
      for I in S'Range loop
         Values (I) := From_String (Self_Id, To_Text (S (I)));
      end loop;
      return Create_Array (Type_Refs.Text_Type_Array, Values);
   end To_Generic_SA;

   ---------------------
   -- From_Generic_SA --
   ---------------------

   function From_Generic_SA (S : Value_Ref) return Text_Type_Array is
   begin
      return Result : Text_Type_Array (1 .. Array_Length (S)) do
         for I in Result'Range loop
            Result (I) := To_Unbounded_Text (As_String (Array_Item (S, I)));
         end loop;
      end return;
   end From_Generic_SA;

   -----------
   -- Image --
   -----------

   function Image (S : Text_Type_Array) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '[');
      for I in S'Range loop
         if I > S'First then
            Append (Result, ", ");
         end if;
         Append (Result, Image (To_Text (S (I)), With_Quotes => True));
      end loop;
      Append (Result, ']');
      return To_String (Result);
   end Image;

   -----------
   -- Check --
   -----------

   procedure Check (S1, S2 : Text_Type_Array) is
   begin
      Put_Line ("Calling P_Prop with:");
      Put_Line ("  S1 = " & Image (S1));
      Put_Line ("  S2 = " & Image (S2));

      Put_Line ("  Regular API: " & Image (N.P_Prop (S1, S2)));

      declare
         GS1    : constant Value_Ref := To_Generic_SA (S1);
         GS2    : constant Value_Ref := To_Generic_SA (S2);
         Result : constant Value_Ref :=
           Eval_Node_Member (GN, Member_Refs.Foo_Node_P_Prop, (GS1, GS2));
      begin
         Put_Line ("  Generic API: " & Image (From_Generic_SA (Result)));
      end;
   end Check;

   Foo : constant Unbounded_Text_Type := To_Unbounded_Text ("foo");
   Bar : constant Unbounded_Text_Type := To_Unbounded_Text ("bar");
   Qux : constant Unbounded_Text_Type := To_Unbounded_Text ("qux");
begin
   Check ([], []);
   Check ([Foo, Bar], []);
   Check ([], [Foo, Bar]);
   Check ([Foo, Bar], [Qux]);
   Put_Line ("Done.");
end Main;
