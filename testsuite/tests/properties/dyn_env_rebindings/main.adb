with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libfoolang.Analysis; use Libfoolang.Analysis;
with Libfoolang.Common;   use Libfoolang.Common;

procedure Main is
   function Load_Unit
     (Filename : String;
      Buffer   : String) return Analysis_Unit;
   --  Wrapper around ``Get_From_Buffer`` that also checks that there are no
   --  diagnostics.

   function Pretty_Type (T : Type_Decl) return Text_Type;
   --  Pretty print the given type declaration by expanding instantiations

   procedure Resolve_Aliases (U : Analysis_Unit);
   --  Print the result of resolving the type expression in the RHS of each
   --  type aliases in the given analysis unit.

   Ctx  : constant Analysis_Context := Create_Context;

   ----------------
   --  Load_Unit --
   ----------------

   function Load_Unit
     (Filename : String;
      Buffer   : String) return Analysis_Unit
   is
      U : constant Analysis_Unit := Ctx.Get_From_Buffer
        (Filename => Filename, Buffer => Buffer);
   begin
      if U.Has_Diagnostics then
         for D of U.Diagnostics loop
            Put_Line (U.Format_GNU_Diagnostic (D));
         end loop;
         raise Program_Error;
      end if;
      return U;
   end Load_Unit;

   ------------------
   --  Pretty_Type --
   ------------------

   function Pretty_Type (T : Type_Decl) return Text_Type is
      Name : constant Text_Type := T.F_Name.Text;
   begin
      if T.Kind = Foo_Generic_Struct then
         return Name & "(" & Pretty_Type
           (T.As_Generic_Struct.P_Actual_Type) & ")";
      else
         return Name;
      end if;
   end Pretty_Type;

   ----------------------
   --  Resolve_Aliases --
   ----------------------

   procedure Resolve_Aliases (U : Analysis_Unit) is
      T : Type_Expr;
   begin
      for N of U.Root.Children loop
         if N.Kind = Foo_Type_Alias then
            T := N.As_Type_Alias.F_Type_Expr;
            Put_Line (T.Image & " -> " & Image (Pretty_Type (T.P_Resolve)));
         end if;
      end loop;
   end Resolve_Aliases;

   Base : constant Analysis_Unit := Load_Unit
     (Filename => "base.txt",
      Buffer   => "def Foo(T)" & ASCII.LF
         & "def Bar(T)" & ASCII.LF
         & "def Baz" & ASCII.LF);

   A : constant Analysis_Unit := Load_Unit
     (Filename => "a.txt",
      Buffer   => "def a = Baz" & ASCII.LF
         & "def b = Bar(Baz)" & ASCII.LF
         & "def c = Foo(b)" & ASCII.LF);

   B : constant Analysis_Unit := Load_Unit
     (Filename => "b.txt",
      Buffer   => "def d = Foo(Bar(Bar(Baz)))" & ASCII.LF
         & "def e = Foo(c)" & ASCII.LF);
begin
   Put_Line ("main.adb: Running...");
   Base.Populate_Lexical_Env;

   --  First, check that everything resolves correctly
   Resolve_Aliases (A);
   Resolve_Aliases (B);

   New_Line;
   Put_Line ("Reparsing a.txt...");
   New_Line;

   --  Next, since variable `d` and `e` in b.txt refer to declarations in
   --  a.txt, check that reparsing a.txt does not affect the results. In
   --  particular, the dynamic env for the instantiation `Foo(c)` contains
   --  an entry which maps `T` to a rebound entity `Foo(b)` declared in a.txt.
   --  Thus, this checks that we don't end up with stale rebindings when
   --  reparsing a.txt but that they are correctly recomputed based on the
   --  new content of a.txt.
   A.Reparse (Buffer => "def c = Foo(Baz)");

   Resolve_Aliases (B);

   Put_Line ("main.adb: Done.");
end Main;
