with Ada.Containers; use Ada.Containers;
with Ada.Containers.Hashed_Sets;
with Ada.Text_IO;    use Ada.Text_IO;

with Libfoolang.Analysis; use Libfoolang.Analysis;

procedure Main is

   package Context_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Analysis_Context,
      Hash                => Hash,
      Equivalent_Elements => "=",
      "="                 => "=");
   package Unit_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Analysis_Unit,
      Hash                => Hash,
      Equivalent_Elements => "=",
      "="                 => "=");
   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Foo_Node,
      Hash                => Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

   CS : Context_Sets.Set;
   US : Unit_Sets.Set;
   NS : Node_Sets.Set;

   C : constant Analysis_Context := Create_Context;
   U1 : constant Analysis_Unit := C.Get_From_Buffer (Filename => "main1.txt",
                                                     Buffer   => "example");
   U2 : constant Analysis_Unit := C.Get_From_Buffer (Filename => "main2.txt",
                                                     Buffer   => "example");
begin
   CS.Insert (Create_Context);
   CS.Insert (C);

   US.Insert (U1);
   US.Include (U1);
   US.Insert (U2);
   if US.Length /= 2 then
      raise Program_Error;
   end if;

   NS.Insert (U1.Root);
   NS.Include (U1.Root);
   NS.Insert (U2.Root);
   if NS.Length /= 2 then
      raise Program_Error;
   end if;

   Put_Line ("Done.");
end Main;
