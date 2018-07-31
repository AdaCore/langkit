with Ada.Containers.Hashed_Sets;
with Ada.Text_IO; use Ada.Text_IO;

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

   CS : Context_Sets.Set;
   US : Unit_Sets.Set;

   C : constant Analysis_Context := Create;
   U1 : constant Analysis_Unit := C.Get_From_Buffer (Filename => "main1.txt",
                                                     Buffer   => "example");
   U2 : constant Analysis_Unit := C.Get_From_Buffer (Filename => "main2.txt",
                                                     Buffer   => "example");
begin
   CS.Insert (Create);
   CS.Insert (C);

   US.Insert (U1);
   US.Include (U1);
   US.Insert (U2);

   Put_Line ("Done.");
end Main;
