## vim: filetype=makoada

package body ${ada_lib_name}.Public_Converters is

   type File_Reader_Wrapper_Access is access all File_Reader_Wrapper;
   type Unit_Provider_Wrapper_Access is access all Unit_Provider_Wrapper;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Self : in out File_Reader_Wrapper) is
   begin
      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref
     (Self : in out File_Reader_Wrapper) return Boolean is
   begin
      Self.Ref_Count := Self.Ref_Count - 1;
      if Self.Ref_Count = 0 then
         return True;
      else
         return False;
      end if;
   end Dec_Ref;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : File_Reader_Wrapper;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector) is
   begin
      Self.Internal.Get.Read
        (Filename, Charset, Read_BOM, Contents, Diagnostics);
   end Read;

   -----------------------------
   -- Wrap_Public_File_Reader --
   -----------------------------

   function Wrap_Public_File_Reader
     (File_Reader : File_Reader_Reference) return Internal_File_Reader_Access
   is
      use type File_Reader_Reference;
   begin
      if File_Reader = No_File_Reader_Reference then
         return null;
      end if;

      declare
         Result : constant File_Reader_Wrapper_Access :=
            new File_Reader_Wrapper'(Ref_Count => 1, Internal => File_Reader);
      begin
         return Internal_File_Reader_Access (Result);
      end;
   end Wrap_Public_File_Reader;

   -------------
   -- Inc_Ref --
   -------------

   overriding procedure Inc_Ref (Provider : in out Unit_Provider_Wrapper) is
   begin
      Provider.Ref_Count := Provider.Ref_Count + 1;
   end Inc_Ref;

   -------------
   -- Dec_Ref --
   -------------

   overriding function Dec_Ref
     (Provider : in out Unit_Provider_Wrapper) return Boolean is
   begin
      Provider.Ref_Count := Provider.Ref_Count - 1;
      if Provider.Ref_Count = 0 then
         return True;
      else
         return False;
      end if;
   end Dec_Ref;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Unit_Provider_Wrapper;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String
   is (Provider.Internal.Get.Get_Unit_Filename (Name, Kind));

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Unit_Provider_Wrapper;
      Context     : Internal_Context;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Internal_Unit
   is
      Ctx : constant Analysis_Context := Wrap_Context (Context);
   begin
      return Unwrap_Unit (Provider.Internal.Get.Get_Unit
        (Ctx, Name, Kind, Charset, Reparse));
   end Get_Unit;

   --------------------------
   -- Wrap_Public_Provider --
   --------------------------

   function Wrap_Public_Provider
     (Provider : Unit_Provider_Reference) return Internal_Unit_Provider_Access
   is
      use type Unit_Provider_Reference;
   begin
      if Provider = No_Unit_Provider_Reference then
         return null;
      end if;

      declare
         Result : constant Unit_Provider_Wrapper_Access :=
            new Unit_Provider_Wrapper'(Ref_Count => 1, Internal => Provider);
      begin
         return Internal_Unit_Provider_Access (Result);
      end;
   end Wrap_Public_Provider;

end ${ada_lib_name}.Public_Converters;
