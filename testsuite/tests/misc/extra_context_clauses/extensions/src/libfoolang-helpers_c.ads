with Support;

--  Attempts to add "with Libfoolang.Helpers_C;" in Libfoolang.Analysis will
--  create compilation errors (this is a private package, so "private with"
--  needed).

private package Libfoolang.Helpers_C is
   procedure Say_Hello is new Standard.Support.Say_Hello ("C");
end Libfoolang.Helpers_C;
