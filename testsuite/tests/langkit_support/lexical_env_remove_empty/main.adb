--  Check that calling Remove on the empty environment works

with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with Support; use Support;

procedure Main is
   Syms    : Symbol_Table := Create_Symbol_Table;
   Foo_Sym : constant Symbol_Type := Find (Syms, "foo");
begin
   Envs.Remove (Envs.Empty_Env, Foo_Sym, 'A');
   Destroy (Syms);
end Main;
