package Foo is
    type F1;
    type F2 is end;

    package Bar is
        type FB1 is end;
    end;

    type F1 is end;

    ref F1;
    ref F2;
    ref Bar.FB1;
    ref Foo.F1;
    ref Foo.Bar.FB1;
    ref Foo.No_Such;
    ref No_Such.F1;
private
    type FP1 is end;
end;

package Bar is
    type B1;
end;
