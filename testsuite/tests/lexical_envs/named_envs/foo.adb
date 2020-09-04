package body Foo is
    ref F1;
    ref F2;
    ref Bar.FB1;
    ref Foo.F1;
    ref Foo.Bar.FB1;
    ref Foo.No_Such;
    ref No_Such.F1;
    ref FP1;
    ref Foo.FP1;

    package Qux is
        type FQ1 is end;
    end;

    ref FQ1;
    ref Qux.FQ1;

    package body Qux is
        ref FQ1;
    end;
end;

package Bar.Child is
    ref B1;
end;
