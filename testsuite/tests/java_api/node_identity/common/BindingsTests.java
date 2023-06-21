import java.io.File;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

import java.math.BigInteger;

import com.adacore.libfoolang.Libfoolang.*;

public final class BindingsTests {
    /**
     * Test that the Java bindings correctly implement node equality and
     * hashing, in particular in the presence of metadata.
     */
    public static void main(String[] args) {
        try(
            AnalysisContext context = AnalysisContext.create()
        ) {
            AnalysisUnit unit = context.getUnitFromBuffer(
                "example", "example"
            );
            FooNode root = unit.getRoot();

            FooNode e1 = root.pWithMd(true, false);
            FooNode e2 = root.pWithMd(true, true);
            FooNode e3 = root.pWithMd(false, true);

            System.out.println("e1 == e2? " + e1.equals(e2));
            System.out.println("e1 == e3? " + e1.equals(e3));
            System.out.println("hash(e1) == hash(e2)? " +
                               (e1.hashCode() == e2.hashCode()));
            System.out.println("hash(e1) == hash(e3)? " +
                               (e1.hashCode() == e3.hashCode()));
        }
    }

}
