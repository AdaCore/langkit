package com.adacore.langkit_support;

import java.lang.foreign.Arena;
import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.SymbolLookup;
import java.lang.foreign.ValueLayout;
import java.lang.invoke.MethodHandle;

import org.graalvm.nativeimage.hosted.Feature;
import org.graalvm.nativeimage.hosted.RuntimeForeignAccess;

public class NativeTools {

    // ----- Public API -----

    /**
     * Whether the current OS is Windows.
     *
     * Having this as a static constant allows opt-out code during compilation.
     */
    public static final boolean IS_WINDOWS =
        System.getProperty("os.name")
            .toLowerCase()
            .startsWith("windows");

    /**
     * Set the value of a variable in the current environment. This method uses
     * the right native function, according to the system, under the hood to
     * perform its purpose.
     *
     * @throws Throwable if any error occurs during the native function call.
     */
    public static void setenv(String name, String value) throws Throwable {
        try (Arena arena = Arena.ofConfined()) {
            // Create native values from arguments
            MemorySegment nativeName = arena.allocateFrom(name);
            MemorySegment nativeValue = arena.allocateFrom(value);

            // Call the 'setenv' native function
            if (IS_WINDOWS) {
                int _ = (int) getPutenvHandle().invokeExact(
                    nativeName,
                    nativeValue
                );
            } else {
                int _ = (int) getSetenvHandle().invokeExact(
                    nativeName,
                    nativeValue,
                    1
                );
            }
        }
    }

    // ----- Native Image support -----

    /**
     * Class used to register foreign accesses during the native-image
     * compilation process.
     */
    public static class ForeignRegistrationFeature implements Feature {
        @Override
        public void duringSetup(DuringSetupAccess access) {
            RuntimeForeignAccess.registerForDowncall(setenvSig);
            RuntimeForeignAccess.registerForDowncall(putenvSig);
        }
    }

    // ----- Handles to native functions -----

    /** Native linker, used to access symbol of native libraries. */
    private static final Linker nativeLinker = Linker.nativeLinker();

    /** Symbol lookup entry for the standard library. */
    private static final SymbolLookup stdlibLookup =
        nativeLinker.defaultLookup();

    // --- Support for the 'setenv' native function (Linux only)
    private static final FunctionDescriptor setenvSig = FunctionDescriptor.of(
        ValueLayout.JAVA_INT,
        ValueLayout.ADDRESS,
        ValueLayout.ADDRESS,
        ValueLayout.JAVA_INT
    );
    private static MethodHandle setenvHandle = null;

    private static MethodHandle getSetenvHandle() {
        if (setenvHandle == null) {
            // Get the address of the 'setenv' functions
            MemorySegment setenvAddr = stdlibLookup.findOrThrow("setenv");

            // Then create the handle to the native function
            setenvHandle = nativeLinker.downcallHandle(
                setenvAddr,
                setenvSig
            );
        }
        return setenvHandle;
    }

    // --- Support for the '_putenv_s' native function (Windows only)
    private static final FunctionDescriptor putenvSig = FunctionDescriptor.of(
        ValueLayout.JAVA_INT,
        ValueLayout.ADDRESS,
        ValueLayout.ADDRESS
    );
    private static MethodHandle putenvHandle = null;

    private static MethodHandle getPutenvHandle() {
        if (putenvHandle == null) {
            // Get the address of the '_putenv_s' functions
            MemorySegment putenvAddr = stdlibLookup.findOrThrow("_putenv_s");

            // Then create the handle to the native function
            putenvHandle = nativeLinker.downcallHandle(
                putenvAddr,
                putenvSig
            );
        }
        return putenvHandle;
    }
}
