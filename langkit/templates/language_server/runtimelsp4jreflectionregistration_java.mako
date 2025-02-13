package com.adacore.lklsp;

import java.util.Arrays;
import java.util.Set;
import java.util.stream.Collectors;

import org.graalvm.nativeimage.hosted.Feature;
import org.graalvm.nativeimage.hosted.RuntimeReflection;

import org.reflections.Reflections;
import org.reflections.scanners.SubTypesScanner;

/** Native image feature to collect reflection information about LSP4J. */
class RuntimeLSP4jReflectionRegistration implements Feature {
    static private Reflections reflections =
            new Reflections("org.eclipse.lsp4j", new SubTypesScanner(false));

    static public Set<Class<?>> findAllInheriting(Class<?> c) {
        return reflections.getSubTypesOf(c)
            .stream()
            .collect(Collectors.toSet());
    }

    public void registerClass(Class c) {
        RuntimeReflection.register(c);
        for (var f: c.getDeclaredConstructors())
            RuntimeReflection.register(f);
        for (var f: c.getDeclaredMethods())
            RuntimeReflection.register(f);
        for (var f: c.getDeclaredFields())
            RuntimeReflection.register(f);
    }

    public void beforeAnalysis(BeforeAnalysisAccess access) {
        for (var c: findAllInheriting(Object.class))
            registerClass(c);
        for (var c: findAllInheriting(Enum.class))
            registerClass(c);
    }
}
