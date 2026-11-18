package com.bencodez.votingplugin.tests.fabric;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BooleanSupplier;
import java.util.function.Function;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class FabricBootstrapTest {
    private static final String PACKAGE = "com.bencodez.votingplugin.fabric.";

    @TempDir Path configRoot;

    @Test
    void initializesDefaultsAndPreservesExistingConfiguration() throws Throwable {
        Object defaults = invoke(null, "FabricConfiguration", "load", Path.class, configRoot);
        assertEquals(true, invoke(defaults, "FabricConfiguration", "logLifecycle", null, null));
        Path file = configRoot.resolve("votingplugin/fabric.properties");
        assertTrue(Files.readString(file).contains("logLifecycle=true"));
        String existing = "# operator setting\nlogLifecycle=false\n";
        Files.writeString(file, existing);
        Object configuration = invoke(null, "FabricConfiguration", "load", Path.class, configRoot);
        assertEquals(false, invoke(configuration, "FabricConfiguration", "logLifecycle", null, null));
        assertEquals(existing, Files.readString(file));
    }

    @Test
    void rejectsInvalidAndLinkedConfigurationWithoutOverwritingIt() throws IOException {
        Path directory = Files.createDirectory(configRoot.resolve("votingplugin"));
        Path file = directory.resolve("fabric.properties");
        Files.writeString(file, "logLifecycle=invalid\n");
        assertThrows(IOException.class, () -> invoke(null, "FabricConfiguration", "load", Path.class, configRoot));
        Files.delete(file);
        Path target = Files.writeString(configRoot.resolve("outside.properties"), "logLifecycle=false\n");
        Files.createSymbolicLink(file, target);
        assertThrows(IOException.class, () -> invoke(null, "FabricConfiguration", "load", Path.class, configRoot));
        assertEquals("logLifecycle=false\n", Files.readString(target));
    }

    @Test
    void dispatchesIdentityLookupOnOwnerAndRejectsUnfinishedWorkOnStop() throws Throwable {
        Thread owner = Thread.currentThread();
        UUID player = UUID.randomUUID();
        var identityConstructor = Class.forName(PACKAGE + "FabricServerRuntime$PlayerIdentity")
                .getDeclaredConstructor(UUID.class, String.class);
        identityConstructor.setAccessible(true);
        Object identity = identityConstructor.newInstance(player, "ExamplePlayer");
        var constructor = Class.forName(PACKAGE + "FabricServerRuntime")
                .getDeclaredConstructor(BooleanSupplier.class, Function.class);
        constructor.setAccessible(true);
        Object runtime = constructor.newInstance((BooleanSupplier) () -> Thread.currentThread() == owner,
                (Function<UUID, Optional<?>>) uuid -> {
                    assertSame(owner, Thread.currentThread());
                    return player.equals(uuid) ? Optional.of(identity) : Optional.empty();
                });

        var submitted = new AtomicReference<CompletableFuture<?>>();
        var failure = new AtomicReference<Throwable>();
        Thread worker = Thread.ofVirtual().start(() -> {
            try {
                submitted.set(lookup(runtime, player));
                assertThrows(IllegalStateException.class,
                        () -> invoke(runtime, "FabricServerRuntime", "tick", null, null));
            } catch (Throwable error) {
                failure.set(error);
            }
        });
        worker.join();
        assertNull(failure.get());
        assertFalse(submitted.get().isDone());
        var offline = lookup(runtime, UUID.randomUUID());
        invoke(runtime, "FabricServerRuntime", "tick", null, null);
        assertEquals(Optional.of(identity), submitted.get().join());
        assertEquals(Optional.empty(), offline.join());

        var pending = lookup(runtime, player);
        invoke(runtime, "FabricServerRuntime", "close", null, null);
        assertInstanceOf(RejectedExecutionException.class,
                assertThrows(CompletionException.class, pending::join).getCause());
        assertInstanceOf(RejectedExecutionException.class,
                assertThrows(CompletionException.class, () -> lookup(runtime, player).join()).getCause());
    }

    private static CompletableFuture<?> lookup(Object runtime, UUID uuid) throws Throwable {
        return (CompletableFuture<?>) invoke(runtime, "FabricServerRuntime", "onlinePlayer", UUID.class, uuid);
    }

    private static Object invoke(Object receiver, String className, String methodName, Class<?> parameterType,
                                 Object argument) throws Throwable {
        var type = Class.forName(PACKAGE + className);
        var method = parameterType == null ? type.getDeclaredMethod(methodName)
                : type.getDeclaredMethod(methodName, parameterType);
        method.setAccessible(true);
        try {
            return parameterType == null ? method.invoke(receiver) : method.invoke(receiver, argument);
        } catch (InvocationTargetException failure) {
            throw failure.getCause();
        }
    }
}
