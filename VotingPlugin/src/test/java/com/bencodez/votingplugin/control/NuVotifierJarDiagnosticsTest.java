package com.bencodez.votingplugin.control;

import com.google.gson.JsonObject;
import java.lang.reflect.*;
import java.net.*;
import java.nio.file.*;
import java.util.*;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/** Exercises actual released NuVotifier classes without starting a plugin or forwarding votes. */
class NuVotifierJarDiagnosticsTest {
    @Test void releasedProxyAdaptersObserveOnlyBoundedForwardingFacts() throws Exception {
        String configured = System.getProperty("nuvotifier.health.candidates");
        if (configured == null) configured = Path.of(com.vexsoftware.votifier.support.forwarding.ServerFilter.class
                .getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        for (String file : configured.split(java.util.regex.Pattern.quote(System.getProperty("path.separator")))) {
            assertTrue(!file.isBlank() && Files.isRegularFile(Path.of(file)), "Candidate must exist");
            try (URLClassLoader loader = new ChildFirstNuLoader(Path.of(file).toUri().toURL())) {
                for (String platform : List.of("bungee.NuVotifier", "velocity.VotifierPlugin")) {
                    Class<?> plugin = Class.forName("com.vexsoftware.votifier." + platform, false, loader);
                    Object provider = mock(plugin); // platform constructors/lifecycle never run
                    Object socket = socketSource(loader, List.of("actual-target"));
                    setSource(provider, socket);
                    assertEquals("actual-target", facts(provider).getAsJsonArray("forwardingDestinations").get(0).getAsString());
                    assertTrue(facts(provider).get("votifierForwardingEnabled").getAsBoolean());
                    Object active = mock(plugin); setSource(active, socket);
                    Object disabled = mock(plugin); setSource(disabled, socketSource(loader, List.of()));
                    assertFalse(facts(disabled).get("votifierForwardingEnabled").getAsBoolean());
                    for (List<?> order : List.of(List.of(disabled, active), List.of(active, disabled), List.of(
                            new OptionalVotifierDiagnosticsTest.VotifierProvider(new OptionalVotifierDiagnosticsTest.Snapshot(List.of())), active))) {
                        assertTrue(facts(order).get("votifierForwardingEnabled").getAsBoolean());
                    }
                    setSource(provider, null);
                    assertUnknown(provider);
                    assertUnknown(List.of(new OptionalVotifierDiagnosticsTest.VotifierProvider(
                            new OptionalVotifierDiagnosticsTest.Snapshot(List.of())), provider));
                    setSource(provider, mock(Class.forName("com.vexsoftware.votifier.support.forwarding.ForwardingVoteSource", false, loader)));
                    assertUnknown(provider);
                    for (List<String> malformed : List.of(List.of("duplicate", "duplicate"), List.of("bad\nname"),
                            java.util.stream.IntStream.range(0, 101).mapToObj(i -> "backend-" + i).toList())) {
                        setSource(provider, socketSource(loader, malformed)); assertUnknown(provider);
                    }
                    assertUnknown(Collections.nCopies(129, disabled));
                    for (String sourceName : List.of("PluginMessagingForwardingSource", "OnlineForwardPluginMessagingForwardingSource")) {
                        String packageName = platform.substring(0, platform.indexOf('.'));
                        Object pm = mock(Class.forName("com.vexsoftware.votifier." + packageName + "." + sourceName, false, loader));
                        Class<?> base = pm.getClass().getSuperclass();
                        Field owner = base.getDeclaredField("plugin"); owner.setAccessible(true); owner.set(pm, provider);
                        Field filter = base.getDeclaredField("serverFilter"); filter.setAccessible(true);
                        Class<?> backendApi = Class.forName("com.vexsoftware.votifier.platform.BackendServer", false, loader);
                        Object backend = mock(backendApi);
                        when(backendApi.getMethod("getName").invoke(backend)).thenReturn("backend-a");
                        Class<?> proxyApi = Class.forName("com.vexsoftware.votifier.platform.ProxyVotifierPlugin", false, loader);
                        when(proxyApi.getMethod("getAllBackendServers").invoke(provider)).thenReturn(List.of(backend));
                        Constructor<?> filterCtor = Class.forName("com.vexsoftware.votifier.support.forwarding.ServerFilter", false, loader)
                                .getConstructor(Collection.class, boolean.class);
                        setSource(provider, pm);
                        filter.set(pm, filterCtor.newInstance(List.of("backend-a"), true));
                        assertTrue(facts(provider).get("votifierForwardingEnabled").getAsBoolean());
                        assertEquals("backend-a", facts(provider).getAsJsonArray("forwardingDestinations").get(0).getAsString());
                        filter.set(pm, filterCtor.newInstance(List.of(), true));
                        assertFalse(facts(provider).get("votifierForwardingEnabled").getAsBoolean());
                        filter.set(pm, filterCtor.newInstance(List.of("backend-a"), false));
                        assertFalse(facts(provider).get("votifierForwardingEnabled").getAsBoolean());
                        if (sourceName.startsWith("OnlineForward")) {
                            Field fallback = pm.getClass().getDeclaredField("fallbackServer"); fallback.setAccessible(true);
                            when(proxyApi.getMethod("getServer", String.class).invoke(provider, "backend-a"))
                                    .thenReturn(Optional.of(backend));
                            fallback.set(pm, "backend-a");
                            // Both an empty whitelist and a rejecting blacklist still allow the fallback.
                            for (boolean whitelist : List.of(true, false)) {
                                filter.set(pm, filterCtor.newInstance(whitelist ? List.of() : List.of("backend-a"), whitelist));
                                assertTrue(facts(provider).get("votifierForwardingEnabled").getAsBoolean());
                                assertEquals("backend-a", facts(provider).getAsJsonArray("forwardingDestinations").get(0).getAsString());
                            }
                            when(proxyApi.getMethod("getServer", String.class).invoke(provider, "backend-a"))
                                    .thenReturn(Optional.empty());
                            assertFalse(facts(provider).get("votifierForwardingEnabled").getAsBoolean());
                            fallback.set(pm, "bad\nname"); assertUnknown(provider);
                            fallback.set(pm, null);
                        }
                        filter.set(pm, null); assertUnknown(provider);
                    }
                }
            }
        }
    }
    private static Object socketSource(ClassLoader loader, List<String> names) throws Exception {
        Class<?> source = Class.forName("com.vexsoftware.votifier.support.forwarding.proxy.ProxyForwardingVoteSource", false, loader);
        Constructor<?> backend = Class.forName(source.getName() + "$BackendServer", false, loader)
                .getConstructor(String.class, InetSocketAddress.class, java.security.Key.class);
        List<Object> targets = new ArrayList<>();
        for (String name : names) targets.add(backend.newInstance(name,
                InetSocketAddress.createUnresolved("not-exported.invalid", 8192), null));
        Constructor<?> ctor = Arrays.stream(source.getConstructors()).filter(c -> c.getParameterCount() == 4).findFirst().orElseThrow();
        return ctor.newInstance(null, null, targets, null); // stores fields; no start/forward call
    }
    private static void setSource(Object provider, Object source) throws Exception {
        Field field = provider.getClass().getDeclaredField("forwardingMethod"); field.setAccessible(true); field.set(provider, source);
    }
    private static JsonObject facts(Object provider) { return facts(List.of(provider)); }
    private static JsonObject facts(List<?> providers) {
        JsonObject facts = new JsonObject(); OptionalVotifierDiagnostics.add(facts, providers);
        assertFalse(facts.toString().contains("not-exported.invalid"));
        return facts;
    }
    private static void assertUnknown(Object provider) { assertFalse(facts(provider).has("votifierForwardingEnabled")); }
    private static void assertUnknown(List<?> providers) { assertFalse(facts(providers).has("votifierForwardingEnabled")); }
    private static final class ChildFirstNuLoader extends URLClassLoader {
        ChildFirstNuLoader(URL url) { super(new URL[]{url}, NuVotifierJarDiagnosticsTest.class.getClassLoader()); }
        @Override protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
            if (name.startsWith("com.vexsoftware.votifier.")) synchronized (getClassLoadingLock(name)) {
                Class<?> loaded = findLoadedClass(name);
                if (loaded == null) try { loaded = findClass(name); } catch (ClassNotFoundException ignored) { }
                if (loaded != null) { if (resolve) resolveClass(loaded); return loaded; }
            }
            return super.loadClass(name, resolve);
        }
    }
}
