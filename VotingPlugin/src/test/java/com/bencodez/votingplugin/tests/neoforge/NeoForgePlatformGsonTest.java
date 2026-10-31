package com.bencodez.votingplugin.tests.neoforge;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.HexFormat;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.bencodez.simpleapi.servercomm.codec.JsonEnvelope;
import com.bencodez.votingplugin.neoforge.NeoForgeRuntime;
import com.google.gson.Gson;

/** Real platform-library linkage, isolated from Maven's newer provided Gson. */
public class NeoForgePlatformGsonTest {
    @Test void completeBrokerIngressUsesOnlyTheActualMinecraftGson() throws Exception {
        verify(NeoForgeRuntime.class.getProtectionDomain().getCodeSource().getLocation(), false);
    }

    /** Test-only entry point shared with the downloadable-JAR acceptance test. */
    public static void verify(URL plugin, boolean packaged) throws Exception {
        Path gson = Path.of(System.getProperty("votingplugin.neoforgeGsonJar"));
        assertEquals("4241c14a7727c34feea6507ec801318a3d4a90f070e4525681079fb94ee4c593",
                HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(gson))));
        URL[] urls = packaged ? new URL[] {plugin, gson.toUri().toURL()}
                : new URL[] {plugin, gson.toUri().toURL(),
                        JsonEnvelope.class.getProtectionDomain().getCodeSource().getLocation()};
        try (URLClassLoader loader = new URLClassLoader(urls, ClassLoader.getPlatformClassLoader())) {
            assertEquals(gson.toUri().toURL(), Class.forName("com.google.gson.Gson", true, loader)
                    .getProtectionDomain().getCodeSource().getLocation());
            assertThrows(ClassNotFoundException.class,
                    () -> Class.forName("com.google.gson.Strictness", false, loader));
            for (String deferred : new String[] {"Configuration", "IdentityResolver", "Protocol", "Service"}) {
                assertThrows(ClassNotFoundException.class, () -> Class.forName(
                        "com.bencodez.votingplugin.neoforge.NeoForgeVotifier" + deferred, false, loader),
                        "Direct Votifier ingress is excluded from the proxy transport artifact");
            }
            String envelope = "{\"t\":\"status\",\"v\":1,\"f\":{\"note\":\"sample 雪\"}}";
            assertNotNull(broker(loader, envelope));
            assertNotNull(broker(loader, envelope.replace("sample 雪", "sample\\n雪")));
            assertNotNull(broker(loader, new Gson().toJson(Map.of("t", "status", "f", Map.of(
                    "note", "literal {\"t\":\"data\"} \\ / 雪\n\r\t")))));
            for (char control = 0; control < 0x20; control++) {
                String raw = envelope.replace("sample 雪", "sample" + control + "雪");
                assertThrows(IOException.class, () -> broker(loader, raw));
                // Escaped controls are legal JSON string data, unlike literal control characters.
                assertNotNull(broker(loader, new Gson().toJson(Map.of("t", "status", "f", Map.of(
                        "note", "sample" + control + "雪")))));
            }
            for (String invalidEscape : new String[] {"\\x", "\\v", "\\u12", "\\uZZZZ", "\\u００００"}) {
                assertThrows(IOException.class,
                        () -> broker(loader, envelope.replace("sample 雪", invalidEscape)));
            }
            for (String malformed : new String[] {envelope + " {}",
                    envelope.replace("sample 雪", "sample\n雪"),
                    envelope.replace("sample 雪", "sample\t雪"),
                    envelope.replace("sample 雪", "sample\\'雪")}) {
                assertThrows(IOException.class, () -> broker(loader, malformed));
            }
        }
    }

    private static Object broker(ClassLoader loader, String json) throws Exception {
        return invoke(loader, "NeoForgeProxyEnvelopeCodec", new Class<?>[] {byte[].class},
                (Object) json.getBytes(StandardCharsets.UTF_8));
    }

    private static Object invoke(ClassLoader loader, String name, Class<?>[] types, Object... values) throws Exception {
        var method = Class.forName("com.bencodez.votingplugin.neoforge." + name, true, loader)
                .getDeclaredMethod("decode", types);
        method.setAccessible(true);
        try { return method.invoke(null, values); }
        catch (InvocationTargetException wrapped) {
            if (wrapped.getCause() instanceof Exception failure) throw failure;
            if (wrapped.getCause() instanceof Error failure) throw failure;
            throw wrapped;
        }
    }
}
