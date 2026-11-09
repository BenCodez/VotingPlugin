package com.bencodez.votingplugin.tests.neoforge;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.GeneralSecurityException;
import java.security.KeyPairGenerator;
import java.util.Base64;

import com.bencodez.votingplugin.proxy.security.ProxyOriginEnvelopeSignature;

/** Independently generated synthetic proxy-only keys; no deployment values. */
public final class NeoForgeProxyOriginFixture {
    public static ProxyOriginEnvelopeSignature create(Path directory) throws IOException {
        try {
            var pair = KeyPairGenerator.getInstance("Ed25519").generateKeyPair();
            Files.writeString(directory.resolve("synthetic-proxy-private.key"), Base64.getEncoder().encodeToString(pair.getPrivate().getEncoded()));
            Files.writeString(directory.resolve("synthetic-proxy-public.key"), Base64.getEncoder().encodeToString(pair.getPublic().getEncoded()));
            return ProxyOriginEnvelopeSignature.loadSigner(directory.resolve("synthetic-proxy-private.key"));
        } catch (GeneralSecurityException failure) {
            throw new IOException("Synthetic signing fixture unavailable", failure);
        }
    }

    public static ProxyOriginEnvelopeSignature verifier(Path directory) throws IOException {
        return ProxyOriginEnvelopeSignature.loadVerifier(directory.resolve("synthetic-proxy-public.key"));
    }

    public static String settings() {
        return "BungeeServer: {Name: proxy-test}\nProxyVerificationKeyFile: synthetic-proxy-public.key\n";
    }
}
