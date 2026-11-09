package com.bencodez.votingplugin.neoforge;

import java.io.IOException;

/** Strict JSON string lexicon for bounded inputs; callers enforce the object and value grammar. */
final class NeoForgeJsonStrings {
    private NeoForgeJsonStrings() { }

    static void validate(String json) throws IOException {
        // Gson 2.10.1's setLenient(false) still accepts raw controls and legacy string escapes.
        // Validate the string lexicon before the streaming reader without depending on newer Gson APIs.
        boolean quoted = false;
        for (int i = 0; i < json.length(); i++) {
            char c = json.charAt(i);
            if (!quoted) {
                if (c == '"') quoted = true;
                continue;
            }
            if (c == '"') { quoted = false; continue; }
            if (c < 0x20) throw invalid();
            if (c != '\\') continue;
            if (++i == json.length()) throw invalid();
            switch (json.charAt(i)) {
                case '"', '\\', '/', 'b', 'f', 'n', 'r', 't' -> { }
                case 'u' -> {
                    if (json.length() - i <= 4) throw invalid();
                    for (int end = i + 4; i < end;) {
                        char digit = json.charAt(++i);
                        if (!(digit >= '0' && digit <= '9' || digit >= 'a' && digit <= 'f'
                                || digit >= 'A' && digit <= 'F')) throw invalid();
                    }
                }
                default -> throw invalid();
            }
        }
        if (quoted) throw invalid();
    }

    private static IOException invalid() { return new IOException("Invalid JSON string"); }
}
