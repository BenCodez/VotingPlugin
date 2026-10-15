package com.bencodez.votingplugin.npc;

import java.io.IOException;
import java.io.StringReader;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonToken;
import com.google.gson.stream.JsonWriter;
import com.bencodez.votingplugin.util.DurableFiles;

/** Single storage-worker owner. Never call this file boundary on a gameplay thread. */
class VotingNPCStore {
    static final int MAX_NPCS = 64;
    private static final int MAX_BYTES = 128 * 1024;
    private static final Set<String> FIELDS = Set.of("id", "identity", "world", "x", "y", "z", "yaw", "pitch", "name");
    private final Path file;
    private boolean initialized;
    private boolean uncertain;

    VotingNPCStore(Path file) { this.file = file.toAbsolutePath().normalize(); }

    synchronized List<VotingNPCDefinition> load() throws IOException {
        initialized = false;
        checkParent();
        BasicFileAttributes attributes;
        try { attributes = Files.readAttributes(file, BasicFileAttributes.class, LinkOption.NOFOLLOW_LINKS); }
        catch (NoSuchFileException missing) {
            if (uncertain) throw new IOException("Uncertain voting NPC definitions are missing", missing);
            initialized = true;
            return List.of();
        }
        if (!attributes.isRegularFile() || attributes.isSymbolicLink() || attributes.size() > MAX_BYTES) {
            throw new IOException("Voting NPC definitions are not a bounded regular file");
        }
        byte[] bytes;
        try (var input = Files.newInputStream(file, LinkOption.NOFOLLOW_LINKS)) {
            bytes = input.readNBytes(MAX_BYTES + 1);
        }
        if (bytes.length > MAX_BYTES) throw new IOException("Voting NPC definitions exceed limit");
        String text;
        try { text = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
                .onUnmappableCharacter(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(bytes)).toString(); }
        catch (CharacterCodingException invalid) { throw new IOException("Invalid voting NPC encoding", invalid); }
        List<VotingNPCDefinition> result = decode(text);
        uncertain = false;
        initialized = true;
        return result;
    }

    synchronized void save(List<VotingNPCDefinition> definitions) throws IOException {
        if (!initialized || uncertain) throw new IOException("Load valid voting NPC definitions before editing");
        validate(definitions);
        byte[] bytes = encode(definitions).getBytes(StandardCharsets.UTF_8);
        if (bytes.length > MAX_BYTES) throw new IOException("Voting NPC definitions exceed limit");
        checkParent();
        if (Files.isSymbolicLink(file)) throw new IOException("Voting NPC definitions cannot be a symbolic link");
        Path staged = Files.createTempFile(file.getParent(), ".voting-npcs-", ".tmp");
        boolean published = false;
        Throwable primary = null;
        try {
            try (FileChannel output = FileChannel.open(staged, StandardOpenOption.WRITE)) {
                ByteBuffer buffer = ByteBuffer.wrap(bytes);
                while (buffer.hasRemaining()) output.write(buffer);
                output.force(true);
            }
            Files.move(staged, file, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
            published = true;
            forceDirectory(file.getParent());
        } catch (IOException | RuntimeException | Error failure) {
            // A visible replacement followed by a failed directory sync is not a
            // rollback. Fence later mutations rather than overwrite it from old state.
            if (published) uncertain = true;
            primary = failure;
            throw failure;
        } finally {
            try { Files.deleteIfExists(staged); }
            catch (IOException cleanupFailure) {
                if (primary != null) primary.addSuppressed(cleanupFailure);
                else {
                    if (published) uncertain = true;
                    throw cleanupFailure;
                }
            }
        }
    }

    private void checkParent() throws IOException {
        // Operators may place the data directory below a directory alias. Only
        // the definitions and lease files themselves must remain non-links.
        if (!Files.isDirectory(file.getParent().toRealPath(), LinkOption.NOFOLLOW_LINKS)) {
            throw new IOException("Voting NPC data directory is unavailable");
        }
    }

    /** Kept as a narrow I/O seam so failure-after-publication can be tested. */
    void forceDirectory(Path directory) throws IOException {
        DurableFiles.forceDirectory(directory);
    }

    private static void validate(List<VotingNPCDefinition> values) throws IOException {
        if (values == null || values.size() > MAX_NPCS) throw new IOException("Voting NPC definition limit exceeded");
        Set<String> ids = new HashSet<>();
        Set<UUID> identities = new HashSet<>();
        for (VotingNPCDefinition value : values) {
            if (value == null || !ids.add(value.id()) || !identities.add(value.identity())) {
                throw new IOException("Duplicate or missing voting NPC identity");
            }
        }
    }

    private static String encode(List<VotingNPCDefinition> values) throws IOException {
        var text = new java.io.StringWriter();
        try (JsonWriter writer = new JsonWriter(text)) {
            writer.setIndent("  ");
            writer.beginObject().name("version").value(1).name("npcs").beginArray();
            for (VotingNPCDefinition value : values) {
                writer.beginObject().name("id").value(value.id()).name("identity").value(value.identity().toString())
                        .name("world").value(value.world().toString()).name("x").value(value.x()).name("y").value(value.y())
                        .name("z").value(value.z()).name("yaw").value(value.yaw()).name("pitch").value(value.pitch())
                        .name("name").value(value.name()).endObject();
            }
            writer.endArray().endObject();
        }
        return text.toString() + "\n";
    }

    private static List<VotingNPCDefinition> decode(String text) throws IOException {
        try (JsonReader reader = new JsonReader(new StringReader(text))) {
            Set<String> keys = new HashSet<>();
            List<VotingNPCDefinition> values = null;
            reader.beginObject();
            while (reader.hasNext()) {
                String key = reader.nextName();
                if (!keys.add(key)) throw new IOException("Duplicate voting NPC document field");
                if (key.equals("version")) {
                    if (reader.peek() != JsonToken.NUMBER || !reader.nextString().equals("1")) {
                        throw new IOException("Unsupported voting NPC document version");
                    }
                } else if (key.equals("npcs")) {
                    values = new ArrayList<>();
                    reader.beginArray();
                    while (reader.hasNext()) {
                        if (values.size() >= MAX_NPCS) throw new IOException("Voting NPC definition limit exceeded");
                        values.add(readDefinition(reader));
                    }
                    reader.endArray();
                } else throw new IOException("Unknown voting NPC document field");
            }
            reader.endObject();
            if (!keys.equals(Set.of("version", "npcs")) || reader.peek() != JsonToken.END_DOCUMENT) {
                throw new IOException("Incomplete or trailing voting NPC document");
            }
            validate(values);
            return List.copyOf(values);
        } catch (IllegalArgumentException | IllegalStateException invalid) {
            throw new IOException("Invalid voting NPC definitions", invalid);
        }
    }

    private static VotingNPCDefinition readDefinition(JsonReader reader) throws IOException {
        var fields = new java.util.HashMap<String, String>();
        reader.beginObject();
        while (reader.hasNext()) {
            String field = reader.nextName();
            if (!FIELDS.contains(field) || fields.containsKey(field)) throw new IOException("Unknown or duplicate voting NPC field");
            boolean numeric = Set.of("x", "y", "z", "yaw", "pitch").contains(field);
            if (reader.peek() != (numeric ? JsonToken.NUMBER : JsonToken.STRING)) throw new IOException("Invalid voting NPC field type");
            fields.put(field, reader.nextString());
        }
        reader.endObject();
        if (!fields.keySet().equals(FIELDS)) throw new IOException("Missing voting NPC field");
        return new VotingNPCDefinition(fields.get("id"), uuid(fields.get("identity")), uuid(fields.get("world")),
                Double.parseDouble(fields.get("x")), Double.parseDouble(fields.get("y")), Double.parseDouble(fields.get("z")),
                Float.parseFloat(fields.get("yaw")), Float.parseFloat(fields.get("pitch")), fields.get("name"));
    }

    private static UUID uuid(String text) {
        UUID value = UUID.fromString(text);
        if (!value.toString().equals(text)) throw new IllegalArgumentException("Noncanonical voting NPC identity");
        return value;
    }
}
