package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.votingplugin.proxy.cache.VoteCacheDurability;

class VoteCacheJsonSyntaxTest {
    @TempDir Path directory;

    @TestFactory
    Stream<DynamicTest> rejectsNonJsonSyntaxEvenWithLegacyNonLenientGson() {
        List<String> invalid = List.of(
                "{\"value\":TRUE}", "{\"value\":False}", "{\"value\":NULL}",
                "{\"value\":+1}", "{\"value\":01}", "{\"value\":.1}", "{\"value\":1.}",
                "{\"value\":NaN}", "{\"value\":Infinity}", "{\"value\":1e}",
                "{\"value\":\"raw\nnewline\"}", "{\"value\":\"raw\ttab\"}",
                "{\"value\":\"bad\\xescape\"}", "{\"value\":\"bad\\u00zz\"}",
                "{\"value\":\"bad\\u１２３４\"}", "{\"value\":1}\f",
                "{'value':1}", "{value:1}", "{\"value\":1,}",
                "{\"value\":1,//comment\n\"other\":2}", "{}{}",
                "{\"value\":1,\"value\":2}");
        return java.util.stream.IntStream.range(0, invalid.size()).mapToObj(index ->
                DynamicTest.dynamicTest("invalid-syntax-" + index, () -> {
                    Path file = directory.resolve("cache-" + index + ".json");
                    Files.writeString(file, invalid.get(index));
                    assertThrows(IOException.class, () -> VoteCacheDurability.readJournal(file));
                }));
    }

    @Test
    void validEscapesAndNumbersSurviveReaderBufferBoundaries() throws Exception {
        Path file = directory.resolve("valid.json");
        String padding = "synthetic".repeat(200);
        Files.writeString(file, "{\"text\":\"" + padding + "\\u0041\\n\\t\\/\\\\\\\"\","
                + "\"values\":[true,false,null,-1.25e+2,0]} \r\n");
        var document = VoteCacheDurability.readJournal(file);
        assertEquals(padding + "A\n\t/\\\"", document.get("text").getAsString());
        assertEquals(-125, document.getAsJsonArray("values").get(3).getAsInt());
    }
}
