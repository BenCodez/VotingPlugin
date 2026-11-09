package com.bencodez.votingplugin.tests.commands;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.command.PlayerCommandHandler;
import com.bencodez.votingplugin.commands.CommandDocumentationExporter;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

class CommandDocumentationExporterTest {
    @TempDir
    Path temp;

    @Test
    void capturesNewCommandsWithoutAHandMaintainedList() {
        CommandHandler shop = handler("/vote", "/vote Shop", "VotingPlugin.Commands.Vote.Shop|VotingPlugin.Player",
                "Open shop");
        CommandHandler points = handler("/vote", "/vote Points", "VotingPlugin.Commands.Vote.Points|VotingPlugin.Player",
                "View points");
        CommandHandler admin = handler("/av", "/av Reload", "VotingPlugin.Commands.AdminVote.Reload|VotingPlugin.Admin",
                "Reload plugin");
        var rows = CommandDocumentationExporter.capture("/vote", List.of(shop, points), List.of(admin));
        JsonObject json = JsonParser.parseString(CommandDocumentationExporter.toJson(rows)).getAsJsonObject();
        assertEquals(1, json.get("schemaVersion").getAsInt());
        JsonArray commands = json.getAsJsonArray("commands");
        assertEquals(3, commands.size());
        assertEquals("/vote Points", commands.get(0).getAsJsonObject().get("syntax").getAsString());
        assertEquals("/vote Shop", commands.get(1).getAsJsonObject().get("syntax").getAsString());
        assertEquals("/av Reload", commands.get(2).getAsJsonObject().get("syntax").getAsString());
        assertEquals(2, commands.get(0).getAsJsonObject().getAsJsonArray("permissions").size());
    }

    @Test
    void capturesBulkPermissionsAndSenderRestrictions() {
        PlayerCommandHandler bulk = mock(PlayerCommandHandler.class);
        when(bulk.getHelpLineCommand("/av")).thenReturn("/av User (player) AddPoints (number)");
        when(bulk.getPerm()).thenReturn("VotingPlugin.Commands.AdminVote.AddPoints|VotingPlugin.Admin");
        when(bulk.getHelpMessage()).thenReturn("Add points");
        when(bulk.getAdditionalPermissions()).thenReturn(List.of("VotingPlugin.Commands.AdminVote.AddPoints.All"));
        when(bulk.isAllowMultiplePermissions()).thenReturn(true);
        when(bulk.isForceConsole()).thenReturn(true);

        var rows = CommandDocumentationExporter.capture("/vote", List.of(), List.of(bulk));
        JsonObject entry = JsonParser.parseString(CommandDocumentationExporter.toJson(rows))
                .getAsJsonObject().getAsJsonArray("commands").get(0).getAsJsonObject();
        assertEquals("console", entry.get("sender").getAsString());
        assertEquals("VotingPlugin.Commands.AdminVote.AddPoints.All",
                entry.getAsJsonArray("allTargetPermissions").get(0).getAsString());
        assertTrue(entry.get("multiplePermissionChecks").getAsBoolean());
    }

    @Test
    void rendersMarkdownSafelyAndRespectsConfiguredPlayerRoot() {
        CommandHandler handle = handler("/customvote", "/customvote Help/?",
                "VotingPlugin.Player|VotingPlugin.Commands.Vote.Help", "Help | usage\nwith newline");
        String markdown = CommandDocumentationExporter.toMarkdown(
                CommandDocumentationExporter.capture("/customvote", List.of(handle), List.of()));
        assertTrue(markdown.contains("## /customvote"));
        assertTrue(markdown.contains("Help \\| usage with newline"));
        assertTrue(markdown.contains("VotingPlugin.Player\\|VotingPlugin.Commands.Vote.Help"));
        assertFalse(markdown.contains("usage\nwith newline"));
    }

    @Test
    void writesOnlyWhenOutputChanges() throws Exception {
        CommandHandler original = handler("/vote", "/vote Shop", "VotingPlugin.Player", "Shop");
        assertTrue(CommandDocumentationExporter.write(temp, "/vote", List.of(original), List.of()));
        Path folder = temp.resolve(CommandDocumentationExporter.DIRECTORY);
        Path json = folder.resolve(CommandDocumentationExporter.JSON_NAME);
        Path markdown = folder.resolve(CommandDocumentationExporter.MARKDOWN_NAME);
        assertTrue(Files.readString(json, StandardCharsets.UTF_8).contains("/vote Shop"));
        assertTrue(Files.readString(markdown, StandardCharsets.UTF_8).contains("/vote Shop"));
        assertFalse(CommandDocumentationExporter.write(temp, "/vote", List.of(original), List.of()));
        CommandHandler added = handler("/vote", "/vote Next", "VotingPlugin.Player", "Next vote");
        assertTrue(CommandDocumentationExporter.write(temp, "/vote", List.of(original, added), List.of()));
        assertEquals(2, JsonParser.parseString(Files.readString(json))
                .getAsJsonObject().getAsJsonArray("commands").size());
    }

    private static CommandHandler handler(String root, String syntax, String permission, String description) {
        CommandHandler handle = mock(CommandHandler.class);
        when(handle.getHelpLineCommand(root)).thenReturn(syntax);
        when(handle.getPerm()).thenReturn(permission);
        when(handle.getHelpMessage()).thenReturn(description);
        when(handle.isAllowConsole()).thenReturn(true);
        when(handle.isAllowMultiplePermissions()).thenReturn(true);
        return handle;
    }
}
