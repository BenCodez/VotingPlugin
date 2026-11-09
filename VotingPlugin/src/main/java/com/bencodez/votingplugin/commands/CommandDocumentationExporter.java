package com.bencodez.votingplugin.commands;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.bencodez.advancedcore.api.command.CommandHandler;
import com.bencodez.advancedcore.api.command.PlayerCommandHandler;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

/**
 * Exports the actual Bukkit command-handler registrations as a deterministic
 * machine-readable manifest and an administrator-friendly Markdown reference.
 * It never executes a command or accesses user data.
 */
public final class CommandDocumentationExporter {
    public static final String DIRECTORY = "GeneratedDocs";
    public static final String JSON_NAME = "commands.json";
    public static final String MARKDOWN_NAME = "commands.md";

    private CommandDocumentationExporter() {
    }

    public static final class CommandRow {
        private final String root;
        private final String syntax;
        private final String description;
        private final String permissionExpression;
        private final List<String> permissions;
        private final List<String> allTargetPermissions;
        private final boolean multiplePermissionChecks;
        private final String sender;

        private CommandRow(String root, String syntax, String description, String permissionExpression,
                List<String> permissions, List<String> allTargetPermissions,
                boolean multiplePermissionChecks, String sender) {
            this.root = root;
            this.syntax = syntax;
            this.description = description;
            this.permissionExpression = permissionExpression;
            this.permissions = List.copyOf(permissions);
            this.allTargetPermissions = List.copyOf(allTargetPermissions);
            this.multiplePermissionChecks = multiplePermissionChecks;
            this.sender = sender;
        }
    }

    /** Capture only loaded handlers, including conditionally registered handlers. */
    public static List<CommandRow> capture(String playerRoot, List<? extends CommandHandler> playerHandlers,
            List<? extends CommandHandler> adminHandlers) {
        List<CommandRow> result = new ArrayList<>();
        result.addAll(captureGroup(playerRoot, playerHandlers));
        result.addAll(captureGroup("/av", adminHandlers));
        return List.copyOf(result);
    }

    private static List<CommandRow> captureGroup(String root, List<? extends CommandHandler> handlers) {
        List<CommandRow> result = new ArrayList<>();
        if (handlers == null) return result;
        for (CommandHandler handler : handlers) {
            if (handler == null) continue;
            String syntax = safe(handler.getHelpLineCommand(root));
            if (syntax.isEmpty()) continue;
            String permission = safe(handler.getPerm());
            List<String> allTarget = List.of();
            if (handler instanceof PlayerCommandHandler) {
                List<String> additional = ((PlayerCommandHandler) handler).getAdditionalPermissions();
                if (additional != null) allTarget = new ArrayList<>(new LinkedHashSet<>(additional));
            }
            String sender = handler.isForceConsole() ? "console" :
                    handler.isAllowConsole() ? "player or console" : "player";
            result.add(new CommandRow(root, syntax, safe(handler.getHelpMessage()), permission,
                    splitPermissions(permission), allTarget, handler.isAllowMultiplePermissions(), sender));
        }
        result.sort(Comparator.comparing((CommandRow row) -> row.syntax, String.CASE_INSENSITIVE_ORDER)
                .thenComparing(row -> row.permissionExpression, String.CASE_INSENSITIVE_ORDER));
        return result;
    }

    private static String safe(String text) {
        return text == null ? "" : text.trim();
    }

    private static List<String> splitPermissions(String expression) {
        Set<String> unique = new LinkedHashSet<>();
        for (String part : expression.split("\\|")) {
            String value = part.trim();
            if (!value.isEmpty()) unique.add(value);
        }
        return new ArrayList<>(unique);
    }

    /** Stable JSON for future documentation bots; it contains no credentials or player data. */
    public static String toJson(List<CommandRow> rows) {
        JsonObject doc = new JsonObject();
        doc.addProperty("schemaVersion", 1);
        doc.addProperty("scope", "VotingPlugin Bukkit registered handlers");
        JsonArray commands = new JsonArray();
        for (CommandRow row : rows) {
            JsonObject object = new JsonObject();
            object.addProperty("root", row.root);
            object.addProperty("syntax", row.syntax);
            object.addProperty("description", row.description);
            object.addProperty("permissionExpression", row.permissionExpression);
            object.add("permissions", asArray(row.permissions));
            object.add("allTargetPermissions", asArray(row.allTargetPermissions));
            object.addProperty("multiplePermissionChecks", row.multiplePermissionChecks);
            object.addProperty("sender", row.sender);
            commands.add(object);
        }
        doc.add("commands", commands);
        return new GsonBuilder().disableHtmlEscaping().setPrettyPrinting().create().toJson(doc) + "\n";
    }

    private static JsonArray asArray(List<String> values) {
        JsonArray array = new JsonArray();
        for (String value : values) array.add(value);
        return array;
    }

    /** Reader-friendly generated snapshot for reviewing changes without running code. */
    public static String toMarkdown(List<CommandRow> rows) {
        StringBuilder markdown = new StringBuilder();
        markdown.append("# VotingPlugin Bukkit commands (generated)\n\n");
        markdown.append("This page is generated from the command handlers loaded by the running server. ");
        markdown.append("It is not a canonical release reference: configured command labels and ");
        markdown.append("conditionally loaded commands may differ between servers.\n\n");
        markdown.append("Permission expressions separated by | may provide alternatives when ");
        markdown.append("multiple-permission checks are enabled. Extra all-target permissions ");
        markdown.append("apply only to supported bulk player commands.\n\n");
        markdown.append("This export does not cover separate Velocity/BungeeCord proxy commands ");
        markdown.append("or standalone plugin.yml convenience aliases.\n\n");

        Map<String, List<CommandRow>> groups = new LinkedHashMap<>();
        for (CommandRow row : rows) groups.computeIfAbsent(row.root, key -> new ArrayList<>()).add(row);
        for (Map.Entry<String, List<CommandRow>> group : groups.entrySet()) {
            markdown.append("## ").append(escapeCell(group.getKey())).append("\n\n");
            markdown.append("| Command | Description | Permission expression | All-target permissions | Sender |\n");
            markdown.append("|---|---|---|---|---|\n");
            for (CommandRow row : group.getValue()) {
                markdown.append("| ").append(code(row.syntax));
                markdown.append(" | ").append(escapeCell(row.description));
                markdown.append(" | ").append(code(row.permissionExpression.isEmpty() ? "(none)" : row.permissionExpression));
                markdown.append(" | ").append(row.allTargetPermissions.isEmpty() ? "—" :
                        code(String.join(", ", row.allTargetPermissions)));
                markdown.append(" | ").append(escapeCell(row.sender)).append(" |\n");
            }
            markdown.append("\n");
        }
        return markdown.toString();
    }

    private static String code(String value) {
        return String.valueOf((char) 96) + escapeCell(value) + (char) 96;
    }

    private static String escapeCell(String value) {
        return safe(value).replace("\r", " ").replace("\n", " ").replace("|", "\\|")
                .replace(String.valueOf((char) 96), "&#96;");
    }

    /**
     * Write only when content changes, using temporary files and atomic moves
     * when supported. No writes occur outside the plugin data folder.
     */
    public static boolean write(Path pluginDataFolder, String playerRoot,
            List<? extends CommandHandler> playerHandlers, List<? extends CommandHandler> adminHandlers)
            throws IOException {
        List<CommandRow> rows = capture(playerRoot, playerHandlers, adminHandlers);
        Path directory = pluginDataFolder.resolve(DIRECTORY);
        Files.createDirectories(directory);
        boolean jsonChanged = writeIfChanged(directory.resolve(JSON_NAME), toJson(rows));
        boolean markdownChanged = writeIfChanged(directory.resolve(MARKDOWN_NAME), toMarkdown(rows));
        return jsonChanged || markdownChanged;
    }

    private static boolean writeIfChanged(Path target, String content) throws IOException {
        if (Files.isSymbolicLink(target)) throw new IOException("Refusing to overwrite symbolic link: " + target);
        if (Files.isRegularFile(target) && content.equals(Files.readString(target, StandardCharsets.UTF_8))) {
            return false;
        }
        Path temporary = Files.createTempFile(target.getParent(), target.getFileName().toString(), ".tmp");
        try {
            Files.writeString(temporary, content, StandardCharsets.UTF_8);
            try {
                Files.move(temporary, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
            } catch (AtomicMoveNotSupportedException unsupported) {
                Files.move(temporary, target, StandardCopyOption.REPLACE_EXISTING);
            }
        } finally {
            Files.deleteIfExists(temporary);
        }
        return true;
    }
}
