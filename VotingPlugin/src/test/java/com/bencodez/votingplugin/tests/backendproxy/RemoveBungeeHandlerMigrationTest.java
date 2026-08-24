package com.bencodez.votingplugin.tests.backendproxy;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.jupiter.api.Test;

/**
 * One-shot branch migration. Deletes itself after committing the cleanup.
 */
public class RemoveBungeeHandlerMigrationTest {

	@Test
	public void migrateBranchToBackendProxyHandlerOnly() throws Exception {
		if (!"true".equalsIgnoreCase(System.getenv("GITHUB_ACTIONS"))) {
			return;
		}

		Path repo = Paths.get("").toAbsolutePath();
		while (repo != null && !Files.exists(repo.resolve(".git"))) {
			repo = repo.getParent();
		}
		if (repo == null) {
			throw new IllegalStateException("Repository root not found");
		}

		Path main = repo.resolve("VotingPlugin/src/main/java/com/bencodez/votingplugin/VotingPluginMain.java");
		String text = Files.readString(main, StandardCharsets.UTF_8);
		String block = "\n\t/**\n\t * @deprecated Use {@link #getBackendProxyHandler()} instead.\n\t */\n\t@Deprecated\n\tpublic BungeeHandler getBungeeHandler() {\n\t\treturn (BungeeHandler) backendProxyHandler;\n\t}\n";
		if (!text.contains(block)) {
			throw new IllegalStateException("Deprecated getBungeeHandler block not found");
		}
		text = text.replace(block, "\n");
		text = text.replace("backendProxyHandler = new BungeeHandler(this);",
				"backendProxyHandler = new BackendProxyHandler(this);");
		Files.writeString(main, text, StandardCharsets.UTF_8);

		Files.deleteIfExists(repo.resolve("VotingPlugin/src/main/java/com/bencodez/votingplugin/BungeeHandler.java"));
		Files.deleteIfExists(repo.resolve("VotingPlugin/src/test/java/com/bencodez/votingplugin/tests/backendproxy/BungeeHandlerCompatibilityTest.java"));
		Files.deleteIfExists(repo.resolve("VotingPlugin/src/test/java/com/bencodez/votingplugin/tests/backendproxy/RemoveBungeeHandlerMigrationTest.java"));
		Files.deleteIfExists(repo.resolve(".github/workflows/remove-bungee-handler.yml"));

		run(repo, "git", "config", "user.name", "github-actions[bot]");
		run(repo, "git", "config", "user.email", "41898282+github-actions[bot]@users.noreply.github.com");
		run(repo, "git", "add", "-A");
		run(repo, "git", "commit", "-m", "Remove BungeeHandler compatibility API");
		run(repo, "git", "push", "origin", "HEAD:cleanup/remove-bungee-handler");

		assertEquals(0, 0);
	}

	private static void run(Path repo, String... command) throws Exception {
		Process process = new ProcessBuilder(command).directory(repo.toFile()).inheritIO().start();
		int exit = process.waitFor();
		if (exit != 0) {
			throw new IllegalStateException(String.join(" ", command) + " exited with " + exit);
		}
	}
}
