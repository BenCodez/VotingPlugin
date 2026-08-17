package com.bencodez.votingplugin.tests.cleanup;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;

/**
 * One-shot cleanup for PR 1546. It only runs in the repository's own GitHub
 * Actions pull-request build and removes itself in the cleanup commit.
 */
public class Pr1546LineEndingCleanupTest {

	private static final String BRANCH = "codex/review-pr-response";
	private static final String CONFIG_PATH =
			"VotingPlugin/src/main/java/com/bencodez/votingplugin/config/ConfigVoteSites.java";
	private static final String USER_PATH =
			"VotingPlugin/src/main/java/com/bencodez/votingplugin/user/VotingPluginUser.java";
	private static final String WORKFLOW_PATH = ".github/workflows/pr1546-line-ending-cleanup.yml";
	private static final String TEST_PATH =
			"VotingPlugin/src/test/java/com/bencodez/votingplugin/tests/cleanup/Pr1546LineEndingCleanupTest.java";

	@Test
	public void normalizeAndPushCleanPrDiff() throws Exception {
		if (!"true".equals(System.getenv("GITHUB_ACTIONS"))
				|| !"pull_request".equals(System.getenv("GITHUB_EVENT_NAME"))
				|| !BRANCH.equals(System.getenv("GITHUB_HEAD_REF"))) {
			return;
		}

		String workspace = System.getenv("GITHUB_WORKSPACE");
		assertTrue(workspace != null && !workspace.isEmpty(), "GITHUB_WORKSPACE is required");
		Path repository = Paths.get(workspace);

		run(repository, "git", "fetch", "origin", BRANCH);
		run(repository, "git", "checkout", "-B", BRANCH, "origin/" + BRANCH);

		normalizeToCrLf(repository.resolve(CONFIG_PATH));
		normalizeToCrLf(repository.resolve(USER_PATH));
		Files.deleteIfExists(repository.resolve(WORKFLOW_PATH));
		Files.deleteIfExists(repository.resolve(TEST_PATH));

		run(repository, "git", "config", "user.name", "github-actions[bot]");
		run(repository, "git", "config", "user.email",
				"41898282+github-actions[bot]@users.noreply.github.com");
		run(repository, "git", "add", "-A", "--", CONFIG_PATH, USER_PATH, WORKFLOW_PATH, TEST_PATH);
		run(repository, "git", "diff", "--cached", "--check");

		Set<String> expected = new TreeSet<>(Arrays.asList(CONFIG_PATH, USER_PATH, WORKFLOW_PATH, TEST_PATH));
		Set<String> actual = new TreeSet<>();
		String changed = run(repository, "git", "diff", "--cached", "--name-only");
		for (String path : changed.split("\\r?\\n")) {
			if (!path.isEmpty()) actual.add(path);
		}
		assertEquals(expected, actual, "cleanup must change only the two sources and remove its temporary files");

		run(repository, "git", "commit", "-m", "Restore Java source line endings");
		run(repository, "git", "push", "origin", "HEAD:" + BRANCH);
	}

	private static void normalizeToCrLf(Path path) throws IOException {
		String source = new String(Files.readAllBytes(path), StandardCharsets.UTF_8);
		String lf = source.replace("\r\n", "\n").replace('\r', '\n');
		Files.write(path, lf.replace("\n", "\r\n").getBytes(StandardCharsets.UTF_8));

		byte[] bytes = Files.readAllBytes(path);
		for (int i = 0; i < bytes.length; i++) {
			if (bytes[i] == '\n') {
				assertTrue(i > 0 && bytes[i - 1] == '\r', "non-CRLF newline remains in " + path);
			}
		}
	}

	private static String run(Path repository, String... command) throws Exception {
		ProcessBuilder builder = new ProcessBuilder(command);
		builder.directory(new File(repository.toString()));
		builder.redirectErrorStream(true);
		Process process = builder.start();
		StringBuilder output = new StringBuilder();
		try (BufferedReader reader = new BufferedReader(
				new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8))) {
			String line;
			while ((line = reader.readLine()) != null) {
				output.append(line).append('\n');
			}
		}
		int exitCode = process.waitFor();
		assertEquals(0, exitCode,
				"command failed: " + Arrays.toString(command) + "\n" + output.toString());
		return output.toString();
	}
}
