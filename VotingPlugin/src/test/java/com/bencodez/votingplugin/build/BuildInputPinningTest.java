package com.bencodez.votingplugin.build;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;

class BuildInputPinningTest {

	@Test
	void releaseDocumentationUsesVerifiedTagCommit() throws IOException {
		String workflow = Files.readString(Path.of("..", ".github", "workflows", "publish-javadoc.yml"));
		String eligibilityJob = job(workflow, "release-eligibility");
		String buildJob = job(workflow, "build");
		String deployJob = job(workflow, "deploy");

		assertTrue(eligibilityJob.contains("git merge-base --is-ancestor \"$tag_commit\" origin/master"));
		assertTrue(eligibilityJob.contains("echo \"commit=$tag_commit\" >> \"$GITHUB_OUTPUT\""));
		assertFalse(workflow.contains("target_commitish"));
		assertTrue(buildJob.contains("ref: ${{ needs.release-eligibility.outputs.commit }}"));
		assertTrue(deployJob.contains("needs: [release-eligibility, build]"));
		assertTrue(deployJob.contains("needs.release-eligibility.outputs.eligible == 'true'"));
	}

	@Test
	void writeScopedCheckoutDoesNotPersistCredentials() throws IOException {
		String workflow = Files.readString(Path.of("..", ".github", "workflows", "maven.yml"));
		String submissionJob = job(workflow, "dependency-submission");

		assertTrue(submissionJob.contains("contents: write"));
		assertTrue(submissionJob.contains("persist-credentials: false"));
	}

	private static String job(String workflow, String name) {
		Matcher matcher = Pattern.compile("(?ms)^  " + Pattern.quote(name)
				+ ":\\R(?<job>.*?)(?=^  [A-Za-z0-9_-]+:\\R|\\z)").matcher(workflow);
		assertTrue(matcher.find(), () -> "Missing " + name + " job");
		return matcher.group("job");
	}
}
