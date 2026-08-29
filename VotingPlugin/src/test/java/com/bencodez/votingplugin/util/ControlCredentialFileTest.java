package com.bencodez.votingplugin.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ControlCredentialFileTest {
	@TempDir Path directory;

	@Test void createsBlankContainedCredentialFileBeforeEnrollment() throws Exception {
		Path root = Files.createDirectory(directory.resolve("plugin"));
		Path credential = root.resolve("control/control-credential.txt");

		assertThrows(java.io.IOException.class,
				() -> ControlCredentialFile.read(root, "control/control-credential.txt"));

		assertTrue(Files.isRegularFile(credential));
		assertEquals("", Files.readString(credential));
		PosixFileAttributeView posix = Files.getFileAttributeView(credential, PosixFileAttributeView.class);
		if (posix != null) {
			assertEquals(Set.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE),
					posix.readAttributes().permissions());
		}
	}

	@Test void automaticEnrollmentSurvivesRestartUntilAcknowledged() throws Exception {
		Path root = Files.createDirectory(directory.resolve("auto-plugin"));
		String configured = "control/control-credential.txt";

		ControlCredentialFile.PendingAutoEnrollment first =
				ControlCredentialFile.prepareAutoEnrollment(root, configured, "proxy-a");
		String credential = ControlCredentialFile.read(root, configured);
		ControlCredentialFile.PendingAutoEnrollment recovered =
				ControlCredentialFile.prepareAutoEnrollment(root, configured, "proxy-a");

		assertTrue(credential.startsWith("vpctl_node_"));
		assertEquals(first.verifier(), recovered.verifier());
		assertTrue(Files.isRegularFile(root.resolve(configured + ".auto-enroll")));
		ControlCredentialFile.completeAutoEnrollment(recovered);
		assertFalse(Files.exists(root.resolve(configured + ".auto-enroll")));
		ControlCredentialFile.completeAutoEnrollment(recovered);
		assertNull(ControlCredentialFile.prepareAutoEnrollment(root, configured, "proxy-a"));
		assertEquals(credential, ControlCredentialFile.read(root, configured));
	}

	@Test void existingManualCredentialIsNeverReplaced() throws Exception {
		Path root = Files.createDirectory(directory.resolve("manual-plugin"));
		Path control = Files.createDirectories(root.resolve("control"));
		Path credential = control.resolve("control-credential.txt");
		Files.writeString(credential, "vpctl_node_manually_enrolled");

		assertNull(ControlCredentialFile.prepareAutoEnrollment(root,
				"control/control-credential.txt", "proxy-a"));
		assertEquals("vpctl_node_manually_enrolled", Files.readString(credential));
		assertFalse(Files.exists(control.resolve("control-credential.txt.auto-enroll")));
	}

	@Test void enrollmentMarkerContainsOnlyBoundedNonSecretState() throws Exception {
		Path root = Files.createDirectory(directory.resolve("marker-plugin"));
		String configured = "control/control-credential.txt";
		ControlCredentialFile.prepareAutoEnrollment(root, configured, "survival");
		Path credential = root.resolve(configured);
		Path marker = root.resolve(configured + ".auto-enroll");

		String secret = Files.readString(credential);
		String state = Files.readString(marker);
		assertFalse(state.contains(secret));
		Files.writeString(marker, state + "\nunexpected");
		assertThrows(java.io.IOException.class,
				() -> ControlCredentialFile.prepareAutoEnrollment(root, configured, "survival"));
	}

	@Test void readsContainedFileAndRejectsFileDirectoryAndTraversalEscapes() throws Exception {
		Path root = Files.createDirectory(directory.resolve("plugin"));
		Path outside = Files.createDirectory(directory.resolve("outside"));
		Files.writeString(root.resolve("credential.txt"), "vpctl_node_valid\n");
		assertEquals("vpctl_node_valid", ControlCredentialFile.read(root, "credential.txt"));

		Path external = outside.resolve("external.txt");
		Files.writeString(external, "vpctl_node_external");
		Files.createSymbolicLink(root.resolve("linked.txt"), external);
		assertThrows(java.io.IOException.class, () -> ControlCredentialFile.read(root, "linked.txt"));
		Files.createSymbolicLink(root.resolve("linked-directory"), outside);
		assertThrows(java.io.IOException.class,
				() -> ControlCredentialFile.read(root, "linked-directory/external.txt"));
		assertThrows(java.io.IOException.class, () -> ControlCredentialFile.read(root, "../outside/external.txt"));
	}
}
