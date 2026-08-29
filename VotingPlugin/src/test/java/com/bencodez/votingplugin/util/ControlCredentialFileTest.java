package com.bencodez.votingplugin.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
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
