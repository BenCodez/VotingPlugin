package com.bencodez.votingplugin.util;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ControlCredentialFileTest {
	@TempDir Path directory;

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
