package com.bencodez.votingplugin.util;

import java.io.IOException;
import java.nio.channels.FileChannel;
import java.nio.file.AccessDeniedException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Locale;

/** Cross-platform helpers for forcing file contents and published directory entries. */
public final class DurableFiles {
	private DurableFiles() { }

	public static void forceFile(Path file) throws IOException {
		try (FileChannel channel = FileChannel.open(file, StandardOpenOption.WRITE, LinkOption.NOFOLLOW_LINKS)) {
			channel.force(true);
		}
	}

	public static void forceDirectory(Path directory) throws IOException {
		if (directory == null) return;
		try {
			try (FileChannel channel = FileChannel.open(directory, StandardOpenOption.READ)) {
				channel.force(true);
			}
		} catch (AccessDeniedException unsupportedDirectoryHandle) {
			// The Windows NIO provider cannot open directory handles. File contents are
			// still forced before atomic publication; do not make persistence unusable.
			if (!isWindowsName(System.getProperty("os.name", ""))) throw unsupportedDirectoryHandle;
		} catch (UnsupportedOperationException unsupportedDirectoryForce) {
			// Some providers support atomic moves but expose no directory-force operation.
		}
	}

	public static boolean deleteIfExists(Path target) throws IOException {
		boolean deleted = Files.deleteIfExists(target);
		if (deleted) forceDirectory(target.toAbsolutePath().normalize().getParent());
		return deleted;
	}

	public static void forceMoveDirectories(Path source, Path target) throws IOException {
		Path sourceParent = source.toAbsolutePath().normalize().getParent();
		Path targetParent = target.toAbsolutePath().normalize().getParent();
		forceDirectory(targetParent);
		if (sourceParent != null && !sourceParent.equals(targetParent)) forceDirectory(sourceParent);
	}

	public static boolean isWindowsName(String name) {
		return name != null && name.trim().toLowerCase(Locale.ROOT).startsWith("windows");
	}
}
