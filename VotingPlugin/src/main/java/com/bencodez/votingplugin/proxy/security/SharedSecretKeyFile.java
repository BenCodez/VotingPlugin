package com.bencodez.votingplugin.proxy.security;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.EnumSet;
import java.util.Set;

/** Creates the shared transport key without ever replacing an operator-provided key. */
public final class SharedSecretKeyFile {
	private static final int KEY_BYTES = 32;

	private SharedSecretKeyFile() {
	}

	public static boolean ensure(Path keyFile) throws IOException {
		if (Files.isRegularFile(keyFile)) return false;
		Path parent = keyFile.toAbsolutePath().getParent();
		if (parent != null) Files.createDirectories(parent);
		byte[] key = new byte[KEY_BYTES];
		new SecureRandom().nextBytes(key);
		byte[] encoded = Base64.getEncoder().encode(key);
		boolean created = false;
		try {
			Set<java.nio.file.OpenOption> options = Set.of(java.nio.file.StandardOpenOption.CREATE_NEW,
					java.nio.file.StandardOpenOption.WRITE);
			java.nio.channels.SeekableByteChannel opened;
			if (Files.getFileStore(parent).supportsFileAttributeView("posix")) {
				opened = Files.newByteChannel(keyFile, options, PosixFilePermissions.asFileAttribute(
						EnumSet.of(PosixFilePermission.OWNER_READ, PosixFilePermission.OWNER_WRITE)));
			} else {
				opened = Files.newByteChannel(keyFile, options);
			}
			created = true;
			try (java.nio.channels.SeekableByteChannel channel = opened) {
				java.nio.ByteBuffer buffer = java.nio.ByteBuffer.wrap(encoded);
				while (buffer.hasRemaining()) channel.write(buffer);
			}
			return true;
		} catch (java.nio.file.FileAlreadyExistsException raced) {
			return false;
		} catch (IOException failure) {
			if (created) Files.deleteIfExists(keyFile);
			throw failure;
		} finally {
			java.util.Arrays.fill(encoded, (byte) 0);
			java.util.Arrays.fill(key, (byte) 0);
		}
	}
}
