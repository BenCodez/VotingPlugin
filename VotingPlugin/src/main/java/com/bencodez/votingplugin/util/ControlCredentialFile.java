package com.bencodez.votingplugin.util;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/** Symlink-safe, bounded credential loading shared by proxy and Bukkit Control connectors. */
public final class ControlCredentialFile {
	private static final int MAX_BYTES = 512;

	private ControlCredentialFile() { }

	public static String read(Path rootDirectory, String configuredPath) throws IOException {
		if (configuredPath == null || configuredPath.isBlank()) throw invalid();
		Path lexicalRoot = rootDirectory.toAbsolutePath().normalize();
		Path requested = lexicalRoot.resolve(configuredPath).normalize();
		if (!requested.startsWith(lexicalRoot) || requested.getParent() == null) throw invalid();
		Path realRoot = lexicalRoot.toRealPath();
		Path realParent = requested.getParent().toRealPath();
		if (!realParent.startsWith(realRoot)) throw invalid();
		Path target = realParent.resolve(requested.getFileName());
		if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS) || Files.isSymbolicLink(target)) throw invalid();

		ByteBuffer bytes = ByteBuffer.allocate(MAX_BYTES + 1);
		try (SeekableByteChannel channel = Files.newByteChannel(target, StandardOpenOption.READ,
				LinkOption.NOFOLLOW_LINKS)) {
			while (bytes.hasRemaining() && channel.read(bytes) > 0) { }
		}
		if (bytes.position() == 0 || bytes.position() > MAX_BYTES) throw invalid();
		bytes.flip();
		String credential;
		try {
			credential = StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
					.onUnmappableCharacter(CodingErrorAction.REPORT).decode(bytes).toString().trim();
		} catch (java.nio.charset.CharacterCodingException e) {
			throw invalid();
		}
		if (credential.isEmpty() || credential.length() > MAX_BYTES || credential.indexOf('\r') >= 0
				|| credential.indexOf('\n') >= 0) throw invalid();
		return credential;
	}

	private static IOException invalid() {
		return new IOException("Control credential file is missing or invalid");
	}
}
