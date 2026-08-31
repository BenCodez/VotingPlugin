package com.bencodez.votingplugin.proxy.control;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class ProxyConfigurationFileServiceTest {
	@TempDir Path directory;

	@Test
	void readMasksCredentialsJdbcDetailsAndControlPaths() throws Exception {
		Path file = write("""
				Database:
				  Host: db.internal
				  Port: 3306
				  Database: voting
				  Username: admin
				  Password: secret
				Redis:
				  Host: redis.internal
				  Password: redis-secret
				MQTT:
				  BrokerURL: tcp://user:pass@broker.internal:1883
				Control:
				  CredentialFile: control/credential.txt
				  Hosted:
				    JarFile: control/control.jar
				    DataDirectory: control/data
				BungeeMethod: PLUGINMESSAGING
				""");

		String content = service(file).read(ProxyConfigurationFileService.FILE_NAME).content();

		assertFalse(content.contains("db.internal"));
		assertFalse(content.contains("voting"));
		assertFalse(content.contains("admin"));
		assertFalse(content.contains("secret"));
		assertFalse(content.contains("user:pass"));
		assertFalse(content.contains("control/credential.txt"));
		assertFalse(content.contains("control/control.jar"));
		assertFalse(content.contains("control/data"));
		assertFalse(content.contains("redis.internal"));
		assertFalse(content.contains("redis-secret"));
		assertTrue(content.contains(ProxyConfigurationFileService.REDACTED));
	}

	@Test
	void masksAndRestoresSecretsNestedInSequences() throws Exception {
		Path file = write("""
				Hooks:
				  - Name: primary
				    Authorization: sequence-secret # sequence-secret
				    Enabled: true
				  - Redis:
				      Host: redis.internal # redis.internal
				      Port: 6379
				      Password: nested-password
				      SSL: true
				Endpoints:
				  - jdbc:mysql://sequence-user:sequence-password@db.internal/votes # sequence-password
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("sequence-secret"));
		assertFalse(current.content().contains("redis.internal"));
		assertFalse(current.content().contains("nested-password"));
		assertFalse(current.content().contains("sequence-user"));
		assertFalse(current.content().contains("sequence-password"));
		assertTrue(current.content().contains("SSL: true"));
		assertTrue(current.content().contains(ProxyConfigurationFileService.REDACTED));

		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("Authorization: sequence-secret # sequence-secret"));
		assertTrue(preview.resolvedContent().contains("Host: redis.internal # redis.internal"));
		assertTrue(preview.resolvedContent().contains("Password: nested-password"));
		assertTrue(preview.resolvedContent().contains("jdbc:mysql://sequence-user:sequence-password@db.internal/votes # sequence-password"));
		assertTrue(preview.resolvedContent().contains("SSL: true"));
		service.apply(ProxyConfigurationFileService.FILE_NAME, proposal, current.revision());
		String applied = Files.readString(file);
		assertTrue(applied.contains("Authorization: sequence-secret # sequence-secret"));
		assertTrue(applied.contains("Host: redis.internal # redis.internal"));
		assertTrue(applied.contains("Password: nested-password"));
		assertTrue(applied.contains("jdbc:mysql://sequence-user:sequence-password@db.internal/votes # sequence-password"));
		assertTrue(applied.contains("Debug: true"));
	}

	@Test
	void masksAndRestoresMultiProxyRedisInfrastructure() throws Exception {
		Path file = write("""
				MultiProxyRedis:
				  Host: multi-redis.internal # multi-redis.internal
				  Port: 6380
				  Username: multi-user
				  Password: multi-password
				  Db-Index: 2
				  SSL: true
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("multi-redis.internal"));
		assertFalse(current.content().contains("6380"));
		assertFalse(current.content().contains("multi-user"));
		assertFalse(current.content().contains("multi-password"));
		assertFalse(current.content().contains("Db-Index: 2"));
		assertTrue(current.content().contains("SSL: true"));

		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("Host: multi-redis.internal # multi-redis.internal"));
		assertTrue(preview.resolvedContent().contains("Port: 6380"));
		assertTrue(preview.resolvedContent().contains("Username: multi-user"));
		assertTrue(preview.resolvedContent().contains("Password: multi-password"));
		assertTrue(preview.resolvedContent().contains("Db-Index: 2"));
		service.apply(ProxyConfigurationFileService.FILE_NAME, proposal, current.revision());
		assertTrue(Files.readString(file).contains("Host: multi-redis.internal # multi-redis.internal"));
	}

	@Test
	void rejectsRemovedReorderedOrIntroducedSequenceSecretMarkers() throws Exception {
		Path file = write("""
				Hooks:
				  - Name: primary
				    Authorization: sequence-secret
				  - Name: secondary
				    Enabled: true
				""");
		ProxyConfigurationFileService service = service(file);
		String proposal = service.read(ProxyConfigurationFileService.FILE_NAME).content();
		String removed = "Hooks:\n  - Name: primary\n  - Name: secondary\n    Enabled: true\n";
		String reordered = "Hooks:\n  - Name: secondary\n    Enabled: true\n  - Name: primary\n"
				+ "    Authorization: " + ProxyConfigurationFileService.REDACTED + "\n";

		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME, removed));
		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME, reordered));
		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME,
						proposal + "Unexpected:\n  - " + ProxyConfigurationFileService.REDACTED + "\n"));
	}

	@Test
	void previewRestoresMaskedValuesAndAllowsSafeNestedAdditions() throws Exception {
		Path file = write("""
				Database:
				  Host: db.internal
				  Password: secret
				BungeeMethod: PLUGINMESSAGING
				""");
		ProxyConfigurationFileService service = service(file);
		String proposal = service.read(ProxyConfigurationFileService.FILE_NAME).content()
				+ "NewSection:\n  Enabled: true\n";

		ProxyConfigurationFileService.Preview preview = service.preview(
				ProxyConfigurationFileService.FILE_NAME, proposal);

		assertTrue(preview.resolvedContent().contains("db.internal"));
		assertTrue(preview.resolvedContent().contains("secret"));
		assertTrue(preview.resolvedContent().contains("NewSection"));
		assertTrue(preview.changes().contains("added NewSection.Enabled"));
	}

	@Test
	void roundTripsCommentsStylesAndNestedAdditionsWithoutLeakingSecrets() throws Exception {
		Path file = write("""
				# public header
				General: # public inline
				  # nested public comment
				  Message: "hello" # still public
				  Items:
				    - first # sequence comment
				    - |
				      a block value
				      remains styled
				  Flow: {Enabled: true, Label: 'quoted'}
				Database:
				  Password: secret-value # password is secret-value
				MQTT:
				  BrokerURL: tcp://user:password@broker.internal:1883 # jdbc://user:password@host
				Debug: 'false'
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document document = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertTrue(document.content().contains("# public header"));
		assertTrue(document.content().contains("# nested public comment"));
		assertTrue(document.content().contains("# sequence comment"));
		assertTrue(document.content().contains("Message: \"hello\""));
		assertTrue(document.content().contains("Flow: {"));
		assertTrue(document.content().contains("Label: 'quoted'"));
		assertTrue(document.content().contains("Debug: 'false'"));
		assertFalse(document.content().contains("secret-value"));
		assertFalse(document.content().contains("user:password"));
		assertFalse(document.content().contains("jdbc://"));
		assertTrue(document.content().contains("# " + ProxyConfigurationFileService.REDACTED));

		String proposal = document.content() + "Added:\n  Value: true\n";
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("# public header"));
		assertTrue(preview.resolvedContent().contains("# nested public comment"));
		assertTrue(preview.resolvedContent().contains("# sequence comment"));
		assertTrue(preview.resolvedContent().contains("Password: secret-value # password is secret-value"));
		assertTrue(preview.resolvedContent().contains("BrokerURL: tcp://user:password@broker.internal:1883"));
		assertTrue(preview.resolvedContent().contains("Added:\n  Value: true"));
		service.apply(ProxyConfigurationFileService.FILE_NAME, proposal, document.revision());
		String applied = Files.readString(file);
		assertTrue(applied.contains("# public header"));
		assertTrue(applied.contains("# nested public comment"));
		assertTrue(applied.contains("# sequence comment"));
		assertTrue(applied.contains("Password: secret-value # password is secret-value"));
		assertTrue(applied.contains("Added:\n  Value: true"));
	}

	@Test
	void masksShortAndBooleanLikeSecretsRepeatedInOtherwisePublicComments() throws Exception {
		Path file = write("""
				Database:
				  Password: abcde
				Redis:
				  Password: false
				MQTT:
				  Password: on
				Socket:
				  Password: no
				Other:
				  Password: x
				Debug: false # repeats abcde
				Feature: true # false
				Mode: safe # on
				Fallback: safe # no
				Marker: safe # x
				""");

		String content = service(file).read(ProxyConfigurationFileService.FILE_NAME).content();

		assertFalse(content.contains("abcde"));
		assertFalse(content.contains("# false"));
		assertFalse(content.contains("# on"));
		assertFalse(content.contains("# no"));
		assertFalse(content.contains("# x"));
		assertTrue(content.contains("# " + ProxyConfigurationFileService.REDACTED));
	}

	@Test
	void previewAllowsAnExplicitReplacementForAnExistingSecret() throws Exception {
		Path file = write("Database:\n  Password: old-secret\nDebug: false\n");
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		String proposal = current.content().replace("Password: " + ProxyConfigurationFileService.REDACTED,
				"Password: new-secret");

		ProxyConfigurationFileService.Preview preview = service.preview(
				ProxyConfigurationFileService.FILE_NAME, proposal);

		assertTrue(preview.resolvedContent().contains("Password: new-secret"));
		assertFalse(preview.resolvedContent().contains("old-secret"));
	}

	@Test
	void rejectsEditedDeletedOrMovedSecretValueAndCommentMarkers() throws Exception {
		Path file = write("""
				Database:
				  Password: secret # a password comment
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);
		String proposal = service.read(ProxyConfigurationFileService.FILE_NAME).content();

		assertThrows(IllegalArgumentException.class, () -> service.preview(ProxyConfigurationFileService.FILE_NAME,
				proposal.replace(ProxyConfigurationFileService.REDACTED, "changed")));
		assertThrows(IllegalArgumentException.class, () -> service.preview(ProxyConfigurationFileService.FILE_NAME,
				proposal.replace("  Password: " + ProxyConfigurationFileService.REDACTED + " # "
						+ ProxyConfigurationFileService.REDACTED + "\n", "")));
		assertThrows(IllegalArgumentException.class, () -> service.preview(ProxyConfigurationFileService.FILE_NAME,
				proposal.replace("# " + ProxyConfigurationFileService.REDACTED, "# edited")));
		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME, "Debug: false\n"));
		String moved = proposal.replace("  Password: " + ProxyConfigurationFileService.REDACTED + " # "
				+ ProxyConfigurationFileService.REDACTED + "\n", "")
					+ "OtherPassword: " + ProxyConfigurationFileService.REDACTED + " # "
					+ ProxyConfigurationFileService.REDACTED + "\n";
		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME, moved));
	}

	@Test
	void applyPublishesConfigurationAndPreservesTargetPermissions() throws Exception {
		Path file = write("BungeeMethod: PLUGINMESSAGING\nDebug: false\n");
		try {
			Files.setPosixFilePermissions(file, java.nio.file.attribute.PosixFilePermissions.fromString("rw-------"));
		} catch (UnsupportedOperationException ignored) { }
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);

		ProxyConfigurationFileService.ApplyResult applied = service.apply(ProxyConfigurationFileService.FILE_NAME,
				current.content().replace("false", "true"), current.revision());

		assertTrue(Files.readString(file).contains("Debug: true"));
		assertFalse(applied.rolledBack());
		assertTrue(Files.isRegularFile(directory.resolve("bungeeconfig.yml.control-backup")));
		try {
			assertEquals("rw-------", java.nio.file.attribute.PosixFilePermissions.toString(
					Files.getPosixFilePermissions(file)));
		} catch (UnsupportedOperationException ignored) { }
	}

	@Test
	void publicationFailureLeavesOriginalReadableAndRetryable() throws Exception {
		Path file = write("BungeeMethod: PLUGINMESSAGING\nDebug: false\n");
		String original = Files.readString(file);
		AtomicInteger moves = new AtomicInteger();
		ProxyConfigurationFileService failing = new ProxyConfigurationFileService(file, (source, destination) -> {
			if (moves.incrementAndGet() == 2) throw new IOException("forced publication failure " + destination);
			atomicMove(source, destination);
		});
		ProxyConfigurationFileService.Document current = failing.read(ProxyConfigurationFileService.FILE_NAME);

		assertThrows(ProxyConfigurationFileService.ApplyFailureException.class,
				() -> failing.apply(ProxyConfigurationFileService.FILE_NAME,
						current.content().replace("false", "true"), current.revision()));
		assertEquals(original, Files.readString(file));
		try (java.util.stream.Stream<Path> files = Files.list(directory)) {
			assertFalse(files.anyMatch(path -> path.getFileName().toString().startsWith(".control-proxy-")));
		}

		ProxyConfigurationFileService retry = service(file);
		ProxyConfigurationFileService.ApplyResult applied = retry.apply(ProxyConfigurationFileService.FILE_NAME,
				retry.read(ProxyConfigurationFileService.FILE_NAME).content().replace("false", "true"),
				retry.read(ProxyConfigurationFileService.FILE_NAME).revision());
		assertTrue(Files.readString(file).contains("Debug: true"));
		assertFalse(applied.rolledBack());
	}

	@Test
	void publishedDurabilityFailureRollsBackOriginal() throws Exception {
		Path file = write("BungeeMethod: PLUGINMESSAGING\nDebug: false\n");
		try {
			Files.setPosixFilePermissions(file, java.nio.file.attribute.PosixFilePermissions.fromString("rw-r-----"));
		} catch (UnsupportedOperationException ignored) { }
		String original = Files.readString(file);
		AtomicInteger moves = new AtomicInteger();
		ProxyConfigurationFileService failing = new ProxyConfigurationFileService(file, (source, destination) -> {
			atomicMove(source, destination);
			if (moves.incrementAndGet() == 2) {
				throw new com.bencodez.votingplugin.util.DurableFiles.PublishedException(
						new IOException("forced directory sync failure"));
			}
		});
		ProxyConfigurationFileService.Document current = failing.read(ProxyConfigurationFileService.FILE_NAME);

		ProxyConfigurationFileService.ApplyFailureException failure = assertThrows(
				ProxyConfigurationFileService.ApplyFailureException.class,
				() -> failing.apply(ProxyConfigurationFileService.FILE_NAME,
						current.content().replace("false", "true"), current.revision()));

		assertTrue(failure.rolledBack());
		assertEquals(original, Files.readString(file));
		try {
			assertEquals("rw-r-----", java.nio.file.attribute.PosixFilePermissions.toString(
					Files.getPosixFilePermissions(file)));
		} catch (UnsupportedOperationException ignored) { }
	}

	@Test
	void cleansTheFirstTemporaryFileWhenBackupStagingCannotBeCreated() throws Exception {
		Path file = write("BungeeMethod: PLUGINMESSAGING\nDebug: false\n");
		AtomicInteger calls = new AtomicInteger();
		ProxyConfigurationFileService service = new ProxyConfigurationFileService(file,
				ProxyConfigurationFileServiceTest::atomicMove, (parent, prefix, suffix) -> {
					if (calls.incrementAndGet() == 2) throw new IOException("forced backup-stage failure");
					return Files.createTempFile(parent, prefix, suffix);
				});
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);

		assertThrows(ProxyConfigurationFileService.ApplyFailureException.class,
				() -> service.apply(ProxyConfigurationFileService.FILE_NAME,
						current.content().replace("false", "true"), current.revision()));
		assertEquals("BungeeMethod: PLUGINMESSAGING\nDebug: false\n", Files.readString(file));
		try (java.util.stream.Stream<Path> files = Files.list(directory)) {
			assertFalse(files.anyMatch(path -> path.getFileName().toString().startsWith(".control-proxy-")));
		}
	}

	@Test
	void rejectsDuplicateKeysAliasesInvalidPlaceholderAndStaleRevision() throws Exception {
		Path file = write("BungeeMethod: PLUGINMESSAGING\nDebug: false\n");
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);

		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME, "Debug: true\nDebug: false\n"));
		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME, "Base: &base {Debug: true}\nCopy: *base\n"));
		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME,
						"Debug: " + ProxyConfigurationFileService.REDACTED + "\n"));
		Files.writeString(file, "BungeeMethod: REDIS\nDebug: false\n");
		assertThrows(ProxyConfigurationFileService.StaleRevisionException.class,
				() -> service.apply(ProxyConfigurationFileService.FILE_NAME, current.content(), current.revision()));
	}

	@Test
	void rejectsSymlinkTargetsAndInvalidUtf8() throws Exception {
		Path real = directory.resolve("real.yml");
		Files.writeString(real, "Debug: false\n");
		Path linked = directory.resolve("bungeeconfig.yml");
		try {
			Files.createSymbolicLink(linked, real.getFileName());
		} catch (UnsupportedOperationException | IOException unsupported) {
			return;
		}
		assertThrows(IOException.class,
				() -> service(linked).read(ProxyConfigurationFileService.FILE_NAME));
		Files.delete(linked);
		Files.write(linked, new byte[] {(byte) 0xc3, (byte) 0x28});
		assertThrows(IOException.class,
				() -> service(linked).read(ProxyConfigurationFileService.FILE_NAME));
	}

	private Path write(String content) throws IOException {
		Path file = directory.resolve("bungeeconfig.yml");
		Files.writeString(file, content, StandardCharsets.UTF_8);
		return file;
	}

	private static ProxyConfigurationFileService service(Path file) {
		return new ProxyConfigurationFileService(file, ProxyConfigurationFileServiceTest::atomicMove);
	}

	private static void atomicMove(Path source, Path destination) throws IOException {
		Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
	}
}
