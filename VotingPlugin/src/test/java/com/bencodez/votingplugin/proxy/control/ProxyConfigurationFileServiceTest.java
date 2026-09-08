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
				  ClientID: private-client
				  BrokerURL: ssl://broker.internal:8883
				  Username: mqtt-user
				  Password: mqtt-password
				  Prefix: private-prefix
				Control:
				  Endpoint: http://control.internal:8080
				  CredentialFile: control/credential.txt
				  Hosted:
				    JarFile: control/control.jar
				    DataDirectory: control/data
				    Host: control-host.internal
				    Port: 8081
				MultiProxySocketHost:
				  Host: socket.internal
				  Port: 1234
				MultiProxyServers:
				  second:
				    Host: second.internal
				    Port: 1235
				BungeeMethod: PLUGINMESSAGING
				""");

		String content = service(file).read(ProxyConfigurationFileService.FILE_NAME).content();

		assertFalse(content.contains("db.internal"));
		assertFalse(content.contains("voting"));
		assertFalse(content.contains("admin"));
		assertFalse(content.contains("secret"));
		assertFalse(content.contains("private-client"));
		assertFalse(content.contains("broker.internal"));
		assertFalse(content.contains("mqtt-user"));
		assertFalse(content.contains("mqtt-password"));
		assertFalse(content.contains("private-prefix"));
		assertFalse(content.contains("control.internal"));
		assertFalse(content.contains("control-host.internal"));
		assertFalse(content.contains("socket.internal"));
		assertFalse(content.contains("second.internal"));
		assertFalse(content.contains("control/credential.txt"));
		assertFalse(content.contains("control/control.jar"));
		assertFalse(content.contains("control/data"));
		assertFalse(content.contains("redis.internal"));
		assertFalse(content.contains("redis-secret"));
		assertTrue(content.contains(ProxyConfigurationFileService.REDACTED));
	}

	@Test
	void masksAndRestoresDottedCredentialFieldNames() throws Exception {
		Path file = write("""
				API.Key: api-key-secret
				Access.Key: access-key-secret
				Client.Secret: client-secret
				Pass.Phrase: pass-phrase-secret
				Database.User: service_account
				DatabaseUsername: database_operator
				DatabaseUserName: database_named_operator
				Database.UserName: database_dotted_operator
				MySQLUserName: mysql_named_operator
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("api-key-secret"));
		assertFalse(current.content().contains("access-key-secret"));
		assertFalse(current.content().contains("client-secret"));
		assertFalse(current.content().contains("pass-phrase-secret"));
		assertFalse(current.content().contains("service_account"));
		assertFalse(current.content().contains("database_operator"));
		assertFalse(current.content().contains("database_named_operator"));
		assertFalse(current.content().contains("database_dotted_operator"));
		assertFalse(current.content().contains("mysql_named_operator"));

		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("API.Key: api-key-secret"));
		assertTrue(preview.resolvedContent().contains("Access.Key: access-key-secret"));
		assertTrue(preview.resolvedContent().contains("Client.Secret: client-secret"));
		assertTrue(preview.resolvedContent().contains("Pass.Phrase: pass-phrase-secret"));
		assertTrue(preview.resolvedContent().contains("Database.User: service_account"));
		assertTrue(preview.resolvedContent().contains("DatabaseUsername: database_operator"));
		assertTrue(preview.resolvedContent().contains("DatabaseUserName: database_named_operator"));
		assertTrue(preview.resolvedContent().contains("Database.UserName: database_dotted_operator"));
		assertTrue(preview.resolvedContent().contains("MySQLUserName: mysql_named_operator"));

		service.apply(ProxyConfigurationFileService.FILE_NAME, proposal, current.revision());
		assertTrue(Files.readString(file).contains("Debug: true"));
	}

	@Test
	void masksAndRestoresPunctuationDelimitedCredentialFieldNames() throws Exception {
		Path file = write("""
				API/Key: api-slash-secret
				Private/Key: private-slash-secret
				Auth|Token: token-punctuation-secret
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("api-slash-secret"));
		assertFalse(current.content().contains("private-slash-secret"));
		assertFalse(current.content().contains("token-punctuation-secret"));

		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME,
				current.content().replace("Debug: false", "Debug: true"));
		assertTrue(preview.resolvedContent().contains("API/Key: api-slash-secret"));
		assertTrue(preview.resolvedContent().contains("Private/Key: private-slash-secret"));
		assertTrue(preview.resolvedContent().contains("Auth|Token: token-punctuation-secret"));
	}

	@Test
	void masksAndRestoresCompoundInfrastructureScalarNames() throws Exception {
		Path file = write("""
				ControlEndpoint: https://control.internal
				RedisHost: cache.internal
				DatabaseServer: database.internal
				RedisServer: redis.internal
				BungeeMethod: PLUGINMESSAGING
				DedicatedVotingProxy: true
				ProxyServerName: public-name
				MultiProxySupport: true
				HttpTransport: enabled
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("control.internal"));
		assertFalse(current.content().contains("cache.internal"));
		assertFalse(current.content().contains("database.internal"));
		assertFalse(current.content().contains("redis.internal"));
		assertTrue(current.content().contains("BungeeMethod: PLUGINMESSAGING"));
		assertTrue(current.content().contains("DedicatedVotingProxy: true"));
		assertTrue(current.content().contains("ProxyServerName: public-name"));
		assertTrue(current.content().contains("MultiProxySupport: true"));
		assertTrue(current.content().contains("HttpTransport: enabled"));

		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("ControlEndpoint: https://control.internal"));
		assertTrue(preview.resolvedContent().contains("RedisHost: cache.internal"));
		assertTrue(preview.resolvedContent().contains("DatabaseServer: database.internal"));
		assertTrue(preview.resolvedContent().contains("RedisServer: redis.internal"));
	}

	@Test
	void masksAndRestoresCompoundCredentialFields() throws Exception {
		Path file = write("""
				AuthToken: auth-token-value
				AccessToken: access-token-value
				Credential: credential-value
				PrivateKey: plain-private-material
				Private Key: spaced-private-key-value
				Access Key: spaced-access-key-value
				API Key: spaced-api-key-value
				KeyPassphrase: passphrase-value
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("auth-token-value"));
		assertFalse(current.content().contains("access-token-value"));
		assertFalse(current.content().contains("credential-value"));
		assertFalse(current.content().contains("plain-private-material"));
		assertFalse(current.content().contains("spaced-private-key-value"));
		assertFalse(current.content().contains("spaced-access-key-value"));
		assertFalse(current.content().contains("spaced-api-key-value"));
		assertFalse(current.content().contains("passphrase-value"));

		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("AuthToken: auth-token-value"));
		assertTrue(preview.resolvedContent().contains("AccessToken: access-token-value"));
		assertTrue(preview.resolvedContent().contains("Credential: credential-value"));
		assertTrue(preview.resolvedContent().contains("PrivateKey: plain-private-material"));
		assertTrue(preview.resolvedContent().contains("Private Key: spaced-private-key-value"));
		assertTrue(preview.resolvedContent().contains("Access Key: spaced-access-key-value"));
		assertTrue(preview.resolvedContent().contains("API Key: spaced-api-key-value"));
		assertTrue(preview.resolvedContent().contains("KeyPassphrase: passphrase-value"));
	}

	@Test
	void previewAgainstSnapshotDoesNotResolveMarkersFromALaterFileVersion() throws Exception {
		String revisionA = "AuthToken: secret-a\nDebug: false\n";
		Path file = write(revisionA);
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		Files.writeString(file, "AuthToken: secret-b\nDebug: false\n");

		ProxyConfigurationFileService.Preview preview = service.previewAgainstSnapshot(
				current.content().replace("Debug: false", "Debug: true"), revisionA);

		assertTrue(preview.resolvedContent().contains("AuthToken: secret-a"));
		assertFalse(preview.resolvedContent().contains("secret-b"));
	}

	@Test
	void masksHostedControlDownloadUrlAndRestoresItForEdits() throws Exception {
		String downloadUrl = "https://control.internal/download/token-value";
		Path file = write("""
				Control:
				  Hosted:
				    DownloadUrl: %s
				Debug: false
				""".formatted(downloadUrl));
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains(downloadUrl));
		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains(downloadUrl));
		service.apply(ProxyConfigurationFileService.FILE_NAME, proposal, current.revision());
		assertTrue(Files.readString(file).contains(downloadUrl));
	}

	@Test
	void masksAndRestoresEverySupportedDatabaseLayout() throws Exception {
		Path file = write("""
				Host: root.internal
				Port: 3306
				Database: rootdb
				Name: legacy_table
				Username: rootuser
				MySQL:
				  Host: mysql.internal
				  Username: mysqluser
				VoteCache:
				  Host: vote-cache.internal
				  Username: cacheuser
				NonVotedCache:
				  Host: non-voted.internal
				  Database: nonvoted
				VoteLogging:
				  Host: vote-log.internal
				  Line: '&useSSL=true'
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("root.internal"));
		assertFalse(current.content().contains("legacy_table"));
		assertFalse(current.content().contains("mysql.internal"));
		assertFalse(current.content().contains("vote-cache.internal"));
		assertFalse(current.content().contains("non-voted.internal"));
		assertFalse(current.content().contains("vote-log.internal"));
		assertFalse(current.content().contains("cacheuser"));
		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		service.apply(ProxyConfigurationFileService.FILE_NAME, proposal, current.revision());

		assertTrue(preview.resolvedContent().contains("vote-cache.internal"));
		String applied = Files.readString(file);
		assertTrue(applied.contains("root.internal"));
		assertTrue(applied.contains("legacy_table"));
		assertTrue(applied.contains("mysql.internal"));
		assertTrue(applied.contains("non-voted.internal"));
		assertTrue(applied.contains("vote-log.internal"));
		assertTrue(applied.contains("Debug: true"));
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
				  - MQTT:
				      BrokerURL: ssl://sequence-broker.internal:8883 # sequence-broker.internal
				      Username: sequence-mqtt-user
				      Prefix: sequence-prefix
				  - Control:
				      Endpoint: http://sequence-control.internal:8080 # sequence-control.internal
				      Hosted:
				        Host: sequence-control-host.internal
				        Port: 8081
				Endpoints:
				  - jdbc:mysql://sequence-user:sequence-password@db.internal/votes # sequence-password
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("sequence-secret"));
		assertFalse(current.content().contains("redis.internal"));
		assertFalse(current.content().contains("nested-password"));
		assertFalse(current.content().contains("sequence-broker.internal"));
		assertFalse(current.content().contains("sequence-mqtt-user"));
		assertFalse(current.content().contains("sequence-prefix"));
		assertFalse(current.content().contains("sequence-control.internal"));
		assertFalse(current.content().contains("sequence-control-host.internal"));
		assertFalse(current.content().contains("sequence-user"));
		assertFalse(current.content().contains("sequence-password"));
		assertTrue(current.content().contains("SSL: true"));
		assertTrue(current.content().contains(ProxyConfigurationFileService.REDACTED));

		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("Authorization: sequence-secret # sequence-secret"));
		assertTrue(preview.resolvedContent().contains("Host: redis.internal # redis.internal"));
		assertTrue(preview.resolvedContent().contains("Password: nested-password"));
		assertTrue(preview.resolvedContent().contains("BrokerURL: ssl://sequence-broker.internal:8883 # sequence-broker.internal"));
		assertTrue(preview.resolvedContent().contains("Username: sequence-mqtt-user"));
		assertTrue(preview.resolvedContent().contains("Prefix: sequence-prefix"));
		assertTrue(preview.resolvedContent().contains("Endpoint: http://sequence-control.internal:8080 # sequence-control.internal"));
		assertTrue(preview.resolvedContent().contains("Host: sequence-control-host.internal"));
		assertTrue(preview.resolvedContent().contains("Port: 8081"));
		assertTrue(preview.resolvedContent().contains("jdbc:mysql://sequence-user:sequence-password@db.internal/votes # sequence-password"));
		assertTrue(preview.resolvedContent().contains("SSL: true"));
		service.apply(ProxyConfigurationFileService.FILE_NAME, proposal, current.revision());
		String applied = Files.readString(file);
		assertTrue(applied.contains("Authorization: sequence-secret # sequence-secret"));
		assertTrue(applied.contains("Host: redis.internal # redis.internal"));
		assertTrue(applied.contains("Password: nested-password"));
		assertTrue(applied.contains("BrokerURL: ssl://sequence-broker.internal:8883 # sequence-broker.internal"));
		assertTrue(applied.contains("Endpoint: http://sequence-control.internal:8080 # sequence-control.internal"));
		assertTrue(applied.contains("jdbc:mysql://sequence-user:sequence-password@db.internal/votes # sequence-password"));
		assertTrue(applied.contains("Debug: true"));
	}

	@Test
	void rejectsReorderingSecretBearingSequenceEntriesButAllowsInPlaceEdits() throws Exception {
		Path file = write("""
				Hooks:
				  - Name: alpha
				    Password: alpha-secret
				    Enabled: true
				  - Name: beta
				    Password: beta-secret
				    Enabled: true
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		String unchangedOrder = current.content().replace("Debug: false", "Debug: true");
		assertTrue(service.preview(ProxyConfigurationFileService.FILE_NAME, unchangedOrder).resolvedContent()
				.contains("Password: alpha-secret"));
		String rotatedSecret = current.content().replace("Password: " + ProxyConfigurationFileService.REDACTED,
				"Password: rotated-secret");
		assertTrue(service.preview(ProxyConfigurationFileService.FILE_NAME, rotatedSecret).resolvedContent()
				.contains("Password: rotated-secret"));
		String publicEdit = current.content().replaceFirst("Enabled: true", "Enabled: false");
		String publicEditResolved = service.preview(ProxyConfigurationFileService.FILE_NAME, publicEdit).resolvedContent();
		assertTrue(publicEditResolved.contains("Enabled: false"));
		assertTrue(publicEditResolved.contains("Password: alpha-secret"));
		String reordered = current.content().replace("Name: alpha", "Name: __swapped__")
				.replace("Name: beta", "Name: alpha").replace("Name: __swapped__", "Name: beta");
		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME, reordered));
	}

	@Test
	void allowsAddingOrRemovingNonSecretFieldsFromIdentifiedSecretEntries() throws Exception {
		Path file = write("""
				Hooks:
				  - Name: primary
				    Password: primary-secret
				    Enabled: true
				""");
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);

		String added = current.content().replace("Password: " + ProxyConfigurationFileService.REDACTED + "\n",
				"Password: " + ProxyConfigurationFileService.REDACTED + "\n  Description: primary\n");
		assertTrue(service.preview(ProxyConfigurationFileService.FILE_NAME, added).resolvedContent()
				.contains("Description: primary"));

		String removed = current.content().replaceFirst("  Enabled: true\\n", "");
		assertTrue(service.preview(ProxyConfigurationFileService.FILE_NAME, removed).resolvedContent()
				.contains("Password: primary-secret"));
	}

	@Test
	void rejectsPublicEditsWhenSecretBearingEntriesHaveDuplicateIdentities() throws Exception {
		Path file = write("""
				Hooks:
				  - Name: duplicate
				    Password: first-secret
				    Enabled: true
				  - Name: duplicate
				    Password: second-secret
				    Enabled: false
				""");
		ProxyConfigurationFileService service = service(file);
		String current = service.read(ProxyConfigurationFileService.FILE_NAME).content();
		String swappedPublicValues = current.replace("Enabled: true", "Enabled: __swapped__")
				.replace("Enabled: false", "Enabled: true").replace("Enabled: __swapped__", "Enabled: false");

		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME, swappedPublicValues));
		String removedPublicField = current.replaceFirst("  Enabled: true\\n", "");
		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME, removedPublicField));
	}

	@Test
	void preservesNullEntriesInSequencesAcrossReadPreviewAndApply() throws Exception {
		Path file = write("Servers: [null, active]\nEmptyEntries:\n  -\nDebug: false\n");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		service.apply(ProxyConfigurationFileService.FILE_NAME, proposal, current.revision());

		assertTrue(preview.resolvedContent().contains("null"));
		assertTrue(Files.readString(file).contains("Debug: true"));
	}

	@Test
	void masksAndRestoresPrimarySocketsEndpointsAndComments() throws Exception {
		Path file = write("""
				BungeeServer:
				  Host: proxy.internal # proxy.internal
				  Port: 1297 # listener port 1297
				SpigotServers:
				  Host:
				    Host: survival.internal # survival.internal
				    Port: 1298 # backend port 1298
				    Enabled: true
				BungeeMethod: SOCKETS
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("proxy.internal"));
		assertFalse(current.content().contains("survival.internal"));
		assertFalse(current.content().contains("1297"));
		assertFalse(current.content().contains("1298"));
		assertTrue(current.content().contains("SpigotServers:\n  Host:"));
		assertTrue(current.content().contains("Enabled: true"));

		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("Host: proxy.internal # proxy.internal"));
		assertTrue(preview.resolvedContent().contains("Port: 1297 # listener port 1297"));
		assertTrue(preview.resolvedContent().contains("Host: survival.internal # survival.internal"));
		assertTrue(preview.resolvedContent().contains("Port: 1298 # backend port 1298"));
		service.apply(ProxyConfigurationFileService.FILE_NAME, proposal, current.revision());
		String applied = Files.readString(file);
		assertTrue(applied.contains("Host: proxy.internal # proxy.internal"));
		assertTrue(applied.contains("Host: survival.internal # survival.internal"));
		assertTrue(applied.contains("Debug: true"));
	}

	@Test
	void aliasValidationUsesYamlSyntaxInsteadOfScalarOrCommentText() throws Exception {
		Path file = write("""
				General:
				  Broadcast: "Hello &aPlayer and *literal"
				  Explanation: |
				    Keep << text, &literal, and *literal unchanged.
				  "<<": quoted-key
				# use *name and &name in documentation
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertTrue(current.content().contains("Hello &aPlayer and *literal"));
		assertTrue(current.content().contains("Keep << text, &literal, and *literal unchanged."));
		assertTrue(current.content().contains("# use *name and &name in documentation"));
		assertTrue(current.content().contains("'<<': quoted-key")
				|| current.content().contains("\"<<\": quoted-key"));
		String proposal = current.content().replace("Debug: false", "Debug: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("Hello &aPlayer and *literal"));
		service.apply(ProxyConfigurationFileService.FILE_NAME, proposal, current.revision());
		assertTrue(Files.readString(file).contains("Debug: true"));

		assertThrows(IllegalArgumentException.class, () -> service.preview(ProxyConfigurationFileService.FILE_NAME,
				"Primary: &name value\nCopy: *name\n"));
		assertThrows(IllegalArgumentException.class, () -> service.preview(ProxyConfigurationFileService.FILE_NAME,
				"Primary: &values\n  - one\nCopy: *values\n"));
		assertThrows(IllegalArgumentException.class, () -> service.preview(ProxyConfigurationFileService.FILE_NAME,
				"Primary: &unused value\nDebug: false\n"));
		assertThrows(IllegalArgumentException.class, () -> service.preview(ProxyConfigurationFileService.FILE_NAME,
				"General:\n  <<: {Debug: true}\n"));
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
	void allowsOnlySafeSuffixRemovalFromSecretBearingLists() throws Exception {
		Path file = write("""
				Hooks:
				  - Name: primary
				    Password: primary-secret
				  - Name: removable-tail
				    Enabled: true
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		String safeSuffixRemoval = "Hooks:\n  - Name: primary\n    Password: "
				+ ProxyConfigurationFileService.REDACTED + "\nDebug: false\n";
		String rotationWithSafeSuffixRemoval = "Hooks:\n  - Name: primary\n    Password: rotated-secret\nDebug: false\n";

		ProxyConfigurationFileService.Preview rotated = service.preview(
				ProxyConfigurationFileService.FILE_NAME, rotationWithSafeSuffixRemoval);
		assertTrue(rotated.resolvedContent().contains("Password: rotated-secret"));
		assertFalse(rotated.resolvedContent().contains("removable-tail"));

		ProxyConfigurationFileService.Preview preview = service.preview(
				ProxyConfigurationFileService.FILE_NAME, safeSuffixRemoval);
		assertTrue(preview.resolvedContent().contains("Password: primary-secret"));
		assertFalse(preview.resolvedContent().contains("removable-tail"));
		service.apply(ProxyConfigurationFileService.FILE_NAME, safeSuffixRemoval, current.revision());
		assertFalse(Files.readString(file).contains("removable-tail"));

		Path secretTail = write("""
				Hooks:
				  - Name: public
				    Enabled: true
				  - Name: protected-tail
				    Password: protected-secret
				Debug: false
				""");
		ProxyConfigurationFileService protectedTailService = service(secretTail);
		assertThrows(IllegalArgumentException.class, () -> protectedTailService.preview(
				ProxyConfigurationFileService.FILE_NAME, "Hooks:\n  - Name: public\n    Enabled: true\nDebug: false\n"));

		Path shiftedSecret = write("""
				Endpoints:
				  - jdbc:mysql://old-user:old-password@db.invalid/votes
				  - public-endpoint
				""");
		ProxyConfigurationFileService shiftedSecretService = service(shiftedSecret);
		assertThrows(IllegalArgumentException.class, () -> shiftedSecretService.preview(
				ProxyConfigurationFileService.FILE_NAME, "Endpoints:\n  - public-endpoint\n"));

		ProxyConfigurationFileService.Preview scalarRotation = shiftedSecretService.preview(
				ProxyConfigurationFileService.FILE_NAME,
				"Endpoints:\n  - jdbc:mysql://new-user:new-password@db.invalid/votes\n");
		assertTrue(scalarRotation.resolvedContent().contains("jdbc:mysql://new-user:new-password@db.invalid/votes"));
		assertFalse(scalarRotation.resolvedContent().contains("public-endpoint"));
	}

	@Test
	void previewRestoresMaskedValuesAndAllowsSafeNestedAdditions() throws Exception {
		Path file = write("""
				Database:
				  Host: db.internal
				  Name: votes_table
				  Password: secret
				BungeeMethod: PLUGINMESSAGING
				""");
		ProxyConfigurationFileService service = service(file);
		String proposal = service.read(ProxyConfigurationFileService.FILE_NAME).content()
				+ "NewSection:\n  Enabled: true\n";
		assertFalse(proposal.contains("votes_table"));

		ProxyConfigurationFileService.Preview preview = service.preview(
				ProxyConfigurationFileService.FILE_NAME, proposal);

		assertTrue(preview.resolvedContent().contains("db.internal"));
		assertTrue(preview.resolvedContent().contains("votes_table"));
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
	void masksInfrastructureLabelsAndOrdinaryConnectionUrlsInComments() throws Exception {
		Path file = write("""
				# failover host: replica.internal
				# ProxyHost: proxy.internal
				# ControlEndpoint: https://control-alt.internal
				# RedisHost: cache-alt.internal
				# PrimaryRedisHost: redis-primary.internal
				# FailoverDatabaseHost: db-failover.internal
				# MultiProxyRedisHost: redis-multiproxy.internal
				# MultiProxySocketHost: socket-multiproxy.internal
				# BungeeServerHost: bungee.internal
				# SpigotServersHost: spigot.internal
				# CustomHostName: custom-db.internal
				# FailoverDbName: votes_failover
				# DBHost: db-acronym.internal
				# DBPORT: 3306
				# REDISPORT: 6379
				# MySQLHost: mysql-acronym.internal
				# DBURL: cluster.internal:5432
				# APIURL: control-api.internal
				# AuthToken: bearer-value
				FeatureTwo: false # ApiToken: alternate-bearer
				# Credential: secret-value
				# AccessKey: access-value
				# PrivateKey: private-value
				# SSHPrivateKeyData: private-key-data
				# AWSAccessKeyId: aws-access-id
				# ApiKeyId: api-key-id
				# IP: 10.0.0.8
				# IPv4: 10.0.0.9
				# IPv6: fd00::10
				# connect through 10.0.0.10
				# fail over to bare-db.internal
				# Note: failover host: nested-label.internal
				# management endpoint = https://control.internal:8443
				# cache connection: redis://cache.internal:6379
				Feature: false # database host: primary.internal
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("replica.internal"));
		assertFalse(current.content().contains("proxy.internal"));
		assertFalse(current.content().contains("control-alt.internal"));
		assertFalse(current.content().contains("cache-alt.internal"));
		assertFalse(current.content().contains("redis-primary.internal"));
		assertFalse(current.content().contains("db-failover.internal"));
		assertFalse(current.content().contains("redis-multiproxy.internal"));
		assertFalse(current.content().contains("socket-multiproxy.internal"));
		assertFalse(current.content().contains("bungee.internal"));
		assertFalse(current.content().contains("spigot.internal"));
		assertFalse(current.content().contains("custom-db.internal"));
		assertFalse(current.content().contains("votes_failover"));
		assertFalse(current.content().contains("db-acronym.internal"));
		assertFalse(current.content().contains("3306"));
		assertFalse(current.content().contains("6379"));
		assertFalse(current.content().contains("mysql-acronym.internal"));
		assertFalse(current.content().contains("cluster.internal"));
		assertFalse(current.content().contains("control-api.internal"));
		assertFalse(current.content().contains("bearer-value"));
		assertFalse(current.content().contains("alternate-bearer"));
		assertFalse(current.content().contains("secret-value"));
		assertFalse(current.content().contains("access-value"));
		assertFalse(current.content().contains("private-value"));
		assertFalse(current.content().contains("private-key-data"));
		assertFalse(current.content().contains("aws-access-id"));
		assertFalse(current.content().contains("api-key-id"));
		assertFalse(current.content().contains("10.0.0.8"));
		assertFalse(current.content().contains("10.0.0.9"));
		assertFalse(current.content().contains("fd00::10"));
		assertFalse(current.content().contains("10.0.0.10"));
		assertFalse(current.content().contains("bare-db.internal"));
		assertFalse(current.content().contains("nested-label.internal"));
		assertFalse(current.content().contains("control.internal"));
		assertFalse(current.content().contains("cache.internal"));
		assertFalse(current.content().contains("primary.internal"));
		assertTrue(current.content().contains(ProxyConfigurationFileService.REDACTED));

		String proposal = current.content().replace("Feature: false", "Feature: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("replica.internal"));
		assertTrue(preview.resolvedContent().contains("ProxyHost: proxy.internal"));
		assertTrue(preview.resolvedContent().contains("ControlEndpoint: https://control-alt.internal"));
		assertTrue(preview.resolvedContent().contains("RedisHost: cache-alt.internal"));
		assertTrue(preview.resolvedContent().contains("PrimaryRedisHost: redis-primary.internal"));
		assertTrue(preview.resolvedContent().contains("FailoverDatabaseHost: db-failover.internal"));
		assertTrue(preview.resolvedContent().contains("MultiProxyRedisHost: redis-multiproxy.internal"));
		assertTrue(preview.resolvedContent().contains("MultiProxySocketHost: socket-multiproxy.internal"));
		assertTrue(preview.resolvedContent().contains("BungeeServerHost: bungee.internal"));
		assertTrue(preview.resolvedContent().contains("SpigotServersHost: spigot.internal"));
		assertTrue(preview.resolvedContent().contains("CustomHostName: custom-db.internal"));
		assertTrue(preview.resolvedContent().contains("FailoverDbName: votes_failover"));
		assertTrue(preview.resolvedContent().contains("DBHost: db-acronym.internal"));
		assertTrue(preview.resolvedContent().contains("DBPORT: 3306"));
		assertTrue(preview.resolvedContent().contains("REDISPORT: 6379"));
		assertTrue(preview.resolvedContent().contains("MySQLHost: mysql-acronym.internal"));
		assertTrue(preview.resolvedContent().contains("DBURL: cluster.internal:5432"));
		assertTrue(preview.resolvedContent().contains("APIURL: control-api.internal"));
		assertTrue(preview.resolvedContent().contains("AuthToken: bearer-value"));
		assertTrue(preview.resolvedContent().contains("ApiToken: alternate-bearer"));
		assertTrue(preview.resolvedContent().contains("Credential: secret-value"));
		assertTrue(preview.resolvedContent().contains("AccessKey: access-value"));
		assertTrue(preview.resolvedContent().contains("PrivateKey: private-value"));
		assertTrue(preview.resolvedContent().contains("SSHPrivateKeyData: private-key-data"));
		assertTrue(preview.resolvedContent().contains("AWSAccessKeyId: aws-access-id"));
		assertTrue(preview.resolvedContent().contains("ApiKeyId: api-key-id"));
		assertTrue(preview.resolvedContent().contains("IP: 10.0.0.8"));
		assertTrue(preview.resolvedContent().contains("IPv4: 10.0.0.9"));
		assertTrue(preview.resolvedContent().contains("IPv6: fd00::10"));
		assertTrue(preview.resolvedContent().contains("connect through 10.0.0.10"));
		assertTrue(preview.resolvedContent().contains("fail over to bare-db.internal"));
		assertTrue(preview.resolvedContent().contains("Note: failover host: nested-label.internal"));
		assertTrue(preview.resolvedContent().contains("https://control.internal:8443"));
		assertTrue(preview.resolvedContent().contains("redis://cache.internal:6379"));
		assertTrue(preview.resolvedContent().contains("primary.internal"));
	}

	@Test
	void masksDelimiterFreeSingleLabelInfrastructureHostsInComments() throws Exception {
		Path file = write("""
				# alternate Redis host redis-primary
				# failover endpoint control
				# database host db-production
				# broker localhost:1883
				# port 25565
				# Control URL control-proxy
				Feature: false
				""");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		assertFalse(current.content().contains("redis-primary"));
		assertFalse(current.content().contains("control"));
		assertFalse(current.content().contains("db-production"));
		assertFalse(current.content().contains("localhost"));
		assertFalse(current.content().contains("25565"));
		assertFalse(current.content().contains("control-proxy"));
		assertTrue(current.content().contains(ProxyConfigurationFileService.REDACTED));

		String proposal = current.content().replace("Feature: false", "Feature: true");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);
		assertTrue(preview.resolvedContent().contains("alternate Redis host redis-primary"));
		assertTrue(preview.resolvedContent().contains("failover endpoint control"));
		assertTrue(preview.resolvedContent().contains("database host db-production"));
		assertTrue(preview.resolvedContent().contains("broker localhost:1883"));
		assertTrue(preview.resolvedContent().contains("port 25565"));
		assertTrue(preview.resolvedContent().contains("Control URL control-proxy"));
	}

	@Test
	void allowsSafeEndAppendsToSecretBearingLists() throws Exception {
		Path file = write("""
				Hooks:
				  - Name: primary
				    Password: primary-secret
				  - Name: secondary
				    Password: secondary-secret
				Debug: false
				""");
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);

		String proposal = current.content().replace("Debug: false",
				"- Name: tertiary\n  Password: tertiary-secret\nDebug: false");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);

		assertTrue(preview.resolvedContent().contains("Name: primary"));
		assertTrue(preview.resolvedContent().contains("Password: primary-secret"));
		assertTrue(preview.resolvedContent().contains("Name: secondary"));
		assertTrue(preview.resolvedContent().contains("Password: secondary-secret"));
		assertTrue(preview.resolvedContent().contains("Name: tertiary"));
		assertTrue(preview.resolvedContent().contains("Password: tertiary-secret"));

		String markerAppend = current.content().replace("Debug: false",
				"- Name: tertiary\n  Password: " + ProxyConfigurationFileService.REDACTED + "\nDebug: false");
		assertThrows(IllegalArgumentException.class,
				() -> service.preview(ProxyConfigurationFileService.FILE_NAME, markerAppend));
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
	void previewAllowsAnExplicitReplacementForASecretSequenceItem() throws Exception {
		Path file = write("Endpoints:\n  - jdbc:mysql://old-user:old-password@db.invalid/votes\n");
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		String proposal = current.content().replace(ProxyConfigurationFileService.REDACTED,
				"jdbc:mysql://new-user:new-password@db.invalid/votes");

		ProxyConfigurationFileService.Preview preview = service.preview(
				ProxyConfigurationFileService.FILE_NAME, proposal);

		assertTrue(preview.resolvedContent().contains("jdbc:mysql://new-user:new-password@db.invalid/votes"));
	}

	@Test
	void restoresCommentsForDottedAndNestedKeysWithoutLocationCollisions() throws Exception {
		Path file = write("\"Database.Password\": first-secret # first secret comment\n"
				+ "Database:\n  Password: second-secret # second secret comment\n");
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);

		ProxyConfigurationFileService.Preview preview = service.preview(
				ProxyConfigurationFileService.FILE_NAME, current.content());

		assertTrue(preview.resolvedContent().contains("first-secret # first secret comment"));
		assertTrue(preview.resolvedContent().contains("second-secret # second secret comment"));
	}

	@Test
	void masksBareAndNestedWebhookFields() throws Exception {
		Path file = write("Webhook: https://hooks.example.invalid/services/private-token\n"
				+ "Notifications:\n  Webhook: https://notify.example.invalid/private-token\n"
				+ "Webhooks:\n  Primary:\n    URL: https://primary.example.invalid/private-token\n"
				+ "  Secondary:\n    URI: https://secondary.example.invalid/private-token\n"
				+ "Debug: false\n");

		String content = service(file).read(ProxyConfigurationFileService.FILE_NAME).content();

		assertFalse(content.contains("hooks.example.invalid"));
		assertFalse(content.contains("notify.example.invalid"));
		assertFalse(content.contains("primary.example.invalid"));
		assertFalse(content.contains("secondary.example.invalid"));
		assertEquals(4, content.lines().filter(line -> line.contains(ProxyConfigurationFileService.REDACTED)).count());
	}

	@Test
	void permitsPublicCommentInsertionAroundRedactedCommentButRequiresSameMarkerOwners() throws Exception {
		Path file = write("Database:\n  # Password: secret\n  Password: secret\nDebug: false\n");
		ProxyConfigurationFileService service = service(file);
		String current = service.read(ProxyConfigurationFileService.FILE_NAME).content();
		String withPublicComment = current.replace("# " + ProxyConfigurationFileService.REDACTED,
				"# public documentation\n  # " + ProxyConfigurationFileService.REDACTED);

		assertTrue(service.preview(ProxyConfigurationFileService.FILE_NAME, withPublicComment).resolvedContent()
				.contains("Password: secret"));
		assertThrows(IllegalArgumentException.class, () -> service.preview(ProxyConfigurationFileService.FILE_NAME,
				withPublicComment.replace("# " + ProxyConfigurationFileService.REDACTED, "# removed")));
	}

	@Test
	void comparesRedactedCommentOwnersRegardlessOfMappingOrder() throws Exception {
		Path file = write("Alpha:\n  Enabled: true # Password: alpha-secret\n"
				+ "Beta:\n  Enabled: true # Token: beta-secret\n");
		ProxyConfigurationFileService service = service(file);

		String reordered = "Beta:\n  Enabled: false # " + ProxyConfigurationFileService.REDACTED + "\n"
				+ "Alpha:\n  Enabled: true # " + ProxyConfigurationFileService.REDACTED + "\n";
		ProxyConfigurationFileService.Preview preview = service.preview(
				ProxyConfigurationFileService.FILE_NAME, reordered);
		assertTrue(preview.resolvedContent().contains("Token: beta-secret"));
		assertTrue(preview.resolvedContent().contains("Password: alpha-secret"));

		String moved = "Beta:\n  Enabled: false # " + ProxyConfigurationFileService.REDACTED
				+ "\n  Extra: true # " + ProxyConfigurationFileService.REDACTED + "\n"
				+ "Alpha:\n  Enabled: true\n";
		assertThrows(IllegalArgumentException.class, () -> service.preview(
				ProxyConfigurationFileService.FILE_NAME, moved));
	}

	@Test
	void changeDescriptionsDistinguishDottedKeysFromNestedPaths() throws Exception {
		Path file = write("\"a.b.x\": dotted\na:\n  b:\n    x: nested\n");
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);

		String proposal = current.content().replace("dotted", "changed-dotted").replace("x: nested", "x: changed-nested");
		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME, proposal);

		assertEquals(java.util.List.of("changed a.b.x", "changed [\"a.b.x\"]"), preview.changes());
	}

	@Test
	void changeDescriptionsPreserveYamlValueTypes() throws Exception {
		Path file = write("Enabled: \"true\"\nPort: \"1\"\n");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Preview preview = service.preview(ProxyConfigurationFileService.FILE_NAME,
				"Enabled: true\nPort: 1\n");

		assertEquals(java.util.List.of("changed Enabled", "changed Port"), preview.changes());
	}

	@Test
	void changeDescriptionsReportEmptyMappingAdditionsAndRemovals() throws Exception {
		Path file = write("Enabled: true\nSection: {}\n");
		ProxyConfigurationFileService service = service(file);

		ProxyConfigurationFileService.Preview removal = service.preview(ProxyConfigurationFileService.FILE_NAME,
				"Enabled: true\n");
		assertEquals(java.util.List.of("removed Section"), removal.changes());

		Files.writeString(file, "Enabled: true\n");
		ProxyConfigurationFileService.Preview addition = service.preview(ProxyConfigurationFileService.FILE_NAME,
				"Enabled: true\nSection: {}\n");
		assertEquals(java.util.List.of("added Section"), addition.changes());
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
	void preparedApplyBindsResolutionAndPublicationToOneRevisionCheckedSnapshot() throws Exception {
		String original = "Password: secret-b\nDebug: false\n";
		Path file = write(original);
		ProxyConfigurationFileService service = service(file);
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);
		String proposal = current.content().replace("Debug: false", "Debug: true");

		Files.writeString(file, "Password: secret-a\nDebug: false\n");
		assertThrows(ProxyConfigurationFileService.StaleRevisionException.class,
				() -> service.prepareApply(ProxyConfigurationFileService.FILE_NAME, proposal, current.revision()));

		Files.writeString(file, original);
		ProxyConfigurationFileService.PreparedApply prepared = service.prepareApply(
				ProxyConfigurationFileService.FILE_NAME, proposal, current.revision());
		assertTrue(prepared.preview().resolvedContent().contains("Password: secret-b"));
		ProxyConfigurationFileService.ApplyResult applied = service.apply(prepared);

		assertTrue(Files.readString(file).contains("Password: secret-b"));
		assertTrue(Files.readString(file).contains("Debug: true"));
		assertEquals(ProxyConfigurationFileService.revision(prepared.preview().resolvedContent()),
				applied.document().revision());
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
	void doesNotRollbackOverAnAdministratorEditDuringRollbackStaging() throws Exception {
		Path file = write("BungeeMethod: PLUGINMESSAGING\nDebug: false\n");
		AtomicInteger moves = new AtomicInteger(), tempFiles = new AtomicInteger();
		ProxyConfigurationFileService service = new ProxyConfigurationFileService(file, (source, destination) -> {
			atomicMove(source, destination);
			if (moves.incrementAndGet() == 2)
				throw new com.bencodez.votingplugin.util.DurableFiles.PublishedException(
						new IOException("forced publication failure"));
		}, (parent, prefix, suffix) -> {
			Path temporary = Files.createTempFile(parent, prefix, suffix);
			if (tempFiles.incrementAndGet() == 3)
				Files.writeString(file, "BungeeMethod: ADMIN_EDIT\nDebug: true\n");
			return temporary;
		});
		ProxyConfigurationFileService.Document current = service.read(ProxyConfigurationFileService.FILE_NAME);

		ProxyConfigurationFileService.ApplyFailureException failure = assertThrows(
				ProxyConfigurationFileService.ApplyFailureException.class,
				() -> service.apply(ProxyConfigurationFileService.FILE_NAME,
						current.content().replace("false", "true"), current.revision()));

		assertFalse(failure.rolledBack());
		assertEquals("BungeeMethod: ADMIN_EDIT\nDebug: true\n", Files.readString(file));
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
