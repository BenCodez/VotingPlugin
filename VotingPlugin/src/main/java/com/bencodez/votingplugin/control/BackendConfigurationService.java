package com.bencodez.votingplugin.control;

import java.io.IOException;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.channels.Channels;
import java.nio.channels.SeekableByteChannel;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.HexFormat;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;

import com.bencodez.votingplugin.proxy.BungeeMethod;
import com.bencodez.votingplugin.util.DurableFiles;

/** Safe, revisioned access to every user-facing VotingPlugin YAML configuration file. */
public final class BackendConfigurationService {
	public static final String REDACTED = "__VOTINGPLUGIN_CONTROL_REDACTED__";
	public static final int MAX_CONTENT_BYTES = 512 * 1024;
	private static final Set<String> TOP_LEVEL = Set.of("Config.yml", "VoteSites.yml", "SpecialRewards.yml",
			"GUI.yml", "Shop.yml", "BungeeSettings.yml");
	private static final Pattern COMMENT_SECRET = Pattern.compile(
			"(?i)([\"']?\\b(?:[\\w-]*(?:password|secret)[\\w-]*|token|api[ _.-]?key|authorization|[\\w.-]*webhook[ _.-]?url)"
					+ "\\b[\"']?\\s*[:=]\\s*)(.*)$");
	private static final Pattern SECRET_PATH_URL = Pattern.compile("(?i)([\"']?\\burl\\b[\"']?\\s*[:=]\\s*)(.*)$");
	private static final Pattern BLOCK_SCALAR_INDICATOR = Pattern.compile("[|>](?:[+-][1-9]?|[1-9][+-]?)?");

	private final Path dataDirectory;
	private final ApplyAction reload;
	private final MoveAction mover;

	public BackendConfigurationService(Path dataDirectory, ReloadAction reload) {
		this(dataDirectory, ignored -> reload.run());
	}

	public BackendConfigurationService(Path dataDirectory, ApplyAction reload) {
		this(dataDirectory, reload, BackendConfigurationService::move);
	}

	BackendConfigurationService(Path dataDirectory, ApplyAction reload, MoveAction mover) {
		this.dataDirectory = dataDirectory.toAbsolutePath().normalize();
		this.reload = reload;
		this.mover = mover;
	}

	public Document read(String fileName) throws IOException {
		Path path = resolve(fileName);
		String raw = readRaw(path, false);
		YamlConfiguration yaml = parse(raw);
		return new Document(fileName, mask(yaml), revision(raw));
	}

	public Preview preview(String fileName, String proposedContent) throws IOException {
		Path path = resolve(fileName);
		String current = readRaw(path, false);
		return preview(fileName, proposedContent, current);
	}

	Document proposedDocument(Preview preview) {
		return new Document(preview.fileName(), mask(parse(preview.resolvedContent())),
				revision(preview.resolvedContent()));
	}

	public ApplyResult apply(String fileName, String proposedContent, String expectedRevision) throws IOException {
		Path target = resolve(fileName);
		String current = readRaw(target, false);
		if (expectedRevision == null || !revision(current).equals(expectedRevision)) throw new StaleRevisionException();
		Preview preview = preview(fileName, proposedContent, current);
		Path backup = target.resolveSibling(target.getFileName() + ".control-backup");
		Path staging = Files.createTempFile(target.getParent(), ".control-", ".yml");
		Path backupStaging = Files.createTempFile(target.getParent(), ".control-backup-", ".yml");
		boolean installed = false;
		try {
			Files.writeString(staging, preview.resolvedContent(), StandardCharsets.UTF_8,
					StandardOpenOption.TRUNCATE_EXISTING);
			rejectSymbolicBackup(backup);
			Files.writeString(backupStaging, current, StandardCharsets.UTF_8,
					StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
			if (!revision(readRaw(target, false)).equals(expectedRevision)) throw new StaleRevisionException();
			mover.move(backupStaging, backup);
			if (!revision(readRaw(target, false)).equals(expectedRevision)) throw new StaleRevisionException();
			try {
				mover.move(staging, target);
				installed = true;
			} catch (DurableFiles.PublishedException published) {
				installed = true;
				throw published;
			}
			reload.run(fileName);
			String applied = readRaw(target, false);
			String installedRevision = revision(preview.resolvedContent());
			if (!revision(applied).equals(installedRevision)) {
				reconcileConcurrentEdit(fileName, target);
				throw new StaleRevisionException();
			}
			return new ApplyResult(new Document(fileName, mask(parse(applied)), revision(applied)),
					preview.changes(), false);
		} catch (StaleRevisionException stale) {
			throw stale;
		} catch (Exception failure) {
			boolean rolledBack = false;
			if (installed) {
				try {
					String installedRevision = revision(preview.resolvedContent());
					if (!revision(readRaw(target, false)).equals(installedRevision)) {
						throw new IOException("Managed configuration changed while reload failed; backup was not restored");
					}
					try {
						copyBackupNoFollow(backup, target, installedRevision);
					} catch (DurableFiles.PublishedException published) {
						// The backup is already active. Preserve the durability warning, but
						// still reload it so runtime and disk cannot diverge.
						failure.addSuppressed(published);
					}
					reload.run(fileName);
					rolledBack = true;
				} catch (Exception rollbackFailure) {
					failure.addSuppressed(rollbackFailure);
				}
			}
			throw new ApplyFailureException(rolledBack, failure);
		} finally {
			Files.deleteIfExists(staging);
			Files.deleteIfExists(backupStaging);
		}
	}

	private void reconcileConcurrentEdit(String fileName, Path target) throws Exception {
		for (int attempt = 0; attempt < 3; attempt++) {
			String snapshotRevision = revision(readRaw(target, false));
			reload.run(fileName);
			if (revision(readRaw(target, false)).equals(snapshotRevision)) return;
		}
		throw new StaleRevisionException();
	}

	private static void rejectSymbolicBackup(Path backup) throws IOException {
		if (Files.isSymbolicLink(backup)) throw new IOException("Symbolic configuration backups are not allowed");
	}

	private void copyBackupNoFollow(Path backup, Path target, String expectedTargetRevision) throws IOException {
		if (!Files.isRegularFile(backup, LinkOption.NOFOLLOW_LINKS)) {
			throw new IOException("Control backup is unavailable or unsafe");
		}
		Path staging = Files.createTempFile(target.getParent(), ".control-rollback-", ".yml");
		try {
			try (SeekableByteChannel source = Files.newByteChannel(backup,
					Set.of(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS))) {
				Files.copy(Channels.newInputStream(source), staging, StandardCopyOption.REPLACE_EXISTING);
			}
			if (!revision(readRaw(target, false)).equals(expectedTargetRevision)) {
				throw new IOException("Managed configuration changed while rollback was staged");
			}
			mover.move(staging, target);
		} finally {
			Files.deleteIfExists(staging);
		}
	}

	private Preview preview(String fileName, String proposedContent, String current) {
		YamlConfiguration currentYaml = parse(current);
		YamlConfiguration resolvedYaml = resolveSecrets(parse(proposedContent), currentYaml);
		if ("BungeeSettings.yml".equals(fileName) && resolvedYaml.contains("BungeeMethod")) {
			resolvedYaml.set("BungeeMethod", canonicalBungeeMethod(resolvedYaml.getString("BungeeMethod")));
		}
		String resolved = resolvedYaml.saveToString();
		ensureBounded(resolved);
		return new Preview(fileName, resolved, revision(current), changes(currentYaml, parse(resolved)));
	}

	public QuickPreview previewQuickSetup(String preset, Map<String, String> options) throws IOException {
		String fileName = quickSetupFile(preset);
		String current = readRaw(resolve(fileName), false);
		QuickProposal proposal = quickProposal(preset, options, fileName, current);
		return new QuickPreview(proposal, revision(current), changes(parse(current), parse(proposal.content())));
	}

	String proposedQuickSetupRevision(QuickPreview preview) {
		return revision(preview.proposal().content());
	}

	String currentQuickSetupRevision(String preset) throws IOException {
		return read(quickSetupFile(preset)).revision();
	}

	public ApplyResult applyQuickSetup(String preset, Map<String, String> options, String expectedRevision)
			throws IOException {
		String fileName = quickSetupFile(preset);
		String current = readRaw(resolve(fileName), false);
		if (expectedRevision == null || !revision(current).equals(expectedRevision)) throw new StaleRevisionException();
		QuickProposal proposal = quickProposal(preset, options, fileName, current);
		return apply(proposal.fileName(), proposal.content(), expectedRevision);
	}

	private static String quickSetupFile(String preset) {
		if ("standalone".equals(preset) || "proxy-backend".equals(preset)) return "BungeeSettings.yml";
		if ("vote-site".equals(preset) || "easy-reward".equals(preset)) return "VoteSites.yml";
		if ("common-settings".equals(preset)) return "Config.yml";
		if ("vote-party".equals(preset)) return "SpecialRewards.yml";
		throw new IllegalArgumentException("quick setup preset is unsupported");
	}

	private QuickProposal quickProposal(String preset, Map<String, String> options, String fileName,
			String current) {
		YamlConfiguration yaml = parse(current);
		if ("standalone".equals(preset) || "proxy-backend".equals(preset)) {
			boolean proxy = "proxy-backend".equals(preset);
			yaml.set("UseBungeecord", proxy);
			if (proxy) {
				String server = option(options, "server", "[A-Za-z0-9][A-Za-z0-9._-]{0,63}");
				yaml.set("Server", server);
				yaml.set("BungeeMethod", bungeeMethodOption(options));
			}
			return new QuickProposal(fileName, yaml.saveToString());
		}
		if ("vote-site".equals(preset)) {
			String name = option(options, "name", "[A-Za-z0-9_-]{1,64}");
			String root = "VoteSites." + name;
			yaml.set(root + ".Enabled", true);
			yaml.set(root + ".Name", options.getOrDefault("displayName", name));
			yaml.set(root + ".Priority", boundedInteger(options.getOrDefault("priority", "5"), 0, 100));
			yaml.set(root + ".Hidden", false);
			yaml.set(root + ".ServiceSite", option(options, "serviceSite", ".{1,200}"));
			yaml.set(root + ".VoteURL", option(options, "voteUrl", ".{1,500}"));
			yaml.set(root + ".VoteDelay", options.getOrDefault("voteDelay", "24h"));
			yaml.set(root + ".DisplayItem.Material", options.getOrDefault("material", "DIAMOND"));
			yaml.set(root + ".DisplayItem.Amount", 1);
			return new QuickProposal(fileName, yaml.saveToString());
		}
		if ("easy-reward".equals(preset)) {
			String scope = options == null ? "site" : options.getOrDefault("scope", "site");
			String root;
			if ("every-site".equals(scope)) {
				root = "EverySiteReward";
			} else if ("site".equals(scope)) {
				root = "VoteSites." + option(options, "name", "[A-Za-z0-9_-]{1,64}") + ".Rewards";
			} else {
				throw new IllegalArgumentException("quick setup option scope is invalid");
			}
			String command = optional(options, "command", 500);
			String message = optional(options, "message", 500);
			if (command.isBlank() && message.isBlank()) {
				throw new IllegalArgumentException("easy reward requires a command or player message");
			}
			if (!command.isBlank()) yaml.set(root + ".Commands", List.of(command));
			if (!message.isBlank()) yaml.set(root + ".Messages.Player", message);
			return new QuickProposal(fileName, yaml.saveToString());
		}
		if ("common-settings".equals(preset)) {
			yaml.set("ProcessRewards", booleanOption(options, "processRewards"));
			yaml.set("AutoCreateVoteSites", booleanOption(options, "autoCreateVoteSites"));
			yaml.set("ExtraAllSitesCheck", booleanOption(options, "extraAllSitesCheck"));
			yaml.set("CountFakeVotes", booleanOption(options, "countFakeVotes"));
			yaml.set("DisableNoServiceSiteMessage", booleanOption(options, "disableNoServiceSiteMessage"));
			yaml.set("DisableUpdateChecking", booleanOption(options, "disableUpdateChecking"));
			return new QuickProposal(fileName, yaml.saveToString());
		}
		if ("vote-party".equals(preset)) {
			yaml.set("VoteParty.Enabled", true);
			yaml.set("VoteParty.VotesRequired", boundedInteger(option(options, "votesRequired", "[0-9]{1,6}"), 1, 100000));
			yaml.set("VoteParty.GiveAllPlayers", booleanOption(options, "giveAllPlayers"));
			yaml.set("VoteParty.GiveOnlinePlayersOnly", booleanOption(options, "onlineOnly"));
			String command = optional(options, "command", 500);
			String broadcast = optional(options, "broadcast", 500);
			if (!command.isBlank()) yaml.set("VoteParty.Rewards.Commands", List.of(command));
			if (!broadcast.isBlank()) yaml.set("VoteParty.Broadcast", broadcast);
			return new QuickProposal(fileName, yaml.saveToString());
		}
		throw new IllegalArgumentException("quick setup preset is unsupported");
	}

	private Path resolve(String fileName) throws IOException {
		if (fileName == null || (!TOP_LEVEL.contains(fileName)
				&& !fileName.matches("VoteSites/[A-Za-z0-9._-]{1,100}\\.yml"))) {
			throw new IllegalArgumentException("configuration file is not managed");
		}
		Path requested = dataDirectory.resolve(fileName).normalize();
		if (!requested.startsWith(dataDirectory)) {
			throw new IllegalArgumentException("configuration path escapes data folder");
		}
		Path root = dataDirectory.toRealPath();
		Path parent = requested.getParent();
		if (parent == null) throw new IOException("configuration parent is unavailable");
		Path realParent = parent.toRealPath();
		if (!realParent.startsWith(root)) throw new IOException("configuration path escapes data folder");
		Path target = realParent.resolve(requested.getFileName()).normalize();
		if (!target.startsWith(root) || Files.isSymbolicLink(target)) {
			throw new IOException("symbolic configuration paths are not allowed");
		}
		return target;
	}

	private static String readRaw(Path path, boolean allowMissing) throws IOException {
		if (!Files.exists(path) && allowMissing) return "";
		if (!Files.isRegularFile(path, java.nio.file.LinkOption.NOFOLLOW_LINKS)
				|| Files.size(path) > MAX_CONTENT_BYTES) {
			throw new IOException("configuration file is missing or too large");
		}
		byte[] bytes = Files.readAllBytes(path);
		try {
			return StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
					.onUnmappableCharacter(CodingErrorAction.REPORT).decode(java.nio.ByteBuffer.wrap(bytes)).toString();
		} catch (CharacterCodingException e) {
			throw new IOException("configuration file is not valid UTF-8", e);
		}
	}

	private static YamlConfiguration parse(String content) {
		if (content == null) throw new IllegalArgumentException("configuration content is required");
		ensureBounded(content);
		YamlConfiguration yaml = new YamlConfiguration();
		// Comment retention is part of the hosted editor contract. Keep this
		// explicit even though current Spigot versions default it to true.
		yaml.options().parseComments(true);
		try {
			yaml.loadFromString(content);
		} catch (InvalidConfigurationException e) {
			throw new IllegalArgumentException("configuration is not valid YAML");
		}
		return yaml;
	}

	private static void ensureBounded(String content) {
		if (content == null || content.indexOf('\0') >= 0
				|| content.getBytes(StandardCharsets.UTF_8).length > MAX_CONTENT_BYTES) {
			throw new IllegalArgumentException("configuration content exceeds the 512 KiB limit");
		}
	}

	private static String mask(YamlConfiguration yaml) {
		Set<String> secretValues = new HashSet<>();
		for (String path : new ArrayList<>(yaml.getKeys(true))) {
			if (!(yaml.get(path) instanceof ConfigurationSection) && secret(path)) {
				Object value = yaml.get(path);
				if (value != null) addSecretValues(secretValues, String.valueOf(value));
				yaml.set(path, REDACTED);
			}
		}
		sanitizeCommentMetadata(yaml, secretValues);
		String masked = yaml.saveToString();
		ensureBounded(masked);
		return masked;
	}

	private static void addSecretValues(Set<String> values, String value) {
		if (!value.isBlank()) values.add(value);
		value.lines().map(String::trim).filter(line -> !line.isBlank()).forEach(values::add);
	}

	private static void sanitizeCommentMetadata(YamlConfiguration yaml, Set<String> secretValues) {
		List<String> replacements = secretValues.stream().filter(BackendConfigurationService::safeSecretValue)
				.sorted(java.util.Comparator.comparingInt(String::length).reversed()).toList();
		yaml.options().setHeader(sanitizeComments(yaml.options().getHeader(), replacements, false));
		yaml.options().setFooter(sanitizeComments(yaml.options().getFooter(), replacements, false));
		for (String path : new ArrayList<>(yaml.getKeys(true))) {
			yaml.setComments(path, sanitizeComments(yaml.getComments(path), replacements, secret(path)));
			yaml.setInlineComments(path, sanitizeComments(yaml.getInlineComments(path), replacements, secret(path)));
		}
	}

	private static List<String> sanitizeComments(List<String> comments, List<String> secretValues, boolean secretPath) {
		List<String> sanitized = new ArrayList<>(comments.size());
		boolean redactContinuation = false;
		for (String original : comments) {
			String comment = original;
			if (redactContinuation) {
				if (original.isBlank()) {
					redactContinuation = false;
				} else {
					java.util.regex.Matcher continuedLabel = COMMENT_SECRET.matcher(original);
					if (continuedLabel.find() && !commentContinuation(continuedLabel.group(2))) {
						redactContinuation = false;
					}
					sanitized.add(REDACTED);
					continue;
				}
			}
			for (String value : secretValues) comment = comment.replace(value, REDACTED);
			java.util.regex.Matcher labelledSecret = COMMENT_SECRET.matcher(original);
			if (labelledSecret.find() && commentContinuation(labelledSecret.group(2))) redactContinuation = true;
			comment = COMMENT_SECRET.matcher(comment).replaceAll("$1" + REDACTED);
			if (secretPath) comment = SECRET_PATH_URL.matcher(comment).replaceAll("$1" + REDACTED);
			sanitized.add(comment);
		}
		return sanitized;
	}

	private static boolean commentContinuation(String value) {
		String trimmed = value.trim();
		return trimmed.isEmpty() || BLOCK_SCALAR_INDICATOR.matcher(trimmed).matches();
	}

	private static boolean safeSecretValue(String value) {
		if (value.length() < 6) return false;
		return !Set.of("true", "false", "null", "yes", "no", "on", "off").contains(value.toLowerCase(Locale.ROOT));
	}

	private static YamlConfiguration resolveSecrets(YamlConfiguration proposal, YamlConfiguration current) {
		YamlConfiguration redactedCurrent = parse(mask(parse(current.saveToString())));
		for (String path : new ArrayList<>(proposal.getKeys(true))) {
			if (secret(path) && REDACTED.equals(proposal.getString(path))) proposal.set(path, current.get(path));
		}
		restoreCommentSecrets(proposal, current, redactedCurrent);
		return proposal;
	}

	private static void restoreCommentSecrets(YamlConfiguration proposal, YamlConfiguration current,
			YamlConfiguration redactedCurrent) {
		proposal.options().setHeader(restoreCommentSecrets(proposal.options().getHeader(),
				current.options().getHeader(), redactedCurrent.options().getHeader()));
		proposal.options().setFooter(restoreCommentSecrets(proposal.options().getFooter(),
				current.options().getFooter(), redactedCurrent.options().getFooter()));
		for (String path : new ArrayList<>(proposal.getKeys(true))) {
			proposal.setComments(path, restoreCommentSecrets(proposal.getComments(path), current.getComments(path),
					redactedCurrent.getComments(path)));
			proposal.setInlineComments(path, restoreCommentSecrets(proposal.getInlineComments(path),
					current.getInlineComments(path), redactedCurrent.getInlineComments(path)));
		}
	}

	private static List<String> restoreCommentSecrets(List<String> proposed, List<String> current,
			List<String> redactedCurrent) {
		for (int index = 0; index < redactedCurrent.size(); index++) {
			String redacted = redactedCurrent.get(index);
			if (redacted.contains(REDACTED)
					&& (index >= proposed.size() || !redacted.equals(proposed.get(index)))) {
				throw new IllegalArgumentException("redacted comment placeholders must not be edited or moved");
			}
		}
		List<String> restored = new ArrayList<>(proposed.size());
		for (int index = 0; index < proposed.size(); index++) {
			String comment = proposed.get(index);
			if (!comment.contains(REDACTED)) {
				restored.add(comment);
				continue;
			}
			if (index >= current.size() || index >= redactedCurrent.size()
					|| !comment.equals(redactedCurrent.get(index))) {
				throw new IllegalArgumentException("redacted comment placeholders must not be edited or moved");
			}
			restored.add(current.get(index));
		}
		return restored;
	}

	private static boolean secret(String path) {
		String key = path.substring(path.lastIndexOf('.') + 1).replace("-", "").replace("_", "")
				.toLowerCase(Locale.ROOT);
		return key.contains("password") || key.contains("secret") || key.equals("token")
				|| key.equals("apikey") || key.equals("authorization") || key.equals("webhookurl") || key.equals("url")
				&& path.toLowerCase(Locale.ROOT).contains("webhook");
	}

	private static List<String> changes(YamlConfiguration current, YamlConfiguration proposal) {
		Set<String> paths = new LinkedHashSet<>(current.getKeys(true));
		paths.addAll(proposal.getKeys(true));
		List<String> changes = new ArrayList<>();
		if (!current.options().getHeader().equals(proposal.options().getHeader())
				&& !addChange(changes, "changed header comments")) return List.copyOf(changes);
		if (!current.options().getFooter().equals(proposal.options().getFooter())
				&& !addChange(changes, "changed footer comments")) return List.copyOf(changes);
		for (String path : paths) {
			Object before = current.get(path);
			Object after = proposal.get(path);
			if (!(before instanceof ConfigurationSection) && !(after instanceof ConfigurationSection)
					&& !java.util.Objects.equals(before, after)
					&& !addChange(changes, (secret(path) ? "changed secret " : "changed ") + path)) {
				break;
			}
			if ((!current.getComments(path).equals(proposal.getComments(path))
					|| !current.getInlineComments(path).equals(proposal.getInlineComments(path)))
					&& !addChange(changes, "changed comments " + path)) {
				break;
			}
		}
		return List.copyOf(changes);
	}

	private static boolean addChange(List<String> changes, String change) {
		if (changes.size() == 19) {
			changes.add("additional changes omitted");
			return false;
		}
		changes.add(change);
		return true;
	}

	private static String revision(String content) {
		try {
			return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256")
					.digest(content.getBytes(StandardCharsets.UTF_8)));
		} catch (NoSuchAlgorithmException e) {
			throw new IllegalStateException("SHA-256 is unavailable", e);
		}
	}

	private static void move(Path source, Path target) throws IOException {
		DurableFiles.forceFile(source);
		Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
		DurableFiles.forceMoveDirectories(source, target);
	}

	private static String bungeeMethodOption(Map<String, String> options) {
		String value = options == null ? "PLUGINMESSAGING" : options.getOrDefault("method", "PLUGINMESSAGING");
		return canonicalBungeeMethod(value);
	}

	private static String canonicalBungeeMethod(String value) {
		for (BungeeMethod method : BungeeMethod.values()) {
			if (method.name().equalsIgnoreCase(value)) return method.name();
		}
		throw new IllegalArgumentException("BungeeMethod is invalid");
	}

	private static String option(Map<String, String> options, String name, String pattern) {
		String value = options == null ? null : options.get(name);
		if (value == null || !value.matches(pattern)) throw new IllegalArgumentException("quick setup option " + name + " is invalid");
		return value;
	}

	private static String optional(Map<String, String> options, String name, int maximum) {
		String value = options == null ? "" : options.getOrDefault(name, "").trim();
		if (value.length() > maximum || value.indexOf('\0') >= 0 || value.indexOf('\r') >= 0
				|| value.indexOf('\n') >= 0) {
			throw new IllegalArgumentException("quick setup option " + name + " is invalid");
		}
		return value;
	}

	private static boolean booleanOption(Map<String, String> options, String name) {
		String value = options == null ? null : options.get(name);
		if (!"true".equals(value) && !"false".equals(value)) {
			throw new IllegalArgumentException("quick setup option " + name + " is invalid");
		}
		return Boolean.parseBoolean(value);
	}

	private static int boundedInteger(String value, int minimum, int maximum) {
		try {
			int parsed = Integer.parseInt(value);
			if (parsed < minimum || parsed > maximum) throw new NumberFormatException();
			return parsed;
		} catch (NumberFormatException e) {
			throw new IllegalArgumentException("quick setup number is invalid");
		}
	}

	public record Document(String fileName, String content, String revision) { }
	public record Preview(String fileName, String resolvedContent, String revision, List<String> changes) { }
	public record ApplyResult(Document document, List<String> changes, boolean rolledBack) { }
	public record QuickProposal(String fileName, String content) { }
	public record QuickPreview(QuickProposal proposal, String revision, List<String> changes) { }

	@FunctionalInterface public interface ReloadAction { void run() throws Exception; }
	@FunctionalInterface public interface ApplyAction { void run(String fileName) throws Exception; }
	@FunctionalInterface interface MoveAction { void move(Path source, Path target) throws IOException; }
	@SuppressWarnings("serial") public static final class StaleRevisionException extends RuntimeException { }
	@SuppressWarnings("serial") public static final class ApplyFailureException extends IOException {
		private final boolean rolledBack;
		private ApplyFailureException(boolean rolledBack, Throwable cause) { super(cause); this.rolledBack = rolledBack; }
		public boolean rolledBack() { return rolledBack; }
	}
}
