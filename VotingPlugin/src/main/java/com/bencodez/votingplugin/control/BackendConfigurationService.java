package com.bencodez.votingplugin.control;

import java.io.IOException;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.file.AtomicMoveNotSupportedException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
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

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;

/** Safe, revisioned access to every user-facing VotingPlugin YAML configuration file. */
public final class BackendConfigurationService {
	public static final String REDACTED = "__VOTINGPLUGIN_CONTROL_REDACTED__";
	public static final int MAX_CONTENT_BYTES = 512 * 1024;
	private static final Set<String> TOP_LEVEL = Set.of("Config.yml", "VoteSites.yml", "SpecialRewards.yml",
			"GUI.yml", "Shop.yml", "BungeeSettings.yml");

	private final Path dataDirectory;
	private final ReloadAction reload;

	public BackendConfigurationService(Path dataDirectory, ReloadAction reload) {
		this.dataDirectory = dataDirectory.toAbsolutePath().normalize();
		this.reload = reload;
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

	public ApplyResult apply(String fileName, String proposedContent, String expectedRevision) throws IOException {
		Path target = resolve(fileName);
		String current = readRaw(target, false);
		if (expectedRevision == null || !revision(current).equals(expectedRevision)) throw new StaleRevisionException();
		Preview preview = preview(fileName, proposedContent, current);
		Path backup = target.resolveSibling(target.getFileName() + ".control-backup");
		Path staging = Files.createTempFile(target.getParent(), ".control-", ".yml");
		boolean installed = false;
		try {
			Files.writeString(staging, preview.resolvedContent(), StandardCharsets.UTF_8,
					StandardOpenOption.TRUNCATE_EXISTING);
			Files.writeString(backup, current, StandardCharsets.UTF_8, StandardOpenOption.CREATE,
					StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);
			if (!revision(readRaw(target, false)).equals(expectedRevision)) throw new StaleRevisionException();
			move(staging, target);
			installed = true;
			reload.run();
			String applied = readRaw(target, false);
			return new ApplyResult(new Document(fileName, mask(parse(applied)), revision(applied)),
					preview.changes(), false);
		} catch (StaleRevisionException stale) {
			throw stale;
		} catch (Exception failure) {
			boolean rolledBack = false;
			if (installed) {
				try {
					Files.copy(backup, target, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES);
					reload.run();
					rolledBack = true;
				} catch (Exception rollbackFailure) {
					failure.addSuppressed(rollbackFailure);
				}
			}
			throw new ApplyFailureException(rolledBack, failure);
		} finally {
			Files.deleteIfExists(staging);
		}
	}

	private Preview preview(String fileName, String proposedContent, String current) {
		YamlConfiguration currentYaml = parse(current);
		String resolved = resolveSecrets(parse(proposedContent), currentYaml).saveToString();
		ensureBounded(resolved);
		return new Preview(fileName, resolved, revision(current), changes(currentYaml, parse(resolved)));
	}

	public QuickPreview previewQuickSetup(String preset, Map<String, String> options) throws IOException {
		QuickProposal proposal = quickProposal(preset, options);
		String current = readRaw(resolve(proposal.fileName()), false);
		return new QuickPreview(proposal, revision(current), changes(parse(current), parse(proposal.content())));
	}

	public ApplyResult applyQuickSetup(String preset, Map<String, String> options, String expectedRevision)
			throws IOException {
		QuickProposal proposal = quickProposal(preset, options);
		return apply(proposal.fileName(), proposal.content(), expectedRevision);
	}

	private QuickProposal quickProposal(String preset, Map<String, String> options) throws IOException {
		if ("standalone".equals(preset) || "proxy-backend".equals(preset)) {
			String fileName = "BungeeSettings.yml";
			YamlConfiguration yaml = parse(readRaw(resolve(fileName), false));
			boolean proxy = "proxy-backend".equals(preset);
			yaml.set("UseBungeecord", proxy);
			if (proxy) {
				String server = option(options, "server", "[A-Za-z0-9][A-Za-z0-9._-]{0,63}");
				yaml.set("Server", server);
				yaml.set("BungeeMethod", options.getOrDefault("method", "PLUGINMESSAGING"));
			}
			return new QuickProposal(fileName, yaml.saveToString());
		}
		if ("vote-site".equals(preset)) {
			String fileName = "VoteSites.yml";
			YamlConfiguration yaml = parse(readRaw(resolve(fileName), false));
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
			String fileName = "VoteSites.yml";
			YamlConfiguration yaml = parse(readRaw(resolve(fileName), false));
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
			String fileName = "Config.yml";
			YamlConfiguration yaml = parse(readRaw(resolve(fileName), false));
			yaml.set("ProcessRewards", booleanOption(options, "processRewards"));
			yaml.set("AutoCreateVoteSites", booleanOption(options, "autoCreateVoteSites"));
			yaml.set("ExtraAllSitesCheck", booleanOption(options, "extraAllSitesCheck"));
			yaml.set("CountFakeVotes", booleanOption(options, "countFakeVotes"));
			yaml.set("DisableNoServiceSiteMessage", booleanOption(options, "disableNoServiceSiteMessage"));
			yaml.set("DisableUpdateChecking", booleanOption(options, "disableUpdateChecking"));
			return new QuickProposal(fileName, yaml.saveToString());
		}
		if ("vote-party".equals(preset)) {
			String fileName = "SpecialRewards.yml";
			YamlConfiguration yaml = parse(readRaw(resolve(fileName), false));
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

	private Path resolve(String fileName) {
		if (fileName == null || (!TOP_LEVEL.contains(fileName)
				&& !fileName.matches("VoteSites/[A-Za-z0-9._-]{1,100}\\.yml"))) {
			throw new IllegalArgumentException("configuration file is not managed");
		}
		Path target = dataDirectory.resolve(fileName).normalize();
		if (!target.startsWith(dataDirectory)) throw new IllegalArgumentException("configuration path escapes data folder");
		return target;
	}

	private static String readRaw(Path path, boolean allowMissing) throws IOException {
		if (!Files.exists(path) && allowMissing) return "";
		if (!Files.isRegularFile(path) || Files.size(path) > MAX_CONTENT_BYTES) {
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
		for (String path : new ArrayList<>(yaml.getKeys(true))) {
			if (!(yaml.get(path) instanceof ConfigurationSection) && secret(path)) yaml.set(path, REDACTED);
		}
		return yaml.saveToString();
	}

	private static YamlConfiguration resolveSecrets(YamlConfiguration proposal, YamlConfiguration current) {
		for (String path : new ArrayList<>(proposal.getKeys(true))) {
			if (secret(path) && REDACTED.equals(proposal.getString(path))) proposal.set(path, current.get(path));
		}
		return proposal;
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
		for (String path : paths) {
			Object before = current.get(path);
			Object after = proposal.get(path);
			if (before instanceof ConfigurationSection || after instanceof ConfigurationSection) continue;
			if (!java.util.Objects.equals(before, after)) {
				changes.add((secret(path) ? "changed secret " : "changed ") + path);
				if (changes.size() == 19) {
					changes.add("additional changes omitted");
					break;
				}
			}
		}
		return List.copyOf(changes);
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
		try {
			Files.move(source, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
		} catch (AtomicMoveNotSupportedException e) {
			Files.move(source, target, StandardCopyOption.REPLACE_EXISTING);
		}
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
	@SuppressWarnings("serial") public static final class StaleRevisionException extends RuntimeException { }
	@SuppressWarnings("serial") public static final class ApplyFailureException extends IOException {
		private final boolean rolledBack;
		private ApplyFailureException(boolean rolledBack, Throwable cause) { super(cause); this.rolledBack = rolledBack; }
		public boolean rolledBack() { return rolledBack; }
	}
}
