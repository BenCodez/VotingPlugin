package com.bencodez.votingplugin.proxy.control;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.channels.Channels;
import java.nio.channels.SeekableByteChannel;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HexFormat;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.comments.CommentLine;
import org.yaml.snakeyaml.constructor.SafeConstructor;
import org.yaml.snakeyaml.nodes.MappingNode;
import org.yaml.snakeyaml.nodes.Node;
import org.yaml.snakeyaml.nodes.NodeTuple;
import org.yaml.snakeyaml.nodes.ScalarNode;
import org.yaml.snakeyaml.nodes.SequenceNode;
import org.yaml.snakeyaml.nodes.Tag;

import com.bencodez.votingplugin.proxy.VotingPluginProxy;
import com.bencodez.votingplugin.util.DurableFiles;

/** Strict, revisioned access to the proxy's single bungeeconfig.yml file. */
final class ProxyConfigurationFileService {
	static final String FILE_NAME = "bungeeconfig.yml";
	static final String REDACTED = "__VOTINGPLUGIN_CONTROL_REDACTED__";
	static final int MAX_BYTES = 512 * 1024;
	private static final String REDACTED_COMMENT = " " + REDACTED;
	private final Path target;
	private final MoveAction mover;
	private final TempFileAction tempFiles;

	ProxyConfigurationFileService(VotingPluginProxy proxy) {
		this(proxy.getDataFolderPlugin().toPath().toAbsolutePath().normalize().resolve(FILE_NAME),
				ProxyConfigurationFileService::move, Files::createTempFile);
	}

	ProxyConfigurationFileService(Path target, MoveAction mover) {
		this(target, mover, Files::createTempFile);
	}

	ProxyConfigurationFileService(Path target, MoveAction mover, TempFileAction tempFiles) {
		this.target = target.toAbsolutePath().normalize();
		this.mover = java.util.Objects.requireNonNull(mover, "mover");
		this.tempFiles = java.util.Objects.requireNonNull(tempFiles, "tempFiles");
	}

	Document read(String fileName) throws IOException {
		requireFile(fileName);
		String raw = readRaw();
		Map<String, Object> parsed = parse(raw);
		return new Document(FILE_NAME, renderMasked(raw, parsed), revision(raw));
	}

	Preview preview(String fileName, String proposed) throws IOException {
		requireFile(fileName);
		String currentRaw = readRaw();
		Map<String, Object> current = parse(currentRaw);
		Map<String, Object> proposedValues = parse(proposed);
		Map<String, Object> resolved = resolve(proposedValues, current, "");
		validateRedactedValues(current, proposedValues, "");
		Node currentTree = compose(currentRaw);
		Node currentValuesTree = compose(currentRaw);
		Map<String, CommentLine> redactedComments = redactComments(currentTree, current, "",
				sensitiveValues(currentTree, current));
		Node proposedTree = compose(proposed);
		restoreRedactedComments(proposedTree, redactedComments);
		restoreRedactedValues(proposedTree, currentValuesTree, current, proposedValues, "");
		String content = serialize(proposedTree);
		ensureBounded(content);
		if (!resolved.equals(parse(content))) throw new IllegalArgumentException("proxy configuration content is invalid");
		return new Preview(content, revision(currentRaw), changes(current, resolved));
	}

	ApplyResult apply(String fileName, String proposed, String expectedRevision) throws IOException {
		requireFile(fileName);
		String currentRaw = readRaw();
		if (expectedRevision == null || !revision(currentRaw).equals(expectedRevision)) throw new StaleRevisionException();
		Preview preview = preview(fileName, proposed);
		Path backup = target.resolveSibling(FILE_NAME + ".control-backup");
		Path stage = null;
		Path backupStage = null;
		boolean installed = false;
		try {
			stage = tempFiles.create(target.getParent(), ".control-proxy-", ".yml");
			backupStage = tempFiles.create(target.getParent(), ".control-proxy-backup-", ".yml");
			Files.writeString(stage, preview.resolvedContent, StandardCharsets.UTF_8, StandardOpenOption.TRUNCATE_EXISTING);
			copyPermissions(target, stage);
			parse(readStrict(stage));
			if (Files.isSymbolicLink(backup)) throw new IOException("unsafe proxy configuration backup");
			Files.writeString(backupStage, currentRaw, StandardCharsets.UTF_8, StandardOpenOption.TRUNCATE_EXISTING);
			copyPermissions(target, backupStage);
			if (!revision(readRaw()).equals(expectedRevision)) throw new StaleRevisionException();
			mover.move(backupStage, backup);
			if (!revision(readRaw()).equals(expectedRevision)) throw new StaleRevisionException();
			try {
				mover.move(stage, target);
				installed = true;
			} catch (DurableFiles.PublishedException published) {
				installed = true;
				throw published;
			}
			String applied = readRaw();
			if (!revision(applied).equals(revision(preview.resolvedContent))) throw new StaleRevisionException();
			return new ApplyResult(new Document(FILE_NAME, renderMasked(applied, parse(applied)), revision(applied)),
					preview.changes, false);
		} catch (StaleRevisionException stale) {
			throw stale;
		} catch (Exception failure) {
			boolean rolledBack = false;
			if (installed) {
				try {
					if (!revision(readRaw()).equals(revision(preview.resolvedContent))) {
						throw new IOException("proxy configuration changed during rollback");
					}
					if (!Files.isRegularFile(backup, LinkOption.NOFOLLOW_LINKS)) {
						throw new IOException("proxy configuration backup is unavailable");
					}
					Path rollback = tempFiles.create(target.getParent(), ".control-proxy-rollback-", ".yml");
					try {
						try (SeekableByteChannel source = Files.newByteChannel(backup,
								Set.of(StandardOpenOption.READ, LinkOption.NOFOLLOW_LINKS))) {
							Files.copy(Channels.newInputStream(source), rollback, StandardCopyOption.REPLACE_EXISTING);
						}
						copyPermissions(backup, rollback);
						mover.move(rollback, target);
					} finally { Files.deleteIfExists(rollback); }
					rolledBack = true;
				} catch (Exception rollbackFailure) { failure.addSuppressed(rollbackFailure); }
			}
			throw new ApplyFailureException(rolledBack, failure);
		} finally {
			if (stage != null) Files.deleteIfExists(stage);
			if (backupStage != null) Files.deleteIfExists(backupStage);
		}
	}

	private String readRaw() throws IOException {
		if (!Files.isRegularFile(target, LinkOption.NOFOLLOW_LINKS)) throw new IOException("proxy configuration is unavailable");
		return readStrict(target);
	}

	private static String readStrict(Path path) throws IOException {
		long size = Files.size(path);
		if (size < 0 || size > MAX_BYTES) throw new IOException("proxy configuration exceeds limits");
		byte[] bytes;
		try (InputStream input = Files.newInputStream(path, LinkOption.NOFOLLOW_LINKS)) {
			bytes = input.readNBytes(MAX_BYTES + 1);
		}
		if (bytes.length > MAX_BYTES) throw new IOException("proxy configuration exceeds limits");
		try {
			return StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
					.onUnmappableCharacter(CodingErrorAction.REPORT).decode(java.nio.ByteBuffer.wrap(bytes)).toString();
		} catch (CharacterCodingException failure) { throw new IOException("proxy configuration is not UTF-8", failure); }
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Object> parse(String yaml) {
		ensureBounded(yaml);
		if (yaml.matches("(?s).*(?:^|[\\s\\[{,])(?:[&*][A-Za-z0-9_-]+|<<\\s*:).*")) {
			throw new IllegalArgumentException("proxy configuration aliases are not supported");
		}
		LoaderOptions loaderOptions = loaderOptions();
		SafeConstructor constructor = new SafeConstructor(loaderOptions);
		Object parsed;
		try { parsed = new Yaml(constructor).load(yaml); }
		catch (RuntimeException failure) { throw new IllegalArgumentException("proxy configuration YAML is invalid"); }
		if (!(parsed instanceof Map<?, ?> root)) throw new IllegalArgumentException("proxy configuration must be a mapping");
		Map<String, Object> result = new LinkedHashMap<>();
		for (Map.Entry<?, ?> entry : root.entrySet()) {
			if (!(entry.getKey() instanceof String key)) throw new IllegalArgumentException("proxy configuration keys must be strings");
			result.put(key, normalize(entry.getValue(), 1));
		}
		return result;
	}

	private static Object normalize(Object value, int depth) {
		if (depth > 50) throw new IllegalArgumentException("proxy configuration is too deeply nested");
		if (value == null || value instanceof String || value instanceof Boolean || value instanceof Number) return value;
		if (value instanceof Map<?, ?> map) {
			Map<String, Object> result = new LinkedHashMap<>();
			for (Map.Entry<?, ?> entry : map.entrySet()) {
				if (!(entry.getKey() instanceof String key)) throw new IllegalArgumentException("proxy configuration keys must be strings");
				result.put(key, normalize(entry.getValue(), depth + 1));
			}
			return result;
		}
		if (value instanceof List<?> list) return list.stream().map(item -> normalize(item, depth + 1)).toList();
		throw new IllegalArgumentException("proxy configuration contains an unsupported YAML value");
	}

	private static String renderMasked(String raw, Map<String, Object> parsed) {
		Node tree = compose(raw);
		redactValues(tree, parsed, "", sensitiveValues(tree, parsed));
		String content = serialize(tree);
		ensureBounded(content);
		if (!mask(parsed).equals(parse(content))) throw new IllegalArgumentException("proxy configuration content is invalid");
		return content;
	}

	private static Node compose(String yaml) {
		// Construct first so comment parsing cannot bypass the strict SafeConstructor checks.
		parse(yaml);
		LoaderOptions options = loaderOptions();
		options.setProcessComments(true);
		try {
			Node node = new Yaml(options, dumperOptions()).compose(new StringReader(yaml));
			if (!(node instanceof MappingNode)) throw new IllegalArgumentException("proxy configuration must be a mapping");
			return node;
		} catch (RuntimeException failure) {
			throw new IllegalArgumentException("proxy configuration YAML is invalid");
		}
	}

	private static String serialize(Node node) {
		StringWriter writer = new StringWriter();
		new Yaml(dumperOptions()).serialize(node, writer);
		return writer.toString();
	}

	private static LoaderOptions loaderOptions() {
		LoaderOptions loaderOptions = new LoaderOptions();
		loaderOptions.setAllowDuplicateKeys(false);
		loaderOptions.setMaxAliasesForCollections(0);
		loaderOptions.setNestingDepthLimit(50);
		loaderOptions.setCodePointLimit(MAX_BYTES);
		return loaderOptions;
	}

	private static DumperOptions dumperOptions() {
		DumperOptions options = new DumperOptions();
		options.setIndent(2);
		options.setPrettyFlow(true);
		options.setProcessComments(true);
		return options;
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Object> mask(Map<String, Object> source) {
		return mask(source, "");
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Object> mask(Map<String, Object> source, String path) {
		Map<String, Object> result = new LinkedHashMap<>();
		for (Map.Entry<String, Object> entry : source.entrySet()) {
			Object value = entry.getValue();
			String childPath = path + entry.getKey();
			if (secret(childPath, entry.getKey(), value)) result.put(entry.getKey(), REDACTED);
			else if (value instanceof Map<?, ?> map) result.put(entry.getKey(), mask((Map<String, Object>) map, childPath + "."));
			else result.put(entry.getKey(), value);
		}
		return result;
	}

	private static void redactValues(Node node, Map<String, Object> source, String path, Set<String> values) {
		if (!(node instanceof MappingNode mapping)) {
			redactDescendantComments(node, path, false, values, new LinkedHashMap<>());
			return;
		}
		redactComments(mapping, path, false, values, new LinkedHashMap<>());
		List<NodeTuple> tuples = new ArrayList<>();
		for (NodeTuple tuple : mapping.getValue()) {
			String key = key(tuple.getKeyNode());
			Object value = source.get(key);
			String childPath = path + key;
			boolean hidden = secret(childPath, key, value);
			redactComments(tuple.getKeyNode(), childPath + "#key", hidden, values, new LinkedHashMap<>());
			Node child = tuple.getValueNode();
			if (hidden) {
				redactComments(child, childPath, true, values, new LinkedHashMap<>());
				child = marker(child);
			} else if (value instanceof Map<?, ?> nested) {
				@SuppressWarnings("unchecked") Map<String, Object> nestedValues = (Map<String, Object>) nested;
				redactValues(child, nestedValues, childPath + ".", values);
			} else {
				redactDescendantComments(child, childPath, false, values, new LinkedHashMap<>());
			}
			tuples.add(new NodeTuple(tuple.getKeyNode(), child));
		}
		mapping.setValue(tuples);
	}

	private static void validateRedactedValues(Map<String, Object> current, Map<String, Object> proposed, String path) {
		for (Map.Entry<String, Object> entry : current.entrySet()) {
			String key = entry.getKey();
			Object old = entry.getValue();
			String childPath = path + key;
			if (secret(childPath, key, old)) {
				if (!proposed.containsKey(key) || proposed.get(key) instanceof Map<?, ?>
						|| proposed.get(key) instanceof List<?>) {
					throw new IllegalArgumentException("redacted placeholder is invalid");
				}
				continue;
			}
			Object candidate = proposed.get(key);
			if (old instanceof Map<?, ?> oldMap) {
				if (!(candidate instanceof Map<?, ?> proposedMap)) {
					if (containsSecrets(oldMap, childPath + ".")) {
						throw new IllegalArgumentException("redacted placeholder is invalid");
					}
					continue;
				}
				@SuppressWarnings("unchecked") Map<String, Object> oldValues = (Map<String, Object>) oldMap;
				@SuppressWarnings("unchecked") Map<String, Object> candidateValues = (Map<String, Object>) proposedMap;
				validateRedactedValues(oldValues, candidateValues, childPath + ".");
			}
		}
	}

	@SuppressWarnings("unchecked")
	private static boolean containsSecrets(Map<?, ?> source, String path) {
		for (Map.Entry<?, ?> entry : source.entrySet()) {
			if (!(entry.getKey() instanceof String key)) return true;
			Object value = entry.getValue();
			String childPath = path + key;
			if (secret(childPath, key, value)) return true;
			if (value instanceof Map<?, ?> nested && containsSecrets(nested, childPath + ".")) return true;
		}
		return false;
	}

	private static void restoreRedactedValues(Node proposed, Node current, Map<String, Object> currentValues,
			Map<String, Object> proposedValues, String path) {
		if (!(proposed instanceof MappingNode proposedMap) || !(current instanceof MappingNode currentMap)) return;
		Map<String, NodeTuple> currentTuples = tuples(currentMap);
		List<NodeTuple> restored = new ArrayList<>();
		for (NodeTuple tuple : proposedMap.getValue()) {
			String key = key(tuple.getKeyNode());
			Object old = currentValues.get(key);
			String childPath = path + key;
			Node value = tuple.getValueNode();
			if (currentTuples.containsKey(key) && secret(childPath, key, old)
					&& REDACTED.equals(proposedValues.get(key))) {
				value = restoreSecretNode(currentTuples.get(key).getValueNode(), value);
			} else if (old instanceof Map<?, ?> oldMap && proposedValues.get(key) instanceof Map<?, ?> proposedMapValue) {
				@SuppressWarnings("unchecked") Map<String, Object> oldValues = (Map<String, Object>) oldMap;
				@SuppressWarnings("unchecked") Map<String, Object> candidateValues = (Map<String, Object>) proposedMapValue;
				restoreRedactedValues(value, currentTuples.containsKey(key) ? currentTuples.get(key).getValueNode() : value,
						oldValues, candidateValues, childPath + ".");
			}
			restored.add(new NodeTuple(tuple.getKeyNode(), value));
		}
		proposedMap.setValue(restored);
	}

	private static Node restoreSecretNode(Node current, Node proposed) {
		if (current instanceof ScalarNode oldScalar && proposed instanceof ScalarNode proposedScalar) {
			ScalarNode restored = new ScalarNode(oldScalar.getTag(), oldScalar.getValue(), proposedScalar.getStartMark(),
					proposedScalar.getEndMark(), oldScalar.getScalarStyle());
			copyComments(proposedScalar, restored);
			return restored;
		}
		return current;
	}

	private static Node marker(Node source) {
		ScalarNode marker = new ScalarNode(Tag.STR, REDACTED, source.getStartMark(), source.getEndMark(),
				DumperOptions.ScalarStyle.PLAIN);
		copyComments(source, marker);
		return marker;
	}

	private static Map<String, CommentLine> redactComments(Node node, Map<String, Object> source, String path,
			Set<String> values) {
		Map<String, CommentLine> result = new LinkedHashMap<>();
		redactComments(node, path, false, values, result);
		if (node instanceof MappingNode mapping) {
			for (NodeTuple tuple : mapping.getValue()) {
				String key = key(tuple.getKeyNode());
				Object value = source.get(key);
				String childPath = path + key;
				boolean hidden = secret(childPath, key, value);
				redactComments(tuple.getKeyNode(), childPath + "#key", hidden, values, result);
				if (hidden) redactComments(tuple.getValueNode(), childPath, true, values, result);
				else if (value instanceof Map<?, ?> nested) {
					@SuppressWarnings("unchecked") Map<String, Object> nestedValues = (Map<String, Object>) nested;
					result.putAll(redactComments(tuple.getValueNode(), nestedValues, childPath + ".", values));
				} else redactDescendantComments(tuple.getValueNode(), childPath, false, values, result);
			}
		}
		return result;
	}

	private static void redactDescendantComments(Node node, String path, boolean sensitiveContext, Set<String> values,
			Map<String, CommentLine> redacted) {
		redactComments(node, path, sensitiveContext, values, redacted);
		if (node instanceof MappingNode mapping) {
			for (NodeTuple tuple : mapping.getValue()) {
				String key = key(tuple.getKeyNode());
				redactDescendantComments(tuple.getKeyNode(), path + key + "#key", sensitiveContext, values, redacted);
				redactDescendantComments(tuple.getValueNode(), path + key, sensitiveContext, values, redacted);
			}
		} else if (node instanceof SequenceNode sequence) {
			for (int index = 0; index < sequence.getValue().size(); index++) {
				redactDescendantComments(sequence.getValue().get(index), path + "[" + index + "]", sensitiveContext,
						values, redacted);
			}
		}
	}

	private static void redactComments(Node node, String path, boolean sensitiveContext, Set<String> values,
			Map<String, CommentLine> redacted) {
		redactCommentList(node, path, "block", node.getBlockComments(), sensitiveContext, values, redacted);
		redactCommentList(node, path, "inline", node.getInLineComments(), sensitiveContext, values, redacted);
		redactCommentList(node, path, "end", node.getEndComments(), sensitiveContext, values, redacted);
	}

	private static void redactCommentList(Node node, String path, String kind, List<CommentLine> comments,
			boolean sensitiveContext, Set<String> values, Map<String, CommentLine> redacted) {
		if (comments == null) return;
		List<CommentLine> replacement = new ArrayList<>(comments);
		for (int index = 0; index < replacement.size(); index++) {
			CommentLine line = replacement.get(index);
			if (!sensitiveComment(line.getValue(), sensitiveContext, values)) continue;
			String slot = commentSlot(path, kind, index);
			redacted.put(slot, line);
			replacement.set(index, new CommentLine(line.getStartMark(), line.getEndMark(), REDACTED_COMMENT,
					line.getCommentType()));
		}
		setComments(node, kind, replacement);
	}

	private static void restoreRedactedComments(Node proposed, Map<String, CommentLine> expected) {
		Map<String, CommentReference> comments = commentReferences(proposed, "");
		for (Map.Entry<String, CommentLine> entry : expected.entrySet()) {
			CommentReference reference = comments.get(entry.getKey());
			if (reference == null || !REDACTED_COMMENT.equals(reference.line().getValue())) {
				throw new IllegalArgumentException("redacted placeholder is invalid");
			}
			reference.replace(entry.getValue());
		}
		for (Map.Entry<String, CommentReference> entry : comments.entrySet()) {
			if (REDACTED_COMMENT.equals(entry.getValue().line().getValue()) && !expected.containsKey(entry.getKey())) {
				throw new IllegalArgumentException("redacted placeholder is invalid");
			}
		}
	}

	private static Map<String, CommentReference> commentReferences(Node root, String path) {
		Map<String, CommentReference> result = new LinkedHashMap<>();
		collectComments(root, path, result);
		return result;
	}

	private static void collectComments(Node node, String path, Map<String, CommentReference> result) {
		collectCommentReferences(node, path, "block", node.getBlockComments(), result);
		collectCommentReferences(node, path, "inline", node.getInLineComments(), result);
		collectCommentReferences(node, path, "end", node.getEndComments(), result);
		if (node instanceof MappingNode mapping) {
			for (NodeTuple tuple : mapping.getValue()) {
				String key = key(tuple.getKeyNode());
				String childPath = path + key;
				collectComments(tuple.getKeyNode(), childPath + "#key", result);
				collectComments(tuple.getValueNode(), tuple.getValueNode() instanceof MappingNode ? childPath + "." : childPath,
						result);
			}
		} else if (node instanceof SequenceNode sequence) {
			for (int index = 0; index < sequence.getValue().size(); index++) {
				collectComments(sequence.getValue().get(index), path + "[" + index + "]", result);
			}
		}
	}

	private static void collectCommentReferences(Node node, String path, String kind, List<CommentLine> comments,
			Map<String, CommentReference> result) {
		if (comments == null) return;
		for (int index = 0; index < comments.size(); index++) {
			result.put(commentSlot(path, kind, index), new CommentReference(node, kind, index, comments.get(index)));
		}
	}

	private static void setComments(Node node, String kind, List<CommentLine> comments) {
		switch (kind) {
		case "block" -> node.setBlockComments(comments);
		case "inline" -> node.setInLineComments(comments);
		case "end" -> node.setEndComments(comments);
		default -> throw new IllegalArgumentException("invalid comment kind");
		}
	}

	private static String commentSlot(String path, String kind, int index) {
		return path + "|" + kind + "|" + index;
	}

	private static boolean sensitiveComment(String comment, boolean sensitiveContext, Set<String> values) {
		if (sensitiveContext) return true;
		String lowered = comment.toLowerCase(Locale.ROOT);
		if (lowered.matches("(?s).*\\b(password|secret|token|api[ _-]?key|authorization|jdbc|webhook)\\b.*")
				|| lowered.matches("(?s).*[a-z][a-z0-9+.-]*://[^/@\\s]+:[^/@\\s]+@.*")) return true;
		for (String value : values) {
			if (lowered.contains(value.toLowerCase(Locale.ROOT))) return true;
		}
		return false;
	}

	private static Set<String> sensitiveValues(Node node, Map<String, Object> source) {
		Set<String> values = new java.util.LinkedHashSet<>();
		collectSensitiveValues(node, source, "", values);
		return values;
	}

	@SuppressWarnings("unchecked")
	private static void collectSensitiveValues(Node node, Map<String, Object> source, String path, Set<String> values) {
		if (!(node instanceof MappingNode mapping)) return;
		for (NodeTuple tuple : mapping.getValue()) {
			String key = key(tuple.getKeyNode());
			Object value = source.get(key);
			String childPath = path + key;
			if (secret(childPath, key, value) && tuple.getValueNode() instanceof ScalarNode scalar
					&& safeSecretValue(scalar.getValue())) {
				values.add(scalar.getValue().trim());
			} else if (value instanceof Map<?, ?> nested) {
				collectSensitiveValues(tuple.getValueNode(), (Map<String, Object>) nested, childPath + ".", values);
			}
		}
	}

	private static boolean safeSecretValue(Object value) {
		if (value == null) return false;
		String text = String.valueOf(value).trim();
		return !text.isEmpty();
	}

	private static Map<String, NodeTuple> tuples(MappingNode node) {
		Map<String, NodeTuple> result = new LinkedHashMap<>();
		for (NodeTuple tuple : node.getValue()) result.put(key(tuple.getKeyNode()), tuple);
		return result;
	}

	private static String key(Node node) {
		if (!(node instanceof ScalarNode scalar)) throw new IllegalArgumentException("proxy configuration keys must be strings");
		return scalar.getValue();
	}

	private static void copyComments(Node source, Node target) {
		target.setBlockComments(copyCommentList(source.getBlockComments()));
		target.setInLineComments(copyCommentList(source.getInLineComments()));
		target.setEndComments(copyCommentList(source.getEndComments()));
	}

	private static List<CommentLine> copyCommentList(List<CommentLine> comments) {
		return comments == null ? null : new ArrayList<>(comments);
	}

	private record CommentReference(Node node, String kind, int index, CommentLine line) {
		void replace(CommentLine replacement) {
			List<CommentLine> comments = switch (kind) {
			case "block" -> node.getBlockComments();
			case "inline" -> node.getInLineComments();
			case "end" -> node.getEndComments();
			default -> throw new IllegalArgumentException("invalid comment kind");
			};
			List<CommentLine> updated = new ArrayList<>(comments);
			updated.set(index, replacement);
			setComments(node, kind, updated);
		}
	}

	@SuppressWarnings("unchecked")
	private static Map<String, Object> resolve(Map<String, Object> proposed, Map<String, Object> current, String path) {
		Map<String, Object> result = new LinkedHashMap<>();
		for (Map.Entry<String, Object> entry : proposed.entrySet()) {
			String key = entry.getKey();
			Object value = entry.getValue();
			Object old = current.get(key);
			String childPath = path + key;
			if (REDACTED.equals(value)) {
				if (!secret(childPath, key, old) || !current.containsKey(key)) throw new IllegalArgumentException("redacted placeholder is invalid");
				result.put(key, old);
			} else if (value instanceof Map<?, ?> nested) {
				Map<String, Object> oldValues;
				if (old == null) oldValues = Map.of();
				else if (old instanceof Map<?, ?> oldNested) oldValues = (Map<String, Object>) oldNested;
				else throw new IllegalArgumentException("proxy configuration shape changed");
				result.put(key, resolve((Map<String, Object>) nested, oldValues, childPath + "."));
			} else result.put(key, value);
		}
		return result;
	}

	private static boolean secret(String path, String key, Object value) {
		String normalized = key.toLowerCase(Locale.ROOT).replace("_", "").replace("-", "");
		if (normalized.contains("password") || normalized.contains("secret") || normalized.equals("token")
				|| normalized.contains("apikey") || normalized.contains("authorization")
				|| normalized.contains("webhookurl")) return true;
		String normalizedPath = path.toLowerCase(Locale.ROOT).replace("_", "").replace("-", "");
		if (normalizedPath.startsWith("database.") || normalizedPath.startsWith("globaldata.")) {
			return Set.of("host", "port", "database", "username", "password", "line", "driver", "poolname")
					.contains(normalized);
		}
		if (normalizedPath.startsWith("control.")) {
			return normalized.endsWith("file") || normalized.endsWith("directory");
		}
		if (value instanceof String text) {
			String lowered = text.trim().toLowerCase(Locale.ROOT);
			return lowered.startsWith("jdbc:") || lowered.matches("^[a-z][a-z0-9+.-]*://[^/@\\s]+:[^/@\\s]+@.*");
		}
		return false;
	}

	private static void copyPermissions(Path source, Path destination) throws IOException {
		java.nio.file.attribute.PosixFileAttributeView sourceView = Files.getFileAttributeView(source,
				java.nio.file.attribute.PosixFileAttributeView.class, LinkOption.NOFOLLOW_LINKS);
		java.nio.file.attribute.PosixFileAttributeView destinationView = Files.getFileAttributeView(destination,
				java.nio.file.attribute.PosixFileAttributeView.class, LinkOption.NOFOLLOW_LINKS);
		if (sourceView != null && destinationView != null) {
			destinationView.setPermissions(sourceView.readAttributes().permissions());
		}
	}

	private static List<String> changes(Map<String, Object> before, Map<String, Object> after) {
		Map<String, String> left = flatten(before, "");
		Map<String, String> right = flatten(after, "");
		Set<String> keys = new java.util.TreeSet<>(); keys.addAll(left.keySet()); keys.addAll(right.keySet());
		List<String> result = new ArrayList<>();
		for (String key : keys) {
			if (java.util.Objects.equals(left.get(key), right.get(key))) continue;
			result.add((left.containsKey(key) ? right.containsKey(key) ? "changed " : "removed " : "added ") + key);
			if (result.size() == 20) break;
		}
		return List.copyOf(result);
	}

	@SuppressWarnings("unchecked")
	private static Map<String, String> flatten(Map<String, Object> source, String prefix) {
		Map<String, String> result = new LinkedHashMap<>();
		source.entrySet().stream().sorted(Map.Entry.comparingByKey(Comparator.naturalOrder())).forEach(entry -> {
			String path = prefix + entry.getKey();
			if (entry.getValue() instanceof Map<?, ?> nested) result.putAll(flatten((Map<String, Object>) nested, path + "."));
			else result.put(path, String.valueOf(entry.getValue()));
		});
		return result;
	}

	private static void move(Path source, Path destination) throws IOException {
		try {
			DurableFiles.forceFile(source);
			Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
			DurableFiles.forceMoveDirectories(source, destination);
		} catch (java.nio.file.AtomicMoveNotSupportedException failure) {
			throw new IOException("atomic proxy configuration activation is unsupported", failure);
		}
	}

	private static void requireFile(String name) {
		if (!FILE_NAME.equals(name)) throw new IllegalArgumentException("proxy configuration file is not managed");
	}

	private static void ensureBounded(String value) {
		if (value == null || value.indexOf('\0') >= 0 || value.getBytes(StandardCharsets.UTF_8).length > MAX_BYTES) {
			throw new IllegalArgumentException("proxy configuration content is invalid");
		}
	}

	static String revision(String value) {
		try { return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8))); }
		catch (NoSuchAlgorithmException impossible) { throw new IllegalStateException(impossible); }
	}

	record Document(String fileName, String content, String revision) { }
	record Preview(String resolvedContent, String revision, List<String> changes) { }
	record ApplyResult(Document document, List<String> changes, boolean rolledBack) { }
	@FunctionalInterface interface MoveAction { void move(Path source, Path destination) throws IOException; }
	@FunctionalInterface interface TempFileAction { Path create(Path directory, String prefix, String suffix) throws IOException; }
	@SuppressWarnings("serial") static final class StaleRevisionException extends RuntimeException { }
	@SuppressWarnings("serial") static final class ApplyFailureException extends IOException {
		private final boolean rolledBack;
		private ApplyFailureException(boolean rolledBack, Throwable cause) { super(cause); this.rolledBack = rolledBack; }
		boolean rolledBack() { return rolledBack; }
	}
}
