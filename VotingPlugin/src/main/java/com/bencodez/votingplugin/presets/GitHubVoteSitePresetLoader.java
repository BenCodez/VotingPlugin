package com.bencodez.votingplugin.presets;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpClient.Redirect;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Pattern;

import com.bencodez.votingplugin.util.BoundedHttpBodyHandler;
import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Utility class for loading vote site presets from GitHub.
 *
 * Successful live snapshots are cached in memory so repeated preset commands do
 * not repeatedly query GitHub. Failed or partial refreshes never replace a
 * previously complete snapshot.
 */
public class GitHubVoteSitePresetLoader {
	static final int MAX_LIST_RESPONSE_BYTES = 256 * 1024;
	static final int MAX_PRESET_RESPONSE_BYTES = 64 * 1024;
	static final int MAX_PRESET_COUNT = 64;
	static final Duration REQUEST_TIMEOUT = Duration.ofSeconds(7);
	static final Duration REFRESH_TIMEOUT = Duration.ofSeconds(20);
	static final long CACHE_TTL_NANOS = Duration.ofMinutes(30).toNanos();
	static final long REFRESH_RETRY_NANOS = Duration.ofMinutes(1).toNanos();
	private static final Pattern PRESET_PATH = Pattern.compile(
			"^presets/votesites/[A-Za-z0-9][A-Za-z0-9._-]{0,127}\\.meta\\.json$");

	private final String owner;
	private final String repository;
	private final String branch;
	private final String token;
	private final Gson gson;
	private final AtomicReference<List<VoteSitePreset>> cachedPresets = new AtomicReference<>(Collections.emptyList());
	private volatile long lastSuccessfulRefreshNanos;
	private volatile long lastRefreshAttemptNanos;

	/** Shared HTTP client. */
	private final HttpClient httpClient = HttpClient.newBuilder()
			.followRedirects(Redirect.NORMAL)
			.connectTimeout(Duration.ofSeconds(5))
			.build();

	public GitHubVoteSitePresetLoader(String owner, String repository, String branch) {
		this(owner, repository, branch, null);
	}

	public GitHubVoteSitePresetLoader(String owner, String repository, String branch, String token) {
		this.owner = Objects.requireNonNull(owner);
		this.repository = Objects.requireNonNull(repository);
		this.branch = Objects.requireNonNull(branch);
		this.token = token;
		this.gson = new Gson();
	}

	/** Returns the current immutable in-memory snapshot without performing network I/O. */
	public List<VoteSitePreset> getCachedVoteSitePresets() {
		return cachedPresets.get();
	}

	/** Returns whether the current snapshot came from a recent complete GitHub refresh. */
	public boolean hasFreshRemoteSnapshot() {
		long lastRefresh = lastSuccessfulRefreshNanos;
		return lastRefresh != 0L && System.nanoTime() - lastRefresh < CACHE_TTL_NANOS;
	}

	/** Returns whether another live refresh is outside the failed-refresh retry cooldown. */
	public boolean needsRefresh() {
		if (hasFreshRemoteSnapshot()) return false;
		long lastAttempt = lastRefreshAttemptNanos;
		return lastAttempt == 0L || System.nanoTime() - lastAttempt >= REFRESH_RETRY_NANOS;
	}

	public List<String> listPresetPaths() throws IOException, InterruptedException {
		return listPresetPaths(System.nanoTime() + REQUEST_TIMEOUT.toNanos());
	}

	private List<String> listPresetPaths(long deadline) throws IOException, InterruptedException {
		String apiUrl = String.format(
				"https://api.github.com/repos/%s/%s/contents/presets/votesites?ref=%s",
				owner, repository, branch);

		Duration timeout = remainingTimeout(deadline);
		HttpRequest.Builder builder = HttpRequest.newBuilder()
				.uri(URI.create(apiUrl))
				.GET()
				.timeout(timeout)
				.header("User-Agent", "VotingPlugin-PresetLoader");

		if (token != null && !token.isEmpty()) {
			builder.header("Authorization", "token " + token);
		}

		HttpResponse<byte[]> response = httpClient.send(builder.build(),
				new BoundedHttpBodyHandler(MAX_LIST_RESPONSE_BYTES, timeout));
		return parsePresetPaths(requireSuccessfulResponse(response));
	}

	public VoteSitePreset loadPreset(String path) throws IOException, InterruptedException {
		return loadPreset(path, System.nanoTime() + REQUEST_TIMEOUT.toNanos());
	}

	private VoteSitePreset loadPreset(String path, long deadline) throws IOException, InterruptedException {
		Objects.requireNonNull(path, "path must not be null");
		if (!isPresetPathAllowed(path)) {
			throw new IOException("Invalid vote site preset path");
		}

		String rawUrl = String.format(
				"https://raw.githubusercontent.com/%s/%s/%s/%s",
				owner, repository, branch, path);

		Duration timeout = remainingTimeout(deadline);
		HttpRequest.Builder builder = HttpRequest.newBuilder()
				.uri(URI.create(rawUrl))
				.GET()
				.timeout(timeout)
				.header("User-Agent", "VotingPlugin-PresetLoader");

		if (token != null && !token.isEmpty()) {
			builder.header("Authorization", "token " + token);
		}

		HttpResponse<byte[]> response = httpClient.send(builder.build(),
				new BoundedHttpBodyHandler(MAX_PRESET_RESPONSE_BYTES, timeout));
		byte[] body = requireSuccessfulResponse(response);
		try {
			return gson.fromJson(new String(body, StandardCharsets.UTF_8), VoteSitePreset.class);
		} catch (RuntimeException parseFailure) {
			throw new IOException("Invalid vote site preset JSON", parseFailure);
		}
	}

	/**
	 * Returns the recent cached snapshot or performs one bounded live refresh.
	 * A complete older snapshot is retained if a later refresh is partial or
	 * fails.
	 */
	public synchronized List<VoteSitePreset> listAllVoteSitePresets() throws IOException, InterruptedException {
		List<VoteSitePreset> cached = cachedPresets.get();
		if (hasFreshRemoteSnapshot()) {
			return cached;
		}
		if (!needsRefresh()) {
			if (!cached.isEmpty()) return cached;
			throw new IOException("Vote site preset refresh is temporarily unavailable");
		}

		lastRefreshAttemptNanos = System.nanoTime();
		long deadline = System.nanoTime() + REFRESH_TIMEOUT.toNanos();
		try {
			List<String> paths = listPresetPaths(deadline);
			if (paths.isEmpty()) {
				throw new IOException("GitHub returned no vote site presets");
			}

			List<VoteSitePreset> refreshed = new ArrayList<>(paths.size());
			IOException firstFailure = null;
			for (String path : paths) {
				try {
					VoteSitePreset preset = loadPreset(path, deadline);
					if (preset != null) refreshed.add(preset);
					else if (firstFailure == null) firstFailure = new IOException("Empty vote site preset: " + path);
				} catch (IOException failure) {
					if (firstFailure == null) firstFailure = failure;
				}
			}

			if (refreshed.isEmpty()) {
				if (firstFailure != null) throw firstFailure;
				throw new IOException("GitHub returned no usable vote site presets");
			}

			// Never replace a known-complete snapshot with a partial refresh.
			if (firstFailure != null && !cached.isEmpty()) {
				return cached;
			}

			List<VoteSitePreset> immutable = immutablePresets(refreshed);
			cachedPresets.set(immutable);
			if (firstFailure == null) {
				lastSuccessfulRefreshNanos = System.nanoTime();
			}
			return immutable;
		} catch (IOException failure) {
			if (!cached.isEmpty()) {
				return cached;
			}
			throw failure;
		}
	}

	public VoteSitePreset findVoteSitePresetForURL(String voteURL) throws IOException, InterruptedException {
		String host = normalizeVoteHost(voteURL);
		if (host == null) return null;
		return findVoteSitePresetForHost(host, listAllVoteSitePresets());
	}

	/** Performs URL matching against a supplied snapshot without any network I/O. */
	public VoteSitePreset findVoteSitePresetForURL(String voteURL, List<VoteSitePreset> presets) {
		String host = normalizeVoteHost(voteURL);
		if (host == null || presets == null || presets.isEmpty()) return null;
		return findVoteSitePresetForHost(host, presets);
	}

	private VoteSitePreset findVoteSitePresetForHost(String host, List<VoteSitePreset> presets) {
		for (VoteSitePreset preset : presets) {
			if (preset == null || preset.getMatch() == null || preset.getMatch().getDomains() == null) {
				continue;
			}

			for (String domain : preset.getMatch().getDomains()) {
				if (domain == null || domain.isEmpty()) continue;

				String normalizedDomain = domain.toLowerCase(Locale.ROOT);
				if (normalizedDomain.startsWith("www.")) {
					normalizedDomain = normalizedDomain.substring(4);
				}

				if (host.equals(normalizedDomain) || host.endsWith("." + normalizedDomain)) {
					return preset;
				}
			}
		}
		return null;
	}

	private static String normalizeVoteHost(String voteURL) {
		if (voteURL == null || voteURL.trim().isEmpty()) return null;
		try {
			String host = new URI(voteURL.trim()).getHost();
			if (host == null || host.isEmpty()) return null;
			host = host.toLowerCase(Locale.ROOT);
			return host.startsWith("www.") ? host.substring(4) : host;
		} catch (Exception ignored) {
			return null;
		}
	}

	static List<String> parsePresetPaths(byte[] body) throws IOException {
		try {
			JsonElement root = JsonParser.parseString(new String(body, StandardCharsets.UTF_8));
			if (!root.isJsonArray()) {
				throw new IOException("GitHub preset listing is not an array");
			}

			LinkedHashSet<String> paths = new LinkedHashSet<>();
			for (JsonElement element : root.getAsJsonArray()) {
				if (!element.isJsonObject()) continue;
				JsonObject object = element.getAsJsonObject();
				String type = object.has("type") ? object.get("type").getAsString() : null;
				String path = object.has("path") ? object.get("path").getAsString() : null;
				if (!"file".equals(type) || !isPresetPathAllowed(path)) continue;
				paths.add(path);
				if (paths.size() > MAX_PRESET_COUNT) {
					throw new IOException("GitHub returned too many vote site presets");
				}
			}
			return new ArrayList<>(paths);
		} catch (IOException failure) {
			throw failure;
		} catch (RuntimeException parseFailure) {
			throw new IOException("Invalid GitHub preset listing", parseFailure);
		}
	}

	static boolean isPresetPathAllowed(String path) {
		return path != null && PRESET_PATH.matcher(path).matches();
	}

	static byte[] requireSuccessfulResponse(HttpResponse<byte[]> response) throws IOException {
		if (response.statusCode() != 200) {
			throw new IOException("Preset request returned HTTP " + response.statusCode());
		}
		return response.body();
	}

	private static Duration remainingTimeout(long deadline) throws IOException {
		long remaining = deadline - System.nanoTime();
		if (remaining <= 0L) {
			throw new IOException("Vote site preset refresh timed out");
		}
		return Duration.ofNanos(Math.min(REQUEST_TIMEOUT.toNanos(), remaining));
	}

	private static List<VoteSitePreset> immutablePresets(List<VoteSitePreset> presets) {
		return Collections.unmodifiableList(new ArrayList<>(presets));
	}
}
