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
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;

import com.bencodez.votingplugin.util.BoundedHttpBodyHandler;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Utility class for loading vote site presets from GitHub.
 */
public class GitHubVoteSitePresetLoader {

	static final int MAX_LIST_RESPONSE_BYTES = 256 * 1024;
	static final int MAX_PRESET_RESPONSE_BYTES = 64 * 1024;
	static final int MAX_PRESET_COUNT = 64;
	private static final Duration REQUEST_TIMEOUT = Duration.ofSeconds(7);
	private static final Pattern PRESET_PATH = Pattern.compile(
			"^presets/votesites/[A-Za-z0-9][A-Za-z0-9._-]{0,127}\\.meta\\.json$");

	private final String owner;
	private final String repository;
	private final String branch;
	private final String token;
	private final Gson gson;

	/**
	 * Shared HTTP client.
	 */
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

	public List<String> listPresetPaths() throws IOException, InterruptedException {
		String apiUrl = String.format(
				"https://api.github.com/repos/%s/%s/contents/presets/votesites?ref=%s",
				owner, repository, branch);

		HttpRequest.Builder builder = HttpRequest.newBuilder()
				.uri(URI.create(apiUrl))
				.GET()
				.timeout(REQUEST_TIMEOUT)
				.header("User-Agent", "VotingPlugin-PresetLoader");

		if (token != null && !token.isEmpty()) {
			builder.header("Authorization", "token " + token);
		}

		HttpResponse<byte[]> response = httpClient.send(builder.build(),
				new BoundedHttpBodyHandler(MAX_LIST_RESPONSE_BYTES, REQUEST_TIMEOUT));

		if (response.statusCode() != 200) {
			return Collections.emptyList();
		}

		return parsePresetPaths(response.body());
	}

	public VoteSitePreset loadPreset(String path) throws IOException, InterruptedException {
		Objects.requireNonNull(path, "path must not be null");
		if (!isPresetPathAllowed(path)) {
			throw new IOException("Invalid vote site preset path");
		}

		String rawUrl = String.format(
				"https://raw.githubusercontent.com/%s/%s/%s/%s",
				owner, repository, branch, path);

		HttpRequest.Builder builder = HttpRequest.newBuilder()
				.uri(URI.create(rawUrl))
				.GET()
				.timeout(REQUEST_TIMEOUT)
				.header("User-Agent", "VotingPlugin-PresetLoader");

		if (token != null && !token.isEmpty()) {
			builder.header("Authorization", "token " + token);
		}

		HttpResponse<byte[]> response = httpClient.send(builder.build(),
				new BoundedHttpBodyHandler(MAX_PRESET_RESPONSE_BYTES, REQUEST_TIMEOUT));

		if (response.statusCode() != 200) {
			return null;
		}

		return gson.fromJson(new String(response.body(), StandardCharsets.UTF_8), VoteSitePreset.class);
	}

	public List<VoteSitePreset> listAllVoteSitePresets() throws IOException, InterruptedException {
		List<String> paths = listPresetPaths();
		if (paths.isEmpty()) {
			return Collections.emptyList();
		}

		List<VoteSitePreset> result = new ArrayList<>();

		for (String path : paths) {
			try {
				VoteSitePreset preset = loadPreset(path);
				if (preset != null) {
					result.add(preset);
				}
			} catch (Exception ignored) {
			}
		}

		return result;
	}

	public VoteSitePreset findVoteSitePresetForURL(String voteURL) throws IOException, InterruptedException {
		if (voteURL == null || voteURL.trim().isEmpty()) {
			return null;
		}

		String host;
		try {
			java.net.URI uri = new java.net.URI(voteURL.trim());
			host = uri.getHost();
		} catch (Exception e) {
			return null;
		}

		if (host == null || host.isEmpty()) {
			return null;
		}

		host = host.toLowerCase();
		if (host.startsWith("www.")) {
			host = host.substring(4);
		}

		List<VoteSitePreset> presets = listAllVoteSitePresets();

		for (VoteSitePreset preset : presets) {
			if (preset == null || preset.getMatch() == null || preset.getMatch().getDomains() == null) {
				continue;
			}

			for (String domain : preset.getMatch().getDomains()) {
				if (domain == null || domain.isEmpty()) {
					continue;
				}

				String normalizedDomain = domain.toLowerCase();
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

	static List<String> parsePresetPaths(byte[] body) throws IOException {
		JsonElement root = JsonParser.parseString(new String(body, StandardCharsets.UTF_8));
		if (!root.isJsonArray()) {
			return Collections.emptyList();
		}

		JsonArray array = root.getAsJsonArray();
		List<String> paths = new ArrayList<>();

		for (JsonElement element : array) {
			if (!element.isJsonObject()) continue;

			JsonObject obj = element.getAsJsonObject();
			String type = obj.has("type") ? obj.get("type").getAsString() : null;
			String path = obj.has("path") ? obj.get("path").getAsString() : null;

			if ("file".equals(type) && isPresetPathAllowed(path)) {
				if (paths.size() >= MAX_PRESET_COUNT) {
					throw new IOException("GitHub returned too many vote site presets");
				}
				paths.add(path);
			}
		}

		return paths;
	}

	static boolean isPresetPathAllowed(String path) {
		return path != null && PRESET_PATH.matcher(path).matches();
	}
}
