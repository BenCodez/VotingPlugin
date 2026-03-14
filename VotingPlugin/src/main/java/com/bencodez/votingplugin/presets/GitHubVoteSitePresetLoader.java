package com.bencodez.votingplugin.presets;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

/**
 * Utility class for loading vote site presets directly from a GitHub
 * repository. This loader uses the GitHub REST API and raw file endpoints to
 * enumerate and download preset meta JSON files from the
 * {@code presets/votesites} directory. It can list all available vote site
 * presets and fetch individual presets by their path.
 *
 * To use this loader, supply the owner, repository name and branch (for example
 * "master" or "main") of the presets repo when constructing the loader. The
 * loader performs unauthenticated requests by default; if you encounter rate
 * limits you may specify a personal access token when constructing the loader.
 * The token will be sent in the {@code Authorization} header.
 */
public class GitHubVoteSitePresetLoader {

	/**
	 * The GitHub account or organisation that owns the repository.
	 */
	private final String owner;

	/**
	 * The GitHub repository name containing the presets.
	 */
	private final String repository;

	/**
	 * The branch or ref to read from (e.g. "master" or "main").
	 */
	private final String branch;

	/**
	 * Optional personal access token for authenticated requests. May be
	 * {@code null} to perform unauthenticated requests.
	 */
	private final String token;

	/**
	 * Gson instance used to deserialize JSON into POJOs.
	 */
	private final Gson gson;

	/**
	 * Constructs a new loader for the given GitHub repository using unauthenticated
	 * requests.
	 *
	 * @param owner      the account or organisation name on GitHub
	 * @param repository the repository name containing the presets
	 * @param branch     the branch to read from (e.g. "master" or "main")
	 */
	public GitHubVoteSitePresetLoader(String owner, String repository, String branch) {
		this(owner, repository, branch, null);
	}

	/**
	 * Constructs a new loader for the given GitHub repository.
	 *
	 * @param owner      the account or organisation name on GitHub
	 * @param repository the repository name containing the presets
	 * @param branch     the branch to read from (e.g. "master" or "main")
	 * @param token      a personal access token to authenticate with the GitHub
	 *                   API, or {@code null} to use unauthenticated requests
	 */
	public GitHubVoteSitePresetLoader(String owner, String repository, String branch, String token) {
		this.owner = Objects.requireNonNull(owner, "owner must not be null");
		this.repository = Objects.requireNonNull(repository, "repository must not be null");
		this.branch = Objects.requireNonNull(branch, "branch must not be null");
		this.token = token;
		this.gson = new Gson();
	}

	/**
	 * Lists the relative paths of all vote site preset meta files in the
	 * {@code presets/votesites} directory. This method uses the GitHub contents API
	 * to enumerate files and filters the results for {@code *.meta.json} entries.
	 * If the directory does not exist or there are no meta files, an empty list is
	 * returned.
	 *
	 * @return a list of relative paths (e.g. "presets/votesites/foo.meta.json")
	 * @throws IOException if an I/O error occurs while communicating with the
	 *                     GitHub API
	 */
	public List<String> listPresetPaths() throws IOException {
		String apiUrl = String.format("https://api.github.com/repos/%s/%s/contents/presets/votesites?ref=%s", owner,
				repository, branch);
		HttpURLConnection connection = (HttpURLConnection) new URL(apiUrl).openConnection();
		connection.setRequestMethod("GET");
		connection.setRequestProperty("User-Agent", "VotingPlugin-PresetLoader");
		if (token != null && !token.isEmpty()) {
			connection.setRequestProperty("Authorization", "token " + token);
		}
		int status = connection.getResponseCode();
		if (status != HttpURLConnection.HTTP_OK) {
			// Non-OK response, return empty list
			return Collections.emptyList();
		}
		try (InputStream in = connection.getInputStream();
				Reader reader = new InputStreamReader(in, StandardCharsets.UTF_8)) {
			JsonElement root = JsonParser.parseReader(reader);
			if (!root.isJsonArray()) {
				return Collections.emptyList();
			}
			JsonArray array = root.getAsJsonArray();
			List<String> paths = new ArrayList<>();
			for (JsonElement element : array) {
				if (!element.isJsonObject())
					continue;
				JsonObject obj = element.getAsJsonObject();
				String type = obj.has("type") ? obj.get("type").getAsString() : null;
				String path = obj.has("path") ? obj.get("path").getAsString() : null;
				if ("file".equals(type) && path != null && path.endsWith(".meta.json")) {
					paths.add(path);
				}
			}
			return paths;
		}
	}

	/**
	 * Downloads and deserializes a single vote site preset from the given relative
	 * path. The path should be relative to the repository root (for example
	 * "presets/votesites/crafty-gg.meta.json"). This method uses the
	 * raw.githubusercontent.com endpoint to retrieve the file.
	 *
	 * @param path the relative path of the meta file
	 * @return the deserialized {@link VoteSitePreset}, or {@code null} if the file
	 *         cannot be fetched or parsed
	 * @throws IOException if an I/O error occurs while fetching the file
	 */
	public VoteSitePreset loadPreset(String path) throws IOException {
		Objects.requireNonNull(path, "path must not be null");
		String rawUrl = String.format("https://raw.githubusercontent.com/%s/%s/%s/%s", owner, repository, branch, path);
		HttpURLConnection connection = (HttpURLConnection) new URL(rawUrl).openConnection();
		connection.setRequestMethod("GET");
		connection.setRequestProperty("User-Agent", "VotingPlugin-PresetLoader");
		if (token != null && !token.isEmpty()) {
			connection.setRequestProperty("Authorization", "token " + token);
		}
		int status = connection.getResponseCode();
		if (status != HttpURLConnection.HTTP_OK) {
			return null;
		}
		try (InputStream in = connection.getInputStream();
				Reader reader = new InputStreamReader(in, StandardCharsets.UTF_8)) {
			return gson.fromJson(reader, VoteSitePreset.class);
		}
	}

	/**
	 * Lists and loads all vote site presets available in the repository. This
	 * method first enumerates meta file paths using {@link #listPresetPaths()} and
	 * then downloads each preset using {@link #loadPreset(String)}. If an
	 * individual preset fails to download or parse, it will be skipped.
	 *
	 * @return a list of deserialized {@link VoteSitePreset} objects
	 * @throws IOException if an I/O error occurs while communicating with GitHub
	 */
	public List<VoteSitePreset> listAllVoteSitePresets() throws IOException {
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
			} catch (IOException e) {
				// Skip presets that cannot be fetched or parsed
			}
		}
		return result;
	}
}