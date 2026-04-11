package com.bencodez.votingplugin.servicesites;

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
import java.util.Locale;
import java.util.Map;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

/**
 * Handles service site name/value mappings.
 *
 * Primary runtime storage is serverdata.yml as a list of strings:
 *
 * ServiceSites:
 *   - "PlanetMinecraft - planetminecraft.com"
 *   - "TopG - topg.org"
 *
 * On startup this class:
 * 1. Loads the list from serverdata.yml into memory.
 * 2. Checks the web sources once for updates and overwrites
 *    serverdata.yml if changed.
 *
 * Lookups are case-insensitive while preserving original casing.
 */
public class ServiceSiteHandler {

	private static final String PRIMARY_URL = "https://raw.githubusercontent.com/wiki/BenCodez/VotingPlugin/Minecraft-Server-Lists.md";
	private static final String SECONDARY_URL = "https://wiki.bencodez.com/en/VotingPlugin/Minecraft-Server-Lists";
	private static final String TERTIARY_URL = "https://wiki-backup.bencodez.com/VotingPlugin/Minecraft-Server-Lists/";

	private static final String SERVERDATA_PATH = "ServiceSites";

	private static final Pattern LINE_PATTERN = Pattern.compile("^\\s*(.+?)\\s+-\\s+(.+?)\\s*$");
	private static final Pattern LI_PATTERN = Pattern.compile("<li>\\s*(.*?)\\s*</li>", Pattern.CASE_INSENSITIVE);

	private final VotingPluginMain plugin;

	/**
	 * Shared HTTP client.
	 */
	private final HttpClient httpClient = HttpClient.newBuilder()
			.followRedirects(Redirect.NORMAL)
			.connectTimeout(Duration.ofSeconds(5))
			.build();

	private final AtomicReference<Map<String, String>> sitesRef = new AtomicReference<>(
			Collections.unmodifiableMap(new TreeMap<>(String.CASE_INSENSITIVE_ORDER)));

	@Getter
	private Map<String, String> serviceSites = sitesRef.get();

	public ServiceSiteHandler(VotingPluginMain plugin) {
		this.plugin = plugin;

		loadFromServerData();
		refreshFromWebOnce(true);

		for (Map.Entry<String, String> entry : sitesRef.get().entrySet()) {
			plugin.extraDebug(entry.getKey() + " - " + entry.getValue());
		}
	}

	public boolean contains(String service) {
		return service != null && sitesRef.get().containsKey(service);
	}

	public String match(String service) {
		if (service == null) {
			return null;
		}
		return sitesRef.get().getOrDefault(service, service);
	}

	public String matchReverse(String value) {
		if (value == null) {
			return null;
		}
		for (Map.Entry<String, String> e : sitesRef.get().entrySet()) {
			if (e.getValue().equalsIgnoreCase(value)) {
				return e.getKey();
			}
		}
		return value;
	}

	public void loadFromServerData() {
		List<String> lines = plugin.getServerData().getData().getStringList(SERVERDATA_PATH);

		Map<String, String> parsed = parseLines(lines, true);

		if (!parsed.isEmpty()) {
			setSites(parsed);
			plugin.extraDebug("Loaded " + parsed.size() + " service sites from serverdata.yml");
		}
	}

	public void saveToServerData() {
		List<String> out = toListFormat(sitesRef.get());
		plugin.getServerData().getData().set(SERVERDATA_PATH, out);
		plugin.getServerData().saveData();
	}

	public void refreshFromWebOnce(boolean logMore) {
		try {
			FetchResult res = fetchWithFallback(PRIMARY_URL, SECONDARY_URL, TERTIARY_URL);
			Map<String, String> parsed = parseFromWebBody(res.body, res.contentType);

			if (parsed.isEmpty()) {
				if (logMore) {
					plugin.extraDebug("ServiceSite refresh returned 0 entries from " + res.sourceUrl);
				}
				return;
			}

			if (mapsEqual(sitesRef.get(), parsed)) {
				if (logMore) {
					plugin.extraDebug("ServiceSite refresh: no changes");
				}
				return;
			}

			setSites(parsed);
			saveToServerData();

			plugin.extraDebug("ServiceSite refresh updated from " + res.sourceUrl + " (" + parsed.size() + " entries)");
		} catch (InterruptedException e) {
			Thread.currentThread().interrupt();
			plugin.debug(e);
		} catch (Exception e) {
			plugin.debug(e);
		}
	}

	public List<String> toListFormat(Map<String, String> map) {
		List<String> out = new ArrayList<>(map.size());
		for (Map.Entry<String, String> e : map.entrySet()) {
			out.add(e.getKey() + " - " + e.getValue());
		}
		return out;
	}

	private void setSites(Map<String, String> newSites) {
		Map<String, String> unmodifiable = Collections.unmodifiableMap(
				new TreeMap<String, String>(String.CASE_INSENSITIVE_ORDER) {
					{
						putAll(newSites);
					}
				});
		sitesRef.set(unmodifiable);
		serviceSites = unmodifiable;
	}

	private FetchResult fetchWithFallback(String... urls) throws IOException, InterruptedException {
		IOException last = null;
		for (String url : urls) {
			try {
				return fetch(url);
			} catch (IOException e) {
				last = e;
				plugin.extraDebug("ServiceSite fetch failed: " + url + " (" + e.getMessage() + ")");
			}
		}
		throw last != null ? last : new IOException("No URLs available");
	}

	private FetchResult fetch(String urlStr) throws IOException, InterruptedException {
		HttpRequest request = HttpRequest.newBuilder()
				.uri(URI.create(urlStr))
				.GET()
				.timeout(Duration.ofSeconds(7))
				.header("User-Agent", "VotingPlugin/ServiceSiteHandler")
				.build();

		HttpResponse<String> response = httpClient.send(request,
				HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8));

		int code = response.statusCode();
		if (code < 200 || code >= 300) {
			throw new IOException("HTTP " + code);
		}

		String contentType = response.headers().firstValue("Content-Type").orElse(null);
		return new FetchResult(urlStr, response.body(), contentType);
	}

	private Map<String, String> parseFromWebBody(String body, String contentType) {
		if (body == null || body.isEmpty()) {
			return new TreeMap<>(String.CASE_INSENSITIVE_ORDER);
		}

		boolean isHtml = (contentType != null && contentType.toLowerCase(Locale.ROOT).contains("text/html"))
				|| body.contains("<html") || body.contains("<li");

		if (isHtml) {
			List<String> lines = new ArrayList<>();
			Matcher m = LI_PATTERN.matcher(body);
			while (m.find()) {
				lines.add(htmlStrip(m.group(1)));
			}
			return parseLines(lines, false);
		}

		String[] split = body.split("\n");
		List<String> lines = new ArrayList<>(split.length);
		for (String s : split) {
			lines.add(stripMarkup(s));
		}
		return parseLines(lines, false);
	}

	private Map<String, String> parseLines(List<String> lines, boolean debugInvalid) {
		Map<String, String> parsed = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

		for (String line : lines) {
			if (line == null) {
				continue;
			}

			String trimmed = line.trim();
			if (trimmed.isEmpty()) {
				continue;
			}

			Matcher m = LINE_PATTERN.matcher(trimmed);
			if (!m.matches()) {
				if (debugInvalid) {
					plugin.extraDebug("Unknown ServiceSites entry: " + trimmed + ", may not be added yet");
				}
				continue;
			}

			String key = m.group(1).trim();
			String val = m.group(2).trim();

			if (!key.isEmpty() && !val.isEmpty()) {
				parsed.put(key, val);
			}
		}

		return parsed;
	}

	private static boolean mapsEqual(Map<String, String> a, Map<String, String> b) {
		if (a.size() != b.size()) {
			return false;
		}
		for (Map.Entry<String, String> e : a.entrySet()) {
			if (!e.getValue().equals(b.get(e.getKey()))) {
				return false;
			}
		}
		return true;
	}

	private static String htmlStrip(String s) {
		String out = s.replaceAll("(?i)<br\\s*/?>", "\n");
		out = out.replaceAll("<[^>]+>", "");
		out = out.replace("&amp;", "&").replace("&lt;", "<").replace("&gt;", ">");
		return out.trim();
	}

	private static String stripMarkup(String s) {
		String out = s.replaceAll("\\[(.*?)]\\((.*?)\\)", "$1");
		out = out.replace("`", "");
		out = out.replaceAll("^\\s*[-*]\\s+", "");
		return out.trim();
	}

	private static final class FetchResult {
		private final String sourceUrl;
		private final String body;
		private final String contentType;

		private FetchResult(String sourceUrl, String body, String contentType) {
			this.sourceUrl = sourceUrl;
			this.body = body;
			this.contentType = contentType;
		}
	}
}