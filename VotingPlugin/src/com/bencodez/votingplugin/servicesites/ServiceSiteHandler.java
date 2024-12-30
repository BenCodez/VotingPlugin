package com.bencodez.votingplugin.servicesites;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map.Entry;

import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

public class ServiceSiteHandler {
	@Getter
	private HashMap<String, String> serviceSites = new HashMap<>();
	private VotingPluginMain plugin;

	public ServiceSiteHandler(VotingPluginMain plugin) {
		this.plugin = plugin;
		loadFromGithub();
		for (Entry<String, String> entry : serviceSites.entrySet()) {
			plugin.extraDebug(entry.getKey() + " - " + entry.getValue());
		}

	}

	public boolean contains(String service) {
		for (Entry<String, String> entry : serviceSites.entrySet()) {
			if (entry.getKey().equalsIgnoreCase(service)) {
				return true;
			}
		}
		return false;
	}

	public void loadFromGithub() {
		try {
			readFromWeb("https://github.com/BenCodez/VotingPlugin/wiki/Minecraft-Server-Lists");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public String match(String service) {
		for (Entry<String, String> entry : serviceSites.entrySet()) {
			if (entry.getKey().equalsIgnoreCase(service)) {
				return entry.getValue();
			}
		}
		return service;
	}

	public String matchReverse(String service) {
		for (Entry<String, String> entry : serviceSites.entrySet()) {
			if (entry.getValue().equalsIgnoreCase(service)) {
				return entry.getKey();
			}
		}
		return service;
	}

	public void readFromWeb(String webURL) throws IOException {
		serviceSites.clear();
		URL url = new URL(webURL);
		InputStream is = url.openStream();
		try (BufferedReader br = new BufferedReader(new InputStreamReader(is))) {
			String line;
			boolean rawFound = false;
			while ((line = br.readLine()) != null) {
				if (line.contains("<ul>")) {
					rawFound = true;
				} else if (!line.contains(" - ")) {
					rawFound = false;
				} else if (rawFound) {
					String data = line.replaceAll("<li>", "").replaceAll("</li>", "");
					String[] split = data.split(" - ");
					if (split.length > 0) {
						serviceSites.put(split[0], split[1]);
					}

				}
			}
		} catch (MalformedURLException e) {
			plugin.debug(e);
			// e.printStackTrace();
			// throw new MalformedURLException("URL is malformed!!");
		} catch (IOException e) {
			plugin.debug(e);
			// e.printStackTrace();
			// throw new IOException();
		}

	}
}
