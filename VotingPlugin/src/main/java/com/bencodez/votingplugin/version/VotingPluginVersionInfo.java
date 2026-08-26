package com.bencodez.votingplugin.version;

import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;
import java.security.CodeSource;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.bukkit.configuration.file.YamlConfiguration;

import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;

/** Owns VotingPlugin build metadata embedded in votingpluginversion.yml. */
@Getter
public final class VotingPluginVersionInfo {

	private final VotingPluginMain plugin;
	private String profile = "";
	private String buildNumber = "NOTSET";
	private String time = "";

	public VotingPluginVersionInfo(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void load() {
		YamlConfiguration config = loadVersionFile();
		if (config == null) {
			return;
		}
		time = config.getString("time", "");
		profile = config.getString("profile", "");
		buildNumber = config.getString("buildnumber", "NOTSET");
	}

	private YamlConfiguration loadVersionFile() {
		try {
			CodeSource source = plugin.getClass().getProtectionDomain().getCodeSource();
			if (source == null) {
				return null;
			}
			URL jar = source.getLocation();
			try (ZipInputStream zip = new ZipInputStream(jar.openStream())) {
				ZipEntry entry;
				while ((entry = zip.getNextEntry()) != null) {
					if ("votingpluginversion.yml".equals(entry.getName())) {
						Reader reader = new InputStreamReader(zip);
						return YamlConfiguration.loadConfiguration(reader);
					}
				}
			}
		} catch (Exception e) {
			plugin.debug(e);
		}
		return null;
	}
}
