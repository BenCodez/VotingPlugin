package com.bencodez.votingplugin.bungee.velocity;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.spongepowered.configurate.ConfigurateException;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.serialize.SerializationException;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import io.leangen.geantyref.TypeToken;
import lombok.Getter;

public class VelocityYMLFile {
	@Getter
	private File file;
	@Getter
	private ConfigurationNode conf;
	private YamlConfigurationLoader loader;

	public VelocityYMLFile(File file) {
		this.file = file;
		loader = YamlConfigurationLoader.builder().file(file).build();
		try {
			conf = loader.load();
		} catch (ConfigurateException e) {
			e.printStackTrace();
		}
	}

	public ConfigurationNode getData() {
		return conf;
	}

	public ConfigurationNode getNode(Object... path) {
		return getData().node(path);
	}

	public List<String> getStringList(ConfigurationNode node, ArrayList<String> def) {
		try {
			return node.getList(TypeToken.get(String.class), def);
		} catch (SerializationException e) {
			e.printStackTrace();
			return def;
		}
	}

	public String getString(ConfigurationNode node, String def) {
		return node.getString(def);
	}

	public boolean getBoolean(ConfigurationNode node, boolean def) {
		return node.getBoolean(def);
	}

	public int getInt(ConfigurationNode node, int def) {
		return node.getInt(def);
	}
	
	public ArrayList<String> getKeys(ConfigurationNode node) {
		ArrayList<String> keys = new ArrayList<String>();
		for (ConfigurationNode key : node.childrenList()) {
			keys.add(key.key().toString());
		}
		return keys;
	}

	public void save() {
		try {
			loader.save(conf);
		} catch (ConfigurateException e) {
			e.printStackTrace();
		}
	}
}
