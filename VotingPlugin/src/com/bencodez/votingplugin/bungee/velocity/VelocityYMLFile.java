package com.bencodez.votingplugin.bungee.velocity;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.google.common.reflect.TypeToken;

import lombok.Getter;
import ninja.leaping.configurate.ConfigurationNode;
import ninja.leaping.configurate.objectmapping.ObjectMappingException;
import ninja.leaping.configurate.yaml.YAMLConfigurationLoader;

public class VelocityYMLFile {
	@Getter
	private File file;
	@Getter
	private ConfigurationNode conf;
	private YAMLConfigurationLoader loader;

	public VelocityYMLFile(File file) {

		this.file = file;
		if (!file.exists()) {
			try {
				file.getParentFile().mkdirs();
				file.createNewFile();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		loader = YAMLConfigurationLoader.builder().setPath(file.toPath()).build();

		try {
			conf = loader.load();
		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	public ConfigurationNode getData() {
		return conf;
	}

	public ConfigurationNode getNode(Object... path) {
		return getData().getNode(path);
	}

	public List<String> getStringList(ConfigurationNode node, ArrayList<String> def) {
		try {
			return node.getList(TypeToken.of(String.class), def);
		} catch (ObjectMappingException e) {
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
		for (ConfigurationNode key : node.getChildrenList()) {
			keys.add(key.getKey().toString());
		}
		return keys;
	}
	
	public void reload() {
		loader = YAMLConfigurationLoader.builder().setPath(file.toPath()).build();

		try {
			conf = loader.load();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void save() {
		try {
			loader.save(conf);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
