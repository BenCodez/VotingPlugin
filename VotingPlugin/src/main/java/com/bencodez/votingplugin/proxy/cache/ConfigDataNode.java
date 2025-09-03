package com.bencodez.votingplugin.proxy.cache;

import ninja.leaping.configurate.ConfigurationNode;

public class ConfigDataNode implements DataNode {

	private final ConfigurationNode node;

	public ConfigDataNode(ConfigurationNode node) {
		this.node = node;
	}

	@Override
	public boolean isObject() {
		return node.hasMapChildren();
	}

	@Override
	public boolean isArray() {
		return node.hasListChildren();
	}

	@Override
	public boolean isPrimitive() {
		return !isObject() && !isArray();
	}

	@Override
	public DataNode get(String key) {
		return new ConfigDataNode(node.getNode((Object) key));
	}
	
	@Override
	public long asLong() {
	    return node.getLong();
	}

	
    @Override
    public boolean has(String key) {
        ConfigurationNode child = node.getNode((Object) key);
        return child != null && child.getValue() != null;
    }

    @Override
    public boolean has(int index) {
        ConfigurationNode child = node.getNode(index);
        return child != null && child.getValue() != null;
    }

	@Override
	public DataNode get(int index) {
		return new ConfigDataNode(node.getNode(index));
	}

	@Override
	public void set(String key, Object value) {
		node.getNode((Object) key).setValue(value);
	}

	@Override
	public void set(int index, Object value) {
		node.getNode(index).setValue(value);
	}

	@Override
	public Object asPrimitive() {
		return node.getValue();
	}

	@Override
	public String asString() {
		return node.getString();
	}

	@Override
	public int asInt() {
		return node.getInt();
	}

	@Override
	public boolean asBoolean() {
		return node.getBoolean();
	}

	@Override
	public Object toInternal() {
		return node;
	}
}
