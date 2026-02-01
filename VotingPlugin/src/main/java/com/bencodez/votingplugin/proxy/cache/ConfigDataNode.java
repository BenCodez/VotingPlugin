package com.bencodez.votingplugin.proxy.cache;

import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.serialize.SerializationException;

public class ConfigDataNode implements DataNode {

	private final ConfigurationNode node;

	public ConfigDataNode(ConfigurationNode node) {
		this.node = node;
	}

	@Override
	public boolean isObject() {
		return node.isMap();
	}

	@Override
	public boolean isArray() {
		return node.isList();
	}

	@Override
	public boolean isPrimitive() {
		return !isObject() && !isArray();
	}

	@Override
	public DataNode get(String key) {
		return new ConfigDataNode(node.node((Object) key));
	}
	
	@Override
	public long asLong() {
	    return node.getLong();
	}

	
    @Override
    public boolean has(String key) {
        ConfigurationNode child = node.node((Object) key);
        return child != null && child.raw() != null;
    }

    @Override
    public boolean has(int index) {
        ConfigurationNode child = node.node(index);
        return child != null && child.raw() != null;
    }

	@Override
	public DataNode get(int index) {
		return new ConfigDataNode(node.node(index));
	}

	@Override
	public void set(String key, Object value) {
		try {
			node.node((Object) key).set(value);
		} catch (SerializationException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void set(int index, Object value) {
		try {
			node.node(index).set(value);
		} catch (SerializationException e) {
			e.printStackTrace();
		}
	}

	@Override
	public Object asPrimitive() {
		return node.raw();
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