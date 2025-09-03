package com.bencodez.votingplugin.proxy.cache;

public interface DataNode {
	boolean isObject();

	boolean isArray();

	boolean isPrimitive();

	DataNode get(String key);

	DataNode get(int index);

	void set(String key, Object value);

	void set(int index, Object value);

	Object asPrimitive();

	String asString();
	
	long asLong();
	
    boolean has(String key); 
    boolean has(int index);  

	int asInt();

	boolean asBoolean();

	Object toInternal(); // Returns either JsonElement or ConfigurationNode
}
