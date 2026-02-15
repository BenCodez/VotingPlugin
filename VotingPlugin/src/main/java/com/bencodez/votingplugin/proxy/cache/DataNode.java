package com.bencodez.votingplugin.proxy.cache;

/**
 * Interface for data node operations.
 */
public interface DataNode {
	/**
	 * Checks if this node is an object.
	 *
	 * @return true if this is an object node
	 */
	boolean isObject();

	/**
	 * Checks if this node is an array.
	 *
	 * @return true if this is an array node
	 */
	boolean isArray();

	/**
	 * Checks if this node is a primitive value.
	 *
	 * @return true if this is a primitive node
	 */
	boolean isPrimitive();

	/**
	 * Gets a child node by key.
	 *
	 * @param key the key
	 * @return the child data node
	 */
	DataNode get(String key);

	/**
	 * Gets a child node by index.
	 *
	 * @param index the index
	 * @return the child data node
	 */
	DataNode get(int index);

	/**
	 * Sets a value for a key.
	 *
	 * @param key the key
	 * @param value the value to set
	 */
	void set(String key, Object value);

	/**
	 * Sets a value at an index.
	 *
	 * @param index the index
	 * @param value the value to set
	 */
	void set(int index, Object value);

	/**
	 * Gets the primitive value.
	 *
	 * @return the primitive object
	 */
	Object asPrimitive();

	/**
	 * Gets the value as a string.
	 *
	 * @return the string value
	 */
	String asString();
	
	/**
	 * Gets the value as a long.
	 *
	 * @return the long value
	 */
	long asLong();
	
	/**
	 * Checks if a key exists.
	 *
	 * @param key the key to check
	 * @return true if the key exists
	 */
    boolean has(String key); 
    
    /**
     * Checks if an index exists.
     *
     * @param index the index to check
     * @return true if the index exists
     */
    boolean has(int index);  

	/**
	 * Gets the value as an int.
	 *
	 * @return the int value
	 */
	int asInt();

	/**
	 * Gets the value as a boolean.
	 *
	 * @return the boolean value
	 */
	boolean asBoolean();

	/**
	 * Returns the internal representation (JsonElement or ConfigurationNode).
	 *
	 * @return the internal object
	 */
	Object toInternal();
}
