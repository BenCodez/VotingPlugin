package com.bencodez.votingplugin.proxy.cache;

import com.google.gson.Gson;
import com.google.gson.JsonElement;
import com.google.gson.JsonPrimitive;

public class GsonDataNode implements DataNode {

	private final JsonElement element;

	public GsonDataNode(JsonElement element) {
		this.element = element;
	}

	@Override
	public boolean isObject() {
		return element.isJsonObject();
	}

	@Override
	public boolean isArray() {
		return element.isJsonArray();
	}

	@Override
	public boolean isPrimitive() {
		return element.isJsonPrimitive();
	}

	@Override
	public DataNode get(String key) {
		if (!isObject())
			return null;
		return new GsonDataNode(element.getAsJsonObject().get(key));
	}

	@Override
	public DataNode get(int index) {
		if (!isArray())
			return null;
		return new GsonDataNode(element.getAsJsonArray().get(index));
	}

	@Override
	public void set(String key, Object value) {
		if (!isObject())
			return;
		element.getAsJsonObject().add(key, new Gson().toJsonTree(value));
	}

	@Override
	public void set(int index, Object value) {
		if (!isArray())
			return;
		element.getAsJsonArray().set(index, new Gson().toJsonTree(value));
	}

	@Override
	public long asLong() {
		if (element == null || !element.isJsonPrimitive())
			return 0L;

		JsonPrimitive prim = element.getAsJsonPrimitive();

		if (prim.isNumber()) {
			return prim.getAsLong();
		}

		// Try parsing from string if it’s not a number
		try {
			return Long.parseLong(prim.getAsString());
		} catch (NumberFormatException e) {
			return 0L;
		}
	}

	@Override
	public Object asPrimitive() {
		if (!isPrimitive())
			return null;
		JsonPrimitive prim = element.getAsJsonPrimitive();
		if (prim.isBoolean())
			return prim.getAsBoolean();
		if (prim.isNumber())
			return prim.getAsNumber();
		return prim.getAsString();
	}

	@Override
	public boolean has(String key) {
		return isObject() && element.getAsJsonObject().has(key);
	}

	@Override
	public boolean has(int index) {
		return isArray() && index >= 0 && index < element.getAsJsonArray().size();
	}

	@Override
	public String asString() {
		return element.getAsString();
	}

	@Override
	public int asInt() {
		return element.getAsInt();
	}

	@Override
	public boolean asBoolean() {
		return element.getAsBoolean();
	}

	@Override
	public Object toInternal() {
		return element;
	}
}
