package com.bencodez.votingplugin.util;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.file.YamlConfiguration;

/** Isolates and guards only reward templates that can complete an untrusted action token. */
public final class RewardActionTemplateGuard {
	private RewardActionTemplateGuard() { }

	public static ConfigurationSection isolate(ConfigurationSection root, String path,
			Map<String, String> untrustedPlaceholders) {
		if (root == null || untrustedPlaceholders == null || untrustedPlaceholders.isEmpty()) return root;
		Object value = root.get(path);
		if (!containsBoundary(value, untrustedPlaceholders)) return root;
		YamlConfiguration isolated = new YamlConfiguration();
		copyValue(isolated, path, value, untrustedPlaceholders);
		return isolated;
	}

	private static void copyValue(ConfigurationSection target, String path, Object value,
			Map<String, String> placeholders) {
		if (value instanceof ConfigurationSection section) {
			ConfigurationSection copy = target.createSection(path);
			for (Map.Entry<String, Object> entry : section.getValues(false).entrySet()) {
				copyValue(copy, entry.getKey(), entry.getValue(), placeholders);
			}
			return;
		}
		target.set(path, copyObject(value, placeholders));
	}

	private static Object copyObject(Object value, Map<String, String> placeholders) {
		if (value instanceof String text) {
			return ServiceSiteValidator.inertTemplateBoundaries(text, placeholders);
		}
		if (value instanceof java.util.List<?> list) {
			ArrayList<Object> copy = new ArrayList<>(list.size());
			for (Object entry : list) copy.add(copyObject(entry, placeholders));
			return copy;
		}
		if (value instanceof Map<?, ?> map) {
			LinkedHashMap<Object, Object> copy = new LinkedHashMap<>();
			for (Map.Entry<?, ?> entry : map.entrySet()) {
				copy.put(entry.getKey(), copyObject(entry.getValue(), placeholders));
			}
			return copy;
		}
		return value;
	}

	private static boolean containsBoundary(Object value, Map<String, String> placeholders) {
		if (value instanceof String text) {
			return !text.equals(ServiceSiteValidator.inertTemplateBoundaries(text, placeholders));
		}
		if (value instanceof ConfigurationSection section) value = section.getValues(false);
		if (value instanceof Map<?, ?> map) {
			for (Object nested : map.values()) if (containsBoundary(nested, placeholders)) return true;
		} else if (value instanceof Iterable<?> values) {
			for (Object nested : values) if (containsBoundary(nested, placeholders)) return true;
		}
		return false;
	}
}
