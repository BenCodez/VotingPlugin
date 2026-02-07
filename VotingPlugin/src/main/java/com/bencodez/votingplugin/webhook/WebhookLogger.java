package com.bencodez.votingplugin.webhook;

/**
 * Logging abstraction for the webhook subsystem.
 *
 * <p>This avoids hard-coding Bukkit/SLF4J implementations into the core sender.</p>
 */
@FunctionalInterface
public interface WebhookLogger {

	/**
	 * Logs a warning message.
	 *
	 * @param message message
	 */
	void warn(String message);
}
