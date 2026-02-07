package com.bencodez.votingplugin.webhook;

/**
 * Placeholder expansion abstraction.
 *
 * <p>VotingPlugin should supply an implementation that expands placeholders
 * such as {PLAYER}, {SERVICE_SITE}, etc.</p>
 */
@FunctionalInterface
public interface PlaceholderExpander {

	/**
	 * Expands placeholders inside a string.
	 *
	 * @param input input string (may be null)
	 * @return expanded string (never null)
	 */
	String expand(String input);
}
