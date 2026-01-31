package com.bencodez.votingplugin.commands.tabcompleter;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.player.PlayerCommandSendEvent;

import com.bencodez.votingplugin.VotingPluginMain;

/**
 * Filters VotingPlugin command aliases from tab completion when aliases are
 * disabled.
 * <p>
 * This listener ONLY hides aliases that are explicitly listed in VotingPlugin's
 * plugin.yml under the "aliases:" section for the "vote" and "adminvote"
 * commands.
 * <p>
 * It will NOT hide unrelated commands such as "/votechallenge" unless that
 * exact command name is an alias defined in VotingPlugin's plugin.yml.
 */
public class AliasCommandFilterListener implements Listener {

	private final VotingPluginMain plugin;

	/**
	 * Cached lowercase alias names (without '/') that should be hidden when alias
	 * loading is disabled.
	 */
	private final Set<String> aliasesToHide;

	/**
	 * Creates a new listener and caches aliases from VotingPlugin's plugin.yml.
	 *
	 * @param plugin VotingPlugin main instance
	 */
	public AliasCommandFilterListener(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.aliasesToHide = buildAliasesToHide(plugin);
	}

	/**
	 * Removes VotingPlugin aliases from the player's available command list when
	 * alias loading is disabled.
	 *
	 * @param event Bukkit command send event
	 */
	@EventHandler
	public void onPlayerCommandSend(PlayerCommandSendEvent event) {
		// Only hide when aliases feature is disabled
		if (plugin.getConfigFile().isLoadCommandAliases()) {
			return;
		}

		// Remove ONLY exact alias command names (not "vote*" prefixes)
		event.getCommands().removeIf(this::shouldHideCommand);
	}

	/**
	 * Determines whether a specific command should be hidden.
	 *
	 * @param cmd Command name (without '/'), possibly namespaced (e.g.
	 *            "otherplugin:vote")
	 * @return true if the command should be hidden
	 */
	private boolean shouldHideCommand(String cmd) {
		String name = cmd.toLowerCase(Locale.ROOT);

		// Never touch namespaced commands (otherplugin:vote)
		if (name.contains(":")) {
			return false;
		}

		// Always keep the primary commands visible
		if (name.equals("vote") || name.equals("adminvote")) {
			return false;
		}

		// Hide only exact aliases defined in VotingPlugin plugin.yml
		return aliasesToHide.contains(name);
	}

	/**
	 * Builds the set of aliases to hide from VotingPlugin's plugin.yml.
	 * <p>
	 * Bukkit exposes the plugin.yml "commands" section via
	 * {@code plugin.getDescription().getCommands()}. Each command entry is a map
	 * that may contain an "aliases" key.
	 *
	 * @param plugin VotingPlugin main instance
	 * @return a lowercase set of alias command names
	 */
	private Set<String> buildAliasesToHide(VotingPluginMain plugin) {
		Set<String> aliases = new HashSet<>();

		Map<String, Map<String, Object>> commands = plugin.getDescription().getCommands();
		if (commands == null || commands.isEmpty()) {
			return Collections.emptySet();
		}

		// Only consider aliases for these main commands
		addAliasesFor(commands, "vote", aliases);
		addAliasesFor(commands, "adminvote", aliases);

		return Collections.unmodifiableSet(aliases);
	}

	/**
	 * Adds aliases for a given command from the plugin.yml commands map into the
	 * provided set.
	 *
	 * @param commandsMap full commands map from PluginDescriptionFile
	 * @param commandName the primary command to read aliases for (e.g. "vote")
	 * @param out         output set to add lowercase alias names into
	 */
	@SuppressWarnings("unchecked")
	private static void addAliasesFor(Map<String, Map<String, Object>> commandsMap, String commandName,
			Set<String> out) {
		Map<String, Object> cmdInfo = commandsMap.get(commandName);
		if (cmdInfo == null) {
			return;
		}

		Object aliasesObj = cmdInfo.get("aliases");
		if (aliasesObj instanceof String) {
			// plugin.yml also allows a single alias as a string
			out.add(((String) aliasesObj).toLowerCase(Locale.ROOT));
		} else if (aliasesObj instanceof Collection) {
			for (Object o : (Collection<Object>) aliasesObj) {
				if (o != null) {
					out.add(o.toString().toLowerCase(Locale.ROOT));
				}
			}
		}
	}
}
