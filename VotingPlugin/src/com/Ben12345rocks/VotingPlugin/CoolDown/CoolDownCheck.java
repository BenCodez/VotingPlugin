package com.Ben12345rocks.VotingPlugin.CoolDown;

import org.bukkit.Bukkit;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.Ben12345rocks.AdvancedCore.Rewards.RewardHandler;
import com.Ben12345rocks.AdvancedCore.Rewards.RewardOptions;
import com.Ben12345rocks.AdvancedCore.TimeChecker.Events.DateChangedEvent;
import com.Ben12345rocks.AdvancedCore.UserManager.UUID;
import com.Ben12345rocks.AdvancedCore.UserManager.UserManager;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Events.PlayerVoteCoolDownEndEvent;
import com.Ben12345rocks.VotingPlugin.Objects.User;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteReminding.
 */
public class CoolDownCheck implements Listener {

	/** The instance. */
	static CoolDownCheck instance = new CoolDownCheck();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of VoteReminding.
	 *
	 * @return single instance of VoteReminding
	 */
	public static CoolDownCheck getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new vote reminding.
	 */
	private CoolDownCheck() {
	}

	/**
	 * Instantiates a new vote reminding.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public CoolDownCheck(Main plugin) {
		CoolDownCheck.plugin = plugin;
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onDayChangeEnd(DateChangedEvent event) {
		checkAll();
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteCoolDownEndEvent event) {
		RewardHandler.getInstance().giveReward(event.getPlayer(), Config.getInstance().getData(),
				"VoteCoolDownEndedReward",
				new RewardOptions().addPlaceholder("Votesite", event.getVoteSite().getDisplayName()));
	}

	public void checkAll() {
		Bukkit.getScheduler().runTaskAsynchronously(plugin, new Runnable() {

			@SuppressWarnings("deprecation")
			@Override
			public void run() {
				for (String uuid : UserManager.getInstance().getAllUUIDs()) {
					User user = new User(new UUID(uuid));
					if (user.getUserData().hasData() && user.hasLoggedOnBefore()) {
						user.checkCoolDownEvents();
					}
				}
			}
		});

	}
}
