package com.bencodez.votingplugin.cooldown;

import java.util.TimerTask;

import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;

import com.bencodez.advancedcore.api.rewards.RewardHandler;
import com.bencodez.advancedcore.api.rewards.RewardOptions;
import com.bencodez.advancedcore.api.time.events.DateChangedEvent;
import com.bencodez.advancedcore.api.user.UUID;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.SpecialRewardsConfig;
import com.bencodez.votingplugin.events.PlayerVoteCoolDownEndEvent;
import com.bencodez.votingplugin.objects.User;
import com.bencodez.votingplugin.usermanager.UserManager;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteReminding.
 */
public class CoolDownCheck implements Listener {

	/** The instance. */
	static CoolDownCheck instance = new CoolDownCheck();

	/** The plugin. */
	static VotingPluginMain plugin = VotingPluginMain.plugin;

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
	public CoolDownCheck(VotingPluginMain plugin) {
		CoolDownCheck.plugin = plugin;
	}

	public void checkAll() {
		VotingPluginMain.plugin.getTimer().schedule(new TimerTask() {

			@Override
			public void run() {
				if (VotingPluginMain.plugin != null) {
					for (String uuid : UserManager.getInstance().getAllUUIDs()) {
						if (VotingPluginMain.plugin != null) {
							User user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
							if (user.getUserData().hasData() && user.hasLoggedOnBefore()) {
								user.checkCoolDownEvents();
							}
						} else {
							cancel();
						}
					}
				} else {
					cancel();
				}
			}
		}, 1000 * 60 * 3);

	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCoolDownEnd(PlayerVoteCoolDownEndEvent event) {
		RewardHandler.getInstance().giveReward(event.getPlayer(), SpecialRewardsConfig.getInstance().getData(),
				"VoteCoolDownEndedReward",
				new RewardOptions().addPlaceholder("Votesite", event.getVoteSite().getDisplayName()));
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onDayChangeEnd(DateChangedEvent event) {
		checkAll();
	}
}
