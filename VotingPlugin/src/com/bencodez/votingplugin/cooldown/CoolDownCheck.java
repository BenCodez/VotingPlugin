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
import com.bencodez.votingplugin.events.PlayerVoteCoolDownEndEvent;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

// TODO: Auto-generated Javadoc
/**
 * The Class VoteReminding.
 */
public class CoolDownCheck implements Listener {

	private VotingPluginMain plugin;

	/**
	 * Instantiates a new vote reminding.
	 *
	 * @param plugin the plugin
	 */
	public CoolDownCheck(VotingPluginMain plugin) {
		this.plugin = plugin;
	}

	public void checkAll() {
		plugin.getTimer().schedule(new TimerTask() {

			@Override
			public void run() {
				if (VotingPluginMain.plugin != null) {
					for (String uuid : UserManager.getInstance().getAllUUIDs()) {
						if (VotingPluginMain.plugin != null) {
							VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(new UUID(uuid));
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
		RewardHandler.getInstance().giveReward(event.getPlayer(), plugin.getSpecialRewardsConfig().getData(),
				"VoteCoolDownEndedReward",
				new RewardOptions().addPlaceholder("Votesite", event.getVoteSite().getDisplayName()));
	}

	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onDayChangeEnd(DateChangedEvent event) {
		checkAll();
	}
}
