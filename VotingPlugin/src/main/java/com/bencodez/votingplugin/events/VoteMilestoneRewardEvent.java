package com.bencodez.votingplugin.events;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.bukkit.event.Event;
import org.bukkit.event.HandlerList;

import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestone;
import com.bencodez.votingplugin.user.VotingPluginUser;

import lombok.Getter;

/**
 * Fired after a VoteMilestone reward has been executed successfully.
 */
public class VoteMilestoneRewardEvent extends Event {

	private static final HandlerList HANDLERS = new HandlerList();

	private final VotingPluginUser user;
	private final UUID uuid;
	private final String playerName;

	private final VoteMilestone milestone;
	private final long value;

	private final String groupId;
	private final String rewardPath;
	@Getter
	private final UUID voteUUID;
	private final Map<String, String> placeholders;

	/**
	 * Constructs a new VoteMilestoneRewardEvent.
	 *
	 * @param user         VotingPluginUser who received the reward
	 * @param milestone    milestone that triggered
	 * @param value        resolved total value used for matching
	 * @param groupId      resolved group id used for selection
	 * @param rewardPath   path used to execute rewards
	 * @param placeholders placeholders passed to the reward handler
	 * @param voteUUID     UUID of the vote that triggered this reward (may be null)
	 */
	public VoteMilestoneRewardEvent(VotingPluginUser user, VoteMilestone milestone, long value, String groupId,
			String rewardPath, Map<String, String> placeholders, UUID voteUUID) {
		super(true);
		this.user = user;
		this.uuid = user == null ? null : user.getJavaUUID();
		this.playerName = user == null ? null : user.getPlayerName();
		this.milestone = milestone;
		this.value = value;
		this.groupId = groupId;
		this.rewardPath = rewardPath;
		this.voteUUID = voteUUID;
		if (placeholders == null || placeholders.isEmpty()) {
			this.placeholders = Collections.emptyMap();
		} else {
			this.placeholders = Collections.unmodifiableMap(new HashMap<>(placeholders));
		}
	}

	/**
	 * Gets the voting plugin user associated with this event.
	 *
	 * @return the voting plugin user
	 */
	public VotingPluginUser getUser() {
		return user;
	}

	/**
	 * Gets the player UUID.
	 *
	 * @return the player UUID
	 */
	public UUID getUuid() {
		return uuid;
	}

	/**
	 * Gets the player name.
	 *
	 * @return the player name
	 */
	public String getPlayerName() {
		return playerName;
	}

	/**
	 * Gets the milestone that triggered this event.
	 *
	 * @return milestone instance
	 */
	public VoteMilestone getMilestone() {
		return milestone;
	}

	/**
	 * Gets the milestone ID.
	 *
	 * @return milestone id string
	 */
	public String getMilestoneId() {
		return milestone == null ? null : milestone.getId();
	}

	/**
	 * Gets the value that triggered the milestone.
	 *
	 * @return the milestone value
	 */
	public long getValue() {
		return value;
	}

	/**
	 * Gets the group ID used for selection.
	 *
	 * @return group id string
	 */
	public String getGroupId() {
		return groupId;
	}

	/**
	 * Gets the configuration path for the rewards.
	 *
	 * @return the reward path
	 */
	public String getRewardPath() {
		return rewardPath;
	}

	/**
	 * Gets the placeholders map for this milestone reward.
	 *
	 * @return map of placeholder names to values
	 */
	public Map<String, String> getPlaceholders() {
		return placeholders;
	}

	@Override
	public HandlerList getHandlers() {
		return HANDLERS;
	}

	/**
	 * Gets the handler list for this event.
	 *
	 * @return handler list
	 */
	public static HandlerList getHandlerList() {
		return HANDLERS;
	}
}
