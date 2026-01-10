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

	public VotingPluginUser getUser() {
		return user;
	}

	public UUID getUuid() {
		return uuid;
	}

	public String getPlayerName() {
		return playerName;
	}

	public VoteMilestone getMilestone() {
		return milestone;
	}

	public String getMilestoneId() {
		return milestone == null ? null : milestone.getId();
	}

	public long getValue() {
		return value;
	}

	public String getGroupId() {
		return groupId;
	}

	public String getRewardPath() {
		return rewardPath;
	}

	public Map<String, String> getPlaceholders() {
		return placeholders;
	}

	@Override
	public HandlerList getHandlers() {
		return HANDLERS;
	}

	public static HandlerList getHandlerList() {
		return HANDLERS;
	}
}
