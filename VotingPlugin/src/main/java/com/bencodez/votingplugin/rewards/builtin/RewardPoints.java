package com.bencodez.votingplugin.rewards.builtin;

import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.concurrent.CompletionStage;

import org.bukkit.Material;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.inventory.editgui.EditGUIButton;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueNumber;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.rewards.Reward;
import com.bencodez.advancedcore.api.rewards.RewardEditData;
import com.bencodez.advancedcore.api.rewards.injected.RewardInject;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectInt;
import com.bencodez.advancedcore.api.rewards.injected.RewardInjectValidator;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.user.VotingPluginUser;

/** VotingPlugin's injected Points reward. */
public class RewardPoints extends RewardInjectInt {

	private final VotingPluginMain plugin;

	public RewardPoints(VotingPluginMain plugin) {
		super("Points", 0);
		this.plugin = plugin;
		synchronize().asPlaceholder("newpoints").addEditButton(
				new EditGUIButton(new ItemBuilder(Material.PAPER), new EditGUIValueNumber("Points", null) {
					@Override
					public void setValue(Player player, Number value) {
						RewardEditData reward = (RewardEditData) getInv().getData("Reward");
						reward.setValue("Points", value.intValue());
					}
				}.addLore("Give player voting points"))).validator(new RewardInjectValidator() {
					@Override
					public void onValidate(Reward reward, RewardInject inject, ConfigurationSection data) {
						if (data.getInt(inject.getPath(), -1) == 0) {
							warning(reward, inject, "Points can not be 0");
						}
					}
				});
	}

	@Override
	public String onRewardRequest(Reward reward, com.bencodez.advancedcore.api.user.AdvancedCoreUser user, int num,
			HashMap<String, String> placeholders) {
		VotingPluginUser vpUser = plugin.getVotingPluginUserManager().getVotingPluginUser(user);
		// Reward injection is a synchronous chain, so publish the storage-aware
		// predicted total immediately while shared-MySQL persistence stays off the
		// Bukkit/Folia entity lane. Ordinary storage retains its synchronous path.
		String result = "" + vpUser.addPointsStorageAware(num);
		plugin.debug("Setting points to " + result);
		return result;
	}

	@Override
	public boolean supportsAsyncRequest() {
		return true;
	}

	@Override
	public CompletionStage<String> onRewardRequestAsync(Reward reward,
			com.bencodez.advancedcore.api.user.AdvancedCoreUser user, int num,
			HashMap<String, String> placeholders) {
		VotingPluginUser vpUser = plugin.getVotingPluginUserManager().getVotingPluginUser(user);
		String operationId = replayOperationId(vpUser);
		CompletionStage<Integer> addition = operationId == null ? vpUser.addPointsStorageAwareAsync(num)
				: vpUser.addPointsStorageAwareAsync(num, operationId);
		return addition.thenApply(total -> {
			String result = String.valueOf(total);
			plugin.debug("Setting points to " + result);
			return result;
		});
	}

	@Override
	public CompletionStage<Void> onReplayCheckpointPersisted(Reward reward,
			com.bencodez.advancedcore.api.user.AdvancedCoreUser user, String occurrenceId, String injectionKey) {
		VotingPluginUser vpUser = plugin.getVotingPluginUserManager().getVotingPluginUser(user);
		String operationId = replayOperationId(vpUser, occurrenceId, injectionKey);
		return operationId == null ? java.util.concurrent.CompletableFuture.completedFuture(null)
				: vpUser.acknowledgeStorageAwarePointOperation(operationId);
	}

	/**
	 * AdvancedCore #317 exposes a durable occurrence identity for a queued replay
	 * plus its active stage path. Use both when present, but retain compatibility
	 * with releases that cannot distinguish a retry from a new reward occurrence.
	 */
	private static String replayOperationId(VotingPluginUser user) {
		try {
			Method currentReplayOccurrenceId = Reward.class.getMethod("currentReplayOccurrenceId");
			Method currentReplayKey = Reward.class.getMethod("currentReplayKey");
			Object occurrence = currentReplayOccurrenceId.invoke(null);
			Object value = currentReplayKey.invoke(null);
			if (!(occurrence instanceof String) || ((String) occurrence).isEmpty()
					|| !(value instanceof String) || ((String) value).isEmpty()) return null;
			return replayOperationId(user, (String) occurrence, (String) value);
		} catch (ReflectiveOperationException | SecurityException ignored) {
			return null;
		}
	}

	private static String replayOperationId(VotingPluginUser user, String occurrenceId, String injectionKey) {
		if (occurrenceId == null || occurrenceId.isEmpty() || injectionKey == null || injectionKey.isEmpty()) return null;
		return sha256("VotingPlugin:shared-points-reward:v1\0" + user.getUUID() + '\0' + occurrenceId + '\0'
				+ injectionKey);
	}

	private static String sha256(String value) {
		try {
			byte[] digest = MessageDigest.getInstance("SHA-256").digest(value.getBytes(StandardCharsets.UTF_8));
			StringBuilder result = new StringBuilder(digest.length * 2);
			for (byte element : digest) {
				result.append(Character.forDigit((element >>> 4) & 0xf, 16));
				result.append(Character.forDigit(element & 0xf, 16));
			}
			return result.toString();
		} catch (NoSuchAlgorithmException failure) {
			throw new IllegalStateException("SHA-256 is unavailable", failure);
		}
	}
}
