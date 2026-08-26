package com.bencodez.votingplugin.rewards.builtin;

import java.util.HashMap;

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
		String result = "" + vpUser.addPoints(num);
		plugin.debug("Setting points to " + result);
		return result;
	}
}
