package com.bencodez.votingplugin.commands.gui.admin;

import java.util.ArrayList;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.editgui.EditGUI;
import com.bencodez.advancedcore.api.inventory.editgui.EditGUIButton;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueBoolean;
import com.bencodez.advancedcore.api.inventory.editgui.valuetypes.EditGUIValueString;
import com.bencodez.votingplugin.VotingPluginMain;

public class AdminVoteBungeeSettings extends GUIHandler {

	private VotingPluginMain plugin;

	public AdminVoteBungeeSettings(VotingPluginMain plugin, CommandSender player) {
		super(player);
		this.plugin = plugin;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return null;
	}

	@Override
	public void onBook(Player player) {
	}

	@Override
	public void onChat(CommandSender sender) {

	}

	private void setPathData(String path, Object value) {
		plugin.getBungeeSettings().getData().set(path, value);
		plugin.getBungeeSettings().saveData();
		plugin.reload();
	}

	@Override
	public void onChest(Player player) {
		EditGUI inv = new EditGUI("Edit BungeeSettings");
		inv.requirePermission("VotingPlugin.Commands.AdminVote.Edit.BungeeSettings");

		inv.addButton(new EditGUIButton(
				new EditGUIValueBoolean("UseBungeecord", plugin.getBungeeSettings().isUseBungeecoord()) {

					@Override
					public void setValue(Player player, boolean value) {
						setPathData(getKey(), value);
					}
				}));

		inv.addButton(
				new EditGUIButton(new EditGUIValueString("BungeeMethod", plugin.getBungeeSettings().getBungeeMethod()) {

					@Override
					public void setValue(Player player, String value) {
						setPathData(getKey(), value);
					}
				}).addOptions("PLUGINMESSAGING", "SOCKETS", "MYSQL"));

		inv.addButton(
				new EditGUIButton(new EditGUIValueBoolean("BungeeDebug", plugin.getBungeeSettings().isBungeeDebug()) {

					@Override
					public void setValue(Player player, boolean value) {
						setPathData(getKey(), value);
					}
				}));

		inv.addButton(new EditGUIButton(
				new EditGUIValueBoolean("BungeeBroadcast", plugin.getBungeeSettings().isBungeeBroadcast()) {

					@Override
					public void setValue(Player player, boolean value) {
						setPathData(getKey(), value);
					}
				}.addLore("Triggers a broadcast for the entire network, most cases it is best for this to be false")));

		inv.addButton(new EditGUIButton(
				new EditGUIValueBoolean("BungeeBroadcastAlways", plugin.getBungeeSettings().isBungeeBroadcastAlways()) {

					@Override
					public void setValue(Player player, boolean value) {
						setPathData(getKey(), value);
					}
				}.addLore("Whether or not to broadcast on cached votes").addLore("PLUGINMESSAGING ONLY")));

		inv.addButton(new EditGUIButton(
				new EditGUIValueBoolean("PerServerRewards", plugin.getBungeeSettings().isPerServerRewards()) {

					@Override
					public void setValue(Player player, boolean value) {
						setPathData(getKey(), value);
					}
				}));

		inv.addButton(new EditGUIButton(
				new EditGUIValueBoolean("PerServerPoints", plugin.getBungeeSettings().isPerServerPoints()) {

					@Override
					public void setValue(Player player, boolean value) {
						setPathData(getKey(), value);
					}
				}));

		inv.addButton(new EditGUIButton(
				new EditGUIValueBoolean("TriggerVotifierEvent", plugin.getBungeeSettings().isTriggerVotifierEvent()) {

					@Override
					public void setValue(Player player, boolean value) {
						setPathData(getKey(), value);
					}
				}));

		inv.addButton(new EditGUIButton(new EditGUIValueString("Server", plugin.getBungeeSettings().getServer()) {

			@Override
			public void setValue(Player player, String value) {
				setPathData(getKey(), value);
			}
		}.addLore("Server name as stated in proxy")));

		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.CHEST);
	}

}
