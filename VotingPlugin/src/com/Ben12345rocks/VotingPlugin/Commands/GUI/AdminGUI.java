package com.Ben12345rocks.VotingPlugin.Commands.GUI;

import java.util.ArrayList;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.AdvancedCore.Util.EditGUI.EditGUI;
import com.Ben12345rocks.AdvancedCore.Util.EditGUI.EditGUIButton;
import com.Ben12345rocks.AdvancedCore.Util.EditGUI.EditGUIValueType;
import com.Ben12345rocks.AdvancedCore.Util.EditGUI.ValueTypes.EditGUIValueBoolean;
import com.Ben12345rocks.AdvancedCore.Util.EditGUI.ValueTypes.EditGUIValueNumber;
import com.Ben12345rocks.AdvancedCore.Util.EditGUI.ValueTypes.EditGUIValueString;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.Misc.PlayerUtils;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.InputMethod;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.ValueRequest;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.ValueRequestBuilder;
import com.Ben12345rocks.AdvancedCore.Util.ValueRequest.Listeners.StringListener;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Config.ConfigVoteSites;
import com.Ben12345rocks.VotingPlugin.Events.PlayerVoteEvent;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;

public class AdminGUI {
	static AdminGUI instance = new AdminGUI();

	/**
	 * Gets the single instance of Commands.
	 *
	 * @return single instance of Commands
	 */
	public static AdminGUI getInstance() {
		return instance;
	}

	/** The plugin. */
	Main plugin = Main.plugin;

	/**
	 * Instantiates a new commands.
	 */
	private AdminGUI() {
	}

	/**
	 * Open admin GUI.
	 *
	 * @return ArrayList of buttons
	 */
	public ArrayList<BInventoryButton> adminGUIButtons() {
		ArrayList<BInventoryButton> buttons = new ArrayList<BInventoryButton>();
		buttons.add(new BInventoryButton("&cVoteSites",
				new String[] { "&cOnly enabled sites are listed in this section", "&cMiddle Click to create" },
				new ItemStack(Material.STONE)) {

			@Override
			public void onClick(ClickEvent event) {
				Player player = event.getPlayer();
				if (event.getClick().equals(ClickType.MIDDLE)) {
					new ValueRequest().requestString(player, new StringListener() {

						@Override
						public void onInput(Player player, String value) {
							ConfigVoteSites.getInstance().generateVoteSite(value);
							player.sendMessage("Generated site");
							plugin.reload();
						}
					});
				} else {
					openAdminGUIVoteSites(player);
				}
			}
		});

		buttons.add(new BInventoryButton("&cReload Plugin", new String[] {}, new ItemStack(Material.STONE, 1)) {

			@Override
			public void onClick(ClickEvent event) {
				plugin.reload();
			}

		});
		return buttons;
	}

	public void loadHook() {
		for (BInventoryButton b : adminGUIButtons()) {
			com.Ben12345rocks.AdvancedCore.Commands.GUI.AdminGUI.getInstance().addButton(b);
		}
	}

	/**
	 * Open admin GUI vote sites.
	 *
	 * @param player
	 *            the player
	 */
	public void openAdminGUIVoteSites(Player player) {
		BInventory inv = new BInventory("VoteSites");
		int count = 0;
		for (VoteSite voteSite : plugin.getVoteSites()) {
			ArrayList<String> lore = new ArrayList<String>();
			lore.add("Priority: " + voteSite.getPriority());
			lore.add("Name: " + voteSite.getDisplayName());
			lore.add("ServiceSite: " + voteSite.getServiceSite());
			lore.add("VoteURL: " + voteSite.getVoteURL());
			lore.add("VoteDelay: " + voteSite.getVoteDelay());

			inv.addButton(count, new BInventoryButton(voteSite.getKey(), ArrayUtils.getInstance().convert(lore),
					new ItemStack(Material.STONE)) {

				@Override
				public void onClick(ClickEvent event) {

					Player player = event.getWhoClicked();
					openAdminGUIVoteSiteSite(player, voteSite);

				}
			});
			count++;
		}
		inv.openInventory(player);
	}

	/**
	 * Open admin GUI vote site site.
	 *
	 * @param player
	 *            the player
	 * @param voteSite
	 *            the vote site
	 */
	@SuppressWarnings("deprecation")
	public void openAdminGUIVoteSiteSite(Player player, VoteSite voteSite) {
		EditGUI inv = new EditGUI("VoteSite: " + voteSite.getDisplayName());
		inv.setMeta(player, "VoteSite", voteSite);

		inv.addButton(new BInventoryButton(voteSite.getItem().setName("&cForce vote")) {

			@Override
			public void onClick(ClickEvent event) {
				ArrayList<String> playerNames = new ArrayList<String>();
				for (Player p : Bukkit.getOnlinePlayers()) {
					playerNames.add(p.getName());
				}
				new ValueRequestBuilder(new StringListener() {

					@Override
					public void onInput(Player player, String value) {
						Object ob = PlayerUtils.getInstance().getPlayerMeta(player, "VoteSite");
						if (ob != null) {
							VoteSite site = (VoteSite) ob;
							PlayerVoteEvent voteEvent = new PlayerVoteEvent(site, value, site.getServiceSite(), false);
							plugin.getServer().getPluginManager().callEvent(voteEvent);
						}
					}
				}, ArrayUtils.getInstance().convert(playerNames)).usingMethod(InputMethod.INVENTORY)
						.allowCustomOption(true).request(event.getWhoClicked());
			}
		});

		inv.addButton(new EditGUIButton(new EditGUIValueNumber("Priority", voteSite.getPriority()) {

			@Override
			public void setValue(Player player, Number num) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				ConfigVoteSites.getInstance().setPriority(voteSite.getKey(), num.intValue());
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueString("ServiceSite", voteSite.getServiceSite()) {

			@Override
			public void setValue(Player player, String value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				ConfigVoteSites.getInstance().setServiceSite(siteName, (String) value);
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueString("Name", voteSite.getDisplayName()) {

			@Override
			public void setValue(Player player, String value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				ConfigVoteSites.getInstance().setDisplayName(voteSite.getKey(), (String) value);
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueString("VoteURL", voteSite.getVoteURL()) {

			@Override
			public void setValue(Player player, String value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				ConfigVoteSites.getInstance().setVoteURL(siteName, (String) value);
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new ItemBuilder(Material.DIAMOND), "VoteDelay", voteSite.getVoteDelay(),
				EditGUIValueType.NUMBER) {

			@Override
			public void setValue(Player player, Object value) {
				VoteSite voteSite = (VoteSite) getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				ConfigVoteSites.getInstance().setVoteDelay(siteName, (int) value);
				plugin.reload();
			}
		});

		inv.addButton(new EditGUIButton(new EditGUIValueBoolean("Enabled", voteSite.isEnabled()) {

			@Override
			public void setValue(Player player, boolean value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				ConfigVoteSites.getInstance().setEnabled(siteName, value);
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueBoolean("ForceOffline", voteSite.isGiveOffline()) {

			@Override
			public void setValue(Player player, boolean value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				ConfigVoteSites.getInstance().setForceOffline(siteName, (boolean) value);
				plugin.reload();
			}
		}));

		inv.addButton(new EditGUIButton(new EditGUIValueBoolean("VoteDelayDaily", voteSite.isVoteDelayDaily()) {

			@Override
			public void setValue(Player player, boolean value) {
				VoteSite voteSite = (VoteSite) getInv().getMeta(player, "VoteSite");
				String siteName = voteSite.getKey();
				ConfigVoteSites.getInstance().setVoteDelayDaily(siteName, (boolean) value);
				plugin.reload();
			}
		}));

		inv.openInventory(player);
	}
}
