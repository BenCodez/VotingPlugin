package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.Arrays;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.bookgui.BookWrapper;
import com.bencodez.advancedcore.api.bookgui.Layout;
import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.inventory.UpdatingBInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.simpleapi.messages.MessageAPI;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.user.VotingPluginUser;

import xyz.upperlevel.spigot.book.BookUtil;

public class VoteNext extends GUIHandler {

	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteNext(VotingPluginMain plugin, CommandSender player, VotingPluginUser user) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		ArrayList<String> msg = new ArrayList<>();

		String playerName = user.getPlayerName();

		msg.add(MessageAPI.colorize(MessageAPI
				.replaceIgnoreCase(plugin.getConfigFile().getFormatCommandsVoteNextTitle(), "%player%", playerName)));

		for (VoteSite voteSite : plugin.getVoteSitesEnabled()) {
			if (!voteSite.isHidden()) {
				String msgLine = plugin.getConfigFile().getFormatCommandsVoteNextLayout();

				msgLine = MessageAPI.replaceIgnoreCase(msgLine, "%info%", user.voteCommandNextInfo(voteSite));

				msgLine = MessageAPI.replaceIgnoreCase(msgLine, "%SiteName%", voteSite.getDisplayName());
				msg.add(MessageAPI.colorize(msgLine));
			}
		}
		return msg;
	}

	@Override
	public void onBook(Player player) {
		BookWrapper book = new BookWrapper(plugin.getGui().getBookVoteURLBookGUITitle());

		// add colors/config options
		for (VoteSite site : plugin.getVoteSitesEnabled()) {
			if (!site.isHidden()) {
				Layout nextLayout = new Layout(new ArrayList<>(Arrays.asList("[Json]")));
				nextLayout.replaceTextComponent("[Json]",
						BookUtil.TextBuilder.of(book.colorize(site.getDisplayName()))
								.onClick(BookUtil.ClickAction.openUrl(site.getVoteURLJsonStrip()))
								.onHover(BookUtil.HoverAction.showText(user.voteCommandNextInfo(site))).build());
				book.addLayout(nextLayout);
			}
		}
		book.addLine();

		book.open(player);
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {
		BInventory inv = new BInventory(plugin.getGui().getChestVoteNextName());
		inv.addPlaceholder("player", user.getPlayerName());
		for (VoteSite site : plugin.getVoteSitesEnabled()) {
			if (!site.isHidden()) {
				String siteNameDisplay = plugin.getGui().getChestVoteNextCustomSiteNamesDisplays(site.getKey());
				if (siteNameDisplay.isEmpty()) {
					siteNameDisplay = site.getDisplayName();
				}
				inv.addButton(inv.getNextSlot(),
						new UpdatingBInventoryButton(plugin,
								site.getItem().setName(siteNameDisplay)
										.setLore(PlaceholderUtils.replacePlaceHolder(
												plugin.getGui().getChestVoteNextLine(), "time",
												user.voteCommandNextInfo(site)))
										.setAmountNone(1),
								1000, 1000) {

							@Override
							public void onClick(ClickEvent clickEvent) {

							}

							@Override
							public ItemBuilder onUpdate(Player player) {
								return site.getItem().setName(site.getDisplayName())
										.setLore(user.voteCommandNextInfo(site)).setAmountNone(1)
										.addPlaceholder("player", user.getPlayerName());
							}
						});
			}
		}

		String guiPath = "VoteNext";
		for (final String str : plugin.getGui().getChestGUIExtraItems(guiPath)) {
			inv.addButton(
					new BInventoryButton(new ItemBuilder(plugin.getGui().getChestGUIExtraItemsItem(guiPath, str))) {

						@Override
						public void onClick(ClickEvent clickEvent) {
							new RewardBuilder(plugin.getGui().getData(),
									"CHEST." + guiPath + ".ExtraItems." + str + "."
											+ clickEvent.getButton().getLastRewardsPath(player))
									.setGiveOffline(false).send(clickEvent.getPlayer());

						}
					});
		}

		if (plugin.getGui().isChestVoteNextBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}
		inv.openInventory(player);
	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodNext().toUpperCase()));
	}

}
