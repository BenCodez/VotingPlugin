package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.bencodez.advancedcore.api.bookgui.BookWrapper;
import com.bencodez.advancedcore.api.bookgui.Layout;
import com.bencodez.advancedcore.api.gui.GUIHandler;
import com.bencodez.advancedcore.api.gui.GUIMethod;
import com.bencodez.advancedcore.api.inventory.BInventory;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.UpdatingBInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.MessageBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.UserManager;
import com.bencodez.votingplugin.user.VotingPluginUser;

import xyz.upperlevel.spigot.book.BookUtil;

public class VoteURL extends GUIHandler {

	private boolean json;
	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteURL(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, boolean json) {
		super(player);
		this.plugin = plugin;
		this.user = user;
		this.json = json;
		if (user == null && player instanceof Player) {
			this.user = UserManager.getInstance().getVotingPluginUser((Player) player);
		}
		if (player != null && user != null && player.getName().equals(user.getPlayerName())) {
			if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
				user = UserManager.getInstance().getVotingPluginUser(user.getPrimaryAccount());
			}
		}
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		ArrayList<String> sites = new ArrayList<String>();

		List<String> title = plugin.getConfigFile().getFormatCommandsVoteText();
		if (title != null) {
			sites.addAll(title);
		}
		if (plugin.getConfigFile().getFormatCommandsVoteAutoInputSites()) {
			int counter = 0;
			for (VoteSite voteSite : plugin.getVoteSites()) {
				if (!voteSite.isHidden()) {
					counter++;
					String voteURL = voteSite.getVoteURL(json);
					MessageBuilder message = new MessageBuilder(plugin.getConfigFile().getFormatCommandsVoteURLS());
					message.replacePlaceholder("num", Integer.toString(counter)).replacePlaceholder("url", voteURL)
							.replacePlaceholder("SiteName", voteSite.getDisplayName());
					if (user != null && user.getPlayerName() != null) {
						message.replacePlaceholder("player", "" + user.getPlayerName()).replacePlaceholder("Next",
								"" + user.voteCommandNextInfo(voteSite));
					}
					sites.add(message.colorize().getText());
				}
			}
		}
		if (user != null) {
			HashMap<String, String> phs = new HashMap<String, String>();
			phs.put("DailyTotal", "" + user.getTotal(TopVoter.Daily));
			phs.put("WeekTotal", "" + user.getTotal(TopVoter.Weekly));
			phs.put("MonthTotal", "" + user.getTotal(TopVoter.Monthly));
			phs.put("Total", "" + user.getTotal(TopVoter.AllTime));

			sites = ArrayUtils.getInstance().replacePlaceHolder(sites, phs);
		}

		return ArrayUtils.getInstance().colorize(sites);
	}

	private ItemBuilder getItemAll() {
		ItemBuilder builderAll = new ItemBuilder(plugin.getGui().getChestVoteURLAlreadyVotedAllUrlsButtonItemSection());
		if (plugin.getGui().isChestVoteURLAllUrlsButtonrequireAllSitesVoted()) {
			if (user.canVoteAny()) {
				builderAll = new ItemBuilder(plugin.getGui().getChestVoteURLCanVoteAllUrlsButtonItemSection());
			}
		} else {
			if (user.canVoteAll()) {
				builderAll = new ItemBuilder(plugin.getGui().getChestVoteURLCanVoteAllUrlsButtonItemSection());
			}
		}

		if (!builderAll.hasCustomDisplayName()) {
			builderAll.setName("&4All Voting Sites");
		}
		if (!builderAll.hasCustomLore()) {
			builderAll.setLore("&cClick Me");
		}
		int slot = plugin.getGui().getChestVoteURLAllUrlsButtonSlot();
		if (slot >= 0) {
			builderAll.setSlot(slot);
		}
		return builderAll;
	}

	private ItemBuilder getItemVoteSite(VoteSite voteSite) {
		ItemBuilder builder = new ItemBuilder(plugin.getGui().getChestVoteURLAlreadyVotedItemSection());
		if (user.canVoteSite(voteSite)) {
			builder = new ItemBuilder(plugin.getGui().getChestVoteURLCanVoteItemSection());
		} else {
			builder.addLoreLine(
					plugin.getGui().getChestVoteURLNextVote().replace("%Info%", user.voteCommandNextInfo(voteSite)));
		}

		builder.setName(plugin.getGui().getChestVoteURLGUISiteName().replace("%Name%", voteSite.getDisplayName()));
		return builder;
	}

	@Override
	public void onBook(Player player) {
		BookWrapper book = new BookWrapper(plugin.getGui().getBookVoteURLBookGUITitle());

		for (VoteSite site : plugin.getVoteSites()) {
			Layout layout = new Layout(plugin.getGui().getBookVoteURLBookGUILayout()).addPlaceholder("sitename",
					site.getDisplayName());
			String text = plugin.getGui().getBookVoteURLBookGUIAlreadyVotedText();
			ChatColor color = ChatColor.valueOf(plugin.getGui().getBookVoteURLBookGUIAlreadyVotedColor());
			if (user.canVoteSite(site)) {
				color = ChatColor.valueOf(plugin.getGui().getBookVoteURLBookGUICanVoteColor());
				text = plugin.getGui().getBookVoteURLBookGUICanVoteText();
			}
			String url = StringParser.getInstance().replacePlaceHolder(site.getVoteURLJsonStrip(), "player",
					user.getPlayerName());
			layout.replaceTextComponent("[UrlText]", BookUtil.TextBuilder.of(text).color(color)
					.onClick(BookUtil.ClickAction.openUrl(url)).onHover(BookUtil.HoverAction.showText(url)).build());
			book.addLayout(layout);

		}

		book.open(player);
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {
		// normal GUI
		BInventory inv = new BInventory(plugin.getGui().getChestVoteURLName());
		if (plugin.getGui().getChestVoteURLViewAllUrlsButtonEnabled()) {
			ItemBuilder builderAll = getItemAll();

			inv.addButton(new UpdatingBInventoryButton(builderAll, 1000, 1000) {

				@Override
				public void onClick(ClickEvent event) {
					VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(event.getPlayer());
					json = true;
					user.sendMessage(getChat(player));
				}

				@Override
				public ItemBuilder onUpdate(Player player) {
					return getItemAll();
				}
			});
		}

		int startSlot = plugin.getGui().getChestVoteURLAllUrlsButtonStartSlot();
		for (final VoteSite voteSite : plugin.getVoteSites()) {
			if (!voteSite.isHidden()) {
				ItemBuilder builder = getItemVoteSite(voteSite);
				if (startSlot >= 0) {
					builder.setSlot(startSlot);
					startSlot++;
				}

				inv.addButton(new UpdatingBInventoryButton(builder, 1000, 1000) {

					@Override
					public void onClick(ClickEvent event) {
						Player player = event.getPlayer();
						if (player != null) {
							VotingPluginUser user = UserManager.getInstance().getVotingPluginUser(player);
							user.sendMessage(StringParser.getInstance().replacePlaceHolder(StringParser.getInstance()
									.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
											plugin.getGui().getChestVoteURLURLText(), "voteurl", voteSite.getVoteURL()),
											"sitename", voteSite.getDisplayName()),
									"player", player.getName()));

						}

					}

					@Override
					public ItemBuilder onUpdate(Player player) {
						return getItemVoteSite(voteSite);
					}

				});
			}
		}

		for (final String str : plugin.getGui().getChestVoteURLExtraItems()) {
			inv.addButton(new BInventoryButton(new ItemBuilder(plugin.getGui().getChestVoteURLExtraItemsItem(str))) {

				@Override
				public void onClick(ClickEvent clickEvent) {
					new RewardBuilder(plugin.getGui().getData(), "CHEST.VoteURL.ExtraItems." + str + ".Rewards")
							.setGiveOffline(false).send(clickEvent.getPlayer());
					;
				}
			});
		}

		if (plugin.getGui().getChestVoteURLBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}

		inv.openInventory(player);

	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodURL().toUpperCase()));
	}

}
