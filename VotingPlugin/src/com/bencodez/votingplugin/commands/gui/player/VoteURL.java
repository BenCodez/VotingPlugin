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
import com.bencodez.advancedcore.api.inventory.BInventory.ClickEvent;
import com.bencodez.advancedcore.api.inventory.BInventoryButton;
import com.bencodez.advancedcore.api.inventory.UpdatingBInventoryButton;
import com.bencodez.advancedcore.api.item.ItemBuilder;
import com.bencodez.advancedcore.api.messages.MessageBuilder;
import com.bencodez.advancedcore.api.messages.StringParser;
import com.bencodez.advancedcore.api.misc.ArrayUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.objects.VoteSite;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;

import xyz.upperlevel.spigot.book.BookUtil;

public class VoteURL extends GUIHandler {

	private boolean json;
	private VotingPluginMain plugin;
	private VotingPluginUser user;

	public VoteURL(VotingPluginMain plugin, CommandSender player, VotingPluginUser user, boolean json) {
		super(plugin, player);
		this.plugin = plugin;
		this.user = user;
		this.json = json;
		if (user == null && player instanceof Player) {
			this.user = plugin.getVotingPluginUserManager().getVotingPluginUser((Player) player);
		}
		if (player != null && user != null && player.getName().equals(user.getPlayerName())) {
			if (plugin.getConfigFile().isUsePrimaryAccountForPlaceholders() && user.hasPrimaryAccount()) {
				user = plugin.getVotingPluginUserManager().getVotingPluginUser(user.getPrimaryAccount());
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
		if (plugin.getConfigFile().isFormatCommandsVoteAutoInputSites()) {
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
			builder.setName(plugin.getGui().getChestVoteURLGUISiteNameCanVote());
		} else {
			builder.setName(plugin.getGui().getChestVoteURLGUISiteName());
			builder.addLoreLine(plugin.getGui().getChestVoteURLNextVote());
		}
		builder.addPlaceholder("ServiceSite", voteSite.getServiceSite());
		builder.addPlaceholder("Name", voteSite.getDisplayName());
		builder.addPlaceholder("VoteDelay", "" + voteSite.getVoteDelay());
		builder.addPlaceholder("VoteHour", "" + voteSite.getVoteDelayDailyHour());
		builder.addPlaceholder("Info", user.voteCommandNextInfo(voteSite));

		return builder;
	}

	@Override
	public void onBook(Player player) {
		BookWrapper book = new BookWrapper(plugin.getGui().getBookVoteURLBookGUITitle());

		if (plugin.getGui().isBookVoteURLBookGUIManual()) {
			Layout layout = new Layout(plugin.getGui().getBookVoteURLBookGUITopLayout());
			book.addLayout(layout);
		}
		int i = 1;
		for (VoteSite site : plugin.getVoteSites()) {
			Layout layout = new Layout(plugin.getGui().getBookVoteURLBookGUILayout()).addPlaceholder("sitename",
					site.getDisplayName());
			String text = plugin.getGui().getBookVoteURLBookGUIAlreadyVotedText();
			ChatColor color = ChatColor.valueOf(plugin.getGui().getBookVoteURLBookGUIAlreadyVotedColor());
			if (user.canVoteSite(site)) {
				color = ChatColor.valueOf(plugin.getGui().getBookVoteURLBookGUICanVoteColor());
				text = plugin.getGui().getBookVoteURLBookGUICanVoteText();
			}
			String url = StringParser.getInstance().replacePlaceHolder(StringParser.getInstance()
					.replacePlaceHolder(site.getVoteURLJsonStrip(), "player", user.getPlayerName()), "num", "" + i);
			layout.replaceTextComponent("[UrlText]", BookUtil.TextBuilder.of(text).color(color)
					.onClick(BookUtil.ClickAction.openUrl(url)).onHover(BookUtil.HoverAction.showText(url)).build());
			book.addLayout(layout);
			i++;
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
		if (plugin.getGui().isChestVoteURLViewAllUrlsButtonEnabled()) {
			ItemBuilder builderAll = getItemAll();

			inv.addButton(new UpdatingBInventoryButton(plugin, builderAll, 5000, 5000) {

				@Override
				public void onClick(ClickEvent event) {
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(event.getPlayer());
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

				inv.addButton(new UpdatingBInventoryButton(plugin, builder, 5000, 5000) {

					@Override
					public void onClick(ClickEvent event) {
						Player player = event.getPlayer();
						if (player != null) {
							VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
							HashMap<String, String> placeholders = new HashMap<String, String>();
							placeholders.put("voteurl", voteSite.getVoteURL());
							placeholders.put("sitename", voteSite.getDisplayName());
							placeholders.put("player", player.getName());
							placeholders.put("servicesite", voteSite.getServiceSite());
							placeholders.put("VoteDelay", "" + voteSite.getVoteDelay());
							placeholders.put("VoteHour", "" + voteSite.getVoteDelayDailyHour());
							user.sendMessage(plugin.getGui().getChestVoteURLURLText(), placeholders);

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
					new RewardBuilder(plugin.getGui().getData(),
							"CHEST.VoteURL.ExtraItems." + str + "." + clickEvent.getButton().getLastRewardsPath(player))
							.setGiveOffline(false).send(clickEvent.getPlayer());

				}
			});
		}

		if (plugin.getGui().isChestVoteURLBackButton()) {
			inv.addButton(plugin.getCommandLoader().getBackButton(user));
		}

		inv.openInventory(player);

	}

	@Override
	public void open() {
		open(GUIMethod.valueOf(plugin.getGui().getGuiMethodURL().toUpperCase()));
	}

}
