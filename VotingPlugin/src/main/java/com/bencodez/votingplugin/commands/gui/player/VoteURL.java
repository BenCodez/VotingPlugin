package com.bencodez.votingplugin.commands.gui.player;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.event.inventory.ClickType;

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
import com.bencodez.advancedcore.api.messages.PlaceholderUtils;
import com.bencodez.advancedcore.api.rewards.RewardBuilder;
import com.bencodez.simpleapi.array.ArrayUtils;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.topvoter.TopVoter;
import com.bencodez.votingplugin.user.VotingPluginUser;
import com.bencodez.votingplugin.votesites.VoteSite;

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
	}

	@Override
	public ArrayList<String> getChat(CommandSender sender) {
		return getChat(sender, false);
	}

	public ArrayList<String> getChat(CommandSender sender, boolean bypassCanVote) {
		ArrayList<String> sites = new ArrayList<>();

		List<String> title = plugin.getConfigFile().getFormatCommandsVoteText();
		if (title != null) {
			sites.addAll(title);
		}
		if (plugin.getConfigFile().isFormatCommandsVoteAutoInputSites()) {
			int counter = 0;
			for (VoteSite voteSite : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
				if (!voteSite.isHidden()) {
					if (voteSite.getPermissionToView().isEmpty()
							|| sender.hasPermission(voteSite.getPermissionToView())) {
						if (!plugin.getConfigFile().isFormatCommandsVoteOnlyShowSitesToVote()
								|| user.canVoteSite(voteSite) || bypassCanVote) {
							counter++;
							String voteURL = voteSite.getVoteURL(json);
							MessageBuilder message = new MessageBuilder(
									plugin.getConfigFile().getFormatCommandsVoteURLS());
							message.replacePlaceholder("num", Integer.toString(counter))
									.replacePlaceholder("url", voteURL)
									.replacePlaceholder("SiteName", voteSite.getDisplayName());
							if (user != null && user.getPlayerName() != null) {
								message.replacePlaceholder("player", "" + user.getPlayerName())
										.replacePlaceholder("Next", "" + user.voteCommandNextInfo(voteSite));
							}

							sites.add(message.colorize().getText());
						}
					}
				}
			}
		}
		if (user != null) {
			HashMap<String, String> phs = new HashMap<>();
			phs.put("DailyTotal", "" + user.getTotal(TopVoter.Daily));
			phs.put("WeekTotal", "" + user.getTotal(TopVoter.Weekly));
			phs.put("MonthTotal", "" + user.getTotal(TopVoter.Monthly));
			phs.put("Total", "" + user.getTotal(TopVoter.AllTime));

			sites = PlaceholderUtils.replacePlaceHolder(sites, phs);
		}

		return ArrayUtils.colorize(sites);
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
		for (VoteSite site : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (!site.isHidden()) {
				if (site.getPermissionToView().isEmpty() || player.hasPermission(site.getPermissionToView())) {
					Layout layout = new Layout(plugin.getGui().getBookVoteURLBookGUILayout())
							.addPlaceholder("sitename", site.getDisplayName()).addPlaceholder("num", "" + i);
					String text = plugin.getGui().getBookVoteURLBookGUIAlreadyVotedText();
					ChatColor color = ChatColor.valueOf(plugin.getGui().getBookVoteURLBookGUIAlreadyVotedColor());
					if (user.canVoteSite(site)) {
						color = ChatColor.valueOf(plugin.getGui().getBookVoteURLBookGUICanVoteColor());
						text = plugin.getGui().getBookVoteURLBookGUICanVoteText();
					}
					String url = PlaceholderUtils.replacePlaceHolder(PlaceholderUtils.replacePlaceHolder(
							site.getVoteURLJsonStrip(), "player", user.getPlayerName()), "num", "" + i);
					layout.replaceTextComponent("[UrlText]",
							BookUtil.TextBuilder.of(text).color(color).onClick(BookUtil.ClickAction.openUrl(url))
									.onHover(BookUtil.HoverAction.showText(url)).build());
					book.addLayout(layout);
					i++;
				}
			}
		}

		book.open(player);
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {

		BInventory inv = new BInventory(plugin.getGui().getChestVoteURLName());

		if (!plugin.getConfigFile().isAlwaysCloseInventory()) {
			inv.dontClose();
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

		if (plugin.getGui().isChestVoteURLViewAllUrlsButtonEnabled()) {
			ItemBuilder builderAll = getItemAll();

			inv.addButton(new UpdatingBInventoryButton(plugin, builderAll, 5000, 5000) {

				@Override
				public void onClick(ClickEvent event) {
					event.closeInventory();
					VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(event.getPlayer());
					json = true;
					if (event.getClick().equals(ClickType.SHIFT_LEFT)
							|| event.getClick().equals(ClickType.SHIFT_RIGHT)) {
						user.sendMessage(getChat(player, true));
					} else {
						user.sendMessage(getChat(player, false));
					}
				}

				@Override
				public ItemBuilder onUpdate(Player player) {
					return getItemAll();
				}
			});
		}

		int startSlot = plugin.getGui().getChestVoteURLAllUrlsButtonStartSlot();
		for (final VoteSite voteSite : plugin.getVoteSiteManager().getVoteSitesEnabled()) {
			if (!voteSite.isHidden()) {
				if (voteSite.getPermissionToView().isEmpty() || player.hasPermission(voteSite.getPermissionToView())) {
					ItemBuilder builder = getItemVoteSite(voteSite);
					if (startSlot >= 0) {
						if (inv.isSlotTaken(startSlot)) {
							boolean found = false;
							while (!found) {
								startSlot++;
								if (!inv.isSlotTaken(startSlot)) {
									found = true;
								}
							}
						}
						builder.setSlot(startSlot);
						startSlot++;
					}

					inv.addButton(new UpdatingBInventoryButton(plugin, builder, 5000, 5000) {

						@Override
						public void onClick(ClickEvent event) {
							event.closeInventory();
							Player player = event.getPlayer();
							if (player != null) {
								VotingPluginUser user = plugin.getVotingPluginUserManager().getVotingPluginUser(player);
								HashMap<String, String> placeholders = new HashMap<>();
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
