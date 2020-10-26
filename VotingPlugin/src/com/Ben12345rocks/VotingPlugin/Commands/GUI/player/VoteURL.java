package com.Ben12345rocks.VotingPlugin.Commands.GUI.player;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.UpdatingBInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Messages.MessageBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Messages.StringParser;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
import com.Ben12345rocks.AdvancedCore.Util.bookgui.BookWrapper;
import com.Ben12345rocks.AdvancedCore.Util.bookgui.Layout;
import com.Ben12345rocks.AdvancedCore.gui.GUIHandler;
import com.Ben12345rocks.AdvancedCore.gui.GUIMethod;
import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Commands.CommandLoader;
import com.Ben12345rocks.VotingPlugin.Config.Config;
import com.Ben12345rocks.VotingPlugin.Config.GUI;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Objects.VoteSite;
import com.Ben12345rocks.VotingPlugin.TopVoter.TopVoter;
import com.Ben12345rocks.VotingPlugin.UserManager.UserManager;

import xyz.upperlevel.spigot.book.BookUtil;

public class VoteURL extends GUIHandler {

	private User user;
	private Main plugin;
	private boolean json;

	public VoteURL(Main plugin, CommandSender player, User user, boolean json) {
		super(player);
		this.plugin = plugin;
		this.user = user;
		this.json = json;
		if (user == null && player instanceof Player) {
			this.user = UserManager.getInstance().getVotingPluginUser((Player) player);
		}
	}

	@Override
	public void onBook(Player player) {
		BookWrapper book = new BookWrapper(GUI.getInstance().getBookVoteURLBookGUITitle());
		
		for (VoteSite site : plugin.getVoteSites()) {
			Layout layout = new Layout(GUI.getInstance().getBookVoteURLBookGUILayout()).addPlaceholder("sitename", site.getDisplayName());
			String text = GUI.getInstance().getBookVoteURLBookGUIAlreadyVotedText();
			ChatColor color = ChatColor.valueOf(GUI.getInstance().getBookVoteURLBookGUIAlreadyVotedColor());
			if (user.canVoteSite(site)) {
				color = ChatColor.valueOf(GUI.getInstance().getBookVoteURLBookGUICanVoteColor());
				text = GUI.getInstance().getBookVoteURLBookGUICanVoteText();
			}
			String url = ChatColor.stripColor(StringParser.getInstance()
					.colorize(StringParser.getInstance().parseJson(site.getVoteURL(false)).toPlainText()));
			if (!url.startsWith("http")) {
				if (!url.startsWith("www.")) {
					url = "https://www." + url;
				} else {
					url = "https://" + url;
				}
			}
			layout.replaceTextComponent("[UrlText]", BookUtil.TextBuilder.of(text).color(color).onClick(BookUtil.ClickAction.openUrl(url))
					.onHover(BookUtil.HoverAction.showText(url)).build());
			book.addLayout(layout);
		
		}
		
		book.open(player);
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	private ItemBuilder getItemAll() {
		ItemBuilder builderAll = new ItemBuilder(
				GUI.getInstance().getChestVoteURLAlreadyVotedAllUrlsButtonItemSection());
		if (GUI.getInstance().isChestVoteURLAllUrlsButtonrequireAllSitesVoted()) {
			if (user.canVoteAny()) {
				builderAll = new ItemBuilder(GUI.getInstance().getChestVoteURLCanVoteAllUrlsButtonItemSection());
			}
		} else {
			if (user.canVoteAll()) {
				builderAll = new ItemBuilder(GUI.getInstance().getChestVoteURLCanVoteAllUrlsButtonItemSection());
			}
		}

		if (!builderAll.hasCustomDisplayName()) {
			builderAll.setName("&4All Voting Sites");
		}
		if (!builderAll.hasCustomLore()) {
			builderAll.setLore("&cClick Me");
		}
		return builderAll;
	}

	@Override
	public void onChest(Player player) {
		// normal GUI
		BInventory inv = new BInventory(GUI.getInstance().getChestVoteURLName());

		int count = 0;
		if (GUI.getInstance().getChestVoteURLViewAllUrlsButtonEnabled()) {
			ItemBuilder builderAll = getItemAll();

			inv.addButton(count, new UpdatingBInventoryButton(builderAll, 1000, 1000) {

				@Override
				public void onClick(ClickEvent event) {
					User user = UserManager.getInstance().getVotingPluginUser(event.getPlayer());
					json = true;
					user.sendMessage(getChat(player));
				}

				@Override
				public ItemBuilder onUpdate(Player player) {
					return getItemAll();
				}
			});

			count++;
		}

		for (final VoteSite voteSite : plugin.getVoteSites()) {
			ItemBuilder builder = getItemVoteSite(voteSite);

			inv.addButton(count, new UpdatingBInventoryButton(builder, 1000, 1000) {

				@Override
				public void onClick(ClickEvent event) {
					Player player = event.getPlayer();
					if (player != null) {
						User user = UserManager.getInstance().getVotingPluginUser(player);
						user.sendMessage(StringParser.getInstance().replacePlaceHolder(
								StringParser.getInstance()
										.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
												GUI.getInstance().getChestVoteURLURLText(), "voteurl",
												voteSite.getVoteURL()), "sitename", voteSite.getDisplayName()),
								"player", player.getName()));

					}

				}

				@Override
				public ItemBuilder onUpdate(Player player) {
					return getItemVoteSite(voteSite);
				}

			});
			count++;
		}

		if (GUI.getInstance().getChestVoteURLBackButton()) {
			inv.addButton(CommandLoader.getInstance().getBackButton(user));
		}

		inv.openInventory(player);

	}

	private ItemBuilder getItemVoteSite(VoteSite voteSite) {
		ItemBuilder builder = new ItemBuilder(GUI.getInstance().getChestVoteURLAlreadyVotedItemSection());
		if (user.canVoteSite(voteSite)) {
			builder = new ItemBuilder(GUI.getInstance().getChestVoteURLCanVoteItemSection());
		} else {
			builder.addLoreLine(
					GUI.getInstance().getChestVoteURLNextVote().replace("%Info%", user.voteCommandNextInfo(voteSite)));
		}

		builder.setName(GUI.getInstance().getChestVoteURLGUISiteName().replace("%Name%", voteSite.getDisplayName()));
		return builder;
	}

	@Override
	public ArrayList<String> getChat(CommandSender arg0) {
		ArrayList<String> sites = new ArrayList<String>();

		List<String> title = Config.getInstance().getFormatCommandsVoteText();
		if (title != null) {
			sites.addAll(title);
		}
		if (Config.getInstance().getFormatCommandsVoteAutoInputSites()) {
			int counter = 0;
			for (VoteSite voteSite : plugin.getVoteSites()) {
				counter++;
				String voteURL = voteSite.getVoteURL(json);
				MessageBuilder message = new MessageBuilder(Config.getInstance().getFormatCommandsVoteURLS());
				message.replacePlaceholder("num", Integer.toString(counter)).replacePlaceholder("url", voteURL)
						.replacePlaceholder("SiteName", voteSite.getDisplayName());
				if (user != null && user.getPlayerName() != null) {
					message.replacePlaceholder("player", "" + user.getPlayerName()).replacePlaceholder("Next",
							"" + user.voteCommandNextInfo(voteSite));
				}
				sites.add(message.colorize().getText());
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

	@Override
	public void open() {
		open(GUIMethod.valueOf(GUI.getInstance().getGuiMethodURL().toUpperCase()));
	}

}
