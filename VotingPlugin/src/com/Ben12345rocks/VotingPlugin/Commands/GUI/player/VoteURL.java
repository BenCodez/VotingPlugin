package com.Ben12345rocks.VotingPlugin.Commands.GUI.player;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventory.ClickEvent;
import com.Ben12345rocks.AdvancedCore.Util.Inventory.BInventoryButton;
import com.Ben12345rocks.AdvancedCore.Util.Item.ItemBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Messages.MessageBuilder;
import com.Ben12345rocks.AdvancedCore.Util.Messages.StringParser;
import com.Ben12345rocks.AdvancedCore.Util.Misc.ArrayUtils;
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

import net.md_5.bungee.api.chat.BaseComponent;
import net.md_5.bungee.api.chat.TextComponent;
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
	}

	@Override
	public void onBook(Player player) {
		ArrayList<BaseComponent[]> builder = new ArrayList<BaseComponent[]>();
		BookUtil.PageBuilder currentPage = new BookUtil.PageBuilder();
		int lines = 0;
		for (final VoteSite site : plugin.getVoteSites()) {

			try {
				ArrayList<String> layout = Config.getInstance().getGUIVoteURLBookGUILayout();

				lines += layout.size();
				if (lines > 14) {
					builder.add(currentPage.build());
					currentPage = new BookUtil.PageBuilder();
					lines = layout.size();
					plugin.debug("New page");
				}

				HashMap<String, String> placeholders = new HashMap<String, String>();
				placeholders.put("Sitename", site.getDisplayName());
				ChatColor color = ChatColor.valueOf(Config.getInstance().getGUIVoteURLBookGUIAlreadyVotedColor());
				String text = Config.getInstance().getGUIVoteURLBookGUIAlreadyVotedText();
				if (user.canVoteSite(site)) {
					color = ChatColor.valueOf(Config.getInstance().getGUIVoteURLBookGUICanVoteColor());
					text = Config.getInstance().getGUIVoteURLBookGUICanVoteText();
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

				for (String str : layout) {
					str = StringParser.getInstance().replacePlaceHolder(str, placeholders);

					if (StringParser.getInstance().containsIgnorecase(str, "[UrlText]")) {
						String[] split = str.split("[UrlText]");
						String s = split[0].substring(0, split[0].length() - 1);
						BaseComponent comp = new TextComponent(StringParser.getInstance().colorize(s));
						comp.addExtra(
								BookUtil.TextBuilder.of(text).color(color).onClick(BookUtil.ClickAction.openUrl(url))
										.onHover(BookUtil.HoverAction.showText(url)).build());
						comp.addExtra(StringParser.getInstance().colorize(str.substring(s.length() + 9)));
						currentPage.add(comp);
					} else {
						currentPage.add(StringParser.getInstance().colorize(str));
					}
					currentPage.newLine();
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		builder.add(currentPage.build());

		ItemStack book = BookUtil.writtenBook().author(player.getName())
				.title(StringParser.getInstance().colorize(Config.getInstance().getGUIVoteURLBookGUITitle()))
				.pages(builder).build();

		Bukkit.getScheduler().runTask(plugin, new Runnable() {

			@Override
			public void run() {
				BookUtil.openPlayer(player, book);
			}
		});
	}

	@Override
	public void onChat(CommandSender sender) {
		sendMessage(getChat(sender));
	}

	@Override
	public void onChest(Player player) {
		User user = UserManager.getInstance().getVotingPluginUser(player);

		// normal GUI
		BInventory inv = new BInventory(Config.getInstance().getGUIVoteURLName());

		int count = 0;
		if (Config.getInstance().getVoteURLViewAllUrlsButtonEnabled()) {
			ItemBuilder builderAll = new ItemBuilder(
					Config.getInstance().getVoteURLAlreadyVotedAllUrlsButtonItemSection());
			if (Config.getInstance().isGuiVoteURLAllUrlsButtonrequireAllSitesVoted()) {
				if (user.canVoteAny()) {
					builderAll = new ItemBuilder(Config.getInstance().getVoteURLCanVoteAllUrlsButtonItemSection());
				}
			} else {
				if (user.canVoteAll()) {
					builderAll = new ItemBuilder(Config.getInstance().getVoteURLCanVoteAllUrlsButtonItemSection());
				}
			}

			if (!builderAll.hasCustomDisplayName()) {
				builderAll.setName("&4All Voting Sites");
			}
			if (!builderAll.hasCustomLore()) {
				builderAll.setLore("&cClick Me");
			}

			inv.addButton(count, new BInventoryButton(builderAll) {

				@Override
				public void onClick(ClickEvent event) {
					User user = UserManager.getInstance().getVotingPluginUser(event.getPlayer());
					json = true;
					user.sendMessage(getChat(player));
				}
			});

			count++;
		}

		for (final VoteSite voteSite : plugin.getVoteSites()) {
			ItemBuilder builder = new ItemBuilder(Config.getInstance().getVoteURLAlreadyVotedItemSection());
			if (user.canVoteSite(voteSite)) {
				builder = new ItemBuilder(Config.getInstance().getVoteURLCanVoteItemSection());
			} else {
				builder.addLoreLine(Config.getInstance().getVoteURLNextVote().replace("%Info%",
						user.voteCommandNextInfo(voteSite)));
			}

			builder.setName(Config.getInstance().getVoteURLSiteName().replace("%Name%", voteSite.getDisplayName()));

			inv.addButton(count, new BInventoryButton(builder) {

				@Override
				public void onClick(ClickEvent event) {
					Player player = event.getPlayer();
					if (player != null) {
						User user = UserManager.getInstance().getVotingPluginUser(player);
						user.sendMessage(StringParser.getInstance().replacePlaceHolder(
								StringParser.getInstance()
										.replacePlaceHolder(StringParser.getInstance().replacePlaceHolder(
												Config.getInstance().getGUIVoteURLURLText(), "voteurl",
												voteSite.getVoteURL()), "sitename", voteSite.getDisplayName()),
								"player", player.getName()));

					}

				}
			});
			count++;
		}

		if (Config.getInstance().getGUIVoteURLBackButton()) {
			inv.addButton(CommandLoader.getInstance().getBackButton(user));
		}

		inv.openInventory(player);

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
