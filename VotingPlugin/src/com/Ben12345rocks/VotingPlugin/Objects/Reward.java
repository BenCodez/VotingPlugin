package com.Ben12345rocks.VotingPlugin.Objects;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.Set;

import org.apache.commons.lang.time.DateUtils;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.ConfigFormat;
import com.Ben12345rocks.VotingPlugin.Config.ConfigRewards;
import com.Ben12345rocks.VotingPlugin.Data.Data;

// TODO: Auto-generated Javadoc
/**
 * The Class Reward.
 */
public class Reward {
	
	/** The plugin. */
	static Main plugin = Main.plugin;

	/** The name. */
	public String name;

	/** The delay enabled. */
	private boolean delayEnabled;
	
	/** The delay hours. */
	private int delayHours;
	
	/** The delay minutes. */
	private int delayMinutes;

	/** The timed enabled. */
	private boolean timedEnabled;
	
	/** The timed hour. */
	private int timedHour;
	
	/** The timed minute. */
	private int timedMinute;

	/** The chance. */
	private double chance;

	/** The random chance. */
	private double randomChance;

	/** The random rewards. */
	private ArrayList<String> randomRewards;

	/** The random fall back. */
	private ArrayList<String> randomFallBack;

	/** The require permission. */
	private boolean requirePermission;

	/** The worlds. */
	private ArrayList<String> worlds;

	/** The give in each world. */
	private boolean giveInEachWorld;

	/** The items. */
	private Set<String> items;

	/** The item material. */
	private HashMap<String, String> itemMaterial;
	
	/** The item skull. */
	private HashMap<String, String> itemSkull;

	/** The item data. */
	private HashMap<String, Integer> itemData;

	/** The item durabilty. */
	private HashMap<String, Integer> itemDurabilty;

	/** The item amount. */
	private HashMap<String, Integer> itemAmount;

	/** The item min amount. */
	private HashMap<String, Integer> itemMinAmount;
	
	/** The item max amount. */
	private HashMap<String, Integer> itemMaxAmount;
	
	/** The item name. */
	private HashMap<String, String> itemName;
	
	/** The item lore. */
	private HashMap<String, ArrayList<String>> itemLore;

	/** The item enchants. */
	private HashMap<String, HashMap<String, Integer>> itemEnchants;

	/** The money. */
	private int money;

	/** The Min money. */
	private int MinMoney;

	/** The Max money. */
	private int MaxMoney;

	/** The exp. */
	private int exp;

	/** The console commands. */
	private ArrayList<String> consoleCommands;
	
	/** The player commands. */
	private ArrayList<String> playerCommands;

	/** The potions. */
	private Set<String> potions;
	
	/** The potions duration. */
	private HashMap<String, Integer> potionsDuration;
	
	/** The potions amplifier. */
	private HashMap<String, Integer> potionsAmplifier;

	/** The reward msg. */
	private String rewardMsg;

	/**
	 * Instantiates a new reward.
	 *
	 * @param plugin the plugin
	 */
	public Reward(Main plugin) {
		Reward.plugin = plugin;
	}

	/**
	 * Instantiates a new reward.
	 *
	 * @param reward the reward
	 */
	public Reward(String reward) {
		name = reward;

		setDelayEnabled(ConfigRewards.getInstance().getDelayedEnabled(reward));
		setDelayHours(ConfigRewards.getInstance().getDelayedHours(reward));
		setDelayMinutes(ConfigRewards.getInstance().getDelayedMinutes(reward));

		setTimedEnabled(ConfigRewards.getInstance().getTimedEnabled(reward));
		setTimedHour(ConfigRewards.getInstance().getTimedHour(reward));
		setTimedMinute(ConfigRewards.getInstance().getTimedMinute(reward));

		setChance(ConfigRewards.getInstance().getChance(reward));
		setRandomChance(ConfigRewards.getInstance().getRandomChance(reward));
		setRandomRewards(ConfigRewards.getInstance().getRandomRewards(reward));
		setRandomFallBack(ConfigRewards.getInstance().getRandomFallBack(reward));

		setRequirePermission(ConfigRewards.getInstance().getRequirePermission(
				reward));
		setWorlds(ConfigRewards.getInstance().getWorlds(reward));
		setGiveInEachWorld(ConfigRewards.getInstance().getGiveInEachWorld(
				reward));

		setItems(ConfigRewards.getInstance().getItems(reward));
		itemMaterial = new HashMap<String, String>();
		itemData = new HashMap<String, Integer>();
		itemSkull = new HashMap<String, String>();
		itemDurabilty = new HashMap<String, Integer>();
		itemAmount = new HashMap<String, Integer>();
		itemMinAmount = new HashMap<String, Integer>();
		itemMaxAmount = new HashMap<String, Integer>();
		itemName = new HashMap<String, String>();
		itemLore = new HashMap<String, ArrayList<String>>();
		itemName = new HashMap<String, String>();
		itemEnchants = new HashMap<String, HashMap<String, Integer>>();
		for (String item : ConfigRewards.getInstance().getItems(reward)) {
			itemMaterial.put(item,
					ConfigRewards.getInstance().getItemMaterial(reward, item));
			itemData.put(item,
					ConfigRewards.getInstance().getItemData(reward, item));
			itemAmount.put(item,
					ConfigRewards.getInstance().getItemAmount(reward, item));
			itemMinAmount.put(item, ConfigRewards.getInstance()
					.getItemMinAmount(reward, item));
			itemMaxAmount.put(item, ConfigRewards.getInstance()
					.getItemMaxAmount(reward, item));
			itemName.put(item,
					ConfigRewards.getInstance().getItemName(reward, item));
			itemLore.put(item,
					ConfigRewards.getInstance().getItemLore(reward, item));
			itemDurabilty.put(item, ConfigRewards.getInstance()
					.getItemDurability(reward, item));
			itemSkull.put(item,
					ConfigRewards.getInstance().getItemSkull(reward, item));
			HashMap<String, Integer> enchants = new HashMap<String, Integer>();
			for (String enchant : ConfigRewards.getInstance().getItemEnchants(
					reward, item)) {
				enchants.put(enchant, ConfigRewards.getInstance()
						.getItemEnchantsLevel(reward, item, enchant));

			}
			itemEnchants.put(item, enchants);
		}

		setMoney(ConfigRewards.getInstance().getMoney(reward));
		setMinMoney(ConfigRewards.getInstance().getMinMoney(reward));
		setMaxMoney(ConfigRewards.getInstance().getMaxMoney(reward));

		setExp(ConfigRewards.getInstance().getEXP(reward));

		setConsoleCommands(ConfigRewards.getInstance().getCommandsConsole(
				reward));
		setPlayerCommands(ConfigRewards.getInstance().getCommandsPlayer(reward));

		potions = ConfigRewards.getInstance().getPotions(reward);
		potionsDuration = new HashMap<String, Integer>();
		potionsAmplifier = new HashMap<String, Integer>();
		for (String potion : potions) {
			potionsDuration.put(potion, ConfigRewards.getInstance()
					.getPotionsDuration(reward, potion));
			potionsAmplifier.put(potion, ConfigRewards.getInstance()
					.getPotionsAmplifier(reward, potion));
		}

		setRewardMsg(ConfigRewards.getInstance().getMessagesReward(reward));

	}

	/**
	 * Check chance.
	 *
	 * @return true, if successful
	 */
	public boolean checkChance() {
		double chance = getChance();

		if ((chance == 0) || (chance == 100)) {
			return true;
		}

		double randomNum = (Math.random() * 100) + 1;

		plugin.debug("Random: " + randomNum + ", Chance: " + chance);

		if (randomNum <= chance) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Check delayed.
	 *
	 * @param user the user
	 * @return true, if successful
	 */
	public boolean checkDelayed(User user) {
		if (!isDelayEnabled()) {
			return false;
		}

		Date time = new Date();
		time = DateUtils.addHours(time, getDelayHours());
		time = DateUtils.addMinutes(time, getDelayMinutes());
		user.setTimedReward(this, time.getTime());

		plugin.debug("Giving reward " + name + " in " + getDelayHours()
				+ " hours " + getDelayMinutes() + " minutes ("
				+ time.toString() + ")");
		return true;

	}

	/**
	 * Check random chance.
	 *
	 * @return true, if successful
	 */
	public boolean checkRandomChance() {
		double chance = getRandomChance();

		if ((chance == 0) || (chance == 100)) {
			return true;
		}

		double randomNum = (Math.random() * 100) + 1;

		plugin.debug("Random: Random: " + randomNum + ", Chance: " + chance);

		if (randomNum <= chance) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Check timed.
	 *
	 * @param user the user
	 * @return true, if successful
	 */
	@SuppressWarnings("deprecation")
	public boolean checkTimed(User user) {
		if (!isTimedEnabled()) {
			return false;
		}

		Date time = new Date();
		time.setHours(getTimedHour());
		time.setMinutes(getTimedMinute());
		if (new Date().after(time)) {
			time = DateUtils.addDays(time, 1);
		}
		user.setTimedReward(this, time.getTime());

		plugin.debug("Giving reward " + name + " at " + time.toString());
		return true;
	}

	/**
	 * Gets the chance.
	 *
	 * @return the chance
	 */
	public double getChance() {
		return chance;
	}

	/**
	 * Gets the console commands.
	 *
	 * @return the console commands
	 */
	public ArrayList<String> getConsoleCommands() {
		return consoleCommands;
	}

	/**
	 * Gets the delay hours.
	 *
	 * @return the delay hours
	 */
	public int getDelayHours() {
		return delayHours;
	}

	/**
	 * Gets the delay minutes.
	 *
	 * @return the delay minutes
	 */
	public int getDelayMinutes() {
		return delayMinutes;
	}

	/**
	 * Gets the exp.
	 *
	 * @return the exp
	 */
	public int getExp() {
		return exp;
	}

	/**
	 * Gets the item amount.
	 *
	 * @return the item amount
	 */
	public HashMap<String, Integer> getItemAmount() {
		return itemAmount;
	}

	/**
	 * Gets the item amount.
	 *
	 * @param item the item
	 * @return the item amount
	 */
	public int getItemAmount(String item) {
		int amount = getItemAmount().get(item);
		int maxAmount = getItemMaxAmount().get(item);
		int minAmount = getItemMinAmount().get(item);
		if ((maxAmount == 0) && (minAmount == 0)) {
			return amount;
		} else {
			int num = (int) (Math.random() * maxAmount);
			num++;
			if (num < minAmount) {
				num = minAmount;
			}
			return num;
		}
	}

	/**
	 * Gets the item data.
	 *
	 * @return the item data
	 */
	public HashMap<String, Integer> getItemData() {
		return itemData;
	}

	/**
	 * Gets the item durabilty.
	 *
	 * @return the item durabilty
	 */
	public HashMap<String, Integer> getItemDurabilty() {
		return itemDurabilty;
	}

	/**
	 * Gets the item enchants.
	 *
	 * @return the item enchants
	 */
	public HashMap<String, HashMap<String, Integer>> getItemEnchants() {
		return itemEnchants;
	}

	/**
	 * Gets the item lore.
	 *
	 * @return the item lore
	 */
	public HashMap<String, ArrayList<String>> getItemLore() {
		return itemLore;
	}

	/**
	 * Gets the item material.
	 *
	 * @return the item material
	 */
	public HashMap<String, String> getItemMaterial() {
		return itemMaterial;
	}

	/**
	 * Gets the item max amount.
	 *
	 * @return the item max amount
	 */
	public HashMap<String, Integer> getItemMaxAmount() {
		return itemMaxAmount;
	}

	/**
	 * Gets the item min amount.
	 *
	 * @return the item min amount
	 */
	public HashMap<String, Integer> getItemMinAmount() {
		return itemMinAmount;
	}

	/**
	 * Gets the item name.
	 *
	 * @return the item name
	 */
	public HashMap<String, String> getItemName() {
		return itemName;
	}

	/**
	 * Gets the items.
	 *
	 * @return the items
	 */
	public Set<String> getItems() {
		return items;
	}

	/**
	 * Gets the item skull.
	 *
	 * @return the item skull
	 */
	public HashMap<String, String> getItemSkull() {
		return itemSkull;
	}

	/**
	 * Gets the max money.
	 *
	 * @return the max money
	 */
	public int getMaxMoney() {
		return MaxMoney;
	}

	/**
	 * Gets the min money.
	 *
	 * @return the min money
	 */
	public int getMinMoney() {
		return MinMoney;
	}

	/**
	 * Gets the money.
	 *
	 * @return the money
	 */
	public int getMoney() {
		return money;
	}

	/**
	 * Gets the money to give.
	 *
	 * @return the money to give
	 */
	public int getMoneyToGive() {
		int amount = getMoney();
		int maxAmount = getMaxMoney();
		int minAmount = getMinMoney();
		if ((maxAmount == 0) && (minAmount == 0)) {
			return amount;
		} else {
			int num = (int) (Math.random() * maxAmount);
			num++;
			if (num < minAmount) {
				num = minAmount;
			}
			return num;
		}
	}

	/**
	 * Gets the player commands.
	 *
	 * @return the player commands
	 */
	public ArrayList<String> getPlayerCommands() {
		return playerCommands;
	}

	/**
	 * Gets the potions.
	 *
	 * @return the potions
	 */
	public Set<String> getPotions() {
		return potions;
	}

	/**
	 * Gets the potions amplifier.
	 *
	 * @return the potions amplifier
	 */
	public HashMap<String, Integer> getPotionsAmplifier() {
		return potionsAmplifier;
	}

	/**
	 * Gets the potions duration.
	 *
	 * @return the potions duration
	 */
	public HashMap<String, Integer> getPotionsDuration() {
		return potionsDuration;
	}

	/**
	 * Gets the random chance.
	 *
	 * @return the random chance
	 */
	public double getRandomChance() {
		return randomChance;
	}

	/**
	 * Gets the random fall back.
	 *
	 * @return the random fall back
	 */
	public ArrayList<String> getRandomFallBack() {
		return randomFallBack;
	}

	/**
	 * Gets the random rewards.
	 *
	 * @return the random rewards
	 */
	public ArrayList<String> getRandomRewards() {
		return randomRewards;
	}

	/**
	 * Gets the reward msg.
	 *
	 * @return the reward msg
	 */
	public String getRewardMsg() {
		return rewardMsg;
	}

	/**
	 * Gets the reward name.
	 *
	 * @return the reward name
	 */
	public String getRewardName() {
		return name;
	}

	/**
	 * Gets the timed hour.
	 *
	 * @return the timed hour
	 */
	public int getTimedHour() {
		return timedHour;
	}

	/**
	 * Gets the timed minute.
	 *
	 * @return the timed minute
	 */
	public int getTimedMinute() {
		return timedMinute;
	}

	/**
	 * Gets the worlds.
	 *
	 * @return the worlds
	 */
	public ArrayList<String> getWorlds() {
		return worlds;
	}

	/**
	 * Give exp.
	 *
	 * @param user the user
	 */
	public void giveExp(User user) {
		user.giveExp(getExp());
	}

	/**
	 * Give items.
	 *
	 * @param user the user
	 */
	public void giveItems(User user) {
		for (String item : getItems()) {
			ItemStack itemStack = new ItemStack(
					Material.valueOf(getItemMaterial().get(item)),
					getItemAmount(item), Short.valueOf(Integer
							.toString(getItemData().get(item))));
			String name = getItemName().get(item);
			if (name != null) {
				itemStack = Utils.getInstance().nameItem(itemStack,
						name.replace("%Player%", user.getPlayerName()));
			}
			itemStack = Utils.getInstance().addLore(
					itemStack,
					Utils.getInstance().replace(getItemLore().get(item),
							"%Player%", user.getPlayerName()));
			itemStack = Utils.getInstance().addEnchants(itemStack,
					getItemEnchants().get(item));
			itemStack = Utils.getInstance().setDurabilty(itemStack,
					getItemDurabilty().get(item));
			String skull = getItemSkull().get(item);
			if (skull != null) {
				itemStack = Utils.getInstance().setSkullOwner(itemStack,
						skull.replace("%Player%", user.getPlayerName()));
			}
			user.giveItem(itemStack);
		}
	}

	/**
	 * Give money.
	 *
	 * @param user the user
	 */
	public void giveMoney(User user) {
		user.giveMoney(getMoneyToGive());
	}

	/**
	 * Give potions.
	 *
	 * @param user the user
	 */
	public void givePotions(User user) {
		for (String potionName : getPotions()) {
			user.givePotionEffect(potionName,
					getPotionsDuration().get(potionName), getPotionsAmplifier()
					.get(potionName));
		}
	}

	/**
	 * Give random.
	 *
	 * @param user the user
	 */
	public void giveRandom(User user) {
		if (checkRandomChance()) {
			ArrayList<String> rewards = getRandomRewards();
			if (rewards != null) {
				if (rewards.size() > 0) {
					String reward = rewards.get((int) Math.random()
							* rewards.size());
					if (reward.equalsIgnoreCase("")) {
						user.giveReward(ConfigRewards.getInstance().getReward(
								reward));
					}
				}
			}
		} else {
			for (String reward : getRandomFallBack()) {
				if (reward.equalsIgnoreCase("")) {
					user.giveReward(ConfigRewards.getInstance().getReward(
							reward));
				}
			}
		}
	}

	/**
	 * Give reward.
	 *
	 * @param user the user
	 */
	public void giveReward(User user) {

		if (checkDelayed(user)) {
			return;
		}

		if (checkTimed(user)) {
			return;
		}

		giveRewardReward(user);
	}

	/**
	 * Give reward reward.
	 *
	 * @param user the user
	 */
	public void giveRewardReward(User user) {
		plugin.debug("Attempting to give " + user.getPlayerName() + " reward "
				+ name);

		if (checkChance()) {
			ArrayList<String> worlds = getWorlds();
			Player player = Bukkit.getPlayer(user.getPlayerName());
			if ((player != null) && (worlds != null)) {
				if (isGiveInEachWorld()) {
					for (String world : worlds) {
						if (player.getWorld().getName().equals(world)) {
							giveRewardUser(user);
						} else {
							Data.getInstance().setOfflineVotesSiteWorld(
									user,
									name,
									world,
									Data.getInstance()
									.getOfflineVotesSiteWorld(user,
											name, world) + 1);
						}
					}
				} else {
					if (worlds.contains(player.getWorld().getName())) {
						giveRewardUser(user);
					} else {
						Data.getInstance().setOfflineVotesSiteWorld(
								user,
								name,
								null,
								Data.getInstance().getOfflineVotesSiteWorld(
										user, name, null) + 1);
					}
				}
			} else {
				giveRewardUser(user);
			}
		}
		giveRandom(user);
	}

	/**
	 * Give the user rewards.
	 *
	 * @param user            User to give rewards to
	 */
	public void giveRewardUser(User user) {
		Player player = Bukkit.getPlayer(user.getPlayerName());
		if (player != null) {
			if (!isRequirePermission()
					|| player.hasPermission("VotingPlugin.Reward." + name)) {
				giveMoney(user);
				giveItems(user);
				giveExp(user);
				runCommands(user);
				givePotions(user);
				sendMessage(user);
				sendTitle(user);
				playSound(user);
				playEffect(user);

				plugin.debug("Gave " + user.getPlayerName() + " reward " + name);

			}
		}
	}

	/**
	 * Checks if is delay enabled.
	 *
	 * @return true, if is delay enabled
	 */
	public boolean isDelayEnabled() {
		return delayEnabled;
	}

	/**
	 * Checks if is give in each world.
	 *
	 * @return true, if is give in each world
	 */
	public boolean isGiveInEachWorld() {
		return giveInEachWorld;
	}

	/**
	 * Checks if is require permission.
	 *
	 * @return true, if is require permission
	 */
	public boolean isRequirePermission() {
		return requirePermission;
	}

	/**
	 * Checks if is timed enabled.
	 *
	 * @return true, if is timed enabled
	 */
	public boolean isTimedEnabled() {
		return timedEnabled;
	}

	/**
	 * Play effect.
	 *
	 * @param user the user
	 */
	public void playEffect(User user) {
		if (ConfigRewards.getInstance().getEffectEnabled(name)) {
			user.playParticleEffect(ConfigRewards.getInstance()
					.getEffectEffect(name), ConfigRewards.getInstance()
					.getEffectData(name), ConfigRewards.getInstance()
					.getEffectParticles(name), ConfigRewards.getInstance()
					.getEffectRadius(name));
		}
	}

	/**
	 * Play sound.
	 *
	 * @param user the user
	 */
	public void playSound(User user) {
		if (ConfigRewards.getInstance().getSoundEnabled(name)) {
			try {
				user.playSound(ConfigRewards.getInstance().getSoundSound(name),
						ConfigRewards.getInstance().getSoundVolume(name),
						ConfigRewards.getInstance().getSoundPitch(name));
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}

	/**
	 * Run commands.
	 *
	 * @param user the user
	 */
	public void runCommands(User user) {
		String playerName = user.getPlayerName();

		// Console commands
		ArrayList<String> consolecmds = getConsoleCommands();

		if (consolecmds != null) {
			for (String consolecmd : consolecmds) {
				if (consolecmd.length() > 0) {
					consolecmd = consolecmd.replace("%player%", playerName);
					Bukkit.getServer().dispatchCommand(
							Bukkit.getConsoleSender(), consolecmd);
				}
			}
		}

		// Player commands
		ArrayList<String> playercmds = getPlayerCommands();

		Player player = Bukkit.getPlayer(playerName);
		if (playercmds != null) {
			for (String playercmd : playercmds) {
				if ((player != null) && (playercmd.length() > 0)) {
					playercmd = playercmd.replace("%player%", playerName);
					player.performCommand(playercmd);
				}
			}
		}
	}

	/**
	 * Send message.
	 *
	 * @param user the user
	 */
	public void sendMessage(User user) {
		if (rewardMsg != null) {
			user.sendMessage(rewardMsg);
		} else {
			user.sendMessage(ConfigFormat.getInstance().getRewardMsg());
		}
	}

	/**
	 * Send title.
	 *
	 * @param user the user
	 */
	public void sendTitle(User user) {
		if (ConfigRewards.getInstance().getTitleEnabled(name)) {
			user.sendTitle(ConfigRewards.getInstance().getTitleTitle(name),

					ConfigRewards.getInstance().getTitleSubTitle(name),

					ConfigRewards.getInstance().getTitleFadeIn(name), ConfigRewards
					.getInstance().getTitleShowTime(name), ConfigRewards
					.getInstance().getTitleFadeOut(name));
		}
	}

	/**
	 * Sets the chance.
	 *
	 * @param chance the new chance
	 */
	public void setChance(double chance) {
		this.chance = chance;
	}

	/**
	 * Sets the console commands.
	 *
	 * @param consoleCommands the new console commands
	 */
	public void setConsoleCommands(ArrayList<String> consoleCommands) {
		this.consoleCommands = consoleCommands;
	}

	/**
	 * Sets the delay enabled.
	 *
	 * @param delayEnabled the new delay enabled
	 */
	public void setDelayEnabled(boolean delayEnabled) {
		this.delayEnabled = delayEnabled;
	}

	/**
	 * Sets the delay hours.
	 *
	 * @param delayHours the new delay hours
	 */
	public void setDelayHours(int delayHours) {
		this.delayHours = delayHours;
	}

	/**
	 * Sets the delay minutes.
	 *
	 * @param delayMinutes the new delay minutes
	 */
	public void setDelayMinutes(int delayMinutes) {
		this.delayMinutes = delayMinutes;
	}

	/**
	 * Sets the exp.
	 *
	 * @param exp the new exp
	 */
	public void setExp(int exp) {
		this.exp = exp;
	}

	/**
	 * Sets the give in each world.
	 *
	 * @param giveInEachWorld the new give in each world
	 */
	public void setGiveInEachWorld(boolean giveInEachWorld) {
		this.giveInEachWorld = giveInEachWorld;
	}

	/**
	 * Sets the item amount.
	 *
	 * @param itemAmount the item amount
	 */
	public void setItemAmount(HashMap<String, Integer> itemAmount) {
		this.itemAmount = itemAmount;
	}

	/**
	 * Sets the item data.
	 *
	 * @param itemData the item data
	 */
	public void setItemData(HashMap<String, Integer> itemData) {
		this.itemData = itemData;
	}

	/**
	 * Sets the item durabilty.
	 *
	 * @param itemDurabilty the item durabilty
	 */
	public void setItemDurabilty(HashMap<String, Integer> itemDurabilty) {
		this.itemDurabilty = itemDurabilty;
	}

	/**
	 * Sets the item enchants.
	 *
	 * @param itemEnchants the item enchants
	 */
	public void setItemEnchants(
			HashMap<String, HashMap<String, Integer>> itemEnchants) {
		this.itemEnchants = itemEnchants;
	}

	/**
	 * Sets the item lore.
	 *
	 * @param itemLore the item lore
	 */
	public void setItemLore(HashMap<String, ArrayList<String>> itemLore) {
		this.itemLore = itemLore;
	}

	/**
	 * Sets the item material.
	 *
	 * @param itemMaterial the item material
	 */
	public void setItemMaterial(HashMap<String, String> itemMaterial) {
		this.itemMaterial = itemMaterial;
	}

	/**
	 * Sets the item max amount.
	 *
	 * @param itemMaxAmount the item max amount
	 */
	public void setItemMaxAmount(HashMap<String, Integer> itemMaxAmount) {
		this.itemMaxAmount = itemMaxAmount;
	}

	/**
	 * Sets the item min amount.
	 *
	 * @param itemMinAmount the item min amount
	 */
	public void setItemMinAmount(HashMap<String, Integer> itemMinAmount) {
		this.itemMinAmount = itemMinAmount;
	}

	/**
	 * Sets the item name.
	 *
	 * @param itemName the item name
	 */
	public void setItemName(HashMap<String, String> itemName) {
		this.itemName = itemName;
	}

	/**
	 * Sets the items.
	 *
	 * @param items the new items
	 */
	public void setItems(Set<String> items) {
		this.items = items;
	}

	/**
	 * Sets the item skull.
	 *
	 * @param itemSkull the item skull
	 */
	public void setItemSkull(HashMap<String, String> itemSkull) {
		this.itemSkull = itemSkull;
	}

	/**
	 * Sets the max money.
	 *
	 * @param maxMoney the new max money
	 */
	public void setMaxMoney(int maxMoney) {
		MaxMoney = maxMoney;
	}

	/**
	 * Sets the min money.
	 *
	 * @param minMoney the new min money
	 */
	public void setMinMoney(int minMoney) {
		MinMoney = minMoney;
	}

	/**
	 * Sets the money.
	 *
	 * @param money the new money
	 */
	public void setMoney(int money) {
		this.money = money;
	}

	/**
	 * Sets the player commands.
	 *
	 * @param playerCommands the new player commands
	 */
	public void setPlayerCommands(ArrayList<String> playerCommands) {
		this.playerCommands = playerCommands;
	}

	/**
	 * Sets the potions.
	 *
	 * @param potions the new potions
	 */
	public void setPotions(Set<String> potions) {
		this.potions = potions;
	}

	/**
	 * Sets the potions amplifier.
	 *
	 * @param potionsAmplifier the potions amplifier
	 */
	public void setPotionsAmplifier(HashMap<String, Integer> potionsAmplifier) {
		this.potionsAmplifier = potionsAmplifier;
	}

	/**
	 * Sets the potions duration.
	 *
	 * @param potionsDuration the potions duration
	 */
	public void setPotionsDuration(HashMap<String, Integer> potionsDuration) {
		this.potionsDuration = potionsDuration;
	}

	/**
	 * Sets the random chance.
	 *
	 * @param randomChance the new random chance
	 */
	public void setRandomChance(double randomChance) {
		this.randomChance = randomChance;
	}

	/**
	 * Sets the random fall back.
	 *
	 * @param randomFallBack the new random fall back
	 */
	public void setRandomFallBack(ArrayList<String> randomFallBack) {
		this.randomFallBack = randomFallBack;
	}

	/**
	 * Sets the random rewards.
	 *
	 * @param randomRewards the new random rewards
	 */
	public void setRandomRewards(ArrayList<String> randomRewards) {
		this.randomRewards = randomRewards;
	}

	/**
	 * Sets the require permission.
	 *
	 * @param requirePermission the new require permission
	 */
	public void setRequirePermission(boolean requirePermission) {
		this.requirePermission = requirePermission;
	}

	/**
	 * Sets the reward msg.
	 *
	 * @param rewardMsg the new reward msg
	 */
	public void setRewardMsg(String rewardMsg) {
		this.rewardMsg = rewardMsg;
	}

	/**
	 * Sets the timed enabled.
	 *
	 * @param timedEnabled the new timed enabled
	 */
	public void setTimedEnabled(boolean timedEnabled) {
		this.timedEnabled = timedEnabled;
	}

	/**
	 * Sets the timed hour.
	 *
	 * @param timedHour the new timed hour
	 */
	public void setTimedHour(int timedHour) {
		this.timedHour = timedHour;
	}

	/**
	 * Sets the timed minute.
	 *
	 * @param timedMinute the new timed minute
	 */
	public void setTimedMinute(int timedMinute) {
		this.timedMinute = timedMinute;
	}

	/**
	 * Sets the worlds.
	 *
	 * @param worlds the new worlds
	 */
	public void setWorlds(ArrayList<String> worlds) {
		this.worlds = worlds;
	}
}
