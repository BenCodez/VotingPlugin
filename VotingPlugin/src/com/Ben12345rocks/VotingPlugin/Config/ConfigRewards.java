package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Data.Data;
import com.Ben12345rocks.VotingPlugin.Objects.Reward;
import com.Ben12345rocks.VotingPlugin.Objects.User;
import com.Ben12345rocks.VotingPlugin.Util.Files.Files;

// TODO: Auto-generated Javadoc
/**
 * The Class ConfigRewards.
 */
public class ConfigRewards {

	/** The instance. */
	static ConfigRewards instance = new ConfigRewards();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of ConfigRewards.
	 *
	 * @return single instance of ConfigRewards
	 */
	public static ConfigRewards getInstance() {
		return instance;
	}

	/**
	 * Instantiates a new config rewards.
	 */
	private ConfigRewards() {
	}

	/**
	 * Instantiates a new config rewards.
	 *
	 * @param plugin
	 *            the plugin
	 */
	public ConfigRewards(Main plugin) {
		ConfigRewards.plugin = plugin;
	}

	/**
	 * Check delayed timed rewards.
	 */
	public void checkDelayedTimedRewards() {
		for (User user : Data.getInstance().getUsers()) {
			for (Reward reward : plugin.rewards) {
				long time = user.getTimedReward(reward);
				if (time != 0) {
					Date timeDate = new Date(time);
					if (new Date().after(timeDate)) {
						reward.giveRewardReward(user, true);
						user.setTimedReward(reward, 0);
					}
				}
			}
		}
	}

	/**
	 * Gets the boss bar enabled.
	 *
	 * @param reward
	 *            the reward
	 * @return the boss bar enabled
	 */
	public boolean getBossBarEnabled(String reward) {
		return getData(reward).getBoolean("BossBar.Enabled");
	}

	/**
	 * Gets the boss bar message.
	 *
	 * @param reward
	 *            the reward
	 * @return the boss bar message
	 */
	public String getBossBarMessage(String reward) {
		return getData(reward).getString("BossBar.Message");
	}

	/**
	 * Gets the boss bar progress.
	 *
	 * @param reward
	 *            the reward
	 * @return the boss bar progress
	 */
	public double getBossBarProgress(String reward) {
		return getData(reward).getDouble("BossBar.Progress");
	}

	/**
	 * Gets the boss bar color.
	 *
	 * @param reward
	 *            the reward
	 * @return the boss bar color
	 */
	public String getBossBarColor(String reward) {
		return getData(reward).getString("BossBar.Color");
	}

	/**
	 * Gets the boss bar style.
	 *
	 * @param reward
	 *            the reward
	 * @return the boss bar style
	 */
	public String getBossBarStyle(String reward) {
		return getData(reward).getString("BossBar.Style");
	}

	/**
	 * Gets the boss bar delay.
	 *
	 * @param reward
	 *            the reward
	 * @return the boss bar delay
	 */
	public int getBossBarDelay(String reward) {
		return getData(reward).getInt("BossBar.Delay");
	}

	/**
	 * Gets the chance.
	 *
	 * @param reward
	 *            the reward
	 * @return the chance
	 */
	public double getChance(String reward) {
		return getData(reward).getDouble("Chance");
	}

	/**
	 * Gets the commands console.
	 *
	 * @param reward
	 *            the reward
	 * @return the commands console
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getCommandsConsole(String reward) {

		return (ArrayList<String>) getData(reward).getList("Commands.Console",
				new ArrayList<String>());

	}

	/**
	 * Gets the commands player.
	 *
	 * @param reward
	 *            the reward
	 * @return the commands player
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getCommandsPlayer(String reward) {

		return (ArrayList<String>) getData(reward).getList("Commands.Player",
				new ArrayList<String>());

	}

	/**
	 * Gets the data.
	 *
	 * @param reward
	 *            the reward
	 * @return the data
	 */
	public FileConfiguration getData(String reward) {
		File dFile = getRewardFile(reward);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		return data;
	}

	/**
	 * Gets the delayed enabled.
	 *
	 * @param reward
	 *            the reward
	 * @return the delayed enabled
	 */
	public boolean getDelayedEnabled(String reward) {
		return getData(reward).getBoolean("Delayed.Enabled");
	}

	/**
	 * Gets the delayed hours.
	 *
	 * @param reward
	 *            the reward
	 * @return the delayed hours
	 */
	public int getDelayedHours(String reward) {
		return getData(reward).getInt("Delayed.Hours");
	}

	/**
	 * Gets the delayed minutes.
	 *
	 * @param reward
	 *            the reward
	 * @return the delayed minutes
	 */
	public int getDelayedMinutes(String reward) {
		return getData(reward).getInt("Delayed.Minutes");
	}

	/**
	 * Gets the effect data.
	 *
	 * @param reward
	 *            the reward
	 * @return the effect data
	 */
	public int getEffectData(String reward) {
		return getData(reward).getInt("Effect.Data");
	}

	/**
	 * Gets the effect effect.
	 *
	 * @param reward
	 *            the reward
	 * @return the effect effect
	 */
	public String getEffectEffect(String reward) {
		return getData(reward).getString("Effect.Effect", "");

	}

	/**
	 * Gets the effect enabled.
	 *
	 * @param reward
	 *            the reward
	 * @return the effect enabled
	 */
	public boolean getEffectEnabled(String reward) {
		return getData(reward).getBoolean("Effect.Enabled");
	}

	/**
	 * Gets the effect particles.
	 *
	 * @param reward
	 *            the reward
	 * @return the effect particles
	 */
	public int getEffectParticles(String reward) {
		return getData(reward).getInt("Effect.Particles");
	}

	/**
	 * Gets the effect radius.
	 *
	 * @param reward
	 *            the reward
	 * @return the effect radius
	 */
	public int getEffectRadius(String reward) {
		return getData(reward).getInt("Effect.Radius");
	}

	/**
	 * Gets the exp.
	 *
	 * @param reward
	 *            the reward
	 * @return the exp
	 */
	public int getEXP(String reward) {
		return getData(reward).getInt("EXP");
	}

	/**
	 * Gets the give in each world.
	 *
	 * @param reward
	 *            the reward
	 * @return the give in each world
	 */
	public boolean getGiveInEachWorld(String reward) {
		return getData(reward).getBoolean("GiveInEachWorld");
	}

	/**
	 * Gets the item amount.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @return the item amount
	 */
	public int getItemAmount(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".Amount");
	}

	/**
	 * Gets the item data.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @return the item data
	 */
	public int getItemData(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".Data");
	}

	/**
	 * Gets the item durability.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @return the item durability
	 */
	public int getItemDurability(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".Durability");
	}

	/**
	 * Gets the item enchants.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @return the item enchants
	 */
	public Set<String> getItemEnchants(String reward, String item) {
		try {
			return getData(reward).getConfigurationSection(
					"Items." + item + ".Enchants").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the item enchants level.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @param enchant
	 *            the enchant
	 * @return the item enchants level
	 */
	public int getItemEnchantsLevel(String reward, String item, String enchant) {
		return getData(reward).getInt("Items." + item + ".Enchants." + enchant);
	}

	/**
	 * Gets the item lore.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @return the item lore
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getItemLore(String reward, String item) {
		return (ArrayList<String>) getData(reward).getList(
				"Items." + item + ".Lore");
	}

	/**
	 * Gets the item material.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @return the item material
	 */
	public String getItemMaterial(String reward, String item) {
		return getData(reward).getString("Items." + item + ".Material");
	}

	/**
	 * Gets the item max amount.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @return the item max amount
	 */
	public int getItemMaxAmount(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".MaxAmount");
	}

	/**
	 * Gets the item min amount.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @return the item min amount
	 */
	public int getItemMinAmount(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".MinAmount");
	}

	/**
	 * Gets the item name.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @return the item name
	 */
	public String getItemName(String reward, String item) {
		return getData(reward).getString("Items." + item + ".Name");
	}

	/**
	 * Gets the items.
	 *
	 * @param reward
	 *            the reward
	 * @return the items
	 */
	public Set<String> getItems(String reward) {
		try {
			return getData(reward).getConfigurationSection("Items").getKeys(
					false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the item skull.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @return the item skull
	 */
	public String getItemSkull(String reward, String item) {
		return getData(reward).getString("Items." + item + ".Skull");
	}

	/**
	 * Gets the max exp.
	 *
	 * @param reward
	 *            the reward
	 * @return the max exp
	 */
	public int getMaxExp(String reward) {
		return getData(reward).getInt("MaxEXP");
	}

	/**
	 * Gets the max money.
	 *
	 * @param reward
	 *            the reward
	 * @return the max money
	 */
	public int getMaxMoney(String reward) {
		return getData(reward).getInt("MaxMoney");
	}

	/**
	 * Gets the action bar message.
	 *
	 * @param reward
	 *            the reward
	 * @return the action bar message
	 */
	public String getActionBarMessage(String reward) {
		return getData(reward).getString("ActionBar.Message");
	}

	/**
	 * Gets the action bar delay.
	 *
	 * @param reward
	 *            the reward
	 * @return the action bar delay
	 */
	public int getActionBarDelay(String reward) {
		return getData(reward).getInt("ActionBar.Delay");
	}

	/**
	 * Gets the messages reward.
	 *
	 * @param reward
	 *            the reward
	 * @return the messages reward
	 */
	public String getMessagesReward(String reward) {
		String msg = getData(reward).getString("Messages.Reward",
				ConfigFormat.getInstance().getRewardMsg());
		return msg;

	}

	/**
	 * Gets the min exp.
	 *
	 * @param reward
	 *            the reward
	 * @return the min exp
	 */
	public int getMinExp(String reward) {
		return getData(reward).getInt("MinEXP");
	}

	/**
	 * Gets the min money.
	 *
	 * @param reward
	 *            the reward
	 * @return the min money
	 */
	public int getMinMoney(String reward) {
		return getData(reward).getInt("MinMoney");
	}

	/**
	 * Gets the money.
	 *
	 * @param reward
	 *            the reward
	 * @return the money
	 */
	public int getMoney(String reward) {
		return getData(reward).getInt("Money");
	}

	/**
	 * Gets the permission.
	 *
	 * @param reward
	 *            the reward
	 * @return the permission
	 */
	public String getPermission(String reward) {
		return "VotingPlugin.Reward." + reward;
	}

	/**
	 * Gets the potions.
	 *
	 * @param reward
	 *            the reward
	 * @return the potions
	 */
	public Set<String> getPotions(String reward) {
		try {
			return getData(reward).getConfigurationSection("Potions").getKeys(
					false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	/**
	 * Gets the potions amplifier.
	 *
	 * @param reward
	 *            the reward
	 * @param potion
	 *            the potion
	 * @return the potions amplifier
	 */
	public int getPotionsAmplifier(String reward, String potion) {
		return getData(reward).getInt("Potions." + potion + ".Amplifier");
	}

	/**
	 * Gets the potions duration.
	 *
	 * @param reward
	 *            the reward
	 * @param potion
	 *            the potion
	 * @return the potions duration
	 */
	public int getPotionsDuration(String reward, String potion) {
		return getData(reward).getInt("Potions." + potion + ".Duration");
	}

	/**
	 * Gets the random chance.
	 *
	 * @param reward
	 *            the reward
	 * @return the random chance
	 */
	public double getRandomChance(String reward) {
		return getData(reward).getDouble("Random.Chance");
	}

	/**
	 * Gets the random fall back.
	 *
	 * @param reward
	 *            the reward
	 * @return the random fall back
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getRandomFallBack(String reward) {
		try {
			return (ArrayList<String>) getData(reward).getList(
					"Random.FallBack");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	/**
	 * Gets the random rewards.
	 *
	 * @param reward
	 *            the reward
	 * @return the random rewards
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getRandomRewards(String reward) {

		return (ArrayList<String>) getData(reward).getList("Random.Rewards",
				new ArrayList<String>());

	}

	/**
	 * Gets the require permission.
	 *
	 * @param reward
	 *            the reward
	 * @return the require permission
	 */
	public boolean getRequirePermission(String reward) {
		return getData(reward).getBoolean("RequirePermission");
	}

	/**
	 * Gets the reward.
	 *
	 * @param reward
	 *            the reward
	 * @return the reward
	 */
	public Reward getReward(String reward) {
		reward = reward.replace(" ", "_");
		if (plugin.rewards != null) {
			for (Reward rewardFile : plugin.rewards) {
				if (rewardFile.name.equals(reward)) {
					return rewardFile;
				}
			}
		}
		return new Reward(reward);
	}

	/**
	 * Gets the reward file.
	 *
	 * @param reward
	 *            the reward
	 * @return the reward file
	 */
	public File getRewardFile(String reward) {
		File dFile = new File(plugin.getDataFolder() + File.separator
				+ "Rewards", reward + ".yml");
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		if (!dFile.exists()) {
			try {
				data.save(dFile);
			} catch (IOException e) {
				plugin.getLogger().severe(
						ChatColor.RED + "Could not create Rewards/" + reward
								+ ".yml!");

			}
		}
		return dFile;

	}

	/**
	 * Gets the reward files.
	 *
	 * @return the reward files
	 */
	public ArrayList<String> getRewardFiles() {
		File folder = new File(plugin.getDataFolder() + File.separator
				+ "Rewards");
		String[] fileNames = folder.list();
		return Utils.getInstance().convertArray(fileNames);
	}

	/**
	 * Gets the reward names.
	 *
	 * @return the reward names
	 */
	public ArrayList<String> getRewardNames() {
		ArrayList<String> rewardFiles = getRewardFiles();
		if (rewardFiles == null) {
			return new ArrayList<String>();
		}
		for (int i = 0; i < rewardFiles.size(); i++) {
			rewardFiles.set(i, rewardFiles.get(i).replace(".yml", ""));
		}

		Collections.sort(rewardFiles, String.CASE_INSENSITIVE_ORDER);

		return rewardFiles;
	}

	/**
	 * Gets the reward type.
	 *
	 * @param reward
	 *            the reward
	 * @return the reward type
	 */
	public String getRewardType(String reward) {
		String str = getData(reward).getString("RewardType");
		if (str != null) {
			if (str.equalsIgnoreCase("online")) {
				return "ONLINE";
			} else if (str.equalsIgnoreCase("offline")) {
				return "OFFLINE";
			} else {
				return "BOTH";
			}
		}
		return "BOTH";
	}

	/**
	 * Gets the sound enabled.
	 *
	 * @param reward
	 *            the reward
	 * @return the sound enabled
	 */
	public boolean getSoundEnabled(String reward) {
		return getData(reward).getBoolean("Sound.Enabled");
	}

	/**
	 * Gets the sound pitch.
	 *
	 * @param reward
	 *            the reward
	 * @return the sound pitch
	 */
	public float getSoundPitch(String reward) {
		return (float) getData(reward).getDouble("Sound.Pitch");
	}

	/**
	 * Gets the sound sound.
	 *
	 * @param reward
	 *            the reward
	 * @return the sound sound
	 */
	public String getSoundSound(String reward) {
		return getData(reward).getString("Sound.Sound");
	}

	/**
	 * Gets the sound volume.
	 *
	 * @param reward
	 *            the reward
	 * @return the sound volume
	 */
	public float getSoundVolume(String reward) {
		return (float) getData(reward).getDouble("Sound.Volume");
	}

	/**
	 * Gets the timed enabled.
	 *
	 * @param reward
	 *            the reward
	 * @return the timed enabled
	 */
	public boolean getTimedEnabled(String reward) {
		return getData(reward).getBoolean("Timed.Enabled");
	}

	/**
	 * Gets the timed hour.
	 *
	 * @param reward
	 *            the reward
	 * @return the timed hour
	 */
	public int getTimedHour(String reward) {
		return getData(reward).getInt("Timed.Hour");
	}

	/**
	 * Gets the timed minute.
	 *
	 * @param reward
	 *            the reward
	 * @return the timed minute
	 */
	public int getTimedMinute(String reward) {
		return getData(reward).getInt("Timed.Minute");
	}

	/**
	 * Gets the title enabled.
	 *
	 * @param reward
	 *            the reward
	 * @return the title enabled
	 */
	public boolean getTitleEnabled(String reward) {
		return getData(reward).getBoolean("Title.Enabled");
	}

	/**
	 * Gets the title fade in.
	 *
	 * @param reward
	 *            the reward
	 * @return the title fade in
	 */
	public int getTitleFadeIn(String reward) {
		return getData(reward).getInt("Title.FadeIn");
	}

	/**
	 * Gets the title fade out.
	 *
	 * @param reward
	 *            the reward
	 * @return the title fade out
	 */
	public int getTitleFadeOut(String reward) {
		return getData(reward).getInt("Title.FadeOut");
	}

	/**
	 * Gets the title show time.
	 *
	 * @param reward
	 *            the reward
	 * @return the title show time
	 */
	public int getTitleShowTime(String reward) {
		return getData(reward).getInt("Title.ShowTime");
	}

	/**
	 * Gets the title sub title.
	 *
	 * @param reward
	 *            the reward
	 * @return the title sub title
	 */
	public String getTitleSubTitle(String reward) {
		return getData(reward).getString("Title.SubTitle");
	}

	/**
	 * Gets the title title.
	 *
	 * @param reward
	 *            the reward
	 * @return the title title
	 */
	public String getTitleTitle(String reward) {
		return getData(reward).getString("Title.Title");
	}

	/**
	 * Gets the worlds.
	 *
	 * @param reward
	 *            the reward
	 * @return the worlds
	 */
	@SuppressWarnings("unchecked")
	public ArrayList<String> getWorlds(String reward) {

		return (ArrayList<String>) getData(reward).getList("Worlds",
				new ArrayList<String>());

	}

	/**
	 * Rename reward.
	 *
	 * @param reward
	 *            the reward
	 * @param newName
	 *            the new name
	 * @return true, if successful
	 */
	public boolean renameReward(String reward, String newName) {
		return getRewardFile(reward).renameTo(
				new File(plugin.getDataFolder() + File.separator + "Rewards",
						newName + ".yml"));
	}

	/**
	 * Sets the.
	 *
	 * @param reward
	 *            the reward
	 * @param path
	 *            the path
	 * @param value
	 *            the value
	 */
	public void set(String reward, String path, Object value) {
		File dFile = getRewardFile(reward);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		data.set(path, value);
		Files.getInstance().editFile(dFile, data);
	}

	/**
	 * Sets the chance.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setChance(String reward, int value) {
		set(reward, "Chance", value);
	}

	/**
	 * Sets the commands console.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setCommandsConsole(String reward, ArrayList<String> value) {
		set(reward, "Commands.Console", value);
	}

	/**
	 * Sets the commands plater.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setCommandsPlayer(String reward, ArrayList<String> value) {
		set(reward, "Commands.Player", value);
	}

	/**
	 * Sets the give in each world.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setGiveInEachWorld(String reward, boolean value) {
		set(reward, "GiveInEachWorld", value);
	}

	/**
	 * Sets the item amount.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @param value
	 *            the value
	 */
	public void setItemAmount(String reward, String item, int value) {
		set(reward, "Items." + item + ".Amount", value);
	}

	/**
	 * Sets the item data.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @param value
	 *            the value
	 */
	public void setItemData(String reward, String item, int value) {
		set(reward, "Items." + item + ".Data", value);
	}

	/**
	 * Sets the item enchant.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @param enchant
	 *            the enchant
	 * @param value
	 *            the value
	 */
	public void setItemEnchant(String reward, String item, String enchant,
			int value) {
		set(reward, "Items." + item + ".Enchants." + enchant, value);
	}

	/**
	 * Sets the item lore.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @param value
	 *            the value
	 */
	public void setItemLore(String reward, String item, ArrayList<String> value) {
		set(reward, "Items." + item + ".Lore", value);
	}

	/**
	 * Sets the item material.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @param value
	 *            the value
	 */
	public void setItemMaterial(String reward, String item, String value) {
		set(reward, "Items." + item + ".Material", value);
	}

	/**
	 * Sets the item max amount.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @param value
	 *            the value
	 */
	public void setItemMaxAmount(String reward, String item, int value) {
		set(reward, "Items." + item + ".MaxAmount", value);
	}

	/**
	 * Sets the item min amount.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @param value
	 *            the value
	 */
	public void setItemMinAmount(String reward, String item, int value) {
		set(reward, "Items." + item + ".MinAmount", value);
	}

	/**
	 * Sets the item name.
	 *
	 * @param reward
	 *            the reward
	 * @param item
	 *            the item
	 * @param value
	 *            the value
	 */
	public void setItemName(String reward, String item, String value) {
		set(reward, "Items." + item + ".Name", value);
	}

	/**
	 * Sets the max money.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setMaxMoney(String reward, int value) {
		set(reward, "MaxMoney", value);
	}

	/**
	 * Sets the messages reward.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setMessagesReward(String reward, String value) {
		set(reward, "Messages.Reward", value);
	}

	/**
	 * Sets the min money.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setMinMoney(String reward, int value) {
		set(reward, "MinMoney", value);
	}

	/**
	 * Sets the money.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setMoney(String reward, int value) {
		set(reward, "Money", value);
	}

	/**
	 * Sets the potions amplifier.
	 *
	 * @param reward
	 *            the reward
	 * @param potion
	 *            the potion
	 * @param value
	 *            the value
	 */
	public void setPotionsAmplifier(String reward, String potion, int value) {
		set(reward, "Potions." + potion + ".Amplifier", value);
	}

	/**
	 * Sets the potions duration.
	 *
	 * @param reward
	 *            the reward
	 * @param potion
	 *            the potion
	 * @param value
	 *            the value
	 */
	public void setPotionsDuration(String reward, String potion, int value) {
		set(reward, "Potions." + potion + ".Duration", value);
	}

	/**
	 * Sets the require permission.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setRequirePermission(String reward, boolean value) {
		set(reward, "RequirePermission", value);
	}

	/**
	 * Setup example.
	 */
	public void setupExample() {
		if (!plugin.getDataFolder().exists()) {
			plugin.getDataFolder().mkdir();
		}

		File file = new File(plugin.getDataFolder(), "Rewards" + File.separator
				+ "ExampleReward.yml");
		if (!file.exists()) {
			plugin.saveResource("Rewards" + File.separator
					+ "ExampleReward.yml", true);
		}

	}

	/**
	 * Sets the worlds.
	 *
	 * @param reward
	 *            the reward
	 * @param value
	 *            the value
	 */
	public void setWorlds(String reward, ArrayList<String> value) {
		set(reward, "Worlds", value);
	}

}
