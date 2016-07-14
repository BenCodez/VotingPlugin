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
import com.Ben12345rocks.VotingPlugin.Files.Files;
import com.Ben12345rocks.VotingPlugin.Objects.Reward;
import com.Ben12345rocks.VotingPlugin.Objects.User;

public class ConfigRewards {

	static ConfigRewards instance = new ConfigRewards();

	static Main plugin = Main.plugin;

	public static ConfigRewards getInstance() {
		return instance;
	}

	private ConfigRewards() {
	}

	public ConfigRewards(Main plugin) {
		ConfigRewards.plugin = plugin;
	}

	public void checkDelayedTimedRewards() {
		for (User user : Data.getInstance().getUsers()) {
			for (Reward reward : plugin.rewards) {
				long time = user.getTimedReward(reward);
				if (time != 0) {
					Date timeDate = new Date(time);
					if (new Date().after(timeDate)) {
						reward.giveRewardReward(user);
						user.setTimedReward(reward, 0);
					}
				}
			}
		}
	}

	public double getChance(String reward) {
		return getData(reward).getDouble("Chance");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getCommandsConsole(String reward) {
		try {
			return (ArrayList<String>) getData(reward).getList(
					"Commands.Console");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getCommandsPlayer(String reward) {
		try {
			return (ArrayList<String>) getData(reward).getList(
					"Commands.Player");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	public FileConfiguration getData(String reward) {
		File dFile = getRewardFile(reward);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		return data;
	}

	public boolean getDelayedEnabled(String reward) {
		return getData(reward).getBoolean("Delayed.Enabled");
	}

	public int getDelayedHours(String reward) {
		return getData(reward).getInt("Delayed.Hours");
	}

	public int getDelayedMinutes(String reward) {
		return getData(reward).getInt("Delayed.Minutes");
	}

	public int getEffectData(String reward) {
		return getData(reward).getInt("Effect.Data");
	}

	public String getEffectEffect(String reward) {
		String str = getData(reward).getString("Effect.Effect");
		if (str != null) {
			return str;
		}
		return "";
	}

	public boolean getEffectEnabled(String reward) {
		return getData(reward).getBoolean("Effect.Enabled");
	}

	public int getEffectParticles(String reward) {
		return getData(reward).getInt("Effect.Particles");
	}

	public int getEffectRadius(String reward) {
		return getData(reward).getInt("Effect.Radius");
	}

	public int getEXP(String reward) {
		return getData(reward).getInt("EXP");
	}

	public boolean getGiveInEachWorld(String reward) {
		return getData(reward).getBoolean("GiveInEachWorld");
	}

	public int getItemAmount(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".Amount");
	}

	public int getItemData(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".Data");
	}

	public int getItemDurability(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".Durability");
	}

	public Set<String> getItemEnchants(String reward, String item) {
		try {
			return getData(reward).getConfigurationSection(
					"Items." + item + ".Enchants").getKeys(false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public int getItemEnchantsLevel(String reward, String item, String enchant) {
		return getData(reward).getInt("Items." + item + ".Enchants." + enchant);
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getItemLore(String reward, String item) {
		return (ArrayList<String>) getData(reward).getList(
				"Items." + item + ".Lore");
	}

	public String getItemMaterial(String reward, String item) {
		return getData(reward).getString("Items." + item + ".Material");
	}

	public int getItemMaxAmount(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".MaxAmount");
	}

	public int getItemMinAmount(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".MinAmount");
	}

	public String getItemName(String reward, String item) {
		return getData(reward).getString("Items." + item + ".Name");
	}

	public Set<String> getItems(String reward) {
		try {
			return getData(reward).getConfigurationSection("Items").getKeys(
					false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public int getMaxMoney(String reward) {
		return getData(reward).getInt("MaxMoney");
	}

	public String getMessagesReward(String reward) {
		String msg = getData(reward).getString("Messages.Reward");
		if (msg != null) {
			return msg;
		} else {
			return ConfigFormat.getInstance().getRewardMsg();
		}
	}

	public int getMinMoney(String reward) {
		return getData(reward).getInt("MinMoney");
	}

	public int getMoney(String reward) {
		return getData(reward).getInt("Money");
	}

	public String getPermission(String reward) {
		return "VotingPlugin.Reward." + reward;
	}

	public Set<String> getPotions(String reward) {
		try {
			return getData(reward).getConfigurationSection("Potions").getKeys(
					false);
		} catch (Exception ex) {
			return new HashSet<String>();
		}
	}

	public int getPotionsAmplifier(String reward, String potion) {
		return getData(reward).getInt("Potions." + potion + ".Amplifier");
	}

	public int getPotionsDuration(String reward, String potion) {
		return getData(reward).getInt("Potions." + potion + ".Duration");
	}

	public double getRandomChance(String reward) {
		return getData(reward).getDouble("Random.Chance");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getRandomFallBack(String reward) {
		try {
			return (ArrayList<String>) getData(reward).getList(
					"Random.FallBack");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getRandomRewards(String reward) {
		try {
			return (ArrayList<String>) getData(reward)
					.getList("Random.Rewards");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	public boolean getRequirePermission(String reward) {
		return getData(reward).getBoolean("RequirePermission");
	}

	public Reward getReward(String reward) {
		if (plugin.rewards != null) {
			for (Reward rewardFile : plugin.rewards) {
				if (rewardFile.name.equals(reward)) {
					return rewardFile;
				}
			}
		}
		return new Reward(reward);
	}

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

	public ArrayList<String> getRewardFiles() {
		File folder = new File(plugin.getDataFolder() + File.separator
				+ "Rewards");
		String[] fileNames = folder.list();
		return Utils.getInstance().convertArray(fileNames);
	}

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

	public boolean getSoundEnabled(String reward) {
		return getData(reward).getBoolean("Sound.Enabled");
	}

	public float getSoundPitch(String reward) {
		return (float) getData(reward).getDouble("Sound.Pitch");
	}

	public String getSoundSound(String reward) {
		return getData(reward).getString("Sound.Sound");
	}

	public float getSoundVolume(String reward) {
		return (float) getData(reward).getDouble("Sound.Volume");
	}

	public boolean getTimedEnabled(String reward) {
		return getData(reward).getBoolean("Timed.Enabled");
	}

	public int getTimedHour(String reward) {
		return getData(reward).getInt("Delayed.Hour");
	}

	public int getTimedMinute(String reward) {
		return getData(reward).getInt("Delayed.Minute");
	}

	public boolean getTitleEnabled(String reward) {
		return getData(reward).getBoolean("Title.Enabled");
	}

	public int getTitleFadeIn(String reward) {
		return getData(reward).getInt("Title.FadeIn");
	}

	public int getTitleFadeOut(String reward) {
		return getData(reward).getInt("Title.FadeOut");
	}

	public int getTitleShowTime(String reward) {
		return getData(reward).getInt("Title.ShowTime");
	}

	public String getTitleSubTitle(String reward) {
		return getData(reward).getString("Title.SubTitle");
	}

	public String getTitleTitle(String reward) {
		return getData(reward).getString("Title.Title");
	}

	@SuppressWarnings("unchecked")
	public ArrayList<String> getWorlds(String reward) {
		try {
			return (ArrayList<String>) getData(reward).getList("Worlds");
		} catch (Exception ex) {
			return new ArrayList<String>();
		}
	}

	public boolean renameReward(String reward, String newName) {
		return getRewardFile(reward).renameTo(
				new File(plugin.getDataFolder() + File.separator + "Rewards",
						newName + ".yml"));
	}

	public void set(String reward, String path, Object value) {
		File dFile = getRewardFile(reward);
		FileConfiguration data = YamlConfiguration.loadConfiguration(dFile);
		data.set(path, value);
		Files.getInstance().editFile(dFile, data);
	}

	public void setChance(String reward, int value) {
		set(reward, "Chance", value);
	}

	public void setCommandsConsole(String reward, ArrayList<String> value) {
		set(reward, "Commands.Console", value);
	}

	public void setCommandsPlater(String reward, ArrayList<String> value) {
		set(reward, "Commands.Player", value);
	}

	public void setGiveInEachWorld(String reward, boolean value) {
		set(reward, "GiveInEachWorld", value);
	}

	public void setItemAmount(String reward, String item, int value) {
		set(reward, "Items." + item + ".Amount", value);
	}

	public void setItemData(String reward, String item, int value) {
		set(reward, "Items." + item + ".Data", value);
	}

	public void setItemEnchant(String reward, String item, String enchant,
			int value) {
		set(reward, "Items." + item + ".Enchants." + enchant, value);
	}

	public void setItemLore(String reward, String item, ArrayList<String> value) {
		set(reward, "Items." + item + ".Lore", value);
	}

	public void setItemMaterial(String reward, String item, String value) {
		set(reward, "Items." + item + ".Material", value);
	}

	public void setItemMaxAmount(String reward, String item, int value) {
		set(reward, "Items." + item + ".MaxAmount", value);
	}

	public void setItemMinAmount(String reward, String item, int value) {
		set(reward, "Items." + item + ".MinAmount", value);
	}

	public void setItemName(String reward, String item, String value) {
		set(reward, "Items." + item + ".Name", value);
	}

	public void setMaxMoney(String reward, int value) {
		set(reward, "MaxMoney", value);
	}

	public void setMessagesReward(String reward, String value) {
		set(reward, "Messages.Reward", value);
	}

	public void setMinMoney(String reward, int value) {
		set(reward, "MinMoney", value);
	}

	public void setMoney(String reward, int value) {
		set(reward, "Money", value);
	}

	public void setPotionsAmplifier(String reward, String potion, int value) {
		set(reward, "Potions." + potion + ".Amplifier", value);
	}

	public void setPotionsDuration(String reward, String potion, int value) {
		set(reward, "Potions." + potion + ".Duration", value);
	}

	public void setRequirePermission(String reward, boolean value) {
		set(reward, "RequirePermission", value);
	}

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

	public void setWorlds(String reward, ArrayList<String> value) {
		set(reward, "Worlds", value);
	}

}
