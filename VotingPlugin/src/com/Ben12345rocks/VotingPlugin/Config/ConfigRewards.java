package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Files.Files;
import com.Ben12345rocks.VotingPlugin.Objects.Reward;

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

	public int getChance(String reward) {
		return getData(reward).getInt("Chance");
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

	public boolean getGiveInEachWorld(String reward) {
		return getData(reward).getBoolean("GiveInEachWorld");
	}

	public int getItemAmount(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".Amount");
	}

	public int getItemData(String reward, String item) {
		return getData(reward).getInt("Items." + item + ".Data");
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
		return getData(reward).getConfigurationSection("Items").getKeys(false);
	}

	public int getMaxMoney(String reward) {
		return getData(reward).getInt("MaxMoney");
	}

	public String getMessagesOfflineReward(String reward) {
		String msg = getData(reward).getString("Messages.OfflineReward");
		if (msg != null) {
			return msg;
		} else {
			return "&aMessage when vote was offline";
		}
	}

	public String getMessagesReward(String reward) {
		String msg = getData(reward).getString("Messages.Reward");
		if (msg != null) {
			return msg;
		} else {
			return "&aMessage on reward";
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

	@SuppressWarnings("unchecked")
	public ArrayList<String> getWorlds(String reward) {
		return (ArrayList<String>) getData(reward).getList("Worlds");
	}

	public boolean renameVoteSite(String reward, String newName) {
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

	public void setMessagesOfflineReward(String reward, String value) {
		set(reward, "Messages.OfflineReward", value);
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

	public void setRequirePermission(String reward, boolean value) {
		set(reward, "RequirePermission", value);
	}

	public void setWorlds(String reward, ArrayList<String> value) {
		set(reward, "Worlds", value);
	}

}
