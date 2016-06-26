/* Obtained from https://www.spigotmc.org/threads/libish-inventory-api-kinda.49339/
 */

package com.Ben12345rocks.VotingPlugin.Inventory;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import org.bukkit.Bukkit;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Utils;
import com.Ben12345rocks.VotingPlugin.Config.Config;

public class BInventory implements Listener {

	public static void openInventory(Player player, BInventory inventory) {
		Inventory inv = Bukkit.createInventory(player,
				inventory.getInventorySize(), inventory.getInventoryName());
		Iterator<Entry<Integer, BInventoryButton>> it = inventory.getButtons()
				.entrySet().iterator();
		while (it.hasNext()) {
			Map.Entry<Integer, BInventoryButton> pair = it.next();
			{
				ItemStack item = pair.getValue().getItem();
				ItemMeta meta = item.getItemMeta();
				if (pair.getValue().getName() != null) {
					meta.setDisplayName(pair.getValue().getName());
				}
				if (pair.getValue().getLore() != null) {
					meta.setLore(new ArrayList<String>(Arrays.asList(pair
							.getValue().getLore())));
				}
				item.setItemMeta(meta);
				inv.setItem(pair.getKey(), item);
			}
			inv.setItem(pair.getKey(), pair.getValue().getItem());
		}
		player.openInventory(inv);
	}

	private String inventoryName;

	private Map<Integer, BInventoryButton> buttons = new HashMap<Integer, BInventoryButton>();

	public BInventory(String name) {
		setInventoryName(name);
		Bukkit.getPluginManager().registerEvents(this,
				Bukkit.getPluginManager().getPlugins()[0]);
	}

	public void addButton(int position, BInventoryButton button) {
		getButtons().put(position, button);
	}

	/**
	 * @return the inventory buttons
	 */
	public Map<Integer, BInventoryButton> getButtons() {
		return buttons;
	}

	public int getHighestSlot() {
		int highestNum = 0;
		for (int num : buttons.keySet()) {
			if (num > highestNum) {
				highestNum = num;
			}
		}
		return highestNum;
	}

	public String getInventoryName() {
		return inventoryName;
	}

	public int getInventorySize() {
		int highestSlot = getHighestSlot();
		if (highestSlot <= 9) {
			return 9;
		} else if (highestSlot < 18) {
			return 18;
		} else if (highestSlot < 27) {
			return 27;
		} else if (highestSlot < 36) {
			return 36;
		} else if (highestSlot < 45) {
			return 45;
		} else {
			return 45;
		}
	}

	// event handling
	@EventHandler(priority = EventPriority.LOWEST, ignoreCancelled = true)
	public void onInventoryClick(InventoryClickEvent event) {
		if (!(event.getWhoClicked() instanceof Player)) {
			return;
		}
		if (Config.getInstance().getDebugEnabled()) {
			Main.plugin.getLogger().info("Event trigger");
		}
		if (event.isCancelled()) {
			if (Config.getInstance().getDebugEnabled()) {
				Main.plugin.getLogger().info("Event cancelled");
			}
			return;
		}
		ItemStack clickedItem = event.getCurrentItem();
		Inventory inv = event.getInventory();
		if (inv.getName().equalsIgnoreCase(getInventoryName())) {
			for (BInventoryButton button : getButtons().values()) {
				if (clickedItem != null) {
					if (clickedItem.getItemMeta() != null) {
						if (clickedItem.getItemMeta().getDisplayName()
								.equals(button.getName())
								&& clickedItem.getType() == button.getItem()
								.getType()) {
							if (Config.getInstance().getDebugEnabled()) {
								Main.plugin.getLogger().info("Running code");
							}
							button.onClick(event);
							event.setCancelled(true);

							return;
						}
					}
				}
			}
		}
	}

	public void setInventoryName(String inventoryName) {
		this.inventoryName = Utils.getInstance().colorize(inventoryName);
	}

}