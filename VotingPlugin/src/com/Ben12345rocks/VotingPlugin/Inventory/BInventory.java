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
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;

import com.Ben12345rocks.VotingPlugin.Utils;

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
				meta.setDisplayName(pair.getValue().getName());
				meta.setLore(new ArrayList<String>(Arrays.asList(pair
						.getValue().getLore())));
				item.setItemMeta(meta);
				inv.setItem(pair.getKey(), item);
			}
			inv.setItem(pair.getKey(), pair.getValue().getItem());
		}
		player.openInventory(inv);
	}

	private String inventoryName;
	private int inventorySize;

	private Map<Integer, BInventoryButton> buttons = new HashMap<Integer, BInventoryButton>();

	public BInventory(String name, int size) {
		setInventoryName(name);
		setInventorySize(size);
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

	public String getInventoryName() {
		return inventoryName;
	}

	public int getInventorySize() {
		return inventorySize;
	}

	// event handling
	@EventHandler
	public void onInventoryClick(InventoryClickEvent event) {
		if (!(event.getWhoClicked() instanceof Player)) {
			return;
		}
		// Main.plugin.getLogger().info("Event trigger");
		if (event.isCancelled()) {
			// Main.plugin.getLogger().info("Event cancelled");
			return;
		}
		ItemStack clickedItem = event.getCurrentItem();
		Inventory inv = event.getInventory();
		if (inv.getName().equalsIgnoreCase(getInventoryName())) {
			for (BInventoryButton button : getButtons().values()) {
				if (clickedItem.getItemMeta() != null) {
					if (clickedItem.getItemMeta().getDisplayName()
							.equals(button.getName())
							&& clickedItem.getType() == button.getItem()
									.getType()) {
						// Main.plugin.getLogger().info("Running code");
						button.onClick(event);
						event.setCancelled(true);

						return;
					}
				}
			}
		}
	}

	public void setInventoryName(String inventoryName) {
		this.inventoryName = Utils.getInstance().colorize(inventoryName);
	}

	public void setInventorySize(int inventorySize) {
		this.inventorySize = inventorySize;
	}

}