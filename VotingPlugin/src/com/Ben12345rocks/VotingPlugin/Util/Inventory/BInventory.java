/* Obtained from https://www.spigotmc.org/threads/libish-inventory-api-kinda.49339/
 */

package com.Ben12345rocks.VotingPlugin.Util.Inventory;

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

import com.Ben12345rocks.VotingPlugin.Utils;

// TODO: Auto-generated Javadoc
/**
 * The Class BInventory.
 */
public class BInventory implements Listener {

	/**
	 * Open inventory.
	 *
	 * @param player
	 *            the player
	 * @param inventory
	 *            the inventory
	 */
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

	/** The inventory name. */
	private String inventoryName;

	/** The buttons. */
	private Map<Integer, BInventoryButton> buttons = new HashMap<Integer, BInventoryButton>();

	/**
	 * Instantiates a new b inventory.
	 *
	 * @param name
	 *            the name
	 */
	public BInventory(String name) {
		setInventoryName(name);
		Bukkit.getPluginManager().registerEvents(this,
				Bukkit.getPluginManager().getPlugins()[0]);
	}

	/**
	 * Adds the button.
	 *
	 * @param position
	 *            the position
	 * @param button
	 *            the button
	 */
	public void addButton(int position, BInventoryButton button) {
		getButtons().put(position, button);
	}

	/**
	 * Gets the buttons.
	 *
	 * @return the buttons
	 */
	public Map<Integer, BInventoryButton> getButtons() {
		return buttons;
	}

	/**
	 * Gets the highest slot.
	 *
	 * @return the highest slot
	 */
	public int getHighestSlot() {
		int highestNum = 0;
		for (int num : buttons.keySet()) {
			if (num > highestNum) {
				highestNum = num;
			}
		}
		return highestNum;
	}

	/**
	 * Gets the inventory name.
	 *
	 * @return the inventory name
	 */
	public String getInventoryName() {
		return inventoryName;
	}

	/**
	 * Gets the inventory size.
	 *
	 * @return the inventory size
	 */
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

	/**
	 * On inventory click.
	 *
	 * @param event
	 *            the event
	 */
	@EventHandler(priority = EventPriority.LOWEST, ignoreCancelled = true)
	public void onInventoryClick(InventoryClickEvent event) {
		if (!(event.getWhoClicked() instanceof Player)) {
			return;
		}

		// Main.plugin.debug("Event ran");

		Inventory inv = event.getInventory();
		if (inv.getName().equalsIgnoreCase(getInventoryName())) {
			// Main.plugin.debug("Iventory equal");
			for (int buttonSlot : getButtons().keySet()) {
				BInventoryButton button = getButtons().get(buttonSlot);
				if (event.getSlot() == buttonSlot) {
					// Main.plugin.debug("Running onclick");
					button.onClick(event);
					event.setCancelled(true);

					return;
				}

			}
		}
	}

	/**
	 * Sets the inventory name.
	 *
	 * @param inventoryName
	 *            the new inventory name
	 */
	public void setInventoryName(String inventoryName) {
		this.inventoryName = Utils.getInstance().colorize(inventoryName);
	}

}