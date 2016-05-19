/* Obtained from https://www.spigotmc.org/threads/libish-inventory-api-kinda.49339/
 */

package com.Ben12345rocks.VotingPlugin.Inventory;

import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.inventory.ItemStack;

import com.Ben12345rocks.VotingPlugin.Utils;

public abstract class BInventoryButton {

	private String name;
	private String[] lore;
	private ItemStack item;

	public BInventoryButton(String name, String[] lore, ItemStack item) {
		setName(name);
		setLore(lore);
		setItem(item);
	}

	public abstract void onClick(InventoryClickEvent event);

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = Utils.getInstance().colorize(name);
	}

	/**
	 * @return the lore
	 */
	public String[] getLore() {
		return lore;
	}

	/**
	 * @param lore
	 *            the lore to set
	 */
	public void setLore(String[] lore) {
		this.lore = Utils.getInstance().colorize(lore);
	}

	/**
	 * @return the item
	 */
	public ItemStack getItem() {
		return item;
	}

	/**
	 * @param item
	 *            the item to set
	 */
	public void setItem(ItemStack item) {
		this.item = item;
	}

}