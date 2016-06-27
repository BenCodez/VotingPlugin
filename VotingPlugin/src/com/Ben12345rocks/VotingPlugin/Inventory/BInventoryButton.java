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
	private int slot;

	public BInventoryButton(String name, String[] lore, ItemStack item) {
		setName(name);
		setLore(lore);
		setItem(item);
	}

	/**
	 * @return the item
	 */
	public ItemStack getItem() {
		return item;
	}

	/**
	 * @return the lore
	 */
	public String[] getLore() {
		return lore;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	public abstract void onClick(InventoryClickEvent event);

	/**
	 * @param item
	 *            the item to set
	 */
	public void setItem(ItemStack item) {
		this.item = item;
	}

	/**
	 * @param lore
	 *            the lore to set
	 */
	public void setLore(String[] lore) {
		this.lore = Utils.getInstance().colorize(lore);
	}

	/**
	 * @param name
	 *            the name to set
	 */
	public void setName(String name) {
		this.name = Utils.getInstance().colorize(name);
	}

	public int getSlot() {
		return slot;
	}

	public void setSlot(int slot) {
		this.slot = slot;
	}

}