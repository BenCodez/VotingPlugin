package com.bencodez.votingplugin.signs;

import java.util.ArrayList;

import org.bukkit.Location;

import com.bencodez.votingplugin.VotingPluginMain;

import lombok.Getter;
import lombok.Setter;

// TODO: Auto-generated Javadoc
/**
 * The Class Signs.
 */
public class Signs {
	private VotingPluginMain plugin;

	@Getter
	@Setter
	private ArrayList<SignHandler> signs;

	public Signs(VotingPluginMain plugin) {
		this.plugin = plugin;
		this.signs = new ArrayList<SignHandler>();
	}

	/**
	 * Gets the sign from location.
	 *
	 * @param loc the loc
	 * @return the sign from location
	 */
	public String getSignFromLocation(Location loc) {
		for (String sign : plugin.getServerData().getSigns()) {
			if (plugin.getServerData().getSignLocation(sign).equals(loc)) {
				return sign;
			}
		}

		return null;
	}

	/**
	 * Load signs.
	 */
	public void loadSigns() {
		setSigns(new ArrayList<SignHandler>());
		for (String sign : plugin.getServerData().getSigns()) {
			// plugin.getLogger().info("Loading sign " + sign);
			getSigns().add(new SignHandler(plugin, sign, plugin.getServerData().getSignLocation(sign),
					plugin.getServerData().getSignSkullLocation(sign), plugin.getServerData().getSignData(sign),
					plugin.getServerData().getSignPosition(sign)));
		}
	}

	/**
	 * Store signs.
	 */
	public void storeSigns() {
		for (SignHandler sign : getSigns()) {
			sign.storeSign();
		}
	}

	/**
	 * Update signs.
	 */
	public void updateSigns() {
		int time = 0;
		for (int i = getSigns().size() - 1; i >= 0; i--) {
			if (!getSigns().get(i).isValid()) {
				plugin.debug(
						"Sign " + i + " invalid, removing from data: " + getSigns().get(i).getLocation().toString());
				getSigns().get(i).removeSign();
				getSigns().remove(i);
			} else {
				getSigns().get(i).updateLines();
				getSigns().get(i).updateSign(time);
				time += 5;
			}
		}

		plugin.debug("Signs updated");
	}

}
