package com.bencodez.votingplugin.broadcast;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;

/**
 * Defines HOW vote broadcasts are rendered.
 *
 * Terminology:
 * - BroadcastMsg : used when EXACTLY ONE item exists (single-site vote)
 * - Header       : used ONCE when MULTIPLE items exist (batch/interval)
 * - ListLine     : used ONCE PER ITEM when MULTIPLE items exist
 *
 * Placeholders (always available):
 * - %player%        : player name (or "Server" for interval summaries)
 * - %site%          : the current site/item for this line
 * - %sites_count%   : number of items in the list
 * - %sites%         : csv of items in the list
 * - %reason%        : internal reason string ("vote", "batch", "interval", etc)
 *
 * Extra context placeholders (INTERVAL_SUMMARY_GLOBAL):
 * - %players% / %numberofplayers%
 * - %sites% / %numberofsites%   (note: %sites% is csv; %numberofsites% is count)
 */
public final class BroadcastFormat {

	private final String broadcastMsg;
	private final String header;
	private final String listLine;

	public BroadcastFormat(String broadcastMsg, String header, String listLine) {
		this.broadcastMsg = broadcastMsg == null ? "" : broadcastMsg;
		this.header = header == null ? "" : header;
		this.listLine = listLine == null ? "" : listLine;
	}

	public List<String> render(String playerName, List<String> items, String reason) {
		return render(playerName, items, reason, Collections.<String, String>emptyMap());
	}

	public List<String> render(String playerName, List<String> items, String reason, Map<String, String> context) {
		List<String> output = new ArrayList<String>();
		int count = items == null ? 0 : items.size();

		String csv = "";
		if (count > 0) {
			StringJoiner joiner = new StringJoiner(", ");
			for (String s : items) {
				if (s != null && !s.isEmpty()) {
					joiner.add(s);
				}
			}
			csv = joiner.toString();
		}

		// SINGLE ITEM → BroadcastMsg
		if (count == 1 && !broadcastMsg.isEmpty()) {
			output.add(apply(broadcastMsg, playerName, items.get(0), count, csv, reason, context));
			return output;
		}

		// MULTI ITEM → Header + ListLine per item
		if (!header.isEmpty()) {
			output.add(apply(header, playerName, "", count, csv, reason, context));
		}

		if (count > 0 && !listLine.isEmpty()) {
			for (String s : items) {
				output.add(apply(listLine, playerName, s, count, csv, reason, context));
			}
		}

		return output;
	}

	private String apply(String template,
			String player,
			String item,
			int count,
			String csv,
			String reason,
			Map<String, String> context) {

		String out = template
				.replace("%player%", player == null ? "" : player)
				.replace("%site%", item == null ? "" : item)
				.replace("%sites_count%", String.valueOf(count))
				.replace("%sites%", csv == null ? "" : csv)
				.replace("%reason%", reason == null ? "" : reason);

		if (context != null && !context.isEmpty()) {
			for (Map.Entry<String, String> e : context.entrySet()) {
				out = out.replace("%" + e.getKey() + "%", e.getValue());
			}
		}

		return out;
	}
}
