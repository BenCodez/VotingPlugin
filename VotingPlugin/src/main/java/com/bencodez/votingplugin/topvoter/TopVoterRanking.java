package com.bencodez.votingplugin.topvoter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

/** Ranking utilities shared by top-voter loading and reward handling. */
public final class TopVoterRanking {

	private TopVoterRanking() {
	}

	public static LinkedHashMap<TopVoterPlayer, Integer> sortByValues(
			LinkedHashMap<TopVoterPlayer, Integer> map, boolean ascending) {
		List<Entry<TopVoterPlayer, Integer>> entries = new ArrayList<>(map.entrySet());
		entries.sort((first, second) -> {
			int result = ascending ? first.getValue().compareTo(second.getValue())
					: second.getValue().compareTo(first.getValue());
			if (result != 0) {
				return result;
			}
			return ascending
					? second.getKey().getLastVoteTime().compareTo(first.getKey().getLastVoteTime())
					: first.getKey().getLastVoteTime().compareTo(second.getKey().getLastVoteTime());
		});

		LinkedHashMap<TopVoterPlayer, Integer> sorted = new LinkedHashMap<>();
		for (Entry<TopVoterPlayer, Integer> entry : entries) {
			sorted.put(entry.getKey(), entry.getValue());
		}
		return sorted;
	}

	public static HashMap<Integer, String> mapAwardPlaces(Set<String> places) {
		HashMap<Integer, String> mapped = new HashMap<>();
		for (String place : places) {
			String[] data = place.split("-");
			try {
				if (data.length > 1) {
					for (int index = Integer.parseInt(data[0]); index < Integer.parseInt(data[1]); index++) {
						mapped.put(index, place);
					}
				} else {
					mapped.put(Integer.parseInt(data[0]), place);
				}
			} catch (RuntimeException e) {
				e.printStackTrace();
			}
		}
		return mapped;
	}
}
