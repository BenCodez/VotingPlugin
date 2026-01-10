package com.bencodez.votingplugin.tests.votemilestones;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.Map;

import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneGroupParser;
import com.bencodez.votingplugin.specialrewards.votemilestones.VoteMilestoneGroupSelect;

class VoteMilestoneGroupParserTest {

	@Test
	void returnsDefaultAllWhenOptionsMissing() {
		MemoryConfiguration root = new MemoryConfiguration();

		Map<String, VoteMilestoneGroupSelect> modes = VoteMilestoneGroupParser.parseGroups(root);
		assertNotNull(modes);
		assertEquals(VoteMilestoneGroupSelect.ALL, modes.get("default"));
	}

	@Test
	void parsesDefaultAndCustomGroups() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection opts = root.createSection("VoteMilestonesOptions");
		ConfigurationSection groups = opts.createSection("Groups");

		groups.set("Default", "HIGHEST");
		groups.set("MyGroup", "FIRST");
		groups.set("OtherGroup", "ALL");

		Map<String, VoteMilestoneGroupSelect> modes = VoteMilestoneGroupParser.parseGroups(root);
		assertNotNull(modes);

		assertEquals(VoteMilestoneGroupSelect.HIGHEST, modes.get("default"));
		assertEquals(VoteMilestoneGroupSelect.FIRST, modes.get("MyGroup"));
		assertEquals(VoteMilestoneGroupSelect.ALL, modes.get("OtherGroup"));
	}

	@Test
	void ignoresDefaultKeyAsCustomEntry() {
		MemoryConfiguration root = new MemoryConfiguration();
		ConfigurationSection opts = root.createSection("VoteMilestonesOptions");
		ConfigurationSection groups = opts.createSection("Groups");

		groups.set("Default", "FIRST");
		groups.set("default", "ALL"); // should be ignored as "Default" alias

		Map<String, VoteMilestoneGroupSelect> modes = VoteMilestoneGroupParser.parseGroups(root);
		assertEquals(VoteMilestoneGroupSelect.FIRST, modes.get("default"));
	}
}
