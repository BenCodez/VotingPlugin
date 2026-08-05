package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.util.ServiceSiteValidator;

class ServiceSiteValidatorTest {

	@Test
	void acceptsCommonServiceSiteNames() {
		for (String serviceSite : new String[] { "PlanetMinecraft.com", "Minecraft Server List", "Crafty.gg",
				"https://example.com/vote", "site_name-2" }) {
			assertTrue(ServiceSiteValidator.isValid(serviceSite), serviceSite);
		}
	}

	@Test
	void rejectsUnsupportedCharacters() {
		for (String serviceSite : new String[] { "[Javascript=1]", "Site's", "\"Site\"", "Site%player%",
				"Site,Other", "Site;Other", "Site#Comment", "Site`Name", "Site\\Name", "Site\nName",
				"Site\tName" }) {
			assertFalse(ServiceSiteValidator.isValid(serviceSite), serviceSite);
		}
	}

	@Test
	void rejectsMissingAndOversizedNames() {
		assertFalse(ServiceSiteValidator.isValid(null));
		assertFalse(ServiceSiteValidator.isValid(""));
		assertFalse(ServiceSiteValidator.isValid(" "));
		assertFalse(ServiceSiteValidator.isValid("A".repeat(129)));
		assertTrue(ServiceSiteValidator.isValid("A".repeat(128)));
	}
}
