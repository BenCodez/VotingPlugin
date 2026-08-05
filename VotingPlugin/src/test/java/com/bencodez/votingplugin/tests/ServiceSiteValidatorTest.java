package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.util.ServiceSiteValidator;

class ServiceSiteValidatorTest {

	@Test
	void acceptsCommonServiceSiteNames() {
		for (String serviceSite : new String[] { "PlanetMinecraft.com", "Minecraft Server List", "Crafty.gg",
				"https://example.com/vote", "https://list.example/vote?id=1&source=proxy#top",
				"https://example.com/search?q=site%20name+network", "site_name-2", "Site, Other; Network!",
				"Serviço de votação", "Site\u00A0Name", "Site\uFE0F", "Cafe\u0301" }) {
			assertTrue(ServiceSiteValidator.isValid(serviceSite), serviceSite);
		}
	}

	@Test
	void rejectsUnsupportedCharacters() {
		for (String serviceSite : new String[] { "[Javascript=1]", "Site's", "\"Site\"", "Site`Name",
				"Site\\Name", "Site\nName", "Site\tName", "Site\u0000Name", "Site\u200BName" }) {
			assertFalse(ServiceSiteValidator.isValid(serviceSite), serviceSite);
		}
	}

	@Test
	void rejectsMissingAndOversizedNames() {
		assertFalse(ServiceSiteValidator.isValid(null));
		assertFalse(ServiceSiteValidator.isValid(""));
		assertFalse(ServiceSiteValidator.isValid(" "));
		assertFalse(ServiceSiteValidator.isValid("\u00A0"));
		assertFalse(ServiceSiteValidator.isValid("\u2007"));
		assertFalse(ServiceSiteValidator.isValid("\u202F"));
		assertFalse(ServiceSiteValidator.isValid(" \u00A0\u2007\u202F "));
		assertFalse(ServiceSiteValidator.isValid("\uFE0F"));
		assertFalse(ServiceSiteValidator.isValid("\u034F"));
		assertFalse(ServiceSiteValidator.isValid("\u0301"));
		assertFalse(ServiceSiteValidator.isValid("\uFE0F\u034F\u0301"));
		assertFalse(ServiceSiteValidator.isValid("A".repeat(2049)));
		assertTrue(ServiceSiteValidator.isValid("A".repeat(2048)));
	}
}
