package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.bencodez.votingplugin.util.ServiceSiteValidator;

class ServiceSiteValidatorTest {

	@Test
	void acceptsCommonServiceSiteNames() {
		for (String serviceSite : new String[] { "PlanetMinecraft.com", "Minecraft Server List", "Crafty.gg",
				"https://example.com/vote", "https://list.example/vote?id=1&source=proxy#top",
				"Research & Development", "site_name-2", "Site, Other; Network!",
				"site%20name", "site%2Bname", "site%20%2Bname", "site%20name%2Bnetwork",
				"Serviço de votação",
				"Site\u00A0Name", "Site\uFE0F", "Cafe\u0301", "Site\u3164Name" }) {
			assertTrue(ServiceSiteValidator.isValid(serviceSite), serviceSite);
		}
	}

	@Test
	void rejectsUnsupportedCharacters() {
		for (String serviceSite : new String[] { "[Javascript=1]", "Site's", "\"Site\"", "Site`Name",
				"Site\\Name", "Site\nName", "Site\tName", "Site\u0000Name", "Site\u200BName",
				"%player_name%", "%javascript_vote%", "%be_secret%20",
				"%25player_name%25", "{player}", "site%", "site%2", "site%GG",
				"&kSpoofed", "&aGreen", "&xHex", "&#ff0000Spoofed", "&#ABCDEFText",
				"\u00A7kSpoofed", "\u00A7aGreen" }) {
			assertFalse(ServiceSiteValidator.isValid(serviceSite), serviceSite);
		}
	}

	@Test
	void formattingGuardPreventsTokensAcrossTemplateBoundaries() {
		String guarded = ServiceSiteValidator.inertForFormatting("kSpoofed&");

		assertFalse(("&" + guarded + "a").contains("&k"));
		assertFalse(("%" + guarded + "player_name%").contains("%kSpoofed&player_name%"));
		assertEquals("kSpoofed&", guarded.replace("\u2060", ""));
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
		assertFalse(ServiceSiteValidator.isValid("\u115F"));
		assertFalse(ServiceSiteValidator.isValid("\u1160"));
		assertFalse(ServiceSiteValidator.isValid("\u3164"));
		assertFalse(ServiceSiteValidator.isValid("\uFFA0"));
		assertFalse(ServiceSiteValidator.isValid("\uFFF0"));
		assertFalse(ServiceSiteValidator.isValid(new String(Character.toChars(0xE0001))));
		assertFalse(ServiceSiteValidator.isValid("A".repeat(2049)));
		assertTrue(ServiceSiteValidator.isValid("A".repeat(2048)));
	}
}
