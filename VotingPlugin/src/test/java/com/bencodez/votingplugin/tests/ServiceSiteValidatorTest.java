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
				"Research & Development", "R&D", "site_name-2", "Site, Other; Network!",
				"site%20name", "site%2Bname", "site%20%2Bname", "site%20name%2Bnetwork",
				"site%20name_with%20spaces", "Top 100% Servers", "%player_name%", "%javascript_vote%",
				"%be_secret%20", "%25player_name%25", "site%", "site%2", "site%GG",
				"&kSpoofed", "&aGreen", "&rReset",
				"&xHex", "&#ff0000Spoofed", "&#ABCDEFText",
				"Serviço de votação",
				"Site\u00A0Name", "Site\uFE0F", "Cafe\u0301", "Site\u3164Name" }) {
			assertTrue(ServiceSiteValidator.isValid(serviceSite), serviceSite);
		}
	}

	@Test
	void rejectsUnsupportedCharacters() {
		for (String serviceSite : new String[] { "[Javascript=1]", "Site's", "\"Site\"", "Site`Name",
				"Site\\Name", "Site\nName", "Site\tName", "Site\u0000Name", "Site\u200BName",
				"{player}", "\u00A7kSpoofed", "\u00A7aGreen" }) {
			assertFalse(ServiceSiteValidator.isValid(serviceSite), serviceSite);
		}
	}

	@Test
	void formattingGuardPreventsTokensAcrossTemplateBoundaries() {
		String value = "%player_name% &aGreen &#ff0000Red kSpoofed&";
		String guarded = ServiceSiteValidator.inertForFormatting(value);

		assertFalse(("&" + guarded + "a").contains("&k"));
		assertFalse(("%" + guarded + "player_name%").contains("%kSpoofed&player_name%"));
		assertFalse(guarded.contains("%player_name%"));
		assertFalse(guarded.contains("&a"));
		assertFalse(guarded.contains("&#ff0000"));
		assertEquals(value, guarded.replace("\u2060", ""));
	}

	@Test
	void actionGuardPreventsTokensAcrossTemplateBoundaries() {
		String guarded = ServiceSiteValidator.inertForActions("player_name%", true);

		assertFalse(("%" + guarded).contains("%player_name%"));
		assertFalse(("&" + ServiceSiteValidator.inertForActions("aGreen", true)).contains("&aGreen"));
		assertEquals("player_name%", guarded.replace("\u2060", ""));
	}

	@Test
	void actionGuardPreservesBenignPercentAndAmpersandDelimiters() {
		assertEquals("site%20name", ServiceSiteValidator.inertForActions("site%20name"));
		assertEquals("site%20name%2Bnetwork",
				ServiceSiteValidator.inertForActions("site%20name%2Bnetwork"));
		assertEquals("example?x=1&source=proxy",
				ServiceSiteValidator.inertForActions("example?x=1&source=proxy"));
	}

	@Test
	void detectsOnlyTemplatesThatOpenSyntaxBeforeThePlaceholder() {
		assertTrue(ServiceSiteValidator.requiresLeadingActionBoundary("say %%ServiceSite%%", "ServiceSite"));
		assertTrue(ServiceSiteValidator.requiresLeadingActionBoundary("say &%SiteName%", "SiteName"));
		assertTrue(ServiceSiteValidator.requiresLeadingActionBoundary("say &#ab%SiteName%", "SiteName"));
		assertTrue(ServiceSiteValidator.requiresLeadingActionBoundary(
				"say %player_%ServiceSite%%", "ServiceSite"));
		assertFalse(ServiceSiteValidator.requiresLeadingActionBoundary("say %ServiceSite%", "ServiceSite"));
		assertFalse(ServiceSiteValidator.requiresLeadingActionBoundary(
				"say %SiteName%%ServiceSite%", "ServiceSite"));
		assertFalse(ServiceSiteValidator.requiresLeadingActionBoundary("say &a%SiteName%", "SiteName"));
		assertFalse(ServiceSiteValidator.requiresLeadingActionBoundary(
				"say 50% %ServiceSite%", "ServiceSite"));
	}

	@Test
	void guardsOnlyOffendingTemplateOccurrences() {
		assertEquals("safe %ServiceSite%",
				ServiceSiteValidator.inertTemplateBoundaries("safe %ServiceSite%", "ServiceSite"));
		assertEquals("unsafe %\u2060%ServiceSite% and &\u2060%ServiceSite%",
				ServiceSiteValidator.inertTemplateBoundaries(
						"unsafe %%ServiceSite% and &%ServiceSite%", "ServiceSite"));
		assertEquals("adjacent %SiteName%%ServiceSite%",
				ServiceSiteValidator.inertTemplateBoundaries(
						"adjacent %SiteName%%ServiceSite%", "ServiceSite"));
		assertEquals("unsafe %player_\u2060%ServiceSite%%",
				ServiceSiteValidator.inertTemplateBoundaries(
						"unsafe %player_%ServiceSite%%", "ServiceSite"));
		assertEquals("unsafe &#ab\u2060%ServiceSite%",
				ServiceSiteValidator.inertTemplateBoundaries(
						"unsafe &#ab%ServiceSite%", "ServiceSite"));
		assertEquals("literal 50% %ServiceSite% and &source %ServiceSite%",
				ServiceSiteValidator.inertTemplateBoundaries(
						"literal 50% %ServiceSite% and &source %ServiceSite%", "ServiceSite"));
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
