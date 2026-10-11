package com.bencodez.votingplugin.tests;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.Test;

import com.bencodez.advancedcore.api.javascript.JavascriptTextTemplate;
import com.bencodez.votingplugin.util.ServiceSiteValidator;

class ServiceSiteValidatorTest {

	@Test
	void acceptsCommonServiceSiteNames() {
		for (String serviceSite : new String[] { "PlanetMinecraft.com", "Minecraft Server List", "Crafty.gg",
				"https://example.com/vote", "https://list.example/vote?id=1#top",
				"https://example.com/search?q=site+name+network", "site_name-2", "Site, Other; Network!",
				"Serviço de votação", "Site\u00A0Name", "Site\uFE0F", "Cafe\u0301", "Site\u3164Name" }) {
			assertTrue(ServiceSiteValidator.isValid(serviceSite), serviceSite);
		}
	}

	@Test
	void acceptsLegacyFormattingIdentifiersWithoutChangingMatching() {
		for (String value : new String[] { "R&D", "&kSpoofed", "%player_name%", "§kSpoofed", "{RED}",
				"https://list.example/vote?id=1&source=proxy", "https://example.com/site%20name" }) {
			assertTrue(ServiceSiteValidator.isValid(value), value);
			String display = ServiceSiteValidator.inertForFormatting(value);
			assertFalse(display.contains("&") || display.contains("%") || display.contains("§")
					|| display.contains("{") || display.contains("}"));
			assertFalse(display.contains("\u2060"));
		}
	}

	@Test
	void formattingGuardPreventsTokensAcrossTemplateBoundaries() {
		AtomicInteger executions = new AtomicInteger();
		for (String[] layout : new String[][] {
				{ "[%SiteName%]", "Javascript=1" },
				{ "[Java%SiteName%]", "script=1" },
				{ "[%SiteName%script=1]", "Java" } }) {
			assertTrue(ServiceSiteValidator.isValid(layout[1]));
			String rendered = layout[0].replace("%SiteName%",
					ServiceSiteValidator.inertForFormatting(layout[1]));
			assertEquals(rendered, JavascriptTextTemplate.parse(rendered).evaluate(text -> text, script -> {
				executions.incrementAndGet();
				return "executed";
			}));
		}
		assertEquals(0, executions.get());
		assertEquals("& kSpoofed ", "&%SiteName%".replace("%SiteName%",
				ServiceSiteValidator.inertForFormatting("kSpoofed")));
		assertEquals("% player_name %", "%" + ServiceSiteValidator.inertForFormatting("player_name") + "%");
		assertEquals("", ServiceSiteValidator.inertForFormatting(null));
		assertEquals("", ServiceSiteValidator.inertForFormatting(""));
	}

	@Test
	void rejectsUnsupportedCharacters() {
		for (String value : new String[] { "[Javascript=1]", "Site's", "\"Site\"", "Site`Name",
				"Site\\Name", "Site\nName", "Site\tName", "Site\u0000Name", "Site\u200BName" }) {
			assertFalse(ServiceSiteValidator.isValid(value), value);
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
