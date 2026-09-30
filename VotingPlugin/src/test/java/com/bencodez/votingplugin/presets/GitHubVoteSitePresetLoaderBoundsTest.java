package com.bencodez.votingplugin.presets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.Test;

class GitHubVoteSitePresetLoaderBoundsTest {
	@Test
	void startsWithoutBundledPresetData() {
		GitHubVoteSitePresetLoader loader = new GitHubVoteSitePresetLoader("BenCodez", "VotingPlugin-Presets", "main");
		assertTrue(loader.getCachedVoteSitePresets().isEmpty());
	}

	@Test
	void invalidUrlDoesNotTriggerPresetRefresh() throws Exception {
		AtomicBoolean loaded = new AtomicBoolean();
		GitHubVoteSitePresetLoader loader = new GitHubVoteSitePresetLoader("BenCodez", "VotingPlugin-Presets", "main") {
			@Override
			public synchronized List<VoteSitePreset> listAllVoteSitePresets() {
				loaded.set(true);
				return Collections.emptyList();
			}
		};

		assertNull(loader.findVoteSitePresetForURL("not a url"));
		assertFalse(loaded.get());
	}

	@Test
	void acceptsOnlyDirectVoteSiteMetaJsonPaths() {
		assertTrue(GitHubVoteSitePresetLoader.isPresetPathAllowed("presets/votesites/crafty-gg.meta.json"));
		assertFalse(GitHubVoteSitePresetLoader.isPresetPathAllowed("presets/rewards/crafty-gg.meta.json"));
		assertFalse(GitHubVoteSitePresetLoader.isPresetPathAllowed("presets/votesites/../secret.meta.json"));
		assertFalse(GitHubVoteSitePresetLoader.isPresetPathAllowed("presets/votesites/nested/site.meta.json"));
		assertFalse(GitHubVoteSitePresetLoader.isPresetPathAllowed("https://example.com/site.meta.json"));
	}

	@Test
	void rejectsTooManyPresetPaths() {
		StringBuilder json = new StringBuilder("[");
		for (int i = 0; i <= GitHubVoteSitePresetLoader.MAX_PRESET_COUNT; i++) {
			if (i > 0) json.append(',');
			json.append("{\"type\":\"file\",\"path\":\"presets/votesites/site-")
					.append(i).append(".meta.json\"}");
		}
		json.append(']');

		assertThrows(IOException.class, () -> GitHubVoteSitePresetLoader
				.parsePresetPaths(json.toString().getBytes(StandardCharsets.UTF_8)));
	}

	@Test
	void deduplicatesPresetPaths() throws Exception {
		String json = "["
				+ "{\"type\":\"file\",\"path\":\"presets/votesites/crafty-gg.meta.json\"},"
				+ "{\"type\":\"file\",\"path\":\"presets/votesites/crafty-gg.meta.json\"}"
				+ "]";

		assertEquals(java.util.List.of("presets/votesites/crafty-gg.meta.json"),
				GitHubVoteSitePresetLoader.parsePresetPaths(json.getBytes(StandardCharsets.UTF_8)));
	}

	@Test
	void rejectsNonSuccessfulResponses() {
		@SuppressWarnings("unchecked")
		HttpResponse<byte[]> response = mock(HttpResponse.class);
		when(response.statusCode()).thenReturn(500);

		assertThrows(IOException.class, () -> GitHubVoteSitePresetLoader.requireSuccessfulResponse(response));
	}

	@Test
	void ignoresNonPresetEntriesInDirectoryListing() throws Exception {
		String json = "["
				+ "{\"type\":\"file\",\"path\":\"presets/votesites/crafty-gg.meta.json\"},"
				+ "{\"type\":\"file\",\"path\":\"presets/votesites/generic.votesites.yml\"},"
				+ "{\"type\":\"dir\",\"path\":\"presets/votesites/nested.meta.json\"}"
				+ "]";

		assertEquals(java.util.List.of("presets/votesites/crafty-gg.meta.json"),
				GitHubVoteSitePresetLoader.parsePresetPaths(json.getBytes(StandardCharsets.UTF_8)));
	}
}
